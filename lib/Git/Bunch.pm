package Git::Bunch;

use 5.010001;
use strict;
use warnings;
use Log::Any '$log';

use Builtin::Logged qw(system my_qx);
use Cwd ();
use File::chdir;
use File::Path qw(make_path);
use String::ShellQuote;

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(check_bunch sync_bunch backup_bunch exec_bunch);

# VERSION

our %SPEC;

$SPEC{":package"} = {
    v => 1.1,
    summary => 'Manage gitbunch directory (directory which contain git repos)',
};

our %common_args_spec = (
    source           => {
        summary      => 'Directory to check',
        schema       => ['str*'],
        req          => 1,
        pos          => 0,
    },
    sort             => {
        summary      => 'Order entries in bunch',
        schema       => ['str' => {
            default => '-mtime',
            in      => [qw/name -name mtime -mtime rand/],
        }],
    },
    include_repos    => {
        summary      => 'Specific git repos to sync, if not specified '.
            'all repos in the bunch will be processed',
        schema       => ['array' => {
            of => 'str*',
        }],
        cmdline_aliases => {
            repos => {},
        },
    },
    include_repos_pat=> {
        summary      => 'Specify regex pattern of repos to include',
        schema       => ['str'],
    },
    exclude_repos    => {
        summary      => 'Exclude some repos from processing',
        schema       => ['array*' => {of => 'str*'}],
    },
    exclude_non_git_dirs => {
        summary      => 'Exclude non-git dirs from processing',
        schema       => ['bool'],
        description  => <<'_',

This only applies to 'backup_bunch' and 'sync_bunch' operations. Operations like
'check_bunch' and 'exec_bunch' already ignore these and only operate on git
repos.

_
    },
    exclude_files    => {
        summary      => 'Exclude files from processing',
        schema       => ['bool'],
    description      => <<'_',

This only applies to 'backup_bunch' and 'sync_bunch' operations. Operations like
'check_bunch' and 'exec_bunch' already ignore these and only operate on git
repos.

_
    },
    exclude_repos_pat=> {
        summary      => 'Specify regex pattern of repos to exclude',
        schema       => ['str'],
    },
);

our %target_arg_spec = (
    target           => {
        summary      => 'Destination bunch',
        schema       => ['str*'],
        req          => 1,
        pos          => 1,
    },
);

sub _check_common_args {
    my ($args, $requires_target) = @_;
    my $res;

    $args->{source} or return [400, "Please specify source"];
    $args->{source} =~ s!/+$!!;
    $res = _check_bunch_sanity(\$args->{source}, 'Source');
    return $res unless $res->[0] == 200;

    my $ir = $args->{include_repos};
    return [400, "include_repos must be an array"]
        if defined($ir) && ref($ir) ne 'ARRAY';
    my $irp = $args->{include_repos_pat};
    if (defined $irp) {
        return [400, "Invalid include_repos_pat: must be a string"]
            if ref($irp);
        return [400, "Invalid include_repos_pat: $@"]
            if !(eval q{qr/$irp/});
    }
    my $er = $args->{exclude_repos};
    return [400, "exclude_repos must be an array"]
        if defined($er) && ref($er) ne 'ARRAY';
    my $erp = $args->{exclude_repos_pat};
    if (defined $erp) {
        return [400, "Invalid exclude_repos_pat: must be a string"]
            if ref($erp);
        return [400, "Invalid exclude_repos_pat: must be a string"]
            if !(eval q{qr/$erp/});
    }

    if ($requires_target) {
        $args->{target} or return [400, "Please specify target"];
        $res = _check_bunch_sanity(\$args->{target}, 'Target', 0);
        return $res unless $res->[0] == 200;
    }

    # XXX rand is not proper shuffle
    my $sort = $args->{sort} // "-mtime";
    my $sortsub;
    if (!$sort) {
        $sortsub = sub { 1 };
    } elsif ($sort eq '-mtime') {
        $sortsub = sub {((-M $a)//0) <=> ((-M $b)//0)};
    } elsif ($sort eq 'mtime') {
        $sortsub = sub {((-M $b)//0) <=> ((-M $a)//0)};
    } elsif ($sort eq '-name') {
        $sortsub = sub {$b cmp $a};
    } elsif ($sort eq 'name') {
        $sortsub = sub {$a cmp $b};
    } else { # rand
        $sortsub = sub {int(3*rand())-1};
    }
    [200, "OK", undef, {sortsub=>$sortsub}];
}

# return 1 if normal git repo, 2 if bare git repo, 0 if not repo
sub _is_repo {
    my $dir = shift;

    return 0 unless (-d $dir);
    return 1 if (-d "$dir/.git");
    return 2 if (-d "$dir/branches") && (-f "$dir/HEAD");
    0;
}

# return true if entry should be skipped
sub _skip_process_entry {
    my ($e, $args, $dir, $skip_non_repo) = @_;

    return 1 if $e eq '.' || $e eq '..';
    my $is_repo = _is_repo($dir);

    if ($skip_non_repo && !$is_repo) {
        $log->warn("Skipped $e (not a git repo), ".
                       "please remove it or rename to .$e");
        return 1;
    }
    if ($is_repo) {
        my $ir = $args->{include_repos};
        if ($ir && !($e ~~ @$ir)) {
            $log->debug("Skipped $e (not in include_repos)");
            return 1;
        }
        my $irp = $args->{include_repos_pat};
        if (defined($irp) && $e !~ qr/$irp/) {
            $log->debug("Skipped $e (not matched include_repos_pat)");
            return 1;
        }
        my $er = $args->{exclude_repos};
        if ($er && $e ~~ @$er) {
            $log->debug("Skipped $e (in exclude_repos)");
            return 1;
        }
        my $erp = $args->{exclude_repos_pat};
        if (defined($erp) && $e =~ qr/$erp/) {
            $log->debug("Skipped $e (not matched exclude_repos_pat)");
            return 1;
        }
    } elsif ((-f $dir) && $args->{exclude_files}) {
        $log->debug("Skipped $e (exclude_files)");
        return 1;
    } elsif ((-d $dir) && $args->{exclude_non_git_dirs}) {
        $log->debug("Skipped $e (exclude_non_git_dirs)");
        return 1;
    }
    return;
}

sub _skip_process_repo {
    my ($repo, $args, $dir) = @_;
    _skip_process_entry($repo, $args, $dir, 1);
}

sub _check_bunch_sanity {
    my ($path_ref, $title, $must_exist) = @_;
    $title //= "Directory";
    $$path_ref =~ s!/+$!!;
    if ($must_exist // 1) {
        (-d $$path_ref) or return [404, "$title doesn't exist"];
    }
    _is_repo($$path_ref) and
        return [400, "$title is probably a git repo, ".
                    "you should specify a dir *containing* ".
                        "git repos instead"];
    [200, "OK"];
}

$SPEC{check_bunch} = {
    v             => 1.1,
    summary       =>
        'Check status of git repositories inside gitbunch directory',
    description   => <<'_',

Will perform a 'git status' for each git repositories inside the bunch and
report which repositories are clean/unclean.

Will die if can't chdir into bunch or git repository.

_
    args          => {
        %common_args_spec,
    },
    deps => {
        all => [
            {prog => 'git'},
        ],
    },
    features => {
        progress => 1,
    },
};
sub check_bunch {
    my %args = @_;
    my $res;

    my $progress = $args{-progress};

    # XXX schema
    $res = _check_common_args(\%args);
    return $res unless $res->[0] == 200;
    my $sortsub = $res->[3]{sortsub};
    my $source = $args{source};

    $log->info("Checking bunch $source ...");

    my $has_unclean;
    my %res;
    local $CWD = $source;

    my @entries = sort $sortsub grep {-d} <*>;

    my $i = 0;
    $progress->pos(0) if $progress;
    $progress->target(~~@entries) if $progress;
  REPO:
    for my $repo (@entries) {
        $CWD = $i++ ? "../$repo" : $repo;
        next REPO if _skip_process_repo($repo, \%args, ".");

        $progress->update(pos => $i,
                          message =>
                              "Checking repo $repo ...")
            if $progress;

        my $output = `LANG=C git status 2>&1`;
        my $exit = $? >> 8;
        if ($exit == 0 && $output =~ /nothing to commit/) {
            $log->info("$repo is clean");
            $res{$repo} = [200, "Clean"];
            next;
        }

        $has_unclean++;
        if ($exit == 0 &&
                $output =~ /(
                                Changes \s to \s be \s committed |
                                Changes \s not \s staged \s for \s commit |
                                Changed \s but
                            )/mx) {
            $log->warn("$repo needs commit");
            $res{$repo} = [500, "Needs commit"];
        } elsif ($exit == 0 &&
                     $output =~ /(
                                     Untracked \s files
                                 )/x) {
            $log->warn("$repo has untracked files");
            $res{$repo} = [500, "Has untracked files"];
        } elsif ($exit == 0 && $output =~ /Unmerged paths:/) {
            $log->warn("$repo needs merging");
            $res{$repo} = [500, "Needs merging"];
        } elsif ($exit == 128 && $output =~ /Not a git repository/) {
            $log->warn("$repo is not a git repo (2)");
            $res{$repo} = [500, "Not a git repo (2)"];
        } else {
            $log->error("Can't figure out result of 'git status' ".
                            "for repo $repo: exit=$exit, output=$output");
            $res{$repo} = [500, "Unknown (exit=$exit, output=$output)"];
        }
    }
    $progress->finish if $progress;
    [200,
     $has_unclean ? "Some repos unclean" : "All repos clean",
     \%res,
     {"cmdline.display_result" => 0}];
}

sub _sync_repo {
    my ($src, $dest, $repo, $opts) = @_;
    my $exit;

    my @src_branches;
    my @dest_branches;
    my %src_heads;  # last revisions for each branch
    my %dest_heads; # last revisions for each branch

    local $CWD = "$src/$repo";
    @src_branches = map {(/^[* ] (.+)/, $1)[-1]} my_qx("LANG=C git branch");
    $exit = $? >> 8;
    if ($exit) {
        $log->error("Can't list branches on src repo $src/$repo: $exit");
        return [500, "Can't list source branches"];
    }
    $log->debugf("Source branches: %s", \@src_branches);

    for my $branch (@src_branches) {
        my $output = my_qx("LANG=C git log -1 '$branch'");
        $exit = $? >> 8;
        if ($exit) {
            $log->error("Can't find out head for branch $branch on src repo ".
                            "$src/$repo: $exit");
            return [500, "Can't find out head for source branch $branch"];
        }
        $output =~ /commit (\S+)/ or do {
            $log->error("Can't recognize git log output ".
                            "(searching for commit XXX): $output");
            return [500, "Can't recognize git log output on src: $output"];
        };
        $src_heads{$branch} = $1;
    }
    $log->debugf("Source branch heads: %s", \%src_heads);

    $CWD = "$dest/$repo";
    my $is_bare = _is_repo(".") == 2;
    @dest_branches = map {(/^[* ] (.+)/, $1)[-1]} my_qx("LANG=C git branch");
    if ($exit) {
        $log->error("Can't list branches on dest repo $repo: $exit");
        return [500, "Can't list branches on dest: $exit"];
    }
    $log->debugf("Dest branches: %s", \@dest_branches);
    for my $branch (@dest_branches) {
        my $output = my_qx("LANG=C git log -1 '$branch'");
        $exit = $? >> 8;
        if ($exit) {
            $log->error("Can't find out head for branch $branch on dest repo ".
                            "$dest/$repo: $exit");
            return [500, "Can't find out head for dest branch $branch"];
        }
        $output =~ /commit (\S+)/ or do {
            $log->error("Can't recognize git log output ".
                            "(searching for commit XXX): $output");
            return [500, "Can't recognize git log output on src: $output"];
        };
        $dest_heads{$branch} = $1;
    }
    $log->debugf("Dest branch heads: %s", \%dest_heads);

    my $output;
    my $lock_deleted;
    my $changed_branch;
  BRANCH:
    for my $branch (@src_branches) {
        # XXX we should allow fetching tags only even if head is the same, but
        # right now tags are not that important
        if ($src_heads{$branch} && $dest_heads{$branch} &&
                $src_heads{$branch} eq $dest_heads{$branch}) {
            $log->debug("Skipping branch $branch because heads are the same");
            next BRANCH;
        }
        $changed_branch++;
        if (0 && !$lock_deleted++) {
            $log->debug("Deleting locks first ...");
            unlink "$src/$repo" .($is_bare ? "" : "/.git")."/index.lock";
            unlink "$dest/$repo".($is_bare ? "" : "/.git")."/index.lock";
        }
        $log->info("Updating branch $branch of repo $repo ...")
            if @src_branches > 1;
        if ($is_bare) {
            $output = my_qx(
                join("",
                     "cd '$src/$repo'; ",
                     "LANG=C git push '$dest/$repo' '$branch' 2>&1",
                 ));
        } else {
            $output = my_qx(
                join("",
                     "cd '$dest/$repo'; ",
                     ($branch ~~ @dest_branches ? "":"git branch '$branch'; "),
                     "git checkout '$branch' 2>/dev/null; ",
                     "LANG=C git pull '$src/$repo' '$branch' 2>&1"
                 ));
        }
        $exit = $? >> 8;
        if ($exit == 0 && $output =~ /Already up-to-date/) {
            $log->debug("Branch $branch of repo $repo is up to date");
            next BRANCH;
        } elsif ($output =~ /^error: (.+)/m) {
            $log->error("Can't successfully git pull/push branch $branch of ".
                            "repo $repo: $1");
            return [500, "git pull/push branch $branch failed: $1"];
        } elsif ($exit == 0 &&
                     $output =~ /^Updating \s|
                                 ^Merge \s made \s by \s recursive|
                                 ^Merge \s made \s by \s the \s 'recursive'|
                                /mx) {
            $log->warn("Branch $branch of repo $repo updated")
                if @src_branches > 1;
            $log->warn("Repo $repo updated")
                if @src_branches == 1;
        } else {
            $log->error(
                "Can't recognize 'git pull/push' output for branch ".
                    "$branch of repo $repo: exit=$exit, output=$output");
            return [500, "Can't recognize git pull/push output: $output"];
        }
        $log->debug("Result of 'git pull/push' for branch $branch of repo ".
                        "$repo: exit=$exit, output=$output");

        $output = my_qx("cd '$dest/$repo'; ".
                            "LANG=C git fetch --tags '$src/$repo' 2>&1");
        $exit = $? >> 8;
        if ($exit != 0) {
            $log->debug("Failed fetching tags: ".
                            "$output (exit=$exit)");
            return [500, "git fetch --tags failed: $1"];
        }
    }

    if ($opts->{delete_branch}) {
        for my $branch (@dest_branches) {
            next if $branch ~~ @src_branches;
            next if $branch eq 'master'; # can't delete master branch
            $changed_branch++;
            $log->info("Deleting branch $branch of repo $repo because ".
                           "it no longer exists in src ...");
            system("cd '$dest/$repo' && git checkout master 2>/dev/null && ".
                       "git branch -D '$branch' 2>/dev/null");
            $exit = $? >> 8;
            $log->error("Failed deleting branch $branch of repo $repo: $exit")
                if $exit;
        }
    }

    if ($changed_branch) {
        return [200, "OK"];
    } else {
        return [304, "Not modified"];
    }
}

$SPEC{sync_bunch} = {
    v             => 1.1,
    summary       =>
        'Synchronize bunch to another bunch',
    description   => <<'_',

For each git repository in the bunch, will perform a 'git pull/push' for each
branch. If repository in destination doesn't exist, it will be rsync-ed first
from source. When 'git pull' fails, will exit to let you fix the problem
manually.

For all other non-git repos, will simply synchronize by one-way rsync.

_
    args          => {
        %common_args_spec,
        %target_arg_spec,
        delete_branch    => {
            summary      => 'Whether to delete branches in dest repos '.
                'not existing in source repos',
            schema       => ['bool' => default => 0],
        },
        rsync_opt_maintain_ownership => {
            summary      => 'Whether or not, when rsync-ing from source, '.
                'we use -a (= -rlptgoD) or -rlptD (-a minus -go)',
            schema       => ['bool' => default => 0],
            description  => <<'_',

Sometimes using -a results in failure to preserve permission modes on
sshfs-mounted filesystem, while -rlptD succeeds, so by default we don't maintain
ownership. If you need to maintain ownership (e.g. you run as root and the repos
are not owned by root), turn this option on.

_
        },
        create_bare_target => {
            summary      => 'Whether to create bare git repo '.
                'when target does not exist',
            schema       => ['bool'],
            description  => <<'_',

When target repo does not exist, gitbunch can either copy the source repo using
`rsync` (the default, if this setting is undefined), or it can create target
repo with `git init --bare` (if this setting is set to 1), or it can create
target repo with `git init` (if this setting is set to 0).

Bare git repositories contain only contents of the .git folder inside the
directory and no working copies of your source files.

Creating bare repos are apt for backup purposes since they are more
space-efficient.

Non-repos will still be copied/rsync-ed.

_
            cmdline_aliases => {
                # old name, deprecated since v0.29, remove in later releases
                use_bare => {},
            },
        },
        backup => {
            summary     => 'Whether doing backup to target',
            schema      => ['bool'],
            description => <<'_',

This setting lets you express that you want to perform synchronizing to a backup
target, and that you do not do work on the target. Thus, you do not care about
uncommitted or untracked files/dirs in the target repos (might happen if you
also do periodic copying of repos to backup using cp/rsync). When this setting
is turned on, the function will first do a `git clean -f -d` (to delete
untracked files/dirs) and then `git checkout .` (to discard all uncommitted
changes). This setting will also implicitly turn on `create_bare` setting
(unless that setting has been explicitly enabled/disabled).

_
        },
    },
    deps => {
        all => [
            {prog => 'git'},
            {prog => 'rsync'},
        ],
    },
    features => {
        progress => 1,
    },
};
sub sync_bunch {
    require Capture::Tiny;
    require UUID::Random;

    my %args = @_;
    my $res;

    my $progress = $args{-progress};

    # XXX schema
    $res = _check_common_args(\%args, 1);
    return $res unless $res->[0] == 200;
    my $sortsub = $res->[3]{sortsub};
    my $delete_branch = $args{delete_branch} // 0;
    my $source = $args{source};
    my $target = $args{target};
    my $create_bare = $args{create_bare_target};
    my $backup = $args{backup};
    my $exit;

    $create_bare //= 1 if $backup;

    my $cmd;

    unless (-d $target) {
        $log->debugf("Creating target directory %s ...", $target);
        make_path($target)
            or return [500, "Can't create target directory $target: $!"];
    }
    $target = Cwd::abs_path($target);

    my $a = $args{rsync_opt_maintain_ownership} ? "aH" : "rlptDH";

    my @entries;
    opendir my($d), $source; @entries = sort $sortsub readdir($d);

    $source = Cwd::abs_path($source);
    local $CWD = $target;
    my %res;
    my $i = 0;
    $progress->pos(0) if $progress;
    $progress->target(~~@entries) if $progress;
  ENTRY:
    for my $e (@entries) {
        ++$i;
        next ENTRY if _skip_process_entry($e, \%args, "$source/$e");
        my $is_repo = _is_repo("$source/$e");
        if (!$is_repo) {
            $progress->update(pos => $i,
                              message =>
                                  "Sync-ing non-git file/directory $e ...")
                 if $progress;
            # just some random unique string so we can detect whether any
            # file/dir is modified/added to target. to check files deleted in
            # target, we use /^deleting /x
            my $uuid = UUID::Random::generate();
            my $v = $log->is_debug ? "-v" : "";
            $cmd = "LANG=C rsync --log-format=$uuid -${a}z $v --del --force ".
                shell_quote("$source/$e")." .";
            my ($stdout, @result) = Capture::Tiny::capture_stdout(
                sub { system($cmd) });
            if ($result[0]) {
                $log->warn("Rsync failed, please check: $result[0]");
                $res{$e} = [500, "rsync failed: $result[0]"];
            } else {
                if ($stdout =~ /^(deleting |\Q$uuid\E)/m) {
                    $log->warn("Non-git file/dir '$e' updated");
                }
                $res{$e} = [200, "rsync-ed"];
            }
            next ENTRY;
        }

        my $created;
        if (!(-e $e)) {
            if ($create_bare) {
                $log->info("Initializing target repo $e (bare) ...");
                $cmd = "mkdir ".shell_quote($e)." && cd ".shell_quote($e).
                    " && git init --bare";
                system($cmd);
                $exit = $? >> 8;
                if ($exit) {
                    $log->warn("Git init failed, please check: $exit");
                    $res{$e} = [500, "git init --bare failed: $exit"];
                    next ENTRY;
                }
                $created++;
                # continue to sync-ing
            } elsif (defined $create_bare) {
                $log->info("Initializing target repo $e (non-bare) ...");
                $cmd = "mkdir ".shell_quote($e)." && cd ".shell_quote($e).
                    " && git init";
                system($cmd);
                $exit = $? >> 8;
                if ($exit) {
                    $log->warn("Git init failed, please check: $exit");
                    $res{$e} = [500, "git init failed: $exit"];
                    next ENTRY;
                }
                $created++;
                # continue to sync-ing
            } else {
                $progress->update(pos => $i,
                                  message =>
                                      "Copying repo $e ...")
                     if $progress;
                $cmd = "rsync -${a}z ".shell_quote("$source/$e")." .";
                system($cmd);
                $exit = $? >> 8;
                if ($exit) {
                    $log->warn("Rsync failed, please check: $exit");
                    $res{$e} = [500, "rsync failed: $exit"];
                } else {
                    $res{$e} = [200, "rsync-ed"];
                }
                $log->warn("Repo $e copied");
                next ENTRY;
            }
        }

        if ($backup && !$created) {
            $log->debug("Discarding changes in target repo $e ...");
            local $CWD = $e;
            system "git clean -f -d && git checkout .";
            # ignore error for now, let's go ahead and sync anyway
        }

        $progress->update(pos => $i, message => "Sync-ing repo $e ...")
             if $progress;
        my $res = _sync_repo(
            $source, $target, $e,
            {delete_branch => $delete_branch},
        );
        $res{$e} = $res;
    }
    $progress->finish if $progress;

    [200,
     "OK",
     \%res,
     {"cmdline.display_result" => 0}];
}

$SPEC{exec_bunch} = {
    v             => 1.1,
    summary       =>
        'Execute a command for each repo in the bunch',
    description   => <<'_',

For each git repository in the bunch, will chdir to it and execute specified
command.

_
    args          => {
        %common_args_spec,
        command   => {
            summary  => 'Command to execute',
            schema   => ['str*'],
            req      => 1,
            pos      => 1,
            greedy   => 1,
        },
    },
};
sub exec_bunch {
    my %args = @_;
    my $res;
    my $exit;

    # XXX schema
    $res = _check_common_args(\%args);
    return $res unless $res->[0] == 200;
    my $sortsub = $res->[3]{sortsub};
    my $source  = $args{source};
    my $command = $args{command};
    defined($command) or return [400, "Please specify command"];

    local $CWD = $source;
    my %res;
    my $i = 0;
  REPO:
    for my $repo (sort $sortsub grep {-d} <*>) {
        $CWD = $i++ ? "../$repo" : $repo;
        next REPO if _skip_process_repo($repo, \%args, ".");
        $log->info("Executing command on $repo ...");
        system($command);
        $exit = $? >> 8;
        if ($exit) {
            $log->warn("Command failed: $exit");
            $res{$repo} = [500, "Command failed: $exit"];
        } else {
            $res{$repo} = [200, "Command successful"];
        }
        next REPO;
    }

    [200,
     "OK",
     \%res,
     {"cmdline.display_result" => 0}];
}

1;
# ABSTRACT: Manage gitbunch directory (directory which contain git repos)

=head1 SYNOPSIS

To check the status of bunch (will do a 'git status' for each git repo inside
the bunch and report which repos are 'unclean', e.g. needs commit, has untracked
files, etc):

 % gitbunch check ~/repos

To synchronize bunch to another (will do a 'git pull/push' for each git repo,
and do an rsync for everything else):

 % gitbunch sync ~/repos /mnt/laptop/repos


=head1 DESCRIPTION

A B<gitbunch> or B<bunch> directory is just a term I coined to refer to a
directory which contains, well, a bunch of git repositories. It can also contain
other stuffs like files and non-git repositories (but they must be dot-dirs).
Example:

 repos/            -> a gitbunch dir
   proj1/          -> a git repo
   proj2/          -> ditto
   perl-Git-Bunch/ -> ditto
   ...
   .foo/           -> a non-git dir
   README.txt      -> file

A little bit of history: after B<git> got popular, in 2008 I started using it
for software projects, replacing Subversion and Bazaar. Soon, I moved everything
to git: notes & writings, Emacs .org agenda files, configuration, even temporary
downloads/browser-saved HTML files. Currently, except large media files, all my
personal data resides in git repositories. I put them all in ~/repos (and add
symlinks to various places for convenience). This setup makes it easy to sync to
laptops, backup to disk, etc. Git::Bunch is the library/script I wrote to do
this.

See also L<File::RsyBak>, which I wrote to backup everything else.


=head1 FAQ


=head1 TODO

=over 4

=item * Can't handle bare source repos

=item * Sync-ing to bare repos (e.g. with --backup) still produces error messages

Although the sync process succeeds, the error messages (like C<< fatal: Not a
git repository >>) might be confusing.


=back


=head1 SEE ALSO

B<mr>, http://joeyh.name/code/mr/ . You probably want to use this instead. mr
supports other control version software aside from git, doesn't restrict you to
put all your repos in one directory, supports more operations, and has been
developed since 2007. Had I known about mr, I probably wouldn't have started
Git::Bunch. On the other hand, Git::Bunch is simpler (I think), doesn't require
any config file, and can copy/sync files/directories not under source control. I
mainly use Git::Bunch to quickly: 1) check whether there are any of my
repositories which have uncommitted changes; 2) synchronize (pull/push) to other
locations. I put all my data in one big gitbunch directory; I find it simpler.
Git::Bunch works for me and I use it daily.

=cut
