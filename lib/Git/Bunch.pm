package Git::Bunch;
# ABSTRACT: Manage gitbunch directory (directory which contain git repos)

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use File::chdir;
use File::Path qw(make_path);
use String::ShellQuote;

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(check_bunch sync_bunch backup_bunch);

our %SUBS;

$SUBS{check_bunch} = {
    summary       =>
        'Check status of git repositories inside gitbunch directory',
    description   => <<'_',

Will perform a 'git status' for each git repositories inside the bunch and
report which repositories are 'unclean' (e.g. needs commit, has untracked files,
etc).

_
    required_args => [qw/source/],
    args          => {
        source           => ['str*'   => {
            summary      => 'Directory to check',
            arg_pos      => 0,
        }],
    },
    cmdline_suppress_output => 1,
};
sub check_bunch {
    my %args = @_;
    my $source = $args{source} or return [400, "Please specify source"];
    $source =~ s!/+$!!;
    (-d $source) or return [404, "Source doesn't exist"];

    $log->info("Checking bunch $source ...");

    my %res;
    local $CWD = $source;
    for my $repo (grep {-d} <*>) {
        $CWD = $repo;
        $log->debug("Checking repo $repo ...");

        unless (-d ".git") {
            $log->warn("$repo is not a git repo, ".
                           "please remove it or rename to .$repo");
            $res{$repo} = [400, "Not a git repository"];
        };

        my $output = `LANG=C git status 2>&1`;
        my $exit = $? & 255;
        if ($exit == 0 && $output =~ /nothing to commit/) {
            $log->info("$repo is clean");
        } elsif ($exit == 0 &&
                     $output =~ /Changes to be committed|Changed but/) {
            $log->warn("$repo needs commit");
            $res{$repo} = [500, "Needs commit"];
        } elsif ($exit == 0 && $output =~ /Untracked files/) {
            $log->warn("$repo has untracked files");
            $res{$repo} = [500, "Has untracked files"];
        } elsif ($exit == 128 && $output =~ /Not a git repository/) {
            $log->warn("$repo is not a git repo (2)");
            $res{$repo} = [500, "Not a git repo (2)"];
        } else {
            $log->error("Can't figure out result of 'git status' ".
                            "for repo `$repo`: exit=$exit, output=$output");
            $res{$repo} = [500, "Unknown (exit=$exit, output=$output)"];
        }
        $CWD = "..";
    }
    [200, "OK", \%res];
}

$SUBS{backup_bunch} = {
    summary       =>
        'Backup bunch directory to another directory using rsync',
    description   => <<'_',

Simply uses rsync to copy bunch directory to another, except that for all git
projects, only .git/ will be rsync-ed. This utilizes the fact that .git/
contains the whole project's data, the working copy can be checked out from
.git/.

Will run check_bunch first and require all repos to be clean before running the
backup, unless 'check' is turned off.

Note: Saving only .git/ subdirectory saves disk space, but will not save
uncommited changes, untracked files, or .gitignore'd files. Make sure you have
committed everything to git before doing backup. Also note that if you need to
restore files, they will be checked out from the repository, and the original
ctime/mtime information is not preserved. backup_bunch() does store this
information for you by saving the output of 'ls -laR' command, but have *not*
implemented routine to restore this data into restored files.

_
    required_args => [qw/source target/],
    args          => {
        source           => ['str*'   => {
            summary      => 'Directory to backup',
            arg_pos      => 0,
        }],
        target           => ['str*'   => {
            summary      => 'Backup destination',
            arg_pos      => 1,
        }],
        check            => ['bool'   => {
            summary      =>
                'Whether to check bunch first before doing backup',
            default      => 1,
        }],
        backup           => ['bool'   => {
            summary      => 'Whether to do actual backup/rsync',
            description  => <<'_',

You can set backup=0 and index=1 to only run indexing, for example.

_
            default      => 1,
        }],
        index            => ['bool'   => {
            summary      => 'Whether to do "ls -laR" after backup',
            default      => 1,
        }],
        extra_rsync_opts => [array    => {
            of           => 'str*',
            summary      => 'Pass extra options to rsync command',
            description  => <<'_',

Extra options to pass to rsync command. Note that the options will be shell
quoted, , so you should pass it unquoted, e.g. ['--exclude', '/Program Files'].

_
        }],
    },

    cmdline_examples => [
        {
            cmd         => '/home/steven/repos /backup/steven/repos --nocheck',
            description => <<'_',

Backup /home/steven/repos to /backup/steven/repos. All git projects inside
/home/steven/repos will be backed up by only copying its .git/, all non-git
directories will be backed up in its entirety. Do not run check_bunch() first.

_
        },
    ],
};
sub backup_bunch {
    my %args = @_;

    # XXX schema
    my $source    = $args{source} or return [400, "Please specify source"];
    $source =~ s!/+$!!;
    (-d $source) or return [404, "Source doesn't exist"];
    my $target    = $args{target} or return [400, "Please specify target"];
    $target       =~ s!/+$!!;
    my $check     = $args{check}  // 1;
    my $backup    = $args{backup} // 1;
    my $index     = $args{index}  // 1;

    my $res;
    if ($check) {
        $res = check_bunch(source => $source);
        return $res unless $res->[0];
        return [500, "Some repos are not clean, please fix first"]
            if keys %{$res->[2]};
    }

    unless (-d $target) {
        $log->debugf("Creating target directory %s ...", $target);
        make_path($target)
            or return [500, "Can't create target directory $target: $!"];
    }

    if ($backup) {
        $log->info("Backing up bunch $source ===> $target ...");
        my $cmd = join(
            "",
            "rsync -az ",
            ($log->is_trace() ? "-Pv" : ($log->is_debug() ? "-v" : "")), " ",
            "--include / ",
            "--include '/*' --include '/*/.git' --include '/*/.git/**' ",
            #"--include '/.remote' --include '/.remote/**' ",
            "--exclude '*' ",
            "--del --force --delete-excluded ",
            shell_quote($source), "/ ",
            shell_quote($target), "/"
        );
        $log->trace("system(): $cmd");
        system $cmd;
        return [500, "Backup did not succeed, please check: $!"] if $?;
    }

    if ($index) {
        $log->info("Indexing bunch $source ...");
        local $CWD = $source;
        my $cmd = "( ls -laR | gzip -c > .ls-laR.gz ) && ".
            "cp .ls-laR.gz ".shell_quote($target);
        $log->trace("system(): $cmd");
        return [500, "Indexing did not succeed, please check: $!"] if $?;
    }

    [200, "OK"];
}

1;
__END__

=head1 SYNOPSIS

To check the status of bunch (will do a 'git status' for each git repo inside
the bunch and report which repos are 'unclean', e.g. needs commit, has untracked
files, etc):

 % gitbunch check ~/repos

To synchronize bunch to another (will do a 'git pull' for each git repo, and do
an rsync for everything else):

 % gitbunch sync ~/repos /mnt/laptop/repos

To backup bunch (will only rsync .git/ for each git repo to destination, and
rsync everything else in full):

 % gitbunch backup ~/repos /media/flashdisk


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


=head1 FUNCTIONS

None of the functions are exported by default, but they are exportable.


=head1 FAQ


=head1 TODO


=head1 SEE ALSO

=cut

