#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use File::chdir;
use File::Slurp::Shortcuts qw(slurp_cq write_file);
use File::Temp qw(tempdir);
use File::Which;
use Git::Bunch qw(check_bunch sync_bunch backup_bunch);
use Probe::Perl;
use String::ShellQuote;

for (qw(git rsync rm)) {
    plan skip_all => "$_ not available in PATH" unless which($_);
}
# due to shell quoting etc
my $pp = Probe::Perl->new;
plan skip_all => 'currently only test on Unix'
    unless $pp->os_type eq 'Unix';

my $rootdir = tempdir(CLEANUP=>1);
$CWD = $rootdir;
create_test_data($rootdir);

test_gb(
    sub     => "check_bunch",
    name    => "all repos in bunch are clean",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is( $res->[2]{repo1}[0], 200, "repo1 is clean");
        is( $res->[2]{repo2}[0], 200, "repo2 is clean");
        ok(!$res->[2]{"file1"}, "file is skipped");
        ok(!$res->[2]{".nonrepo1"}, "dotdir is skipped");
    },
);
test_gb(
    sub     => "check_bunch",
    name    => "handling / suffix in bunch name",
    args    => {source=>"src/bunch1/"},
    status  => 200,
);
test_gb(
    sub     => "check_bunch",
    name    => "bunch doesn't exist",
    args    => {source=>"src/bunch1x"},
    status  => 404,
);
test_gb(
    sub     => "check_bunch",
    name    => "using repo instead of bunch will be rejected",
    args    => {source=>"src/bunch1/repo1"},
    status  => 400,
);

mkdir "src/bunch1/nonrepo2";
test_gb(
    sub     => "check_bunch",
    name    => "reject nondot, nongit dir",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is( $res->[2]{nonrepo2}[0], 400, "nonrepo2 is rejected");
    },
);
rmdir "src/bunch1/nonrepo2";

write_file("src/bunch1/repo1/a", "");
test_gb(
    sub     => "check_bunch",
    name    => "needs commit",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is  ($res->[2]{repo1}[0], 500, "repo1 is unclean (status)");
        like($res->[2]{repo1}[1], qr/needs commit/i,
             "repo1 is unclean (message)");
    },
);
system "cd src/bunch1/repo1 && git commit -am 'commit2-repo1'";
test_gb(
    sub     => "check_bunch",
    name    => "needs commit (committed)",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is( $res->[2]{repo1}[0], 200, "repo1 is clean again");
    },
);

write_file("src/bunch1/repo1/c", "cherry");
test_gb(
    sub     => "check_bunch",
    name    => "has untracked files",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is  ($res->[2]{repo1}[0], 500, "repo1 is unclean");
        like($res->[2]{repo1}[1], qr/has untracked files/i,
             "repo1 is unclean (message)");
    },
);
unlink "src/bunch1/repo1/c";
test_gb(
    sub     => "check_bunch",
    name    => "has untracked files (deleted)",
    args    => {source=>"src/bunch1"},
    status  => 200,
    test_res => sub {
        my ($res) = @_;
        is( $res->[2]{repo1}[0], 200, "repo1 is clean again");
    },
);

mkdir "src/bunch1/repo3", 0;
test_gb(
    sub     => "check_bunch",
    name    => "can't chdir to repo -> dies",
    args    => {source=>"src/bunch1"},
    dies    => 1,
);
rmdir "src/bunch1/repo3";

SKIP: {
    for (qw(ls gzip)) {
        plan skip_all => "skipping backup_bunch tests because ".
            "$_ is not available in PATH" unless which($_);
    }

    test_gb(
        sub     => "backup_bunch",
        name    => "source bunch doesn't exist",
        args    => {source=>"src/bunch1x", target=>"bak"},
        status  => 404,
    );
    test_gb(
        sub     => "backup_bunch",
        name    => "using repo instead of bunch in source will be rejected",
        args    => {source=>"src/bunch1/repo1", target=>"bak"},
        status  => 400,
    );

  TODO: {
        local $TODO = "todo";
        fail("refuse to backup when there are unclean repos");
        fail("force backup unclean repos with --nocheck");
    }

    test_gb(
        sub     => "backup_bunch",
        name    => "main test", # also tests handling / suffix in src & target
        args    => {source=>"src/bunch1/", target=>"bak/1/"},
        status  => 200,
        test_res => sub {
            my ($res) = @_;
            ok((-d "bak/1"), "target directory created") or return;
            is(slurp_cq("bak/1/file1"), "foo", "files copied");
            ok((-d "bak/1/.nonrepo1"), "nongit dotdir copied (exists)");
            is(slurp_cq("bak/1/.nonrepo1/t"), "tea",
               "nongit dotdir copied (content)");
            for my $repo (qw(repo1 repo2)) {
                ok( (-d "bak/1/$repo"), "repo $repo copied (exists)");
                ok( (-d "bak/1/$repo/.git"), "repo $repo copied (.git exists)");
                ok(!(-e "bak/1/$repo/file1"),
                   "repo $repo copied (working copy not copied)");
                like(`cd bak/1/$repo && git log`, qr/commit1-$repo/i,
                     "repo $repo copied (git log works)");
            }
        },
    );
  TODO: {
        local $TODO = "todo";
        fail("update backup");
    }
}
goto DONE_TESTING;

test_gb(
    sub     => "sync_bunch",
    name    => "handling / suffix in source bunch name",
    args    => {source=>"t/data/bunch1/", target=>"XXX"},
    status  => 200,
);
test_gb(
    sub     => "sync_bunch",
    name    => "source bunch doesn't exist",
    args    => {source=>"t/data/bunch1x", target=>"XXX"},
    status  => 404,
);
test_gb(
    sub     => "sync_bunch",
    name    => "using repo instead of bunch in source will be rejected",
    args    => {source=>"t/data/bunch1/repo1", target=>"XXX"},
    status  => 400,
);
test_gb(
    sub     => "sync_bunch",
    name    => "using repo instead of bunch in target will be rejected",
    args    => {source=>"t/data/bunch1", target=>"XXX"},
    status  => 400,
);
# XXX update

DONE_TESTING:
done_testing();
if (Test::More->builder->is_passing) {
    diag "all tests successful, deleting test data dir";
    $CWD = "/";
} else {
    # don't delete test data dir if there are errors
    diag "there are failing tests, not deleting test data dir $rootdir";
}

sub test_gb {
    my (%args) = @_;
    my $sub = $args{sub};
    my $sub_args = $args{args};

    subtest "$sub: $args{name}" => sub {

        my $res;
        my $eval_err;
        if ($sub =~ /^(check|backup|sync)_bunch$/) {
            no strict 'refs';
            eval { $res = $sub->(%$sub_args) }; $eval_err = $@;
        } else {
            die "Unknown sub to test: $sub";
        }

        if ($args{dies}) {
            ok($eval_err, "dies");
        }
        if ($args{status}) {
            is($res->[0], $args{status}, "status $args{status}");
        }
        if ($args{test_res}) {
            $args{test_res}->($res);
        }

    };
}

sub create_test_data {
    die unless $rootdir;
    local $CWD = $rootdir;
    use autodie;

    mkdir      "src";
    mkdir      "src/bunch1";
    mkdir      "src/bunch1/.nonrepo1";
    write_file "src/bunch1/.nonrepo1/t", "tea";
    write_file "src/bunch1/file1", "foo";

    mkdir      "src/bunch1/repo1";
    write_file "src/bunch1/repo1/a", "apple";
    mkdir      "src/bunch1/repo1/d";
    write_file "src/bunch1/repo1/d/b", "banana";
    $CWD     = "src/bunch1/repo1";
    system     "git init";
    system     "git add .";
    system     "git commit -am 'commit1-repo1'";
    $CWD     = "../../..";

    mkdir      "src/bunch1/repo2";
    write_file "src/bunch1/repo2/a", "avocado";
    mkdir      "src/bunch1/repo2/d";
    write_file "src/bunch1/repo2/d/b", "blueberry";
    $CWD     = "src/bunch1/repo2";
    system     "git init";
    system     "git add .";
    system     "git commit -am 'commit1-repo2'";
    write_file   "a", "anggur";
    system     "git commit -am 'commit2-repo2'";
    $CWD     = "../../..";

    mkdir      "dest";

    #mkdir      "bak";
}

sub delete_test_data {
    die unless $rootdir;
    my @dirs = @_ ? @_ : ("src", "dest", "bak");
    system "rm -rf ".join(" ", map {shell_quote("$rootdir/$_")} @dirs);
}
