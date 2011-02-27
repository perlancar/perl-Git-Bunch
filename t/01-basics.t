#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.96;

use File::chdir;
use File::Temp qw(tempdir);
use File::Which;
use Git::Bunch qw(check_bunch sync_bunch backup_bunch);
use Probe::Perl;

for (qw(git rsync)) {
    plan skip_all => "$_ not available in PATH" unless which($_);
}
my $pp = Probe::Perl->new;
plan skip_all => 'currently only test on Unix'
    unless $pp->os_type eq 'Unix';

# XXX setup test data

test_gb(
    sub     => "check_bunch",
    name    => "all repos in bunch are clean",
    args    => {source=>"t/data/bunch1"},
    status  => 200,
);
test_gb(
    sub     => "check_bunch",
    name    => "handling / suffix in bunch name",
    args    => {source=>"t/data/bunch1/"},
    status  => 200,
);
test_gb(
    sub     => "check_bunch",
    name    => "bunch doesn't exist",
    args    => {source=>"t/data/bunch1x"},
    status  => 404,
);
test_gb(
    sub     => "check_bunch",
    name    => "using repo instead of bunch will be rejected",
    args    => {source=>"t/data/bunch1/repo1"},
    status  => 400,
);
# XXX there is nondot, nongitdir -> error
# XXX all clean
# XXX untracked files
# XXX needs commit
exit;

SKIP: {
    for (qw(ls gzip)) {
        plan skip_all => "$_ not available in PATH" unless which($_);
    }

    test_gb(
        sub     => "backup_bunch",
        name    => "handling / suffix in source bunch name",
        args    => {source=>"t/data/bunch1/", target=>"XXX"},
        status  => 200,
    );
    test_gb(
        sub     => "backup_bunch",
        name    => "source bunch doesn't exist",
        args    => {source=>"t/data/bunch1x", target=>"XXX"},
        status  => 404,
    );
    test_gb(
        sub     => "backup_bunch",
        name    => "using repo instead of bunch in source will be rejected",
        args    => {source=>"t/data/bunch1/repo1", target=>"XXX"},
        status  => 400,
    );
    # XXX .ls-laR exists
    # XXX files outside .git/ not copied
    # XXX 'git log' on backup repo works
    # XXX update
}

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
done_testing();

sub test_gb {
    my (%args) = @_;
    my $sub_args = $args{args};

    subtest $args{name} => sub {

        my $res;
        my $sub = $args{sub};
        if ($sub eq 'check_bunch') {
            $res = check_bunch(%$sub_args);
        } elsif ($sub eq 'backup_bunch') {
            $res = backup_bunch(%$sub_args);
        } elsif ($sub eq 'sync_bunch') {
            $res = synch_bunch(%$sub_args);
        } else {
            die "Unknown sub to test: $sub";
        }

        if ($args{status}) {
            is($res->[0], $args{status}, "status $args{status}");
        }

    };
}

