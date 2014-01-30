#!./perl

BEGIN {
    $| = 1;
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
    skip_all_if_miniperl();
    require Config; import Config;

    $Config{useithreads} && $Config{i_pthread}
        or skip_all("No pthreads or no useithreads");
}

use threads;
use strict;
use XS::APItest;

watchdog(60);

my $got_int = 0;
$SIG{INT} = sub { ++$got_int };

# signals across threads
# if the child doesn't set a handler, it's delivered to the parent
{

    my $thread = threads->create(
       sub {
	 sleep 5;
       });
    XS::APItest::pthread_kill($thread->_handle, "INT");
    $thread->join();
    is($got_int, 1, "signal handled by main thread");
}

