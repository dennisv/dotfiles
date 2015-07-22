#!/usr/bin/perl
use Capture::Tiny ':all';

map {
    my $file = $_;
    my $d = 0;

    my ($stdout, $stderr, @result) = capture {
    `ffmpeg -i $file -f null /dev/null 2>&1`;
    };

    map {
    my $goo = $_;
    my ($frames, $dupes) = (0,0);
    $frames = $1 if $goo =~ /frame=\s*(\d+)/;
    $dupes = $1 if $goo =~ /dup=\s*(\d+)/;
    $d = $frames - $dupes - 1 if $frames > 0;
    } @result;

    my $image = $file . "_LAST_$d.jpg";
    `ffmpeg -i $file -vf "select='eq(n,$d)'" -vframes 1 $image` if $d;
} @ARGV;
