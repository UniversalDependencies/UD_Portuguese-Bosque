#!/usr/bin/env perl

# cat bosque-ud/*.udep.conll | perl test-conversion.pl &> error

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');
use Getopt::Long;
use Lingua::Interset::Converter;

use lib '.';
use freeling;

my $FREELINGDIR = "/home/fcbr/bin/freeling-4.0";
my $DATA = $FREELINGDIR."/share/freeling/";
my $LANG="pt";

my $ts = new freeling::tagset($DATA.$LANG."/tagset.dat");

my $tagset1 = 'pt::freeling';

my $c = new Lingua::Interset::Converter ('to' => $tagset1, 'from' => 'mul::uposf');

# Read the CoNLL-U file from STDIN or from files given as arguments.
while(<>)
{
    unless(m/^#/ || m/^\s*$/ || m/^\d+-\d/)
    {
        chomp();
        my @f = split(/\t/, $_);

        if (scalar @f eq 8)
        {
            my $tag = "$f[3]\t$f[5]";

            my $ftag1 = $c->convert($tag);
            print "\n\n";
            print "< tag = $tag, dan = $ftag1\n";

            my $msd = $ts->get_msd_string($ftag1);
            my $ftag2 = $ts->msd_to_tag('', $msd);
            print "\n";
            print "> msd = $ftag2, eag = $msd\n";
            $f[3] = $ftag2;
            $f[5] = $msd;
        }

        $_ = join("\t", @f)."\n";
    }
    # Write the modified line to the standard output.
    # print();
}
