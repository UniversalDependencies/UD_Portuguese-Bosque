#!/usr/bin/env perl

# Converts from UD to Freeling EAGLES PT

# Based on the following script: coll_convert_tags_to_uposf.pl, with this description:

# Reads CoNLL-X from STDIN, converts CPOS+POS+FEAT to the universal POS and features, writes the result to STDOUT.
# The output contains the universal POS tag in the CPOS column and the universal features in the FEAT column.
# The POS column is copied over from the input.
# Copyright © 2015 Dan Zeman <zeman@ufal.mff.cuni.cz>
# License: GNU GPL

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');
use Getopt::Long;
use Lingua::Interset::Converter;

use lib '.';
use freeling;

my $FREELINGDIR = "/usr/local";
my $DATA = $FREELINGDIR."/share/freeling/";
my $LANG="pt";
my $PUNCT = $DATA."common/punct.dat";

my $ts = new freeling::tagset($DATA.$LANG."/tagset.dat");

my $tagset1 = 'pt::freeling';

my $c = new Lingua::Interset::Converter ('to' => $tagset1, 'from' => 'mul::uposf');

my %punct_conversion;

open(my $ph, '<:encoding(UTF-8)', $PUNCT) or die "Could not open '$PUNCT' $!";
while (my $row = <$ph>)
{
    chomp $row;
    my @punct = split ' ', $row;
    $punct_conversion{$punct[1]} = $punct[2];
}
close($ph);

# Read the CoNLL-U file from STDIN or from files given as arguments.
while(<>)
{
    # Skip comment lines before sentences.
    # Skip empty lines after sentences.
    # Skip initial lines of multi-word tokens.
    unless(m/^#/ || m/^\s*$/ || m/^\d+-\d/)
    {
        chomp();
        my @f = split(/\t/, $_);
	# print STDERR scalar @f;
	# print STDERR "\n";

        if (scalar @f eq 10)
        {
            my $tag = "$f[3]\t$f[5]";
            my $lemma = $f[2];
            my $ftag1 = $c->convert($tag);
            # print STDERR "< $tag # $ftag1\n";

            if ($ftag1 eq 'Fz')
            {
                $ftag1 = $punct_conversion{$lemma}
                if exists $punct_conversion{$lemma};
            }

            my $msd = $ts->get_msd_string($ftag1);
            my $ftag2 = $ts->msd_to_tag('', $msd);

            if ($ftag2 =~ /^NP/)
            {
                $ftag2 = 'NP00000';
            }

            my $short = $ts->get_short_tag($ftag2);

            if ($ftag2 eq 'Z0') { $ftag2 = 'Z'; }
            if ($short eq 'Z0') { $short = 'Z'; }
            
            $f[3] = $short;
            $f[4] = $ftag2;
            $f[5] = $msd;
        }

        $_ = join("\t", @f)."\n";
    }
    # Write the modified line to the standard output.
    print();
}
