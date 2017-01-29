#!/usr/bin/env perl
# Reaches to the bosque-UD repository and generates the pt-ud-test.conllu file.
# The only purpose is to obtain their sentence ids.
# Copyright © 2017 Dan Zeman <zeman@ufal.mff.cuni.cz>
# License: GNU GPL

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');

# We assume that the current folder is UD_Portuguese and that bosque-UD is a sibling.
# Read the list of ids of sentences that are part of the test data.
my $listpath = '../bosque-UD/pt-ud-test.txt';
my @idlist;
open(LIST, $listpath) or die("Cannot read $listpath: $!");
while(<LIST>)
{
    s/\r?\n$//;
    push(@idlist, $_);
}
close(LIST);
# Read all sentences into memory. It should not be a problem with data of this size.
my $bpath = '../bosque-UD/documents';
opendir(DIR, $bpath) or die("Cannot read folder $bpath: $!");
my @files = grep {m/^C[FP].*\.conllu$/} (readdir(DIR));
closedir(DIR);
my %hash;
foreach my $file (@files)
{
    my $fpath = "$bpath/$file";
    open(FILE, $fpath) or die("Cannot read $fpath: $!");
    my @sentence = ();
    my $id = undef;
    my @outfile = ();
    while(<FILE>)
    {
        # Other tools expect a different format of sentence ids in the data, so that's what we will output.
        s/sent_id\s*=\s*/sent_id /;
        s/text\s*=\s*/text /;
        push(@sentence, $_);
        if(m/^\#\s*sent_id\s*(.+)/)
        {
            $id = $1;
            $id =~ s/\r?\n$//;
        }
        elsif(m/^\s*$/)
        {
            if(defined($id))
            {
                unless(exists($hash{$id}))
                {
                    my @copy = @sentence;
                    $hash{$id} = \@copy;
                }
                else
                {
                    print STDERR ("Duplicate sentence id '$id'!\n");
                }
            }
            else
            {
                print STDERR ("Unknown sentence id!\n");
            }
            @sentence = ();
            $id = undef;
        }
    }
    close(FILE);
}
my $n = scalar(keys(%hash));
print STDERR ("Read $n sentences.\n");
# Print the required sentences.
foreach my $id (@idlist)
{
    if(exists($hash{$id}))
    {
        print(join('', @{$hash{$id}}));
    }
    else
    {
        print STDERR ("Unknown sentence id '$id'!\n");
    }
}
