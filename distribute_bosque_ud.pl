#!/usr/bin/env perl
# Distributes my version of Bosque to the files in bosque-UD so that a pull request for Alexandre can be created.
# Copyright © 2017 Dan Zeman <zeman@ufal.mff.cuni.cz>
# License: GNU GPL

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');

# We assume that the current folder is UD_Portuguese and that bosque-UD is a sibling.
# Read all sentences into memory. It should not be a problem with data of this size.
@ARGV = ('pt-ud-train.conllu', 'pt-ud-dev.conllu', 'pt-ud-test.conllu');
my %hash;
my @sentence = ();
my $id;
while(<>)
{
    push(@sentence, $_);
    if(m/^\#\s*sent_id\s*=\s*(.+)/)
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
my $n = scalar(keys(%hash));
print("Read $n sentences.\n");
# Now process all CF*.conllu and CP*.conllu files in the bosque-UD/documents/old folder.
# Leave unmatched sentences as they are (but report them!)
# Replace matching sentences with my version from memory.
my $bpath = '../bosque-UD/documents/old';
opendir(DIR, $bpath) or die("Cannot read folder $bpath: $!");
my @files = grep {m/^C[FP].*\.conllu$/} (readdir(DIR));
closedir(DIR);
foreach my $file (@files)
{
    my $fpath = "$bpath/$file";
    open(FILE, $fpath) or die("Cannot read $fpath: $!");
    @sentence = ();
    $id = undef;
    my @outfile = ();
    while(<FILE>)
    {
        push(@sentence, $_);
        if(m/^\#\s*new_sent_id\s*=\s*(.+)/)
        {
            $id = $1;
            $id =~ s/\r?\n$//;
        }
        elsif(m/^\s*$/)
        {
            if(defined($id))
            {
                if(exists($hash{$id}))
                {
                    push(@outfile, @{$hash{$id}});
                }
                else
                {
                    print STDERR ("Unmatched sentence id '$id'!\n");
                    push(@outfile, @sentence);
                }
            }
            else
            {
                print STDERR ("Unknown sentence id!\n");
                push(@outfile, @sentence);
            }
            @sentence = ();
            $id = undef;
        }
    }
    close(FILE);
    # Write the new contents of the file.
    open(FILE, ">$fpath") or die("Cannot write $fpath: $!");
    print FILE (join('', @outfile));
    close(FILE);
}
