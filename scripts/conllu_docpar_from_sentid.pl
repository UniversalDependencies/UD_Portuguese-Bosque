#!/usr/bin/env perl
# Generates document and paragraph boundary tags from sentence ids in CoNLL-U files.
# If there are any document/paragraph tags already in the input, they will be removed.
# Assumes sentence id format as in the Prague Dependency Treebank:
# cmpr9406-001-p2s1
# ln94200-100-p1s1
# mf920901-001-p1s1A
# vesm9211-001-p1s1
# Czech Academic Corpus:
# a01w-s1
# Prague Arabic Dependency Treebank:
# afp.20000715.0001:p2u1
# Copyright © 2017 Dan Zeman <zeman@ufal.mff.cuni.cz>
# License: GNU GPL

use utf8;
use open ':utf8';
binmode(STDIN, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDERR, ':utf8');
use Getopt::Long;

# Normally we use sent_id to detect new documents and paragraphs.
# PROIEL treebanks have the source attribute instead.
# However, source must be explicitly asked for, because Portuguese has both source and sent_id, and we do not want two newdocs before one sentence.
my $source = 0;
GetOptions
(
    'source' => \$source
);

my $current_did = '';
my $current_pid = '';
while(<>)
{
    s/\r?\n$//;
    # Ignore any document or paragraph tags in the input.
    # (We work only with sentence-leve paragraph boundaries. There could be also the NewPar=Yes MISC attribute, but not in PDT.)
    if(m/^\#\s*new(doc|par)(\s|$)/)
    {
        next;
    }
    # All sentences should have a sent_id attribute, which includes the ids of the current document and paragraph.
    # Compare it to the sent_id of the previous sentence.
    if(m/^\#\s*sent_id\s*=\s*(.+)/)
    {
        my $sid = $1;
        if($sid =~ m/^((.+)-p\d+)s[-0-9A-Z]+$/)
        {
            my $pid = $1;
            my $did = $2;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            if($pid ne $current_pid)
            {
                print("# newpar id = $pid\n");
                $current_pid = $pid;
            }
            print("# sent_id = $sid\n");
        }
        # Czech Academic Corpus
        elsif($sid =~ m/^(.+)-s[0-9A-Z]+$/)
        {
            my $did = $1;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            print("# sent_id = $sid\n");
        }
        # Prague Arabic Dependency Treebank
        elsif($sid =~ m/^((.+):p\d+)u[0-9A-Z]+$/)
        {
            my $pid = $1;
            my $did = $2;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            if($pid ne $current_pid)
            {
                print("# newpar id = $pid\n");
                $current_pid = $pid;
            }
            print("# sent_id = $sid\n");
        }
        # Slovenian UD treebank
        elsif($sid =~ m/^((ssj\d+)\.\d+)\.\d+$/)
        {
            my $pid = $1;
            my $did = $2;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            if($pid ne $current_pid)
            {
                print("# newpar id = $pid\n");
                $current_pid = $pid;
            }
            print("# sent_id = $sid\n");
        }
        # Ancient Greek Dependency Treebank
        # tlg0008.tlg001.perseus-grc1.13.tb.xml@1163
        elsif($sid =~ m/^(.+)\@\d+$/)
        {
            my $did = $1;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            print("# sent_id = $sid\n");
        }
        # Greek Dependency Treebank
        # gdt-20120321-elwikinews-5251-1
        elsif($sid =~ m/^(.+)-\d+$/)
        {
            my $did = $1;
            if($did ne $current_did)
            {
                print("# newdoc id = $did\n");
                $current_did = $did;
            }
            print("# sent_id = $sid\n");
        }
        else
        {
            # Do not complain about PROIEL sentence ids. They are plain integers
            # but in PROIEL we have document ids from the source attribute.
            unless($sid =~ m/^\d+$/ && $current_did ne '')
            {
                print STDERR ("Unexpected sentence id '$sid'\n");
            }
            print("# sent_id = $sid\n");
        }
    }
    # PROIEL treebanks have an attribute called "source". When it changes we have a new document.
    elsif($source && m/^\#\s*source\s*=\s*(.+)/)
    {
        my $did = $1;
        if($did ne $current_did)
        {
            my $xdid = $did;
            $xdid =~ s/\s+/_/g;
            print("# newdoc id = $xdid\n");
            $current_did = $did;
        }
        print("# source = $did\n");
    }
    # SynTagRus has an unnamed comment of the form "# 2003Anketa.xml 1", which contains file name (document) and sentence number.
    elsif($source && m/^\#\s*((.+\.xml)\s+\d+)$/i)
    {
        my $dsid = $1;
        my $did = $2;
        if($did ne $current_did)
        {
            my $xdid = $did;
            $xdid =~ s/\s+/_/g;
            print("# newdoc id = $xdid\n");
            $current_did = $did;
        }
        print("# source = $dsid\n");
    }
    else
    {
        print("$_\n");
    }
}
