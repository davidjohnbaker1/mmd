#!/usr/bin/perl
#
# Description: Adds reference records to the start of a Humdrum file.
# The directory is found in the first column.
# The Humdrum filename is found in the second column.  
# The first row contains the reference record keys (starting in the third col.)
#

use strict;

die "Usage $0 tsvfile" if @ARGV != 1;
my $tsvfile = $ARGV[0];

open(FILE, $tsvfile) or die "Cannot read $tsvfile";
my @spreadsheet = <FILE>;
close FILE;

for (my $i=1; $i<@spreadsheet; $i++) {
   processEntry($spreadsheet[0], $spreadsheet[$i]);
}

sub processEntry {
   my ($firstrow, $entry) = @_;
   chomp $entry;
   chomp $firstrow;
   my @heading = split(/\t/, $firstrow);
   my @data    = split(/\t/, $entry);
   my $directory = $data[0];
   my $filename  = $data[1];
   if (!-r "$directory/$filename") {
      print "WARNING: Cannot find $filename, skipping...\n";
      next;
   }
   open (HFILE, "$directory/$filename") or die "Cannot read $filename";
   my @contents = <HFILE>;
   close HFILE;
   open (HFILE, ">$directory/$filename") or die "Cannot write $filename";
   my $startline;
   for (my $i=2; $i<@data; $i++) {
      print HFILE "!!!$heading[$i]:\t$data[$i]\n";
   }
   print HFILE @contents;
   close HFILE;
}
