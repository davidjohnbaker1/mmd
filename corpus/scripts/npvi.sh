# nPVI
#
# This script implements Grabe and Low (2002) measure of
# "normalized Pairwise Variability Index".  This provides
# a measure of the variability in successive durations in
# a passage.  That is, it measures the durational difference
# between each pair of successive notes relative to the
# the average length of the pair.
# 
#                       m-1
#                        --  | dk - dk+1  |
#             100        \   | ---------  |
# nPVI  =   -------   X   >  | dk + dk+1  |
#            m - 1       /   | ---------  |
#                        --  |     2      |
#                       k=1
#
# where:    m   is the number of successive note-pairs in a passage
#           dk  is the duration of the kth note pair
#

# Change grace notes to 64th notes.  Translate the input to durational
# information (**dur), preserve barlines and rest indicators.

#sed 's/^q/64/' $1 | dur | sed 's/\\//g' | humsed 's/[^]_0-9.r=]//g' | \
sed 's/^q/64/' $1 > /tmp/$$pvi1.tmp
dur /tmp/$$pvi1.tmp > /tmp/$$pvi2.tmp
sed 's/\\//g' /tmp/$$pvi2.tmp > /tmp/$$pvi3.tmp
humsed 's/[^]_0-9.r=]//g' /tmp/$$pvi3.tmp > /tmp/$$pvi4.tmp

# Remove everything else.
#rid -GLId | grep -v =  | \
rid -GLId /tmp/$$pvi4.tmp > /tmp/$$pvi5.tmp
grep -v = /tmp/$$pvi5.tmp > /tmp/$$pvi6.tmp

# Add the duration of ties and rests to the duration of any preceding note.
awk '{
	if ($1 ~ /[]_r]/)
		{
		temp = $1
		gsub("[]_r]","",temp)
		duration += temp
		}
	else	{
		if (duration != 0)
			{
			print duration
			duration = $1
			}
		else duration = $1
		}
	} END {print duration}' /tmp/$$pvi6.tmp > /tmp/$$pvi7.tmp
#	} END {print duration}' | \

# Create duration pairs.
#context -n 2 | \
context -n 2 /tmp/$$pvi7.tmp > /tmp/$$pvi8.tmp

# Calculate the nPVI:
awk '{
	# Each input line represents a pair of durations.
	m++

	if ($1 > $2) dk_minus_dk1 = $1 - $2
	else dk_minus_dk1 = $2 - $1

	dk_plus_dk1 = $1 + $2

	if (dk_plus_dk1 != 0) average = (dk_minus_dk1 / dk_plus_dk1) / 2
	else average = 0

	summation += average

	# Note: In order to make the output values compatible with
	#       Patel and Daniele (2003), the output is multipled by 4.
	#       (Humdrum assumes 1 for whole note; Patel & Daniele define
	#        1 for the quarter duration.)
	} END { if (m != 0) print (100/m) * summation * 4.}' /tmp/$$pvi8.tmp
#	} END { if (m != 0) print (100/m) * summation * 4.}'

rm /tmp/$$pvi*.tmp

