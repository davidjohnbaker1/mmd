for i in *.krn
do
	 awk 'NR==5{print "XX"}1' $i
done
