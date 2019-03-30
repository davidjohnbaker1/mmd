for i in *.krn
do
	solfa -x $i | rid -GLId | grep -v "=" > $i.solfa
done
