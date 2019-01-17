for i in *.krn
do
	printf "Working on $i\n"
	sed -i '' '/\*met(c)/d' $i 
	sed -i '' '/\*met(c|)/d' $i 
done

