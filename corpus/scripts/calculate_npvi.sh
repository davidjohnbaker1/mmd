for i in *.krn
do
	npvi=$(./npvi.sh $i )
	printf "Now working on $i\t$npvi\n"
done
