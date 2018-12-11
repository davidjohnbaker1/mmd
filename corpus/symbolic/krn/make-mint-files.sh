for i in *.krn
do
printf "working on $i"
mint $i | grep -v "=" | grep -v "r" | rid -GLId  > $i.mnt
done

rename 's/\.krn\.mnt$/.mnt/' *.krn.mnt ## Renames extension

