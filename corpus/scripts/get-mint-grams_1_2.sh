mint *.krn | rid -GLId | grep -v "="| grep -v '\[' | grep -v 'P'| grep -v 'r' | context -n $1 | sortcount| head -n $2


first make all Solfa files of interest and save them 
then run infot -bH on all the experimental files 

then combine them all into strings 