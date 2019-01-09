mint *.krn | rid -GLId | grep -v "="| grep -v '\[' | grep -v 'P'| grep -v 'r' | context -n $1 | sortcount| head -n $2
