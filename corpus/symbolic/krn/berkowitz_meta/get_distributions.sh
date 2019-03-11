solfa -x *.krn | rid -GLId | grep -v "r" | grep -v "=" | context -n 2 | sortcount| head -n 1000 > bi_grams.tsv
solfa -x *.krn | rid -GLId | grep -v "r" | grep -v "=" | context -n 3 | sortcount| head -n 1000 > tri_grams.tsv
solfa -x *.krn | rid -GLId | grep -v "r" | grep -v "=" | context -n 5 | sortcount| head -n 1000 > quint_grams.tsv
