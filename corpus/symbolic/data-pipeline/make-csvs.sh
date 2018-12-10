cd ../midi
ls *.mid | melconv -f csv -s -I
mv *.csv ../csv
