for i in *.krn
do
printf "Working on $i \n"
hum2muse $i | muse2ps | ps2pdf - - > $i.pdf
done

rename 's/\.krn\.pdf$/.pdf/' *.pdf ## Renames extension

for k in *.pdf
do
printf "Now cropping $k \n"
pdfcrop --margins 10 $k $k
done

for j in *.pdf
do
printf "Now Cropping $j \n"
sips -s format png $j --out $j.png
done

rename 's/\.pdf\.png$/.png/' *.png ## Renames extension
rm *.pdf

