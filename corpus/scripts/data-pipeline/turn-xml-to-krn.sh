for i in *.xml
do
xml2hum $i > $i.krn
printf "Doing XML for $i \n"
done

for i in *.musicxml
do
xml2hum $i > $i.krn
printf "Doing MusicXML for $i \n"
done


rename 's/\.xml\.krn$/.krn/' *.xml.krn
rename 's/\.musicxml\.krn$/.krn/' *.musicxml.krn
