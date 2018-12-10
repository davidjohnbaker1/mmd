for i in *.xml
do
xml2hum $i > $i.krn
done

rename 's/\.xml\.krn$/.krn/' *.xml.krn
