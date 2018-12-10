# How To Update Music Data 

1. Put new XML into ```data-pipeline```
2. Run ```turn-xml-to-krns.sh```
3. Run ```move-krns.sh```
4. Open MuseScore 2
5. Run Batch Convert chosing xml to midi on new files
6. Run ```move-midis.sh```
7. Run ```make-csvs.sh```
8. Run ```move-xmls.sh```
9. Open ```corpus.Rproj```
10. Run ```make-fantastic-features.R```
