# Corpus Documentation

This document monitors the progress of the encoding of the Berkowitz corpus 

Files are originally encoded in ```.xml``` via MuseScore 2.
From there, files were then either batch covereted to kern using the hum2krn command line tool of Humdrum Extras or batch coverted to MIDI with the MuseScore 2 batch coverter plug-in.
From MIDI, they were coverted to ```.csv``` with Klaus' [Melconv] command line tool.

The ```.csv``` files can be used for Feature Analysis with FANTASTIC.
The ```.krn``` files are used for the IDyOM modeling.

Scripts to do all the conversions are found in the scripts directory.


