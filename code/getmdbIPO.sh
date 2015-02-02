#!/bin/bash
tmpcsv=`mktemp -t ipo_XXXX.csv`
echo Exporting mdb to csv ...
time mdb-export -d\; -Q -D "%d/%m/%y"  IPOStatPRO.mdb IPOStatPro > IPOStatPro.csv
echo Removing junk symbols ...
sed s/\(//g IPOStatPro.csv > $tmpcsv
sed s/\)//g $tmpcsv > IPOStatPro.csv 
