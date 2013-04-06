#!/bin/bash

EXE=$1

for VF in *.v 
do	
	$EXE $VF > $VF.output 2> $VF.log
	cat $VF.log
done

echo "Done"
