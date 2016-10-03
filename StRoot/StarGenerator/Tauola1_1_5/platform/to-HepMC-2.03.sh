#!/bin/bash
echo "Convert to HepMC v2.03..."

FILES=( "src/eventRecordInterfaces/TauolaHepMCEvent.cxx" "examples/single_tau_gun_example.c" "examples/taumain_pythia_example.c" )
CHECK=( "688" "134" "140" )
BCPEXT="-toHepMC"

for (( i=0; i<${#FILES[*]}; i++ ))
do
	FILE=${FILES[$i]}
	FILECHECK=${CHECK[$i]}
	BCPFILE=$FILE$BCPEXT
	if [ ! -e $FILE ]
	then
		echo "ERROR: $FILE missing!"
		exit -1
	fi
	if [ -e $BCPFILE ]
	then
		echo "INFO: $BCPFILE exists!"
		echo "      Which means it's already patched. Delete it to force re-run."
		continue
	fi
	cp -f $FILE $BCPFILE
	sed -e 's/.*momentum_unit()/\/\/&/g' $BCPFILE > $FILE-1
	sed -e 's/.*length_unit()/\/\/&/g' $FILE-1 > $FILE-2
	sed -e 's/.*use_units(/\/\/&/g' $FILE-2 > $FILE
	rm -f $FILE-1 $FILE-2

	#Check
	VAR=`diff $BCPFILE $FILE | wc -c`
	if [ $VAR -eq 0 ]
	then
		echo "ERROR: $FILE not modified!"
		echo "       Something is wrong with the script or TAUOLA Interface version!"
		exit -1
	#Verify
	elif [ ! "$VAR" == "$FILECHECK" ]
	then
		echo "WARNING: $FILE version changed."
		echo "         ($VAR!=$FILECHECK) Might not compile correctly."
	else
		echo "INFO: $FILE file patched."
	fi
done
echo "Done."
echo ""
echo "INFO: Converted to HepMC 2.03"
echo "      Be sure to provide input HepMC event record in GeV and mm,"
echo "      and expect that output will be in the same units as well."
echo ""
