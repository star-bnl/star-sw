#!/bin/sh

FSET=$1
DIR=`pwd`
SUBMIT_DIR=Submit

if [ "$FSET" == "" ]; then
	echo "Usage: $0 <FSET>"
	exit -1
fi

if [ $FSET  -eq $FSET 2> /dev/null ]; then
	echo "FSET $FSET"
	else
	echo "FSET is not a number (enter, e.g., \"100\"). "
	exit -2
fi

if [ ! -d $SUBMIT_DIR ]; then
	echo "Error, directory \"$SUBMIT_DIR\" containing XML files does not exist, Abort!"
	exit -3
	else 
	echo "Submitting files in directory \"$SUBMIT_DIR\""
fi

cd $SUBMIT_DIR

#LIST=`ls -l  | grep '^d' | grep "PtHard_[0-9]" | rev | cut -f1 -d" " | rev`
LIST=`ls`
for LINE in $LIST; do
	echo ""
	echo "In directory $LINE..."
	cd $LINE
	PTMIN=`echo $LINE | cut -f2 -d"_"`
	PTMAX=`echo $LINE | cut -f3 -d"_"`
	XML=`ls *.xml`
	if [ ! -e $XML ]; then
		echo "     Error, XML file $XML does not exist in folder $LINE, Skip!"
		continue
		echo ""
	fi
	echo "     star-submit-template -template ${XML} -entities FSET=$FSET"
	echo ""
#	star-submit-template -template ${XML} -entities FSET=$FSET
	cd $DIR/$SUBMIT_DIR
done

exit 0
