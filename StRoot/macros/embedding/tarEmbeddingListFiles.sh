#!/bin/bash

XML=$1
FSET_MIN=$2
FSET_MAX=$3

#Check input arguments
if [ "$XML" == "" ]; then
	echo "Error, Usage: <XML> (enter, e.g., \"embed_template_P10ih.xml\")."
	echo "ABORT"
	exit -1
fi
if [ "$FSET_MIN" == "" ]; then
	echo "Error, Usage: <FSET Range Minimum> (enter, e.g., \"100\")."
	echo "ABORT"
	exit -1
fi
if [ "$FSET_MAX" == "" ]; then
	echo "Error, Usage: <FSET Range Maximum> (enter, e.g., \"201\")."
	echo "ABORT"
	exit -1
fi
#___________________________________________________________________________________#

#Make sure input parameters exist, or are of correct form
if [ ! -e "$XML" ]; then
	echo "Error, xml file \"$XML\" does NOT exist!"
	echo "ABORT"
	exit -2
else
	echo "Extracting information from XML file $XML"
fi
if [ $FSET_MIN  -eq $FSET_MIN 2> /dev/null ]; then
	echo "FSET minimum = $FSET_MIN"
else
	echo "FSET Minimum Range $FSET_MIN is not a number (enter, e.g., \"100\"). "
	exit -2
fi
if [ $FSET_MAX  -eq $FSET_MAX 2> /dev/null ]; then
	echo "FSET maximum = $FSET_MAX"
else
	echo "FSET Minimum Range $FSET_MAX is not a number (enter, e.g., \"201\"). "
	exit -2
fi
echo ""
#___________________________________________________________________________________#

#Obtain the list of subdirectories to tar. Ensure the FSETs are in the correct range.
LISTDIR=`grep "setenv EMLIST" $XML | cut -f3- -d" " | rev | cut -f2- -d"/" | rev`
echo "Directory containing list files is \"$LISTDIR\""
FSETS_LIST=`ls $LISTDIR | grep -v ".tgz"`
if [ "$FSETS_LIST" == "" ]; then
	echo "Folder $LISTDIR is empty."
	echo "ABORT"
	exit -3
fi
FSETS_LIST_RANGE=fset.list
echo "" > $FSETS_LIST_RANGE

FSET_MAX_LIST=0
FSET_MIN_LIST=10000
FSET_MAX_ALL=0
FSET_MIN_ALL=10000
for LINE in $FSETS_LIST; do
	FSET_NUMBER=`echo $LINE | cut -f1 -d"_" | cut -f2 -d"T"`
	if [ $FSET_NUMBER  -eq $FSET_NUMBER 2> /dev/null ]; then
		if [ $FSET_NUMBER -ge $FSET_MAX_ALL ]; then
			FSET_MAX_ALL=$FSET_NUMBER
		fi
		if [ $FSET_NUMBER -le $FSET_MIN_ALL ]; then
			FSET_MIN_ALL=$FSET_NUMBER
		fi
		if [ $FSET_NUMBER -ge $FSET_MIN ] && [ $FSET_NUMBER -le $FSET_MAX ]; then
			echo -e "$LINE" >> $FSETS_LIST_RANGE
			if [ $FSET_NUMBER -ge $FSET_MAX_LIST ]; then
				FSET_MAX_LIST=$FSET_NUMBER
			fi
			if [ $FSET_NUMBER -le $FSET_MIN_LIST ]; then
				FSET_MIN_LIST=$FSET_NUMBER
			fi
		fi
	fi
done
FSET_SANITY=`wc -w $FSETS_LIST_RANGE | cut -f1 -d" "`
if [ $FSET_SANITY -le 0 ]; then
	echo "Folder $LISTDIR does not have FSETs within the correct range."
	echo "ABORT"
	exit -3
fi
echo "Range of FSETs is $FSET_MIN_ALL - $FSET_MAX_ALL"
echo "Range of FSETs within specified range is $FSET_MIN_LIST - $FSET_MAX_LIST"
if [ $FSET_MIN -eq $FSET_MIN_LIST ]; then
	echo -e "" > /dev/null
else 
	echo "Setting FSET minimum = $FSET_MIN_LIST"
	FSET_MIN=$FSET_MIN_LIST
fi
if [ $FSET_MAX -eq $FSET_MAX_LIST ]; then
	echo -e "" > /dev/null
else 
	echo "Setting FSET maximum = $FSET_MAX_LIST"
	FSET_MAX=$FSET_MAX_LIST
fi
FSETS=`cat "$FSETS_LIST_RANGE" | tr "\n" " "`
echo ""
echo "Directories to compress:"
cat $FSETS_LIST_RANGE
rm $FSETS_LIST_RANGE
#___________________________________________________________________________________#

#Define tar output name. If tar file exists, ask to overwrite or abort. 
FSET_NAME=`ls $LISTDIR | grep "FSET" | grep -v ".tgz" | head -1 | cut -f2- -d"_"`
FSET_TAR=FSET_${FSET_MIN}_${FSET_MAX}_${FSET_NAME}.tgz
if [ -e "$LISTDIR/$FSET_TAR" ]; then
	echo "File $FSET_TAR already exists!"
	read -s -n1 -p "Do you want to continue ? y/n (this will over-write file $FSET_TAR)" key
	case $key in
		"Y" | "y" )
		echo ""
		echo "File $FSET_TAR will be over-written."
		;;
		"N" | "n") 
		echo "Process terminated."
		exit 0
		;; 
	esac
fi
#___________________________________________________________________________________#

#Change directory and tar relevent folders
echo ""
echo "Changing directory to $LISTDIR"
cd $LISTDIR
echo "Directories will be compressed into file \"$FSET_TAR\""
echo ""
echo "tar czfv $FSET_TAR $FSETS"
echo ""
tar czfv $FSET_TAR $FSETS
rm -r $FSETS
echo "Original directories have been deleted."
echo ""
echo "Compression complete!"
echo ""
#___________________________________________________________________________________#

exit 0
