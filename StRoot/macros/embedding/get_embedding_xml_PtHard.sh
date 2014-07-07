#!/bin/bash

############################################################
#
# Specify pt bins below. 
# All other parameters are set in $REQUEST_FILE
# Script calls get_embedding.xml for each pt range, 
# and puts XML and code in Submit/Pt_<MIN>_<MAX>.
# If only 1 pt bin is needed, do not use this script. 
# Use get_embedding.xml directly.
#
############################################################

#Specify pthard bins
#N_PT_BINS=11
#PT_MIN=( 3 4 5 7 9 11 15 25 35 45 55 )
#PT_MAX=( 4 5 7 9 11 15 25 35 45 55 65 )
N_PT_BINS=2
PT_MIN=( 0 10 )
PT_MAX=( 10 30 )

#Declare inputs parameters and scripts/macros
REQUEST_FILE=$1
EMBEDDING_SCRIPT=StRoot/macros/embedding/get_embedding_xml.pl
SUBMIT_DIR=Submit
DIR=`pwd`

#Check if files exist
if [ "$REQUEST_FILE" == "" ]; then
	echo "Usage: $0 <REQUEST>"
	echo "(Enter input file containing embedding request parameters)"
	exit -1
fi

if [ ! -e $REQUEST_FILE ]; then
	echo "Error, file \"$REQUEST_FILE\" does not exist, Abort!"
	exit -1
fi

if [ ! -e $EMBEDDING_SCRIPT ]; then 
	echo "Error, embedding script \"$EMBEDDING_SCRIPT\" does not exist, Abort!"
	exit -1
fi	

#Check if pthard bins are correct
echo "Getting ready to create XML files for the following pt hard bins:"	
for (( PT=0; PT<$N_PT_BINS; PT++ )); do
	echo "     $PT)  ${PT_MIN[PT]} - ${PT_MAX[PT]} GeV"
done
read -s -n1 -p "Is this correct? Y/n " key
case $key in
	"Y" | "y" )
	echo ""
	;;
	"N" | "n") 
	echo ""
	exit -1
	;; 
esac

#Get local library
# 32sl44 -->  .sl44_gcc346
# sl53   -->  .sl53_gcc432
CHOS=`echo \$CHOS`
if [ $CHOS == *32sl44* ]; then
	LIB=.sl44_gcc346
elif [ $CHOS == *sl53* ]; then	
	LIB=.sl53_gcc432
else
  echo "Unknown OS : $CHOS. Set the sl53 path"
	LIB=.sl53_gcc432
	echo ""
fi

#Create directory to contain all pthard folders and xml files
[ -d $SUBMIT_DIR ] || mkdir $SUBMIT_DIR 

REQUEST_FILE_COMPLETE=requestScriptComplete.tmp.txt

#Create xml file for each pthard bin
#Edit kumac template for given pthard bin
#Link libraries
for (( PT=0; PT<$N_PT_BINS; PT++ )); do
	echo ""
	echo "Creating xml for Pt Bin ${PT_MIN[PT]} - ${PT_MAX[PT]} GeV..."	
	OUTDIR=$SUBMIT_DIR/PtHard_${PT_MIN[PT]}_${PT_MAX[PT]}
	[ -d $OUTDIR ] || mkdir $OUTDIR
	cat $REQUEST_FILE > $REQUEST_FILE_COMPLETE
	FINAL_REQUEST=request_PtHard_${PT_MIN[PT]}_${PT_MAX[PT]}.txt
	echo "  -ptmin ${PT_MIN[PT]}"
	echo -e " -ptmin ${PT_MIN[PT]}" >> $REQUEST_FILE_COMPLETE
	echo "  -ptmax ${PT_MAX[PT]}"
	echo -e " -ptmax ${PT_MAX[PT]}" >> $REQUEST_FILE_COMPLETE
	cat $REQUEST_FILE_COMPLETE | tr "\n" " " >	$FINAL_REQUEST
	CREATE_SCRIPT=`cat $FINAL_REQUEST`
	mv $FINAL_REQUEST $OUTDIR
	cd $OUTDIR
	ln -s $DIR/StRoot
	ln -s $DIR/$LIB
	echo "perl $EMBEDDING_SCRIPT $CREATE_SCRIPT"
	perl $EMBEDDING_SCRIPT $CREATE_SCRIPT
	cd $DIR
	echo "Done!"

done	

rm $REQUEST_FILE_COMPLETE
echo ""
echo "Successfully created xml files for all Pt Bins!"
echo ""
exit 0
