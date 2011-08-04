#!/bin/bash

#Specify pthard bins
N_PT_HARD_BINS=11
PT_HARD_MIN=( 3 4 5 7 9 11 15 25 35 45 55 )
PT_HARD_MAX=( 4 5 7 9 11 15 25 35 45 55 65 )

#Declare inputs parameters and scripts/macros
REQUEST_FILE=$1
KUMAC_PATH=StRoot/macros/embedding
KUMAC_TEMPLATE=pythiaTuneA_template.kumac
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

if [ ! -d $KUMAC_PATH ]; then 
	echo "Error, kumac path \"$KUMAC_PATH\" does not exist, Abort!"
	exit -1
fi	

if [ ! -e $KUMAC_PATH/$KUMAC_TEMPLATE ]; then 
	echo "Error, kumac template \"$KUMAC_PATH/$KUMAC_TEMPLATE\" does not exist, Abort!"
	exit -1
fi	

if [ ! -e $EMBEDDING_SCRIPT ]; then 
	echo "Error, embedding script \"$EMBEDDING_SCRIPT\" does not exist, Abort!"
	exit -1
fi	

#Check if pthard bins are correct
echo "Getting ready to create XML files for the following pt hard bins:"	
for (( PT=0; PT<$N_PT_HARD_BINS; PT++ )); do
	echo "     $PT)  ${PT_HARD_MIN[PT]} - ${PT_HARD_MAX[PT]} GeV"
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
  echo "Unknown OS : $CHOS. Set the sl44 path"
	LIB=.sl44_gcc346
	echo ""
fi

#Create directory to contain all pthard folders and xml files
[ -d $SUBMIT_DIR ] || mkdir $SUBMIT_DIR 

REQUEST_FILE_COMPLETE=requestScriptComplete.tmp.txt

#Create xml file for each pthard bin
#Edit kumac template for given pthard bin
#Link libraries
for (( PT=0; PT<$N_PT_HARD_BINS; PT++ )); do
	echo ""
	echo "Creating xml for Pt Bin ${PT_HARD_MIN[PT]} - ${PT_HARD_MAX[PT]} GeV..."	
	OUTDIR=$SUBMIT_DIR/PtHard_${PT_HARD_MIN[PT]}_${PT_HARD_MAX[PT]}
	[ -d $OUTDIR ] || mkdir $OUTDIR
	cat $REQUEST_FILE > $REQUEST_FILE_COMPLETE
	FINAL_REQUEST=request_PtHard_${PT_HARD_MIN[PT]}_${PT_HARD_MAX[PT]}.txt
	echo "  -ptmin ${PT_HARD_MIN[PT]}"
	echo -e " -ptmin ${PT_HARD_MIN[PT]}" >> $REQUEST_FILE_COMPLETE
	echo "  -ptmax ${PT_HARD_MAX[PT]}"
	echo -e " -ptmax ${PT_HARD_MAX[PT]}" >> $REQUEST_FILE_COMPLETE
	echo "  -kumacFile $DIR/$OUTDIR/pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac"
	echo -e " -kumacFile $DIR/$OUTDIR/pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac" >> $REQUEST_FILE_COMPLETE
	cat $REQUEST_FILE_COMPLETE | tr "\n" " " >	$FINAL_REQUEST
	CREATE_SCRIPT=`cat $FINAL_REQUEST`
	mv $FINAL_REQUEST $OUTDIR
	cd $OUTDIR
	ln -s $DIR/StRoot
	ln -s $DIR/$LIB
	sed "s/CKIN 3=PTMIN/CKIN 3=${PT_HARD_MIN[PT]}.0/" $KUMAC_PATH/$KUMAC_TEMPLATE > pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac.tmp
	sed "s/CKIN 4=PTMAX/CKIN 4=${PT_HARD_MAX[PT]}.0/" pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac.tmp > pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac
	rm pyth_${PT_HARD_MIN[PT]}-${PT_HARD_MAX[PT]}.kumac.tmp
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
