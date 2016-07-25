#!/bin/ksh

########################################################################
# root_draw.ksh
########################################################################
# Written by: Kevin Reil
########################################################################
# Given an ascii file as input, it creates a root kumac 
# to create an ntuple containing as many variables as the ascii file
# has columns
#
# The variables will be named x1,x2,x3,...xn
#
# Single column files will start by creating an x1 distribution
# Multi  column files will start by creating an x1:x2 distribution
#
# To avoid corruption of original data file(s) and to facilitate
# multiple input files, all files are cat(entated) together into a temp
# file in /tmp directory
#
# A file "temp.root" will remain after you exit root
# which contains your ntuple.
#
########################################################################

flist="$*"

rk="/tmp/$$.temp.root_kumac.C"
filelist=""
nodraw=0
batchon=0
datafile="/tmp/$$.data.txt"
speccol=0
numcol=0
cmds=""

########################################################################
# Read a few parameters and get file list
########################################################################

for file in $flist
do
    if [[ -f $file ]]
    then
	filelist="$filelist $file"
	line1=`cat $file |head -1`
	numcol=0
	for col in $line1
	do
	    (( numcol = $numcol + 1 ))
	done
	echo "$file: Found $numcol columns"    
	continue
    fi
    if [[ "$file" = "-nodraw" ]]
    then
	nodraw=1
	echo "$file : will not start root for you (Type: \"root $rk\")."
	continue
    fi
    if [[ "$file" = "-batch" ]] || [[ "$file" = "-b" ]]
    then
	batchon=1
	echo "$file : will start root in batch mode."
	continue
    fi
    if [[ "${file#\-ncols=*}" != "$file" ]]
    then
	speccol=${file#\-ncols=*}
	echo "$file : specified $speccol colums"
	continue
    fi
    cmds="$cmds$file; "
done

########################################################################
# Catenate all input files into datafile
########################################################################

touch /tmp/t2$$.txt
for file in $filelist
do
    cat $file /tmp/t2$$.txt >> /tmp/t1$$.txt
    mv /tmp/t1$$.txt /tmp/t2$$.txt
done
mv /tmp/t2$$.txt $datafile

if (( $speccol > 0 ))
then
    (( numcol = $speccol ))
fi
if (( $numcol < 1 ))
then
    echo "Data must have at least one column"
    exit
fi

########################################################################
# Create kumac to read in ntuple.
########################################################################

echo "{" > $rk
echo "gROOT->Reset();" >> $rk
echo "TFile *f = new TFile(\"temp.root\",\"RECREATE\");" >> $rk
echo "FILE *fin = fopen(\"$datafile\",\"r\");" >> $rk
echo "Float_t x[$numcol];" >> $rk
echo -n "TNtuple *ntuple = new TNtuple(\"ntuple\",\"ntuple\",\"x1" >> $rk
cc=2
while (( $cc <= $numcol ))
do
    (( c = $cc ))
    echo -n ":x${c}" >> $rk
    (( cc = $cc + 1 ))
done
echo "\");" >> $rk
echo "int linec=0;" >>$rk
echo "while (1) {" >> $rk
echo "   ++linec;" >>$rk
echo "   char str[2048];" >> $rk
echo "   char tstr[2048];" >> $rk
echo "   int start[$numcol];" >> $rk
echo "   int end[$numcol];" >> $rk
echo "   char gots = fgets(&str[0],2048,fin);" >> $rk
echo "   if (gots == NULL) break;" >> $rk
#echo "   printf(\"input: %s\\\n\",str);" >> $rk
echo "   for (int i=0; i<$numcol; ++i) {" >> $rk
echo "      start[i]=end[i]=0;" >> $rk
echo "   }" >> $rk
echo "   int cc=0;" >> $rk    
echo "   int on=0;" >> $rk    
echo "   for (int i=0; i<2048; ++i) {" >> $rk
echo "      if (cc>=${numcol}) break;" >> $rk
echo "      if (str[i]=='\\\0' || str[i]=='\\\n') {" >> $rk
echo "         on=0;" >> $rk
echo "         end[cc]=i;" >> $rk
echo "         ++cc;" >> $rk
echo "         break;" >> $rk
echo "         continue;" >> $rk
echo "      }" >> $rk
echo "      if (on==0 && str[i]!=' ') {" >> $rk
echo "         on=1;" >> $rk
echo "         start[cc]=i;" >> $rk
echo "         continue;" >> $rk
echo "      }" >> $rk
echo "      if (on==1 && str[i]==' ') {" >> $rk
echo "         on=0;" >> $rk
echo "         end[cc]=i;" >> $rk
echo "         ++cc;" >> $rk
echo "         continue;" >> $rk
echo "      }" >> $rk
echo "   }" >> $rk
echo "   for (int i=0; i<$numcol; ++i) {" >> $rk
echo "      strncpy(&tstr[0],&str[start[i]],end[i]);" >> $rk
echo "      tstr[end[i]-start[i]]='\\\0';" >> $rk
echo "      int r = sscanf(&tstr[0],\"%f\",&x[i]);" >> $rk 
#echo "      x[i]=atof(&tstr[0]);" >> $rk
#echo "      printf(\"%4.2f \",x[i]);" >> $rk
#echo "      printf(\"%d=%s \",i,tstr);" >> $rk
echo "   }" >> $rk
#echo "   printf(\"\\\n\");" >> $rk
echo "" >> $rk
#echo "printf(\"%d %d\\\n\",cc,$numcol);" >> $rk
echo "   if (cc==$numcol) {" >> $rk
echo "      ntuple->Fill(&x[0]);" >> $rk
#echo "      printf(\"Good row : %s\",str);" >> $rk
echo "   } else {" >> $rk
echo "      fprintf(stderr,\"Bad  row : %d %d!=%d\",linec,cc,$numcol);" >> $rk
echo "   }" >> $rk
echo "   if (linec%100==0)" >> $rk
echo "      printf(\"%d lines read in.\\\n\",linec);" >> $rk
#echo "break;" >> $rk
echo "}" >> $rk

echo "printf(\"%d lines read in.\\\n\",linec);" >> $rk

echo "gStyle->SetFillColor(kWhite);" >> $rk
echo "gStyle->SetMarkerSize(0.5);" >> $rk
echo "gStyle->cd();" >> $rk

if [[ "$cmds" != "" ]]
then
    echo "$cmds" >> $rk
else
    if (( $numcol > 1 ))
    then
	echo "ntuple->Draw(\"x1:x2\",\"\",\"p\");" >> $rk
    else
	echo "ntuple->Draw(\"x1\");" >> $rk
    fi
fi
echo "htemp->SetMarkerSize(0.5);" >> $rk
echo "htemp->SetMarkerStyle(22);" >> $rk
echo "c1->Modified();" >> $rk
echo "f->Write();" >> $rk
echo "}" >> $rk

echo "Generated script:"
echo "/**************************************************************/"
cat $rk
echo "/**************************************************************/"

########################################################################
# Start root if not asked not to
########################################################################

if (( $nodraw == 0 ))
then
    if (( $batchon == 1 ))
    then
	root -b $rk
    else
	root $rk
    fi
fi

rm $datafile
rm $rk

