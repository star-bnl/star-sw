#! /opt/star/bin/perl -w

use File::Basename;
use Getopt::Std;
use Cwd 'abs_path';     # aka realpath()

my %opt; 
getopts('htoz:b:m:adn:q:x:y:s',\%opt);
my $usage = "$0  combinedDir \n";
#----

my $combinedDir = shift or die $usage;

#my $pwd = abs_path ( $ENV { 'PWD' } );

my $pwd = `pwd`;
if ($pwd=~/direct/) { #if at RCF, /direct/star+dataxx/ does not work ! ANNOYING
$pwd=~ s/\/direct//;
$pwd=~ s/\+/\//;   # change /direct/star+dataxx to /star/dataxx
       }
chomp($pwd);

@flowCents = (
# "cen1",
# "cen2",
# "cen3",
 "cen4"
# "cen5",
# "cen6",
# "cen7",
# "cen8",
# "cen9"
);



my $stdHistName = "flow.hist.root";

 my $tempMacroName="myWriteHeader";
 open ( tempMacro, ">".$tempMacroName.".C");
 print tempMacro  "void $tempMacroName(const char* histFileName, const char* headerFileName) {  \n";
 print tempMacro  "   TFile* histFile = new TFile(histFileName,\"READ\"); \n";
 print tempMacro  "    TVectorD* cumulConstants =(TVectorD* ) histFile->Get(\"CumulConstants\"); \n";

 print tempMacro  "    int nHars             = (*cumulConstants)(0);  \n";
 print tempMacro  "    int nSels             = (*cumulConstants)(1);  \n";
 print tempMacro  "    int nSubs             = (*cumulConstants)(2);  \n";
 print tempMacro  "    int nPhiBins          = (*cumulConstants)(3);  \n";
 print tempMacro  "    int nPhiBinsFtpc      = (*cumulConstants)(4);  \n";
 print tempMacro  "    int nEtaBins          = (*cumulConstants)(5);  \n";
 print tempMacro  "    int nPtBins           = (*cumulConstants)(6);  \n";
 print tempMacro  "    int nCumulIntegOrders = (*cumulConstants)(7);  \n";
 print tempMacro  "    int nCumulInteg_qMax  = (*cumulConstants)(8);  \n";
 print tempMacro  "    int nCumulDiffOrders  = (*cumulConstants)(9);  \n";
 print tempMacro  "    int nCumulDiff_qMax   = (*cumulConstants)(10); \n";
 print tempMacro  "    int nCumulMixHar_pMax   = (*cumulConstants)(11); \n";
 print tempMacro  "    int nCumulMixHar_qMax   = (*cumulConstants)(12); \n";
 print tempMacro  "    double r0             = (*cumulConstants)(13); \n";
 print tempMacro  "    int m_M               = (*cumulConstants)(14); \n";
 print tempMacro  "    int isPidFlow         = (*cumulConstants)(15); \n";
 print tempMacro  "    double r0Mix          = (*cumulConstants)(16); \n";
 print tempMacro  "    double profScale      = (*cumulConstants)(17); \n";
 print tempMacro  "    TObjString* oldNewTag = (TObjString* )histFile->Get(\"CumulMethodTag\"); \n";
 print tempMacro  "    int isNewMethod = ((oldNewTag->GetString()).Contains(\"cumulNew\")) ? 1 : 0;  \n";

 print tempMacro  "     FILE* f = fopen(headerFileName,\"w\");        \n";

 print tempMacro  "     fprintf(f, \"const int nHars             = %d; \\n\", nHars); \n";
 print tempMacro  "     fprintf(f, \"const int nSels             = %d; \\n\", nSels); \n";
 print tempMacro  "     fprintf(f, \"const int nSubs             = %d; \\n\", nSubs); \n";
 print tempMacro  "     fprintf(f, \"const int nPhiBins          = %d; \\n\", nPhiBins); \n";
 print tempMacro  "     fprintf(f, \"const int nPhiBinsFtpc      = %d; \\n\", nPhiBinsFtpc); \n";
 print tempMacro  "     fprintf(f, \"const int nEtaBins          = %d; \\n\", nEtaBins);  \n";
 print tempMacro  "     fprintf(f, \"const int nPtBins           = %d; \\n\", nPtBins); \n";
 print tempMacro  "     fprintf(f, \"const int nCumulIntegOrders = %d; \\n\", nCumulIntegOrders); \n";
 print tempMacro  "     fprintf(f, \"const int nCumulInteg_qMax  = %d; \\n\", nCumulInteg_qMax);  \n";
 print tempMacro  "     fprintf(f, \"const int nCumulDiffOrders  = %d; \\n\", nCumulDiffOrders); \n";
 print tempMacro  "     fprintf(f, \"const int nCumulDiff_qMax   = %d; \\n\", nCumulDiff_qMax);  \n";
 print tempMacro  "     fprintf(f, \"const int nCumulMixHar_pMax = %d; \\n\", nCumulMixHar_pMax);  \n";
 print tempMacro  "     fprintf(f, \"const int nCumulMixHar_qMax = %d; \\n\", nCumulMixHar_qMax);  \n";
 print tempMacro  "     fprintf(f, \"const double r0             = %f; \\n\", r0);  \n";
 print tempMacro  "     fprintf(f, \"const int m_M               = %d; \\n\", m_M);  \n";
 print tempMacro  "     fprintf(f, \"const int isPidFlow         = %d; \\n\", isPidFlow);  \n";
 print tempMacro  "     fprintf(f, \"const double r0Mix          = %f; \\n\", r0Mix);  \n";
 print tempMacro  "     fprintf(f, \"const double profScale      = %f; \\n\", profScale);  \n";
 print tempMacro  "     fprintf(f, \"const int isNewMethod       = %f; \\n\", 1);  \n";
 print tempMacro  "     fclose(f); \n";
 print tempMacro  "   } \n";

 close tempMacro;

# do not move around the two lines below.
 my $writeHeaderMacroName = $tempMacroName;
 my $tempMacroName="calculateCumulant"; # do not change this name



if (-e "$pwd/myWriteCalcCumuHeaderFile.log"){
print `rm $pwd/myWriteCalcCumuHeaderFile.log \n`;
}

$dummy = $flowCents[0];

 `time root4star -b -q '$writeHeaderMacroName.C("$pwd/$combinedDir/$dummy/$stdHistName", "$tempMacroName.h")' > myWriteCalcCumuHeaderFile.log`;



 foreach $eachCent(@flowCents) {
  print  "calculating flow result from cumulants for $cent \n";

  print "$pwd/$combinedDir/$eachCent/$stdHistName \n";

if (-e "$pwd/$combinedDir/$eachCent/myCalcCumulantStd$cent.log"){
print `rm $pwd/$combinedDir/$eachCent/myCalcCumulantStd$cent.log \n`;
}

 `time root4star -b -q '$tempMacroName.C("$pwd/$combinedDir/$eachCent/$stdHistName")' > $pwd/$combinedDir/$eachCent/myCalcCumulantStd$cent.log`;

#if (-e "$pwd/$combinedDir/$eachCent/myCalcCumulantCumul$cent.log"){
#print `rm $pwd/$combinedDir/$eachCent/myCalcCumulantCumul$cent.log \n`;
#}

# `time root4star -b -q '$tempMacroName.C("$pwd/$combinedDir/$eachCent/$stdHistFile/$cumulHistName")' > $pwd/$combinedDir/$eachCent/myCalcCumulantCumul$cent.log`;

}



 
exit;
