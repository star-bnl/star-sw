#! /opt/star/bin/perl -w

 use Carp ();
 local $SIG{__WARN__} = \&Carp::cluck;

$help =<<"EOF";
usage: $0 --f=
--f, the configuration file of a job.
EOF

use Cwd 'abs_path';     # aka realpath()
use Getopt::Long;

GetOptions( "f=s" => \$configFile   );

 die $help," Pl. specify the configuration file."
 unless defined $configFile;

# read in config file
  open (CONFIG, "< $configFile") or die "can't open $configFile $!";
  while (<CONFIG>){
       chomp;                          # no newline
       s/#.*//;                        # no comments
       s/^\s+//;                       # no leading white
       s/\s+$//;                       # no trailing white
       next unless length;             # anything left?
       my ($var, $value) = split (/\s*:\s*/, $_,2);
       $ConfigMap{$var} = $value;
     }
  close(CONFIG);

@flowCents = (
 "cen1",  # do not change these names
 "cen2",
 "cen3",
 "cen4",
 "cen5",
 "cen6",
 "cen7",
 "cen8"
);



my $macroName   = $ConfigMap{"macroName"};
my $libVersion  = $ConfigMap{"libVersion"};
my $runJob      = $ConfigMap{"scriptName"};
my $jobPrefix   = $ConfigMap{"partnPrefix"};
my $picoDstLink = $ConfigMap{"picoDSTLink"};
my $NPartition  = $ConfigMap{"NPartition"};
my $jobDir      = $ConfigMap{"jobDir"};
my $jobName     = $ConfigMap{"jobName"};
my $dataDir     = $ConfigMap{"dataDir"};
my $logFile     = $ConfigMap{"logFileName"};
my $histFile    = $ConfigMap{"histoFile"};
my $meanGFile   = $ConfigMap{"meanGFile"};
my $weightFile  = $ConfigMap{"weightFile"};
my $yieldFile   = $ConfigMap{"yieldFile"};

my $jobCombinedDirName = "jobAll"; # dir name for putting combined histos.


# check the existence
 for ($m=1; $m<$NPartition+1; $m++) { 
   foreach $cent(@flowCents){
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$histFile"  or
     die " $jobDir/$jobName/$jobPrefix$m/$cent/$histFile doesn't exists \n";
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$logFile" or
     die " $jobDir/$jobName/$jobPrefix$m/$cent/$logFile doesn't exists \n";
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$yieldFile" or 
     die " $jobDir/$jobName/$jobPrefix$m/$cent/$yieldFile doesn't exists \n";
   }
 }


my $starVer = $ENV{STAR_LEVEL};
if ($libVersion !~ m/$starVer/) {
  die "#### Your current STAR_LEVEL is $starVer, please change it to $libVersion and run this script again. \n";
}


# make new dir for putting the combined histo (come later in this script)
 print `mkdir $jobDir/$jobName/$jobCombinedDirName`;
 foreach $cent(@flowCents) {
 print `mkdir $jobDir/$jobName/$jobCombinedDirName/$cent`;
}




# combine histo 
 my $tempMacroName="myCombine";
   foreach $cent(@flowCents){

# create a macro that combine files
 open ( tempMacro, ">".$tempMacroName.".C");
 print tempMacro  "void $tempMacroName(const char* histoFileName, const char* newfilename) {  \n";
 print tempMacro  "  TObjArray* toBeAddedFiles = new TObjArray(); \n";

     for ($m=1; $m<$NPartition+1; $m++) {
 print tempMacro  "char  dummyName$m\[256\]; \n";
 print tempMacro  "sprintf(dummyName$m,\"$jobDir/$jobName/$jobPrefix$m/$cent/\%s\",histoFileName); \n";
 print tempMacro  "toBeAddedFiles->AddLast((TObject *)(new TObjString(dummyName$m))); \n";
}

 print tempMacro  "   TFile* newfile = new TFile(newfilename,\"RECREATE\"); \n";
   
 print tempMacro  "   TFile* oldfile1 = new  TFile(((TObjString *)(toBeAddedFiles->At(0)))->GetName(),\"READ\"); \n";
 print tempMacro  "   TList* list = oldfile1->GetListOfKeys(); \n";
 print tempMacro  "      TIter next(list); \n";
 print tempMacro  "      TKey* key; \n";
 print tempMacro  "      TObject* obj; \n";
 print tempMacro  "   while (key = (TKey*)next()) { \n";
 print tempMacro  "        obj = oldfile1->Get(key->GetName()); \n";
 print tempMacro  "     if (!obj) return; \n";
 print tempMacro  "   if(obj->IsA() == TDirectory::Class()){ \n";
 print tempMacro  "                        delete obj;   \n";
 print tempMacro  "                         obj = NULL;  \n";
 print tempMacro  "                         continue;    \n";
 print tempMacro  "                 }                   \n";
 print tempMacro  "     TObject* newobj = obj->Clone(); \n";
 print tempMacro  "	if (newobj->InheritsFrom( TH1::Class())) { \n";
 print tempMacro  " for (int k=1; k<toBeAddedFiles->GetEntries(); k++){ \n";
 print tempMacro  "	  TFile* f =new TFile(((TObjString *)(toBeAddedFiles->At(k)))->GetName(), \"READ\"); \n";
 print tempMacro  "((TH1 *) newobj)->Add(((TH1 *)f->Get(key->GetName()))); \n";
 print tempMacro  "	 delete f; \n";
 print tempMacro  "	} \n";
 print tempMacro  "		 } \n";
 print tempMacro  "         newfile->cd(); \n";
 print tempMacro  "        newobj->Write(); \n";
 print tempMacro  "       delete newobj; \n";
 print tempMacro  "   } \n";
 print tempMacro  "   gROOT->cd(); \n";
 print tempMacro  "  delete key; \n";
 print tempMacro  "    newfile->Write(); \n";
 print tempMacro  "    newfile->Close(); \n";
 print tempMacro  " } \n";

 close tempMacro;



# now run the combine macro
  print  "combining  $histFile for $cent \n";
 `time root4star -b -q '$tempMacroName.C("$histFile", "$histFile")' > myCombineCumulant$cent.log`;
  print  "combining  $yieldFile for $cent \n";
 `time root4star -b -q '$tempMacroName.C("$yieldFile", "$yieldFile")' > myCombineYieldHist$cent.log`;

# copy the combined files 
 `cp $histFile $jobDir/$jobName/$jobCombinedDirName/$cent/$histFile `;
 `cp $yieldFile $jobDir/$jobName/$jobCombinedDirName/$cent/$yieldFile `;

 
 `rm $histFile`;
 `rm $yieldFile`;
 `rm myCombineCumulant$cent.log`;
 `rm myCombineYieldHist$cent.log`;

}



# put flow.cumulant.root($histFile) into flow.hist.root($yieldFile)
# not necessary if this has been done in doFlowEvent.C, but no hurt to repeat
# it here, just in case if it's no done.

 my $tempMacroName="myMergy";
 open ( tempMacro, ">".$tempMacroName.".C");
 print tempMacro  "void $tempMacroName(const char* stdHistoFileName, const char* cumuHistoFileName) {  \n";
 print tempMacro  " TFile stdFile(stdHistoFileName, \"UPDATE\"); \n";
 print tempMacro  " TFile cumuFile(cumuHistoFileName, \"READ\"); \n";
 print tempMacro  "  if (cumuFile.IsOpen()) { \n";
 print tempMacro  "    cumuFile.ReadAll();  \n";
 print tempMacro  "    if (stdFile.IsOpen()) { \n";
 print tempMacro  "      stdFile.cd();          \n";
 print tempMacro  "    cumuFile.GetList()->Write(); \n";
 print tempMacro  "      stdFile.Close();  \n";
 print tempMacro  "    } else { cout<<\" *** Can't open \"<<stdHistoFileName<<endl; } \n";
 print tempMacro  "   } else { cout<<\" *** Can't open \"<<cumuHistoFileName<<endl; } \n";
 print tempMacro  " } \n";
 close tempMacro;

 foreach $cent(@flowCents) {
 `time root4star -b -q '$tempMacroName.C("$jobDir/$jobName/$jobCombinedDirName/$cent/$yieldFile", "$jobDir/$jobName/$jobCombinedDirName/$cent/$histFile")' > myCopyCumulHistoToStd$cent.log`;
 `rm myCopyCumulHistoToStd$cent.log`;
}

#now all combined histo must be in flow.hist.root. Let's do cumulant calc.

#write out a header file for cumul calc macro
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
 print tempMacro  "    double r0             = (*cumulConstants)(11);        \n";
 print tempMacro  "    int m_M               = (*cumulConstants)(12);        \n";
 print tempMacro  "    int isPidFlow         = (*cumulConstants)(13);        \n";
 print tempMacro  "    double profScale      = (*cumulConstants)(14);        \n";
 print tempMacro  "   //  double r0 = 1.5;                              \n";
 print tempMacro  "   //  int    m_M = 1;                               \n";
 print tempMacro  "   //  int    isPidFlow = 0;                               \n";
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
 print tempMacro  "     fprintf(f, \"const int isNewMethod       = %d; \\n\", isNewMethod);  \n";
 print tempMacro  "     fprintf(f, \"const int m_M               = %d; \\n\", m_M);  \n";
 print tempMacro  "     fprintf(f, \"const int isPidFlow         = %d; \\n\", isPidFlow);  \n";
 print tempMacro  "     fprintf(f, \"const double r0             = %f; \\n\", r0);  \n";
 print tempMacro  "     fprintf(f, \"const double profScale      = %f; \\n\", profScale);  \n";
 print tempMacro  "     fclose(f); \n";
 print tempMacro  "   } \n";

 close tempMacro;

# do not move around the two lines below.
 my $writeHeaderMacroName = $tempMacroName;
 my $tempMacroName="calculateCumulant"; # do not change this name

 `time root4star -b -q '$writeHeaderMacroName.C("$jobDir/$jobName/$jobCombinedDirName/cen1/$histFile", "$tempMacroName.h")' > myWriteCalcCumuHeaderFile.log`;

 `rm myWriteCalcCumuHeaderFile.log`;


 foreach $cent(@flowCents) {
  print  "calculating flow result from cumulants for $cent \n";
 `time root4star -b -q '$tempMacroName.C("$jobDir/$jobName/$jobCombinedDirName/$cent/$yieldFile"' > myCalcCumulant$cent.log`;
 `rm myCalcCumulant$cent.log`;
 `time root4star -b -q '$tempMacroName.C("$jobDir/$jobName/$jobCombinedDirName/$cent/$histFile"' > myCalcCumulant$cent.log`;
 `rm myCalcCumulant$cent.log`;
}

exit;
