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


 ($meanGCombined = $meanGFile) =~ s/New//;
 ($weightCombined = $weightFile) =~ s/New//;


my $starVer = $ENV{STAR_LEVEL};
if ($libVersion !~ m/$starVer/) {
  die "#### Your current STAR_LEVEL is $starVer, please change it to $libVersion and run this script again. \n";
}



# check the existence
 for ($m=1; $m<$NPartition+1; $m++) { 
   foreach $cent(@flowCents){
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$histFile"  or
     die " $jobDir/$jobName/$jobPrefix$m/$cent/$histFile doesn't exists \n";
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$meanGFile" or 
     die " $jobDir/$jobName/$jobPrefix$m/$cent/$meanGFile doesn't exists \n";
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$weightFile" or
     warn " $jobDir/$jobName/$jobPrefix$m/$cent/$weightFile doesn't exists \n";
    -e "$jobDir/$jobName/$jobPrefix$m/$cent/$logFile" or
     warn " $jobDir/$jobName/$jobPrefix$m/$cent/$logFile doesn't exists \n";
   }
 }

# cleaning
 for ($m=1; $m<$NPartition+1; $m++) { 
   foreach $cent(@flowCents){
    `mv $jobDir/$jobName/$jobPrefix$m/$cent/$logFile $jobDir/$jobName/$jobPrefix$m/$cent/$logFile.old`;
    `mv $jobDir/$jobName/$jobPrefix$m/$cent/$histFile $jobDir/$jobName/$jobPrefix$m/$cent/$histFile.old`;
    `mv $jobDir/$jobName/$jobPrefix$m/$cent/$yieldFile $jobDir/$jobName/$jobPrefix$m/$cent/$yieldFile.old`;

   }
 }


# combine histo 
 my $combineMacroName="myCombine";
   foreach $cent(@flowCents){

# create a macro that combine files
 open ( combineMacro, ">".$combineMacroName.".C");
 print combineMacro  "void $combineMacroName(const char* histoFileName, const char* newfilename) {  \n";
 print combineMacro  "  TObjArray* toBeAddedFiles = new TObjArray(); \n";

     for ($m=1; $m<$NPartition+1; $m++) {
 print combineMacro  "char  dummyName$m\[256\]; \n";
 print combineMacro  "sprintf(dummyName$m,\"$jobDir/$jobName/$jobPrefix$m/$cent/\%s\",histoFileName); \n";
 print combineMacro  "toBeAddedFiles->AddLast((TObject *)(new TObjString(dummyName$m))); \n";
}

 print combineMacro  "   TFile* newfile = new TFile(newfilename,\"RECREATE\"); \n";
   
 print combineMacro  "   TFile* oldfile1 = new  TFile(((TObjString *)(toBeAddedFiles->At(0)))->GetName(),\"READ\"); \n";
 print combineMacro  "   TList* list = oldfile1->GetListOfKeys(); \n";
 print combineMacro  "      TIter next(list); \n";
 print combineMacro  "      TKey* key; \n";
 print combineMacro  "      TObject* obj; \n";
 print combineMacro  "   while (key = (TKey*)next()) { \n";
 print combineMacro  "        obj = oldfile1->Get(key->GetName()); \n";
 print combineMacro  "     if (!obj) return; \n";
 print combineMacro  "   if(obj->IsA() == TDirectory::Class()){ \n";
 print combineMacro  "                        delete obj;   \n";
 print combineMacro  "                         obj = NULL;  \n";
 print combineMacro  "                         continue;    \n";
 print combineMacro  "                 }                   \n";
 print combineMacro  "     TObject* newobj = obj->Clone(); \n";
 print combineMacro  "	if (newobj->InheritsFrom( TH1::Class())) { \n";
 print combineMacro  " for (int k=1; k<toBeAddedFiles->GetEntries(); k++){ \n";
 print combineMacro  "	  TFile* f =new TFile(((TObjString *)(toBeAddedFiles->At(k)))->GetName(), \"READ\"); \n";
 print combineMacro  "((TH1 *) newobj)->Add(((TH1 *)f->Get(key->GetName()))); \n";
 print combineMacro  "	 delete f; \n";
 print combineMacro  "	} \n";
 print combineMacro  "		 } \n";
 print combineMacro  "         newfile->cd(); \n";
 print combineMacro  "        newobj->Write(); \n";
 print combineMacro  "       delete newobj; \n";
 print combineMacro  "   } \n";
 print combineMacro  "   gROOT->cd(); \n";
 print combineMacro  "  delete key; \n";
 print combineMacro  "    newfile->Write(); \n";
 print combineMacro  "    newfile->Close(); \n";
 print combineMacro  " } \n";

 close combineMacro;



# now run the combine macro
# do not combine the phi weight file!! otherwise the combined weight 
# is around 1*NPartition, NOT GOOD!  Should change phiweight from histo.to 
# TProfile.

  print  "combining $meanGFile  for $cent \n";
 `source /auto/u/aihong/.starver $libVersion \n`;
 `time root4star -b -q '$combineMacroName.C("$meanGFile", "$meanGCombined")' > myCombineMeanG$cent.log`;


# copy the combined files to run dirs
     for ($m=1; $m<$NPartition+1; $m++) {
 `cp $meanGCombined $jobDir/$jobName/$jobPrefix$m/$cent/$meanGCombined `;

}
 
 `rm $meanGCombined`;

 `rm myCombineMeanG$cent.log`;
}

# re-submit jobs
 for ($m=1; $m<$NPartition+1; $m++) { 
  foreach $cent(@flowCents) { 
  print `bsub -q medium  <  $jobDir/$jobName/$jobPrefix$m/$cent/$runJob`;
 }
}

exit;
