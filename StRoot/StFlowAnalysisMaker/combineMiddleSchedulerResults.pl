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
chomp($pwd);



if (!(-d $pwd."/$combinedDir") ){
  mkdir $pwd."/$combinedDir", 0755  or die "can not mkdir $combinedDir :$!\n";
} else {
  print "$combinedDir exists, will add combined files to it \n";
}


@jobDirs = (
"/star/data10/GC/aihong/codeCleanUpTest/runSchedulerJobsExample"
);


@flowCents = (
#"cen1",
#"cen2",
#"cen3",
"cen4"
#"cen5",
#"cen6",
#"cen7",
#"cen8",
#"cen9"
);


my $BeforeCombineName = "denominatorNew.root";
my $AfterCombineName = "denominator.root";

my $combineMacroName="myCombine";


foreach $eachCent (@flowCents) {

$totalJobs=0.;
$finishedJobs=0.;

if (!(-d $pwd."/$combinedDir/$eachCent")) {
mkdir $pwd."/$combinedDir/$eachCent", 0755  or die "can not mkdir $combinedDir/$eachCent :$!\n";
} else {
print "$combinedDir/$eachCent exists, will add Files to it \n";
}


    foreach $jobDir (@jobDirs) {

  foreach (glob ("$jobDir/$eachCent/sched*$BeforeCombineName") ){
      my $eachDenomFile = $_;

      @fields = split(/\//,$eachDenomFile) ;
      $eachDenomFile= $fields[$#fields];
      chomp $eachDenomFile;

      print `ln -s $_ $pwd/$combinedDir/$eachCent/$eachDenomFile`;
  }
       
   my @tempScripts = glob( "$jobDir/$eachCent/sched*.csh" );
   my @tempFinished = glob( "$jobDir/$eachCent/sched*flow.hist.root" ) ;

              $totalJobs +=scalar(@tempScripts);
	   $finishedJobs +=scalar(@tempFinished);
       
    }

my $perCentFinished = 0.;
if ($totalJobs != 0) {$perCentFinished = ($finishedJobs/$totalJobs)*100.;}

 print "for $jobDir/$eachCent, finished $finishedJobs jobs out of $totalJobs, ".$perCentFinished."% completed \n";

 open ( combineMacro, ">$pwd/$combinedDir/$eachCent/$combineMacroName.C");

 print combineMacro  "#include \"TObject.h\"  \n";
 print combineMacro  "#include \"TObjArray.h\" \n";
 print combineMacro  "#include \"TKey.h\" \n";
 print combineMacro  "#include \"TH1.h\" \n";
 print combineMacro  "#include \"TFile.h\" \n";
 print combineMacro  "#include \"TList.h\" \n";
 print combineMacro  "#include \"TString.h\" \n";
 print combineMacro  "#include \"TObjString.h\" \n";
 print combineMacro  "#include \"iostream.h\" \n";
 print combineMacro  "void $combineMacroName(const char* newfilename) {  \n";
 print combineMacro  "  TObjArray* toBeAddedFiles = new TObjArray(); \n";

$m=1;
foreach $eachDenom (`ls $pwd/$combinedDir/$eachCent/sched*$BeforeCombineName`) {
chomp($eachDenom);
  if ( -s $eachDenom ) {
 print combineMacro  "char  dummyName$m\[256\]; \n";
 print combineMacro  "sprintf(dummyName$m,\"\%s\",\"$eachDenom\"); \n";
 print combineMacro  "toBeAddedFiles->AddLast((TObject *)(new TObjString(dummyName$m))); \n";
$m ++;
  }
}



 print combineMacro  "   TFile* newfile = new TFile(newfilename,\"RECREATE\"); \n";

 print combineMacro  "   TFile* oldfile1 = new  TFile(((TObjString *)(toBeAddedFiles->At(0)))->GetName(),\"READ\"); \n";
 print combineMacro  "   TList* list = oldfile1->GetListOfKeys(); \n";
 print combineMacro  "      TIter next(list); \n";
 print combineMacro  "      TKey* key; \n";
 print combineMacro  "      TObject* obj; \n";
 print combineMacro  "   while (key = (TKey*)next()) { \n";
 print combineMacro  "   TString tempStr(key->GetName()); \n";
 print combineMacro  "   if (tempStr.Contains(\"CumulDenomEta\") || tempStr.Contains(\"CumulDenomPt\") || tempStr.Contains(\"CumulDenom2D\") ) continue; \n";
 print combineMacro  "        obj = oldfile1->Get(key->GetName()); \n";
 print combineMacro  "     if (!obj) return; \n";
 print combineMacro  "   if(obj->IsA() == TDirectory::Class()){ \n";
 print combineMacro  "                        delete obj;   \n";
 print combineMacro  "                         obj = NULL;  \n";
 print combineMacro  "                         continue;    \n";
 print combineMacro  "                 }                   \n";
 print combineMacro  "     TObject* newobj = obj->Clone(); \n";
 print combineMacro  "  if (newobj->InheritsFrom( TH1::Class())) { \n";
# do not need to newobj->Reset(), because k begins with 1 below !!
 print combineMacro  " for (int k=1; k<toBeAddedFiles->GetEntries(); k++){ \n";
 print combineMacro  "    TFile* f =new TFile(((TObjString *)(toBeAddedFiles->At(k)))->GetName(), \"READ\"); \n";
 print combineMacro  "((TH1 *) newobj)->Add(((TH1 *)f->Get(key->GetName()))); \n";
 print combineMacro  "   delete f; \n";
 print combineMacro  "  } \n";
 print combineMacro  "           } \n";
 print combineMacro  "         newfile->cd(); \n";
 print combineMacro  "        newobj->Write(key->GetName(),TObject::kOverwrite | TObject::kSingleKey); \n";
 print combineMacro  "       delete newobj; \n";
 print combineMacro  "   } \n";
 print combineMacro  "   gROOT->cd(); \n";
 print combineMacro  "  delete key; \n";
 print combineMacro  "    newfile->Write(); \n";
 print combineMacro  "    newfile->Close(); \n";
 print combineMacro  " } \n";

 close combineMacro;


 

 open (combineScript,">$pwd/$combinedDir/$eachCent/$combineMacroName.csh");
  print combineScript "#!/bin/csh \n";
  print combineScript "#BSUB -J Combine$eachCent \n";
#  print combineScript "#BSUB -u aihong\@cnr2.kent.edu\n"; 
  print combineScript "set logfile=$combineMacroName.log \n";
  print combineScript "set rundir=$pwd/$combinedDir/$eachCent \n";
  print combineScript "\n";
  print combineScript "source \$GROUP_DIR/.starver dev \n";
  print combineScript "cd \$rundir \n";
  print combineScript "root4star -b -q '$combineMacroName.C++(\"$pwd/$combinedDir/$eachCent/$AfterCombineName\")' >& \${logfile} \n";
  print combineScript "exit 0 \n";
 close combineScript;

   print `bsub  -q star_cas_short -sp 100 -o  x.out < $pwd/$combinedDir/$eachCent/$combineMacroName.csh `;


}


 
exit;
