#! /opt/star/bin/perl -w

# combine flow.cumulant.root and flow.hist.root



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

  mkdir $pwd."/$combinedDir", 0755  or die "can not mkdir $combinedDir :$!\n";


@jobDirs = (
"/star/data10/GC/aihong/codeCleanUpTest/runSchedulerJobsExample"
);


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



my $cumulHistName = "flow.cumulant.root";
my $stdHistName = "flow.hist.root";

my $combineMacroName="myCombine";


foreach $eachCent (@flowCents) {

$totalJobs=0.;
$finishedJobs=0.;

mkdir $pwd."/$combinedDir/$eachCent", 0755  or die "can not mkdir $combinedDir/$eachCent :$!\n";

    foreach $jobDir (@jobDirs) {


  foreach (glob ("$jobDir/$eachCent/sched*$stdHistName") ){
      my $eachStdHistFile = $_;

      $passTag=1;

      @fields = split(/\//,$eachStdHistFile) ;
      $eachStdHistFileNoPath= $fields[$#fields];
      chomp $eachStdHistFileNoPath;

# at the end of doFlowEvents.C, the content of flow.cumulant.root is written to flow.hist.root, so we do not combine flow.cumulant.root here.

  #    ($eachCumulHistFileNoPath = $eachStdHistFileNoPath) =~ s/$stdHistName/$cumulHistName/;

   #  ($eachCumulHistFile = $eachStdHistFile) =~ s/$stdHistName/$cumulHistName/;
    #  chomp $eachCumulHistFile;



      if (! (-s $eachStdHistFile) )#&& (-s $eachCumulHistFile) ) 
	{ $passTag=0;};

#  get job idex out

      @fields2 = split(/\.flow\./, $eachStdHistFileNoPath);
      @fields2a = split(/\_/,$fields2[0]);
      $jobIdx = $fields2a[$#fields2a];

      print "$jobIdx ";

# check if the same job finished for all centalities. This is to make sure minbias sample not get distorted.

      foreach $EC (@flowCents){
	  @x1 = `ls $jobDir/$EC/sched*_$jobIdx.$stdHistName | wc -l`;
#	  @x2 = `ls $jobDir/$EC/sched*_$jobIdx.$cumulHistName | wc -l`;
	  @y1 = `ls $jobDir/$EC/sched*_$jobIdx.$stdHistName`;
#	  @y2 = `ls $jobDir/$EC/sched*_$jobIdx.$cumulHistName`;

	  if ( ($x1[$#x1]==0)){# || ($x2[$#x2]==0) ) { # does not exist
	      $passTag=0;
	  } else { # exist but has 0 size
	      chomp($y1[0]); #chomp($y2[0]);
	      if ( (!(-s "$y1[0]")) ) #|| (!(-s "$y2[0]")) ) 
	      { $passTag=0;}
	  }

      }

      print "$passTag \n";

      if ($passTag==1){
	  print `ln -s $eachStdHistFile $pwd/$combinedDir/$eachCent/$eachStdHistFileNoPath`;
#	  print `ln -s $eachCumulHistFile $pwd/$combinedDir/$eachCent/$eachCumulHistFileNoPath`;
      }




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
 print combineMacro  "#include \"TObjString.h\" \n";
 print combineMacro  "#include \"TVectorD.h\" \n";
 print combineMacro  "#include \"Stiostream.h\" \n";
 print combineMacro  "void $combineMacroName(const char* histoFileName, const char* newfilename) {  \n";
 print combineMacro  "  TObjArray* toBeAddedFiles = new TObjArray(); \n";

$m=1;
foreach $eachStdHistFile ( glob("$pwd/$combinedDir/$eachCent/sched*$stdHistName") ) {
chomp($eachStdHistFile);

      @fields = split(/\//,$eachStdHistFile) ;
      $eachStdHistFileNoPath= $fields[$#fields];
      chomp $eachStdHistFileNoPath;

      ($eachJobPrefix = $eachStdHistFileNoPath) =~ s/$stdHistName//;
      chomp $eachJobPrefix;

 #     ($eachCumulHistFileNoPath = $eachStdHistFileNoPath) =~ s/$stdHistName/$cumulHistName/;
  #    chomp $eachCumulHistFileNoPath;

      ($eachPath = $eachStdHistFile) =~ s/$eachStdHistFileNoPath//;
      chomp $eachPath;



  if ( (-s "$eachPath$eachStdHistFileNoPath" ))# && (-s "$eachPath$eachCumulHistFileNoPath")  ) 
  {
 print combineMacro  "char  dummyName$m\[256\]; \n";
 print combineMacro  "sprintf(dummyName$m,\"$eachPath$eachJobPrefix\%s\",histoFileName); \n";
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
# print combineMacro  "        newobj->Write(); \n";
 print combineMacro  "        newobj->Write(key->GetName(),TObject::kOverwrite | TObject::kSingleKey); \n";
 print combineMacro  "       delete newobj; \n";
 print combineMacro  "   } \n";

#### temporary fix for TVectorD. 
#### in new ROOT, TVectorD can not be copied from one file to another !

 print combineMacro  "  newfile->cd(); \n";

 print combineMacro  "  TVectorD* cumulConstants = new TVectorD(30); \n";
 print combineMacro  "   TObjString* cumulMethodTag = new TObjString( \"cumulNew\" ); \n";

 print combineMacro  "  for (int mm=0; mm<30; mm++) (*cumulConstants)(mm) = (*((TVectorD* )oldfile1->Get(\"CumulConstants\")))(mm);\n";

 print combineMacro  "   cumulConstants->Write(\"CumulConstants\",TObject::kOverwrite | TObject::kSingleKey); \n";
 print combineMacro  "   cumulMethodTag->Write(\"CumulMethodTag\",TObject::kOverwrite | TObject::kSingleKey); \n";

#### end of temporary fix

 print combineMacro  "   gROOT->cd(); \n";
 print combineMacro  "  delete key; \n";
 print combineMacro  "    newfile->Write(); \n";
 print combineMacro  "    newfile->Close(); \n";
 print combineMacro  " } \n";

 close combineMacro;


# print `bsub  -u aihong -q star_cas_short -sp 100 -L /bin/tcsh -J $combinedDir/$eachCent -o $pwd/$combinedDir/$eachCent/$combineMacroName.log root4star -q -b '$pwd/$combinedDir/$eachCent/$combineMacroName.C\+\+(\"$cumulHistName\", \"$pwd/$combinedDir/$eachCent/$cumulHistName\"\)'`;
 print `bsub  -u aihong -q star_cas_short -sp 100 -L /bin/tcsh -J $combinedDir/$eachCent -o $pwd/$combinedDir/$eachCent/$combineMacroName.log root4star -q -b '$pwd/$combinedDir/$eachCent/$combineMacroName.C\+\+(\"$stdHistName\", \"$pwd/$combinedDir/$eachCent/$stdHistName\"\)'`;





# print ` root4star -q -b '$pwd/$combinedDir/$eachCent/$combineMacroName.C\+\+(\"$cumulHistName\", \"$pwd/$combinedDir/$eachCent/$cumulHistName\"\)'`;
# print ` root4star -q -b '$pwd/$combinedDir/$eachCent/$combineMacroName.C\+\+(\"$stdHistName\", \"$pwd/$combinedDir/$eachCent/$stdHistName\"\)'`;


}


 
exit;


