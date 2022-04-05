#include <string.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TTree.h"
#include "TKey.h"


TList *FileList;
TFile *Target;

void MergeRootfile( TDirectory *target, TList *sourcelist );

void hadd(int set=1) {
  TString iPath;
   iPath="/star/data05/scratch/balewski/sched-calibTPv6/R";
  

  TString out=iPath;
  out="./";
  char *runL=0;
  if(set==1) {//fms day 43-50
    runL="9043035 9043036 9043040 9043041 9043042 9043043 9043044 9043047 9043048 9043061 9044023 9044034 9044035 9045006 9045007 9045011 9045014 9045020 9045021 9046008 9046009 9046015 9046016 9046017 9046024 9046025 9046026 9046029 9046030 9046031 9046035 9046037 9046071 9046072 9046078 9046082 9046084 9046087 9046089 9046091 9046093 9046094 9046095 9046098 9046099 9046103 9046104 9047001 9047013 9047029 9047031 9047037 9047038 9047039 9047043 9047070 9047071 9047075 9047077 9047078 9047081 9047082 9047083 9048005 9048007 9048008 9048036 9048042 9048044 9049016 9049017 9049018 9049040 9049044 9049045 9049048 9049049 9049050";  out+="sumD43.barCal";
  } else if(set==2) {//fms day 50-53
    runL="9050016 9050017 9050018 9050030 9050031 9050033 9050034 9050035 9050051 9050052 9050070 9050071 9050074 9050076 9050077 9050078 9050079 9050080 9050081 9050082 9050089 9050090 9050091 9050092 9050093 9050103 9050179 9050185 9050189 9051001 9051002 9051003 9051007 9051012 9051015 9051083 9051084 9051086 9051090 9051091 9051092 9051095 9051096 9051102 9051103 9052002 9052003 9052005 9052006 9052007 9052008 9052009 9052030 9052032 9052033 9052037 9052038 9052039 9052042 9052043 9052044 9052045 9052046 9052047 9052054 9052055 9052056 9052057 9052058 9053005 9053006 9053010 9053011 9053012 9053017 9053018 9053019 9053020 9053021 9053023 9053024 9053027 9053031 9053037 9053039 9053040 9053041 9053058 9053060 9053062 9053063 9053081 9053082 9053083 9053084 9053108 9053115 9053116 9053117 9053119 9053121 9053123 9053129 9053130 9053132 9053136 9053137 9053138 9053140";  out+="sumD50.barCal";
  } else if(set==3) {//fms day 54-58
    runL="9054001 9054002 9054008 9054009 9054010 9054012 9054013 9054019 9054021 9054022 9054023 9054028 9054029 9054030 9054031 9054032 9054033 9054034 9054035 9054036 9054042 9054043 9054044 9054045 9054046 9054057 9054058 9054059 9054064 9054065 9054067 9054069 9054071 9054072 9054073 9054074 9054075 9054076 9054080 9054083 9054084 9054085 9055012 9055013 9055014 9055023 9055024 9055025 9055028 9055029 9055031 9055032 9055036 9056005 9056008 9056009 9056013 9056014 9056016 9056017 9056019 9056020 9056021 9056022 9056025 9056029 9056030 9056031 9056036 9056037 9056038 9056039 9056067 9056068 9056069 9056074 9056076 9056077 9056079 9056081 9056082 9056085 9056087 9056091 9056092 9056093 9056095 9056097 9056102 9057004 9057009 9057011 9057013 9057014 9057020 9057026 9057028 9057030 9057032 9057037 9057039 9057042 9057043 9057045 9057047 9057069 9057073 9057076 9057078 9057079 9057081 9057082 9057086 9057087 9058055 9058064 9058068 ";  out+="sumD54.barCal";
  } else if(set==4) {  // fms day 59 
    runL="9059003 9059004 9059005 9059007 9059009 9059013 9059014 9059015 9059018 9059041 9059043 9059044 9059046 9059051 9059052 9059057 9059058 9059059 9059060 9059061 9059069 9060008 9060015 9062008 9062009 9062010 9062012 9062013 9062014 9062015 9062016 9062019 9062020 9062033 9062034 9062035 9062036 9062039 9062043 9062044 9062045 9062046 9062047 9062057 9062081 9062082 9062085 9062087 9062088 9062101 9062102 9062109 9062110 9063037 9063121 9063126 9063138 9063139 9063142 9063143 9064001 9064002 9064005 9064006 9064009 9064010 9064011 9060016 9060017 9060018 9060022 9060023 9060027 9060031 9060032 9060036 9060040 9060041 9060064 9060068 9060070 9060074 9060075 9060077 9060078 9060079 9060085 9060086 9060087 9061007 9061008 9061011 9061013 9061014 9061015 9061019 9061022 9061023 9061026 9061028 9061030 9061031 9061037 9061051 9061067 9061082 9061096 9061098 9061099 9061100 9061101 9061102 9061103 9061104 9061105";  out+="sumD59.barCal";
  } else if(set==5) {   // fms Day 64
    runL="9064019 9064020 9064023 9064025 9064027 9064029 9064030 9064031 9064032 9064033 9064036 9064037 9064041 9064042 9064043 9065009 9065010 9065016 9065018 9065021 9065023 9065026 9065030 9065031 9065032 9065057 9065058 9065062 9065063 9065065 9065068 9065073 9066001 9066012 9066013 9066015 9066016 9066027 9066036 9066042 9066045 9066049 9066053 9066058 9066059 9066065 9066066 9066068 9066070 9066071 9066074 9066086 9066087 9067005 9067007 9067008 9067010 9067013 9067018 9067019 9067020 9067022 9067023 9067028 9067040 9067041 9067044 9067045 9067060 9067061 9067063 9067064 9067065 9067095 9068008 9068009 9068010 9068012 9068014 9068015 9068021 9068022 9068032 9068035 9068037 9068038 9068055 9068056 9068058 9068063 9068066 9068074 9068075 9068078 9068079 9068080 9068081 9068082 9068084 9068085 9068086 9068087 9068088 9068124 9069005 9069009 9069010 9069011 9069013 9069014 9069021 9069023 9069024 9069025 9069042 9069053 9069058 9069059 9069064 9069065 9069066 9069079 9070006 9070007";  out+="sumD64.barCal";
  } else if(set==0) { // all together, second pass 
    char *runL="sumD43 sumD50 sumD54 sumD59 sumD64";  out+="sumD43-70"; iPath="./";
  } else assert(7==8);

  Target = TFile::Open( out+".hist.root", "RECREATE" );
  printf("Output-->%s\n",Target->GetName());
  FileList = new TList();

  char *run=strtok(runL," "); // init 'strtok'
  int i=0;
  int m0=0,m1=0;
  do {
    printf("add run i=%d '%s'  nFound=%d\n",i++,run,m1+1);
    TString fullName=iPath+run+".barCal.hist.root";  
    fd=TFile::Open(fullName);
    if(fd==0){m0++; continue;}
    m1++;
    FileList->Add( fd);
    // if(m1>3) break;//tmp
  } while(run=strtok(0," "));  // advance by one nam

  MergeRootfile(  Target, FileList );
  printf("%d files on merge list, found %d, missed %d\n",i,m1,m0); 
}   

void MergeRootfile(  TDirectory *target, TList *sourcelist ) {

  //  cout << "Target path: " << target->GetPath() << endl;
  TString path( (char*)strstr( target->GetPath(), ":" ) );
  path.Remove( 0, 2 );

  TFile *first_source = (TFile*)sourcelist->First();
  first_source->cd( path );
  TDirectory *current_sourcedir = gDirectory;

  int nh=0;
  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey( current_sourcedir->GetListOfKeys() );
  TKey *key;
  while ( (key = (TKey*)nextkey())) {
    const char *name=key->GetName();
    if(nh%5==0) printf("nh=%d addingX %s\n",nh,name);
    nh++;
    
    // read object from first source file
    first_source->cd( path );
    TObject *obj = key->ReadObj();
    
    if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
      // descendant of TH1 -> merge it
      
      //      cout << "Merging histogram " << obj->GetName() << endl;
      TH1 *h1 = (TH1*)obj;

      // loop over all source files and add the content of the
      // correspondant histogram to the one pointed to by "h1"
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      while ( nextsource ) {
        
        // make sure we are at the correct directory level by cd'ing to path
        nextsource->cd( path );
        TH1 *h2 = (TH1*)gDirectory->Get( h1->GetName() );
        if ( h2 ) {
          h1->Add( h2 );
          delete h2; // don't know if this is necessary, i.e. if 
                     // h2 is created by the call to gDirectory above.
        }

        nextsource = (TFile*)sourcelist->After( nextsource );
      }
    }
    else if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
      
      // loop over all source files create a chain of Trees "globChain"
      const char* obj_name= obj->GetName();

      globChain = new TChain(obj_name);
      globChain->Add(first_source->GetName());
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      //      const char* file_name = nextsource->GetName();
      // cout << "file name  " << file_name << endl;
     while ( nextsource ) {
     	  
       globChain->Add(nextsource->GetName());
       nextsource = (TFile*)sourcelist->After( nextsource );
     }
     
    } else if ( obj->IsA()->InheritsFrom( "TDirectory" ) ) {
      // it's a subdirectory
      
      cout << "Found subdirectory " << obj->GetName() << endl;
      
      // create a new subdir of same name and title in the target file
      target->cd();
      TDirectory *newdir = target->mkdir( obj->GetName(), obj->GetTitle() );

      // newdir is now the starting point of another round of merging
      // newdir still knows its depth within the target file via
      // GetPath(), so we can still figure out where we are in the recursion
      MergeRootfile( core,newdir, sourcelist );
      
    } else {
      
      // object is of no type that we know or can handle
      cout << "Unknown object type, name: " 
           << obj->GetName() << " title: " << obj->GetTitle() << endl;
    }
    
    // now write the merged histogram (which is "in" obj) to the target file
    // note that this will just store obj in the current directory level,
    // which is not persistent until the complete directory itself is stored
    // by "target->Write()" below
    if ( obj ) {
      target->cd();
      
      //!!if the object is a tree, it is stored in globChain...
      if(obj->IsA()->InheritsFrom( "TTree" ))
	globChain->Write( key->GetName() );
      else
	obj->Write( key->GetName() );
    }
    
  } // while ( ( TKey *key = (TKey*)nextkey() ) )
  
  // save modifications to target file
  target->Write();
  
}


/*

  This macro will add histograms from a list of root files and write them
  to a target root file. The target file is newly created and must not be
  identical to one of the source files.

  Author: Sven A. Schmidt, sven.schmidt@cern.ch
  Date:   13.2.2001

  This code is based on the hadd.C example by Rene Brun and Dirk Geppert,
  which had a problem with directories more than one level deep.
  (see macro hadd_old.C for this previous implementation).
  
  The macro from Sven has been enhanced by 
     Anne-Sylvie Nicollerat <Anne-Sylvie.Nicollerat@cern.ch>
   to automatically add Trees (via a chain of trees).
  
  To use this macro, modify the file names in function hadd.
  
  NB: This macro is provided as a tutorial.
      Use $ROOTSYS/bin/hadd to merge many histogram files

 */

