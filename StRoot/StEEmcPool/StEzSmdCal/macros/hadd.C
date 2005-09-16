#include <string.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TTree.h"
#include "TKey.h"
#include "Riostream.h"

TList *FileList;
TFile *Target;

void MergeRootfile( TDirectory *target, TList *sourcelist );

void hadd(int sectID=5) {
  TString iPath="/star/data05/scratch/balewski/2005-eemcCal/day49-hist/iter4-out/";
  iPath="iter5-pp/";
  TString out=iPath;

  out+="sum-sect";  out+=sectID;
  Target = TFile::Open( out+".hist.root", "RECREATE" );

  //... CuCu minB events ....
  char *runL="R60490911 R60490912  R60490921 R60490922 R6049126 R60491291 R60491292  R6049130  R6049131 R60500161 R60500162  R60500171  R60500172 R6050018 R6050019 R60500201   R60500202";

  //... pp minB events ....
  char *runL=" R61710371  R61710372  R61710373  R61720911 R61720912 R61720913 R61730681 R61730682 R61730683 R61730771 R61730772 R61730773 ";
  FileList = new TList();

  char *run=strtok(runL," "); // init 'strtok'
  int i=0;
  do {
    printf("add run %d '%s' \n",i++,run);
    TString fullName=iPath+run+".hist.root";  
    FileList->Add( TFile::Open(fullName));
  } while(run=strtok(0," "));  // advance by one nam

  char core[100];
  sprintf(core,"%02d",sectID);
  MergeRootfile( core, Target, FileList );

}   

void MergeRootfile( char *core, TDirectory *target, TList *sourcelist ) {
  printf("merging only histos with core=%s=\n",core);

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
    char * c1=strstr(name,core);
    if(c1==0) continue;
    if(c1-name>2) continue;
    nh++;
    if(nh%100==0) printf("nh=%d addingX %s\n",nh,name);
    
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

