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


#include <string.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TTree.h"
#include "TKey.h"
#include "Riostream.h"

TList *fileList;
TDirectory *targetFile;

void MergeRootfile( TDirectory *targetFile, TList *sourcelist );


void AddHistos(const char * file1, 
	       const char * file2, 
	       const char * target) 
{
  targetFile = TFile::Open(target, "RECREATE" );
  fileList = new TList();
  fileList->Add( TFile::Open(file1));
  fileList->Add( TFile::Open(file2));
  MergeRootfile( targetFile, fileList );
}
   
void AddHistos(const char * file1, 
	       const char * file2, 
	       const char * file3, 
	       const char * target) 
{
  targetFile = TFile::Open(target, "RECREATE" );
  fileList = new TList();
  fileList->Add( TFile::Open(file1));
  fileList->Add( TFile::Open(file2));
  fileList->Add( TFile::Open(file3));
  MergeRootfile( targetFile, fileList );
} 

void AddHistos(const char * file1, 
	       const char * file2, 
	       const char * file3, 
	       const char * file4, 
	       const char * target) 
{
  targetFile = TFile::Open(target, "RECREATE" );
  fileList = new TList();
  fileList->Add( TFile::Open(file1));
  fileList->Add( TFile::Open(file2));
  fileList->Add( TFile::Open(file3));
  fileList->Add( TFile::Open(file4));
  MergeRootfile( targetFile, fileList );
} 

void AddHistos(const char * file1, 
	       const char * file2, 
	       const char * file3, 
	       const char * file4, 
	       const char * file5, 
	       const char * target) 
{
  targetFile = TFile::Open(target, "RECREATE" );
  fileList = new TList();
  fileList->Add( TFile::Open(file1));
  fileList->Add( TFile::Open(file2));
  fileList->Add( TFile::Open(file3));
  fileList->Add( TFile::Open(file4));
  fileList->Add( TFile::Open(file5));
  MergeRootfile( targetFile, fileList );
} 


void MergeRootfile( TDirectory *targetFile, TList *sourcelist ) {

  //  cout << "Target path: " << targetFile->GetPath() << endl;
  TString path( (char*)strstr( targetFile->GetPath(), ":" ) );
  path.Remove( 0, 2 );

  TFile *first_source = (TFile*)sourcelist->First();
  first_source->cd( path );
  TDirectory *current_sourcedir = gDirectory;

  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey( current_sourcedir->GetListOfKeys() );
  TKey *key;
  while ( (key = (TKey*)nextkey())) {

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
      targetFile->cd();
      TDirectory *newdir = targetFile->mkdir( obj->GetName(), obj->GetTitle() );

      // newdir is now the starting point of another round of merging
      // newdir still knows its depth within the target file via
      // GetPath(), so we can still figure out where we are in the recursion
      MergeRootfile( newdir, sourcelist );

    } else {

      // object is of no type that we know or can handle
      cout << "Unknown object type, name: " 
           << obj->GetName() << " title: " << obj->GetTitle() << endl;
    }

    // now write the merged histogram (which is "in" obj) to the target file
    // note that this will just store obj in the current directory level,
    // which is not persistent until the complete directory itself is stored
    // by "targetFile->Write()" below
    if ( obj ) {
      targetFile->cd();

      //!!if the object is a tree, it is stored in globChain...
	if(obj->IsA()->InheritsFrom( "TTree" ))
	  globChain->Write( key->GetName() );
	else
	obj->Write( key->GetName() );
    }

  } // while ( ( TKey *key = (TKey*)nextkey() ) )

  // save modifications to target file
  targetFile->Write();

}


