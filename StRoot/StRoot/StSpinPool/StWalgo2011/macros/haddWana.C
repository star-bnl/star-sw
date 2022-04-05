//$Id: haddWana.C,v 1.2 2012/03/12 23:11:43 smirnovd Exp $
// line added after tag=DNP2009 by Jan
// run list chaned to match final run selection for SL09g

#include <string.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TTree.h"
#include "TKey.h"
#include "Riostream.h"

TList *FileList;
TFile *Target;


int isAPS2010pol = 0; /* affects only setP1-P4, not setA-D,
		       set it to 0 to see all usable polarized fills */



void MergeRootfile( TDirectory *target, TList *sourcelist );


void haddWana(char *set = "sumFeb21_2011", TString iPath = "./")
{
   TString out = iPath;

   if (strstr("sumFeb21_2011", set) > 0) {
      char *runL = " cutsOnly_Feb21 raw_sumFeb21";
   }
   else if (strstr("setXX", set) > 0) {
      char *runL = "HHHHH "; //total:  NNN
   }
   else {
      printf(" hadd: set=%s= NOT found, quit\n", set);
      return;
   }
   printf(" hadd: set=%s= path=%s= ...\n", set, iPath.Data());
   out += set;
   Target = TFile::Open( out + ".wana.hist.root", "RECREATE" );
   FileList = new TList();
   printf("sum Output '%s' \n", Target->GetName());

   char *run = strtok(runL, " "); // init 'strtok'
   int i = 1;
   do {
      printf("add run %d '%s' \n", i++, run);
      if (isAPS2010pol && strstr(set, "run9setP") > 0 ) { // remove runs from fills w/o official pol for APS-2010
         int irun = atoi(run + 1);
         if (irun >= 10083013 && irun <= 10083058) {
            printf("\tdrop %s from F10415\n", run);
            continue;
         }
         if (irun >= 10098015 && irun <= 10098015) {
            printf("\tdrop %s from F10508\n", run);
            continue;
         }
      }
      TString fullName = iPath + run + ".wana.hist.root";
      FileList->Add( TFile::Open(fullName));
   }
   while (run = strtok(0, " ")); // advance by one nam

   MergeRootfile(  Target, FileList );
   printf("finished Output '%s' \n", Target->GetName());

}

void MergeRootfile(  TDirectory *target, TList *sourcelist )
{

   cout << "Target path: " << target->GetPath() << endl;
   TString path( (char*)strstr( target->GetPath(), ":" ) );
   path.Remove( 0, 2 );

   TFile *first_source = (TFile*)sourcelist->First();
   first_source->cd( path );
   TDirectory *current_sourcedir = gDirectory;

   int nh = 0;
   // loop over all keys in this directory
   TChain *globChain = 0;
   TIter nextkey( current_sourcedir->GetListOfKeys() );
   TKey *key;
   while ( (key = (TKey*)nextkey())) {
      const char *name = key->GetName();
      nh++;
      if (nh % 100 == 0) printf("nh=%d addingX %s\n", nh, name);

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
         const char* obj_name = obj->GetName();

         globChain = new TChain(obj_name);
         globChain->Add(first_source->GetName());
         TFile *nextsource = (TFile*)sourcelist->After( first_source );
         //      const char* file_name = nextsource->GetName();
         // cout << "file name  " << file_name << endl;
         while ( nextsource ) {

            globChain->Add(nextsource->GetName());
            nextsource = (TFile*)sourcelist->After( nextsource );
         }

      }
      else if ( obj->IsA()->InheritsFrom( "TDirectory" ) ) {
         // it's a subdirectory

         cout << "Found subdirectory " << obj->GetName() << endl;

         // create a new subdir of same name and title in the target file
         target->cd();
         TDirectory *newdir = target->mkdir( obj->GetName(), obj->GetTitle() );

         // newdir is now the starting point of another round of merging
         // newdir still knows its depth within the target file via
         // GetPath(), so we can still figure out where we are in the recursion
         MergeRootfile( core, newdir, sourcelist );

      }
      else {

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
         if (obj->IsA()->InheritsFrom( "TTree" ))
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



// $Log: haddWana.C,v $
// Revision 1.2  2012/03/12 23:11:43  smirnovd
// *** empty log message ***
//
// Revision 1.1  2011/02/23 14:00:12  balewski
// *** empty log message ***
//
// Revision 1.18  2010/05/19 20:49:50  balewski
// removed 5 runs tagged by Ross
//
// Revision 1.17  2010/03/15 17:05:51  balewski
// cleanup, used for W AL sort March 15, 2010
//
// Revision 1.16  2010/03/14 19:37:27  balewski
// Removed F10383, has TPC problem according to Gene
//
// Revision 1.15  2010/02/24 18:26:24  balewski
// added macros computing/plotting AL
//
// Revision 1.14  2010/02/06 01:12:46  balewski
// skips unpol CNI fills
//
// Revision 1.13  2010/02/04 03:48:25  balewski
// add ET for lumi monitor
//
// Revision 1.12  2010/01/27 22:12:26  balewski
// spin code matched to x-section code
//
// Revision 1.11  2010/01/10 03:01:39  balewski
// cleanup & nicer histos
//
// Revision 1.10  2010/01/06 05:21:59  balewski
// cleanup
//
// Revision 1.9  2010/01/06 04:22:18  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.8  2010/01/05 03:23:02  balewski
// change logic for filling btow status tables, added printout to Z-code
//
// Revision 1.7  2010/01/04 05:12:02  balewski
// added 4x4 cut to Z-algo, cleanup
//
// Revision 1.6  2010/01/03 04:38:27  balewski
// reorganized Z-algo
//
// Revision 1.5  2010/01/03 01:58:19  balewski
// run list for setABCD updated to SL09g
//
// Revision 1.4  2009/12/30 18:27:52  balewski
// after tag I added a test line
//
// Revision 1.3  2009/12/30 18:27:02  balewski
// added tag for testing
//
// Revision 1.2  2009/12/08 16:53:01  balewski
// *** empty log message ***
//
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//
