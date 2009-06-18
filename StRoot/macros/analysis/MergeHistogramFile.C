/*

 * $Id: MergeHistogramFile.C,v 3.17 2009/06/18 22:01:21 fine Exp $
  Author: Valeri Fine fine@bnl.gov
  Date:   25.06.2006


 */



#ifdef __CINT__
// #  include "TDirIter.cxx"
#else
#  include "TDirIter.h"
#  include "StFileIter.h"
#  include "TFileIter.h"
#  include "TFile.h"
#  include "TTree.h"
#  include "TH1.h"
#  include "TStopwatch.h"
#endif
   
//________________________________________________________________________________
void MergeSimpleHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0) 
{
   // This is the deprecated version. To be dleted after debugging
  if (TargetName && TargetName[0] && inputFilesPattern && inputFilesPattern[0] ) {
    TStopwatch time;
    Int_t fileCounter = 0;
    Int_t histogramCounter = 0;
     // Create the output file
     TFile *outFile = TFile::Open(TargetName,"RECREATE");     
     TDirIter listOfFiles(inputFilesPattern);
     const char *fileName = 0;
     while ( (fileName =  listOfFiles.NextFile() ) ) {
        printf(".");
        fileCounter++;
        TFileIter file(fileName);
        TObject *obj = 0;
        while ( (obj = *file) ) {
           if ( obj->IsA()->InheritsFrom( "TH1" ) ) {
              // descendant of TH1 -> merge it
              // printf("Merging histogram: %s\n",obj->GetName() ); 
//              std::cout << "Merging histogram " << obj->GetName() << std::endl;
              TH1 *h1 = (TH1*)obj;
              TH1 *dstHistogram = 0;
              // Check whether we found the new histogram
              if ( (dstHistogram = (TH1 *)outFile->FindObject(h1->GetName()))) {
                 // Accumulate  the  histogram
                  dstHistogram->Add(h1);
                  delete h1;  // Optional, to reduce the memory consumption
                  printf("+");
              } else {
                // First time - move the histogram
                h1->SetDirectory(outFile);
                printf(" The new Histogram found: %s \n", h1->GetName() );
                histogramCounter++;
              }
           } else {
              // printf("Skipping object: %s\n",obj->GetName() ); 
           }
           ++file;
        }
              
     }
     printf("\n Finishing  . . . \n");
     outFile->ls();
     outFile->Write();
     outFile->Close();     
     delete outFile;
     printf(" Total files merged: %d \n", fileCounter);
     printf(" Total histograms merged: %d \n", histogramCounter);
     time.Print("Merge");
  } else {
     printf("\nUsage: root MergeHistogramFile.C(\"DestinationFileName\",\"InputFilesPattern\",kTRUE)\n");     
     printf("------        where InputFilesPattern  ::= <regexp_pattern_for_the_input_files>|@indirect_file_list\n");
     printf("                    indirect_file_list ::= a text file with the list of the files\n");
     printf("                    indirect_file_list can be create by the shell command:\n");
     printf("                         ls -1 --color=never *.root>indirect_file_list \n\n");
  }
}

//________________________________________________________________________________
void MergeComplexHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0) 
{
  if (TargetName && TargetName[0] && inputFilesPattern && inputFilesPattern[0] ) {
    printf(" An experimental version of macro.\n");
    TStopwatch time;
    Int_t fileCounter = 0;
    Int_t dirCounter = 0;
    Int_t treeCounter = 0;
    Int_t histogramCounter = 0;
     // Create the output file
     TFile *outFile = TFile::Open(TargetName,"RECREATE");
     TDirectory *outDir = outFile;
     TDirIter listOfFiles(inputFilesPattern);
     const char *fileName = 0;
     while ( (fileName =  listOfFiles.NextFile() ) ) {
        Int_t currentDirDepth = 0;
        printf(".");
        fileCounter++;
        StFileIter file(fileName);
        TObject *obj = 0;
        while ( (obj = *file) ) {
           Int_t depth = file.GetDepth();
           while (depth < currentDirDepth) {
                outDir = outDir->GetMotherDir();
                currentDirDepth--;
           }
           if ( obj->IsA()->InheritsFrom(TH1::Class()) ) {
              // descendant of TH1 -> merge it
              // printf("Merging histogram: %s\n",obj->GetName() ); 
//              std::cout << "Merging histogram " << obj->GetName() << std::endl;
              TH1 *h1 = (TH1*)obj;
              TH1 *dstHistogram = 0;
              // Check whether we found the new histogram
              if ( (dstHistogram = (TH1 *)outDir->FindObject(h1->GetName()))) {
                 // Accumulate  the  histogram
                  dstHistogram->Add(h1);
                  delete h1;  // Optional, to reduce the memory consumption
                  printf("h");
              } else {
                // First time - move the histogram
                h1->SetDirectory(outDir);
                printf(" The new Histogram found: %s \n", h1->GetName() );
                histogramCounter++;
              }
           } else  if ( obj->IsA()->InheritsFrom(TTree::Class()) ) {
              // descendant of TTree  -> merge it
              // printf("Merging Tree %p:%s\n",obj, obj->GetName() ); 
              TTree *tree = (TTree*)obj;
              TTree *dstTree = 0;
              // Check whether we found the new histogram
              if ( (dstTree = (TTree *)outDir->FindObject(tree->GetName()))) {
                   // printf("Merging %p:%s with the existing Tree %p:%s\n"
                   //       ,tree,tree->GetName(),dstTree, dstTree->GetName() ); 
                  // Merge  the  tree
                  TList *nextTree = new TList(); nextTree->Add(tree); 
                  dstTree->Merge(nextTree);
                  delete tree;  // Optional, to reduce the memory consumption
                  delete nextTree;
                  printf("t");
              } else {
                // First time - move the TTree
                TDirectory *saveDir = 0;
                if (outDir != gDirectory) {
                   saveDir = gDirectory;
                   outDir->cd();
                }
                TList *nextTree = new TList(); nextTree->Add(tree);               
                dstTree = TTree::MergeTrees(nextTree);
                if (saveDir) saveDir->cd();
                // printf(" The new TTree found: %p:%s \n",tree, tree->GetName() );
                // printf(" Create the destination Tree %p:%s\n\n",dstTree, dstTree->GetName() );
                delete tree;  // Optional, to reduce the memory consumption
                delete nextTree;
                treeCounter++;
              }
           } else if ( obj->IsA()->InheritsFrom(TDirectory::Class()) ) {
               printf("The input sub-TDirectory object: %s depth=%d\n",obj->GetName(), depth); 
               TDirectory *d =  (TDirectory *)outDir->FindObject(obj->GetName());
               if (!d) {
                  d = outDir->mkdir(obj->GetName());
                  dirCounter++;
                  printf("The new TDirectory object: %s depth=%d\n",d->GetPathStatic(), depth); 
               }
               if (d) {
                  outDir = d;                  
                  printf("The output sub-TDirectory object: %s depth=%d\n",outDir->GetPathStatic(), depth); 

               }
           } else {
              printf("I have no idea how to merge the %s objects of the %s class. Skipping .... \n",obj->GetName(), obj->ClassName() ); 
           }
           ++file;
        }
              
     }
     printf("\n Finishing  . . . \n");
     outFile->Write();  // this creates a second copy of the TTree ???
     outFile->Close();     
     delete outFile;
     if (fileCounter)      printf(" Total files merged: %d \n", fileCounter);
     if (dirCounter)       printf(" Total TDirectory objects merged: %d \n", dirCounter);
     if (histogramCounter) printf(" Total histograms merged: %d \n", histogramCounter);
     if (treeCounter)      printf(" Total TTree\'s merged: %d \n",treeCounter);
     if (dirCounter || treeCounter) printf(" You have used the experimental version of the program. Please check the output file\n");
        
     time.Print("Merge");
  } else {
     printf("\nUsage: root MergeHistogramFile.C(\"DestinationFileName\",\"InputFilesPattern\")\n");     
     printf("------        where InputFilesPattern  ::= <regexp_pattern_for_the_input_files>|@indirect_file_list\n");
     printf("                    indirect_file_list ::= a text file with the list of the files\n");
     printf("                    indirect_file_list can be create by the shell command:\n");
     printf("                         ls -1 *.root>indirect_file_list \n\n");
  }
}



//________________________________________________________________________________
void MergeHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0, Bool_t simple=kFALSE) 
{  
   if (TargetName && TargetName[0] && inputFilesPattern && inputFilesPattern[0] ) {
      // by default hadd can merge Trees in a file that can go up to 100 Gbytes
      Long64_t maxsize = 100000000; //100GB
      maxsize *= 100;  //to bypass some compiler limitations with big constants
      TTree::SetMaxTreeSize(maxsize);
      if (simple) {
         // Use the deprecated version
         MergeSimpleHistogramFile(TargetName, inputFilesPattern);
      } else {
#ifdef __CINT__         
         gSystem->Load("St_base");
#endif         
         MergeComplexHistogramFile(TargetName, inputFilesPattern);
      }
  } else {
     printf("\nUsage: root MergeHistogramFile.C(\"DestinationFileName\",\"InputFilesPattern\",kTRUE)\n");     
     printf("------        where InputFilesPattern  ::= <regexp_pattern_for_the_input_files>|@indirect_file_list\n");
     printf("                    indirect_file_list ::= a text file with the list of the files\n");
     printf("                    indirect_file_list can be create by the shell command:\n\n");
     printf("                         ls -1 --color=never *.root>indirect_file_list \n\n");
     printf("                    The last parameter defines whether one wants to merge the \"simple\" ROOT files\n");
     printf("                    The \"simple\" ROOT files are those with no sub-TDirectrory objects inside and with no TTree/TNtuples\n");
     printf("                    This is the default option and it can be omitted\n");
     printf("                    To merge the ROOT files with sub-TDirectory or / and TTree pass the kFALSE as the third macro parameter\n\n");
     printf("------------- To use with ACliC - load the \"St_base.so\" shared library first\n");
     
  }
}
