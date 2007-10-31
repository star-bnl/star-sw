/*

 * $Id: MergeHistogramFile.C,v 3.6 2007/10/31 22:27:30 fine Exp $
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
#  include "TH1.h"
#  include "TStopwatch.h"
#endif

//________________________________________________________________________________
void MergeHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0, Bool_t simple=kTRUE) 
{  
   if (TargetName && TargetName[0] && inputFilesPattern && inputFilesPattern[0] ) {
      if (simple) {
         // Use the deprecated version
         MergeSimpleHistogramFile(TargetName, inputFilesPattern);
      } else {
         gSystem->Load("St_base");
         MergeComplexHistogramFile(TargetName, inputFilesPattern);
      }
  } else {
     printf("\nUsage: root MergeHistogramFile.C(\"DestinationFileName\",\"InputFilesPattern\",kTRUE)\n");     
     printf("------        where InputFilesPattern  ::= <regexp_pattern_for_the_input_files>|@indirect_file_list\n");
     printf("                    indirect_file_list ::= a text file with the list of the files\n");
     printf("                    indirect_file_list can be create by the shell command:\n\n");
     printf("                         ls -1 *.root>indirect_file_list \n\n");
     printf("                    The last parameter defines whether one wants to merge the \"simple\" ROOT files\n");
     printf("                    The \"simple\" ROOT files are those with no sub-TDirectrory objects inside\n");
     printf("                    This is the default option and it can be omitted\n");
     printf("                    To merge the ROOT files with sub-TDirectory pass the kFALSE as the 3d macro parameter\n\n");
     printf("Attention: Macro doesn\'t merge any TTree yet.\n\n");
  }
}
   
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
     printf("                         ls -1 *.root>indirect_file_list \n\n");
  }
}

//________________________________________________________________________________
void MergeComplexHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0) 
{
  if (TargetName && TargetName[0] && inputFilesPattern && inputFilesPattern[0] ) {
    printf(" An experimental version of macro.\n");
    TStopwatch time;
    Int_t fileCounter = 0;
    Int_t histogramCounter = 0;
    Int_t currentDirDepth = 0;
     // Create the output file
     TFile *outFile = TFile::Open(TargetName,"RECREATE");
     TDirectory *outDir = outFile;
     TDirIter listOfFiles(inputFilesPattern);
     const char *fileName = 0;
     while ( (fileName =  listOfFiles.NextFile() ) ) {
        printf(".");
        fileCounter++;
        StFileIter file(fileName);
        TObject *obj = 0;
        while ( (obj = *file) ) {
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
                  printf("+");
              } else {
                // First time - move the histogram
                h1->SetDirectory(outDir);
                printf(" The new Histogram found: %s \n", h1->GetName() );
                histogramCounter++;
              }
           } else if ( obj->IsA()->InheritsFrom(TDirectory::Class()) ) {
               Int_t depth = file.GetDepth();
               printf("The input sub-TDirectory object: %s\n",obj->GetName(), depth); 
               if (depth = currentDirDepth) outDir = outDir->GetMotherDir();
               TDirectory *d =  (TDirectory *)outDir->FindObject(obj->GetName());
               if (!d) d = outDir->mkdir(obj->GetName());
               if (d) {
                  outDir = d;                  
                  printf("The output sub-TDirectory object: %s\n",outDir->GetPathStatic(), depth); 

               }
           } else {
              printf("Skipping object: %s\n",obj->GetName() ); 
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
     printf(" Total histogram merged: %d \n", histogramCounter);
     time.Print("Merge");
  } else {
     printf("\nUsage: root MergeHistogramFile.C(\"DestinationFileName\",\"InputFilesPattern\")\n");     
     printf("------        where InputFilesPattern  ::= <regexp_pattern_for_the_input_files>|@indirect_file_list\n");
     printf("                    indirect_file_list ::= a text file with the list of the files\n");
     printf("                    indirect_file_list can be create by the shell command:\n");
     printf("                         ls -1 *.root>indirect_file_list \n\n");
  }
}
