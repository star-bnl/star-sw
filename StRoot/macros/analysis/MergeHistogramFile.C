/*

 * $Id: MergeHistogramFile.C,v 3.3 2006/07/25 22:42:02 fine Exp $
  Author: Valeri Fine fine@bnl.gov
  Date:   25.06.2006


 */



#ifdef __CINT__
// #  include "TDirIter.cxx"
#else
#  include "TDirIter.h"
#  include "TFileIter.h"
#  include "TFile.h"
#  include "TH1.h"
#  include "TStopwatch.h"
#endif
   
void MergeHistogramFile( const Char_t *TargetName=0, const Char_t *inputFilesPattern=0) 
{
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
              //      cout << "Merging histogram " << obj->GetName() << endl;
              TH1 *h1 = (TH1*)obj;
              TH1 *dstHistogram = 0;
              // Check whether we found the new histogram
              if ( (dstHistogram = (TH1 *)outFile->FindObject(h1->GetName()))) {
                 // Accumulate  the  histogram
                  dstHistogram->Add(h1);
                  printf("+");
              } else {
                // First time - move the histogram
                h1->SetDirectory(outFile);
                printf(" The new Histogram found: %s \n", h1->GetName() );
                histogramCounter++;
              }
              ++file;
           }
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
