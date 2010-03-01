#include "StCloseFileOnTerminate.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TFile.h"

StCloseFileOnTerminate *StCloseFileOnTerminate::fgCloseFileOnTerminate = 0;
//_________________________________________________________
StCloseFileOnTerminate &StCloseFileOnTerminate::Instantiate()
{
   // Create Asynch signal handler

  if (! fgCloseFileOnTerminate ) {
     fgCloseFileOnTerminate = new StCloseFileOnTerminate;
     fgCloseFileOnTerminate->Add(); 
  }
  return *fgCloseFileOnTerminate;
}
//_________________________________________________________
Bool_t StCloseFileOnTerminate::Notify() {
  // close all TFile
  Error(__FUNCTION__," Closing files   . . . . ");
  TSeqCollection   *files = gROOT->GetListOfFiles();
  int count = 0;
  if (files && files->GetSize() >0 ) {
     TIter next(files);
      while( TFile *f = (TFile *)next() ) { f->Close(); ++count; }
      files->Delete();
  }
  if (count) Error(__FUNCTION__, "%d files have been closed", count);
  else Print(" There was no open file to close");
  return kTRUE;
}
