#include "StCloseFileOnTerminate.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TApplication.h"
#include "TFile.h"

StCloseFileOnTerminate *StCloseFileOnTerminate::fgCloseFileOnTerminate = 0;
//_________________________________________________________
StCloseFileOnTerminate:: StCloseFileOnTerminate() : TSignalHandler(kSigTermination, kFALSE)
{
   Add();
}
//_________________________________________________________
StCloseFileOnTerminate &StCloseFileOnTerminate::Instantiate()
{
   // Create Asynch signal handler
  if (! fgCloseFileOnTerminate ) 
     fgCloseFileOnTerminate = new StCloseFileOnTerminate;
  return *fgCloseFileOnTerminate;
}
//_________________________________________________________
Bool_t StCloseFileOnTerminate::Notify() {
   // close all TFile  & Terminate
   Error(__FUNCTION__," Closing all TFiles   . . . . ");
   TSeqCollection   *files = gROOT->GetListOfFiles();
   int count = 0;
   if (files && files->GetSize() >0 ) {
       TIter next(files);
       while( TFile *f = (TFile *)next() ) { f->Close(); ++count; }
       files->Delete();
   }
   if (count) Error(__FUNCTION__, "%d files have been closed", count);
   else Print(" There was no open file to close");
   Error(__FUNCTION__,"Terminating  . . . . ");
   gApplication->Terminate(15);
   return kTRUE;
}
//_________________________________________________________
StTerminateNotified::StTerminateNotified() : fTerminateHandler()
{
   fTerminateHandler = new StNotifyOnTerminate(*this);
   // check the global handler 
   if (StCloseFileOnTerminate::Exists()) StCloseFileOnTerminate::Instantiate().DeActivate();
}
//_________________________________________________________
StTerminateNotified::~StTerminateNotified()
{
   fTerminateHandler->DeActivate();
   // restore the global handler if needed
   if (StCloseFileOnTerminate::Exists() && ((&StCloseFileOnTerminate::Instantiate()) !=fTerminateHandler))  
      StCloseFileOnTerminate::Instantiate().Activate();
   delete fTerminateHandler;  fTerminateHandler = 0;
}

//_________________________________________________________
Bool_t StNotifyOnTerminate::Notify() 
{
   fNotificator.SetNotifiedCallBack();
   Error(__FUNCTION__," Job will be terminated soon by the external signal . . . . ");
   return kTRUE;
}
