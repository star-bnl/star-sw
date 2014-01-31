#include "StCloseFileOnTerminate.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TApplication.h"
#include "TFile.h"
#include "TError.h"
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
  if (! fgCloseFileOnTerminate ) {
     fgCloseFileOnTerminate = new StCloseFileOnTerminate;
     ::Warning("StCloseFileOnTerminate::Instantiate","Asynch signal handler has been created");
  }
  return *fgCloseFileOnTerminate;
}
//_________________________________________________________
Bool_t StCloseFileOnTerminate::Notify() {
   // close all TFile  & Terminate
#if 1 /* ROOT_VERSION_CODE < ROOT_VERSION(5,31,1) */
   Error(__FUNCTION__," Closing all TFiles   . . . . ");
   TSeqCollection   *files = gROOT->GetListOfFiles();
   int count = 0;
   if (files && files->GetSize() >0 ) {
       TIter next(files);
       while( TFile *f = (TFile *) next() ) { 
	 if ( f-> IsWritable() ) {
	   Error(__FUNCTION__, "file %s will be closed", f->GetName());
	   f->Write();
	   f->Close(); ++count; 
	   Error(__FUNCTION__, "file %s has been closed", f->GetName());
	 }
       }
       files->Delete();
   }
   if (count) Error(__FUNCTION__, "%d files have been closed", count);
   else Print(" There was no open file to close");
#else
   Error(__FUNCTION__,"Close any files or sockets before emptying CINT   . . . . ");
   gROOT->CloseFiles(); 
#endif
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
