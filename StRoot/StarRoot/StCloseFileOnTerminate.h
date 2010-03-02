#ifndef STAR_STCLOSEFILEONTERMINATE
#define STAR_STCLOSEFILEONTERMINATE
// $Id: StCloseFileOnTerminate.h,v 1.4 2010/03/02 00:41:25 fine Exp $

#include "TSysEvtHandler.h"

//! \author Valery Fine(fine@bnl.gov)
//! \date 27=5/02/2010

/*! \brief  Class StTerminateNotified is a abstract visitor to propagate the termination signal
 */
class StTerminateNotified {
    public:
      StTerminateNotified(){;}
      virtual ~StTerminateNotified(){;}
      virtual  void SetNotified() = 0;
      virtual  bool Notified() const = 0;
};
/*! \brief  Class StCloseFileOnTerminate - catch the SIGTERM signal (sent by Condor for example) to close all open ROOT files.
 */
class StCloseFileOnTerminate : public TSignalHandler {
 private:
       static StCloseFileOnTerminate *fgCloseFileOnTerminate;
       StTerminateNotified  *fNotificator;
 protected:
       StCloseFileOnTerminate() : TSignalHandler(kSigTermination, kFALSE),fNotificator(0) { }
 public:
       //! Create an instance of the signal handler (Should be called at once  by code)
       static StCloseFileOnTerminate &Instantiate();
       //! Close the open ROOT files upon SIGTERM sent by system to terminate the job
       virtual Bool_t Notify();
       virtual void SetNotificator( StTerminateNotified *ms){ fNotificator=ms; }
       StTerminateNotified  *Notificator() const { return fNotificator; }
       ClassDef(StCloseFileOnTerminate,0)
};
#endif
 
