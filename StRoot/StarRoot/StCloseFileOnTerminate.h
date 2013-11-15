#ifndef STAR_STCLOSEFILEONTERMINATE
#define STAR_STCLOSEFILEONTERMINATE
// $Id: StCloseFileOnTerminate.h,v 1.7 2011/06/20 22:36:48 fisyak Exp $

#include "TSysEvtHandler.h"

//! \author Valery Fine(fine@bnl.gov)
//! \date 27=5/02/2010
class StNotifyOnTerminate;
/*! \brief  Class StTerminateNotified is a abstract visitor to propagate the termination signal
    It is to provide the custom responce to the SIGTERM system signal.
    One is supposed to sublclass it and reimplement the SetNotifiedCallBack() method.
    The SetNotifiedCallBack() is to be called by system upon signal SIGTERM emition 
    (for exampe with the kill <PID> shell command or 
    by the batch system manager (like Condor) upon the job eviction
 */
class StTerminateNotified {
    private:
       StNotifyOnTerminate *fTerminateHandler;
    public:
      StTerminateNotified();
      virtual ~StTerminateNotified();
      virtual  void SetNotifiedCallBack() = 0;
};

/*! \brief  Class StCloseFileOnTerminate - it is a singleton to catch the SIGTERM signal (sent by Condor for example) to close all open ROOT files.
    To activate, insert into your code:
    StCloseFileOnTerminate::Instantiate();
    On kill <PID> the Notify method is to close all existing ROOT files and terminated the applcation.
    It is simple. However it is error-prone. The signal SIGTERM can be emitted at the time of I/O operation.
    As result this I/O operation can not be completed and no way the file can be close properly.
 */
class StCloseFileOnTerminate : public TSignalHandler {
 private:
       static StCloseFileOnTerminate *fgCloseFileOnTerminate;
 protected:
       // ! singleton ctor 
       StCloseFileOnTerminate();
       //! Create an instance of the signal handler (Should be called at once  by code)
 public:
       static StCloseFileOnTerminate &Instantiate();
 protected:
       static bool Exists() {return fgCloseFileOnTerminate;}
       //! Close the open ROOT files upon SIGTERM sent by system to terminate the job
       virtual Bool_t Notify();
       ClassDef(StCloseFileOnTerminate,0)
};

class StNotifyOnTerminate : public StCloseFileOnTerminate {
     friend class StTerminateNotified;
 private:
     StTerminateNotified  &fNotificator;
 protected:
     StNotifyOnTerminate(StTerminateNotified &notificator):StCloseFileOnTerminate(),fNotificator(notificator)
     {}
 public:
    //! Close the open ROOT files upon SIGTERM sent by system to terminate the job
    virtual Bool_t Notify();
    StTerminateNotified  &Notificator() const { return fNotificator; }
    ClassDef(StNotifyOnTerminate,0)
};

#endif
 
