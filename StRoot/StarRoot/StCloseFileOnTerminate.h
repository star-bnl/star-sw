#ifndef STAR_STCLOSEFILEONTERMINATE
#define STAR_STCLOSEFILEONTERMINATE
// $Id: StCloseFileOnTerminate.h,v 1.1 2010/02/25 20:44:05 fine Exp $

#include "TSysEvtHandler.h"

//! \author Valery Fine(fine@bnl.gov)
//! \date 27=5/02/2010

/*! \brief  Class StCloseFileOnTerminate - catch the SIGTERM signal *sent by Condor for example) to close all open ROOT files.to draw the 3D primitives like 3D points and 3D lines
 */
class StCloseFileOnTerminate : public TSignalHandler {
 private:
       static StCloseFileOnTerminate *fgCloseFileOnTerminate;
 protected:
       StCloseFileOnTerminate() : TSignalHandler(kSigTermination) { }
 public:
       //! Create an instance of the signal handler (Should be called at once  by code)
       static StCloseFileOnTerminate &Instantiate();
       //! Close the open ROOT files upon SIGTERM sent by system to terminate the job
       virtual Bool_t Notify();
       ClassDef(StCloseFileOnTerminate,0)
};
#endif
 
