// $Id: StMCTimeStepping.h,v 1.1 2005/03/09 18:35:34 perev Exp $
//
//
// Class StMCTimeStepping
// ------------------


#ifndef STMC_TIME_STEPPING_H
#define STMC_TIME_STEPPING_H

#include "TString.h"
#include "TLorentzVector.h"
#include "GCall.h"

class StMCTimeStepping : public GCall
{
  public:
//   All cases
  enum SteppingCase {
  kNewTrack 		 =   1,
  kRootGeometrySupported =   2,
  kTrackAlive 		 =   4,
  kTrackDisappeared 	 =   8,
  kTrackEntering 	 =  16,
  kTrackExiting 	 =  32,
  kTrackInside           =  64,
  kTrackOut              = 128,
  kTrackStop             = 256};

         StMCTimeStepping(const char *name="",const char *tit="");
virtual ~StMCTimeStepping(){}    
    // methods
virtual int  Fun();
virtual void Print(const Option_t* opt=0) const;
virtual void Finish(const Option_t* opt=0);
protected:
    // data members
    ClassDef(StMCTimeStepping,0) // Extended TParticle
};

#endif //STMC_TIME_STEPPING_H   
   

