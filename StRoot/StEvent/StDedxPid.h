/***************************************************************************
 *
 * $Id: StDedxPid.h,v 1.3 1999/05/02 00:00:16 fisyak Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.h,v $
 * Revision 1.3  1999/05/02 00:00:16  fisyak
 * Add default ctors
 *
 * Revision 1.3  1999/05/02 00:00:16  fisyak
 * Add default ctors
 *
 * Revision 1.2  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.1  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/08 14:56:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StDedxPid_hh
#define StDedxPid_hh

class StGlobalTrack;

#include "StObject.h"
class StDedxPid : public StObject {
public: 
  StDedxPid() : mTrack(0) { /* noop */ };
  StDedxPid(StGlobalTrack*);
  virtual ~StDedxPid();

  virtual Int_t    detectorInfoAvailable() const       = 0;
  virtual Int_t    meetsStandardPid() const            = 0;
  virtual Double_t numberOfSigma(Double_t mass) const    = 0;
  virtual Double_t meanPidFunction(Double_t mass) const  = 0;
  virtual Double_t sigmaPidFunction(Double_t mass) const = 0;
    
protected:
  StGlobalTrack* mTrack;
  ClassDef(StDedxPid,1)  //StDedxPid structure
};


#endif
