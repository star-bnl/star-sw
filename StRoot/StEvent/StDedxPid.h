/***************************************************************************
 *
 * $Id: StDedxPid.h,v 1.2 1999/04/30 13:16:27 fisyak Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.h,v $
 * Revision 1.2  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
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

    StDedxPid(StGlobalTrack*);
    virtual ~StDedxPid();
  StDedxPid() : mTrack(0) { /* noop */ };
    virtual Int_t    detectorInfoAvailable() const       = 0;
    virtual Int_t    meetsStandardPid() const            = 0;
    virtual Double_t numberOfSigma(Double_t mass) const    = 0;
    virtual Double_t meanPidFunction(Double_t mass) const  = 0;
    virtual Double_t sigmaPidFunction(Double_t mass) const = 0;
  virtual Double_t numberOfSigma(Double_t mass) const    = 0;
  virtual Double_t meanPidFunction(Double_t mass) const  = 0;
    StGlobalTrack* mTrack;
    ClassDef(StDedxPid,1)  //StDedxPid structure
protected:
  StGlobalTrack* mTrack;
  ClassDef(StDedxPid,1)  //StDedxPid structure
};


#endif
