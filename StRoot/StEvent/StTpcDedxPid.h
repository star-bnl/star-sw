/***************************************************************************
 *
 * $Id: StTpcDedxPid.h,v 1.2 1999/04/30 13:16:29 fisyak Exp $
 *
 * Author: Craig Ogilvie, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPid.h,v $
 * Revision 1.2  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.2  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.1  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/08 14:56:30  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcDedxPid_hh

#define StTpcDedxPid_hh
public: 
    StTpcDedxPid(StGlobalTrack*);
    ~StTpcDedxPid();
    
    Int_t detectorInfoAvailable() const;
    Int_t meetsStandardPid() const;
    Double_t numberOfSigma(Double_t mass) const;
    Double_t meanPidFunction(Double_t mass) const;
    Double_t sigmaPidFunction(Double_t mass) const;
  static Int_t quickPid(Float_t rig, Float_t dedx);


  ClassDef(StTpcDedxPid,1)  //StTpcDedxPid structure
};


#endif
