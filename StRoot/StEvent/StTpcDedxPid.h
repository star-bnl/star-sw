/***************************************************************************
 *
 * $Id: StTpcDedxPid.h,v 1.7 1999/08/11 20:41:48 fisyak Exp $
 *
 * Author: Craig Ogilvie, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPid.h,v $
 * Revision 1.7  1999/08/11 20:41:48  fisyak
 * Add quickPid from Aihong Tang
 *
 * Revision 1.7  1999/08/11 20:41:48  fisyak
 * Add quickPid from Aihong Tang
 *
 * Revision 1.6  1999/08/03 18:22:08  ogilvie
 * improved dE/dx parameterization
 *
 * Revision 1.5  1999/07/14 12:50:50  fisyak
 * Add lost Set/Get functions
 *
 * Revision 1.4  1999/07/13 13:20:35  fisyak
 * Add lost Craig functions, use gufld for magnetic field
 *
 * Revision 1.3  1999/05/02 00:00:17  fisyak
 * Add default ctors
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
#include "StObject.h"
#include "StDedxPid.h"
class StTpcDedxPid : public StDedxPid {
public:
  StTpcDedxPid() : StDedxPid() { /* noop */ };
  StTpcDedxPid(StGlobalTrack*);
  ~StTpcDedxPid();
  Int_t detectorInfoAvailable() const;
  Int_t meetsStandardPid() const;
  Double_t numberOfSigma(Double_t mass) const;
  Double_t meanPidFunction(Double_t mass) const;
  Double_t sigmaPidFunction(Double_t mass) const;

  void setTpcDedxGain(Double_t gain)     {mTpcDedxGain = gain ;} 
  void setTpcDedxOffset(Double_t offset) {mTpcDedxOffset=offset;} 
  void setTpcDedxRise(Double_t rise)     {mTpcDedxRise = rise ;} 
  void setTpcDedxTcut(Double_t cut)     {mTpcDedxTcut = cut ;} 

  Double_t getTpcDedxGain()              { return mTpcDedxGain ;} 
  Double_t getTpcDedxOffset()            { return mTpcDedxOffset ;} 
  Double_t getTpcDedxRise()              { return mTpcDedxRise;}
  Double_t getTpcDedxTcut()              { return mTpcDedxTcut;}
    
  static Double_t mTpcDedxGain ;
  static Double_t mTpcDedxOffset ;
  static Double_t mTpcDedxRise ; 
  static Double_t mTpcDedxTcut ;  

  static Int_t quickPid(Float_t rig, Float_t dedx);


  ClassDef(StTpcDedxPid,1)  //StTpcDedxPid structure
};


#endif
