// $Id: StFmsFastSimulatorMaker.h,v 1.8 2017/09/28 17:07:19 akio Exp $
//
// $Log: StFmsFastSimulatorMaker.h,v $
// Revision 1.8  2017/09/28 17:07:19  akio
// addoing bitshiftgain
//
// Revision 1.7  2017/05/03 15:54:21  akio
// added gain scaling when attenuation was on during geant simulation
//
// Revision 1.6  2015/09/29 16:28:58  akio
// setFmsZS(int v) and if ADC<v, drop the hit (default=2)
// adding poisson distribution for FPS, with setFpsNPhotonPerMIP(float v)
//  (default=0 for now, which turns this off. It should be around 100?)
//
// Revision 1.5  2015/09/18 18:44:28  akio
// uses StEnumeration
//
// Revision 1.4  2015/07/28 14:50:05  jeromel
// Fix string literal spacing for C++11 compliance
//
// Revision 1.3  2015/02/26 23:53:04  yuxip
// new update from Akio
//
/**
 \file StFmsFastSimulatorMaker.h
       Declaration of StFmsFastSimulatorMaker, the FMS fast simulator
 \author Pibero Djawotho <pibero@tamu.edu>
 \date 4 Jan 2011
 */
#ifndef ST_FMS_SIMULATOR_MAKER_H
#define ST_FMS_SIMULATOR_MAKER_H

class g2t_emc_hit_st;
class StFmsHit;
class StEvent;

#include "StChain/StMaker.h"

/**
 The FMS fast simulator maker.
 
 Populates the FMS hit collection in StEvent with StFmsHits, using Geant hits
 from the g2t table as input.
 Simulates digitisation of hit energy using gains from the database.
 
 For descriptions of the FMS hit structures in GEANT:
  <ul>
  <li>http://www.star.bnl.gov/protected/spin/akio/fpd_geant/</li>
  <li>http://drupal.star.bnl.gov/STAR/event/2011/01/05/software-and-computing-phone-meeting/fms-simulation-open-request-and-readiness</li>
  </ul>
 For descriptions of the FMS geometry and mapping in the FMS database:
  <ul>
  <li>http://drupal.star.bnl.gov/STAR/book/export/html/15527</li>
  </ul>
 or look at the relevant structures directly:
  <ul>
  <li>$STAR/StDb/idl/fmsChannelGeometry.idl</li>
  <li>$STAR/StDb/idl/fmsDetectorPosition.idl</li>
  <li>$STAR/StDb/idl/fmsDetectorPosition.idl</li>
  <li>$STAR/StDb/idl/fmsPatchPannelMap.idl</li>
  <li>$STAR/StDb/idl/fmsQTMap.idl</li>
  </ul>
 */
class StFmsFastSimulatorMaker : public StMaker {
 public:
  /**
   Destructor.
   */
  virtual ~StFmsFastSimulatorMaker() { }
  /**
   Default constructor.
   */
  explicit StFmsFastSimulatorMaker(const Char_t* name = "fmsSim");
  // Note we the use default copy constructor and assignment operator produced
  // by the compiler as we do not allocate any resources on the heap.

  /**
   Populate StEvent with FMS g2t Geant hits for the current event.
   Returns kStOk in case of success, kStError otherwise.
   */
  Int_t Make();

  // Setting zero suppression # of channels
  void setFmsZS(int v) {mFmsZSch=v;}

  // Set to use Bit Shift Gain from DB when digitizing
  void setBitShiftGain(int v) {mFmsBitShiftGain=v;}
 
  // Setting average energy loss per MIP for FPS
  void  setFpsDEPerMIP(float v) {mFpsDEPerMIP=v;}

  // Setting average # of photon per MIP for FPS
  void  setFpsNPhotonPerMIP(float v) {mFpsNPhotonPerMIP=v;}

  // Setting FMS gain scaling when attenuation is on
  void  setAttenuationGainScale(float v) {mAttenuationGainScale=v;}

  /**
   Standard overload of StMaker::GetCVS()
   */
  virtual const char* GetCVS() const;

 private:


  /**
   Enumeration for FPD and FMS subdetectors.
   
   The numerical values are defined for compatibility with the detector ID
   values expected by StFmsHit i.e. the detector ID numbers defined by
   StFmsChannelGeometry in the database. Only subdetectors that are possible
   return values from getDetectorId() are included; there are other
   subdetectors, but they are not valid return values (hits corresponding
   to these subdetectors will return kFmsInvalidDetectorId).
   */
  /* See StRoot/StEvent/StEnumerations.h   
  enum StFmsDetectorId {
    kFpdNorth = 0,
    kFpdSouth = 1,
    kFpdNorthPreshower = 2,
    kFpdSouthPreshower = 3,
    kFmsNorthLarge = 8,
    kFmsSouthLarge = 9,
    kFmsNorthSmall = 10,
    kFmsSouthSmall = 11,
    kFPS = 14,
    kFmsInvalidDetectorId = -1
  };
    */

  /**
   Returns the detector ID of an FPD or FMS detector subsystem.   
   Detector ID numbers are those used by StFmsChannelGeometry (or see
   StFmsDetectorID in this class).
   Returns kFmsInvalidDetectorId if the detector ID cannot be determined,
   or corresponds to an unsupported subdetector.
   */
  Int_t getDetectorId(const g2t_emc_hit_st& hit) const;

  /**
   Fills the StFmsCollection of StEvent with hits from an StMcEvent.
   */
  void fillStEvent(StEvent* event);

  /**
   Prints the total number of hits and energy in each detector subsystem.
   */
  void printStEventSummary(const StEvent* event);

  Int_t mFmsZSch=1; //default set to 1 (zero suppresss adc=0 and 1) 20170925 -akio  (It was 2 before)
  Int_t mFmsBitShiftGain=1; //use BitGainShift from DB when digitize 20170925 -akio  

  Float_t mFpsDEPerMIP;
  Float_t mFpsNPhotonPerMIP;
  
  Int_t mAttenuation=0;
  Float_t mAttenuationGainScale=0.35;

  ClassDef(StFmsFastSimulatorMaker, 0)
};

inline const char* StFmsFastSimulatorMaker::GetCVS() const {
  static const char cvs[]="Tag $Name:  $ $Id: StFmsFastSimulatorMaker.h,v 1.8 2017/09/28 17:07:19 akio Exp $ built " __DATE__ " " __TIME__ ;
  return cvs;
}

#endif  // ST_FMS_SIMULATOR_MAKER_H
