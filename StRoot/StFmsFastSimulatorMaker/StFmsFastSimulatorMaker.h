// $Id: StFmsFastSimulatorMaker.h,v 1.2 2014/08/06 11:43:15 jeromel Exp $
//
// $Log: StFmsFastSimulatorMaker.h,v $
// Revision 1.2  2014/08/06 11:43:15  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2014/05/06 16:02:04  jeromel
// First version of StFmsFastSimulatorMaker deliverred upon review
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
  enum StFmsDetectorId {
    kFpdNorth = 0,
    kFpdSouth = 1,
    kFpdNorthPreshower = 2,
    kFpdSouthPreshower = 3,
    kFmsNorthLarge = 8,
    kFmsSouthLarge = 9,
    kFmsNorthSmall = 10,
    kFmsSouthSmall = 11,
    kFmsInvalidDetectorId = -1
  };
  /**
   Returns the detector ID of an FPD or FMS detector subsystem.
   
   Detector ID numbers are those used by StFmsChannelGeometry (or see
   StFmsDetectorID in this class).
   Returns kFmsInvalidDetectorId if the detector ID cannot be determined,
   or corresponds to an unsupported subdetector.
   */
  Int_t getDetectorId(const g2t_emc_hit_st& hit) const;
  /**
   Constructs and returns an StFmsHit from a g2t_emc_hit_st.

   The StFmsHit is allocated via new and so needs to be deleted by the
   user, or passed to a container that takes ownership of it.
   */
  StFmsHit* makeFmsHit(const g2t_emc_hit_st& hit);
  /**
   Fills the StFmsCollection of StEvent with hits from an StMcEvent.
   */
  void fillStEvent(StEvent* event);
  /**
   Prints the total number of hits and energy in each detector subsystem.
   */
  void printStEventSummary(const StEvent* event);
  ClassDef(StFmsFastSimulatorMaker, 0)
};

inline const char* StFmsFastSimulatorMaker::GetCVS() const {
  static const char cvs[]="Tag $Name:  $ $Id: StFmsFastSimulatorMaker.h,v 1.2 2014/08/06 11:43:15 jeromel Exp $ built " __DATE__ " " __TIME__;
  return cvs;
}

#endif  // ST_FMS_SIMULATOR_MAKER_H
