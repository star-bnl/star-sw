/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *         base class for triplet-wise cuts
 *         Users inherit from this class and must add their own quantities
 *
 ***************************************************************************/

#ifndef StHbtTripletCut_hh
#define StHbtTripletCut_hh

#include <string>

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTriplet.hh"
#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"

class StHbtTripletCut : public StHbtCutMonitorHandler {

public:

  StHbtTripletCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtTripletCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtTriplet* Triplet) =0;  // true if passes, false if not

  //  virtual string Report() =0;    // user-written method to return string describing cuts
  virtual StHbtString Report() =0;    // user-written method to return string describing cuts

#ifdef __ROOT__
  ClassDef(StHbtTripletCut, 0)
#endif
  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtThreeParticleAnalysis;
  StHbtThreeParticleAnalysis* HbtAnalysis(){return myAnalysis;};

protected:
  StHbtThreeParticleAnalysis* myAnalysis;


};

#endif
