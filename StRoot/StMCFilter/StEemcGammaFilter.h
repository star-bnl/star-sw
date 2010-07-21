#ifndef _STAREEMCGAMMAFILTER_
#define _STAREEMCGAMMAFILTER_

//////////////////////////////////////////////////
//    StEemcGammaFilter                             //
//                                              //
//    Description....                           //
//                                              //
//////////////////////////////////////////////////

#include "StMCFilter/StMCFilter.h"


// Forward declarations
class StGenParticleMaster;


class StEemcGammaFilter : public StMCFilter 
{

 public:

  StEemcGammaFilter();
  virtual ~StEemcGammaFilter() {};
       
  // Reject after vertex sampling
  int RejectGT(const StGenParticleMaster &ptl) const;

 private:

  static const double mConeRadius;
  static const double mSeedThreshold;
  static const double mClusterThreshold;
  static const double mEtaLow;
  static const double mEtaHigh;
  static const double mCalDepth;
  static const double mHadronScale;
  static const double mMinPartEnergy;
  static const double mMaxVertex;
      
  int    mPrintLevel;
  int    mFilterMode;

};

#endif

