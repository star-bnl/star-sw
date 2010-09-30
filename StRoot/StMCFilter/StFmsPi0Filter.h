// @(#)STAR/eg:$Id: StFmsPi0Filter.h,v 1.1 2010/09/30 20:05:10 jwebb Exp $
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFmsPi0Filter: FMS pi0 filter class for EvGen and Geant                		//
// more info in http://www.star.bnl.gov/~perev/SIM/mcFi;lter.ppt
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFmsPi0Filter
#define STAR_StFmsPi0Filter
#include "StMCFilter.h"
#include <string>
#include <iostream>
using namespace std;

class StHepParticleMaster;
class StG3ParticleMaster;
class StGenParticleMaster;

class StFmsPi0Filter : public StMCFilter {
public:
  // ****** constructors and destructor
  StFmsPi0Filter();
  virtual ~StFmsPi0Filter(){;}

  // user reject function called immediately after Event Generator.
  // Currently Pythia Vertex in zero. EG==EventGenerator
   int RejectEG(const StGenParticleMaster &ptl) const;

  // user reject function called before GEANT tracking 
  // Vertex is already generated. GT == GeantTracker
   int RejectGT(const StGenParticleMaster &ptl) const;

  // user reject function called after GEANT tracking 
  // Vertex and tracks are already generated. GE==GeantEnd
  int RejectGE(const StGenParticleMaster &ptl) const;

  // Changing parameters
  void ChangeConfig(string attr, float val);

private:
  float mEtaMin;
  float mEtaMax;
  float mPtMin;
};

#endif

