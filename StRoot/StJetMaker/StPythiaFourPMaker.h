// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.3 2008/07/22 05:06:23 tai Exp $
/*!
  \class StPythiaFourPMaker
  \author M.L. Miller (MIT Software)

  StPythiaFourPMaker is used to fill the list of 4-momenta that is then passed to a
  StJetFinder instance.  StPythiaFourPMaker simply instantiates an object of type
  StPythiaMuTrackFourVec for every final state particle in the event.
*/
#ifndef StPythiaFourPMaker_HH
#define StPYthiaFourPMaker_HH


#include "StFourPMaker.h"

#include <MCParticleList.h>

#include <vector>

class StMcEventMaker;
class StMCAsymMaker;
class StMuTrackFourVec;

namespace StSpinJet {
  class StJetMC;
}

class StPythiaFourPMaker : public StFourPMaker {

public:
    
  StPythiaFourPMaker(const char *name, StMCAsymMaker*, StMcEventMaker*);
  virtual ~StPythiaFourPMaker() { };
    
  FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  Int_t Init();
  Int_t Make();
    
  void Clear(Option_t* opt);
    
protected:

  FourList tracks;

  typedef std::vector<StMuTrackFourVec*> Pythia4Vec;
  Pythia4Vec mVec;
    
private:

  StSpinJet::StJetMC* _mc;

  ClassDef(StPythiaFourPMaker,1)
};


#endif
