/*!
  \class StPythiaFourPMaker
  \author M.L. Miller (MIT Software)

  StPythiaFourPMaker is used to fill the list of 4-momenta that is then passed to a
  StJetFinder instance.  StPythiaFourPMaker simply instantiates an object of type
  StPythiaMuTrackFourVec for every final state particle in the event.
*/

#ifndef StPythiaFourPMaker_HH
#define StPYthiaFourPMaker_HH

#include <vector>
using namespace std;

#include "StFourPMaker.h"
#include "StPythia/StPythiaMuTrackFourVec.h"

#include <MCParticleList.h>

class StMcEventMaker;
class StMCAsymMaker;

class StPythiaFourPMaker : public StFourPMaker
{

public:
    
  StPythiaFourPMaker(const char *name, StMCAsymMaker*, StMcEventMaker*);
  virtual ~StPythiaFourPMaker() { };
    
  virtual FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  virtual Int_t Make();
    
  virtual void Clear(Option_t* opt);
    
protected:

  FourList tracks;

  typedef vector<StMuTrackFourVec*> Pythia4Vec;
  Pythia4Vec mVec;
    
  StMCAsymMaker* mSimuMaker;
  StMcEventMaker* mMcEventMaker;
    
private:

  StSpinJet::MCParticleList getMCPartilceList();

  ClassDef(StPythiaFourPMaker,1)

};


#endif
