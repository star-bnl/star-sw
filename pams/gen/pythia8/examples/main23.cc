// main23.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how decays could be handled externally.

#include "Pythia.h"

using namespace Pythia8; 

//**************************************************************************

// A derived class to do J/psi decays.

class JpsiDecay : public DecayHandler {

public:

  // Constructor.
  JpsiDecay() {times = 0;}

  // Routine for doing the decay.
  bool decay(vector<int>& idProd, vector<double>& mProd, 
    vector<Vec4>& pProd, int iDec, const Event& event);

private:

  // Count number of times JpsiDecay is called.
  int times;

};

//*********

// The actual J/psi decay routine.
// Not intended for realism, just to illustrate the principles.

bool JpsiDecay::decay(vector<int>& idProd, vector<double>& mProd, 
  vector<Vec4>& pProd, int iDec, const Event& event) {

  // Always do decay J/psi -> mu+ mu-; store the muons.
  idProd.push_back(-13);
  idProd.push_back(13);
  
  // Muon mass(es), here from Pythia tables, also stored.
  double mMuon = ParticleDataTable::m0(13); 
  mProd.push_back(mMuon);
  mProd.push_back(mMuon);

  // Calculate muon energy and momentum in J/psi rest frame.
  double eMuon = 0.5 * mProd[0];
  double pAbsMuon = sqrt(eMuon * eMuon - mMuon * mMuon);

  // Assume decay angles isotropic in rest frame.
  double cosTheta = 2. * Rndm::flat() - 1.;
  double sinTheta = sqrt(max(0., 1. - cosTheta * cosTheta));
  double phi = 2. * M_PI * Rndm::flat();
  double pxMuon = pAbsMuon * sinTheta * cos(phi); 
  double pyMuon = pAbsMuon * sinTheta * sin(phi); 
  double pzMuon = pAbsMuon * cosTheta; 

  // Define mu+ and mu- four-vectors in the J/psi rest frame.
  Vec4 pMuPlus(   pxMuon,  pyMuon,  pzMuon, eMuon);  
  Vec4 pMuMinus( -pxMuon, -pyMuon, -pzMuon, eMuon);  

  // Boost them by velocity vector of the J/psi mother and store.
  pMuPlus.bst(pProd[0]);
  pMuMinus.bst(pProd[0]);
  pProd.push_back(pMuPlus);
  pProd.push_back(pMuMinus);

  // Print message the first few times, to show that it works.
  if (times++ < 10) {
    int iMother = event[iDec].mother1();
    int idMother = event[iMother].id();
    cout << " J/psi decay performed, J/psi in line " << iDec 
         << ", mother id = " << idMother << "\n";
  }

  // Done
  return true;

}

//**************************************************************************

int main() {

  // Number of events to generate and to list. Max number of errors.
  int nEvent = 100;
  int nList = 2;
  int nAbort = 5;

  // Pythia generator.
  Pythia pythia;

  // A class to do J/psi decays externally. 
  DecayHandler* handleDecays = new JpsiDecay();

  // The list of particles the class can handle.
  vector<int> handledParticles;
  handledParticles.push_back(443);

  // Hand pointer and list to Pythia.
  pythia.setDecayPtr( handleDecays, handledParticles);

  // Initialization for charmonium (singlet+octet) production at the LHC.
  pythia.readString("Charmonium:all = on");
  pythia.readString("PhaseSpace:pTHatMin = 20.");
  pythia.init( 2212, 2212, 14000.);

  // Begin event loop.
  int iList = 0;
  int iAbort = 0;
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%(max(1,nEvent/20)) == 0) cout << " Now begin event " 
      << iEvent << "\n";

    // Generate events. Quit if many failures.
    if (!pythia.next()) {
      if (++iAbort < nAbort) continue;
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }

    // Look for event with externally handled decays.
    bool externalDecay = false;
    for (int i = 0; i < pythia.event.size(); ++i) {
      int status = pythia.event[i].statusAbs();
      if (status == 93 || status == 94) {externalDecay = true; break;}
    }  
 
    // List first few events with external decay.
    if (externalDecay && ++iList <= nList) { 
      pythia.process.list();
      pythia.event.list();
    }

  // End of event loop.
  }

  // Final statistics.
  pythia.statistics();

  // Done.
  return 0;
}
