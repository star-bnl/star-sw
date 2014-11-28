// main25.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Simple illustration how to provide your own cross-section class,
// with an instance handed in to Pythia for internal generation.
// The class is a lighly modified copy of an existing Pythia class,
// and cross sections are compared with the internal implementation. 

#include "Pythia.h"

using namespace Pythia8; 
 
//**************************************************************************

// A derived class for b g -> H0 b (Standard Model Higgs).

class Sigma2bg2Hb : public Sigma2Process {

public:

  // Constructor.
  Sigma2bg2Hb() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for decay angles.
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return "b g -> H b";}
  virtual int    code()    const {return 10001;}
  virtual string inFlux()  const {return "qg";}
  virtual int    id3Mass() const {return 25;}
  virtual int    id4Mass() const {return 5;}

private:

  // Store flavour-specific process information and standard prefactor.
  double m2b, m2W, thetaWRat, openFrac, sigma;

};

//*********

// Initialize process. 
  
void Sigma2bg2Hb::initProc() {

  // Masses and couplings.
  m2b       = pow2( ParticleDataTable::m0(5) );
  m2W       = pow2( ParticleDataTable::m0(24) );
  thetaWRat = 1. / (24. * CoupEW::sin2thetaW()); 

  // Suppression from secondary widths.
  openFrac = ParticleDataTable::resOpenFrac(25);
  
} 

//*********

// Evaluate sigmaHat(sHat); first step when inflavours unknown. 

void Sigma2bg2Hb::sigmaKin() { 

  // Initial values. Running mass provides coupling.
  double mHat   = sqrt(sH);
  double m2bRun = pow2( ParticleDataTable::mRun(5, mHat) );

  // Cross section.
  sigma = (M_PI / sH2) * alpS * alpEM * thetaWRat * (m2bRun/m2W) 
    * ( sH / (m2b - uH) + 2. * m2b * (s3 - uH) / pow2(m2b - uH) 
    + (m2b - uH) / sH - 2. * m2b / (m2b - uH) 
    + 2. * (s3 - uH)  * (s3 - m2b - sH) / ((m2b - uH) * sH) );
  sigma *= openFrac;

}

//*********

// Evaluate sigmaHat(sHat); second step for given inflavours.

double Sigma2bg2Hb::sigmaHat() { 

  // Cross section vanishing except for incoming b(bar) quark.
  if (abs(id1) != 5 && abs(id2) != 5) return 0.;
 
  // Cross section as already calculated.
  return sigma;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2bg2Hb::setIdColAcol() {

  // Flavour set up for b g -> H0 q.
  int idq = (id2 == 21) ? id1 : id2;
  setId( id1, id2, 25, idq);

  // tH defined between b_in and b_out: must swap tHat <-> uHat if b g in.
  swapTU = (id2 == 21); 

  // Colour flow topologies. Swap when antiquarks.
  if (id2 == 21) setColAcol( 1, 0, 2, 1, 0, 0, 2, 0);
  else           setColAcol( 2, 1, 1, 0, 0, 0, 2, 0);
  if (idq < 0) swapColAcol();

}

//*********

// Evaluate weight for Z0 Z0 or W+W- decay angles in Higgs decay.

double Sigma2bg2Hb::weightDecay( Event& process, int iResBeg,
  int iResEnd) {

  // For Higgs decay hand over to standard routine, else done.
  if (process[process[iResBeg].mother1()].idAbs() == 25) 
       return weightHiggsDecay( process, iResBeg, iResEnd);
  else return 1.; 

}

//**************************************************************************

int main() {

  // Number of events to generate and to list. Max number of errors.
  // Warning: generation of complete events is much slower than if you use
  // PartonLevel:all = off to only get cross sections, so adjust nEvent.
  int nEvent = 1000;
  int nList  = 1;
  int nAbort = 5;

  // Pythia generator.
  Pythia pythia;

  // A class to generate the b g -> H b process from external matrix element. 
  SigmaProcess* sigma2bg2Hb = new Sigma2bg2Hb();

  // Hand pointer to Pythia.
  pythia.setSigmaPtr( sigma2bg2Hb);

  // Optionally compare with (almost) same process implemented internally.
  pythia.readString("HiggsSM:qg2Hq = on");

  // Phase space cut on pThat.
  pythia.readString("PhaseSpace:pTHatMin = 20.");

  // Optionally only want to study H0 -> gamma gamma channel.
  pythia.readString("25:onMode = off");
  pythia.readString("25:onIfMatch = 22 22");

  // Optionally only compare cross sections.
  //pythia.readString("PartonLevel:all = off");

  // Initialization for LHC.
  pythia.init( 2212, 2212, 14000.);

  // List changes.
  pythia.settings.listChanged();
  pythia.particleData.listChanged();

  // Begin event loop.
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
 
    // List first few events.
    if (iEvent < nList) { 
      pythia.info.list();
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
