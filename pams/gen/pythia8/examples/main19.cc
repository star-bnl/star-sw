// main19.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This program runs four instances of Pythia simultaneously,
// one for signal events, one for pileup background ones, and two
// For beam-gas background ones. Note that Pythia does not do nuclear
// effects, so beam-gas is represented by "fixed-target" pp collisions.
// The = and += overloaded operators are used to join several 
// event records into one, but should be used with caution.

// Warning: several elements of Pythia are static, so this facility
// should be used with caution:
// 1) The Settings database is static, but the initialization step of
//    most classes store their values in local non-static copies. 
//    It is therefore possible to initialize with different settings.
//    In particular, it is possible to set up two Pythia instances 
//    that have different beams and generate different sets of processes.
// 2) The ParticleData database is also static and used as such,
//    so it is not possible to use different particle data 
//    (and also not different resonance width expressions).
// 3) The random-number generation is static.
// 4) Interfaces to external Fortran programs are "by definition" static.
//    Thus it is not a good idea to use LHAPDF to set different PDF's
//    in different instances.

#include "Pythia.h"
using namespace Pythia8; 

//**************************************************************************

// Method to pick a number according to a Poissonian distribution.

int poisson(double nAvg) {

  // Set maximum to avoid overflow.
  const int NMAX = 100;

  // Random number.
  double rPoisson = Rndm::flat() * exp(nAvg);

  // Initialize.
  double rSum  = 0.;
  double rTerm = 1.;
  
  // Add to sum and check whether done.
  for (int i = 0; i < NMAX; ) {
    rSum += rTerm;
    if (rSum > rPoisson) return i;

    // Evaluate next term. 
    ++i;
    rTerm *= nAvg / i;
  }

  // Emergency return.
  return NMAX; 
}

//**************************************************************************

int main() {

  // Number of signal events to generate.
  int nEvent = 100;

  // Average number of pileup events per signal event.
  double nPileupAvg = 2.5;

  // Average number of beam-gas events per signal ones, on two sides.
  double nBeamAGasAvg = 0.5;
  double nBeamBGasAvg = 0.5;

  // Four generator instances.
  Pythia pythiaSignal;
  Pythia pythiaPileup;
  Pythia pythiaBeamAGas;
  Pythia pythiaBeamBGas;

  // One object where all individual events are to be collected.
  Event sumEvent;
  
  // Initialize generator for signal processes. 
  pythiaSignal.readString("HardQCD:all = on");    
  pythiaSignal.readString("PhaseSpace:pTHatMin = 50.");
  pythiaSignal.init( 2212, 2212, 14000.);
  // Selected process(es) must be switched back off before next init,
  // since Settings database is static.
  pythiaSignal.readString("HardQCD:all = off");    

  // Initialize generator for pileup (background) processes. 
  pythiaPileup.readString("SoftQCD:all = on");    
  pythiaPileup.init( 2212, 2212, 14000.);

  // Initialize generators for beam-gas (background) processes. 
  pythiaBeamAGas.init( 2212, 2212, 14000., 0.);
  pythiaBeamBGas.init( 2212, 2212, 0., 14000.);

  // Histograms: number of pileups, total charged multiplicity.
  Hist nPileH("number of pileup events per signal event", 100, -0.5, 99.5);
  Hist nAGH("number of beam A + gas events per signal event", 100, -0.5, 99.5);
  Hist nBGH("number of beam B + gas events per signal event", 100, -0.5, 99.5);
  Hist nChgH("number of charged multiplicity",100, -0.5, 1999.5);  
  Hist sumPZH("total pZ of system",100, -100000., 100000.);  

  // Loop over events. 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {

    // Generate a signal event. Copy this event into sumEvent. 
    if (!pythiaSignal.next()) continue;
    sumEvent = pythiaSignal.event;

    // Select the number of pileup events to generate.
    int nPileup = poisson(nPileupAvg); 
    nPileH.fill( nPileup );

    // Generate a number of pileup events. Add them to sumEvent.      
    for (int iPileup = 0; iPileup < nPileup; ++iPileup) {
      pythiaPileup.next();
      sumEvent += pythiaPileup.event;
    }

    // Select the number of beam A + gas events to generate.
    int nBeamAGas = poisson(nBeamAGasAvg); 
    nAGH.fill( nBeamAGas );

    // Generate a number of beam A + gas events. Add them to sumEvent.      
    for (int iAG = 0; iAG < nBeamAGas; ++iAG) {
      pythiaBeamAGas.next();
      sumEvent += pythiaBeamAGas.event;
    }
  
    // Select the number of beam B + gas events to generate.
    int nBeamBGas = poisson(nBeamBGasAvg); 
    nBGH.fill( nBeamBGas );

    // Generate a number of beam B + gas events. Add them to sumEvent.      
    for (int iBG = 0; iBG < nBeamBGas; ++iBG) {
      pythiaBeamBGas.next();
      sumEvent += pythiaBeamBGas.event;
    }
  
    // List first few events.
    if (iEvent < 1) {
      pythiaSignal.info.list();
      pythiaSignal.process.list();
      sumEvent.list();
    } 

    // Find charged multiplicity.
    int nChg = 0;
    for (int i = 0; i < sumEvent.size(); ++i) 
      if (sumEvent[i].isFinal() && sumEvent[i].isCharged()) ++nChg; 
    nChgH.fill( nChg );

    // Fill net pZ - nonvanishing owing to beam + gas.
    sumPZH.fill( sumEvent[0].pz() ); 
 
  // End of event loop 
  }

  // Statistics. Histograms.
  pythiaSignal.statistics();
  pythiaPileup.statistics();
  pythiaBeamAGas.statistics();
  pythiaBeamBGas.statistics();
  cout << nPileH << nAGH << nBGH << nChgH << sumPZH;

  return 0;
}
