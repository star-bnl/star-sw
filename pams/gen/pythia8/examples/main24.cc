// main24.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrated how random numbers could be generated externally.

#include "Pythia.h"

using namespace Pythia8; 

//**************************************************************************

// A derived class to generate random numbers.
// A guranteed VERY STUPID generator, just to show principles.

class stupidRndm : public RndmEngine {

public:

  // Constructor.
  stupidRndm() { init();}

  // Routine for generating a random number.
  double flat();

private:

  // Initialization.
  void init();

  // State of the generator.
  int number;
  double sqrt2, sqrt3, sqrt5, sqrt7, sqrt11, value;   

};

//*********

// Initialization method for the random numbers.

void stupidRndm::init() {

  // Calculater some irrantional numbers, truncated to below 1.
  sqrt2 = sqrt(2.) - 1.;
  sqrt3 = sqrt(2.) - 1.;
  sqrt5 = sqrt(2.) - 2.;
  sqrt7 = sqrt(2.) - 2.;
  sqrt11 = sqrt(11.) - 3.;
    
  // Initial values.
  number = 0;
  value = 0.; 

} 

//*********

// Initialization method for the random numbers.

double stupidRndm::flat() {

  // Update counter. Add to current value.
  do {
    number = (++number)%210;
    value += number%2 * sqrt2 + number%3 * sqrt3 + number%5 * sqrt5
           + number%7 * sqrt7 + sqrt11;
    value -= double(int(value));
    if (value < 0.) value += 1.; 
  } while (value <= 0. || value >= 1.);  

  // Return new value.
  return value;

}

//**************************************************************************

int main() {

  // Number of events to generate and to list. Max number of errors.
  int nEvent = 100;
  int nList = 1;
  int nAbort = 5;

  // Pythia generator.
  Pythia pythia;

  // Study standard Pythia random number generator.
  Hist rndmDist("standard random number distribution", 100, 0., 1.);
  Hist rndmCorr("standard random number correlation", 100, 0., 1.);
  double rndmNow;
  double rndmOld = Rndm::flat();
  for (int i = 0; i < 1000000; ++i) {
    rndmNow = Rndm::flat();
    rndmDist.fill(rndmNow);
    rndmCorr.fill( abs(rndmNow - rndmOld) );
    rndmOld = rndmNow;
  }    
  cout << rndmDist << rndmCorr;

  // A class to do random numbers externally. Hand pointer to Pythia.
  RndmEngine* badRndm = new stupidRndm();
  pythia.setRndmEnginePtr( badRndm);

  // Generate and show some "random" numbers.
  /*
  cout << fixed << setprecision(8); 
  for (int i = 0; i < 500; ++i) {
    cout << setw(12) << Rndm::flat();
    if(i%10 == 9) cout << "\n";
  } 
  */

  // Study bad "new" random number generator.
  Hist rndmDist2("stupid random number distribution", 100, 0., 1.);
  Hist rndmCorr2("stupid random number correlation", 100, 0., 1.);
  rndmOld = Rndm::flat();
  for (int i = 0; i < 100000; ++i) {
    rndmNow = Rndm::flat();
    rndmDist2.fill(rndmNow);
    rndmCorr2.fill( abs(rndmNow - rndmOld) );
    rndmOld = rndmNow;
  }    
  cout << rndmDist2 << rndmCorr2;

  // Initialization for ttbar production at the LHC.
  pythia.readString("Top:gg2ttbar = on");
  pythia.readString("Top:qqbar2ttbar = on");
  pythia.init( 2212, 2212, 14000.);

  // Begin event loop.
  int iAbort = 0;
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%(max(1,nEvent/20)) == 0) cout << " Now begin event " 
      << iEvent << "\n";

    // Generate events. Quit if failure.
    if (!pythia.next()) {
      if (++iAbort < nAbort) continue;
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }
 
    // List first few events.
    if (++iEvent <= nList) { 
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
