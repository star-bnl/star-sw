// main07.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how to generate and analyze minimum-bias events.
// All input is specified in the main07.cmnd file.

#include "Pythia.h"

using namespace Pythia8; 

//**************************************************************************

int main() {

  // Generator. Shorthand for the event and for settings.
  Pythia pythia;
  Event& event = pythia.event;
  Settings& settings = pythia.settings;

  // Read in commands from external file.
  pythia.readFile("main07.cmnd");    

  // Extract settings to be used in the main program.
  int    nEvent    = settings.mode("Main:numberOfEvents");
  int    nList     = settings.mode("Main:numberToList");
  int    nShow     = settings.mode("Main:timesToShow");
  int    nAbort    = settings.mode("Main:timesAllowErrors");
  bool   showCS    = settings.flag("Main:showChangedSettings");
  bool   showAS    = settings.flag("Main:showAllSettings");
  bool   showCPD   = settings.flag("Main:showChangedParticleData");
  bool   showAPD   = settings.flag("Main:showAllParticleData");
  bool   showAStat = settings.flag("Main:showAllStatistics");
 
  // Initialize. Beam parameters set in .cmnd file.
  pythia.init();

  // List changed data.
  if (showCS) settings.listChanged();
  if (showAS) settings.listAll();

  // List particle data.  
  if (showCPD) ParticleDataTable::listChanged();
  if (showAPD) ParticleDataTable::listAll();

  // Book histograms.
  double pTmax = 20.;
  double bMax = 4.;
  Hist nChg("number of charged particles", 100, -0.5, 799.5);
  Hist nChg1("number of charged particles in |eta| < 1", 100, -0.5, 99.5);
  Hist pTnChg1("<pT>(n_charged) in |eta| < 1", 100, -0.5, 99.5);
  Hist pTspec("scattering pT spectrum", 100, 0., pTmax); 
  Hist bSpec("b impact parameter spectrum", 100, 0., bMax);
  Hist enhanceSpec("b enhancement spectrum", 100, 0., 10.);
  Hist number("number of interactions", 100, -0.5, 99.5);
  Hist pTb1("pT spectrum for b < 0.5", 100, 0., pTmax); 
  Hist pTb2("pT spectrum for 0.5 < b < 1", 100, 0., pTmax); 
  Hist pTb3("pT spectrum for 1 < b < 1.5", 100, 0., pTmax); 
  Hist pTb4("pT spectrum for 1.5 < b", 100, 0., pTmax); 
  Hist bpT1("b spectrum for pT < 2", 100, 0., bMax);
  Hist bpT2("b spectrum for 2 < pT < 5", 100, 0., bMax);
  Hist bpT3("b spectrum for 5 < pT < 15", 100, 0., bMax);
  Hist bpT4("b spectrum for 15 < pT", 100, 0., bMax);
 
  // Begin event loop.
  int nShowPace = max(1,nEvent/nShow); 
  int iAbort = 0; 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%nShowPace == 0) cout << " Now begin event " 
      << iEvent << endl;

    // Generate events. Quit if too many failures.
    if (!pythia.next()) {
      if (++iAbort < nAbort) continue;
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }
 
    // List first few events, both hard process and complete events.
    if (iEvent < nList) { 
      pythia.info.list();
      pythia.process.list();
      event.list();
    }

    // Charged multiplicity. It and pT in central rapidity range.
    int    nch     = 0;
    int    nch1    = 0;
    double pTsum1  = 0.;
    for (int i = 1; i < event.size(); ++i)
    if (event[i].isFinal() && event[i].isCharged()) { 
      ++nch;
      if ( abs(event[i].eta()) < 1.) {
        ++nch1;
        pTsum1 += event[i].pT();
      }
    }    
    nChg.fill( nch );
    nChg1.fill( nch1 );
    if (nch1 > 0) pTnChg1.fill( nch1, pTsum1 / nch1 );

    // Study event in (pT, b) space.
    double pT = pythia.info.pTHat(); 
    double b = pythia.info.bMI();
    double enhance = pythia.info.enhanceMI();
    int nMI = pythia.info.nMI();
    pTspec.fill( pT );
    bSpec.fill( b );
    enhanceSpec.fill( enhance );
    number.fill( nMI );
    if (b < 0.5) pTb1.fill( pT );
    else if (b < 1.0) pTb2.fill( pT );
    else if (b < 1.5) pTb3.fill( pT );
    else pTb4.fill( pT );
    if (pT < 2.) bpT1.fill( b );
    else if (pT < 5.) bpT2.fill( b );
    else if (pT < 15.) bpT3.fill( b );
    else bpT4.fill( b );

  // End of event loop.
  }

  // Final statistics.
  pythia.statistics(showAStat);
  pTnChg1 /= nChg1;
  cout << nChg << nChg1 << pTnChg1 << pTspec << bSpec 
       << enhanceSpec << number;
  cout << pTb1 << pTb2 << pTb3 << pTb4;
  cout << bpT1 << bpT2 << bpT3 << bpT4;

  // Done.
  return 0;
}
