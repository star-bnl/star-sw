// main04.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how different processes can be selected and studied.
// All input is specified in the main04.cmnd file.

#include "Pythia.h"

using namespace Pythia8; 

int main() {

  // Generator. Shorthand for the event and the (static) Settings.
  Pythia pythia;
  Event& event = pythia.event;
  Settings& settings = pythia.settings;

  // Read in commands from external file.
  pythia.readFile("main04.cmnd");    

  // Extract settings to be used in the main program.
  int    nEvent  = settings.mode("Main:numberOfEvents");
  int    nList   = settings.mode("Main:numberToList");
  int    nShow   = settings.mode("Main:timesToShow");
  int    nAbort  = settings.mode("Main:timesAllowErrors");
  bool   showCS  = settings.flag("Main:showChangedSettings");
  bool   showAS  = settings.flag("Main:showAllSettings");

  // Also need the CM energy.
  double eCM     = settings.parm("Beams:eCM");

  // Initialization using beam parameters in the .cmnd file.
  pythia.init();

  // List changed or all data.
  if (showCS) settings.listChanged();
  if (showAS) settings.listAll();

  // Histograms.
  double epTol = 1e-6 * eCM;
  Hist epCons("deviation from energy-momentum conservation",100,0.,epTol);
  Hist nFinal("final particle multiplicity",100,-0.5,799.5);
  Hist dNdEta("dn/deta for particles",100,-10.,10.);

  // Begin event loop.
  int nPace = max(1,nEvent/nShow); 
  int iAbort = 0;
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%nPace == 0) cout << " Now begin event " << iEvent << endl;

    // Generate events. Quit if failure.
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

    // Loop over final particles in the event. 
    int nFin = 0;
    Vec4 pSum;
    for (int i = 0; i < event.size(); ++i) if (event[i].isFinal()) {
      nFin++;
      pSum += event[i].p();
      dNdEta.fill(event[i].eta());
    }

    // Check and print event with too big energy-momentum deviation.
    nFinal.fill(nFin);
    double epDev = abs(pSum.e() - eCM) + abs(pSum.px()) + abs(pSum.py())
      + abs(pSum.pz());
    epCons.fill(epDev);
    if (epDev > epTol) {
      cout << " Warning! Event with epDev = " << scientific 
           << setprecision(4) << epDev << " now listed:";
      event.list();
    }

  // End of event loop.
  }

  // Final statistics and histogram output.
  pythia.statistics();
  dNdEta *= 5./nEvent;
  cout << epCons << nFinal << dNdEta; 

  return 0;
}
