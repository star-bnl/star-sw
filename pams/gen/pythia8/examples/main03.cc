// main03.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates a simple analysis of QCD jet events.
// All input is specified in the main03.cmnd file.

#include "Pythia.h"

using namespace Pythia8; 

int main() {

  // Generator.
  Pythia pythia;

  // Shorthand for some public members of pythia (also static ones).
  Event& event = pythia.event;
  ParticleDataTable& pdt = pythia.particleData;
  Info& info = pythia.info;

  // Read in commands from external file.
  pythia.readFile("main03.cmnd");    

  // Extract settings to be used in the main program.
  int    nEvent  = pythia.mode("Main:numberOfEvents");
  int    nList   = pythia.mode("Main:numberToList");
  int    nShow   = pythia.mode("Main:timesToShow");
  bool   showCS  = pythia.flag("Main:showChangedSettings");
  bool   showCPD = pythia.flag("Main:showChangedParticleData");

  // Initialize. Beam parameters set in .cmnd file.
  pythia.init();

  // List changed data.
  if (showCS) pythia.settings.listChanged();
  if (showCPD) pdt.listChanged();

  // Book histograms.
  Hist pThard("process pT scale", 100, 0., 200.);
  Hist mult("charged particle multiplicity", 100, -0.5, 799.5);
  Hist dndy("dn/dy for charged particles", 100, -10., 10.);
  Hist dndpT("dn/dpT for charged particles", 100, 0., 10.);

  // Begin event loop.
  int nPace = max(1,nEvent/nShow); 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%nPace == 0) cout << " Now begin event " << iEvent << endl;

    // Generate events. Quit if failure.
    if (!pythia.next()) {
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }
 
    // List first few events, both info, hard process and complete events.
    if (iEvent < nList) { 
      info.list();
      pythia.process.list();
      event.list();
    }

    // Fill hard scale of event.
    pThard.fill( info. pTHat() );

    // Loop over final charged particles in the event. 
    int nCharged = 0;
    for (int i = 0; i < event.size(); ++i) 
    if (event[i].isFinal() && event[i].isCharged()) {

      // Analyze charged particles and fill histograms.
      ++nCharged;
      dndy.fill( event[i].y() );
      dndpT.fill( event[i].pT() );

    // End of particle and event loops. Fill charged multiplicity.
    }
    mult.fill( nCharged );
  }

  // Final statistics. Normalize and output histograms.
  pythia.statistics();
  dndy *= 5. / nEvent;
  dndpT *= 10. / nEvent;
  cout << pThard << mult << dndy << dndpT; 

  // Done.
  return 0;
}
