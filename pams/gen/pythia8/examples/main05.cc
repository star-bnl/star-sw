// main05.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It studies jet production at the LHC, using CellJet.

#include "Pythia.h"
using namespace Pythia8;
 
int main() {

  // Number of events, generated and listed ones.
  int nEvent = 200;
  int nList = 0;
  int nListJets = 5;

  // Generator. Process selection. LHC initialization.
  Pythia pythia;
  pythia.readString("HardQCD:all = on");    
  pythia.readString("PhaseSpace:pTHatMin = 200.");    
  pythia.init( 2212, 2212, 14000.);

  // Jet finder. Note that arguments can be used to override defaults. 
  CellJet cellJet;

  // Histograms.
  Hist nJets("number of jets", 20, -0.5, 19.5);
  Hist eTjets("eT for jets", 100, 0., 500.);
  Hist etaJets("eta for jets", 100, -5., 5.);
  Hist phiJets("phi for jets", 100, -M_PI, M_PI);  
  Hist distJets("R distance between jets", 100, 0., 10.);
  Hist eTdiff("eT difference", 100, -100., 400.);

  // Begin event loop. Generate event. Skip if error. list first few. 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (!pythia.next()) continue;

    // List first few events.
    if (iEvent < nList) {
      pythia.info.list(); 
      pythia.process.list();
      pythia.event.list();
    }

    // Analyze jet properties. List first few. 
    cellJet. analyze( pythia.event );
    if (iEvent < nListJets) cellJet.list();

    // Fill inclusive jet distributions.
    nJets.fill( cellJet.size() );
    for (int i = 0; i < cellJet.size(); ++i) {
      eTjets.fill( cellJet.eT(i) );
      etaJets.fill( cellJet.etaWeighted(i) );
      phiJets.fill( cellJet.phiWeighted(i) );
    }

    // Fill distance between jets.
    for (int i = 0; i < cellJet.size() - 1; ++i)
    for (int j = i +1; j < cellJet.size(); ++j) {
      double dEta = cellJet.etaWeighted(i) 
        - cellJet.etaWeighted(j);
      double dPhi = abs( cellJet.phiWeighted(i) 
        - cellJet.phiWeighted(j) );
      if (dPhi > M_PI) dPhi = 2. * M_PI - dPhi;
      double dR = sqrt( pow2(dEta) + pow2(dPhi) );
      distJets.fill( dR );
    }

    // Fill ET-difference between jets (to check ordering of list).
    for (int i = 1; i < cellJet.size(); ++i) 
      eTdiff.fill( cellJet.eT(i-1)- cellJet.eT(i) );

  // End of event loop. Statistics. Histograms. 
  }
  pythia.statistics();
  cout << nJets << eTjets << etaJets << phiJets 
       << distJets << eTdiff;

  // Done. 
  return 0;
}
