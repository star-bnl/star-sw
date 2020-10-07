// main08.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how to combine subruns in pT bins.

#include "Pythia.h"

using namespace Pythia8; 

int main() {

  // Two different modes are illustrated for setting the pT ranges.
  // 1 : Hardcoded in the main program.
  // 2 : Using the Main:subrun keyword in a separate command file.
  int mode = 2;

  // Number of events to generate per bin, and to list.
  int nEvent = 10000;

  // Book histograms.
  Hist pTraw("pTHat distribution, unweighted", 100, 0., 1000.);
  Hist pTnorm("pTHat distribution, weighted", 100, 0., 1000.);
  Hist pTpow3("pTHat distribution, pT3*weighted", 100, 0., 1000.);
  Hist pTpow5("pTHat distribution, pT5*weighted", 100, 0., 1000.);
  Hist pTnormPart("pTHat distribution, weighted", 100, 0., 1000.);
  Hist pTpow3Part("pTHat distribution, pT3*weighted", 100, 0., 1000.);
  Hist pTpow5Part("pTHat distribution, pT5*weighted", 100, 0., 1000.);

  // Generator.
  Pythia pythia;

  // Shorthand for some public members of pythia (also static ones).
  Settings& settings = pythia.settings;
  Info& info = pythia.info;

  // Set up to generate QCD jets, but only the hard process itself.
  pythia.readString("HardQCD:all = on");  
  pythia.readString("PartonLevel:all = off");  

  // Number of bins to use. In mode 2 read from main08.cmnd file.
  int nBin = 5;
  if (mode == 2) {
    pythia.readFile("main08.cmnd"); 
    nBin = pythia.mode("Main:numberOfSubruns");
  } 

  // Mode 1: set up five pT bins - last one open-ended.
  double pTlimit[6] = {100., 150., 250., 400., 600., 0.};

  // Loop over number of bins, i.e. number of subruns.
  for (int iBin = 0; iBin < nBin; ++iBin) {

     // Mode 1: hardcoded here. Using settings.parm allows non-string input.  
     if (mode <= 1) { 
       settings.parm("PhaseSpace:pTHatMin", pTlimit[iBin]);  
       settings.parm("PhaseSpace:pTHatMax", pTlimit[iBin + 1]);
     }

     // Mode 2: subruns stored in the main08.cmnd file.
     else pythia.readFile("main08.cmnd", iBin);  

     // Initialize for LHC at 14 TeV.
     pythia.readString("Beams:eCM = 14000.");  
     pythia.init();

    // List changed data.
    settings.listChanged();

    // Reset local histograms (that need to be rescaled before added).
    pTnormPart.null();
    pTpow3Part.null();
    pTpow5Part.null();

    // Begin event loop.
    for (int iEvent = 0; iEvent < nEvent; ++iEvent) {

      // Generate events. Quit if failure.
      if (!pythia.next()) break;

      // Fill hard scale of event.
      double pTHat = info. pTHat();
      pTraw.fill( pTHat );
      pTnormPart.fill( pTHat );
      pTpow3Part.fill( pTHat, pow3(pTHat) );
      pTpow5Part.fill( pTHat, pow5(pTHat) );

    // End of event loop. Statistics.
    }
    pythia.stat();

    // Normalize each case to cross section/(bin * event), and add to sum.
    double sigmaNorm = info.sigmaGen() / (10. * nEvent);
    pTnorm += sigmaNorm * pTnormPart;
    pTpow3 += sigmaNorm * pTpow3Part;
    pTpow5 += sigmaNorm * pTpow5Part;

  // End of pT-bin loop.
  }

  // Output histograms.
  cout << pTraw << pTnorm << pTpow3 << pTpow5; 

  // Done.
  return 0;
}
