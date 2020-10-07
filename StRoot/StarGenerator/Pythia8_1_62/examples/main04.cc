// main04.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how to generate and study "total cross section" processes,
// i.e. elastic, single and double diffractive, and the "minimum-bias" rest.
// All input is specified in the main06.cmnd file.
// Note that the "total" cross section does NOT include 
// the Coulomb contribution to elastic scattering, as switched on here.

#include "Pythia.h"

using namespace Pythia8; 

//==========================================================================

int main() {

  // Generator. Shorthand for the event.
  Pythia pythia;
  Event& event = pythia.event;

  // Read in commands from external file.
  pythia.readFile("main04.cmnd");    

  // Extract settings to be used in the main program.
  int    nEvent    = pythia.mode("Main:numberOfEvents");
  int    nAbort    = pythia.mode("Main:timesAllowErrors");
 
  // Initialize. 
  pythia.init();

  // Book histograms: multiplicities and mean transverse momenta.
  Hist nChg("number of charged particles; all", 100, -0.5, 799.5);
  Hist nChgSD("number of charged particles; single diffraction", 
    100, -0.5, 799.5);
  Hist nChgDD("number of charged particles, double diffractive", 
    100, -0.5, 799.5);
  Hist nChgND("number of charged particles, non-diffractive", 
    100, -0.5, 799.5);
  Hist pTnChg("<pt>(n_charged) all", 100, -0.5, 799.5);
  Hist pTnChgSD("<pt>(n_charged) single diffraction", 100, -0.5, 799.5);
  Hist pTnChgDD("<pt>(n_charged) double diffraction", 100, -0.5, 799.5);
  Hist pTnChgND("<pt>(n_charged) non-diffractive   ", 100, -0.5, 799.5);

  // Book histograms: ditto as function of separate subsystem mass.
  Hist mLogInel("log10(mass), by diffractive system", 100, 0., 5.);
  Hist nChgmLog("<n_charged>(log10(mass))", 100, 0., 5.);
  Hist pTmLog("<pT>_charged>(log10(mass))", 100, 0., 5.);

  // Book histograms: elastic/diffractive.
  Hist tSpecEl("elastic |t| spectrum", 100, 0., 1.);
  Hist tSpecElLog("elastic log10(|t|) spectrum", 100, -5., 0.);
  Hist tSpecSD("single diffractive |t| spectrum", 100, 0., 2.); 
  Hist tSpecDD("double diffractive |t| spectrum", 100, 0., 5.); 
  Hist mSpec("diffractive mass spectrum", 100, 0., 100.); 
  Hist mLogSpec("log10(diffractive mass spectrum)", 100, 0., 4.); 

  // Book histograms: inelastic nondiffractive "minbias".
  double pTmax = 20.;
  double bMax = 4.;
  Hist pTspec("total pT_hard spectrum", 100, 0., pTmax); 
  Hist pTspecND("nondiffractive pT_hard spectrum", 100, 0., pTmax); 
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
  int iAbort = 0; 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {

    // Generate events. Quit if too many failures.
    if (!pythia.next()) {
      if (++iAbort < nAbort) continue;
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }

    // Extract event classification.
    int code = pythia.info.code();
    
    // Charged multiplicity and mean pT: all and by event class.
    int nch = 0;
    double pTsum = 0.; 
    for (int i = 1; i < event.size(); ++i)
    if (event[i].isFinal() && event[i].isCharged()) {
      ++nch; 
      pTsum += event[i].pT();
    }
    nChg.fill( nch );
    if (nch > 0) pTnChg.fill( nch, pTsum/nch);
    if (code == 103 || code == 104) {
      nChgSD.fill( nch );
      if (nch > 0) pTnChgSD.fill( nch, pTsum/nch);
    } else if (code == 105) {
      nChgDD.fill( nch );
      if (nch > 0) pTnChgDD.fill( nch, pTsum/nch);
    } else if (code == 101) {
      nChgND.fill( nch );
      if (nch > 0) pTnChgND.fill( nch, pTsum/nch);
      double mLog = log10( event[0].m() ); 
      mLogInel.fill( mLog );  
      nChgmLog.fill( mLog, nch );  
      if (nch > 0) pTmLog.fill( mLog, pTsum / nch );  
    }

    // Charged multiplicity and mean pT: per diffractive system.
    for (int iDiff = 0; iDiff < 2; ++iDiff) 
    if ( (iDiff == 0 && pythia.info.isDiffractiveA()) 
      || (iDiff == 1 && pythia.info.isDiffractiveB()) ) {
      int ndiff = 0;
      double pTdiff = 0.; 
      for (int i = 5; i < event.size(); ++i) 
      if (event[i].isFinal() && event[i].isCharged()) {
        // Trace back final particle to see which system it comes from.
        int k = i;
        do k = event[k].mother1(); 
        while (k > 4);
        if (k == iDiff + 3) {
          ++ndiff;
          pTdiff += event[i].pT();
        }
      } 
      double mLog = log10(event[iDiff+3].m() );  
      mLogInel.fill( mLog );  
      nChgmLog.fill( mLog, ndiff );  
      if (ndiff > 0) pTmLog.fill( mLog, pTdiff / ndiff );  
    }

    // Study pT spectrum of all hard collisions, no distinction.
    double pT = pythia.info.pTHat();
    pTspec.fill( pT );

    // Study t distribution of elastic/diffractive events.
    if (code > 101) {
      double tAbs = abs(pythia.info.tHat());
      if (code == 102) {
        tSpecEl.fill(tAbs);
        tSpecElLog.fill(log10(tAbs));
      }
      else if (code == 103 || code == 104) tSpecSD.fill(tAbs);
      else if (code == 105) tSpecDD.fill(tAbs);

      // Study diffractive mass spectrum.
      if (pythia.info.isDiffractiveA()) {
        mSpec.fill( event[3].m() );  
        mLogSpec.fill( log10(event[3].m()) );
      }
      if (pythia.info.isDiffractiveB()) { 
        mSpec.fill( event[4].m() );
        mLogSpec.fill( log10(event[4].m()) );
      }

    // Study nondiffractive inelastic events in (pT, b) space.
    } else {
      double b = pythia.info.bMPI();
      double enhance = pythia.info.enhanceMPI();
      int nMPI = pythia.info.nMPI();
      pTspecND.fill( pT );
      bSpec.fill( b );
      enhanceSpec.fill( enhance );
      number.fill( nMPI );
      if (b < 0.5) pTb1.fill( pT );
      else if (b < 1.0) pTb2.fill( pT );
      else if (b < 1.5) pTb3.fill( pT );
      else pTb4.fill( pT );
      if (pT < 2.) bpT1.fill( b );
      else if (pT < 5.) bpT2.fill( b );
      else if (pT < 15.) bpT3.fill( b );
      else bpT4.fill( b );
    }

  // End of event loop.
  }

  // Final statistics and histograms.
  pythia.stat();
  pTnChg   /= nChg;
  pTnChgSD /= nChgSD;
  pTnChgDD /= nChgDD;
  pTnChgND /= nChgND;
  nChgmLog /= mLogInel;
  pTmLog   /= mLogInel;
  cout << nChg << nChgSD << nChgDD << nChgND
       << pTnChg << pTnChgSD << pTnChgDD << pTnChgND
       << mLogInel << nChgmLog << pTmLog
       << tSpecEl << tSpecElLog << tSpecSD << tSpecDD << mSpec << mLogSpec 
       << pTspec << pTspecND << bSpec << enhanceSpec << number
       << pTb1 << pTb2 << pTb3 << pTb4 << bpT1 << bpT2 << bpT3 << bpT4;

  // Done.
  return 0;
}
