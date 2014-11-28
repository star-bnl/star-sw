// main06.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how to use internal Pythia8 processes,
// with special emphasis on elastic/diffractive processes.
// All input is specified in the main06.cmnd file.

#include "Pythia.h"

using namespace Pythia8; 

//**************************************************************************

int main() {

  // Generator. Shorthand for the event and for settings.
  Pythia pythia;
  Event& event = pythia.event;
  Settings& settings = pythia.settings;

  // Read in commands from external file.
  pythia.readFile("main06.cmnd");    

  // Extract settings to be used in the main program.
  int    nEvent    = settings.mode("Main:numberOfEvents");
  int    nList     = settings.mode("Main:numberToList");
  int    nShow     = settings.mode("Main:timesToShow");
  int    nAbort    = settings.mode("Main:timesAllowErrors");
  bool   showCS    = settings.flag("Main:showChangedSettings");
  bool   showAS    = settings.flag("Main:showAllSettings");
  bool   showCPD   = settings.flag("Main:showChangedParticleData");
  bool   showAPD   = settings.flag("Main:showAllParticleData");

  // Debug: kill Coulomb part but keep rest of elastic description. 
  //Settings::forceParm("StandardModel:alphaEM0", 1e-10);
  //Settings::forceParm("StandardModel:alphaEMmZ", 1e-10);
 
  // Initialize. Beam parameters set in .cmnd file.
  pythia.init();

  // List settings.
  if (showCS) settings.listChanged();
  if (showAS) settings.listAll();

  // List particle data.  
  if (showCPD) ParticleDataTable::listChanged();
  if (showAPD) ParticleDataTable::listAll();

  // Book histograms.
  Hist pTspec("scattering pT spectrum", 100, 0., 2.5); 
  Hist tSpecEl("elastic |t| spectrum", 100, 0., 1.);
  Hist tSpecElLog("elastic log10(|t|) spectrum", 100, -5., 0.);
  Hist tSpecSD("single diffractive |t| spectrum", 100, 0., 2.); 
  Hist tSpecDD("double diffractive |t| spectrum", 100, 0., 5.); 
  Hist mSpec("scattering mass spectrum", 100, 0., 100.); 
  Hist mLogSpec("log10(scattering mass spectrum)", 100, 0., 4.); 
  Hist nChg("number of charged particles in diffractive event", 
    100, -0.5, 99.5);
  Hist nDiff("number of final particles in diffractive system", 
    100, -0.5, 99.5);
  Hist mSpec1("scattering mass spectrum when 1 product", 100, 0., 10.); 
 
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

    // Study generic event properties.
    double pT = pythia.info.pTHat();
    pTspec.fill( pT );

    // Study properties geared towards elastic/diffractive events.
    mSpec.fill( pythia.info.m3Hat() );
    mSpec.fill( pythia.info.m4Hat() );
    mLogSpec.fill( log10(pythia.info.m3Hat()) );
    mLogSpec.fill( log10(pythia.info.m4Hat()) );
    int code = pythia.info.code();
    double tAbs = abs(pythia.info.tHat());
    if (code == 102) {
      tSpecEl.fill(tAbs);
      tSpecElLog.fill(log10(tAbs));
    }
    else if (code == 103 || code == 104) tSpecSD.fill(tAbs);
    else if (code == 105) tSpecDD.fill(tAbs);

    // Study properties geared towards minimum-bias or jet physics.
    int nch = 0;
    for (int i = 1; i < event.size(); ++i)
      if (event[i].isFinal() && event[i].isCharged()) ++nch; 
    if (code != 102) nChg.fill( nch );

    // Multiplicity distribution in diffractive system.
    for (int i = 0; i < 2; ++i) 
    if ( (i == 0 && pythia.info.isDiffractiveA()) 
      || (i == 1 && pythia.info.isDiffractiveB()) ) {
      int ndiff = 0;
      for (int j = 5; j < event.size(); ++j) 
      if (event[j].isFinal()) {
        // Trace back final particle to see which system it comes from.
        int k = j;
        do k = event[k].mother1(); 
        while (k > 4);
        if (k == i + 3) ++ndiff;
      }  
      nDiff.fill( ndiff );
      if (ndiff == 1) mSpec1.fill( event[i +3].m() ); 
    }

  // End of event loop.
  }

  // Final statistics and histograms.
  pythia.statistics();
  cout << pTspec << tSpecEl << tSpecElLog << tSpecSD << tSpecDD << mSpec 
       << mLogSpec << nChg << nDiff << mSpec1;

  // Done.
  return 0;
}
