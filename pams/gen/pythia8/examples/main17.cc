// main17.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates (a) how to collect most of the interaction with Pythia,
// in a separate class, with actions mostly dictated by a .cmnd file,
// and (b) how to provide the .cmnd filename on the command line

// Once you have linked the main program you can run it with a command line
// ./main17.exe main17.cmnd > out17

#include "Pythia.h"

using namespace Pythia8; 

//**************************************************************************

// Put most of your Pythia interaction in the PythiaWrapper class.
// Note: this way you restrict yourself to a subset of the full Pythia
// functionality, for better or worse.

class PythiaWrapper {

public:

  // Constructor can be empty.
  PythiaWrapper() {}

  // Initialization actions: pythia.init() and more.
  // Input from command-line arguments of main program.
  bool beg(int argc, char* argv[]);
 
  // Event generation actions: pythia.next() and more.
  bool gen();

  // Final actions: statistics.
  bool fin();

  // Provide number of events to generate.
  int  nEvt() const {return nEvent;}

  // Restricted access to the pythia.event event record.
  int  size() const {return pythia.event.size();}
  const Particle& operator[](int i) const {return pythia.event[i];}

private:

  // The Pythia class that does the job.
  Pythia pythia;

  // Various quantities read in at the beginning and used later.
  int nEvent, nList, nShow, nAbort, nPace, iEvent, iList, iAbort;

};

//*********

// The initialization code. 

bool PythiaWrapper::beg(int argc, char* argv[]) {

  // Check that correct number of command-line arguments
  if (argc != 2) {
    cerr << " Unexpected number of command-line arguments. \n"
         << " You are expected to provide a file name and nothing else. \n"
         << " Program stopped! " << endl;
    return false;
  }

  // Check that the provided file name corresponds to an existing file.
  ifstream is(argv[1]);  
  if (!is) {
    cerr << " Command-line file " << argv[1] << " was not found. \n"
         << " Program stopped! " << endl;
    return false;
  }

  // Confirm that external file will be used for settings..
  cout << " PYTHIA settings will be read from file " << argv[1] << endl;

  // Read in the cards file with Pythia commands.
  pythia.readFile(argv[1]);
 
  // Initialization, using the Beams settings. Give up if failure.
  if ( !pythia.init() ) return false;

  // Shorthand for pythia.settings and pythia.particleData.
  Settings&          pSet = pythia.settings;
  ParticleDataTable& pDat = pythia.particleData;

  // List settings: changed or all.
  if ( pSet.flag("Main:showChangedSettings") )  pSet.listChanged();
  if ( pSet.flag("Main:showAllSettings") ) pSet.listAll();

  // List particle data; one special, changed (with resonances?), or all.  
  if ( pSet.mode("Main:showOneParticleData") > 0 ) 
    pDat.list( pSet.mode("Main:showOneParticleData") );
  if ( pSet.flag("Main:showChangedParticleData") ) 
    pDat.listChanged( pSet.flag("Main:showChangedResonanceData") );
  if ( pSet.flag("Main:showAllParticleData") ) pDat.listAll();

  // Extract settings to be used in event generation loop.
  nEvent = pSet.mode("Main:numberOfEvents");
  nList  = pSet.mode("Main:numberToList");
  nShow  = pSet.mode("Main:timesToShow");
  nAbort = pSet.mode("Main:timesAllowErrors");

  // Initialize counters to use in event generation loop.
  nPace  = max( 1, nEvent / nShow); 
  iEvent = 0;
  iList  = 0;
  iAbort = 0; 

  // Done.
  return true;

} 

//*********

// The event generation code. 

bool PythiaWrapper::gen() {

  // Handle occasional abort by internal loop.
  for( ;  ;  ) { 

    // At times print line with progress report. Count up event number.
    if (iEvent%nPace == 0) cout << " Now begin event " << iEvent << endl;
    ++iEvent; 

    // Generate events, and check whether generation failed.
    if ( !pythia.next() ) {

      // If failure because reached end of file then quit.
      if ( pythia.info.atEndOfFile() ) return false; 

      // First few failures write off as "acceptable" errors, then quit.
      if ( ++iAbort <= nAbort ) continue;
      return false;
    }

    // End of internal loop - valid event generated.
    break;
  }
 
  // List first few events, both hard process and complete events.
  if ( ++iList <= nList ) { 
    pythia.info.list();
    pythia.process.list();
    pythia.event.list();
  }

  // Done.
  return true;

} 

//*********

// The finishing code. 

bool PythiaWrapper::fin() {

  // Final statistics.
  pythia.statistics( pythia.settings.flag("Main:showAllStatistics") );

  // Done.
  return true;

} 

//**************************************************************************

// You should not need to touch the main program: its actions are 
// determined by the .cmnd file and the rest belongs in MyAnalysis.

int main(int argc, char* argv[]) {

  // Declare generator.
  PythiaWrapper pWrap;

  // Initialize it with command-line arguments. Done if fail.
  if ( !pWrap.beg(argc, argv) ) return 1;

  // Book histograms.
  Hist ZPmass("mass of gamma*/Z0/Z' state", 100, 0., 2000.);
  Hist ZPpT("pT of gamma*/Z0/Z' state", 100, 0., 500.);    

  // Begin of event loop. 
  int nEvent = pWrap.nEvt();
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {

    // Generate event. Quit if returns false.
    if ( !pWrap.gen() ) break;

    // Find the last copy of Z', i.e. after cascades, just before decay.
    int iZP = 0;
    for (int i = 0; i < pWrap.size(); ++i) if (pWrap[i].id() == 32) iZP = i;

    // Histogram its mass and pT.
    ZPmass.fill( pWrap[iZP].m() );
    ZPpT.fill( pWrap[iZP].pT() );

  // End of event loop.
  }

  // Final statistics and histograms.
  pWrap.fin();
  cout << ZPmass << ZPpT;

  // Done.
  return 0;
}
