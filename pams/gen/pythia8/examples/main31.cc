// main31.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Mikhail Kirsanov, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program.
// It illustrates how HepMC can be interfaced to Pythia8.
// It studies the charged multiplicity distribution at the LHC.
// HepMC events are output to the hepmcout31.dat file.
// Written by Mikhail Kirsanov based on main01.cc.

#include "Pythia.h"

#include "HepMCInterface.h"

#include "HepMC/GenEvent.h"

// IO_Ascii writes the event but does not write PDF info.
#include "HepMC/IO_Ascii.h"
// IO_ExtendedAscii.h also writes PDF info.
//#include "HepMC/IO_ExtendedAscii.h" 

//#include "HepMC/IO_AsciiParticles.h"

using namespace Pythia8; 
int main() {

  HepMC::I_Pythia8 ToHepMC;
  //  ToHepMC.set_crash_on_problem();

  // Specify file where HepMC events will be stored.
  // Free to choose whether to include PDF info or not in file.
  HepMC::IO_Ascii ascii_io("hepmcout31.dat",std::ios::out);
  //HepMC::IO_ExtendedAscii ascii_io("hepmcout31.dat",std::ios::out);

  // Not used.
  // HepMC::IO_AsciiParticles ascii_io("hepmcout31.dat",std::ios::out);

  // Generator. Process selection. LHC initialization. Histogram.
  Pythia pythia;
  pythia.readString("HardQCD:all = on");    
  pythia.readString("PhaseSpace:pTHatMin = 20.");    
  pythia.init( 2212, 2212, 14000.);
  Hist mult("charged multiplicity", 100, -0.5, 799.5);
  // Begin event loop. Generate event. Skip if error. List first one.
  for (int iEvent = 0; iEvent < 100; ++iEvent) {
    if (!pythia.next()) continue;
    if (iEvent < 1) {pythia.info.list(); pythia.event.list();} 
    // Find number of all final charged particles and fill histogram.
    int nCharged = 0;
    for (int i = 0; i < pythia.event.size(); ++i) 
      if (pythia.event[i].isFinal() && pythia.event[i].isCharged()) 
        ++nCharged; 
    mult.fill( nCharged );

    // Convert event record to HepMC format.
    HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();

    // With this command only the event record is converted.
    ToHepMC.fill_next_event( pythia.event, hepmcevt );
    // With this command also parton-density information is stored.
    //ToHepMC.fill_next_event( pythia, hepmcevt );

    // Output to file.
    ascii_io << hepmcevt;
    delete hepmcevt;

  // End of event loop. Statistics. Histogram. Done.
  }
  pythia.statistics();
  cout << mult; 
  return 0;
}
