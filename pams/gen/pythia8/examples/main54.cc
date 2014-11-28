// main54.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Mikhail Kirsanov, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates the chain Pythia6 -> Pythia8 -> HepMC.
// All input is specified in the main54.cmnd file.
// HepMC events are output to the hepmcout54.dat file.
// Written by Mikhail Kirsanov based on main52.cc.

#include "Pythia.h"
#include "LHAFortran.h"
#include "Pythia6Interface.h"

#include "HepMCInterface.h"

#include "HepMC/GenEvent.h"
#include "HepMC/IO_Ascii.h"
//#include "HepMC/IO_AsciiParticles.h"

//#include "HepMC/PythiaWrapper.h" // incompatible with Pythia8

using namespace Pythia8; 

//**************************************************************************

// Implement initialization fillHepRup method for Pythia6 example.

bool LHAupFortran::fillHepRup() { 

  // Set process to generate.
  // Example: Z+jet production, must set pTmin, canset mMin.
  Pythia6Interface::pygive("msel = 13"); 
  Pythia6Interface::pygive("ckin(3) = 20."); 
  Pythia6Interface::pygive("ckin(1) = 50."); 

  // Switch off everything but Z0 -> leptons. 
  // Warning: only works with version Pythia 6.411 onwards.
  Pythia6Interface::pygive("23:alloff"); 
  Pythia6Interface::pygive("23:onifany = 11 13 15"); 

  // Speed up initialization: multiple interactions only in C++ code.
  Pythia6Interface::pygive("mstp(81)=0");
    
  // Initialize for 14 TeV pp collider.
  Pythia6Interface::pyinit("cms","p","p",14000.);   

  // Fill initialization information in HEPRUP.
  Pythia6Interface::pyupin();

  // Done.
  return true;

}

//**************************************************************************

// Implement event generation fillHepEup method for Pythia6 example.

bool LHAupFortran::fillHepEup() { 

  // Generate and fill the next Pythia6 event in HEPEUP.
  Pythia6Interface::pyupev();

  // Done.
  return true;

}

//**************************************************************************

int main() {

  //  ToHepMC.set_crash_on_problem();
  HepMC::I_Pythia8 ToHepMC;

  // Specify file where HepMC events will be stored.
  HepMC::IO_Ascii ascii_io("hepmcout54.dat",std::ios::out);
//  HepMC::IO_AsciiParticles ascii_io("hepmcout54.dat",std::ios::out);

  // Generator. Shorthand for the event and for settings.
  Pythia8::Pythia pythia;
  Event& event = pythia.event;
  Settings& settings = pythia.settings;

  // Read in commands from external file.
  pythia.readFile("main54.cmnd");

  // Extract settings to be used in the main program.
  int  nEvent  = settings.mode("Main:numberOfEvents");
  int  nList   = settings.mode("Main:numberToList");
  int  nShow   = settings.mode("Main:timesToShow");
  int  nAbort  = settings.mode("Main:timesAllowErrors");
  bool showCS  = settings.flag("Main:showChangedSettings");
  bool showAS  = settings.flag("Main:showAllSettings");
  bool showCPD = settings.flag("Main:showChangedParticleData");
  bool showAPD = settings.flag("Main:showAllParticleData");

  // Initialize to access Pythia6 generator by Les Houches interface.
  LHAupFortran pythia6;
  pythia.init(&pythia6);    

  // List changed data.
  if (showCS) settings.listChanged();
  if (showAS) settings.listAll();

  // List particle data.  
  if (showCPD) ParticleDataTable::listChanged();
  if (showAPD) ParticleDataTable::listAll();

  // Histograms.
  double eCM   = 14000.;
  double epTol = 1e-7 * eCM;
  Hist epCons("deviation from energy-momentum conservation",100,0.,epTol);
  Hist nFinal("final particle multiplicity",100,-0.5,1599.5);
  Hist nChg("final charged multiplicity",100,-0.5,799.5);
  Hist nISR("number of ISR emissions for hard system",40,-0.5,39.5);
  Hist nMI("number of MI (excluding hard system)",100,-0.5,99.5);
  Hist nISRMI("number of ISR emissions per MI",40,-0.5,39.5);
  Hist nFSR("total number of FSR emissions",100,-0.5,299.5);
  Hist nJUN("number of junctions",10,-0.5,9.5);
  Hist pThard("ISR pT kick to hard system",100,0.,400.);
  Hist sumETparticle("summed ET of particles",100,0.,2000.);
  Hist dnCHparticleDy("dn_charged/dy for particles",100,-10.,10.);
  Hist dETparticleDy("dET/dy for particles",100,-10.,10.);

  // Begin event loop.
  int nShowPace = max(1,nEvent/nShow); 
  int iAbort = 0; 
  bool generated;
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%nShowPace == 0) cout << " Now begin event " 
      << iEvent << endl;

    // Generate events. Quit if too many failures.
    generated = pythia.next();
    if (!generated) {
      if (++iAbort < nAbort) continue;
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }
    cout << " successfully generated = " << generated << endl;
 
    // List first few events, both hard process and complete events.
    if (iEvent < nList) { 
      pythia.process.list();
      event.list();
    }

    // Convert event record to HepMC format and output to file.
    HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();
    ToHepMC.fill_next_event( event, hepmcevt );
    ascii_io << hepmcevt;
    delete hepmcevt;

    // Number of ISR for hard subprocess.
    int nisr = -1;
    int iNow = 3;
    do { iNow = event[iNow].mother1(); ++nisr;}
    while (iNow > 1 && abs(event[iNow].status()) < 50 ); 
    nISR.fill(nisr);

    // Total pT kick of hard subsystem.
    Vec4 pHard;
    for (int i = 0; i < event.size(); ++i) {
      if (abs(event[i].status()) > 30) break;
      if (event[i].status() == -22 || event[i].status() == -23) {      
        int iNow = i;
        while (event[iNow].daughter2() == event[iNow].daughter1() &&
          event[iNow].daughter1() > iNow) iNow = event[iNow].daughter1();
        pHard += event[iNow].p();
      }
    }
    pThard.fill(pHard.pT());

    // Reset quantities to be summed over event.
    int nfin = 0;
    int nch = 0;
    int nmi = 0;
    int nfsr = 0;
    Vec4 pSum = - (event[1].p() + event[2].p());
    double eTsum = 0.;

    // Loop over particles in the event. 
    for (int i = 0; i < event.size(); ++i) {

      // Number of MI and of ISR per MI.
      if (i < event.size() - 1 && event[i].status() == -31 
        && event[i+1].status() == -31) {
        ++nmi;
        int inow = i;
        int nisrmi = -1;
        do { inow = event[inow].mother1(); ++ nisrmi;}
        while (inow > 1 && abs(event[inow].status()) < 50) ; 
        nISRMI.fill(nisrmi);
      }
    
      // Number of FSR branchings.
      if (event[i].status() == -52) ++nfsr; 

      // Specialize to final particles. Total multiplicity and momentum.
      if (event[i].status() > 0) {
        ++nfin;
        if (event[i].isCharged()) ++nch;
        pSum += event[i].p();

        // Final-state particle spectra.
        double eTnow = event[i].pT();
        double ynow = event[i].y();
        eTsum += eTnow;
        if (event[i].isCharged()) dnCHparticleDy.fill(ynow);
        dETparticleDy.fill(ynow,eTnow);

      // End of loop over (final/all) particles.
      }
    }

    // Energy-momentum deviation.
    double epDev = abs(pSum.e()) + abs(pSum.px()) + abs(pSum.py())
      + abs(pSum.pz());
    epCons.fill(epDev);
      
    // Fill summed quantities.
    nFinal.fill(nfin);
    nChg.fill(nch);
    nMI.fill(nmi);
    nFSR.fill(nfsr);
    nJUN.fill( event.sizeJunction() );
    sumETparticle.fill(eTsum);

  // End of event loop.
  }

  // Final statistics.
  pythia.statistics();

  // Histogram normalization.
  double factor = 5. / (nEvent - nAbort);  
  dnCHparticleDy *= factor;
  dETparticleDy *= factor;

  // Histogram output.
  cout << epCons << nFinal<< nChg << nISR << nMI << nISRMI << nFSR 
       << nJUN << pThard << sumETparticle << dnCHparticleDy 
       << dETparticleDy; 

  // Done.
  return 0;
}
