// main22.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Peter Skands, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how to run SUSY processes in Pythia8.
// All input is specified in the main22.cmnd file.

#include "Pythia.h"

using namespace Pythia8; 

int main() {

  // Generator. Shorthand for the event and the (static) Settings.
  Pythia pythia;
  Event& event = pythia.event;
  Settings& settings = pythia.settings;

  // Read in commands from external file.
  pythia.readFile("main22.cmnd");    

  // Extract settings to be used in the main program.
  int nEvent = settings.mode("Main:numberOfEvents");
  int nList  = settings.mode("Main:numberToList");
  int nShow  = settings.mode("Main:timesToShow");
  bool showChangedSettings = settings.flag("Main:showChangedSettings");
  bool showAllSettings = settings.flag("Main:showAllSettings");
  bool showChangedParticleData 
    = settings.flag("Main:showChangedParticleData");
  bool showAllParticleData = settings.flag("Main:showAllParticleData");
  double eCM = settings.parm("Beams:eCM");

  // Initialize. Beam parameters set in .cmnd file.
  pythia.init();

  // List changed data.
  if (showChangedSettings) settings.listChanged();
  if (showAllSettings) settings.listAll();

  // List particle data.  
  if (showChangedParticleData) ParticleDataTable::listChanged();
  if (showAllParticleData) ParticleDataTable::listAll();

  // Histograms.
  double epTol = 1e-6 * eCM;
  Hist epCons("deviation from energy-momentum conservation",100,0.,epTol);
  Hist nFinal("final particle multiplicity",100,-0.5,799.5);
  Hist dnparticledy("dn/dy for particles",100,-10.,10.);

  // Begin event loop.
  int nPace = max(1,nEvent/nShow); 
  for (int iEvent = 0; iEvent < nEvent; ++iEvent) {
    if (iEvent%nPace == 0) cout << " Now begin event " << iEvent << endl;

    // Generate events. Quit if failure.
    if (!pythia.next()) {
      cout << " Event generation aborted prematurely, owing to error!\n"; 
      break;
    }
 
    // List first few events, both hard process and complete events.
    if (iEvent < nList) { 
      pythia.process.list();
      event.list();
    }

    // Loop over final particles in the event. 
    int nFin = 0;
    Vec4 pSum;
    for (int i = 0; i < event.size(); ++i) if (event[i].isFinal()) {
      nFin++;
      pSum += event[i].p();
      dnparticledy.fill(event[i].y());
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
  cout << epCons << nFinal << dnparticledy; 

  return 0;
}

/*

Pythia8107 with main22.spc

| q qbar -> ~chi_10 ~chi_10                     1201 |      195282      18237      18237 |   1.050e-11  4.361e-14 |
| q qbar -> ~chi_10 ~chi_20                     1202 |       20837       1806       1806 |   1.036e-12  1.466e-14 |
| q qbar -> ~chi_10 ~chi_30                     1203 |       14590       3534       3534 |   2.030e-12  1.970e-14 |
| q qbar -> ~chi_10 ~chi_40                     1204 |        4549        789        789 |   4.422e-13  9.488e-15 |
| q qbar -> ~chi_20 ~chi_20                     1205 |      249758      32059      32059 |   1.821e-11  5.781e-14 |
| q qbar -> ~chi_20 ~chi_30                     1206 |       42509      10542      10542 |   6.040e-12  3.347e-14 |
| q qbar -> ~chi_20 ~chi_40                     1207 |       19282       2964       2964 |   1.705e-12  1.809e-14 |
| q qbar -> ~chi_30 ~chi_30                     1208 |          14          1          1 |   1.512e-15  1.512e-15 |
| q qbar -> ~chi_30 ~chi_40                     1209 |      126415      29986      29986 |   1.707e-11  5.545e-14 |
| q qbar -> ~chi_40 ~chi_40                     1210 |         498         82         82 |   4.403e-14  2.890e-15 |

Pythia 6.4.16 with same spectrum and neutralinos forced stable. 

! 216 f + fbar -> ~chi1 + ~chi1    I       177200       1875296 I  1.030E-11 I
! 217 f + fbar -> ~chi2 + ~chi2    I       324265       2542319 I  1.882E-11 I
! 218 f + fbar -> ~chi3 + ~chi3    I           38           163 I  2.196E-15 I
! 219 f + fbar -> ~chi4 + ~chi4    I          789          4886 I  4.536E-14 I
! 220 f + fbar -> ~chi1 + ~chi2    I        17541        207626 I  1.017E-12 I
! 221 f + fbar -> ~chi1 + ~chi3    I        35838        146111 I  2.064E-12 I
! 222 f + fbar -> ~chi1 + ~chi4    I         7859         44297 I  4.502E-13 I
! 223 f + fbar -> ~chi2 + ~chi3    I       105194        426610 I  6.115E-12 I
! 224 f + fbar -> ~chi2 + ~chi4    I        30338        198133 I  1.751E-12 I
! 225 f + fbar -> ~chi3 + ~chi4    I       300938       1266278 I  1.746E-11 I

*/

