// FragmentationFlavZpT.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file contains helper classes for fragmentation.
// StringFlav is used to select quark and hadron flavours.
// StringPT is used to select transverse momenta.
// StringZ is used to sample the fragmentation function f(z).

#ifndef Pythia8_FragmentationFlavZpT_H
#define Pythia8_FragmentationFlavZpT_H

#include "Basics.h"
#include "ParticleData.h"
#include "PythiaStdlib.h"
#include "Settings.h"

namespace Pythia8 {


//**************************************************************************

// The FlavContainer class is a simple container for flavour, 
// including the extra properties needed for popcorn baryon handling.
// id = current flavour. 
// rank = current rank; 0 for endpoint flavour and then increase by 1.
// nPop = number of popcorn mesons yet to be produced (1 or 0).
// idPop = (absolute sign of) popcorn quark, shared between B and Bbar.
// idVtx = (absolute sign of) vertex (= non-shared) quark in diquark.

class FlavContainer {

public:

  // Constructor. 
  FlavContainer(int idIn = 0, int rankIn = 0, int nPopIn = 0, 
    int idPopIn = 0, int idVtxIn = 0) : id(idIn), rank(rankIn), 
    nPop(nPopIn), idPop(idPopIn), idVtx(idVtxIn) {}

  // Overloaded equal operator.
  FlavContainer& operator=(const FlavContainer& flav) { if (this != &flav) { 
    id = flav.id; rank = flav.rank; nPop = flav.nPop; idPop = flav.idPop;
    idVtx = flav.idVtx; } return *this; }

  // Invert flavour.
  FlavContainer& anti() {id = -id; return *this;}

  // Read in a container into another, without/with id sign flip.
  FlavContainer& copy(const FlavContainer& flav) { if (this != &flav) { 
    id = flav.id; rank = flav.rank; nPop = flav.nPop; idPop = flav.idPop;
    idVtx = flav.idVtx; } return *this; }
  FlavContainer& anti(const FlavContainer& flav) { if (this != &flav) { 
    id = -flav.id; rank = flav.rank; nPop = flav.nPop; idPop = flav.idPop;
    idVtx = flav.idVtx; } return *this; }

  // Stored properties.
  int id, rank, nPop, idPop, idVtx;
  
};

//**************************************************************************

// The StringFlav class is used to select quark and hadron flavours.

class StringFlav {

public:

  // Constructor. 
  StringFlav() {}

  // Initialize data members.
  void init();

  // Pick a light d, u or s quark according to fixed ratios.
  int pickLightQ() { double rndmFlav = probQandS * Rndm::flat();
    if (rndmFlav < 1.) return 1; if (rndmFlav < 2.) return 2; return 3; }

  // Pick a new flavour (including diquarks) given an incoming one.
  FlavContainer pick(FlavContainer& flavOld);

  // Combine two flavours (including diquarks) to produce a hadron.
  int combine(FlavContainer& flav1, FlavContainer& flav2);

  // Assign popcorn quark inside an original (= rank 0) diquark.
  void assignPopQ(FlavContainer& flav);

  // Combine two quarks to produce a diquark.
  int makeDiquark(int id1, int id2, int idHad = 0);

private: 

  // Constants: could only be changed in the code itself.
  static const int    mesonMultipletCode[6];
  static const double baryonCGOct[6], baryonCGDec[6]; 

  // Initialization data, to be read from Settings.
  bool   suppressLeadingB;
  double probQQtoQ, probStoUD, probSQtoQQ, probQQ1toQQ0, probQandQQ, 
         probQandS, probQandSinQQ, probQQ1corr, probQQ1corrInv, probQQ1norm, 
         mesonRate[4][6], mesonRateSum[4], mesonMix1[2][6], mesonMix2[2][6], 
         etaSup, etaPrimeSup, decupletSup, baryonCGSum[6], baryonCGMax[6], 
         popcornRate, popcornSpair, popcornSmeson, scbBM[3], popFrac, 
         popS[3], dWT[3][7], lightLeadingBSup, heavyLeadingBSup;
};
 
//**************************************************************************

// The StringZ class is used to sample the fragmentation function f(z).

class StringZ {

public:

  // Constructor. 
  StringZ() {}

  // Initialize data members.
  void init();
  
  // Fragmentation function: top-level to determine parameters.
  double zFrag( int idOld, int idNew = 0, double mT2 = 1.);

private: 

  // Constants: could only be changed in the code itself.
  static const double CFROMUNITY, AFROMZERO, AFROMC, EXPMAX;

  // Initialization data, to be read from Settings.
  bool   usePetersonC, usePetersonB, usePetersonH;
  double mc2, mb2, aLund, bLund, aExtraDiquark, rFactC, rFactB, rFactH, 
         epsilonC, epsilonB, epsilonH;

  // Fragmentation function: select z according to provided parameters.
  double zLund( double a, double b, double c = 1.);
  double zPeterson( double epsilon);

};
 
//**************************************************************************

// The StringPT class is used to select select transverse momenta.

class StringPT {

public:

  // Constructor. 
  StringPT() {}

  // Initialize data members.
  void init();

  // Return px and py separately, but really same routine.
  double px() {return pxy();}
  double py() {return pxy();}

private: 

  // Initialization data, to be read from Settings.
  double sigmaQ, enhancedFraction, enhancedWidth;

  // pT fragmentation spectrum.
  double pxy();

};
 
//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_FragmentationFlavZpT_H
