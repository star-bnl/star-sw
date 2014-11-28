// StandardModel.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file gives access to some Standard Model parameters.
// AlphaStrong: fix or first- or second-order running alpha_strong.

#ifndef Pythia8_StandardModel_H
#define Pythia8_StandardModel_H

#include "ParticleData.h"
#include "PythiaStdlib.h"

namespace Pythia8 {

//**************************************************************************

// The AlphaStrong class calculates the alpha_strong value at an arbitrary 
// scale, given the value at m_Z, to zeroth, first or second order.

class AlphaStrong {

public:

  // Constructors.
  AlphaStrong() : isInit(false) {}
  AlphaStrong(double valueIn, int orderIn = 1) { 
    init( valueIn, orderIn) ;}

  // Initialization for given value at M_Z and given order.
  void init(double valueIn = 0.12, int orderIn = 1);

  // alpha_S value and Lambda values.
  double alphaS(double scale2);
  double alphaS1Ord(double scale2);
  double alphaS2OrdCorr(double scale2);
  double Lambda3() const { return Lambda3Save; }
  double Lambda4() const { return Lambda4Save; }
  double Lambda5() const { return Lambda5Save; }

private:

  // Constants: could only be changed in the code itself.
  static const int    NITER;
  static const double SAFETYMARGIN1, SAFETYMARGIN2;

  // Data members.
  bool   isInit, lastCallToFull;
  int    order;
  double valueRef, valueNow, scale2Now, scale2Min, Lambda3Save, 
         Lambda4Save, Lambda5Save, Lambda3Save2, Lambda4Save2, 
         Lambda5Save2, mc, mb, mZ, mc2, mb2;

};

//**************************************************************************

// The AlphaEM class calculates the alpha_electromagnetic value at an 
// arbitrary scale, given the value at 0 and m_Z, to zeroth or first order.

class AlphaEM {

public:

  // Constructors.
  AlphaEM(int orderIn = 1) {init(orderIn);}

  // First-order initialization for given value at M_Z.
  static void initStatic();

  // Initialization for a given order.
  void init(int orderIn = 1) {order = orderIn;}

  // alpha_EM value.
  double alphaEM(double scale2);

private:

  // Static data members, mostly for first-order matching.
  static double alpEM0, alpEMmZ, mZ2, Q2step[5], bRun[5], alpEMstep[5];

  // Data members.
  int order;

};

//**************************************************************************

// The CoupEW class stores and returns electroweak couplings.

class CoupEW {

public:

   // Constructor.
   CoupEW() {}

  // Initialize, normally from Pythia::init().
  static void initStatic();

   // Return electroweak mixing angle.
   static double sin2thetaW() {return s2tW;}
   static double cos2thetaW() {return c2tW;}
   static double sin2thetaWbar() {return s2tWbar;}

   // Return electroweak couplings of quarks and leptons.
   static double ef(int idAbs) {return efSave[idAbs];}
   static double vf(int idAbs) {return vfSave[idAbs];}
   static double af(int idAbs) {return afSave[idAbs];}
   static double t3f(int idAbs) {return 0.5*afSave[idAbs];}
   static double lf(int idAbs) {return lfSave[idAbs];}
   static double rf(int idAbs) {return rfSave[idAbs];}
  
   // Return some squared couplings and other combinations.
   static double ef2(int idAbs) {return ef2Save[idAbs];}
   static double vf2(int idAbs) {return vf2Save[idAbs];}
   static double af2(int idAbs) {return af2Save[idAbs];}
   static double efvf(int idAbs) {return efvfSave[idAbs];}
   static double vf2af2(int idAbs) {return vf2af2Save[idAbs];}

private:

   // Store couplings.
   static double s2tW, c2tW, s2tWbar, efSave[20], vfSave[20], afSave[20],
                 lfSave[20], rfSave[20], ef2Save[20], vf2Save[20], 
                 af2Save[20], efvfSave[20], vf2af2Save[20];

};

//**************************************************************************

// The VCKM class stores and returns Cabibbo-Kobayashi-Maskawa 

class VCKM {

public:

  // Constructor.
  VCKM() {}

  // Initialize, normally from Pythia::init().
  static void initStatic();

  // Return value or square: first index 1/2/3/4 = u/c/t/t', 
  // second 1/2/3/4 = d/s/b/b'.
  static double Vgen(int genU, int genD) {return Vsave[genU][genD];}
  static double V2gen(int genU, int genD) {return V2save[genU][genD];}

  // Return value or square for incoming flavours (sign irrelevant).
  static double Vid(int id1, int id2);
  static double V2id(int id1, int id2);

  // Return sum of squares for given inflavour, or random outflavour.
  static double V2sum(int id) {return V2out[abs(id)];}
  static int    V2pick(int id);
  
private:

  // Store VCKM matrix (index 0 not used) and sum of squares.
  static double Vsave[5][5], V2save[5][5], V2out[20];

};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_StandardModel_H
