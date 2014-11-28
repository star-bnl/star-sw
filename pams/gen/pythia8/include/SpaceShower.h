// SpaceShower.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the spacelike initial-state showers.
// SpaceDipoleEnd: radiating dipole end in ISR.
// SpaceSystem: info on one interaction (among multiple ones).
// SpaceShower: handles the showering description.

#ifndef Pythia8_SpaceShower_H
#define Pythia8_SpaceShower_H

#include "Basics.h"
#include "BeamParticle.h"
#include "Event.h"
#include "Info.h"
#include "ParticleData.h"
#include "PythiaStdlib.h"
#include "Settings.h"
#include "StandardModel.h"

namespace Pythia8 {
 
//**************************************************************************

// Data on radiating dipole ends, only used inside SpaceSystem and SpaceShower.

class SpaceDipoleEnd {
  
public:

  // Constructor.
  SpaceDipoleEnd( int systemIn = 0, int sideIn = 0, int iRadiatorIn = 0, 
    int iRecoilerIn = 0, double pTmaxIn = 0., int colTypeIn = 0, 
    int chgTypeIn = 0,  int MEtypeIn = 0) : system(systemIn), side(sideIn), 
    iRadiator(iRadiatorIn), iRecoiler(iRecoilerIn), pTmax(pTmaxIn), 
    colType(colTypeIn), chgType(chgTypeIn), MEtype(MEtypeIn), nBranch(0) { }
 
  // Store values for trial emission.
  void store( int idDaughterIn, int idMotherIn, int idSisterIn,   
    double x1In, double x2In, double m2DipIn, double pT2In, double zIn, 
    double Q2In, double mSisterIn, double m2SisterIn, double pT2corrIn, 
    double phiIn) {idDaughter = idDaughterIn; idMother = idMotherIn;
    idSister = idSisterIn; x1 = x1In; x2 = x2In; m2Dip = m2DipIn;
    pT2 = pT2In; z = zIn; Q2 = Q2In; mSister = mSisterIn; 
    m2Sister = m2SisterIn; pT2corr = pT2corrIn; phi = phiIn;}
 
  // Basic properties related to evolution and matrix element corrections.
  int    system, side, iRadiator, iRecoiler;
  double pTmax;
  int    colType, chgType, MEtype;
  
  // Properties specific to current trial emission.
  int    idDaughter, idMother, idSister, nBranch;  
  double x1, x2, m2Dip, pT2, z, Q2, mSister, m2Sister, pT2corr, phi,
         pT2Old, zOld;

} ;
 
//**************************************************************************

// The SpaceShower class does spacelike showers.

class SpaceShower {

public:

  // Constructor.
  SpaceShower() {}

  // Destructor.
  virtual ~SpaceShower() {}

  // Initialize pointer to Info for error messages.
  // (Separated from rest of init since not virtual.)
  void initPtr(Info* infoPtrIn) {infoPtr = infoPtrIn;}

  // Initialize generation. Possibility to force re-initialization by hand.
  virtual void init(BeamParticle* beamAPtrIn, BeamParticle* beamBPtrIn);

  // Find whether to limit maximum scale of emissions.
  virtual bool limitPTmax( Event& event, double Q2Fac = 0., 
    double Q2Ren = 0.);

  // Potential enhancement factor of pTmax scale for hardest emission.
  virtual double enhancePTmax() {return pTmaxFudge;}

  // Prepare system for evolution; identify ME.
  virtual void prepare( int iSys, Event& event, bool limitPTmaxIn = true);

  // Update dipole list after each FSR emission. Currently superfluous.
  // Usage: update( iSys, event).  
  virtual void update( int , Event& ) {}

  // Select next pT in downwards evolution.
  virtual double pTnext( Event& event, double pTbegAll, double pTendAll,
    int nRadIn = -1);

  // ME corrections and kinematics that may give failure,
  virtual bool branch( Event& event); 

  // Tell which system was the last processed one.
  int system() const {return iSysSel;} 

  // Print dipole list; for debug mainly.
  virtual void list(ostream& os = cout);

protected:

  // Pointer to various information on the generation.
  Info* infoPtr;

  // Pointers to the two incoming beams.
  BeamParticle* beamAPtr;
  BeamParticle* beamBPtr;

  // Store index of last processed system.
  int iSysSel;

private: 

  // Constants: could only be changed in the code itself.
  static const int    MAXLOOPTINYPDF;
  static const double CTHRESHOLD, BTHRESHOLD, EVALPDFSTEP, TINYPDF, 
         TINYKERNELPDF, TINYPT2, HEAVYPT2EVOL, HEAVYXEVOL, EXTRASPACEQ, 
         LAMBDA3MARGIN, LEPTONXMIN, LEPTONXMAX, LEPTONPT2MIN, LEPTONFUDGE;

  // Initialization data, normally only set once.
  bool   doQCDshower, doQEDshowerByQ, doQEDshowerByL, useSamePTasMI,
         doMEcorrections, doPhiPolAsym, doRapidityOrder;
  int    pTmaxMatch, pTdampMatch, alphaSorder, alphaEMorder, nQuarkIn;
  double pTmaxFudge, pTdampFudge, mc, mb, m2c, m2b, alphaSvalue, alphaS2pi, 
         Lambda3flav, Lambda4flav, Lambda5flav, Lambda3flav2, Lambda4flav2, 
         Lambda5flav2, pT0Ref, ecmRef, ecmPow, pTmin, sCM, eCM, pT0, 
         pTminChgQ, pTminChgL, pT20, pT2min, pT2minChgQ, pT2minChgL; 

  // alphaStrong and alphaEM calculations.
  AlphaStrong alphaS;
  AlphaEM alphaEM;

  // Some current values.
  bool   dopTdamp;
  int    iNow, iRec, idDaughter, nRad;
  double xDaughter, x1Now, x2Now, m2Dip, pT2damp;

  // All dipole ends
  vector<SpaceDipoleEnd> dipEnd;

  // Pointers to the current and hardest (so far) dipole ends.
  int iDipNow, iSysNow;
  SpaceDipoleEnd* dipEndNow; 
  int iDipSel;
  SpaceDipoleEnd* dipEndSel; 
 
  // Evolve a QCD dipole end. 
  void pT2nextQCD( double pT2begDip, double pT2endDip);

  // Evolve a QCD dipole end near heavy quark threshold region. 
  void pT2nearQCDthreshold( BeamParticle& beam, double m2Massive, 
    double m2Threshold, double zMinAbs, double zMaxMassive);

  // Evolve a QED dipole end. 
  void pT2nextQED( double pT2begDip, double pT2endDip);

  // Find class of ME correction.
  int findMEtype( int iSys, Event& event);

  // Provide maximum of expected ME weight; for preweighting of evolution.
  double calcMEmax( int MEtype, int idMother, int idDaughterIn);

  // Provide actual ME weight for current branching.
  double calcMEcorr(int MEtype, int idMother, int idDaughterIn, double M2, 
    double z, double Q2); 

  // Find coefficient of azimuthal asymmetry from gluon polarization.
  // void findAsymPol(DipoleEnd*);

};
 
//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_SpaceShower_H
