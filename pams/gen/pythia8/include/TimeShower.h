// TimeShower.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the timelike final-state showers.
// TimeDipoleEnd: data on a radiating dipole end.
// TimeShower: handles the showering description.

#ifndef Pythia8_TimeShower_H
#define Pythia8_TimeShower_H

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

// Data on radiating dipole ends; only used inside TimeShower class.

class TimeDipoleEnd {

public:

  // Constructors.
  TimeDipoleEnd() : iRadiator(-1), iRecoiler(-1), pTmax(0.), colType(0), 
    chgType(0), gamType(0), isrType(0), system(0), MEtype(0), 
    iMEpartner(-1), isOctetOnium(false), MEmix(0.), MEorder(true), 
    MEsplit(true), MEgluinoRec(false) { }  
  TimeDipoleEnd(int iRadiatorIn, int iRecoilerIn, double pTmaxIn = 0., 
    int colIn = 0, int chgIn = 0, int gamIn = 0, int isrIn = 0, 
    int systemIn = 0, int MEtypeIn = 0, int iMEpartnerIn = -1, 
    bool isOctetOniumIn = false, double MEmixIn = 0., bool MEorderIn = true, 
    bool MEsplitIn = true, bool MEgluinoRecIn = false) : 
    iRadiator(iRadiatorIn), iRecoiler(iRecoilerIn), pTmax(pTmaxIn), 
    colType(colIn), chgType(chgIn), gamType(gamIn), isrType(isrIn), 
    system(systemIn), MEtype(MEtypeIn), iMEpartner(iMEpartnerIn), 
    isOctetOnium(isOctetOniumIn), MEmix(MEmixIn), MEorder (MEorderIn), 
    MEsplit(MEsplitIn), MEgluinoRec(MEgluinoRecIn) { }

  // Basic properties related to dipole and matrix element corrections.
  int    iRadiator, iRecoiler;
  double pTmax;
  int    colType, chgType, gamType, isrType, system, MEtype, iMEpartner;
  bool   isOctetOnium;
  double MEmix;
  bool   MEorder, MEsplit, MEgluinoRec;

  // Properties specific to current trial emission.
  int    flavour, iAunt;
  double mRad, m2Rad, mRec, m2Rec, mDip, m2Dip, m2DipCorr, 
         pT2, m2, z, mFlavour, asymPol;   
  
} ;

//**************************************************************************

// The TimeShower class does timelike showers.

class TimeShower {

public:

  // Constructor.
  TimeShower() {}

  // Destructor.
  virtual ~TimeShower() {}

  // Initialize pointer to Info for error messages. 
  // (Separated from rest of init since not virtual.)
  void initPtr(Info* infoPtrIn) {infoPtr = infoPtrIn;}

  // Initialize alphaStrong and related pTmin parameters.
  virtual void init( BeamParticle* beamAPtrIn = 0, 
    BeamParticle* beamBPtrIn = 0);

  // Potential enhancement factor of pTmax scale for hardest emission.
  virtual double enhancePTmax() {return pTmaxFudge;}

  // Top-level routine to do a full time-like shower in resonance decay.
  virtual int shower( int iBeg, int iEnd, Event& event, double pTmax);

  // Prepare system for evolution after each new interaction; identify ME.
  virtual void prepare( int iSys, Event& event);

  // Update dipole list after each ISR emission.  
  virtual void update( int iSys, Event& event);

  // Select next pT in downwards evolution.
  virtual double pTnext( Event& event, double pTbegAll, double pTendAll);

  // ME corrections and kinematics that may give failure,
  virtual bool branch( Event& event); 

  // Tell which system was the last processed one.
  int system() const {return iSysSel;}; 

  // Print dipole list; for debug mainly.
  virtual void list( ostream& os = cout);

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
  static const double SIMPLIFYROOT, XMARGIN, XMARGINCOMB, TINYPDF, LARGEM2, 
         THRESHM2, LAMBDA3MARGIN;

  // Initialization data, normally only set once.
  bool   doQCDshower, doQEDshowerByQ, doQEDshowerByL, doQEDshowerByGamma, 
         doMEcorrections, doPhiPolAsym, allowBeamRecoil;
  int    alphaSorder, nGluonToQuark, alphaEMorder, nGammaToQuark, 
         nGammaToLepton;
  double pTmaxFudge, mc, mb, m2c, m2b, alphaSvalue, alphaS2pi, 
         Lambda3flav, Lambda4flav, Lambda5flav, Lambda3flav2, Lambda4flav2, 
         Lambda5flav2, pTcolCutMin, pTcolCut, pT2colCut, pTchgQCut, 
         pT2chgQCut, pTchgLCut, pT2chgLCut, mMaxGamma, m2MaxGamma, 
         octetOniumFraction, octetOniumColFac, mZ, gammaZ, thetaWRat;

  // alphaStrong and alphaEM calculations.
  AlphaStrong alphaS;
  AlphaEM     alphaEM;

  // All dipole ends and a pointer to the selected hardest dipole end.
  vector<TimeDipoleEnd> dipEnd;
  TimeDipoleEnd* dipSel;

  // Setup a dipole end, either QCD or QED/photon one.
  void setupQCDdip( int iSys, int i, int colTag,  int colSign, Event& event,
    bool isOctetOnium = false);
  void setupQEDdip( int iSys, int i, int chgType, int gamType, Event& event); 

  // Evolve a QCD dipole end. 
  void pT2nextQCD( double pT2begDip, double pT2sel, TimeDipoleEnd& dip,
    Event& event);

  // Evolve a QED dipole end (except photon). 
  void pT2nextQED( double pT2begDip, double pT2sel, TimeDipoleEnd& dip,
    Event& event);

  // Find kind of QCD ME correction.
  void findMEtype( Event& event, TimeDipoleEnd& dip);

  // Find type of particle; used by findMEtype.
  int findMEparticle( int id);

  // Find mixture of V and A in gamma/Z: energy- and flavour-dependent. 
  double gammaZmix( Event& event, int iRes, int iDau1, int iDau2);

  // Set up to calculate QCD ME correction with calcMEcorr.
  double findMEcorr(TimeDipoleEnd* dip, Particle& rad, Particle& partner, 
   Particle& emt);

  // Calculate value of QCD ME correction.
  double calcMEcorr( int kind, int combiIn, double mixIn, double x1, 
    double x2, double r1, double r2);

  // Find coefficient of azimuthal asymmetry from gluon polarization.
  void findAsymPol( Event& event, TimeDipoleEnd* dip);

};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_TimeShower_H
