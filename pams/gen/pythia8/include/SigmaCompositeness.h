// SigmaCompositeness.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for compositiness-process differential cross sections.
// Contains classes derived from SigmaProcess via Sigma(1/2)Process.

#ifndef Pythia8_SigmaCompositeness_H
#define Pythia8_SigmaCompositeness_H

#include "SigmaProcess.h"

namespace Pythia8 {
 
//**************************************************************************

// A derived class for q g -> q^* (excited quark state).

class Sigma1qg2qStar : public Sigma1Process {

public:

  // Constructor.
  Sigma1qg2qStar(int idqIn) : idq(idqIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for q* decay angles (else inactive).
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()       const {return nameSave;}
  virtual int    code()       const {return codeSave;}
  virtual string inFlux()     const {return "qg";}
  virtual int    resonanceA() const {return idRes;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idq, idRes, codeSave;
  string nameSave;
  double mRes, GammaRes, m2Res, GamMRat, Lambda, coupFcol, widthIn, sigBW;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* qStarPtr;

};
 
//**************************************************************************

// A derived class for l gamma -> q^* (excited lepton state).

class Sigma1lgm2lStar : public Sigma1Process {

public:

  // Constructor.
  Sigma1lgm2lStar(int idlIn) : idl(idlIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for l* decay angles (else inactive).
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()       const {return nameSave;}
  virtual int    code()       const {return codeSave;}
  virtual string inFlux()     const {return "fgm";}
  virtual int    resonanceA() const {return idRes;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idl, idRes, codeSave;
  string nameSave;
  double mRes, GammaRes, m2Res, GamMRat, Lambda, coupChg, widthIn, sigBW;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* qStarPtr;

};
 
//**************************************************************************

// A derived class for q q' -> q^* q' (excited quark state).

class Sigma2qq2qStarq : public Sigma2Process {

public:

  // Constructor.
  Sigma2qq2qStarq(int idqIn) : idq(idqIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return nameSave;}
  virtual int    code()       const {return codeSave;}
  virtual string inFlux()     const {return "qq";}
  virtual int    id3Mass()    const {return idRes;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idq, idRes, codeSave;
  string nameSave;
  double Lambda, preFac, openFracPos, openFracNeg, sigmaA, sigmaB;

};
 
//**************************************************************************

// A derived class for q qbar -> l^* lbar (excited lepton state).

class Sigma2qqbar2lStarlbar : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2lStarlbar(int idlIn) : idl(idlIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return nameSave;}
  virtual int    code()       const {return codeSave;}
  virtual string inFlux()     const {return "qqbarSame";}
  virtual int    id3Mass()    const {return idRes;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idl, idRes, codeSave;
  string nameSave;
  double Lambda, preFac, openFracPos, openFracNeg, sigma;

};
 
//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_SigmaCompositeness_H
