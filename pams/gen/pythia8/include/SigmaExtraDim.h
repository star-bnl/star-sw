// SigmaExtraDim.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for extra-dimensional-process differential cross sections.
// Contains classes derived from SigmaProcess via Sigma(1/2)Process.

#ifndef Pythia8_SigmaExtraDim_H
#define Pythia8_SigmaExtraDim_H

#include "SigmaProcess.h"

namespace Pythia8 {
 
//**************************************************************************

// A derived class for g g -> G^* (excited graviton state).

class Sigma1gg2GravitonStar : public Sigma1Process {

public:

  // Constructor.
  Sigma1gg2GravitonStar() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for G* decay angle.
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()       const {return "g g -> G*";}
  virtual int    code()       const {return 5001;}
  virtual string inFlux()     const {return "gg";}
  virtual int    resonanceA() const {return idGstar;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idGstar;
  double mRes, GammaRes, m2Res, GamMRat, kappaMG, sigma;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* gStarPtr;

};
 
//**************************************************************************

// A derived class for f fbar -> G^* (excited graviton state).

class Sigma1ffbar2GravitonStar : public Sigma1Process {

public:

  // Constructor.
  Sigma1ffbar2GravitonStar() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return (abs(id1) < 9) ? sigma0 / 3. : sigma0;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for G* decay angle.
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()       const {return "f fbar -> G*";}
  virtual int    code()       const {return 5002;}
  virtual string inFlux()     const {return "ffbarSame";}
  virtual int    resonanceA() const {return idGstar;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idGstar;
  double mRes, GammaRes, m2Res, GamMRat, kappaMG, sigma0;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* gStarPtr;

};
 
//**************************************************************************

// A derived class for g g -> G^* g (excited graviton state).

class Sigma2gg2GravitonStarg : public Sigma2Process {

public:

  // Constructor.
  Sigma2gg2GravitonStarg() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight: currently isotropic (except secondary top decay)..
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return "g g -> G* g";}
  virtual int    code()    const {return 5003;}
  virtual string inFlux()  const {return "gg";}
  virtual int    id3Mass() const {return idGstar;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idGstar;
  double mRes, GammaRes, m2Res, GamMRat, kappaMG, openFrac, sigma;

};
 
//**************************************************************************

// A derived class for q g -> G^* q (excited graviton state).

class Sigma2qg2GravitonStarq : public Sigma2Process {

public:

  // Constructor.
  Sigma2qg2GravitonStarq() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight: currently isotropic (except secondary top decay)..
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return "q g -> G* q";}
  virtual int    code()    const {return 5004;}
  virtual string inFlux()  const {return "qg";}
  virtual int    id3Mass() const {return idGstar;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idGstar;
  double mRes, GammaRes, m2Res, GamMRat, kappaMG, openFrac, sigma;

};
 
//**************************************************************************

// A derived class for q qbar -> G^* g (excited graviton state).

class Sigma2qqbar2GravitonStarg : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2GravitonStarg() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight: currently isotropic (except secondary top decay)..
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return "q qbar -> G* g";}
  virtual int    code()    const {return 5005;}
  virtual string inFlux()  const {return "qqbarSame";}
  virtual int    id3Mass() const {return idGstar;}

private:

  // Parameters set at initialization or for current kinematics. 
  int    idGstar;
  double mRes, GammaRes, m2Res, GamMRat, kappaMG, openFrac, sigma;

};
 
//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_SigmaExtraDim_H
