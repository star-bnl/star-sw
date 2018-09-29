// SigmaDM.h is a part of the PYTHIA event generator.
// Copyright (C) 2018 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL v2 or later, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for Dark Matter process differential cross sections.
// Contains classes derived from SigmaProcess.

#ifndef Pythia8_SigmaDM_H
#define Pythia8_SigmaDM_H

#include "Pythia8/PythiaComplex.h"
#include "Pythia8/SigmaProcess.h"

namespace Pythia8 {

//==========================================================================

// A derived class for f fbar' -> Zprime -> X X. (Zprime a.k.a. DMmed(s=1).)

class Sigma1ffbar2Zp2XX : public Sigma1Process {

public:

  // Constructor.
  Sigma1ffbar2Zp2XX() {}

  // Initialize process.
  virtual void initProc();

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat).
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "f fbar -> Zp -> XX";}
  virtual int    code()       const {return 6001;}
  virtual string inFlux()     const {return "qqbar";}
  virtual int    resonanceA() const {return 55;} // Zprime
  virtual bool   isSChannel() const {return true;}
  virtual int    gmZmode()    const {return 3;}



private:

  // Parameters set at initialization.
  double mRes, GammaRes, m2Res, sigma0, preFac;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* particlePtr;

};

//==========================================================================

class Sigma2qqbar2Zpg2XXj : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2Zpg2XXj() {}

  // Initialize process.
  virtual void initProc();

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat).
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "q qbar -> Zp g -> XX + jet";}
  virtual int    code()       const {return 6002;}
  virtual string inFlux()     const {return "qqbar";}
  virtual int    resonanceA() const {return 55;} // Zprime
  virtual int    id3Mass()    const {return 55;}
  virtual int    id4Mass()    const {return 21;}
  virtual bool   isSChannel() const {return true;}
  virtual int    gmZmode()    const {return 3;}

protected:

  // Parameters set at initialization.
  double mRes, GammaRes, m2Res, preFac, sigma0;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* particlePtr;

};

//==========================================================================

class Sigma2qg2Zpq2XXj : public Sigma2qqbar2Zpg2XXj {

  // Constructor.
  Sigma2qg2Zpq2XXj() {}

  // Info on the subprocess.
  virtual string name()       const {return "q g -> Zp q -> XX + jet";}
  virtual int    code()       const {return 6003;}
  virtual string inFlux()     const {return "qg";}
  virtual int    resonanceA() const {return 55;} // Zprime
  virtual bool   isSChannel() const {return true;}
  virtual int    gmZmode()    const {return 3;}

};

//==========================================================================

// A derived class for f fbar' -> Zprime H, Zprime -> X X.

class Sigma2ffbar2ZpH : public Sigma2Process {

public:

  // Constructor.
  Sigma2ffbar2ZpH() {}

  // Initialize process.
  virtual void initProc();

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat).
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "f fbar -> Zprime H";}
  virtual int    code()       const {return 6004;}
  virtual string inFlux()     const {return "ffbarSame";}
  virtual bool   isSChannel() const {return true;}
  virtual int    id3Mass()    const {return 55;}
  virtual int    id4Mass()    const {return 25;}
  virtual int    resonanceA() const {return 55;} // Zprime
  virtual int    gmZmode()    const {return 3;}

  // virtual bool   convertM2()  const {return true;} // Use |M|^2

private:

  // Parameters set at initialization.
  double mRes, GammaRes, m2Res, sigma0, gZp, coupZpH, openFrac;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* particlePtr;

};

//==========================================================================

class Sigma1gg2S2XX : public Sigma1Process {

public:

  // Constructor.
  Sigma1gg2S2XX() {}

  // Initialize process.
  virtual void initProc();

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat).
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "g g -> S -> XX";}
  virtual int    code()       const {return 6011;}
  virtual string inFlux()     const {return "gg";}
  virtual int    resonanceA() const {return 54;} // scalar mediator
  virtual bool   isSChannel() const {return true;}

private:

  // Parameters set at initialization.
  double mRes, GammaRes, m2Res, sigma0;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* particlePtr;

};

//==========================================================================

class Sigma2gg2Sg2XXj : public Sigma2Process {

public:

  // Constructor.
  Sigma2gg2Sg2XXj() {}

  // Initialize process.
  virtual void initProc();

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate sigmaHat(sHat).
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "g g -> S g -> XX + jet";}
  virtual int    code()       const {return 6012;}
  virtual string inFlux()     const {return "gg";}
  virtual int    resonanceA() const {return 54;} // scalar mediator
  virtual bool   isSChannel() const {return true;}
  virtual int    id3Mass()    const {return 54;}
  virtual int    id4Mass()    const {return 21;}

protected:

  // Parameters set at initialization.
  double mRes, GammaRes, m2Res, propS, sigma0;

  // Pointer to properties of the particle species, to access decay channels.
  ParticleDataEntry* particlePtr;

};

//==========================================================================

class Sigma2qg2Sq2XXj : public Sigma2gg2Sg2XXj {

public:

  // Constructor.
  Sigma2qg2Sq2XXj() {}

  // Info on the subprocess.
  virtual string name()       const {return "q g -> S q -> XX + jet";}
  virtual int    code()       const {return 6013;}
  virtual string inFlux()     const {return "qg";}
  virtual bool   isSChannel() const {return true;}

};

//==========================================================================

} // end namespace Pythia8

#endif // Pythia_SigmaDM_H
