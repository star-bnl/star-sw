// SigmaSUSY.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for Supersymmetric process differential cross sections.
// Contains classes derived from SigmaProcess via Sigma2Process.

#ifndef Pythia8_SigmaSUSY_H
#define Pythia8_SigmaSUSY_H

#include "PythiaComplex.h"
#include "SigmaProcess.h"

namespace Pythia8 {
 
//**************************************************************************

// A derived class for q qbar -> gaugino_i gaugino_j.

class Sigma2qqbar2gauginogaugino : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2gauginogaugino() { }

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()    const {return nameSave;}
  virtual int    code()    const {return codeSave;}
  virtual string inFlux()  const {return "qq";}
  virtual int    id3Mass() const {return id3;}
  virtual int    id4Mass() const {return id4;}

 protected:  

  // Values stored for later use.  
  int     id3chi, id4chi, codeSave;
  string  nameSave;
  double  sigma0, ui, uj, ti, tj, sz, d;
  complex propZ;

  // Couplings.
  // Shorthand for sin2thetaW, mZ, and GammaZ.
  double  sin2W, mZpole, wZpole;      
  // qqZ couplings.
  double  LqqZ[10], RqqZ[10]; 
  // qsqchi_i couplings.
  complex LsqXi[10][10], RsqXi[10][10];
  complex LsqCi[10][10], RsqCi[10][10];
  // qsqchi_j couplings.
  complex LsqXj[10][10], RsqXj[10][10];
  complex LsqCj[10][10], RsqCj[10][10];
  // W/Z chi chi couplings
  complex OL, OR, OLp, ORp, OLpp, ORpp;

  // Code to say whether it is chi0chi0, chi+chi0, or chi+chi+
  int nCharged;

};

class Sigma2qqbar2chi0chi0 : public Sigma2qqbar2gauginogaugino {

public:

  // Constructor.
  Sigma2qqbar2chi0chi0(int id3chiIn, int id4chiIn, int codeIn) { 

    // Save ordering indices and process code
    id3chi=id3chiIn; 
    id4chi=id4chiIn; 
    codeSave=codeIn; 

    // Construct id codes from ordering indices.
    id3                  = 1000022; 
    if (id3chi == 2) id3 = 1000023; 
    if (id3chi == 3) id3 = 1000025; 
    if (id3chi == 4) id3 = 1000035; 
    if (id3chi == 5) id3 = 1000045; 
    id4                  = 1000022; 
    if (id4chi == 2) id4 = 1000023; 
    if (id4chi == 3) id4 = 1000025; 
    if (id4chi == 4) id4 = 1000035; 
    if (id4chi == 5) id4 = 1000045; 

  }

};
  
// A derived class for q qbar -> neutralino_i chargino_j.

class Sigma2qqbar2chi0char : public Sigma2qqbar2gauginogaugino {

public:

  // Constructor.
  Sigma2qqbar2chi0char(int id3chiIn, int id4chiIn, int codeIn) {
    
    // Save ordering indices and process code
    id3chi=id3chiIn; 
    id4chi=id4chiIn; 
    codeSave=codeIn; 

    // Construct id codes from ordering indices.
    id3                  = 1000022; 
    if (id3chi == 2) id3 = 1000023; 
    if (id3chi == 3) id3 = 1000025; 
    if (id3chi == 4) id3 = 1000035; 
    if (id3chi == 5) id3 = 1000045; 
    id4                  = 1000024; 
    if (id4chi == 2) id4 = 1000037; 

  }

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat();

};

// A derived class for q qbar -> chargino_i chargino_j.

class Sigma2qqbar2charchar : public Sigma2qqbar2gauginogaugino {

public:

  // Constructor.
  Sigma2qqbar2charchar(int id3chiIn, int id4chiIn, int codeIn) {

    // Save ordering indices and process code
    id3chi=id3chiIn; 
    id4chi=id4chiIn; 
    codeSave=codeIn; 

    // Construct id codes from ordering indices.
    id3                  = -1000024; 
    if (id3chi == 2) id3 = -1000037; 
    id4                  = 1000024; 
    if (id4chi == 2) id4 = 1000037; 

  }

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat();

};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_SigmaSUSY_H

