// SigmaQCD.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for QCD process differential cross sections.
// Contains classes derived from SigmaProcess via Sigma(0/2)Process.

#ifndef Pythia8_SigmaQCD_H
#define Pythia8_SigmaQCD_H

#include "SigmaProcess.h"

namespace Pythia8 {
 
//**************************************************************************

// A derived class for minimum-bias (inelastic, nondiffractive) events.

class Sigma0minBias : public Sigma0Process {

public:

  // Constructor.
  Sigma0minBias() {}

  // Evaluate sigma. 
  virtual double sigmaHat() {return sigmaTotPtr->sigmaND();} 

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol() {}

  // Info on the subprocess.
  virtual string name()      const {return "minimum bias";}
  virtual int    code()      const {return 101;}
  virtual bool   isMinBias() const {return true;}

private:

};
 
//**************************************************************************

// A derived class for elastic scattering A B -> A B.

class Sigma0AB2AB : public Sigma0Process {

public:

  // Constructor.
  Sigma0AB2AB() {}

  // Evaluate sigma. 
  virtual double sigmaHat() {return sigmaTotPtr->sigmaEl();} 

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "A B -> A B elastic";}
  virtual int    code()       const {return 102;}
  virtual bool   isResolved() const {return false;}

private:

};
 
//**************************************************************************

// A derived class for single diffractive scattering A B -> X B.

class Sigma0AB2XB : public Sigma0Process {

public:

  // Constructor.
  Sigma0AB2XB() {}

  // Evaluate sigma. 
  virtual double sigmaHat() {return sigmaTotPtr->sigmaXB();} 

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "A B -> X B single diffractive";}
  virtual int    code()       const {return 103;}
  virtual bool   isResolved() const {return false;}
  virtual bool   isDiffA()    const {return true;};

private:

};
 
//**************************************************************************

// A derived class for single diffractive scattering A B -> A X.

class Sigma0AB2AX : public Sigma0Process {

public:

  // Constructor.
  Sigma0AB2AX() {}

  // Evaluate sigma. 
  virtual double sigmaHat() {return sigmaTotPtr->sigmaAX();} 

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "A B -> A X single diffractive";}
  virtual int    code()       const {return 104;}
  virtual bool   isResolved() const {return false;}
  virtual bool   isDiffB()    const {return true;};

private:

};
 
//**************************************************************************

// A derived class for double diffractive scattering A B -> X X.

class Sigma0AB2XX : public Sigma0Process {

public:

  // Constructor.
  Sigma0AB2XX() {}

  // Evaluate sigma. 
  virtual double sigmaHat() {return sigmaTotPtr->sigmaXX();} 

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()       const {return "A B -> X X double diffractive";}
  virtual int    code()       const {return 105;}
  virtual bool   isResolved() const {return false;}
  virtual bool   isDiffA()    const {return true;};
  virtual bool   isDiffB()    const {return true;};

private:

};

//**************************************************************************

// A derived class for g g -> g g.

class Sigma2gg2gg : public Sigma2Process {

public:

  // Constructor.
  Sigma2gg2gg() {}

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "g g -> g g";}
  virtual int    code()   const {return 111;}
  virtual string inFlux() const {return "gg";}

private:

  // Values stored for colour flow selection.
  double sigTS, sigUS, sigTU, sigSum, sigma;

};

//**************************************************************************

// A derived class for g g -> q qbar (q = u, d, s, i.e. almost massless).

class Sigma2gg2qqbar : public Sigma2Process {

public:

  // Constructor.
  Sigma2gg2qqbar() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "g g -> q qbar (uds)";}
  virtual int    code()   const {return 112;}
  virtual string inFlux() const {return "gg";}

private:

  // Number of quarks to be considered in massless approximation.
  int    nQuarkNew;

  // Values stored for colour flow selection.
  int    idNew;
  double mNew, m2New, sigTS, sigUS, sigSum, sigma;

};
 
//**************************************************************************

// A derived class for q g -> q g (q = u, d, s, c, b).
// Use massless approximation also for Q since no alternative.

class Sigma2qg2qg : public Sigma2Process {

public:

  // Constructor.
  Sigma2qg2qg() {}

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "q g -> q g";}
  virtual int    code()   const {return 113;}
  virtual string inFlux() const {return "qg";}

private:

  // Values stored for colour flow selection.
  double mNew, m2New, sigTS, sigTU, sigSum, sigma;

};
 
//**************************************************************************

// A derived class for q qbar' -> q qbar' or q q' -> q q' 
// (qbar qbar' -> qbar qbar'), q' may be same as q.

class Sigma2qq2qq : public Sigma2Process {

public:

  // Constructor.
  Sigma2qq2qq() {}

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat();

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "q q(bar)' -> q q(bar)'";}
  virtual int    code()   const {return 114;}
  virtual string inFlux() const {return "qq";}

 private:

  // Values stored for colour flow selection.
  double sigT, sigU, sigTU, sigST, sigSum;

};
 
//**************************************************************************

// A derived class for q qbar -> g g.

class Sigma2qqbar2gg : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2gg() {}

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "q qbar -> g g";}
  virtual int    code()   const {return 115;}
  virtual string inFlux() const {return "qqbarSame";}

 private:

  // Values stored for colour flow selection.
  double sigTS, sigUS, sigSum, sigma;

};
  
//**************************************************************************

// A derived class for q qbar -> q' qbar'.

class Sigma2qqbar2qqbarNew : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2qqbarNew() {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Info on the subprocess.
  virtual string name()   const {return "q qbar -> q' qbar' (uds)";}
  virtual int    code()   const {return 116;}
  virtual string inFlux() const {return "qqbarSame";}

 private:

  // Number of quarks to be considered in massless approximation.
  int    nQuarkNew;

  // Values stored for colour flow selection.
  int    idNew;
  double mNew, m2New, sigS, sigma;

};
 
//**************************************************************************

// A derived class for g g -> Q Qbar (Q = c, b or t).

class Sigma2gg2QQbar : public Sigma2Process {

public:

  // Constructor.
  Sigma2gg2QQbar(int idIn, int codeIn) : idNew(idIn), codeSave(codeIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for W decay angles in top decay (else inactive).
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return nameSave;}
  virtual int    code()    const {return codeSave;}
  virtual string inFlux()  const {return "gg";}
  virtual int    id3Mass() const {return idNew;}
  virtual int    id4Mass() const {return idNew;}

 private:

  // Values stored for process type and colour flow selection.
  int    idNew, codeSave;
  string nameSave;
  double sigTS, sigUS, sigSum, sigma, openFracPair;

};
 
//**************************************************************************

// A derived class for q qbar -> Q Qbar (Q = c, b or t).

class Sigma2qqbar2QQbar : public Sigma2Process {

public:

  // Constructor.
  Sigma2qqbar2QQbar(int idIn, int codeIn) : idNew(idIn), codeSave(codeIn) {}

  // Initialize process. 
  virtual void initProc(); 

  // Calculate flavour-independent parts of cross section.
  virtual void sigmaKin();

  // Evaluate d(sigmaHat)/d(tHat). 
  virtual double sigmaHat() {return sigma;}

  // Select flavour, colour and anticolour.
  virtual void setIdColAcol();

  // Evaluate weight for W decay angles in top decay (else inactive).
  virtual double weightDecay( Event& process, int iResBeg, int iResEnd); 

  // Info on the subprocess.
  virtual string name()    const {return nameSave;}
  virtual int    code()    const {return codeSave;}
  virtual string inFlux()  const {return "qqbarSame";}
  virtual int    id3Mass() const {return idNew;}
  virtual int    id4Mass() const {return idNew;}

 private:

  // Values stored for process type.
  int    idNew, codeSave;
  string nameSave;
  double sigma, openFracPair;

};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_SigmaQCD_H
