// SusyResonanceWidths.h is a part of the PYTHIA event generator.
// Copyright (C) 2014 Torbjorn Sjostrand
// Main author of this file: N. Desai
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for resonance properties: dynamical widths etc.
// SusyResonanceWidths: base class for all SUSY resonances.

#ifndef Pythia8_SusyResonanceWidths_H
#define Pythia8_SusyResonanceWidths_H

#include "Pythia8/ResonanceWidths.h"
#include "Pythia8/SusyCouplings.h"

namespace Pythia8 {

class ParticleData;

//==========================================================================

class WidthFunction {

public:

  // Constructor and destructor.
  WidthFunction() { };
  virtual ~WidthFunction() { };

  void init( ParticleData* particleDataPtrIn, CoupSUSY* coupSUSYPtrIn);

  virtual void setInternal(int idResIn, int id1In, int id2In, int id3In,
    int idIntIn, int) {setInternal2(idResIn, id1In, id2In, id3In, idIntIn);}

  virtual double function(double m12);
  virtual double function(double m12, double m23);
  
protected:

  void setInternal2(int idResIn, int id1In, int id2In, int id3In, int idIntIn);

  ParticleData* particleDataPtr;
  CoupSUSY* coupSUSYPtr;
  int id1,id2,id3;

  // Variables for 3-body decays
  double mRes, mInt, gammaInt, m1,m2,m3;
  int idRes, idInt,iSq,iQ,iX;
  bool isSqDown;

};

//==========================================================================

class Psi: public WidthFunction {

public:

  // Destructor.
  virtual ~Psi() { };

  virtual void setInternal(int idResIn, int id1In, int id2In, int id3In,
    int idIntIn, int);
  virtual double function(double m12);

};

//==========================================================================

class Upsilon: public WidthFunction {

public:

  // Destructor.
  virtual ~Upsilon() { };

  virtual void setInternal(int idResIn, int id1In, int id2In, int id3In,
    int idIntIn, int idInt2);
  virtual double function(double m12);

protected:

  int iSq2, idInt2;
  double mInt2, gammaInt2;

};

//==========================================================================

class Phi: public WidthFunction {

public:

  // Destructor.
  virtual ~Phi() { };

  virtual void setInternal(int idResIn, int id1In, int id2In, int id3In,
    int idIntIn, int idInt2);
  virtual double function(double m12sqIn);

protected:

  int iSq2, idInt2;
  double mInt2, gammaInt2, m12sq;

private:

  double function2(double m23sq);
  double integrateGauss(double m23min, double m23max, double tol);

};

//==========================================================================

class SUSYResonanceWidths : public ResonanceWidths{

public:

  SUSYResonanceWidths() {}

  // Return particle type
  int typeNeut(int idPDG);
  int typeChar(int idPDG);

protected:

  // Virtual methods to handle model-specific (non-SM) part of initialization
  virtual bool initBSM();
  virtual bool allowCalc();

  // Gaussian integrator
  double integrateGauss( WidthFunction* widthFn, double, double, double);
  
  // SUSY couplings
  CoupSUSY* coupSUSYPtr;
  
  static const bool DBSUSY;

};

//==========================================================================

// The ResonanceSquark class handles the Squark resonances.

class ResonanceSquark : public SUSYResonanceWidths {

public:

  // Constructor.
  ResonanceSquark(int idResIn) {initBasic(idResIn);}

private:

  // Locally stored properties and couplings.

  // Initialize constants.
  virtual void initConstants();
 
  // Calculate various common prefactors for the current mass.
  virtual void calcPreFac(bool = false);

  // Caclulate width for currently considered channel.
  virtual void calcWidth(bool calledFromInit = false);

  double s2W;

};
  
//==========================================================================

// The ResonanceGluino class handles the Gluino resonances.

class ResonanceGluino : public SUSYResonanceWidths {

public:

  // Constructor.
  ResonanceGluino(int idResIn) {initBasic(idResIn);}

private:

  // Locally stored properties and couplings.
 
  // Initialize constants.
  virtual void initConstants();
 
  // Calculate various common prefactors for the current mass.
  virtual void calcPreFac(bool = false);

  // Caclulate width for currently considered channel.
  virtual void calcWidth(bool calledFromInit = false);
  
};
  
//==========================================================================

// The ResonanceNeut class handles the Neutralino resonances.

class ResonanceNeut : public SUSYResonanceWidths {

public:

  // Constructor.
  ResonanceNeut(int idResIn) {initBasic(idResIn);}

private:

  // Locally stored properties and couplings.
  double kinFac2;

  // Initialize constants.
  virtual void initConstants();
 
  // Calculate various common prefactors for the current mass.
  virtual void calcPreFac(bool = false);

  // Caclulate width for currently considered channel.
  virtual void calcWidth(bool calledFromInit = false);

  double s2W;

  // Functions for 3-body decays
  Psi psi;
  Phi phi;
  Upsilon upsil;

};
  
//==========================================================================

// The ResonanceChar class handles the Chargino resonances.

class ResonanceChar : public SUSYResonanceWidths {

public:

  // Constructor.
  ResonanceChar(int idResIn) {initBasic(idResIn);}

private:

  // Locally stored properties and couplings.
  double kinFac2;

  // Initialize constants.
  virtual void initConstants();
 
  // Calculate various common prefactors for the current mass.
  virtual void calcPreFac(bool = false);

  // Caclulate width for currently considered channel.
  virtual void calcWidth(bool calledFromInit = false);

  double s2W;

  //Functions for 3-body decays
  Psi psi;
  Phi phi;
  Upsilon upsil;

};
  
//==========================================================================

// The ResonanceSlepton class handles the Slepton/Sneutrino resonances.

class ResonanceSlepton : public SUSYResonanceWidths {

public:

  // Constructor.
  ResonanceSlepton(int idResIn) {initBasic(idResIn);}

private:

  // Locally stored properties and couplings.

  // Initialize constants.
  virtual void initConstants();
 
  // Calculate various common prefactors for the current mass.
  virtual void calcPreFac(bool = false);

  // Calculate width for currently considered channel.
  virtual void calcWidth(bool calledFromInit = false);

  double s2W;

};
  
//==========================================================================

} // end namespace Pythia8

#endif // end Pythia8_SusyResonanceWidths_H
