// HelicityMatrixElements.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Philip Ilten, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for a number of physics classes used in tau decays.

#ifndef Pythia8_HelicityMatrixElements_H
#define Pythia8_HelicityMatrixElements_H

#include "Basics.h"
#include "Event.h"
#include "HelicityBasics.h"
#include "PythiaComplex.h"
#include "PythiaStdlib.h"
#include "StandardModel.h"

namespace Pythia8 {

//==========================================================================

// The helicity matrix element class.

class HelicityMatrixElement {

public:

  // Constructor and destructor.
  HelicityMatrixElement() {};
  virtual ~HelicityMatrixElement() {};

  // Initialize the physics matrices and pointers.
  virtual void initPointers(ParticleData*, Couplings*);

  // Initialize the channel.
  virtual HelicityMatrixElement* initChannel(vector<HelicityParticle>&);

  // Calculate the matrix element weight for a decay.
  virtual double decayWeight(vector<HelicityParticle>&);

  // Calculate the maximum matrix element decay weight.
  virtual double decayWeightMax(vector<HelicityParticle>&)
    {return DECAYWEIGHTMAX;}

  // Calculate the helicity matrix element.
  virtual complex calculateME(vector<int>){return complex(0,0);}

  // Calculate the decay matrix for a particle.
  virtual void calculateD(vector<HelicityParticle>&);

  // Calculate the density matrix for a particle.
  virtual void calculateRho(unsigned int, vector<HelicityParticle>&);

  // Set a fermion line.
  void setFermionLine(int, HelicityParticle&, HelicityParticle&);

  // Calculate Breit-Wigner's with running widths.
  virtual complex sBreitWigner(double m0, double m1, double s,
    double M, double G);
  virtual complex pBreitWigner(double m0, double m1, double s,
    double M, double G);
  virtual complex dBreitWigner(double m0, double m1, double s,
    double M, double G);

protected:

  // Maximum decay weight. WARNING: hardcoded constant.
  double DECAYWEIGHTMAX;

  // Physics matrices.
  vector< GammaMatrix > gamma;

  // Particle map vector.
  vector< int > pMap;

  // Particle ID vector.
  vector< int > pID;

  // Particle mass vector.
  vector< double > pM;
  
  // Wave functions.
  vector< vector< Wave4 > > u;

  // Initialize the constants for the matrix element (called by initChannel).
  virtual void initConstants() {};

  // Initialize the wave functions (called by decayWeight and calculateRho/D).
  virtual void initWaves(vector<HelicityParticle>&) {};

  // Pointer to particle data.
  ParticleData* particleDataPtr;

  // Pointer to Standard Model constants.
  Couplings*    couplingsPtr;

private:
    
  // Recursive sub-method to calculate the density matrix for a particle.
  void calculateRho(unsigned int, vector<HelicityParticle>&,
    vector<int>&, vector<int>&, unsigned int);

  // Recursive sub-method to calculate the decay matrix for a particle.
  void calculateD(vector<HelicityParticle>&, vector<int>&, vector<int>&,
    unsigned int);

  // Recursive sub-method to calculate the matrix element weight for a decay.
  void decayWeight(vector<HelicityParticle>&, vector<int>&, vector<int>&,
    complex&, unsigned int);

  // Calculate the product of the decay matrices for a hard process.
  complex calculateProductD(unsigned int, unsigned int,
    vector<HelicityParticle>&, vector<int>&, vector<int>&);

  // Calculate the product of the decay matrices for a decay process.
  complex calculateProductD(vector<HelicityParticle>&,
    vector<int>&, vector<int>&);

};

//==========================================================================

// Helicity matrix element for the hard process of two fermions -> W ->
// two fermions.
  
class HMETwoFermions2W2TwoFermions : public HelicityMatrixElement {

public:

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

};

//==========================================================================

// Helicity matrix element for the hard process of two fermions ->
// photon -> two fermions.

class HMETwoFermions2Gamma2TwoFermions : public HelicityMatrixElement {

public:

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:

  // Fermion line charge and interaction energy.
  double p0Q, p2Q, s;

};

//==========================================================================

// Helicity matrix element for the hard process of two fermions ->
// Z -> two fermions.
  
class HMETwoFermions2Z2TwoFermions : public HelicityMatrixElement {
    
public:

  void initConstants();

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:

  // Vector and axial couplings.
  double p0CA, p2CA, p0CV, p2CV;

  // Weinberg angle, Z width (on-shell), Z mass (on-shell), and s.
  double cos2W, sin2W, zG, zM, s;

  // Bool whether the incoming fermions are oriented with the z-axis.
  bool zaxis;

};

//==========================================================================

// Helicity matrix element for the hard process of two fermions ->
// photon/Z -> two fermions with full interference.

// In general the initPointers and initChannel methods should not need to be
// redeclared, but in this case each matrix element needs to be initialized.
  
class HMETwoFermions2GammaZ2TwoFermions : public HelicityMatrixElement {

public:

  HelicityMatrixElement* initChannel(vector<HelicityParticle>&);

  void initPointers(ParticleData*, Couplings*);
  
  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:
  
  HMETwoFermions2Z2TwoFermions     zHME;
  HMETwoFermions2Gamma2TwoFermions gHME;

};

//==========================================================================

// Helicity matrix element for the decay of a CP even Higgs ->  two fermions.

// Because the Higgs is spin zero the Higgs production mechanism is not 
// needed for calculating helicity density matrices.
 
class HMEHiggsEven2TwoFermions : public HelicityMatrixElement {

public:

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:

  // Coupling constants of the fermions with the Higgs.
  double p2CA, p2CV;

};

//==========================================================================

// Helicity matrix element for the decay of a CP odd Higgs ->  two fermions.
 
class HMEHiggsOdd2TwoFermions : public HelicityMatrixElement {

public:

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:
  
  // Coupling constants of the fermions with the Higgs.
  double p2CA, p2CV;

};

//==========================================================================

// Helicity matrix element for the decay of a charged Higgs ->  two fermions.
  
class HMEHiggsCharged2TwoFermions : public HelicityMatrixElement {

public:

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

private:
  
  // Coupling constants of the fermions with the Higgs.
  double p2CA, p2CV;

};

//==========================================================================

// Helicity matrix element which provides an unpolarized on-diagonal helicity
// density matrix. Used for unknown hard processes.
 
class HMEUnpolarized : public HelicityMatrixElement {

public:

  void calculateRho(unsigned int, vector<HelicityParticle>&);

};

//==========================================================================

// Base class for all tau decay helicity matrix elements.
   
class HMETauDecay : public HelicityMatrixElement {

public: 

  virtual void initWaves(vector<HelicityParticle>&);

  virtual complex calculateME(vector<int>);

  virtual double decayWeightMax(vector<HelicityParticle>&);

protected:

  virtual void initHadronicCurrent(vector<HelicityParticle>&) {};

  virtual void calculateResonanceWeights(vector<double>&, vector<double>&,
    vector<complex>&);

};

//==========================================================================

// Helicity matrix element for a tau decaying into a single scalar meson.
  
class HMETau2Meson : public HMETauDecay {

public:
    
  void initConstants();
    
  void initHadronicCurrent(vector<HelicityParticle>&);

};

//==========================================================================

// Helicity matrix element for a tau decaying into two leptons.
  
class HMETau2TwoLeptons : public HMETauDecay {
    
public:
    
  void initConstants();

  void initWaves(vector<HelicityParticle>&);

  complex calculateME(vector<int>);

};

//==========================================================================

// Helicity matrix element for a tau decaying into two mesons through a 
// vector meson resonance.
  
class HMETau2TwoMesonsViaVector : public HMETauDecay {

public:

  void initConstants();

  void initHadronicCurrent(vector<HelicityParticle>&);

private:

  // Resonance masses, widths, and weights.
  vector<double>  vecM, vecG, vecP, vecA;
  vector<complex> vecW;

};

//==========================================================================

// Helicity matrix element for a tau decay into two mesons through a vector 
// or scalar meson resonance.
  
class HMETau2TwoMesonsViaVectorScalar : public HMETauDecay {

public:

  void initConstants();
  
  void initHadronicCurrent(vector<HelicityParticle>&);

private:

  // Coupling to vector and scalar resonances.
  double scaC, vecC;

  // Resonance masses, widths, and weights.
  vector<double>  scaM, scaG, scaP, scaA, vecM, vecG, vecP, vecA;
  vector<complex> scaW, vecW;

};

//==========================================================================

// Helicity matrix element for a tau decay into three pions.
   
class HMETau2ThreePions : public HMETauDecay {

public:

  void initConstants();

  void initHadronicCurrent(vector<HelicityParticle>& p);

private:

  // Resonance masses, widths, and weights.
  vector<double>  rhoM, rhoG, rhoPp, rhoAp, rhoPd, rhoAd;
  double          f0M, f0G, f0P, f0A, f2M, f2G, f2P, f2A;
  double          sigM, sigG, sigP, sigA;
  vector<complex> rhoWp, rhoWd;
  complex         f0W, f2W, sigW;

  // Center of mass energies.
  double s1, s2, s3, s4;
  
  // Form factors.
  complex F1();
  complex F2();
  complex F3();
  
  // Running width and Breit-Wigner for the a1.
  double  a1Width(double);
  complex a1BreitWigner(double);

};

//==========================================================================

// Helicity matrix element for a tau decay into four pions.
  
class HMETau2FourPions : public HMETauDecay {

public:

  void initConstants();

  void initHadronicCurrent(vector<HelicityParticle>& p);

private:

  // G-function form factors (fits).
  double G(int i, double s);

  // T-vector functions.
  Wave4 t1(int, int, int, int);
  Wave4 t2(int, int, int, int);
  Wave4 t3(int, int, int, int);

  // Breit-Wigner denominators for the intermediate mesons.
  complex  a1D(double s);
  complex rhoD(double s);
  complex sigD(double s);
  complex omeD(double s);

  // Form factors needed for the a1, rho, and omega.
  double  a1FormFactor(double s);
  double rhoFormFactor1(double s);
  double rhoFormFactor2(double s);
  double omeFormFactor(double s);
  
  // Masses and widths of the intermediate mesons.
  double a1M, a1G, rhoM, rhoG, sigM, sigG, omeM, omeG;

  // Masses for the pions (charged and neutral).
  double picM, pinM;

  // Amplitude, phases, and weights for mixing.
  double  sigA, sigP, omeA, omeP;
  complex sigW, omeW;
  
  // Cut-off for a1 form factor.
  double lambda2;

};

//==========================================================================

// Helicity matrix element for a tau decay into flat phase space.
  
class HMETau2PhaseSpace : public HMETauDecay {

public:

  void initWaves(vector<HelicityParticle>&) {};

  complex calculateME(vector<int>) {return 1;}
  
  void calculateD(vector<HelicityParticle>&) {};

  void calculateRho(unsigned int, vector<HelicityParticle>&) {};

  double decayWeight(vector<HelicityParticle>&) {return 1.0;}

  double decayWeightMax(vector<HelicityParticle>&) {return 1.0;}

};

//==========================================================================

} // end namespace Pythia8

#endif // end Pythia8_HelicityMatrixElements_H
