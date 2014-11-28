#ifndef TGeVSimParticle_h
#define TGeVSimParticle_h

////////////////////////////////////////////////////////////////////////////////
//
// TGeVSimParticle is a helper class for GeVSim event generator
//
// One object of this class keep information about one particle type.
// Those include PDG code, momentum distribution model, and its parameter
// as well as Directed and Elliptic flow parameters.
// 
// For documentation about GeVSim event generator and implemented 
// momentum distributions models refer to TGeVSim
//
// Sylwester Radomski, e-mail: S.Radomski@gsi.de
// GSI, Dec 13, 2002
//
////////////////////////////////////////////////////////////////////////////////

#include "TGeVSim.h"

class TGeVSimParticle : public TObject {

 public:

  ////////////////////////////////////////////////////////////////////////////
  
  TGeVSimParticle() {} 
  TGeVSimParticle(Int_t pdg, TGeVSim::Model_t model, Float_t multiplicity); 
  TGeVSimParticle(Int_t pdg, TGeVSim::Model_t model, Float_t multiplicity, 
		    Float_t T, Float_t dY = 1., Float_t param2=0.);
  
  ~TGeVSimParticle() {}
  
  ////////////////////////////////////////////////////////////////////////////
  
  Int_t GetPdgCode() const {return fPDG;}
  TGeVSim::Model_t GetModel() const {return fModel;}
  
  Float_t GetTemperature() const {return fT;}
  Float_t GetSigmaY() const {return fSigmaY;}
  Float_t GetExpansionVelocity() const;
  Float_t GetSigmaTemp() const;

  void SetModel(TGeVSim::Model_t model);
  void SetTemperature(Float_t T) {fT = T;}
  void SetSigmaY(Float_t sigma) {fSigmaY = sigma;}
  void SetExpansionVelocity(Float_t vel);
  void SetSigmaTemp(Float_t sigT);
  

  // Multiplicity

  void    SetMultiplicity(Float_t mult);
  Float_t GetMultiplicity() {return fN;}

  void   SetMultTotal(Bool_t isTotal = kTRUE);

  Bool_t IsMultTotal() {return fMultTotal;}
  Bool_t IsMultForced() {return fIsSetMult;}
  
  // Flow
  
  void SetDirectedSimple(Float_t v1);
  void SetEllipticSimple(Float_t v2);

  void SetDirectedParam(Float_t v11, Float_t v12=0, Float_t v13=1, Float_t v14=0);
  void SetEllipticParam1(Float_t v21, Float_t pTmax, Float_t v22=0.);
  void SetEllipticParam2(Float_t v21, Float_t v22, Float_t v23);

  Bool_t IsFlowSimple();

  Float_t GetDirectedFlow(Float_t pt, Float_t y);
  Float_t GetEllipticFlow(Float_t pt, Float_t y);
  
  void Print(Option_t* option="") const;  // argument to remove compiler warning about overloaded virtual functions 

  ////////////////////////////////////////////////////////////////////////////
  
 private:
  
  Int_t fPDG;                // Particle type code
  TGeVSim::Model_t fModel;   // Transverse momentum model

  Float_t fN;                // Multiplicity (subject to scalling)
  Bool_t  fMultTotal;        // multiplicity mode: Total or dN/dY
  Bool_t  fIsSetMult;        // force multiplicity mode or use from TGenGeVSim

  Float_t fT;                // Slope Parameter (subject to scalling)
  Float_t fSigmaY;           // Rapidity Width
  Float_t fParam2;           // Expansion Velocity in c units (subject to scalling)
  
  Float_t fV1[4];            // Directed Flow coefficient parameters
  Float_t fV2[3];            // Elliptic Flow coefficient parameters
  
  Bool_t fIsDirectedSimple;  // indicate use constant value for directed (v1) 
  Bool_t fIsEllipticSimple;  // indicate use constant value for elliptic (v2)
  Bool_t fIsEllipticOld;     // linear / quadratical pT parametrisation

 public:
  
  ClassDef(TGeVSimParticle, 1)
    
};

////////////////////////////////////////////////////////////////////////////////

#endif
