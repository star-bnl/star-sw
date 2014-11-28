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


#include "TMath.h"
#include "TGeVSimParticle.h"

#include "TDatabasePDG.h"
#include "TParticlePDG.h"

ClassImp(TGeVSimParticle);

////////////////////////////////////////////////////////////////////////////////////////////////////

TGeVSimParticle::TGeVSimParticle(Int_t pdg, TGeVSim::Model_t  model, Float_t multiplicity,
				     Float_t T, Float_t dY, Float_t param2) {
  //
  // Standard constructor
  //
  // pdg          - Particle type code in PDG standard (see: http://pdg.lbl.gov)
  // model        - momentum distribution model of type TGeVSim::Model_t
  // multiplicity - multiplicity of particle type
  // T            - Inverse slope parameter ("temperature")
  // dY           - Raridity Width (only for models: kBoltzman and kLevy)
  // param2       - Additional parameter
  //
  // Multiplicity is interpreded either as a total multiplicity or as  
  // a multiplicity density dN/dy. Interpretation is determined either on
  // the event level in TGeVSim class or on the particle type level
  // in this class be SetMultTotal() method. If this method is used it overrides 
  // interpretation from TGeVSim.
  //
  // param2 is interpreted according to selected model.
  // If model = TGeVSim::kLevy then it is sigmaTemp
  // If model = TGeVSIm::kExpension then it is expansion velocity
  //

  const char *where = "TGeVSimParticle";
  const char *msg[] = {
	 "Param2 can be used only with model kLevy and kExpansion",
	 "Param2 not set for kLevy assuming sigTemp = 0.1",
    "Param2 not set for kExpansion assuming expVel = 0.5 c"
  };

  SetModel(model);

  fPDG = pdg;
  fT = T;
  fSigmaY = dY;

  if (model != TGeVSim::kLevy && model != TGeVSim::kExpansion && (param2 > 0))
	 Warning(where,msg[0]);

  if (model == TGeVSim::kLevy) {
	 if (param2 > 0.5 || param2 == 0.) {
		Warning(where, msg[1]);
		param2 = 10;
	 } 
	 SetSigmaTemp(param2);
  }

  if (model == TGeVSim::kExpansion) {
	 if (param2 < 0.001) {
		Warning(where, msg[2]);
		param2 = 0.5;
	 }
	 SetExpansionVelocity(param2);
  }

  fN = multiplicity;
  fMultTotal = kTRUE;
  fIsSetMult = kFALSE;

  fV1[0] = fV1[1] = fV1[2] = fV1[3] = 0.;
  fV2[0] = fV2[1] = fV2[2] = 0.;

  fIsEllipticSimple = fIsDirectedSimple = kTRUE;
  fIsEllipticOld = kFALSE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

TGeVSimParticle::TGeVSimParticle(Int_t pdg, TGeVSim::Model_t model, Float_t multiplicity) {
  //
  // pdg - Particle type code in PDG standard (see: http://pdg.lbl.gov)
  //  
  // Note that multiplicity can be interpreted by GeVSim 
  // either as Total multiplicity in the acceptance or dN/dY
  // 
 
  fPDG = pdg;
  fN = multiplicity; 
  fMultTotal = kTRUE;
  fIsSetMult = kFALSE;
  
  SetModel(model);

  fT = 0.;
  fSigmaY = 0.;
  fParam2 = 0.;
  
  fV1[0] = fV1[1] = fV1[2] = fV1[3] = 0.;
  fV2[0] = fV2[1] = fV2[2] = 0.; 

  fIsEllipticSimple = fIsDirectedSimple = kTRUE;
  fIsEllipticOld = kFALSE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void  TGeVSimParticle::SetModel(TGeVSim::Model_t model) {
  //
  // Set Model from enueration type TGeVSim::Model_t
  // For details about standard and custom models refer to
  // TGeVSim class and ALICE NOTE
  // 

  //if ((model < 1 || model > kExpansion) || (model > 5 && model < 10)) 
  //  Error("SetModel","Model Id ( %d ) out of range [1..5] or [10..12]", model);

  fModel = model;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetExpansionVelocity(Float_t vel)
{
  //
  // Expansion velocity for model kExpansion in speed of light units
  // 
  // If model for the particle type is not kExpansion or
  // vel is not between 0 and 1 Error message is issued.
  //

  const char *where = "SetExpansionVelocity";
  const char *msg[] = {
    "Expansion Velocity can be set only for model 'kExpansion'",
	 "Expansion Velocity = %f out of range (0-1)"
  };
  
  if (fModel != TGeVSim::kExpansion) Error(where, msg[0]);
  if (vel <= 0 || vel >= 1) Error(where, msg[1], vel);

  fParam2 = vel;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetSigmaTemp(Float_t sigT) 
{
  //
  // Set SigmaTemp for the Levy distribution
  // 
  // sigT have to ba a positive numeber lesser than 1
  // when sigT -> 0 Levy distribution -> Boltzman 
  // 
  // It is not recomendet to use sigmaTemp greatet than 0.2
  // comparision with HIJING suggest using sigmaTemp ~ 0.1
  // 

  const char *where = "SetSigmaTemp";
  const char *msg[] = {
    "SigmaTemp can be set only for model 'kLevy'",
	 "SigmaTemp = %f have to be positive",
	 "SigmaTemp = %f have to be lesser than 1"
  };

  if (fModel != TGeVSim::kLevy) Warning(where, msg[0]);
  if (sigT <= 0) Error(where, msg[1], sigT);
  if (sigT >= 1) Error(where, msg[2], sigT);

  fParam2 = sigT;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSimParticle::GetExpansionVelocity() const
{
  //
  // Returns expansion velocity
  // is selected model is not kExpansion Warning is issued
  
  if (fModel != TGeVSim::kExpansion) 
	 Warning("GetExpansionVelocity", "Model is not kExpansion");
  
  return fParam2;
}


////////////////////////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSimParticle::GetSigmaTemp() const 
{
  //
  // Returns SigmaTemp.
  // Is selected model is not kLevy Warning is issued
  //
  
  if (fModel != TGeVSim::kLevy)
	 Warning("GetSigmaTemp", "Model is not kLevy");
  
  return fParam2;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void  TGeVSimParticle::SetMultiplicity(Float_t mult) {
  //
  // Set multiplicity. The value is interpreted either as a total multiplciity
  // in the acceptance or as a multiplicity density - dN/dY at midrapidity
  //  
  
  const char *fName = "SetMultiplicity";
  if (mult < 0) Error(fName, "Multiplicity has to be positive");
  if (mult > 50000) Warning(fName, "Multiplicity greater than 50 000");

  fN = mult;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetMultTotal(Bool_t isTotal) {
  //
  // Switch between total multiplicity (kTRUE) and 
  // multiplciity density (kFALSE)
  //
  // If this method is used its overrides mode in TGenGeVSim 
  //
  
  fMultTotal = isTotal;
  fIsSetMult = kTRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
 
void TGeVSimParticle::SetDirectedSimple(Float_t v1) {
  //
  // Set directed flow coefficient to a value independent
  // of transverse momentum and rapidity
  //

  fV1[0] = v1;
  fIsDirectedSimple = kTRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetEllipticSimple(Float_t v2) {
  //
  // Set elliptic flow coefficient to a value independent
  // of transverse momentum and rapidity
  //

  fV2[0] = v2;
  fIsEllipticSimple = kTRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

Bool_t TGeVSimParticle::IsFlowSimple() {
  //
  // Function used by TGenGeVSim 
  //
  // Returns true if both Elliptic and Directed flow has a simple model.
  // If at least one is parametrised returns false. 
  // 

  return (fIsDirectedSimple && fIsEllipticSimple);
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetDirectedParam(Float_t v11, Float_t v12, Float_t v13, Float_t v14) {
  //
  // Set parameters for Directed Flow 
  // Actual flow coefficient is calculated as follows
  //
  // V1(Pt,Y) = (V11 + V12*Pt) * sign(Y) * (V13 + V14 * Y^3)
  //
  // where sign = 1 for Y > 0 and -1 for Y < 0
  // 
  // Default values
  // v11 = v12 = v13 = v14 = 0
  // 

  fV1[0] = v11;
  fV1[1] = v12;
  fV1[2] = v13;
  fV1[3] = v14;
  
  fIsDirectedSimple = kFALSE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetEllipticParam1(Float_t v21, Float_t pTmax, Float_t v22) {
  //
  // Set parameters for Elliptic Flow
  // Actual flow coefficient is calculated as follows
  //
  // pTmax is in GeV 
  // v21 - flow value at saturation
  //
  //
  // V2 = v21 * (pT/pTMax ) * exp (-v22 * y^2)    where pT <= pTmax  
  //      v21 * exp (-v22 * y^2)                   where pT > pTmax  
  //
  // Default values:
  // v21 = pTMax = v23 = 0
  //
  // The parametrisation is suitable for relativistic particles
  // eg. Pions (at RHIC energies)
  //


  fV2[0] = v21;
  fV2[1] = pTmax;
  fV2[2] = v22;

  fIsEllipticSimple = kFALSE;
  fIsEllipticOld = kFALSE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::SetEllipticParam2(Float_t v21, Float_t v22, Float_t v23) {
  //
  // Set parameters for Elliptic Flow
  // Actual flow coefficient is calculated as follows
  //
  // V2 = (V21 + V22 pT^2) * exp (-V23 * y^2)
  //
  // The parameterisation is suitable for heavy particles: proton, kaon
  //
  // Default values
  // v21 = v22 = v23 = 0

  fV2[0] = v21;
  fV2[1] = v22;
  fV2[2] = v23;

  fIsEllipticSimple = kFALSE;
  fIsEllipticOld = kTRUE;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSimParticle::GetDirectedFlow(Float_t pt, Float_t y) {
  //
  // Return coefficient of a directed flow for a given pt and y.
  // For coefficient calculation method refer to SetDirectedParam()
  // 
  
  if (fIsDirectedSimple) return fV1[0];

  Float_t v;
  
  v = (fV1[0] + fV1[1]* pt) * TMath::Sign((Float_t)1.,y) *
    (fV1[2] + fV1[3] * TMath::Abs(y*y*y) );

  return v;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSimParticle::GetEllipticFlow(Float_t pt, Float_t y) {
  //
  // Return coefficient of a elliptic flow for a given pt and y.
  // For coefficient calculation method refer to SetEllipticParam()
  // 

  if (fIsEllipticSimple) return fV2[0];

  if (fIsEllipticOld) {
    
    // old parametrisation
    return (fV2[0]+fV2[1]*pt*pt) * TMath::Exp(-fV2[2]*y*y);

  } else {

    // new "pionic" parameterisation
    if (pt < fV2[1]) return ( (pt / fV2[1]) * fV2[0] * TMath::Exp(-fV2[2]*y*y) ); 
    else  return ( fV2[0] * TMath::Exp(-fV2[2]*y*y) );
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

void TGeVSimParticle::Print(Option_t* option) const
{
  //
  // Print informations about the particle type
  //

  const char *dummy = "unknown type";
  const char *models[15] = {
	 "","Boltzman  ", "Levy      ", "Pratt    ", "Bertsch  ", "Expansion ",
	 "", "", "", "",
	 "1D Formulas", "2D Formula", "1D Histograms", "2D Histogram", "Function"
  };

  const char *name;
  const TParticlePDG *ap = TDatabasePDG::Instance()->GetParticle(fPDG);
  if (ap) name = ap->GetName();
  else name = (char*)dummy;

  printf("** %s\tModel: %s Mult = %4d, T = %.3f GeV\n", name, models[fModel], (Int_t)fN, fT);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
