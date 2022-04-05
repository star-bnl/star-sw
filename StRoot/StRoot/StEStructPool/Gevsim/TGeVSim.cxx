////////////////////////////////////////////////////////////////////////////////
// 
// GeVSim is a simple Monte-Carlo event generator for testing detector and 
// algorythm performance especialy concerning flow and event-by-event studies
//
// In this event generator particles are generated from thermal distributions 
// without dynamics and microscopic simulations. Distribution parameters like
// multiplicity, particle type yields, inverse slope parameters, flow 
// coeficients and expansion velocities are expleicite defined by the user.
//
// GeVSim contains five thermal distributions. Four of them 
// are the same as in  MevSim event generator developed 
// for STAR experiment. In GeVSim also Levy distribution is implemented
// for beter decription of high transverse momentum tracks.
//
// In addition custom distributions can be used be the mean 
// either two dimensional formula (TF2), a two dimensional histogram or
// two one dimensional histograms.
//  
// Azimuthal distribution is deconvoluted from (Pt,Y) distribution
// and is described by two Fourier coefficients representing 
// Directed and Elliptic flow. 
//
// Details on implemented Pt-Y distributions:
//
// User can use either standard distribution or use custom one.
// Distributions are indentified by enumeration type GeVSim::Model_t
//
// Standard distributions can be divided into two classes.
// First class containng kBoltzman and kLevy. Those distributions have 
// Pt deconvoluted from rapidity distribution. Rapidity is aproximated
// bu Gaussian.  
//
// Second class contains; kPratt, kBertsch and kExpansion. In those
// distributions Pt-Y is decribed by 2 dimensional formula. 
// Note that generation from this class is slower.
//  
// Pt-Y distribution as well as its parameters are defined for each particle 
// type. Technically one perticle type corresponds to one object TGeVSimParticle.
// Refer for the documentation of this class for details.
//
// Event by Event:
// 
// GeVSim have extended capabilities on Event-by-Event fluctuations.
// Those are defined by named formula. Refer to method FindScaler for details.
//
// MACROS:
// GeVSim event generator is accompanied be a set of macros.
//
// testGeVSim.C       : a place to start
// testGeVSimCut.C    : acceptance cuts
// testGeVSimPtEta.C  : test implemented Pt and pseud-rapidity distributions
// testGeVSimdNdY.C   : total multiplcity and multiplicity density
// testGeVSimEbyE.C   : multiplicity fluctuations
// testGeVSimScan.C   : multiplicity scan
// testGeVSimForm.C   : custom momentum distribution
//
//
// Sylwester Radomski, mail: S.Radomski@gsi.de
// GSI, Dec 12, 2002
//
////////////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TRandom.h"
#include "TCanvas.h"
#include "TDatabasePDG.h"
#include "TParticle.h"
#include "TObjArray.h"
#include "TF1.h"
#include "TF2.h"
#include "TH1.h"
#include "TH2.h"

#include "TCanvas.h"

#include "TGeVSim.h"
#include "TGeVSimParticle.h"
#include "TGeVSimEvent.h"
#include "TGeVSimParams.h"

#include <iostream>
#include <sys/times.h>

ClassImp(TGeVSim);

//////////////////////////////////////////////////////////////////////////////////

TGeVSim::TGeVSim() : TGenAcceptance() 
{
  //
  // Default constructor
  // To be used by ROOT framework only
  // Please use other constructors.
  //

  fPsi = 0;
  fIsMultTotal = kTRUE;
  fEventNumber = 0;

  //PH  InitFormula();
  for (Int_t i=0; i<4; i++)  
    fPtYFormula[i] = 0;
}

//////////////////////////////////////////////////////////////////////////////////

TGeVSim::TGeVSim(const char *name) 
  :TGenAcceptance(name, "GeVSim Event Generator")
{
  //
  // Standard Constructor
  // Creates generator with a name
  // 
  // Reaction plane angle is assumed = 0
  // Multiplicity mode: TOTAL
  //

  fPsi = 0;
  fIsMultTotal = kTRUE;
  fEventNumber = 0;

  // initalization

  fPartTypes = new TObjArray();
  fPartBuffer = new TClonesArray("TParticle", 5000, kTRUE);  
  fEvent = new TGeVSimEvent(10);

  InitFormula();
}

//////////////////////////////////////////////////////////////////////////////////

TGeVSim::TGeVSim(const char *name, Float_t psi, Bool_t isMultTotal) 
  : TGenAcceptance(name,"GeVSim Event Generator") 
{
  //
  // Standard Constructor.
  // Creates generator with a name, reaction plane angle
  // and multiplicity mode.
  // 
  //  psi   - event plane in degrees [0-360]
  //
  //  isMultTotal - multiplicity mode
  //           kTRUE - total multiplicity (default)
  //           kFALSE - multiplicity density dN/dY at midrapidity
  // 
  // Note that event plane can be modified on the event by event basis
  //

  // checking consistancy
  
  if (psi < 0 || psi > 360 ) 
    Error ("TGeVSim", "Event plane angle ( %d )out of range [0..360]", psi);

  fPsi = psi * TMath::Pi() / 180. ;
  fIsMultTotal = isMultTotal;
  
  fEventNumber = 0;
  
  fPartTypes = new TObjArray();
  fPartBuffer = new TClonesArray("TParticle", 1000);
  fEvent = new TGeVSimEvent(10);

  InitFormula();
}

//////////////////////////////////////////////////////////////////////////////////

TGeVSim::~TGeVSim() 
{
  //
  //  Default Destructor
  //  
  //  Removes TObjArray keeping list of registered particle types
  //

  if (fPartTypes) delete fPartTypes;
  if (fPartBuffer) delete fPartBuffer;
}

//////////////////////////////////////////////////////////////////////////////////

TGeVSim* TGeVSim::GetDefault() 
{
  //
  // Static function
  // 
  // Returns CONFIGURED gevsim event generator with 
  // standard particle multiplcities and themperatures.
  //
  // Currently standard configuration is
  // 1000 Pi+ and 1000 Pi- following Levy distr. with temp = 0.18
  // 200 K+ and 200 K- following Levy distr. with temp = 0.25
  //
  // sigmaTemp parameters of Levy are set to 10 
  // rapdity width = 1
  //
  
  TGeVSim *g = new TGeVSim("default", 0);

  TGeVSimParticle *p;
  
  p = new TGeVSimParticle(211, kLevy, 1000, 0.18, 1, 10);
  p->SetEllipticSimple(0.05);
  g->AddParticleType(p);

  p = new TGeVSimParticle(-211, kLevy, 1000, 0.18, 1, 10);
  p->SetEllipticSimple(0.05);
  g->AddParticleType(p);

  g->AddParticleType(new TGeVSimParticle(321, kLevy, 200, 0.25, 1, 10));
  g->AddParticleType(new TGeVSimParticle(-321, kLevy, 200, 0.25, 1, 10));
  
  g->SetEtaRange(-3, 3);
  //g->SetPtCut(0, 3);

  return g;
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::Print(Option_t* option) const
{
  //
  // Prints Information about configuration of the generator
  // and all registered particle types.
  //
  
  const char *multType;
  if (fIsMultTotal) multType = "TOTAL";
  else multType = "DENSITY dN/dY";

  printf("\n*******************************************************\n**\n");
  printf("** WELCOME to GeVSim\n");
  printf("** NAME: %s TITLE: %s\n**\n", GetName(), GetTitle());
  printf("** Multiplicity type = %s\n", multType);
  printf("** Event Plane %3.2f [deg]\n**\n", fPsi*180/3.14);
  printf("** Registered Particle Types:\n**\n");

  if (fPartTypes) {
    for (Int_t i=0; i<fPartTypes->GetEntries(); i++) 
      ((TGeVSimParticle*)fPartTypes->At(i))->Print();
  }
  
  printf("**\n*******************************************************\n\n");

}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::InitFormula() 
{
  //
  // private function
  //
  // Initalizes formulas used in GeVSim.
  // Manages strings and creates TFormula objects from strings
  // 

  // Deconvoluted Pt Y formula

  // ptForm: pt -> x ,  mass -> [0] , temperature -> [1]
  // levy    pt -> x ,  mass -> [2] , temperature -> [1] sigmaTemp -> [2]

  
  const char* ptNames[2] = {"gevsimPt1", "gevsimPt2"};
  
  const char* ptForm[2]  = {
    " x * exp( -sqrt([0]*[0] + x*x) / [1] )",
    " x * ( 1+ (sqrt([0]*[0]+x*x)*[2])/[1] )^(-1./[2]) "
  };

  for(Int_t i=0; i<2; i++) {
    fPtFormula[i]  = new TF1(ptNames[i], ptForm[i], 0, 4);
    fPtFormula[i]->SetNpx(100);
  }

  fPtFormula[0]->SetParNames("mass", "temperature");
  fPtFormula[0]->SetParameters(1., 1.);
  
  fPtFormula[1]->SetParNames("mass", "temperature", "sigmaTemp");
  fPtFormula[1]->SetParameters(1., 1., 1.);
  
  
  // 2D Models 

  // pt -> x , Y -> y
  // mass -> [0] , temperature -> [1] , expansion velocity -> [2]

  
  const char *formE = " ( sqrt([0]*[0] + x*x) * cosh(y) ) ";
  const char *formG = " ( 1 / sqrt( 1 - [2]*[2] ) ) ";
  const char *formYp = "( [2]*sqrt(([0]*[0]+x*x)*cosh(y)*cosh(y)-[0]*[0])/([1]*sqrt(1-[2]*[2]))) ";

  const char* formula[3] = {
    " x * %s * exp( -%s / [1]) ", 
    " (x * %s) / ( exp( %s / [1]) - 1 ) ",
    " x*%s*exp(-%s*%s/[1])*((sinh(%s)/%s)+([1]/(%s*%s))*(sinh(%s)/%s-cosh(%s)))"
  };

  const char* paramNames[3] = {"mass", "temperature", "expVel"};

  char buffer[1024];

  sprintf(buffer, formula[0], formE, formE);
  fPtYFormula[0] = new TF2("gevsimPtY_2", buffer, 0, 3, -2, 2);

  sprintf(buffer, formula[1], formE, formE);
  fPtYFormula[1] = new TF2("gevsimPtY_3", buffer, 0, 3, -2, 2);

  sprintf(buffer, formula[2], formE, formG, formE, formYp, formYp, formG, formE, formYp, formYp, formYp);
  fPtYFormula[2] = new TF2("gevsimPtY_4", buffer, 0, 3, -2, 2);

  fPtYFormula[3] = 0;


  // setting names & initialisation

  Int_t i, j;
  for (i=0; i<3; i++) {    

    fPtYFormula[i]->SetNpx(100);        // step 30 MeV  
    fPtYFormula[i]->SetNpy(100);        //

    for (j=0; j<3; j++) {

      if ( i != 2 && j == 2 ) continue; // ExpVel
      fPtYFormula[i]->SetParName(j, paramNames[j]);
      fPtYFormula[i]->SetParameter(j, 0.5);
    }
  }
  
  // Phi Flow Formula

  // phi -> x
  // Psi -> [0] , Direct Flow -> [1] , Ellipticla Flow -> [2]

  const char* phiForm = " 1 + 2*[1]*cos(x-[0]) + 2*[2]*cos(2*(x-[0])) ";
  fPhiFormula = new TF1("gevsimPhi", phiForm, 0, 2*TMath::Pi());

  fPhiFormula->SetParNames("psi", "directed", "elliptic");
  fPhiFormula->SetParameters(0., 0., 0.);

  fPhiFormula->SetNpx(180);

}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::AddParticleType(TGeVSimParticle *part) 
{
  //
  // Adds new type of particles.
  // 
  // Parameters are defeined in TGeVSimParticle object
  // This method has to be called for every particle type
  //

  if (!fPartTypes) fPartTypes = new TObjArray();
  fPartTypes->Add(part);
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::SetMultTotal(Bool_t isTotal) 
{
  //
  // Set multiplicity mode
  // isTotal = kTRUE:  Total multiplicity
  //           kFALSE: Multiplicity density dN/dy
  //
  // Particle type can modify this value 
  // 

  fIsMultTotal = isTotal;
}

//////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSim::FindScaler(Int_t paramId, Int_t pdg) 
{
  //
  // private function
  // Finds Scallar for a given parameter.
  // Function used in event-by-event mode.
  //
  // There are two types of scallars: deterministic and random
  // and they can work on either global or particle type level.
  // For every variable there are four scallars defined.  
  //  
  // Scallars are named as folowa
  // deterministic global level : "gevsimParam"        (eg. "gevsimTemp")
  // deterinistig type level    : "gevsimPdgParam"     (eg. "gevsim211Mult")
  // random global level        : "gevsimParamRndm"    (eg. "gevsimMultRndm")
  // random type level          : "gevsimPdgParamRndm" (eg. "gevsim-211V2Rndm");
  //
  // Pdg - code of a particle type in PDG standard (see: http://pdg.lbl.gov)
  // Param - parameter name. Allowed parameters:
  //
  // "Temp"      - inverse slope parameter
  // "SigmaY"    - rapidity width - for model (1) only
  // "ExpVel"    - expansion velocity - for model (4) only
  // "SigmaTemp" - parameter of the Levy distribution
  // "V1"        - directed flow
  // "V2"        - elliptic flow
  // "Mult"      - multiplicity
  //
  
  static const char* params[] = {"Temp", "SigmaY", "ExpVel", "SigmaTemp", "V1", "V2", "Mult"};
  static const char* ending[] = {"", "Rndm"};

  static const char* patt1 = "gevsim%s%s";
  static const char* patt2 = "gevsim%d%s%s";

  char buffer[80];
  TF1 *form;
  
  Float_t scaler = 1.;

  // Scaler evoluation: i - global/local, j - determ/random

  Int_t i, j;
  
  for (i=0; i<2; i++) {
    for (j=0; j<2; j++) {
      
      form = 0;
      
      if (i == 0) sprintf(buffer, patt1, params[paramId], ending[j]);      
      else sprintf(buffer, patt2, pdg, params[paramId], ending[j]);
      
      form = (TF1 *)gROOT->GetFunction(buffer);
		
      if (form != 0) {
	if (j == 0) scaler *= form->Eval(fEventNumber); 
	if (j == 1) {
	  form->SetParameter(0, fEventNumber);
	  scaler *= form->GetRandom();
	}
      }
    }
  }
  
  return scaler;
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::DetermineReactionPlane() 
{
  //
  // private function used by Generate()
  //
  // Retermines Reaction Plane angle and set this value 
  // as a parameter [0] in fPhiFormula
  //
  // Event Plane can be set on the event-by-event basis using 
  // TF1 object named "gevsimPsi" or "gevsimPsiRndm" 
  //
  // Note: if "gevsimPsiRndm" function is found it overrides both 
  //       "gevsimPhi" function and initial fPsi value
  //
  //

  TF1 *form;
  
  form = 0;
  form = (TF1 *)gROOT->GetFunction("gevsimPsi");
  if (form) fPsi = form->Eval(fEventNumber) * TMath::Pi() / 180;
  
  form = 0;
  form = (TF1 *)gROOT->GetFunction("gevsimPsiRndm");
  if (form) fPsi = form->GetRandom() * TMath::Pi() / 180;

  //cout << "Psi = " << fPsi << "\t" << (Int_t)(fPsi*180./TMath::Pi()) << endl;
  
  fPhiFormula->SetParameter(0, fPsi);
}

//////////////////////////////////////////////////////////////////////////////////

Float_t TGeVSim::GetdNdYToTotal() 
{
  //
  // Private, helper function used by Generate()
  // Returns total multiplicity to dN/dY ration using current model.
  //
  // Not testes carefully
  //

  Float_t integ, mag;
  const Double_t maxPt = 3.0, maxY = 2.; 

  switch (fModel) {
	 
  case kBoltzman:
  case kLevy:

    return TMath::Sqrt(2*TMath::Pi()) * fSigmaY;

  case kPratt:
  case kBertsch:
  case kExpansion:
  case kFormula2D:
	 
    integ =  fCurrentForm2D->Integral(0,maxPt, -maxY, maxY);
    mag = fCurrentForm2D->Integral(0, maxPt, -0.1, 0.1) / 0.2;
    return integ/mag;
	 
  case kHist1D:
	 
    integ = fHist[1]->Integral(); 
    mag = fHist[1]->GetBinContent(fHist[0]->FindBin(0.));
    mag /= fHist[1]->GetBinWidth(fHist[0]->FindBin(0.));
    return integ/mag;
	 
  case kHist2D:
	 
    Int_t yBins = fPtYHist->GetNbinsY();
    Int_t ptBins = fPtYHist->GetNbinsX();

    integ = fPtYHist->Integral(0, ptBins, 0, yBins);
    mag = fPtYHist->Integral(0, ptBins, (yBins/2)-1, (yBins/2)+1 ) / 2;
    return integ/mag;
  }
  
  return 1;
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::SetFormula(Int_t pdg) 
{
  //
  // Private function used by Generate() 
  //
  // Configure a formula for a given particle type and model Id (in fModel).
  // If custom formula or histogram was selected the function tries
  // to find it.
  //  
  // The function implements naming conventions for custom distributions names 
  // 

  char buff[40];
  const char* msg[4] = {
    "Custom Formula for Pt Y distribution not found [pdg = %d]",
    "Histogram for Pt distribution not found [pdg = %d]", 
    "Histogram for Y distribution not found [pdg = %d]",
    "HIstogram for Pt Y dostribution not found [pdg = %d]"
  };

  const char* pattern[8] = {
    "gevsimDistPtY", "gevsimDist%dPtY",
    "gevsimHistPt", "gevsimHist%dPt",
    "gevsimHistY", "gevsimHist%dY",
    "gevsimHistPtY", "gevsimHist%dPtY"
  };

  const char *where = "SetFormula";
  
  fCurrentForm1D = fCurrentForm2D = 0;
  
  
  switch (fModel) {

  case kBoltzman:
  case kLevy:
	 
    fCurrentForm1D = fPtFormula[fModel-1];
    return;
	 
  case kPratt:
  case kBertsch:
  case kExpansion:
	 
    fCurrentForm2D = fPtYFormula[fModel-3];
    return;

  case kFormula2D:
	 
    sprintf(buff, pattern[1], pdg);
    fCurrentForm2D = (TF2*)gROOT->GetFunction(buff);
	 
    if (!fCurrentForm2D)
      fCurrentForm2D = (TF2*)gROOT->GetFunction(pattern[0]);
	 
    if (!fCurrentForm2D) Error(where, msg[0], pdg);
    return;
	 
  case kHist1D:
	 
    for (Int_t i=0; i<2; i++) {
		
      fHist[i] = 0;
      sprintf(buff, pattern[3+2*i], pdg);
      fHist[i] = (TH1D*)gROOT->FindObject(buff);
		
      if (!fHist[i])
	fHist[i] = (TH1D*)gROOT->FindObject(pattern[2+2*i]);
      
      if (!fHist[i]) Error(where, msg[1+i], pdg);
    }
    return;
	 
  case kHist2D:
	 
    fPtYHist = 0;
    sprintf(buff, pattern[7], pdg);
    fPtYHist = (TH2D*)gROOT->FindObject(buff);
	 
    if (!fPtYHist)
      fPtYHist = (TH2D*)gROOT->FindObject(pattern[6]);
	 
    if (!fPtYHist) Error(where, msg[4], pdg);
    return;
  }
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim:: AdjustFormula() 
{
  //
  // Private Function
  // Adjust fomula bounds according to acceptance cuts.
  //
  // To improve performance function scan the formulas 
  // and cuts to "resonable" limits.
  //
  // WARNING !
  // If custom formula was provided original cuts are preserved
  // 

  const Double_t kCutMaxPt = 5.0;
  const Double_t kCutMaxY = 6.0;
  Double_t minPt, maxPt, minY, maxY;

	 
  const Int_t NPART = 20;
  const Float_t dX = 1./NPART;
  const Int_t NBINS = 30;

  if (fModel > kExpansion) return;

  // max Pt 
  if (TestBit(kPtRange) && fPtCutMax < kCutMaxPt ) maxPt = fPtCutMax;
  else maxPt = kCutMaxPt;

  // min Pt
  if (TestBit(kPtRange)) minPt = fPtCutMin;
  else minPt = 0;

  // CutMax Pt < CutMax P
  if (TestBit(kPRange) && maxPt > fPCutMax) maxPt = fPCutMax;
 
  // max and min rapidity
  if (TestBit(kYRange)) {
    minY = fYCutMin;
    maxY = fYCutMax;
  } else {
    minY = -kCutMaxY;
    maxY = kCutMaxY;
  }
  
  // adjust formula
  // factored models

  if (fModel < kPratt) {

    Float_t maxValue = 0, value;

    fCurrentForm1D->SetRange(minPt, maxPt);

    // find aprox max value
    maxValue = 0;
    for(Int_t part=NPART; part>1; part--) {
      Float_t x = dX * (maxPt-minPt) * part + minPt;
      value = fCurrentForm1D->Eval(x);
      maxValue = (value>maxValue)? value : maxValue;
    }

    // find cut-off position 
    for(Int_t part=NPART; part>1; part--) {
      Float_t x = dX * (maxPt-minPt) * part + minPt;
      value = fCurrentForm1D->Eval(x);
      if (value> 0.001*maxValue) {
	maxPt = x;
	break;
      }
    }

    fCurrentForm1D->SetRange(minPt, maxPt);
    fCurrentForm1D->SetNpx((int)((maxPt-minPt)*NBINS)); 
  }

  // two dimensional models
  
  if (fModel > kLevy) {

    fCurrentForm2D->SetRange(minPt, minY, maxPt, maxY);
	
    Float_t maxValue = 0, midPt = 0.1, value;

    // find aproximately max value on rapidity = 0 line
    for(Int_t part=NPART; part>1; part--) {
      Float_t x = dX * (maxPt-minPt) * part + minPt;
      value = fCurrentForm2D->Eval(x, 0);
      if (value>maxValue) {
	maxValue = value;
	midPt = x;
      }
    }
	 
    for(Int_t part=NPART; part>1; part--) {
      Float_t x = dX * maxPt * part;
      value = fCurrentForm2D->Eval(x, 0);
      if (value > 0.01 * maxValue) {
	maxPt = x;
	break;
      }
    }
	 
    // trim negative and positive rapidity 

    for (Int_t part=NPART; part>1; part--) {
      Float_t y = -dX * minY * part;
      value = fCurrentForm2D->Eval(midPt, y);
      if (value > 0.005 * maxValue) {
	minY = -y;
	break;
      }
    }
	 
    for (Int_t part=NPART; part>1; part--) {
      Float_t y = dX * maxY * part;
      value = fCurrentForm2D->Eval(midPt, y);
      if (value > 0.005 * maxValue) {
	maxY = y;
	break;
      }
    }
	 	 
    fCurrentForm2D->SetRange(minPt, minY, maxPt, maxY);
    fCurrentForm2D->SetNpx((int)((maxPt-minPt)*NBINS));
    fCurrentForm2D->SetNpy((int)((maxY-minY)*NBINS));  
  }

  // azimuthal cut

  if (TestBit(kPhiRange)) {
    fPhiFormula->SetRange(fPhiCutMin, fPhiCutMax);
    fPhiFormula->SetNpx((int)((fPhiCutMax-fPhiCutMin)*2*NBINS));
  }
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::GetRandomPtY(Double_t &pt, Double_t &y) 
{
  //
  // Private function used by Generate()
  //
  // Returns random values of Pt and Y corresponding to selected
  // distribution.
  //
  
  switch(fModel) {
	 
  case kBoltzman:
  case kLevy:
	 
    pt = fCurrentForm1D->GetRandom();
    y = gRandom->Gaus(0, fSigmaY);
    return;
	 
  case kPratt:
  case kBertsch:
  case kExpansion:
  case kFormula2D:
	 
    fCurrentForm2D->GetRandom2(pt, y);
    return;
	 
  case kHist1D:
	 
    pt = fHist[0]->GetRandom();
    y = fHist[1]->GetRandom();
    return;
	 
  case kHist2D:
	 
    fPtYHist->GetRandom2(pt, y);
    return;	 
	 
    //case kFormula1D:
    //case kFunction:
    //return;
  }
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::GenerateEvent() 
{
  // 
  // One (and preferred) function to generate an event
  //
  // Generated particles are stored in internal TClonesArray 
  // the array can be accesed by GetListOfParticles()
  // refer to macro testGeVSim.C for usage
  //
  
  fEventNumber++;
  fPartBuffer->Clear();
  Generate(kTRUE);
}

//////////////////////////////////////////////////////////////////////////////////

Int_t TGeVSim::ImportParticles(TClonesArray *particles, Option_t *option) 
{
  //
  // Function to generate en event
  //
  // Particles are stored in provided TClonesArray
  //
  
  fEventNumber++;

  fPartBuffer = particles;
  fPartBuffer->Clear();
  Generate(kTRUE);

  return 0;
}

//////////////////////////////////////////////////////////////////////////////////

TObjArray* TGeVSim::ImportParticles(Option_t *option) 
{
  //
  // Function to generate an event
  //
  // Particles are stored in internal TObjArray and can
  // be accesd one by one using GetParticle(i) or 
  // the array can be taken be GetPrimaries()
  //

  fEventNumber++;

  if (fPartBuffer) fPartBuffer = 0;

  fParticles->Clear();
  Generate(kFALSE);

  return fParticles; 
}

//////////////////////////////////////////////////////////////////////////////////

void TGeVSim::Generate(Bool_t clones) 
{
  //
  // Private function used by ImportParticle() and GenerateEvent()
  // functions. This function do actual job and puts particles on the stack.
  //
  //

  long points[20];
  int npoint = 0;


  Int_t pdg;                    // particle type
  Float_t mass;                 // particle mass
  Float_t energy;               // total energy

  Float_t multiplicity = 0;           
  Bool_t isMultTotal = kTRUE;
  
  Int_t nPartTotal = 0;

  Float_t paramScaler;
  Float_t directedScaller = 1., ellipticScaller = 1.;

  TLorentzVector *v = new TLorentzVector(0,0,0,0);
  TLorentzVector *orgin = new TLorentzVector(0,0,0,fEventNumber);

  // Particle params database

  TDatabasePDG *db = TDatabasePDG::Instance(); 
  TParticlePDG *type;
  TGeVSimParticle *partType;
  // TGeVSimParams *evpars;  // MSD

  Int_t nType, nParticle, nParam;

  // reaction plane determination and model
  DetermineReactionPlane();

  // *** MSD ***
  if (!fEvent) {
    //cout << endl << "*** ERROR: NO EVENT INITIALIZED! ***" << endl;
    Warning("Generate","No event initialized");
    return;
  }
  //if (!fEvent) fEvent = new TGeVSimEvent(10);  // Initialize if necessary
  fEvent->NextEvent();                       // Reset for next event
  fEvent->SetEventNumber(fEventNumber);      // Set event number
  fEvent->SetPsi(fPsi);                      // Set reaction plane angle
  TGeVSimParams *evpars = new TGeVSimParams();
//   evpars->Print(); // DEBUG
  // **********


  if (fIsVerbose) Info("Generate", "Generating Event = %d \t Event Plane = %3.2f deg", fEventNumber, fPsi*180/3.14);

  // loop over particle types

  for (nType = 0; nType < fPartTypes->GetEntries(); nType++) {
  
    points[npoint++] = times(0);

    partType = (TGeVSimParticle *)fPartTypes->At(nType);

    pdg = partType->GetPdgCode();
    type = db->GetParticle(pdg);
    mass = type->Mass();
    
    fModel = partType->GetModel();
    SetFormula(pdg);

    // ******* MSD ******
    //if (evpars) delete evpars;
    //TGeVSimParams *evpars = new TGeVSimParams();
    evpars->PDG = pdg; 
    evpars->Model = fModel;
    // ******************

    if (fCurrentForm1D) fCurrentForm1D->SetParameter("mass", mass);
    if (fCurrentForm2D) fCurrentForm2D->SetParameter("mass", mass);
    
    // Evaluation of parameters - loop over parameters
    
    for (nParam = kTemp; nParam <= kMult; nParam++) {
      
      Float_t tmp;

      paramScaler = FindScaler(nParam, pdg);
		
      switch (nParam) {
	
      case kTemp:
	tmp = paramScaler * partType->GetTemperature();
	if (fCurrentForm1D) fCurrentForm1D->SetParameter("temperature", tmp);
	if (fCurrentForm2D) fCurrentForm2D->SetParameter("temperature", tmp);
	evpars->Temp = tmp; // MSD
	break;
	
      case kSigmaY:
	fSigmaY =  paramScaler * partType->GetSigmaY();
	evpars->SigmaY = fSigmaY; // MSD
	break;

      case kExpVel:		  
	if (fModel == kExpansion) {
	  tmp =  paramScaler * partType->GetExpansionVelocity();
	  if (tmp == 0.0) tmp = 0.0001;
	  if (tmp == 1.0) tmp = 0.9999; // Used to be 9.9999			 
	  fCurrentForm2D->SetParameter("expVel", tmp);
	  evpars->ExpVel = tmp; // MSD		 
	}		 
	break;

      case kSigmaTemp:		  
	if (fModel == kLevy) {
	  tmp =paramScaler*partType->GetSigmaTemp();  
	  fCurrentForm1D->SetParameter("sigmaTemp", tmp);
	  evpars->SigmaTemp = tmp; // MSD
	}		  
	break;

      case kV1:
	directedScaller = paramScaler;
	evpars->V1Scalar = paramScaler; // MSD
	break;
		  
      case kV2:
	ellipticScaller = paramScaler;
	evpars->V2Scalar = paramScaler; // MSD
	break;

      case kMult:
	multiplicity = paramScaler * partType->GetMultiplicity();
	break;
      }  // switch
    }

    // Flow defined on the particle type level (not parameterised)
    if (partType->IsFlowSimple()) {
      fPhiFormula->SetParameter(1, partType->GetDirectedFlow(0,0) * directedScaller);
      fPhiFormula->SetParameter(2, partType->GetEllipticFlow(0,0) * ellipticScaller);
    }
	 
    AdjustFormula();
	 
    if (partType->IsMultForced()) isMultTotal = partType->IsMultTotal();
    else isMultTotal = fIsMultTotal;
    
    multiplicity *= (isMultTotal)? 1 : GetdNdYToTotal();
    evpars->Mult = (Int_t)multiplicity+1; // MSD

    if (fIsVerbose) {
      //Info("Generate","PDG = %d \t Mult = %d", pdg, (Int_t)multiplicity);
      printf("Generating PDG = %5d Mult = %d\n", pdg, (Int_t)multiplicity+1 );
    }
    
    // Store the particle parameters for this event  // MSD
    //   NOTE : I have come across a wierd error when adding this object to the array
    //   when I use the command:
    //   fEvent->evParams->Add(evpars);                   
    //   it overwrites every other element too.  I cannot understand why this happens.
    //   As a workaround, I can clone the object and add the clone; this works fine.
    TGeVSimParams *foo = (TGeVSimParams*)evpars->Clone("foo");  // This should NOT be necessary
    fEvent->evParams->Add(foo);    
    //fEvent->evParams->Add(evpars);                   
    evpars->Clear();  // Reset for next particle type
    
    // loop over particles
    points[npoint++] = times(0); 
    
    nParticle = 0;
    while (nParticle < multiplicity) {

      Double_t pt, y, phi;       // momentum in [pt,y,phi]
      Float_t p[3] = {0,0,0};    // particle momentum
      
      GetRandomPtY(pt, y);
      
      // phi distribution configuration when differential flow defined
      // to be optimised in future release 
      
      if (!partType->IsFlowSimple()) {
	fPhiFormula->SetParameter(1, partType->GetDirectedFlow(pt,y) * directedScaller);
	fPhiFormula->SetParameter(2, partType->GetEllipticFlow(pt,y) * ellipticScaller);
      }
      
      phi = fPhiFormula->GetRandom(); 
      
      if (!isMultTotal) nParticle++;
      if (!CheckPtYPhi(pt,y,phi)) continue;
      
      // coordinate transformation
      p[0] = pt * TMath::Cos(phi);
      p[1] = pt * TMath::Sin(phi);
      p[2] = TMath::Sqrt(mass*mass + pt*pt) * TMath::SinH(y); 
      
      // momentum range test
      if ( !CheckPXYZ(p) ) continue;
      
      // putting particle on the stack
      energy = TMath::Sqrt(mass*mass + p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);
      v->SetPxPyPzE(p[0], p[1], p[2], energy);
      
      if (clones) {
	new ((*fEvent->evParts)[nPartTotal]) TParticle(pdg, 0, -1, -1, -1, -1, *v, *orgin);
	new ((*fPartBuffer)[nPartTotal++])   TParticle(pdg, 0, -1, -1, -1, -1, *v, *orgin);
      }
      else
	fParticles->Add(new TParticle(pdg, 0, -1, -1, -1, -1, *v, *orgin));
      
      if (isMultTotal) nParticle++;
    } // loop over particles
    
    points[npoint++] = times(0);
  }  // loop over particle types
  
  // *** MSD ***
  // if (clones) 
  //fEvent->evParts = (TClonesArray*)fPartBuffer->Clone("evParts");  // Copy particles to event
  // ***********

  delete v;
  delete orgin;
  
  
  //for(Int_t i=1; i<npoint; i++) {
  ///	 if (!(i%3)) cout << endl;
  //	 cout << (points[i] - points[i-1]) << endl;
  //}
}

//////////////////////////////////////////////////////////////////////////////////
