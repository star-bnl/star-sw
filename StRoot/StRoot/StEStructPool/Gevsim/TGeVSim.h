#ifndef TGeVSim_h
#define TGeVSim_h

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
// Event by Eveny
// 
// GeVSim have extended capabilities on Event-by-Event fluctuations.
// Those are defined bu named formula. Refer to method FindScaler for details.
//
// MACROS:
// GeVSim event generator is accompanied be a set of macros.
//
// testGeVSim.C       : a place to start
// testGeVSimCut.C    : acceptance cuts
// testGeVSimPtEta.C  : test implemented Pt and pseud-rapidity distributions
// testGeVSimEbyE.C   : multiplicity fluctuations
// testGeVSimScan.C   : multiplicity scan
//
//
// Sylwester Radomski, mail: S.Radomski@gsi.de
// GSI, Dec 12, 2002
//
////////////////////////////////////////////////////////////////////////////////

class TFormula;
class TF1;
class TF2;
class TH1D;
class TH2D;
class TObjArray;
class TClonesArray;
class TGeVSimParticle;
class TGeVSimEvent;  // MSD

#include "TGenAcceptance.h"
#include "TGeVSimEvent.h"

class TGeVSim : public TGenAcceptance {

 public:

  enum Model_t  {
	 
	 kBoltzman   = 1,
	 kLevy       = 2,

	 kPratt      = 3,
	 kBertsch    = 4,
	 kExpansion  = 5,
	 
	 /* kFormula1D  = 10, */
	 kFormula2D  = 11,
	 kHist1D     = 12,
	 kHist2D     = 13
	 /* kFunction   = 14 */
  };
  
  enum Param_t {kTemp, kSigmaY, kExpVel, kSigmaTemp, kV1, kV2, kMult};

  TGeVSim();
  TGeVSim(const char *name);
  TGeVSim(const char *name, Float_t psi, Bool_t isMultTotal = kTRUE);
  
  virtual ~TGeVSim();
  
  static TGeVSim* GetDefault();
 
  /////////////////////////////////////////////////////////////////
  
  void AddParticleType(TGeVSimParticle *part);
  void SetMultTotal(Bool_t isTotal = kTRUE);

  TObjArray* ImportParticles(Option_t *option);
  Int_t ImportParticles(TClonesArray *particles, Option_t *option); 
  
  void GenerateEvent();
  TClonesArray* GetListOfParticles() const {return fPartBuffer;}

  void SetVerbose(Bool_t verb) {fIsVerbose = verb;}
  Bool_t IsVerbose() const {return fIsVerbose;}
  
  void Print(Option_t* option="") const;  // argument to remove compiler warning about overloaded virtual functions 

  TGeVSimEvent* GetCurrentEvent() const {return fEvent;}  // MSD

  /////////////////////////////////////////////////////////////////
  
 private:
  
  Model_t fModel;            // Selected model
  Float_t fPsi;              // Reaction Plane angle (0-2pi)
  Bool_t  fIsMultTotal;      // Mode od multiplicity: total, dN/dY
  
  Int_t fEventNumber;        // Current event number

  TF1 *fPtFormula[2];        //! Pt formula for model (Blz) - (Levy)
  TF2 *fPtYFormula[3];       //! Pt,Y formulae for model (pratt)-(expansion)
  TF1 *fPhiFormula;          //! phi formula 

  Float_t fSigmaY;           //! width of the rapidity for model (Blz) and (L)
    
  TF1 *fCurrentForm1D;       //!
  TF2 *fCurrentForm2D;       //! currently used formula 2D

  TH1D *fHist[2];            //! two 1D histograms (fModel == 6)
  TH2D *fPtYHist;            //! two-dimensional histogram (fModel == 7)

  TObjArray *fPartTypes;     // Registered particles
  TClonesArray *fPartBuffer; // ! temporary array;
  
  Bool_t fIsVerbose;         // 
  
  TGeVSimEvent *fEvent;        // MSD

  void InitFormula();
  void SetFormula(Int_t pdg);
  void AdjustFormula();
  void DetermineReactionPlane();
  void GetRandomPtY(Double_t &pt, Double_t &y);
  void Generate(Bool_t clones);

  Float_t GetdNdYToTotal();
  Float_t FindScaler(Int_t paramId, Int_t pdg);
  
  /////////////////////////////////////////////////////////////////

 public:

  ClassDef(TGeVSim, 2)

};

#endif
