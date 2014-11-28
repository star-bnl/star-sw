#ifndef TGenAcceptance_h
#define TGenAcceptance_h

#include "TGenerator.h"

class TLorentzVector;

class TGenAcceptance : public TGenerator {

 protected:
  
  // acceptance cuts
  
  Float_t fPtCutMin;          // Lower cut for transverse momentum
  Float_t fPtCutMax;          // Upper cut for transverse momentum
  
  Float_t fEtaCutMin;         // Lower cut for pseudo-rapidity
  Float_t fEtaCutMax;         // Upper cut for pseudo-rapidity
  
  Float_t fYCutMin;           // Lower cut for rapidity
  Float_t fYCutMax;           // Upper cut for rapidity
  
  Float_t fPCutMin;           // Lower cut for total momentum
  Float_t fPCutMax;           // Upper cut for total momentum

  //Float_t fThetaCutMin;       // Lower cut for theta angle
  //Float_t fThetaCutMax;       // Upper cut for theta angle
  
  Float_t fPhiCutMin;         // Lower cut for azimuthal angle (in rad)
  Float_t fPhiCutMax;         // Upper cut for azimuthal angle (in rad)
  
  enum { 
	 kPtRange     = BIT(10),
	 kEtaRange    = BIT(11),
	 kYRange      = BIT(12),
	 kPRange      = BIT(13),
	 kThetaRange  = BIT(14),
	 kPhiRange    = BIT(15)
  };

  Bool_t CheckCuts(TLorentzVector *v) const;

  Bool_t CheckPtYPhi(Float_t p[3]) const; 
  Bool_t CheckPtYPhi(Float_t pt, Float_t y, Float_t phi) const; 

  Bool_t CheckPXYZ(Float_t p[3]) const;
  Bool_t CheckPXYZ(Float_t px, Float_t py, Float_t pz) const;

  //Bool_t ChackCuts(TParticle *p);

 public:

  TGenAcceptance() {}
  TGenAcceptance(const char *name, const char *title = "Generator with Acceptance");

  virtual ~TGenAcceptance() {};

  //getters
  Float_t GetPtCutLow() const {return fPtCutMin;}
  Float_t GetPtCutHigh() const {return fPtCutMax;}
  
  Float_t GetEtaCutLow() const {return fEtaCutMin;}
  Float_t GetEtaCutHigh() const {return fEtaCutMax;}

  Float_t GetYCutLow() const {return fYCutMin;}
  Float_t GetYCutHigh() const {return fYCutMax;}
  
  //Float_t GetThetaCutLow() const {return fThetaCutMin;}
  //Float_t GetThetaCutHigh() const {return fThetaCutMax;}

  // setters
  void SetPtRange(Float_t lowPt, Float_t highPt);
  void SetEtaRange(Float_t lowEta, Float_t highEta);
  void SetMomentumRange(Float_t lowP, Float_t highP);
  void SetYRange(Float_t lowY, Float_t highY);
  void SetThetaRange(Float_t lowTheta, Float_t highTheta);
  void SetPhiRange(Float_t lowPhi, Float_t highPhi);
  
  ClassDef(TGenAcceptance,1) // Generator with Acceptance
	 
};


#endif
