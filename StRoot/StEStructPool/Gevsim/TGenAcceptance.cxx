
#include "TGenAcceptance.h"
#include "TLorentzVector.h"

ClassImp(TGenAcceptance);

////////////////////////////////////////////////////////////////////////////////

TGenAcceptance::TGenAcceptance(const char *name, const char *title)
  :TGenerator(name, title)
{
  //
  // Standard Constructor
  //

  // Reset Bits
  
  ResetBit(kPtRange);
  ResetBit(kEtaRange);
  ResetBit(kYRange);
  ResetBit(kPRange);
  ResetBit(kThetaRange);
  ResetBit(kPhiRange);
}

////////////////////////////////////////////////////////////////////////////////
Bool_t TGenAcceptance::CheckPtYPhi(Float_t p[3]) const
{
  //
  //
  //

  return CheckPtYPhi(p[0], p[1], p[2]); 
}

////////////////////////////////////////////////////////////////////////////////

Bool_t  TGenAcceptance::CheckPtYPhi(Float_t pt, Float_t y, Float_t phi) const
{
  //
  // Check bounds of Pt, Rapidity and Azimuthal Angle of a track
  //

  if ( TestBit(kPtRange) && ( pt < fPtCutMin || pt > fPtCutMax )) return kFALSE;
  if ( TestBit(kPhiRange) && ( phi < fPhiCutMin || phi > fPhiCutMax )) return kFALSE;
  if ( TestBit(kYRange) && ( y < fYCutMin || y > fYCutMax )) return kFALSE;

  return kTRUE;
} 

////////////////////////////////////////////////////////////////////////////////

Bool_t  TGenAcceptance::CheckPXYZ(Float_t p[3]) const
{
  //
  //
  //

  return CheckPXYZ(p[0], p[1], p[2]);
}

////////////////////////////////////////////////////////////////////////////////

Bool_t  TGenAcceptance::CheckPXYZ(Float_t px, Float_t py, Float_t pz) const
{
  //
  // Check bounds of a total momentum, pseudo-rapidity and theta of a track
  //


  //if ( TestBit(kThetaRange) ) {
  //  
  //  theta = TMath::ATan2( TMath::Sqrt(px*px + py*py), pz);
  //  if ( theta < fThetaCutMin || theta > fThetaCutMax ) return kFALSE;
  //}

  if ( TestBit(kEtaRange)) {

	 Float_t theta = TMath::ATan2( TMath::Sqrt(px*px + py*py), pz);
	 Double_t eta = - TMath::Log(TMath::Tan(theta/2));	 
	 if ( eta < fEtaCutMin || eta > fEtaCutMax) return kFALSE;
  }

  if ( TestBit(kPRange) ) {
    
    Double_t p2 = px*px + py*py + pz*pz;
    if ( p2 < fPCutMin*fPCutMin || p2 > fPCutMax*fPCutMax) return kFALSE;
  }

  return kTRUE;
}

////////////////////////////////////////////////////////////////////////////////

Bool_t TGenAcceptance::CheckCuts(TLorentzVector *v) const
{
  //
  //
  //

  if (TestBit(kPtRange) && ( v->Pt() < fPtCutMin || v->Pt() > fPtCutMax )) return kFALSE;  
  if (TestBit(kEtaRange) && (v->Eta() < fEtaCutMin || v->Eta() > fEtaCutMax)) return kFALSE;
  if (TestBit(kYRange) && (v->Rapidity() < fYCutMin || v->Rapidity() > fYCutMax)) return kFALSE;
  if (TestBit(kPRange) && (v->P() < fPCutMin || v->P() > fPCutMax)) return kFALSE;
  //if (TestBit(kThetaRange) && (v->Theta() < fThetaCutMin || v->Theta() > fThetaCutMax)) return kFALSE;
  if (TestBit(kPhiRange) && (v->Phi() < fPhiCutMin || v->Phi() > fPhiCutMax)) return kFALSE;

  return kTRUE;
}

////////////////////////////////////////////////////////////////////////////////

void TGenAcceptance::SetPtRange(Float_t lowPt, Float_t highPt)
{
  //
  //
  
  const char *fName = "SetPtRange";
  const char *msg[2] = {
	 "%s transverse momentum [%f GeV/c] negative",
	 "Minimum transverse momentum greater than Maximum"
  };

  if (lowPt < 0) Error(fName, msg[0], "Minimum", lowPt);
  if (highPt < 0) Error(fName, msg[0], "Maximum", highPt);
  if (lowPt >= highPt) Error(fName, msg[1]);

  SetBit(kPtRange);
  fPtCutMin = lowPt;
  fPtCutMax = highPt; 
}

////////////////////////////////////////////////////////////////////////////////

void TGenAcceptance::SetEtaRange(Float_t lowEta, Float_t highEta)
{
  //
  //

  const char *fName = "SetEtaRange";
  const char *msg = "Minimum Eta greater than Maximum";
  
  if (lowEta >= highEta) Error(fName, msg);
  
  SetBit(kEtaRange);
  fEtaCutMin = lowEta;
  fEtaCutMax = highEta;
}

////////////////////////////////////////////////////////////////////////////////

void TGenAcceptance::SetYRange(Float_t lowY, Float_t highY)
{
  //
  //

  const char *fName = "SetYRange";
  const char *msg = "Minumum Rapidity greater that Maximum";

  if (lowY >= highY) Error(fName, msg);
  
  SetBit(kYRange);
  fYCutMin = lowY;
  fYCutMax = highY;
}

////////////////////////////////////////////////////////////////////////////////

void TGenAcceptance::SetThetaRange(Float_t lowTheta, Float_t highTheta)
{
  //
  //
  
  const char *fName = "SetThetaRange";
  const char *msg[2] = {
	 "%s Theta [ %f deg] out of scope 0-180 deg.",
	 "Minumum Theta greater than Maximum Theta"
  };

  if (lowTheta < 0 || lowTheta > 180) Error(fName, msg[0], "Minimum", lowTheta);
  if (highTheta < 0 || highTheta > 180) Error(fName, msg[0], "Maximum", lowTheta);
  if (lowTheta >= highTheta) Error(fName,msg[1]);

  Float_t lowEta = - TMath::Log(TMath::Tan(lowTheta/2));
  Float_t highEta = - TMath::Log(TMath::Tan(highTheta/2));
 
  SetEtaRange(lowEta, highEta);
  
  //SetBit(kThetaRange);
  //fThetaCutMin = lowTheta * TMath::Pi() / 180;
  //fThetaCutMax = highTheta  * TMath::Pi() / 180;
}

////////////////////////////////////////////////////////////////////////////////

void TGenAcceptance::SetPhiRange(Float_t lowPhi, Float_t highPhi)
{
  //
  //
  
  SetBit(kPhiRange);
  fPhiCutMin = lowPhi * TMath::Pi() / 180;
  fPhiCutMax = highPhi * TMath::Pi() / 180;
}

////////////////////////////////////////////////////////////////////////////////
void TGenAcceptance::SetMomentumRange(Float_t lowP, Float_t highP)
{
  //
  //
  //
  
  const char *fName = "SetMomentumRange";
  const char *msg = "Minimum momentum greater that Maximum";

  if (lowP > highP) Error(fName, msg);

  SetBit(kPRange);
  fPCutMin = lowP;
  fPCutMax = highP;
}


////////////////////////////////////////////////////////////////////////////////
