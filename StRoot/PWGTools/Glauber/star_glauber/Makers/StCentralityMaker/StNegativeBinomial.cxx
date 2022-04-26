/******************************************************************************
 * $Id: StNegativeBinomial.cxx,v 1.2 2012/04/25 05:13:24 hmasui Exp $
 * $Log: StNegativeBinomial.cxx,v $
 * Revision 1.2  2012/04/25 05:13:24  hmasui
 * Use STAR logger. Take into account additional constant inefficiency by using the trigger bias parameter
 *
******************************************************************************/

#include <assert.h>

#include "TH1.h"
#include "TMath.h"
#include "TString.h"

#include "StMessMgr.h"

#include "StGlauberUtilities/StGlauberUtilities.h"
#include "StNegativeBinomial.h"

ClassImp(StNegativeBinomial)

  UInt_t StNegativeBinomial::mCounter = 0 ;

//____________________________________________________________________________________________________
StNegativeBinomial::StNegativeBinomial(const Double_t npp, const Double_t k, const Double_t x,
    const Double_t efficiency, const Double_t triggerbias, const Bool_t isConstEfficiency)
  : mEfficiency(efficiency), mTriggerBias(triggerbias), mNpp(npp), mK(k), mX(x), mIsConstEfficiency(isConstEfficiency)
{
  LOG_INFO << Form("StNegativeBinomial  (npp, k, x, eff, trig. bias) = (%1.3f, %1.3f, %1.3f, %1.3f, %1.3f)",
        mNpp, mK, mX, mEfficiency, mTriggerBias)
    << endm;

  if(mIsConstEfficiency){
    LOG_INFO << "StNegativeBinomial  Use constant efficiency" << endm;
  }
  else{
    LOG_INFO << "StNegativeBinomial  Use multiplicity dependent efficiency" << endm;
  }

  /// Initialize histogram
  mhNbd = 0 ;
  InitHistogram() ;
}

//____________________________________________________________________________________________________
StNegativeBinomial::~StNegativeBinomial()
{
  delete mhNbd ;
}

//____________________________________________________________________________________________________
void StNegativeBinomial::InitHistogram()
{
  const Int_t nbin = 100 ;

  if( mhNbd ){
    // Reset current stuffs
    mhNbd->Reset();
  }
  else{
    mhNbd = new TH1D(Form("mhNbd_%d", mCounter++), "", nbin, 0, nbin);
  }

  for(Int_t i=0;i<nbin;i++){
    mhNbd->SetBinContent(i+1, GetNegativeBinomial(i));
  }
}

//____________________________________________________________________________________________________
void StNegativeBinomial::SetParameters(const Double_t npp, const Double_t k, const Double_t x)
{
  mNpp = npp ;
  mK   = k ;

  if( x > 0.0 ){
    mX   = x ;
  }

  /// Initialize histogram
  InitHistogram() ;
}

//____________________________________________________________________________________________________
Double_t StNegativeBinomial::GetTwoComponentMultiplicity(const Double_t npart, const Double_t ncoll) const
{
  /// Remind 0.5. Need 2*npart in d+Au FTPC

  return (mX==0.0) ? npart : 0.5*(1.0-mX)*npart + mX*ncoll ;
}

//____________________________________________________________________________________________________
Int_t StNegativeBinomial::GetMultiplicity(const Double_t npart, const Double_t ncoll) const
{
  /// Get multiplicity from negative binomial distribution
  // Take into account trigger efficiency, multiplicity efficiency

  const Double_t nchPP      = GetTwoComponentMultiplicity(npart, ncoll) ;
  const Double_t nchSampled = nchPP ;

  // Sample multiplicity (including trigger bias)
  const Int_t nchInt = TMath::Nint(nchSampled);
  Int_t multIdeal = 0;
  for(Int_t ich=0; ich<nchInt; ich++){
    multIdeal += (Int_t)mhNbd->GetRandom();
  }

  // Multiplicity dependent efficiency for TPC
  const Double_t efficiency = (mIsConstEfficiency) ?  mEfficiency : GetEfficiency(multIdeal) ;
  //cout<<efficiency<<" ;"<<npart<<" ;"<<ncoll<<"   TEST "<<endl;

  Int_t mult = 0;
  for(Int_t im=0; im<2*multIdeal; im++){
    if( StGlauberUtilities::instance()->GetUniform2() <= efficiency ){
      mult++;
    }
  }

  if ( mTriggerBias == 1.0 ) return mult ;
  
  const Int_t nmult = mult ;
  mult = 0 ;
  for(Int_t im=0; im<nmult; im++){
    if( StGlauberUtilities::instance()->GetUniform() <= mTriggerBias ){
      mult++;
    }
  }

  return mult ;
}

//____________________________________________________________________________________________________
TH1* StNegativeBinomial::GetMultiplicity(const Double_t npart, const Double_t ncoll,
    const Double_t weight) const
{
  // Do not forget to delete histogram
  const Double_t nchPP = GetTwoComponentMultiplicity(npart, ncoll) ;
  const Int_t nch      = TMath::Nint(nchPP * mTriggerBias) ; // with trigger bias

  // Multiplicity dependent efficiency for TPC
//  const Double_t efficiency = (mUseTpc) ?  GetEfficiency(nch) : mEfficiency ;
//  const Double_t efficiency = mEfficiency ;
  const Double_t efficiency = (mIsConstEfficiency) ?  mEfficiency : GetEfficiency(nch) ;
  const Int_t nchSampled    = TMath::Nint(nch * efficiency) ;

  const Int_t nbin = 1000 ;
  TH1* h = new TH1D("hmultTmp", "", nbin, 0, static_cast<Double_t>(nbin));

  for(Int_t ix=0; ix<nbin; ix++){
    // Probability to obtain 'nchSampled' in 'ix'-th bin
    const Double_t probability = GetNegativeBinomial(ix, nchSampled) ;

    // To avoid nan
    if(probability>0.0 && probability<DBL_MAX){
      h->Fill(ix+0.5, probability*weight);
    }
  }

  return h ;
}

//____________________________________________________________________________________________________
Double_t StNegativeBinomial::GetNegativeBinomial(const Int_t n) const
{
  const Double_t Const = TMath::Exp( TMath::LnGamma(n+mK) - TMath::LnGamma(n+1) - TMath::LnGamma(mK) );
  const Double_t term  = n * TMath::Log(mNpp/mK) - (n+mK) * TMath::Log(mNpp/mK+1.0) ;
 
  return Const * TMath::Exp(term);
}

//____________________________________________________________________________________________________
Double_t StNegativeBinomial::GetNegativeBinomial(const Int_t n, const Double_t m) const
{
  // npp and k are scaled by m

  const Double_t Const = TMath::Exp( TMath::LnGamma(n+mK*m) - TMath::LnGamma(n+1) - TMath::LnGamma(mK*m) );
  const Double_t term  = n * TMath::Log(mNpp/mK) - (n+mK*m) * TMath::Log(mNpp/mK+1.0) ;
 
  return Const * TMath::Exp(term);
}

//______________________________________________________________________________________________________
Double_t StNegativeBinomial::GetEfficiency(const Int_t mult) const
{
  /// Check flag
  if( mIsConstEfficiency ){
    Error("StNegativeBinomial::GetEfficiency", "Something is wrong, supposed to be constant efficiency. abort");
    assert(0) ;
  }

  /// TPC efficiency, http://www.star.bnl.gov/protected/strange/atimmins/Glauber/page.html 
  if ( mult < 0 ) return mEfficiency ;

  // Multiplicity dependent efficiency correction (valid only for main TPC refmult)

 // const Double_t eff0 = 1.05 ; // 98 % efficiency at p+p
  const Double_t eff0 = 0.98 ; // 98 % efficiency at p+p
//  const Double_t d    = 0.14 ; // 14 % drop at most central Au + Au
  const Double_t d    = mEfficiency ;

  return eff0 * (1.0 - mult * d/540.0) ;
  //return eff0 * d * (1.0 - mult * 0.14/560.0) ;
}

//______________________________________________________________________________________________________
void StNegativeBinomial::DrawNbd() const
{
  if(mhNbd) mhNbd->Draw() ;
}

