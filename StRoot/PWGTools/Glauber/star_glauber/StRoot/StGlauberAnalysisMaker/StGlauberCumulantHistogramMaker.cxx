/******************************************************************************
 * $Id: StGlauberCumulantHistogramMaker.cxx,v 1.2 2012/04/25 05:03:31 hmasui Exp $
 * $Log: StGlauberCumulantHistogramMaker.cxx,v $
 * Revision 1.2  2012/04/25 05:03:31  hmasui
 * Use STAR logger
 *
******************************************************************************/

#include <assert.h>
#include <fstream>
#include <vector>

#include "StMessMgr.h"

#include "TH1.h"
#include "TMath.h"
#include "TProfile.h"

#include "StGlauberCumulantHistogramMaker.h"

ClassImp(StGlauberCumulantHistogramMaker)

using std::ofstream ;
using std::vector ;

//____________________________________________________________________________________________________
StGlauberCumulantHistogramMaker::StGlauberCumulantHistogramMaker(const TString name, const TString title, const TString ytitle,
    const Int_t ybin, const Double_t ymin, const Double_t ymax, const Bool_t isUnitWeight)
  : StGlauberHistogramMaker(name, title, ytitle, ybin, ymin, ymax, isUnitWeight)
{
  Init() ;
}

//____________________________________________________________________________________________________
// Default destructor
StGlauberCumulantHistogramMaker::~StGlauberCumulantHistogramMaker()
{
  Reset() ;
}

//____________________________________________________________________________________________________
void StGlauberCumulantHistogramMaker::Init()
{
  /// Initialize histograms in StGlauberHistogramMaker first

  /// Initialize histograms
  for(UInt_t io=0; io<mNOrder; io++){
    const UInt_t order = GetOrder(io);

    for(UInt_t ix=0; ix<GetNXaxis(); ix++){
      // 1D (will be used later to store Profile/Weight)
      TH1* h1D = (TH1D*) GetTH1D(Form("%s_%d", GetName().Data(), order), ix) ;
      mHistogram1DCumulant[io].push_back( (TH1D*) h1D );

      // Profile
      TProfile* hProfile = (TProfile*) GetTProfile(Form("%s_%d", GetName().Data(), order), ix, "Profile");
      mProfileCumulant[io].push_back( (TProfile*) hProfile );
    }

    // Print info.
    for(vector<TProfile*>::iterator iter = mProfileCumulant[io].begin();
        iter != mProfileCumulant[io].end(); iter++){
      TProfile* h = (TProfile*) (*iter);
      LOG_INFO << Form("StGlauberCumulantHistogramMaker::Init Initialize TProfile: %s (%s), x:(bin, min, max) = (%4d, %1.2f, %1.2f)", 
            h->GetName(), h->GetTitle(), h->GetNbinsX(), h->GetXaxis()->GetXmin(), h->GetXaxis()->GetXmax())
        << endm;
    }

  }// cumulant order loop

}

//____________________________________________________________________________________________________
void StGlauberCumulantHistogramMaker::Reset()
{
  StGlauberHistogramMaker::Reset() ;

  for(UInt_t io=0; io<mNOrder; io++){
    mProfileCumulant[io].clear() ;
    mHistogram1DCumulant[io].clear() ;
  }
}

//____________________________________________________________________________________________________
void StGlauberCumulantHistogramMaker::Fill(const Double_t y, const Double_t weight)
{
  /// Fill 'y' value with 'weight'
  StGlauberHistogramMaker::Fill(y, weight);

  /// Fill
  for(UInt_t io=0; io<mNOrder; io++){
    Double_t yval = y*y ;
    if( io == 1 ) yval = y*y*y*y ;
    if( io == 2 ) yval = y*y*y*y*y*y ;

    FillProfile(mProfileCumulant[io], yval*weight);
  }
}


//____________________________________________________________________________________________________
void StGlauberCumulantHistogramMaker::Finish(const TString type)
{
  /// Call StGlauberHistogramMaker::Finish() first
  StGlauberHistogramMaker::Finish(type) ;

  /// do weight correction
  for(UInt_t io=0; io<mNOrder; io++){
    DoWeightCorrection(mHistogram1DCumulant[io], mProfileCumulant[io]);
  }

  /// Calculate cumulant
  LOG_INFO << "StGlauberCumulantHistogramMaker::Finish Calculate cumulant" << endm;

  for(UInt_t ix=0; ix<mHistogram1DCumulant[0].size(); ix++){
    // Weight has already corrected, so this should contain val^{mOrder}
    TH1* h1raw2 = (TH1D*) mHistogram1DCumulant[0][ix]->Clone() ;
    TH1* h1raw4 = (TH1D*) mHistogram1DCumulant[1][ix]->Clone() ;
    TH1* h1raw6 = (TH1D*) mHistogram1DCumulant[2][ix]->Clone() ;

    for(Int_t i=0; i<h1raw2->GetNbinsX(); i++){
      const Double_t val[] = { h1raw2->GetBinContent(i+1), h1raw4->GetBinContent(i+1), h1raw6->GetBinContent(i+1) } ;
      const Double_t err[] = { h1raw2->GetBinError(i+1), h1raw4->GetBinError(i+1), h1raw6->GetBinError(i+1) } ;

      for(Int_t io=0; io<mNOrder; io++){
        const UInt_t order = GetOrder(io) ;
        mHistogram1DCumulant[io][ix]->SetBinContent(i+1, GetCumulant(order, val));
        mHistogram1DCumulant[io][ix]->SetBinError(i+1, GetCumulantError(order, val, err));
      }// cumulant order loop
    }

    delete h1raw2 ;
    delete h1raw4 ;
    delete h1raw6 ;
  }// x-axis loop

  // Write table
  for(Int_t io=0; io<mNOrder; io++){
    const TString name(type + "_" + GetName()) ;
    WriteTable(mHistogram1DCumulant[io], Form("%s_%d", name.Data(), GetOrder(io))) ;
  }

  // Write graph
  for(Int_t io=0; io<mNOrder; io++){
    WriteGraphs(mHistogram1DCumulant[io]);
  }
}

//____________________________________________________________________________________________________
UInt_t StGlauberCumulantHistogramMaker::GetOrder(const UInt_t io) const
{
  return (io+1)*2 ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::Get4thOrderCumulant(const Double_t c2, const Double_t c4) const
{
  const Double_t raw  = 2.0*c2*c2 - c4 ;
  const Double_t sign = (raw>0.0) ? 1.0 : -1.0 ;

  return TMath::Power(sign*raw, 0.25) ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::Get6thOrderCumulant(const Double_t c2, const Double_t c4, const Double_t c6) const
{
  const Double_t raw  = 0.25*(c6 - 9.0*c4*c2 + 12.0*c2*c2*c2) ;
  const Double_t sign = (raw>0.0) ? 1.0 : -1.0 ;

  return TMath::Power(sign*raw, 1.0/6.0) ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::GetNthOrderCumulantError(const Double_t order, const Double_t val, const Double_t err) const
{
  if( val == 0.0 ) return 0.0 ;

  return TMath::Abs(order) * TMath::Power(TMath::Abs(val), order-1.0) * err ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::Get2ndOrderCumulantError(const Double_t c2, const Double_t c2error) const
{
  return GetNthOrderCumulantError(0.5, c2, c2error) ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::Get4thOrderCumulantError(const Double_t c2, const Double_t c4,
    const Double_t c2error, const Double_t c4error) const
{
  const Double_t cumulant = Get4thOrderCumulant(c2, c4) ;
  const Double_t error    = TMath::Sqrt(16.0*c2*c2*c2error*c2error + c4error*c4error) ;

  return GetNthOrderCumulantError(0.25, cumulant, error) ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::Get6thOrderCumulantError(const Double_t c2, const Double_t c4, const Double_t c6,
    const Double_t c2error, const Double_t c4error, const Double_t c6error) const
{
  const Double_t cumulant = Get6thOrderCumulant(c2, c4, c6) ;
  const Double_t error1   = (c4!=0.0 && c2!=0.0) ? 
    9.0 * TMath::Abs(c4*c2) * TMath::Sqrt(TMath::Power(c4error/c4, 2.0) + TMath::Power(c2error/c2, 2.0)) : 0.0;
  const Double_t error2   = 12.0 * 3.0 * c2*c2*c2error ;
  const Double_t error    = 0.25 * TMath::Sqrt(c6error*c6error + error1*error1 + error2*error2) ;

  return GetNthOrderCumulantError(1.0/6.0, cumulant, error) ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::GetCumulant(const UInt_t order, const Double_t* val) const
{
  /// val array should contain val[] = {c2, c4, c6, ...}
  if(!val) return -9999. ;

  const Double_t invalid = -9999. ;

  switch ( order ){
    case 2: // 2nd order cumulant
      return (val[0]<0.0) ? invalid : TMath::Sqrt(val[0]) ;

    case 4: // 4th order cumulant
      return Get4thOrderCumulant(val[0], val[1]) ;

    case 6: // 6th order cumulant
      return Get6thOrderCumulant(val[0], val[1], val[2]) ;

    default:
      Error("StGlauberCumulantHistogramMaker::GetCumulant", "Unknown order, order=%3d. abort", order);
      assert(0);
  }

  return -9999. ;
}

//____________________________________________________________________________________________________
Double_t StGlauberCumulantHistogramMaker::GetCumulantError(const UInt_t order, const Double_t* val, const Double_t* err) const
{
  /// val and err array should contain val[] = {c2, c4, c6, ...}, err[] = {c2error, c4error, c6error, ...}
  if(!val) return -9999. ;
  if(!err) return -9999. ;

  switch ( order ){
    case 2: // 2nd order cumulant
      return Get2ndOrderCumulantError(val[0], err[0]);

    case 4: // 4th order cumulant
      return Get4thOrderCumulantError(val[0], val[1], err[0], err[1]) ;

    case 6: // 6th order cumulant
      return Get6thOrderCumulantError(val[0], val[1], val[2], err[0], err[1], err[2]) ;

    default:
      Error("StGlauberCumulantHistogramMaker::GetCumulantError", "Unknown order, order=%3d. abort", order);
      assert(0);
  }

  return -9999. ;
}


