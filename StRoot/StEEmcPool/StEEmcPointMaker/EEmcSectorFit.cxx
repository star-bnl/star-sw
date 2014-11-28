/**
 * \class EEmcSectorFit
 * \brief Simultaneous fit of two smd planes to N gammas
 *
 * This class implements a TMinuit-based fitting algorithm which
 * performs a simultaneous fit of N gamma candidates in 2 SMD 
 * planes.
 *
 * \author Jason C. Webb
 * $Date: 2010/08/26 22:49:26 $
 * $Revision: 1.5 $
 *
 */

#include <cassert>

#include "EEmcSectorFit.h"
#include "eeSinglePeak.h"
#include "TH1F.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TString.h"
#include <iostream>
#include <algorithm>
#include "TLine.h"
#include "TList.h"
ClassImp(EEmcSectorFit);

// ----------------------------------------------------------------------------
// default constructor has enough parameters for 10 gammas per plane
EEmcSectorFit::EEmcSectorFit(Int_t max) : TMinuit(4*max)
{
  fLwarn=false; 
  SetPrintLevel(-1); 
  Command("SET ERR 1");
  mSMD[0]=0;
  mSMD[1]=0;
  doPermutations = true;
  mNDF=0;
  mChi2=0.; 
}


EEmcSectorFit::~EEmcSectorFit()
{
}

// ----------------------------------------------------------------------------
Double_t EEmcSectorFit::FitFunc( Double_t x, Int_t plane ) const
{
  // loop over all gammas
  Double_t sum = 0.;
  for ( UInt_t i=0;i<yield.size();i++ )
    {
      Double_t a=yield[i];
      Double_t s=sigma[i];
      Double_t m=(plane==0)?umean[i]:vmean[i];
      Double_t X[]={x,0.,0.};
      Double_t P[]={a,m,s,0.2,3.5}; // norm, mean, sigma, frac2, rel width
      sum += eeSinglePeak(X,P);
    }
  return sum;
}
// ----------------------------------------------------------------------------
Int_t EEmcSectorFit::Eval(Int_t np,Double_t* gr,Double_t& chi2,Double_t* par,Int_t flg)
{
 
  // initialize chi2 to zero
  Double_t mychi2 = 0.;
  mChi2 = 0.;
  mNDF  = 0; 

  // pass paramerters to the arrays containing parameters
  for ( UInt_t i=0;i<yield.size();i++ )
    {
      yield[i] = par[4*i+0];
      sigma[i] = par[4*i+1];
      umean[i] = par[4*i+2];
      vmean[i] = par[4*i+3];
    }

  // loop over both planes
  for ( Int_t plane=0;plane<2; plane++ )
    {

      // get histogram for this plane
      TH1F *histo = mSMD[plane];
      // loop over all smd strips
      for ( Int_t strip=0;strip<288;strip++ )
	{
	  // get n mips in this plane/strip
	  Double_t nmip = histo -> GetBinContent(strip+1);
	  // evaluate the fit function
	  Double_t x    = (Double_t)strip + 0.5;
	  Double_t f    = FitFunc(x, plane);
	  // add to chi2
	  if ( nmip > 0. ) {
	    mychi2 += (nmip-f)*(nmip-f)/nmip;
	    mNDF++; 
	  }
	  // kludge to count 0's near mean against chi2
	  // need to go to a max likelyhood method
	  else if ( f > 0.499 ) {
	    mNDF++; 
	    mychi2 += (nmip-f)*(nmip-f)/f;
	  }
	}
    }

  chi2 = mychi2;
  mChi2 = mychi2;
  mNDF -= 4*(int)yield.size(); 

  return 0;

}
// ----------------------------------------------------------------------------
Double_t EEmcSectorFit::Residual( Int_t x, Int_t plane ) const
{
  // zero residual if x is w/in +/- 2 strips of another gamma
  for ( UInt_t i=0;i<yield.size();i++ )
    {
      Double_t xx=(plane==0)?umean[i]:vmean[i];
      if ( TMath::Abs(xx-(Double_t)x-0.5) <= 1.0 ) return 0.;
    }
  TH1F *histo=mSMD[plane];
  Int_t bin = x + 1;  
  Double_t r = histo->GetBinContent(bin) - FitFunc( (Double_t)x+0.5, plane );
  return r;
}
// ----------------------------------------------------------------------------
Double_t EEmcSectorFit::Residual( Int_t x, Int_t plane, Int_t dx, Int_t side ) const
{


  Int_t min;//=TMath::Max(x-dx,0);
  Int_t max;//=TMath::Min(x+dx,288);

  
  min=TMath::Max(x-dx,0);
  max=TMath::Min(x+dx,288);
  
  if ( side==1 ) 
    {
      min=TMath::Max(x,0);
    }
  else if ( side==2 )
    {
      max=TMath::Min(x,288);
    }

    
  Double_t r=0.; 
  for ( Int_t xx=min;xx<=max;xx++ )
    {
      r += Residual( xx, plane );
    }
  return r;
} 
// ----------------------------------------------------------------------------
Int_t EEmcSectorFit::MaxStrip(Int_t plane) const
{
  Double_t max=0.;
  Int_t imax=-1;
  for ( Int_t i=0;i<288;i++ )
    {
      Double_t r=Residual(i,plane);
      if ( r>max )
	{
	  max=r;
	  imax=i;
	}
    }
  return imax;
}
// ----------------------------------------------------------------------------
void EEmcSectorFit::AddCandidate(Double_t y, Double_t s, Double_t u, Double_t v)
{
  //  std::cout << "Adding candidate" << std::endl;
  yield.push_back(y);
  sigma.push_back(s);
  umean.push_back(u);
  vmean.push_back(v);
#if 0
  TLine *lU = new TLine(u,0.,u,y);lU->SetLineColor(2);
  TLine *lV = new TLine(v,0.,v,y);lV->SetLineColor(2);
  mSMD[0]->GetListOfFunctions()->Add(lU);
  mSMD[1]->GetListOfFunctions()->Add(lV);
#endif
  /// initialize parameters
  InitParameters();
}
// ----------------------------------------------------------------------------
// initialzes parameters for the N-gamma fit
void EEmcSectorFit::InitParameters()
{
  Int_t flg=0;
  Command("CLEAR");
  Command("SET ERR 1");
  for ( Int_t i=0;i<(Int_t)yield.size();i++ )
    {
      mnparm(4*i+0, TString("yield"), yield[i], 0.1, 0.25*yield[i], 2.0*yield[i], flg );
      mnparm(4*i+1, TString("sigma"), sigma[i], 0.1, 0.5, 2.5,          flg );
      mnparm(4*i+2, TString("umean"), umean[i], 0.1, 0.0, 288.0,        flg );
      mnparm(4*i+3, TString("vmean"), vmean[i], 0.1, 0.0, 288.0,        flg );
    }
}
// ----------------------------------------------------------------------------
void EEmcSectorFit::Draw(Option_t *opts)
{
  //  TCanvas *canvas = new TCanvas("c1","c1",800,400);
  /*
  TPad *canvas=gPad;
  if ( !gPad ) {
    gPad=(TPad *)new TCanvas("c1","c1",500,500);
    gPad->Divide(2);
    canvas=gPad;
  }
  */
  //  canvas->Divide(2);

  /*
  mSMD[0]->GetListOfFunctions()->Delete();
  mSMD[1]->GetListOfFunctions()->Delete();
  */

  assert(mSMD[0]);
  assert(mSMD[1]);

  Double_t umin=999.;
  Double_t vmin=888.;
  Double_t umax=-1.;
  Double_t vmax=-1.;

  /// add fits to histograms
  for ( UInt_t i=0;i<yield.size();i++ )
    {
      TString uname="fitu";uname+=i;      
      TString vname="fitv";vname+=i;
      TF1 *fitu=new TF1(uname,eeSinglePeak,0.,288.,5);
      TF1 *fitv=new TF1(vname,eeSinglePeak,0.,288.,5);
      fitu->SetLineColor(4);
      fitv->SetLineColor(4);
      fitu->SetLineWidth(1);
      fitv->SetLineWidth(1);
      fitu->SetParameter(0, yield[i] );
      fitu->SetParameter(1, umean[i] );
      fitu->SetParameter(2, sigma[i] );
      fitu->SetParameter(3, 0.2 );
      fitu->SetParameter(4, 3.5 );
      fitv->SetParameter(0, yield[i] );
      fitv->SetParameter(1, vmean[i] );
      fitv->SetParameter(2, sigma[i] );
      fitv->SetParameter(3, 0.2 );
      fitv->SetParameter(4, 3.5 );
      mSMD[0]->GetListOfFunctions()->Add( fitu );
      mSMD[1]->GetListOfFunctions()->Add( fitv );
      umin=TMath::Min( fitu->GetParameter(1), umin );
      umax=TMath::Max( fitu->GetParameter(1), umax );
      vmin=TMath::Min( fitv->GetParameter(1), vmin );
      vmax=TMath::Max( fitv->GetParameter(1), vmax );      
    }

  umin=TMath::Max(umin,1.0);
  umax=TMath::Min(umax,287.0);
  vmin=TMath::Max(vmin,1.0);
  vmax=TMath::Min(vmax,287.0);

  if ( umin < umax )
    mSMD[0]->GetXaxis()->SetRangeUser(umin-20.,umax+20.);
  if ( vmin < vmax )
    mSMD[1]->GetXaxis()->SetRangeUser(vmin-20.,vmax+20.);


  //  canvas->cd(1);
  //  mSMD[0]->Draw(opts);
  //  canvas->cd(2);
  //  mSMD[1]->Draw(opts);

}
// ----------------------------------------------------------------------------
void EEmcSectorFit::TryPermutations()
{

  if ( !doPermutations ) return;
  
  std::vector<Double_t> chi2vec;

  /// Vectors of our fit parameters
  std::vector< std::vector<Double_t> > yields;
  std::vector< std::vector<Double_t> > sigmas;
  std::vector< std::vector<Double_t> > umeans;
  std::vector< std::vector<Double_t> > vmeans;
  std::vector< Double_t > mychi2;
  
  std::vector< Double_t > old_yield = yield;
  std::vector< Double_t > old_sigma = sigma;
  std::vector< Double_t > old_umean = umean;
  std::vector< Double_t > old_vmean = vmean;

  /// Vector of resulting chi^2
  while ( std::next_permutation( old_vmean.begin(), old_vmean.end() ) )
    {

      /// Permute on old_vmean
      vmean = old_vmean;
      
      /// Initialze params for this permutaion
      InitParameters();

      /// Fix the widths
      for ( UInt_t i=0;i<sigma.size();i++ ) {
	FixParameter(4*i+1);
      }
      /// Now fit relative to this permutation
      Migrad();
      /// Save parameters and record chi2
      mychi2.push_back( mChi2 );
      yields.push_back( yield );
      sigmas.push_back( sigma );
      umeans.push_back( umean );
      vmeans.push_back( vmean );
      /// restore to original yield, sigma, umean
      yield = old_yield;
      sigma = old_sigma;
      umean = old_umean;

    }

    /// find minimum chi^2, restore and fit
    Double_t min = 9.0E12;
    for ( UInt_t i=0;i<mychi2.size();i++ )
      {
	if ( mychi2[i]< min )
	  {
	    min=mychi2[i];
	    yield=yields[i];
	    sigma=sigmas[i];
	    umean=umeans[i];
	    vmean=vmeans[i];
	  }
      }

    /// Refit to ensure minimum chi^2 is loaded into the fitter
    InitParameters();
    /// Free the widths
    for ( UInt_t i=0;i<sigma.size();i++ ) {
      Release(4*i+1);
    }
    Migrad();
    
}
// ----------------------------------------------------------------------------
void EEmcSectorFit::print() const
{
    std::cout << "doPermutations=" << doPermutations << std::endl;
    std::cout << "chi^2         =" << mChi2 << std::endl; 
    std::cout << "ndf           =" << mNDF << std::endl; 
}
// ----------------------------------------------------------------------------
void EEmcSectorFit::Clear(Option_t *opts)
{
  yield.clear();
  sigma.clear();
  umean.clear();
  vmean.clear();
  Command("CLEAR");
}
// ----------------------------------------------------------------------------
void EEmcSectorFit::AddFits( TH1F *uu, TH1F *vv )
{

  assert(uu);
  assert(vv);

  Double_t umin=999.;
  Double_t vmin=888.;
  Double_t umax=-1.;
  Double_t vmax=-1.;

  /// add fits to histograms
  for ( UInt_t i=0;i<yield.size();i++ )
    {
      TString uname="fitu";uname+=i;      
      TString vname="fitv";vname+=i;
      TF1 *fitu=new TF1(uname,eeSinglePeak,0.,288.,5);
      TF1 *fitv=new TF1(vname,eeSinglePeak,0.,288.,5);
      fitu->SetLineColor(4);
      fitv->SetLineColor(4);
      fitu->SetLineWidth(1);
      fitv->SetLineWidth(1);
      fitu->SetParameter(0, yield[i] );
      fitu->SetParameter(1, umean[i] );
      fitu->SetParameter(2, sigma[i] );
      fitu->SetParameter(3, 0.2 );
      fitu->SetParameter(4, 3.5 );
      fitv->SetParameter(0, yield[i] );
      fitv->SetParameter(1, vmean[i] );
      fitv->SetParameter(2, sigma[i] );
      fitv->SetParameter(3, 0.2 );
      fitv->SetParameter(4, 3.5 );
      uu->GetListOfFunctions()->Add( fitu );
      vv->GetListOfFunctions()->Add( fitv );
      umin=TMath::Min( fitu->GetParameter(1), umin );
      umax=TMath::Max( fitu->GetParameter(1), umax );
      vmin=TMath::Min( fitv->GetParameter(1), vmin );
      vmax=TMath::Max( fitv->GetParameter(1), vmax );      
    }

  umin=TMath::Max(umin,1.0);
  umax=TMath::Min(umax,287.0);
  vmin=TMath::Max(vmin,1.0);
  vmax=TMath::Min(vmax,287.0);

  if ( umin < umax )
    uu->GetXaxis()->SetRangeUser(umin-20.,umax+20.);
  if ( vmin < vmax )
    vv->GetXaxis()->SetRangeUser(vmin-20.,vmax+20.);


}
