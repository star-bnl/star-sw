/**********************************************************************
* StEmcMipSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for MIP Calibration
***********************************************************************/
#include "StEmcMipSpectra.h"
#include <iostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraphErrors.h"
#include "TMinuit.h"
#include "StEmcFit.h"

ClassImp(StEmcMipSpectra);
 
//_____________________________________________________________________________
//These functions are for TMinuit MIP fit
//_____________________________________________________________________________
Double_t Gaussian(Double_t x,Double_t A,Double_t xavg,Double_t sigma)
{
  Double_t ex,arg;
  arg=(x-xavg)/sigma;
  ex=exp(-arg*arg);
  return A*ex;
}
//_____________________________________________________________________________
//The member functions start here
//_____________________________________________________________________________
StEmcMipSpectra::StEmcMipSpectra(const char* cdet):StEmcSpectra(cdet)
{      
}
//_____________________________________________________________________________
StEmcMipSpectra::~StEmcMipSpectra()
{ }
//_____________________________________________________________________________
void StEmcMipSpectra::DrawEtaBin(Int_t etabin)
{
  if(etabin>nEtaBins || etabin<1) return;
  Int_t nadcMax=GetNAdcMax();

  TCanvas* canvas7=new TCanvas("canvas7","EMC Eta Bin Spectrum",500,350);
  
  TH1F* hist=new TH1F("hist","Eta Bin Equalization Spectrum",nadcMax,0,(Float_t)nadcMax-1);

  TArrayF temp=GetEtaBinSpectra(etabin);
  for(Int_t j=0;j<nadcMax;j++) hist->Fill(j,temp[j]);
  
  canvas7->cd();
  hist->Draw();
  emcMipCalib_st* mip=MipTable->GetTable();

  if (mip[etabin-1].Status==1)
  {
    Int_t fitmin=mip[etabin-1].MipFitAdcMin;
    Int_t fitmax=mip[etabin-1].MipFitAdcMax;
    Int_t df=fitmax-fitmin;
    
    TH1F* fit=new TH1F("fit","",df,(Float_t)fitmin,(Float_t)fitmax-1);
    TH1F* fitpeak=new TH1F("fitpeak","",df,(Float_t)fitmin,(Float_t)fitmax-1);
    TH1F* fitback=new TH1F("fitback","",df,(Float_t)fitmin,(Float_t)fitmax-1);
    
    for(Int_t adc=mip[etabin].MipFitAdcMin;adc<mip[etabin].MipFitAdcMax;adc++)
    {
      Double_t gauss1=Gaussian((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[0],
                               (Double_t)mip[etabin-1].MipFitParam[1],
                               (Double_t)mip[etabin-1].MipFitParam[2]);
      Double_t gauss2=Gaussian((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[3],
                               (Double_t)mip[etabin-1].MipFitParam[4],
                              (Double_t)mip[etabin-1].MipFitParam[5]);
//      Double_t gauss2=Pol((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[4],
//                               (Double_t)mip[etabin-1].MipFitParam[5],
//                               (Double_t)mip[etabin-1].MipFitParam[6]);
      fit->Fill((Float_t)adc,gauss1+gauss2);
      fitpeak->Fill((Float_t)adc,gauss1);
      fitback->Fill((Float_t)adc,gauss2);
    }
    fit->SetLineColor(4);
    fitpeak->SetLineColor(2); 
    fitback->SetLineColor(3);
    fit->Draw("sameC");
    fitpeak->Draw("sameC");
    fitback->Draw("sameC");
  }
  return;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateEtaBin(Int_t etabin,Int_t mode)
{
  if(etabin<0 || etabin>GetNEtaBin()) return kFALSE;
  
  if (mode==0) 
  {
    TArrayF SpectraTemp=GetEtaBinSpectra(etabin);            
    Int_t nadcMax=GetNAdcMax();    
    if(CalibrateByMip(etabin,SpectraTemp,0,nadcMax)) return kTRUE;
  }
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateBin(Int_t bin,Int_t mode)
{
  if(bin<1 || bin>GetNBin()) return kFALSE;
  
  if (mode==0) 
  {
    TArrayF SpectraTemp=GetSpectra(bin);        
    Int_t nadcMax=GetNAdcMax();    
    if(CalibrateByMip(bin,SpectraTemp,0,nadcMax)) return kTRUE;
  }
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateByMip(Int_t bin,TArrayF SpectraTemp,
                                    Int_t fitmin,Int_t fitmax)
{
  emcMipCalib_st* mip=MipTable->GetTable();

  StEmcFit *fit=new StEmcFit();
  fit->SetNParms(6);
  Int_t type=1;             // two gaussians
  fit->SetFuncType(type);
  Float_t a[7];
  Int_t   ia[7]={0,1,1,1,1,1,1};
  
  Int_t adcmip=fitmin;
  Int_t firstadc=0;
  for(Int_t i=fitmin;i<fitmax;i++)
      if(SpectraTemp[i]>SpectraTemp[adcmip]) adcmip=i;
      
  if(adcmip<20) 
  {
    firstadc=5;
    a[3]=5;         // mip width
  } 
  else 
  {
    firstadc=10; 
    a[3]=10;        // mip width
  }
  
  a[2]=adcmip;      // mip position
  
  Int_t lastadc=(Int_t)(5.0*a[3]+(Float_t)adcmip);
  
  a[4]=0;
  for(Int_t i=0;i<3;i++) a[4]+=SpectraTemp[firstadc+i]/3;   // background amplitude
  a[5]=(Float_t)firstadc+1.;                      // background center
  
  Float_t b=(Float_t) lastadc;
  Float_t yb=0;
  for(Int_t i=0;i<3;i++) yb+=SpectraTemp[lastadc+i-1]/3;
  a[6]=(b-a[3])/sqrt(fabs(2*log(a[4]/yb)));       // background width
  
  a[1]=SpectraTemp[adcmip]-a[4]*exp(-0.5*pow((a[2]-a[5])/a[6],2));  // mip amplitude
  cout <<"firstadc = "<<firstadc<<"  lastadc = "<<lastadc<<endl;

  for(Int_t i=firstadc;i<lastadc;i++) 
  {
    Float_t sig=sqrt(SpectraTemp[i]);
    if (sig==0) sig=1;
    fit->AddPoint((Float_t)i,SpectraTemp[i],sig);
  }
  for(Int_t i=1;i<=6;i++) 
  {
    cout <<"Parm i = "<<i<<"  Initial a[i] = "<<a[i]<<endl;
    fit->SetParm(i,a[i],ia[i]);
  }
  
  fit->Fit(2000);

  for (int i=1;i<=fit->GetNParam();i++)
  {
    a[i]=fit->GetParameter(i);
    Float_t ea=fit->GetParameterError(i);
    cout <<"Parm i = "<<i<<"  Final a[i] = "<<a[i]<<" +- "<< ea<<endl;
    mip[bin-1].MipFitParam[i-1]=a[i];
    mip[bin-1].MipFitParamError[i-1]=ea;
  }
  Float_t chi=fit->GetChiSquare()/(fit->GetNPoints()-fit->GetNParam());
  cout <<"Final chi2 = "<<chi<<endl;

  cout <<"Covariance Matrix ********************\n";
  for(Int_t i=1;i<=fit->GetNParam();i++)
  {
    for(Int_t j=1;j<=fit->GetNParam();j++) 
    {
      mip[bin-1].MipFitCovMatrix[i-1][j-1]=fit->GetCovariance(i,j);
      cout <<"  "<< fit->GetCovariance(i,j);
    }
    cout <<endl;
  }
  mip[bin-1].Status=1;
  mip[bin-1].MipFitAdcMin=firstadc;
  mip[bin-1].MipFitAdcMax=lastadc;
  
  mip[bin-1].MipFitChiSqr=chi;
  mip[bin-1].MipPeakPosition=mip[bin-1].MipFitParam[1];
  mip[bin-1].MipPeakPositionError=mip[bin-1].MipFitParamError[1];
  mip[bin-1].MipPeakWidth=mip[bin-1].MipFitParam[2];
  mip[bin-1].MipPeakWidthError=mip[bin-1].MipFitParamError[2];
  delete fit;

  return kTRUE; 
}
