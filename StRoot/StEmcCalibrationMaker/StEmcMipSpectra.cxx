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

ClassImp(StEmcMipSpectra);
Double_t PI=3.1415926;
Double_t xfit[4096];
Int_t npointsfit,xinit,xstop;
 
//_____________________________________________________________________________
//These functions are for TMinuit MIP fit
//_____________________________________________________________________________
Double_t Gaussian(Double_t x,Double_t A,Double_t xavg,Double_t sigma,Double_t assym)
{
  Double_t sig=sigma;
  if(x>xavg)sig*=assym;
  Double_t xtemp=(x-xavg)/sig;  
  return (A/(sqrt(2.*PI)*sigma))*exp(-0.5*xtemp*xtemp);
}
Double_t Pol(Double_t x,Double_t A,Double_t B,Double_t C)
{
  return A+B*x+C*x*x;
}
void TwoGaussians(Int_t &npar,Double_t *gin, Double_t &chi, Double_t *par, Int_t iflag)
{
  Double_t chitemp=0,np=0;
  for(Int_t adc=xinit;adc<xstop;adc++)
  {
    if(xfit[adc]>0)
    {
      Double_t peak=Gaussian((Double_t)adc+0.5,par[0],par[1],par[2],par[3]);
      Double_t back=Gaussian((Double_t)adc+0.5,par[4],par[5],par[6],1);
//      Double_t back=Pol((Double_t)adc,par[4],par[5],par[6]);
      Double_t f=peak+back;
      chitemp+=((xfit[adc]-f)*(xfit[adc]-f))/xfit[adc];
      np++;
    }
  }
  if(np>0) chi=sqrt(chitemp/np);
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
                               (Double_t)mip[etabin-1].MipFitParam[2],
                               (Double_t)mip[etabin-1].MipFitParam[3]);
      Double_t gauss2=Gaussian((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[4],
                               (Double_t)mip[etabin-1].MipFitParam[5],
                              (Double_t)mip[etabin-1].MipFitParam[6],1);
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
    Int_t mi,mf,ei,ef;
    CalcEtaBin(etabin,etaBinWidth,&mi,&mf,&ei,&ef);
        
    Int_t firstadc=5;   // first adc to fit
    
    Int_t nadcMax=GetNAdcMax();
    
    Int_t adcmip=firstadc;
    for(Int_t i=firstadc;i<nadcMax;i++)
      if(SpectraTemp[i]>SpectraTemp[adcmip]) adcmip=i;
    
    Int_t lastadc=(Int_t)(2.5*(Float_t)adcmip);

    if(CalibrateByMip(etabin,SpectraTemp,firstadc,lastadc)) return kTRUE;
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
    
    Int_t firstadc=5;   // first adc to fit
    
    Int_t nadcMax=GetNAdcMax();
    
    Int_t adcmip=firstadc;
    for(Int_t i=firstadc;i<nadcMax;i++)
      if(SpectraTemp[i]>SpectraTemp[adcmip]) adcmip=i;
    
    Int_t lastadc=(Int_t)(2.5*(Float_t)adcmip);

    if(CalibrateByMip(bin,SpectraTemp,firstadc,lastadc)) return kTRUE;
  }
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateByMip(Int_t bin,TArrayF SpectraTemp,
                                    Int_t fitmin,Int_t fitmax)
{
  Float_t max=0,countmax=0,sum=0;
  Int_t nadcMax=GetNAdcMax();
  for(Int_t i=0;i<nadcMax;i++)
  {
    xfit[i]=0;
    if(SpectraTemp[i]>0 && i>=fitmin && i<=fitmax) 
    {
      xfit[i]=SpectraTemp[i];
      sum+=xfit[i];
      if(xfit[i]>countmax) { max=(Float_t)i; countmax=xfit[i]; }
    }
  }

  npointsfit=fitmax-fitmin;
  xinit=fitmin;
  xstop=fitmax;
    
// setting fit procedure    
  Double_t para[10],step[10],errp[10],pmin[10],pmax[10];
  for(Int_t i=0;i<10;i++)
  {
    pmin[i]=0; pmax[i]=0;
  }
  
// inicial guesses...
  para[2]=2; 
  para[1]=max;
  para[0]=countmax*(sqrt(2*PI)*para[2]);
  para[3]=1.; pmin[3]=0.9 ; pmax[3]=1.1;

// for gaussian background
  para[4]=(Double_t)sum-para[0];
  para[5]=0;
  para[6]=120; pmin[6]=20; pmax[6]=300;
  
// for parabolic background
//  para[5]=(SpectraTemp[fitmax]-SpectraTemp[fitmin])/(Float_t)(fitmax-fitmin);
//  para[4]=SpectraTemp[fitmin]-para[4]*(Float_t)fitmin;
//  para[6]=0;
  
  for(Int_t i=0;i<10;i++) step[i]=para[i]/50.+0.001;

  TMinuit *fit = new TMinuit(7);    // max 6 parameters to fit
  Int_t ierflag=0;
  fit->Command("SET ERR 1");    // init fit
  fit->SetFCN(TwoGaussians); 
  fit->mnparm(0,"A    ",para[0],step[0],pmin[0],pmax[0],ierflag);
  fit->mnparm(1,"Peak ",para[1],step[1],pmin[1],pmax[1],ierflag);
  fit->mnparm(2,"Width",para[2],step[2],pmin[2],pmax[2],ierflag);  
  fit->mnparm(3,"Assym",para[3],step[3],pmin[3],pmax[3],ierflag);
  fit->mnparm(4,"BG1  ",para[4],step[4],pmin[4],pmax[4],ierflag);
  fit->mnparm(5,"BG2  ",para[5],step[5],pmin[5],pmax[5],ierflag);
  fit->mnparm(6,"BG3  ",para[6],step[6],pmin[6],pmax[6],ierflag);

//Fitting ...
  Double_t chi2=0,edm,errdef,chiold;
  Int_t nvpar,nparx,icstat;

  do
  {
    chiold=chi2;
    fit->Command("MIG 100");
    fit->mnstat(chi2,edm,errdef,nvpar,nparx,icstat);
    cout <<"**************** chi2 = "<<chi2<<"\n";
  } while(fabs(chiold-chi2)>0.000001); //does fit while chi2 changes
//  } while(chiold!=chi2); //does fit while chi2 changes
  
//end of fit 
    
  emcMipCalib_st* mip=MipTable->GetTable();
  
// filling etabin table
  mip[bin-1].Status=1;
  mip[bin-1].MipFitAdcMin=fitmin;
  mip[bin-1].MipFitAdcMax=fitmax;
  
  for(Int_t i=0;i<7;i++)
  {            
    fit->GetParameter(i,para[i],errp[i]);
    cout <<"i = "<<i<<"  para[i] = "<<para[i]<<" err[i] = "<<errp[i]<<"\n";
    mip[bin-1].MipFitParam[i]=para[i];
    mip[bin-1].MipFitParamError[i]=errp[i];
    // need to write covariance matrix
  }
  mip[bin-1].MipFitChiSqr=chi2;
  mip[bin-1].MipPeakPosition=para[1];
  mip[bin-1].MipPeakPositionError=errp[1];
  mip[bin-1].MipPeakWidth=para[2];
  mip[bin-1].MipPeakWidthError=errp[2];
// finished filling eta bin table

  return kTRUE; 
}
