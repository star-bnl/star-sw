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
#include "StEmcUtil/StEmcGeom.h"

ClassImp(StEmcMipSpectra);
 
//_____________________________________________________________________________
//These functions are for TMinuit MIP fit
//_____________________________________________________________________________
Double_t Gaussian(Double_t x,Double_t A,Double_t xavg,Double_t sigma)
{
  Double_t ex,arg;
  arg=(x-xavg)/sigma;
  ex=exp(-0.5*arg*arg);
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
  
  char title[90];
  sprintf(title,"Eta Bin %02d MIP peak spectrum",etabin);
  TH1F* hist=new TH1F("hist",title,nadcMax,0,(Float_t)nadcMax-1);

  TArrayF temp=GetEtaBinSpectra(etabin);
  for(Int_t j=0;j<nadcMax;j++) hist->Fill(j,temp[j]);
  
  canvas7->cd();
  hist->Draw();
  emcMipCalib_st* mip=MipTable->GetTable();

  if (mip[etabin-1].Status==1)
  {
    TH1F* fit=new TH1F("fit","",nadcMax,0,(Float_t)nadcMax-1);
    TH1F* fitpeak=new TH1F("fitpeak","",nadcMax,0,(Float_t)nadcMax-1);
    TH1F* fitback=new TH1F("fitback","",nadcMax,0,(Float_t)nadcMax-1);
    
    for(Int_t adc=0;adc<nadcMax;adc++)
    {
      Double_t gauss1=Gaussian((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[0],
                                             (Double_t)mip[etabin-1].MipFitParam[1],
                                             (Double_t)mip[etabin-1].MipFitParam[2]);

      Double_t gauss2=Gaussian((Double_t)adc,(Double_t)mip[etabin-1].MipFitParam[3],
                                             (Double_t)mip[etabin-1].MipFitParam[4],
                                             (Double_t)mip[etabin-1].MipFitParam[5]);

      fit->Fill((Float_t)adc,gauss1+gauss2);
      fitpeak->Fill((Float_t)adc,gauss1);
      fitback->Fill((Float_t)adc,gauss2);
    }
    hist->SetFillColor(11);
    fit->SetLineColor(4);
    fit->SetLineWidth(2.0);
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
    cout <<"********** Calibrating Eta bin "<<etabin<<endl;
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


/*
  This is the method that performs the MIP peak fit. The fit is 
  dependent on initial guesses so, every case is one case and there is
  no way to find best guesses automaticaly. An initial guess finder is
  implemented but every fit we must check the numbers by hand.
*/


  const Int_t parms=6;
  
  StEmcFit *fit=new StEmcFit();
  fit->SetNParms(parms);
  Int_t type=1;             // two gaussians
  fit->SetFuncType(type);
  Float_t a[parms+1];
  Int_t   ia[]={0,1,1,1,1,1,1};
  
  
  Int_t adcmip=20;
  Int_t firstadc=0;
  for(Int_t i=20;i<fitmax;i++)
      if(SpectraTemp[i]>SpectraTemp[adcmip]) adcmip=i;
      
  firstadc=10; 
  a[3]=10;        // mip width
  
  if(bin==2) adcmip+=1;
  //if(bin==1) ;
  if(bin==4) {firstadc=18;}
  
  Int_t lastadc=(Int_t)(8.0*a[3]+(Float_t)adcmip);
  
  a[4]=0;
  for(Int_t i=0;i<3;i++) a[4]+=SpectraTemp[firstadc+i]/3;   // background amplitude
  
  if(bin==2) {firstadc=7; lastadc=140;}
    
  //if(bin==5) lastadc=150; 
  
  a[5]=(Float_t)firstadc+1.;                      // background center
  
  a[2]=adcmip;      // mip position
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
  for(Int_t i=1;i<=fit->GetNParam();i++) 
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
//_____________________________________________________________________________
TArrayF StEmcMipSpectra::GetEtaBinSpectra(Int_t etabin)
{
  if(etabin>nEtaBins || etabin<1) return 0;
  
  if(!EqualTable) return 0;

  emcEqualization_st* rows=EqualTable->GetTable(); 
  
  TArrayF temp(GetNAdcMax());
  for(Int_t i=0;i<GetNAdcMax();i++) temp[i]=0;

  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,etaBinWidth,&mi,&mf,&ei,&ef);
  
  if(ei!=ef && ef==nEta) ef--;  // remove last channels in eta
    
  si=1; sf=GetNSub();

  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(rows[id-1].EqStatus==1 && GetStatus(id)==1)
        {
          Float_t a=rows[id-1].EqSlope;
          Float_t b=rows[id-1].EqShift;
          if(a!=0) 
          {
            TArrayF temp1=GetSpectra(id,a,b); 
            for(Int_t j=0;j<GetNAdcMax();j++) temp[j]+=temp1[j];
          } 
        }
      }
  
  return temp;
}
