/**********************************************************************
* StEmcMipSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for MIP Calibration
***********************************************************************/
#include "StEmcMipSpectra.h"
#include <Stiostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "StEmcFit.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEvent.h"
#include "StEventTypes.h"

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
StEmcMipSpectra::StEmcMipSpectra(const char* cdet,Int_t nb, Float_t bin0, Float_t bin1):StEmcSpectra(cdet,nb,bin0,bin1)
{      
  for(Int_t i=0;i<MAXBIN;i++ ) doUseGuess[i] = kFALSE;
  Int_t n = GetNBin();
  mMipFit    = new TH2D("mMipFit","mMipFit",n,1,(Float_t)n+1,30,0,30);
  mMipFit->Reset();
  NPars=7;
  //peak = new TF1("peak","[0]*exp(-0.5*( (x-[1])*(x-[1])/([2]*[2]*(1.0-[3]*(x-[1]))   ) ))");
  peak = new TF1("peak","[0]*( exp(-0.5*((x-[1])/[2])*((x-[1])/[2]) ) + [3]*exp(-0.5*((x-([1]+1.5*[2]))/[2])*((x-([1]+1.5*[2]))/[2]) ) )");
  back = new TF1("back","gaus(0)");
}
//_____________________________________________________________________________
StEmcMipSpectra::~StEmcMipSpectra()
{ }
//_____________________________________________________________________________
TH1D* StEmcMipSpectra::DrawEtaBin(Int_t etabin)
{
  if(etabin>GetNEtaBins() || etabin<1) return NULL;
  Int_t nadcMax=(Int_t)GetSpecMax();

  TCanvas* canvas7=new TCanvas("canvas7","EMC Eta Bin Spectrum",500,350);
  
  char title[90];
  sprintf(title,"Eta Bin %02d MIP peak spectrum",etabin);
  TH1D* hist=new TH1D("hist",title,nadcMax,0,(Float_t)nadcMax-1);

  hist->Add(GetEtaBinSpectra(etabin),1);
  Int_t ibin = mMipFit->FindBin((Float_t)etabin,29);
  Int_t status = (Int_t)mMipFit->GetBinContent(ibin);
  
  canvas7->cd();
  hist->Draw();
  
  if (status==1)
  {
    TH1D* fit=new TH1D("fit","",nadcMax,0,(Float_t)nadcMax-1);
    TH1D* fitpeak=new TH1D("fitpeak","",nadcMax,0,(Float_t)nadcMax-1);
    TH1D* fitback=new TH1D("fitback","",nadcMax,0,(Float_t)nadcMax-1);
    
    for(Int_t adc=0;adc<nadcMax;adc++)
    {
      Double_t ADC = (Double_t)adc;
      for(Int_t i=0;i<NPars;i++)
      {    
        ibin = mMipFit->FindBin((Float_t)etabin,i);
        Float_t par = mMipFit->GetBinContent(ibin);
        if(i<4) peak->SetParameter(i,par);
        else back->SetParameter(i-4,par);
      }
      
      fit->Fill((Float_t)adc,peak->Eval(ADC)+back->Eval(ADC));
      fitpeak->Fill((Float_t)adc,peak->Eval(ADC));
      fitback->Fill((Float_t)adc,back->Eval(ADC));
    }
    hist->SetFillColor(11);
    fit->SetLineColor(4);
    fit->SetLineWidth(2);
    fitpeak->SetLineColor(2); 
    fitback->SetLineColor(3);
    fit->Draw("sameC");
    fitpeak->Draw("sameC");
    fitback->Draw("sameC");
  }
  return hist;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateEtaBin(Int_t etabin,Int_t mode)
{
  if(etabin<0 || etabin>GetNEtaBins()) return kFALSE;
  
  if (mode==0 || mode ==1) 
  {
    TH1D* SpectraTemp=GetEtaBinSpectra(etabin);            
    Int_t nmin = (Int_t)GetSpecMin();
    Int_t nmax = (Int_t)GetSpecMax();    
    cout <<"********** Calibrating Eta bin "<<etabin<<endl;
    if(CalibrateByMip(etabin,SpectraTemp,nmin,nmax)) return kTRUE;
  }
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateBin(Int_t bin,Int_t mode)
{
  if(bin<1 || bin>GetNEtaBins()) return kFALSE;
  
  if (mode==0 || mode ==1) 
  {
    TH1D* SpectraTemp=GetSpectra(bin);        
    Int_t nmin = (Int_t)GetSpecMin();
    Int_t nmax = (Int_t)GetSpecMax();    
    if(CalibrateByMip(bin,SpectraTemp,nmin,nmax)) return kTRUE;
  }
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::CalibrateByMip(Int_t bin,TH1D* SpectraTemp,
                                    Int_t fitmin,Int_t fitmax)
{
//  
//  This is the method that performs the MIP peak fit. The fit is 
//  dependent on initial guesses so, every case is one case and there is
//  no way to find best guesses automaticaly. 

  TF1 *func = new TF1("func","[0]*( exp(-0.5*((x-[1])/[2])*((x-[1])/[2]) ) + [3]*exp(-0.5*((x-([1]+1.5*[2]))/[2])*((x-([1]+1.5*[2]))/[2]) ) ) +gaus(4)",(Float_t) fitmin, (Float_t)fitmax);
  if(doUseGuess[bin-1])
  {
    for(Int_t i=0;i<NPars;i++)
    {
      func->SetParameter(i,guess[bin-1][i]);
      if(fixed[bin-1][i]==1) func->FixParameter(i,guess[bin-1][i]);
    }
  }
  SpectraTemp->Fit(func,"RBN");
  
  for (int i=0;i<NPars;i++)
  {
    Float_t a  = func->GetParameter(i);
    Float_t ea = func->GetParError(i);
    cout <<"Parm i = "<<i<<"  Final a = "<<a<<" +- "<< ea<<endl;
    mMipFit->Fill((Float_t)bin,(Float_t)i,a);
    mMipFit->Fill((Float_t)bin,(Float_t)i+10,ea);   
  }
  Float_t chi=func->GetChisquare()/(func->GetNDF());
  cout <<"Final chi2 = "<<chi<<endl;
  
  Float_t mip   = func->GetParameter(1);
  Float_t emip  = func->GetParError(1);
  Float_t w     = func->GetParameter(2);
  Float_t ew    = func->GetParError(2);
  
  mMipFit->Fill((Float_t)bin,20,mip);  // mip peak position
  mMipFit->Fill((Float_t)bin,21,emip); // mip peak position error
  mMipFit->Fill((Float_t)bin,22,w);    // mip peak width
  mMipFit->Fill((Float_t)bin,23,ew);   // mip peak width error
  mMipFit->Fill((Float_t)bin,28,chi);  // chi square of the fit
  mMipFit->Fill((Float_t)bin,29,1);    // status of the fit
  cout <<"MIP Peak position = "<<mip<<" +- "<<emip<<endl;
  cout <<"MIP Peak width    = "<<w<<" +- "<<ew<<endl;
  delete func;
  
  
  /*StEmcFit *fit=new StEmcFit();
  fit->SetNParms(parms);
  Int_t type=1;             // two gaussians
  fit->SetFuncType(type);
  Float_t a[parms+1];
  Float_t ea[parms+1];
  Int_t   ia[]={0,1,1,1,1,1,1};
  
  Int_t firstadc=10; 
  Int_t lastadc;
  
  if(!doUseGuess[bin-1])
  {
    Int_t adcmip=20;
    for(Int_t i=20;i<fitmax;i++) 
    {
      Int_t ibin = SpectraTemp->FindBin((Float_t)i);
      Float_t t = SpectraTemp->GetBinContent(ibin);
      ibin = SpectraTemp->FindBin((Float_t)adcmip);
      Float_t mip = SpectraTemp->FindBin(ibin);
      if(t>mip) adcmip=i;
    }
    a[3]=10;        // mip width  
    a[4]=0;
    for(Int_t i=0;i<3;i++) a[4]+=SpectraTemp->GetBinContent(SpectraTemp->FindBin((Float_t)(firstadc+i)))/3.;   // background amplitude  
    a[5]=(Float_t)firstadc+1.;                      // background center  
    a[2]=adcmip;      // mip position
    lastadc=(Int_t)(8.0*a[3]+(Float_t)adcmip);
    Float_t b=(Float_t) lastadc;
    Float_t yb=0;
    for(Int_t i=0;i<3;i++) yb+=SpectraTemp->GetBinContent(SpectraTemp->FindBin((Float_t)(lastadc+i-1)))/3.;
    a[6]=(b-a[3])/::sqrt(fabs(2*::log(a[4]/yb)));       // background width  
    a[1]=SpectraTemp->GetBinContent(SpectraTemp->FindBin((Float_t)adcmip))-a[4]*exp(-0.5*::pow((a[2]-a[5])/a[6],2));  // mip amplitude
  }
  else
  {
    for(Int_t i=0;i<6;i++) a[i+1] = guess[bin-1][i];
    firstadc=FIRST[bin-1];
    lastadc=LAST[bin-1];
  }
  
  cout <<"firstadc = "<<firstadc<<"  lastadc = "<<lastadc<<endl;

  a[3]=::log(a[2]+a[3])-::log(a[2]);
  a[2]=::log(a[2]);    
  a[6]=::log(a[5]+a[6])-::log(a[5]);
  a[5]=::log(a[5]);

  for(Int_t i=firstadc;i<lastadc;i++) 
  {
    Int_t ibin = SpectraTemp->FindBin(i);
    Float_t value = SpectraTemp->GetBinContent(ibin);
    Float_t sig=::sqrt(value);
    if (sig==0) sig=1;
    fit->AddPoint(::log((Float_t)i),value,sig);
  }
  for(Int_t i=1;i<=fit->GetNParam();i++) 
  {
    cout <<"Parm i = "<<i<<"  Initial a[i] = "<<a[i]<<endl;
    fit->SetParm(i,a[i],ia[i]);
  }  
  fit->Fit(2000);
  for (int i=1;i<=fit->GetNParam();i++)
  {
    a[i-1]=fit->GetParameter(i);
    ea[i-1]=fit->GetParameterError(i);
    cout <<"Parm i = "<<i<<"  Final a[i] = "<<a[i-1]<<" +- "<< ea[i-1]<<endl;
    mMipFit->Fill((Float_t)bin,(Float_t)i-1,a[i-1]);
    mMipFit->Fill((Float_t)bin,(Float_t)i+9,ea[i-1]);   
  }
  Float_t chi=fit->GetChiSquare()/(fit->GetNPoints()-fit->GetNParam());
  cout <<"Final chi2 = "<<chi<<endl;
  
  Float_t expab= exp(a[1]-a[2]);
  Float_t expa = exp(a[1]);
  Float_t expb = exp(a[2]);
  Float_t sa   = ea[1];
  Float_t sb   = ea[2];    
  Float_t w    = expa-expab;
  Float_t ew   = ::sqrt(expab*expab*(sa*sa+sb*sb)+expa*expa*sa*sa);
  Float_t mip  = expa;
  Float_t emip = exp(mip*ea[1]/a[1]);
  
  mMipFit->Fill((Float_t)bin,20,mip);  // mip peak position
  mMipFit->Fill((Float_t)bin,21,emip); // mip peak position error
  mMipFit->Fill((Float_t)bin,22,w);    // mip peak width
  mMipFit->Fill((Float_t)bin,23,ew);   // mip peak width error
  mMipFit->Fill((Float_t)bin,28,chi);  // chi square of the fit
  mMipFit->Fill((Float_t)bin,29,1);    // status of the fit
  cout <<"MIP Peak position = "<<mip<<" +- "<<emip<<endl;
  cout <<"MIP Peak width    = "<<w<<" +- "<<ew<<endl;
  delete fit;*/

  return kTRUE; 
}
//_____________________________________________________________________________
void StEmcMipSpectra::SetInitialGuess(Int_t bin, Float_t* a,Int_t *fix,Int_t f,Int_t l) 
{ 
  doUseGuess[bin-1]=kTRUE; 
  for(Int_t i=0;i<NPars;i++) { guess[bin-1][i]=a[i]; fixed[bin-1][i] = fix[i]; } 
  FIRST[bin-1]=f;
  LAST[bin-1]=l;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::Fill(TH1F* hits, StEvent* event)
{
	cout <<"***** Filling MIP's ...\n";
  StEmcSpectra::Fill();
  
  Int_t NTracks = GetNTracks(event);
  if(NTracks>GetMaxMultiplicity()) return kFALSE;
	
  Int_t mipAccepted=0;
  Bool_t ok=kFALSE;
  
  GetFilter()->setMaxTracksPerTower(1);
  GetFilter()->setEMin(0);
  GetFilter()->setEMax(100);
  GetFilter()->setPtMin(0);
  GetFilter()->setPtMax(100);
  GetFilter()->setPNeighbor(kTRUE);
  GetFilter()->setSNeighbor(kFALSE);
  GetFilter()->setTNeighbor(kFALSE);
	
  Float_t BField = GetFilter()->getBField();
	
  for(Int_t i=0;i<NTracks;i++)
  {    
    StTrack* track = GetTrack(event,i);
    
    Float_t p = track->geometry()->momentum().mag();
//VPunused    Float_t pt = track->geometry()->momentum().perp();
    if(p>=GetMinMomentum())
    {
			StThreeVectorD momentum,position;
			StThreeVectorD momentum1,position1;
      Bool_t tok  = GetPosition()->trackOnEmc(&position,&momentum,(StTrack*)track,(Double_t)BField,(Double_t)GetGeo()->Radius());
      Bool_t tok1 = GetPosition()->trackOnEmc(&position1,&momentum1,(StTrack*)track,(Double_t)BField,(Double_t)GetGeo()->Radius()+30.);
      if(tok && tok1)
      {
				Float_t eta=position.pseudoRapidity();
        Float_t phi=position.phi();
        Int_t m,e,s;
        GetGeo()->getBin(phi,eta,m,e,s);
        //if(s==-1) s=1;
        Int_t id=0;
        if(m<=120 && s!=-1) GetGeo()->getId(m,e,s,id);
				
				Float_t eta1=position1.pseudoRapidity();
        Float_t phi1=position1.phi();
        Int_t m1,e1,s1;
        GetGeo()->getBin(phi1,eta1,m1,e1,s1);
        //if(s1==-1) s1=1;
        Int_t id1=0;
        if(m1<=120 && s!=-1) GetGeo()->getId(m1,e1,s1,id1);
        if(GetFilter()->accept(id) && id==id1)
				{
          Int_t ibin = hits->FindBin((Float_t)id);
		      Float_t adc= hits->GetBinContent(ibin);
					if(GetStatus(id)==1 && adc>0) 
        	{
            cout <<"p="<<p<<" pt tower="<<GetFilter()->getPtTower(id)
				         <<" eta="<<eta<<" phi="<<phi
						     <<" m="<<m<<" e="<<e<<" s="<<s
						     <<" id="<<id<<" stat="<<GetStatus(id)						 
						     <<" ADC="<<adc<<endl;
          	FillSpectra(id,adc);
						ok=kTRUE;
          	mipAccepted++;
        	} 
				} 
      } 
    }      
  }  
  
  Float_t occ=0,x=0,y=0;
  
  if (GetNEtaBins()>0) GetOccupancyEtaBin(GetMinHits(),&x,&y,&occ);
  else GetOccupancy(GetMinHits(),&x,&y,&occ);

  cout   <<"MIP: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  cout   <<"MIP: fraction of bins nevents > minimum = "<<occ<<"\n";
  
  if(occ<GetMinOcc()) return kFALSE;	
	
	return kTRUE; 
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::MipCalib()
{  
  Int_t ndiv;
  if(GetNEtaBins()>0) ndiv=GetNEtaBins();
  else ndiv=GetNBin();
  
  for(Int_t i=1;i<=ndiv;i++)
  {
    cout<<"StEmcCalibrationMaker::MipCalib() - MIP Calibration for EtaBin or tower "<<i<<endl;

    if(GetNEtaBins()>0)
    {
      if(GetSumEtaBin(i)>=GetMinHits()) CalibrateEtaBin(i,0);
    }
    else
    {
      if(GetStatus(i)==1) if(GetSum(i)>=GetMinHits()) CalibrateBin(i,0);
    }
  }

  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcMipSpectra::GetMipPosition(Int_t position, Float_t *MIP, Float_t *ERROR)
{
  Int_t n = GetNBin();
  if(position<1 || position>n) return kFALSE;
  Float_t mip = 0, error = 0, status =0;
  Bool_t ok = kFALSE;
  Int_t etabin = position;
  if(GetNEtaBins()>0) 
  {
    Int_t m,e,s;
    GetGeo()->getBin(position,m,e,s);
    etabin = GetEtaBinId(m,e);    
  }
  Int_t ibin = mMipFit->FindBin((Float_t)etabin,20.0);
  Float_t mipeta = mMipFit->GetBinContent(ibin);
  ibin = mMipFit->FindBin((Float_t)etabin,21.0);
  Float_t erroreta = mMipFit->GetBinContent(ibin);
  ibin = mMipFit->FindBin((Float_t)etabin,29.0);
  status = mMipFit->GetBinContent(ibin);
  if(status==1) ok = kTRUE;    
  if(GetNEtaBins()>0)
  {    
    Float_t a,b;
    Int_t s;
    GetEqualConst(position,&a,&b,&s);
    if(s==1)
    {
      mip = mipeta*a+b;
      error = erroreta*a;
    } else ok = kFALSE;
  }
  else
  {
    mip = mipeta;
    error = erroreta;
  } 
  *MIP = mip;
  *ERROR = error;
  return ok;
}
