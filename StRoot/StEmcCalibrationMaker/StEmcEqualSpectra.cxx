/**********************************************************************
* StEmcEqualSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/
#include "StEmcEqualSpectra.h"
#include <Stiostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEvent/StEvent.h"
#include "TF1.h"
#include "math.h"

ClassImp(StEmcEqualSpectra);

//_____________________________________________________________________________
StEmcEqualSpectra::StEmcEqualSpectra(const char* cdet,Int_t nbis, Float_t bin0, Float_t bin1):StEmcSpectra(cdet,nbis,bin0,bin1)
{  
	mEqualMode = 3;
	mEqualMin = 20;
	mEqualMax = 140;			 
}
//_____________________________________________________________________________
StEmcEqualSpectra::~StEmcEqualSpectra()
{ }
//_____________________________________________________________________________
void StEmcEqualSpectra::DrawEqualConst()
{
	TH2F* equal=GetEqual();
	if(!equal) return;
  
	TCanvas* canvas5=new TCanvas("canvas5","EMC Equalization Slope",500,700);
  canvas5->Divide(1,2);
  TCanvas* canvas6=new TCanvas("canvas6","EMC Equalization Shift",500,700);
  canvas6->Divide(1,2);
  
  TH1F* distr1=new TH1F("distr1","Relative gain distribution",200,0,4);
  TH1F* distr2=new TH1F("distr2","Relative shift distribution",200,-50,50);
  distr1->SetFillColor(11);
  distr2->SetFillColor(11);
  
  const int nbins=GetNBin();
  
  Float_t x[18000],y[18000],y1[18000];
  
  for(Int_t j=0;j<nbins;j++) 
  {
    x[j]=(Float_t)j+1;
		Float_t a,b;
		Int_t s;
    GetEqualConst(j+1,&a,&b,&s);
    //cout <<"bin = "<<j<<"  a = "<<a<<"  b = "<<b<<"  s = "<<s<<endl;
    if (a>0 && s==1)
    { 
      if(fabs(a)<200) y[j]=a;
      if(fabs(b)<200) y1[j]=b;
      distr1->Fill(y[j]);
      distr2->Fill(y1[j]);
    } 
    else
    {
      y[j]=0;
      y1[j]=0;
    }
    if(y[j]>1e10)  y[j]=0;
    if(y1[j]>1e10) y1[j]=0;
  }
  TGraph* graph1=new TGraph(nbins,x,y);
  graph1->SetTitle("Equalization Relative gain");
  TGraph* graph2=new TGraph(nbins,x,y1);
  graph2->SetTitle("Equalization Relative shift");

  canvas5->cd(1);
  graph1->SetMarkerStyle(20);
  graph1->SetMarkerColor(1); 
  graph1->Draw("AP");
  canvas5->cd(2);
  distr1->Draw();
  
  canvas6->cd(1);
  graph2->SetMarkerStyle(20);
  graph2->SetMarkerColor(1); 
  graph2->Draw("AP");
  canvas6->cd(2);
  distr2->Draw();
  
  return;
}

//_____________________________________________________________________________
Bool_t StEmcEqualSpectra::Equalize(Int_t position1,Int_t position2,Int_t mode)
{ 
  // mean and RMS modes  
  // 0 -  mean and RMS with liear average
  // 1 -  mean with linear average
  // 2 -  mean and rms with log average
  // 3 -  mean with log average 
	// 4 -  exponential fit

  if(GetStatus(position1)==0) return kFALSE;
  if(GetStatus(position2)==0) return kFALSE;
   
  Bool_t EqDone=kFALSE;
  Float_t a=0,b=0;
  
  if(mode==-1)
  {
    a = 1;
    b = 0;
    EqDone=kTRUE;
  }
  
  if(mode>=0 && mode <=3)  
  {
    Float_t m1,r1,m2,r2;        
    if(mode==0 || mode==1)
    {
      GetMeanAndRms(position1,mEqualMin,mEqualMax,&m1,&r1);
      GetMeanAndRms(position2,mEqualMin,mEqualMax,&m2,&r2);
    }
    if(mode==2 || mode==3)
    {
      GetLogMeanAndRms(position1,mEqualMin,mEqualMax,&m1,&r1);
      GetLogMeanAndRms(position2,mEqualMin,mEqualMax,&m2,&r2);
    }
    if(mode==0 || mode==2)
    {
      a=r1/r2;
      b=m1-a*m2;
    }
    if(mode==1 || mode==3)
    {    
      a=m1/m2;
      b=0;
    }
    EqDone=kTRUE;
    cout <<"  id = "<<position2<<"  ref = "<<position1<<"  mean = "<<m1<<" , "<<m2
         <<"  rms = "<<r1<<" , "<<r2
         <<"  a = "<<a<<"  b = "<<b<<endl;
  }
  
	if(mode==4)
	{
		TF1 *f=new TF1("ff","[0]*exp(-x/[1])",mEqualMin,mEqualMax);
		TH1D *h=GetSpectra(position1);
    Float_t I1 = h->Integral(h->FindBin(mEqualMin),h->GetNbinsX());
		f->SetParameter(1,10);
		h->Fit(f,"RQNLW");
		Float_t m1,m2,A1,A2;
		m1 = f->GetParameter(1);
		A1 = f->GetParameter(0);
		h=GetSpectra(position2);
    Float_t I2 = h->Integral(h->FindBin(mEqualMin),h->GetNbinsX());
		h->Fit(f,"RQNLW");
		m2 = f->GetParameter(1);
		A2 = f->GetParameter(0);
		a=m1/m2;
		b=-::log((A2*I1)/(A1*I2));
		EqDone=kTRUE;
    if(!finite(a) || !finite(b) || a<=0 || b>1000) EqDone = kFALSE;
    b=0;
    cout <<"  id = "<<position2<<"  ref = "<<position1<<"  slopes = "<<m2<<" , "<<m1
         <<"  a = "<<a<<"  b = "<<b<<"  EQDONE = "<<(Int_t)EqDone<<endl;
		delete f;
	}
  
  if (EqDone)
  {
		TH2F* equal=GetEqual();
		if(!equal) return kFALSE;
		SetEqualConst(position2,a,b,1);
  }  
  else SetEqualConst(position2,0,0,0);
  return EqDone;
}
//_____________________________________________________________________________
Bool_t StEmcEqualSpectra::Fill(TH1F* hits,StEvent* event)
{  
	cout <<"***** Filling equalization ...\n";  
  StEmcSpectra::Fill();
	Int_t NTracks = GetNTracks(event);
  if(NTracks<GetMinMultiplicity()) return kFALSE;  
  for(Int_t j=0;j<GetNBin();j++)
  {
    Int_t did=j+1;
    Int_t ibin = hits->FindBin((Float_t)did);
		Float_t y = hits->GetBinContent(ibin);
    if(GetStatus(did)==1 && y>0)  FillSpectra(did,y);

  }    
  Float_t x,y,z;
  GetOccupancy(GetMinHits(),&x,&y,&z);  
  cout   <<"EQUALIZATION: Avg Nevents/bin = "<<x<<" +- "<<y<<"\n";
  cout   <<"EQUALIZATION: fraction of bins nevents > minimum = "<<z<<"\n";
  if(z<GetMinOcc()) return kFALSE;  // minimum occupancy not reached yed...
  return kTRUE;

}
//_____________________________________________________________________________
Bool_t StEmcEqualSpectra::Equalize()
{ 
  cout <<"****************** Equalizing spectra \n";
  Int_t ndiv=GetNEtaBins();
  
  Float_t sum=0,nb=0;
  for(Int_t i=1;i<=GetNBin();i++)
    if(GetStatus(i)>0 && GetSum(i)>=GetMinHits())
    {
      Float_t mean,rms;
      GetLogMeanAndRms(i,mEqualMin,mEqualMax,&mean,&rms);
      sum+=mean;
      nb++;
    }
  if(nb==0) return kFALSE;
  Float_t MEAN=sum/nb; // global mean value used to choose reference spectra
  
  for(Int_t i=1;i<=ndiv;i++)
  {
    cout<<"StEmcCalibrationMaker::Equalize() - Equalizing EtaBin "<< i<<endl;
    sum=0;
    nb=0;
    Int_t mi,mf,ei,ef;
    CalcEtaBin(i,&mi,&mf,&ei,&ef);
    Int_t numberReady=0;
    for(Int_t m=mi;m<mf+1;m++)
      for(Int_t e=ei;e<ef+1;e++)
        for(Int_t s=1;s<GetNSub()+1;s++)
        {
          Int_t id1=GetID(m,e,s); 
          if(GetStatus(id1)>0 && GetSum(id1)>=GetMinHits()) 
          {
            numberReady++;
            Float_t mean,rms;
            GetMeanAndRms(id1,mEqualMin,mEqualMax,&mean,&rms);
            sum+=mean;
          }
        }      

    if(numberReady>0)
    {
      Float_t LOCALMEAN = sum/numberReady;
      Float_t dmean=5000,refmean=0;
      Int_t ref=0;
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<GetNSub()+1;s++)
          {
            Int_t id1=GetID(m,e,s); 
            if(GetStatus(id1)>0 && GetSum(id1)>=GetMinHits())
            {
              Float_t mean,rms;
              GetMeanAndRms(id1,mEqualMin,mEqualMax,&mean,&rms);
              Float_t dmean1=fabs(mean-LOCALMEAN);
              if(dmean1<dmean) {dmean=dmean1; refmean=mean; ref=id1;}
            }
          }
      cout   <<"***** Ref spectrum for etabin = "<<i
             <<"  ref = "<<ref<<"  mean = "<<refmean<<"  Global mean = "<<MEAN
             <<"  Local mean = "<<LOCALMEAN<<"\n";
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<GetNSub()+1;s++)
          {
            Int_t id1=GetID(m,e,s);
            if(GetStatus(id1)>0 && GetSum(id1)>=GetMinHits()) 
              Equalize(ref,id1,mEqualMode);
          }
    }
    else
    {
      cout   <<"***** No Equalization done for etabin  = "<<i<<endl;
    }
  }
	return kTRUE;
} 
