/**********************************************************************
* StEmcSpectra
* Author: Alexandre A. P. Suaide 
*
* This is a general EMCSpectra class
***********************************************************************/
#include "StEmcSpectra.h"
#include <iostream.h>
#include <math.h>
#include "emc_def.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "TString.h"
#include "TFile.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"

ClassImp(StEmcSpectra);
//_____________________________________________________________________________
const char* StEmcSpectra::GetDetName()   
{ 
  #include "StEmcUtil/others/emcDetectorName.h"
  return detname[mDetNum]; 
}
//_____________________________________________________________________________
StEmcSpectra::StEmcSpectra(const char* cdet,Int_t nb, Float_t bin0, Float_t bin1):St_DataSet(cdet)
{
  #include "StEmcUtil/others/emcDetectorName.h"
  if(detname) {/*touch*/}
  Float_t nadc[4]={4096,1024,1024,1024};
  SetTitle(cdet);
  for(Int_t i=0;i<MAXHIST;i++) mHistTmp[i]=NULL;
  for(Int_t i=0;i<MAXHIST;i++) mHistTmpAsym[i]=NULL;
  //geo=new StEmcGeom(cdet);
  mGeo=StEmcGeom::getEmcGeom(cdet);
  for(Int_t i=0;i<4;i++) if(!strcmp(cdet,detname[i].Data())) mDetNum=i;
  if(nb==0) 
	{
		mSpecMax=nadc[mDetNum]; 
		mSpecMin=0;
		mSpecBins=(Int_t)mSpecMax;
	}
	else
	{
		mSpecMax=bin1;
		mSpecMin=bin0;
		mSpecBins=nb;
	}
  mNModules=mGeo->NModule();
  mNEta=mGeo->NEta();
  mNSub=mGeo->NSub();
  mNBins=mNModules*mNEta*mNSub;
	mMinMult = 0;
	mMaxMult = 50000;
	mMinOcc = 0.9496;
	mMinHits = 4000;
}
//_____________________________________________________________________________
void StEmcSpectra::Init()
{
  #include "StEmcUtil/others/emcDetectorName.h"
  if(detname) {/*touch*/}
  if(mSpectra) delete mSpectra;
	if(mSum) delete mSum;
	
	mSpectra = new TH2F("mSpec",",mSpec",mNBins,1,mNBins+1,mSpecBins,mSpecMin,mSpecMax);
	mSum = new TH1F("mSum","mSum",mNBins,1,mNBins+1);
	mEventCount = new TH1F("mEventCount","mEventCount",2,0,2);
  	
	mRebinTmp = new TH1D("mRebin","mRebin",mSpecBins,mSpecMin,mSpecMax);
	mEtaBinTmp = new TH1D("mRebin","mRebin",mSpecBins,mSpecMin,mSpecMax);

  ZeroAll();
  return;
}
//_____________________________________________________________________________
StEmcSpectra::~StEmcSpectra()
{ 
  if(mSpectra) delete mSpectra;
	if(mSum) delete mSum;
  if(mEventCount) delete mEventCount;
	if(mRebinTmp) delete mRebinTmp;
	if(mEtaBinTmp) delete mEtaBinTmp;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::ZeroAll()
{
  if(mSpectra) mSpectra->Reset();
	if(mSum) mSum->Reset();
  
  return kTRUE;
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetID(Int_t m,Int_t e,Int_t s)
{
  Int_t position;
  mGeo->getId(m,e,s,position);
  return position;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::FillSpectra(Int_t position,Float_t bin)
{
  if(GetStatus(position)==0) return kFALSE;
    
  if(bin<mSpecMin || bin>=mSpecMax) return kFALSE;
  if(position<1 || position>mNBins) return kFALSE;
  
	mSum->Fill((Float_t)position);
	mSpectra->Fill((Float_t)position,bin);
  return kTRUE;
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetAdcValue(Int_t position,Float_t bin)
{
  if(bin<mSpecMin || bin>=mSpecMax) return 0;
  if(position<1 || position>mNBins) return 0;
  
	Int_t ibin = mSpectra->FindBin((Float_t)position,bin);
  return mSpectra->GetBinContent(ibin);
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetStatus(Int_t position) 
{ 
  if(!mIsOnOff) return 0;
	if(position<1 || position>mNBins) return 0;
  Int_t ibin = mIsOnOff->FindBin((Float_t)position);
  return (Int_t)mIsOnOff->GetBinContent(ibin);
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetSum(Int_t position) 
{ 
  if(position<1 || position>mNBins) return 0;
  Int_t ibin = mSum->FindBin((Float_t)position);
  return mSum->GetBinContent(ibin);
}
//_____________________________________________________________________________
Float_t StEmcSpectra::GetMaximum(Int_t position)
{
  if(position<1 || position>mNBins) return 0;
  TH1D *h = GetSpectra(position);
  Int_t ibin = h->GetMaximumBin();
  Float_t x = (Float_t)h->GetBinLowEdge(ibin);
  return x;
}
//_____________________________________________________________________________
void StEmcSpectra::Fit(Int_t position,TF1* func)
{
  if(position<1 || position>mNBins) return;
  TH1D *h = GetSpectra(position);
  h->Fit(func);
  return;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetMeanAndRms(Int_t position,Float_t* m,Float_t* r)
{
  return GetMeanAndRms(position,mSpecMin,mSpecMax,m,r);
}

//_____________________________________________________________________________
Bool_t StEmcSpectra::GetMeanAndRms(Int_t position,Float_t amin,Float_t amax,
                                   Float_t* m,Float_t* r)
{
  Bool_t ok = kTRUE;
  if(GetStatus(position)==0) ok = kFALSE;
  
  Float_t mean=0,rms=0,sum=0;
  
  if(GetSum(position)>0)
  {
	  TH1D *tmp  = GetSpectra(position);
	  Int_t bin0 = tmp->FindBin(amin);
	  Int_t bin1 = tmp->FindBin(amax);
    for(Int_t j=bin0;j<bin1;j++)
    {
      Float_t w = tmp->GetBinContent(j);
		  Float_t x = tmp->GetBinCenter(j);
      mean += w*x;
      sum  += w;
      rms  += x*x*w;
    }
    if(sum>0)
    {
      mean /= sum;
      rms   = sqrt(rms/sum-mean*mean);
    } else ok = kFALSE;
  }
  *m    = mean;
  *r    = rms;
  return ok;

}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetLogMeanAndRms(Int_t position,Float_t amin,Float_t amax,
                                      Float_t* m,Float_t* r)
{
  Bool_t ok = kTRUE;
  if(GetStatus(position)==0) ok = kFALSE;
  Float_t mean=0,rms=0,sum=0;
  
  if(GetSum(position)>0)
  {
	  TH1D *tmp  = GetSpectra(position);
	  Int_t bin0 = tmp->FindBin(amin);
	  Int_t bin1 = tmp->FindBin(amax);
    for(Int_t j=bin0;j<bin1;j++)
    {
      Float_t w = tmp->GetBinContent(j);
			if(w==1) w=1.5;
		  if(w>0) w = log(w);
		  Float_t x = tmp->GetBinCenter(j);
      mean += w*x;
      sum  += w;
      rms  += x*x*w;
    }
    if(sum>0)
    {
	    mean /= sum;
      rms   = sqrt(rms/sum-mean*mean);
    } else ok = kFALSE;
  }
  *m    = mean;
  *r    = rms;
  return ok;

}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetOccupancy(Float_t minimum,Float_t* m,
                                  Float_t* r,Float_t* fr)
{
  Float_t avg=0,sigma=0,number=0,temp=0;
  for(Int_t id=1;id<=mNBins;id++)
  {
    Int_t status=GetStatus(id);
		if(status!=0) 
    {
      Float_t s;
      s=GetSum(id);
      if (s>minimum) number++;
      avg+=s;
      sigma+=s*s;
      temp++;
    }
  }
  if(temp==0) return kFALSE;
  avg=avg/temp;
  sigma = sigma/temp-avg*avg;
  if(sigma<0) sigma=0;
  sigma=sqrt(sigma);
  number=number/temp;
  *m=avg;
  *r=sigma;
  *fr=number;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcSpectra::GetOccupancyEtaBin(Float_t minimum,Float_t* m,
                                        Float_t* r,Float_t* fr)
{
  Float_t avg=0,sigma=0,number=0,temp=0;
  for(Int_t bin=1;bin<=mNEtaBins;bin++)
  {
    Float_t s;
    s=GetSumEtaBin(bin);
    if(s>0)
    {
     if (s>=minimum) number++;
     avg+=s;
     sigma+=s*s;
     temp++;
    }
    cout <<"etabin = "<<bin<<"  sum = "<<s<<"  n = "<<temp<<endl;
  }
  if(temp==0) return kFALSE;
  avg=avg/temp;
  sigma=sqrt(sigma/temp-avg*avg);
  number=number/temp;
  *m=avg;
  *r=sigma;
  *fr=number;
  return kTRUE;
}

//_____________________________________________________________________________
TH1D* StEmcSpectra::Draw(Int_t position)
{
  if(GetStatus(position)==0)
  {
    cout <<"***** StEmcSpectra: This position is turned off. Can not DRAW\n"; 
    return NULL;
  }
  TCanvas* canvas1=new TCanvas("canvas1","EMC Spectra",500,350);
  char title[80];
  sprintf(title,"EMC Spectrum channel %05d",position);
  TH1D* hist=GetSpectra(position);
  canvas1->cd();
  hist->SetFillColor(11);
  hist->Draw();
  return hist;
}

//_____________________________________________________________________________
void StEmcSpectra::DrawOccupancy()
{
  TCanvas* canvas4=new TCanvas("canvas4","EMC Occupancy",500,700);
  canvas4->Divide(1,2);
  canvas4->cd(1);
  mSum->Draw();
	Float_t min = mSum->GetMinimum();
	Float_t max = mSum->GetMaximum();
  Int_t nc=100;
  TH1F* hist2=new TH1F("hist2","Occupancy distribution",nc,min-1,max+1);
  for(Int_t j=1;j<=mNBins;j++) 
  {
    Float_t k=0;
    if (GetStatus(j)) 
    {
      k=GetSum(j);
      hist2->Fill(k);
    }
  }
  canvas4->cd(2);
  hist2->Draw();
}
//_____________________________________________________________________________
TH1D* StEmcSpectra::DrawEtaBin(Int_t etabin)
{
  if(etabin>mNEtaBins || etabin<1) return NULL;
  TCanvas* canvas7=new TCanvas("canvas7","EMC Eta Bin Spectrum",500,350);  
  TH1D* hist=GetEtaBinSpectra(etabin);
  canvas7->cd();
  hist->SetFillColor(11);
  hist->Draw();
	return hist;
}
//_____________________________________________________________________________
void StEmcSpectra::DrawAllEtaBin(Int_t etabin,Float_t Norm)
{
  if(etabin>mNEtaBins || etabin<1) return;
  if(!mEqual) return;
  TCanvas* canvas8=new TCanvas("canvas8","EMC Eta Bin Spectrum",500,700);  
  TH1D* hist=GetEtaBinSpectra(etabin); if(hist){/*unused*/};
  canvas8->Divide(1,2);
  canvas8->cd(1);

  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,&mi,&mf,&ei,&ef);
  si=1; sf=GetNSub();
  Int_t nh = 0;

  Float_t scale1 = 0;
  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(GetStatus(id)==1)
        {
          Float_t a=1;
					Float_t b=0;
          Int_t ss;
				  GetEqualConst(id,&a,&b,&ss);        
          if(a!=0 && ss==1) 
          {
            if(mHistTmp[nh]) {delete mHistTmp[nh]; mHistTmp[nh] = NULL;}
            if(nh>0) if(mHistTmpAsym[nh]) {delete mHistTmp[nh]; mHistTmp[nh] = NULL;}
            mHistTmp[nh] = new TH1D(*ReBin(id,a,b));
            mHistTmp[nh]->Rebin(4);
            char name[30];
            sprintf(name,"id%4d",id);
            mHistTmp[nh]->SetName(name);
            mHistTmp[nh]->SetTitle("Equalized Spectra");
            Float_t scale = mHistTmp[nh]->Integral(mHistTmp[nh]->FindBin(Norm),mHistTmp[nh]->GetNbinsX());
            mHistTmp[nh]->Scale(1./scale);
            if(nh==1) scale1=scale;
            canvas8->cd(1);
            if(nh==0) mHistTmp[nh]->Draw(); else  mHistTmp[nh]->Draw("same");
            if(nh>0) 
            {
              TH1D* tmp = new TH1D(*mHistTmp[0]);
              TH1D* tmp2= new TH1D(*mHistTmp[0]);
              tmp->Add(mHistTmp[nh],-1);
              tmp2->Add(mHistTmp[nh],1);
              tmp->Divide(tmp2);
              delete tmp2;
              mHistTmpAsym[nh] = tmp;
              mHistTmpAsym[nh]->SetName(name);
              mHistTmpAsym[nh]->SetTitle("Asymmetry");
              canvas8->cd(2);
              if(nh==1) mHistTmpAsym[nh]->Draw("P"); else  mHistTmpAsym[nh]->Draw("sameP");
            }
            nh++;
          }
        }
      }
      for(int i=1;i<=mHistTmp[1]->GetNbinsX();i++)
      {
        Float_t y = scale1*mHistTmp[1]->GetBinContent(i);
        Float_t ey = sqrt(y)/scale1;
        mHistTmp[1]->SetBinError(i,ey);
      }
      canvas8->cd(1);
      mHistTmp[1]->SetLineColor(2);
      mHistTmp[1]->SetMarkerStyle(20);
      mHistTmp[1]->SetMarkerSize(0.7);
      mHistTmp[1]->SetMarkerColor(2);
      mHistTmp[1]->SetLineWidth(2);
      mHistTmp[1]->Draw("sameELP");
      canvas8->cd(2);
      mHistTmpAsym[1]->SetLineColor(2);
      mHistTmpAsym[1]->SetLineWidth(2);
      mHistTmpAsym[1]->Draw("sameL");

	return ;
}
//_____________________________________________________________________________
TH1D* StEmcSpectra::GetSpectra(Int_t position)
{
  return GetSpectra(position,1,0);
}
//_____________________________________________________________________________
TH1D* StEmcSpectra::GetSpectra(Int_t position,Float_t a,Float_t b)
{
  return ReBin(position,a,b);
}
//_____________________________________________________________________________
TH1D* StEmcSpectra::ReBin(Int_t position,Float_t a,Float_t b)
{
	Float_t seg = 50;
  if(a<=0) return NULL;	
	TH1D *h = mSpectra->ProjectionY("tmp",(Int_t)position,(Int_t)position);	
	mRebinTmp->Reset();
	
	if(a==1 && b==0) 
	{
		mRebinTmp->Add(h,1);
		delete h;
		return mRebinTmp;
	}
  
	Float_t deltaBin=h->GetBinWidth(1)/seg;
	Int_t nbins=h->GetNbinsX();	
  for(Int_t i=1;i<=nbins;i++)
  {
    Float_t x = h->GetBinLowEdge(i);
		Float_t y = h->GetBinContent(i);
		for(Int_t j=0;j<(Int_t)seg;j++)
		{
			Float_t x1 = x+((Float_t)j+0.5)*deltaBin;
			Float_t x2 = x1*a+b;
      //if (a>1) cout <<position<<"  "<<a<<"  "<<x1<<"  "<<x2<<endl;
			mRebinTmp->Fill(x2,y/seg);
		}
  }
  delete h;
	return mRebinTmp;
}
//_____________________________________________________________________________
void StEmcSpectra::CalcEtaBin(Int_t i,Int_t* mi,Int_t* mf,Int_t* ei,Int_t* ef)
{
  Int_t eei=1;
  Int_t eef=eei+mEtaBinWidth-1;
  Int_t mmi=1;
  Int_t mmf=60;
  
  for(Int_t j=1;j<=mNEtaBins;j++)
  {
    if (i==j) goto etabinok;
    eei=eef+1;
    if(eei>mNEta) {eei=1;mmi=61; mmf=120;}
    eef=eei+(Int_t)mEtaBinWidth-1;
    if(eef>mNEta) eef=mNEta;
  }
  etabinok:
  *ei=eei; *ef=eef;
  *mi=mmi; *mf=mmf;
  return;
}

//_____________________________________________________________________________
TH1D* StEmcSpectra::GetEtaBinSpectra(Int_t etabin, Int_t mode)
{
  if(etabin>mNEtaBins || etabin<1) return NULL;
  if(!mEqual && mode ==0) return NULL;
	mEtaBinTmp->Reset();
  
  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,&mi,&mf,&ei,&ef);
  si=1; sf=GetNSub();

  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(GetStatus(id)==1)
        {
          Float_t a=1;
					Float_t b=0;
          Int_t ss;
					if(mode==0)
					{
						Float_t a,b;
						GetEqualConst(id,&a,&b,&ss);
					}
          if(a!=0 && ss==1) mEtaBinTmp->Add(ReBin(id,a,b),1); 
        }
      }
  
	return mEtaBinTmp;
} 
//_____________________________________________________________________________
Float_t StEmcSpectra::GetSumEtaBin(Int_t etabin)
{
  if(etabin>mNEtaBins || etabin<1) return 0;
  Int_t mi,mf,ei,ef,si,sf;
  CalcEtaBin(etabin,&mi,&mf,&ei,&ef);
  si=1; sf=GetNSub();
  
  Float_t sum=0;
  for(Int_t m=mi;m<=mf;m++)
    for(Int_t e=ei;e<=ef;e++)
      for(Int_t s=si;s<=sf;s++)
      {
        Int_t id=GetID(m,e,s);
        if(GetStatus(id)==1) sum+=GetSum(id);
      }
  return sum;
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetEtaBinId(Int_t m,Int_t e)
{
  for(Int_t etabin=1;etabin<=mNEtaBins;etabin++)
  {
    Int_t mi,mf,ei,ef;
    CalcEtaBin(etabin,&mi,&mf,&ei,&ef);
    if (m>=mi && m<=mf && e>=ei && e<=ef) return etabin;
  }
  return -1;
}
//_____________________________________________________________________________
void StEmcSpectra::SaveAll(char *filename)
{
  TFile *f=new TFile(filename,"RECREATE");
	mSpectra->Write();
	mSum->Write();
  mEventCount->Write();
  f->Close();
	delete f;
	return;
}
//_____________________________________________________________________________
void StEmcSpectra::LoadAll(char *filename)
{
  TFile *f=new TFile(filename,"READ");
	TString a = "mSpec;1";
	TH2F *h1=(TH2F*)f->Get(a.Data());
	if(h1) mSpectra->Add(h1,1);
	a = "mSum;1";
	TH1F *h2=(TH1F*)f->Get(a.Data());
	if(h2) mSum->Add(h2,1);
	a = "mEventCount;1";
	TH1F *h3=(TH1F*)f->Get(a.Data());
	if(h3) mEventCount->Add(h3,1);
	f->Close();
	delete f;
	return;
}
//_____________________________________________________________________________
Int_t StEmcSpectra::GetNTracks(StEvent* event)
{
  Int_t NTracks =0 ;
  if(mDoUseL3) // use L3Tracks
  {
    if(event->l3Trigger())
    {
      StSPtrVecTrackNode& tracks=event->l3Trigger()->trackNodes();
      NTracks=tracks.size();
    }
  }
  else  
  {
    StSPtrVecTrackNode& tracks=event->trackNodes();
    NTracks=tracks.size();
  }
  
	return NTracks;
}
//_____________________________________________________________________________
StTrack* StEmcSpectra::GetTrack(StEvent *event,Int_t i)
{
  StTrack* track = NULL;
  if(mDoUseL3) 
  {
    StSPtrVecTrackNode& tracks =event->l3Trigger()->trackNodes();
    track=tracks[i]->track(0);
  }
  else 
  {
    StSPtrVecTrackNode& tracks=event->trackNodes();
    track=tracks[i]->track(0);
  }
  return track;
}
//_____________________________________________________________________________
void StEmcSpectra::SetEqualConst(Int_t position, Float_t a, Float_t b, Int_t status)
{
	if(!mEqual) return;
	Int_t ibin=mEqual->FindBin((Float_t)position,0.0);
	mEqual->SetBinContent(ibin,0);
	ibin=mEqual->FindBin((Float_t)position,1.0);
	mEqual->SetBinContent(ibin,0);
		
	mEqual->Fill((Float_t)position,0.0,a);
	mEqual->Fill((Float_t)position,1.0,b);
	mEqual->Fill((Float_t)position,9.0,(Float_t)status);
	return ;
}
//_____________________________________________________________________________
void StEmcSpectra::GetEqualConst(Int_t position, Float_t *a, Float_t *b,Int_t* status)
{
	*a=0; *b=0; *status=0;
	if(!mEqual) return;
	Int_t ibin=mEqual->FindBin((Float_t)position,0.0);
	*a=mEqual->GetBinContent(ibin);
	ibin=mEqual->FindBin((Float_t)position,1.0);
	*b=mEqual->GetBinContent(ibin);
	ibin=mEqual->FindBin((Float_t)position,9.0);
  *status =(Int_t) mEqual->GetBinContent(ibin);
	return;
}
