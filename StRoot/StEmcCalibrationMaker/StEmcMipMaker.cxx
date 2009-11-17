#include "StEmcMipMaker.h"
#include "TFile.h"
#include "TROOT.h"
#include "TF1.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"
#include "tables/St_emcCalib_Table.h"
#include "StEmcEqualMaker.h"
#include "TCanvas.h"

ClassImp(StEmcMipMaker)

//_____________________________________________________________________________
StEmcMipMaker::StEmcMipMaker(const char *name):StEmcCalibMaker(name)
{
  mNPoints = 10;
  mPmin = 1.0;
  setRange(200);
  for(int i=0;i<10;i++) mNFailed[i]=0;
  mMipPos 	= 0;
  mMipPosErr 	= 0;
  mMipWid 	= 0;
  mMipWidErr 	= 0;
  mGain 	= 0;
  mGainDistr 	= 0;
  mChi2 	= 0;
  mIntegral 	= 0;
                            
  funcFit	= 0;       
  funcFitPeak	= 0;       
  funcFitBack	= 0;       
              
}
//_____________________________________________________________________________
StEmcMipMaker::~StEmcMipMaker()
{
  delete mMipPos;
  delete mMipPosErr;
  delete mMipWid;
  delete mMipWidErr;
  delete mGain;
  delete mGainDistr;
  delete mChi2;
  delete mIntegral;
                            
  delete funcFit;       
  delete funcFitPeak;       
  delete funcFitBack;       
              
}
//_____________________________________________________________________________
Int_t StEmcMipMaker::Init()
{
  mMipPos = new TH1F("mMipPos","",getNChannel(),0.5,getNChannel()+0.5);
  mMipPosErr = new TH1F("mMipPosErr","",getNChannel(),0.5,getNChannel()+0.5);
  mMipWid = new TH1F("mMipWid","",getNChannel(),0.5,getNChannel()+0.5);
  mMipWidErr = new TH1F("mMipWidErr","",getNChannel(),0.5,getNChannel()+0.5);
  mGain = new TH1F("mGain","",getNChannel(),0.5,getNChannel()+0.5);
  mGainDistr = new TH1F("mGainDistr","",100,0,0.1);
  mChi2 = new TH1F("mChi2","",getNChannel(),0.5,getNChannel()+0.5);
  mIntegral = new TH1F("mIntegral","",getNChannel(),0.5,getNChannel()+0.5); 

  mSpecName="mSpecMip";
  mAcceptName = "mAcceptMip";
  
  return StEmcCalibMaker::Init();
}
//_____________________________________________________________________________
void StEmcMipMaker::Clear(Option_t *option)              
{
}
//_____________________________________________________________________________
Int_t StEmcMipMaker::Make()
{  
  if(!accept()) return kStOk;

  cout <<"Number of tracks = "<<getCalib()->getNTracks()<<endl;      
  for(int i=0;i<getCalib()->getNTracks();i++)
  {
    int id   = getCalib()->getTrackTower(i);
    int id2  = getCalib()->getTrackTowerExit(i);
    float p  = getCalib()->getTrackP(i); 
    //int np   = mCalib->getTrackNPoints(i);
    float adc = (float) getCalib()->getADCPedSub(1,id);
    //cout <<"candidate. p = "<<p<<" Tower id = "<<id<<"  ADC = "<<adc<<endl;
    if(p>mPmin && id!=0 && id2==id /*&& mCalib->isIsolatedTower(id)*/) 
    {
      int nt  = getCalib()->getNTracksInTower(id); 
      float rms = getCalib()->getPedRms(1,id);
      if(adc!=0 && adc>2*rms && nt==1) 
      {
        cout <<"Found MIP. p = "<<p<<" Tower id = "<<id<<"  ADC = "<<adc<<endl;
        fill(id,adc);
      }
    }
  }
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcMipMaker::Finish()
{
  saveHist((char*)mFileName.Data());
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcMipMaker::fit(TH1F* h)
{
  float WIDTH = 10;
  float max = h->GetBinCenter(h->GetMaximumBin());
  max = 40;
  if(max<15) WIDTH = 3;
  if(max<5) max = 15;
  float maxy = h->GetBinContent(h->GetMaximumBin());
  float maxback = h->GetBinContent((int)(h->GetMaximumBin()+6*WIDTH));
      
  int NPars = 6;
  float xmin = 7;
  float xmax = (max*3>120) ? max*3 : 120;
  
  float p[] = {maxy-maxback,max,WIDTH,maxback,0,50};
  
  delete funcFitPeak;
  funcFitPeak = new TF1("peak","gaus(0)",0,1000);
  funcFitPeak->SetLineColor(2);
  funcFitPeak->SetLineWidth(1);
  delete funcFitBack;
  funcFitBack = new TF1("back","gaus(0)",0,1000);
  funcFitBack->SetLineWidth(1);
  funcFitBack->SetLineColor(3);
  delete funcFit;
  funcFit = new TF1("func","gaus(0)+gaus(3)",0,1000);
  funcFit->SetLineWidth(1);
  funcFit->SetLineColor(4);

  TF1 *func = funcFit; 
  func->SetRange(xmin,xmax);
  
  for(int i=0;i<NPars;i++) func->SetParameter(i,p[i]);
  func->SetParLimits(0,0,10000);
  func->SetParLimits(3,0,10000);
  func->SetParLimits(5,30,100);
  
  h->Fit(func,"RNQ");
    
  int nb = h->FindBin(xmax);
  for(int i=h->FindBin(xmin);i<nb;i++)
  {
    //float x = h->GetBinCenter(i);
    float y = h->GetBinContent(i);
    float stat = sqrt(y);
    //float binwidth = h->GetBinWidth(i);
    //float deriv  = (float) func->Derivative(x);
    float sigma = sqrt(stat*stat);//+(deriv*binwidth/2)*(deriv*binwidth/2));
    h->SetBinError(i,sigma);
  }
  
  h->Fit(func,"RNQ");
  
  return; 
}
//_____________________________________________________________________________
TH1F* StEmcMipMaker::findMip(int eta0,int eta1,StEmcEqualMaker* equal)
{
  if(!equal) return NULL;
  TH1F* h = equal->getEtaBinSpec(eta0,eta1,getSpec());
  char name[60];
  sprintf(name,"Mip.%d.%d",eta0,eta1);
  h->SetName(name);
  sprintf(name,"Mip for %d <= eta <= %d",eta0,eta1);
  h->SetTitle(name);
  if(h) fit(h);
  
  TF1* func = funcFit;
  
  int NPars = 6;
  
  for(int i=0;i<NPars;i++)
  {
      float a = func->GetParameter(i);
      if(i<3) funcFitPeak->SetParameter(i,a); else funcFitBack->SetParameter(i-3,a);
  }
  
  float chi=func->GetChisquare()/(func->GetNDF());
  if(chi<0 || chi>1000) chi = 1000;
  float peak   = func->GetParameter(1);
  if(peak<5) peak = 0 ;
  float epeak  = func->GetParError(1);
  float w      = func->GetParameter(2);
  float ew     = func->GetParError(2);
  //float XMAX   = func->GetMaximumX(7,200);
  //if(fabs(XMAX-peak)>2)  chi = 0;
  
  int m1 = 1;
  int m2 = 60;
  if(eta0<0 && eta1<0) {m1 = 61; m2 = 120;}
  if(eta0*eta1<0) {m1 = 1; m2 = 120;}
  eta0 = abs(eta0);
  eta1 = abs(eta1);
  
  StEmcGeom *geo = getGeom();
  int ns = geo->NSub();
  
  for(int m = m1;m<=m2;m++)
    for(int e = eta0; e<=eta1; e++)
      for(int s = 1; s<=ns; s++)
      {
        int id;
        geo->getId(m,e,s,id);
        float a = equal->getA()->GetBinContent(id);
        float b = equal->getB()->GetBinContent(id);
        
        if(a>10)  {a = 0; b = 0;}
        if(a<0.1) {a = 0; b = 0;}
        
        mMipPos->SetBinContent(id,peak*a+b);
        mMipPosErr->SetBinContent(id,epeak*a);
        mMipWid->SetBinContent(id,w*a);
        mMipWidErr->SetBinContent(id,ew*a);
        mChi2->SetBinContent(id,chi);
        mIntegral->SetBinContent(id,0);
        cout <<"MIP peak = "<<peak<<"+-"<<epeak<<"  width = "<<w<<"+-"<<ew<<endl;
        cout <<"Final chi2 = "<<chi<<endl;
        findGain(id,true);
      }
  return h;
}
//_____________________________________________________________________________
TH1F* StEmcMipMaker::findMip(int id, int rebin, bool print)
{
  float mip = 0;
  float width =0;
  float chi = 0;
  delete funcFitPeak; funcFitPeak = NULL;
  delete funcFitBack; funcFitBack = NULL;
  delete funcFit    ; funcFit     = NULL;
  if(!mSpec) return NULL;
  if(gROOT->FindObject("id")) delete (TH1D*)gROOT->FindObject("id");
  TH1F *h = (TH1F*)mSpec->ProjectionY("id",id,id);
  if(!h) { mNFailed[0]++; return NULL;}
  h->Rebin(rebin);
  
  float integral= h->Integral();
  if(integral==0) { mNFailed[0]++; mNFailed[1]++; return h;}
  
  if(print) cout <<"Fitting MIP for id = "<<id<<endl;
  if(print) cout <<"Integral = "<<integral<<endl;
  
  fit(h);
  
  TF1 *func = funcFit;
  
  chi=func->GetChisquare()/(func->GetNDF());
  if(chi<0 || chi>1000) chi = 1000;
  
  int NPars = 6;
  
  for(int i=0;i<NPars;i++)
  {
      float a = func->GetParameter(i);
      if(i<3) funcFitPeak->SetParameter(i,a); else funcFitBack->SetParameter(i-3,a);
  }
  
  float peak   = func->GetParameter(1);
  if(peak<5) peak = 0 ;
  float epeak  = func->GetParError(1);
  float w      = func->GetParameter(2);
  float ew     = func->GetParError(2);
  float XMAX   = func->GetMaximumX(7,200);
  if(fabs(XMAX-peak)>2) 
  {
    mNFailed[0]++; 
    mNFailed[3]++; 
    if(print) cout << "****************** FAILED fabs(XMAX-peak) *****************\n";
    if(print) cout << "fabs(XMAX-peak) = "<<fabs(XMAX-peak)<<endl;
    chi = 0;
  }
  mMipPos->Fill(id,peak);
  mMipPosErr->Fill(id,epeak);
  mMipWid->Fill(id,w);
  mMipWidErr->Fill(id,ew);
  mChi2->Fill(id,chi);
  mIntegral->Fill(id,integral);
  if(print) cout <<"MIP peak = "<<peak<<"+-"<<epeak<<"  width = "<<w<<"+-"<<ew<<endl;
  if(print) cout <<"Final chi2 = "<<chi<<endl;
  mip = peak;
  width = w;
  return h;  
}
//_____________________________________________________________________________
float StEmcMipMaker::findGain(int id,bool print)
{
  float mip = getMipPosition(id);
  if(mip==0) return 0;
  StEmcGeom *geom = StEmcGeom::instance("bemc");
  float eta,phi;
  geom->getEtaPhi(id,eta,phi);
  float theta=2.*atan(exp(-eta));
  float EoverMIP = 0.261;
  float MipE = EoverMIP*(1.+0.056*eta*eta)/sin(theta);
  float gain = MipE/mip;
  float chi  = getChi2(id);
  if(chi<0.001 || chi>30) gain = 0;
  if(gain <0 || gain > 10000) gain = 0;
  if(gain==0) mNFailed[4]++;
  mGain->Fill(id,gain);
  mGainDistr->Fill(gain);
  if(print) cout <<"gain for id "<<id<<" = "<<gain<<endl;
  if(print) cout <<"-------------------------------------------------------\n";
  return gain;
}
//_____________________________________________________________________________
void StEmcMipMaker::mipCalib()
{
  for(int id = 0;id<4800;id++)
  {
    if((id-1)%100==0) cout <<"Doing MIP calibration for id "<<id<<endl;
    delete findMip(id,3,false);
    delete funcFit;
    delete funcFitPeak;
    delete funcFitBack;
    findGain(id,false);
  }
}
//_____________________________________________________________________________
void StEmcMipMaker::mipCalib(int eta0, int eta1, int etabin, StEmcEqualMaker* equal, bool draw)
{
  TCanvas *c1 = NULL; 
  //int ne = (eta1-eta0+1)/etabin;
  int pad = 1;
  for(int e = eta0; e<=eta1;e++)
  {
    TH1F *h = findMip(e,e+etabin-1,equal);
    if(draw)
    {
      if(pad==1) {c1 = new TCanvas(); c1->Divide(2,2);}
      c1->cd(pad);
      h->Draw();
      funcFitBack->Draw("same");
      funcFitPeak->Draw("same");
      funcFit->Draw("same");
      pad++;
      if(pad>4) pad=1;
    }
    else
    {
      delete h;
      delete funcFit;
      delete funcFitPeak;
      delete funcFitBack;  
    }
  }  
}
//_____________________________________________________________________________
TH1F* StEmcMipMaker::compareToDb(char* timeStamp,int mode)
{
  
  StDbManager* mgr=StDbManager::Instance();
	StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
  StDbTable* table = node->addDbTable("bemcCalib");
  mgr->setRequestTime(timeStamp);
  mgr->fetchDbTable(table);
  emcCalib_st *calib = (emcCalib_st*) table->GetTable();
  if(!calib) return NULL;
  
  char line[100];
  sprintf(line,"Comparison with DB - %s",timeStamp);
  TH1F *h = NULL;
  if(mode == 0) h = new TH1F("h",line,4800,0.5,4800.5);
  else h = new TH1F("h",line,100,0,4);
  for(int i=0;i<4800;i++)
  {
    int id = i+1;
    float gain = calib[0].AdcToE[i][1];
    float gain2 = getGain(id);
    float ratio = 0;
    if(gain!=0) ratio = gain2/gain; 
    if(mode==0) h->Fill(id,ratio); else h->Fill(ratio);
  }
  return h;
}
//_____________________________________________________________________________
void StEmcMipMaker::saveToDb(char* timeStamp)
{   
  emcCalib_st tnew;
  for(int i=0;i<4800;i++)
  {
    int id = i+1;
    float gain = getGain(id);
    if(gain==0) tnew.Status[i] = 0; else tnew.Status[i] = 1;
    tnew.AdcToE[i][0] = 0;
    tnew.AdcToE[i][1] = gain;
    tnew.AdcToE[i][2] = 0;
    tnew.AdcToE[i][3] = 0;
    tnew.AdcToE[i][4] = 0;
  }   
  StDbManager* mgr=StDbManager::Instance();
	StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
  StDbTable* table = node->addDbTable("bemcCalib");
	table->SetTable((char*)&tnew,1);
  mgr->setStoreTime(timeStamp);
  mgr->storeDbTable(table);
  return;
}





