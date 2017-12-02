#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <map>
#include <string>
#include "TROOT.h"
#include "TClass.h"
#include "TH1F.h"
#include "TBrowser.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TString.h"
#include "StvDebug.h"
#include "StvGrappa.h"

typedef std::map<std::string, int>   myTallyMap_t;
typedef myTallyMap_t::const_iterator myTallyIter_t;
static  myTallyMap_t  mgTally;

int StvDebug::mgDebug=1; //0=no debug, 1=Normal, 2=count is on
StvGrappa *StvDebug::mgGrappa   = 0;
StvGrappa *StvDebug::mgGrappaTmp= 0;

typedef std::map<std::string, TH1*>   myDebMap_t;
typedef myDebMap_t::const_iterator myDebIter_t;
static  myDebMap_t  myDebMap;

typedef std::map<std::string, TCanvas*>   myCanMap_t;
typedef myCanMap_t::const_iterator myCanIter_t;
myCanMap_t myCanMap;

//______________________________________________________________________________ 
int StvDebug::Break(int key)
{ static int kto=-20102017;
  if (kto != key) return 0;
  printf ("BOT OHO %d\n",key);
  return 1;
}
//______________________________________________________________________________ 
int StvDebug::Break(double x,double y,double z)
{ static double myX=-9999,myY=-9999,myZ=-9999;
  if (fabs(x-myX)>0.2) return 0;
  if (fabs(y-myY)>0.2) return 0;
  if (fabs(z-myZ)>0.5) return 0;
  printf ("BOT OHO %g==%g %g==%g %g==%g\n",x,myX,y,myY,z,myZ);
  return 1;
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,double val)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,100,0.,0.);}
  h->Fill(val);
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,const char *val)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,0,0.,0.);}
  h->Fill(val,1.);
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,double val,double left,double rite)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,100,left,rite);}
  h->Fill(val);
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,double valx, double valy
                                    ,double leftx,double ritex
				    ,double lefty,double ritey)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,leftx,ritex,100,lefty,ritey);}
  h->Fill(valx,valy);
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,double valx,double valy)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,0.,0.,100,0,0);}
  h->Fill(valx,valy);
}
//______________________________________________________________________________ 
void StvDebug::Count(const char *key,const char *valx,double valy)
{
  if (mgDebug<2) return;
  TH2 *&h = (TH2*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,0.,0.,100,0,0);}
  h->Fill(valx,valy,1.);
}
//______________________________________________________________________________ 
//______________________________________________________________________________ 
//______________________________________________________________________________ 
//______________________________________________________________________________ 
#if 0
//______________________________________________________________________________ 
void StvDebug::Sumary(int)
{
  printf("StvDebug::Sumary()\n");
  TH1 *H[4];
  int nH = 0,n=0;
  for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
    TH1 *h = (*iter).second; n++;
    if (h->IsA()->InheritsFrom("TH2")) continue;

    int nEnt = h->GetEntries();
    double mean = h->GetMean();
    double rms  = h->GetRMS();
    printf("TH1 %2d - %12s:\t %5d %g(+-%g)\n",n,h->GetName(),nEnt,mean,rms);
    if (rms<=0) continue;
    if (nH==4) {Draw(nH,H);nH=0;}
    H[nH++] = h;
  }
  if (nH) Draw(nH,H);

  for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
    TH1 *h = (*iter).second; n++;
    if (!h->IsA()->InheritsFrom("TH2")) continue;
    int nEnt = h->GetEntries();
    double mean = h->GetMean();
    double rms  = h->GetRMS();
    printf("TH2 %2d - %12s:\t %5d %g(+-%g)\n",n,h->GetName(),nEnt,mean,rms);
    if (rms<=0) continue;
    H[0]=h;Draw(1,H);
  }

  if (!n) return;
  while(!gSystem->ProcessEvents()){}; 

}
#endif
#if 1
//______________________________________________________________________________ 
void StvDebug::Sumary(int nHist)
{
  if (!nHist) nHist=4;
  printf("StvDebug::Sumary()\n");

  int nH = 0,n=0;
  for (myTallyIter_t iter = mgTally.begin();iter != mgTally.end();++iter) {
    int nn = (*iter).second; n++;
    const char *kto = (*iter).first.c_str();
    printf("%3d - Tally.%s = %d\n",n,kto,nn);
  }




  TH1 *H[10];
  for (int numCha = 0; numCha<2; numCha++) {
    nH = 0;n=0;
    for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
      TH1 *h = (*iter).second; n++;
      if (!h->IsA()->InheritsFrom("TH1")) continue;
      if ( h->IsA()->InheritsFrom("TH2")) continue;

      int nEnt = h->GetEntries();
      if (!nEnt) continue;
      if ((h->GetXaxis()->GetLabels()==0) != (numCha == 0)) continue;
      double mean = h->GetMean();
      double rms  = h->GetRMS();
      printf("TH1 %2d - %12s:\t %5d %g(+-%g)\n",n,h->GetName(),nEnt,mean,rms);
      if (nH==nHist) {Draw(nH,H);nH=0;}
      if (numCha) h->LabelsOption(">V","X");
      H[nH++] = h;
    }
    if (nH) Draw(nH,H);
  }
  for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
    TH1 *h = (*iter).second; n++;
    if (!h->IsA()->InheritsFrom("TH2")) continue;
    int nEnt = h->GetEntries();
    if (!nEnt) continue;
    double mean = h->GetMean();
    double rms  = h->GetRMS();
    printf("TH2 %2d - %12s:\t %5d %g(+-%g)\n",n,h->GetName(),nEnt,mean,rms);
    H[0]=h;//Draw(1,H);
    TString ts(h->GetName()); ts+="_pfx";
    H[1] = ((TH2*)h)->ProfileX(ts.Data());
    Draw(2,H);
  }

  if (!n) return;
  while(!gSystem->ProcessEvents()){}; 

}
#endif
//______________________________________________________________________________ 
void StvDebug::Reset()
{
  for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
    ((*iter).second)->Clear(); 
  }
  for (myCanIter_t iter = myCanMap.begin();iter != myCanMap.end();++iter) {
    ((*iter).second)->Clear(); 
  }
}
//______________________________________________________________________________ 
void StvDebug::Draw(int nH,TH1** H)
{ 
static int nCall=0; nCall++;

  TString ts("C_"); ts += H[0]->GetName();
  TCanvas *&C = myCanMap[ts.Data()];
  if (!C) C = new TCanvas(ts.Data(),ts.Data(),600,800);
  C->Clear();C->Divide(1,nH);
  for (int i=0;i<nH;i++) { C->cd(i+1); H[i]->Draw(); }

  C->Modified();C->Update();
}
//______________________________________________________________________________ 
const char *StvDebug::Env(const char *key)
{
  return gSystem->Getenv(key);
}
//______________________________________________________________________________ 
int StvDebug::Inv(const char *key)
{
  return atoi(Env(key));
}
//______________________________________________________________________________
int  StvDebug::iFlag(const char *flagName,int dflt)
{
  const char *val = gSystem->Getenv(flagName);
  if (!val) return dflt;
  printf("\nStvDebug::iFlag: %s = %s\n\n",flagName,val);

  return atoi(val);
}

//______________________________________________________________________________
double StvDebug::dFlag(const char *flagName, double dflt)
{
  const char *val = gSystem->Getenv(flagName);
  if (!val) return dflt;
  printf("\nStvDebug::dFlag: %s = %s\n\n",flagName,val);

  return atof(val);
}
//______________________________________________________________________________
void StvDebug::Browse(const char *name, TObject *obj)
{
  auto *br = new TBrowser(name,obj);
  if (br){};
  return;
}
//______________________________________________________________________________
void StvDebug::AddGra(double x, double y,double z, int iObj)
{
   if (mgDebug<2) return;
   if (!mgGrappa)mgGrappa =  new StvGrappa("StvDebug");
   mgGrappa->Add(x,y,z,iObj);
}
//______________________________________________________________________________
void StvDebug::ClearGra()
{
   if (!mgGrappa) return;
   mgGrappa->Clear();
}
//______________________________________________________________________________
void StvDebug::ShowGra()
{
   if (!mgGrappa) return;
   mgGrappa->Show();
}
//______________________________________________________________________________
void StvDebug::SetActGra(int akt)
{
   mgGrappa->SetActive(akt);
}
//______________________________________________________________________________
void StvDebug::Show(const StvTrack* tk)
{
  delete mgGrappaTmp; 
  mgGrappaTmp = new StvGrappa("BOT OH");
  mgGrappaTmp->Show(tk);
}
//______________________________________________________________________________
void StvDebug::SaveAll()
{
  TIter nextCanv(gROOT->GetListOfCanvases());
  TCanvas *obj=0;
  while((obj=(TCanvas*)nextCanv())) {// loop over Canvas
    if (!obj->InheritsFrom("TCanvas")) 	continue;
    TString ts(obj->GetName());ts+=".ps";
    obj->SaveAs(ts);
    ts = obj->GetName(); ts+=".png";
    obj->SaveAs(ts);

  }
//  while(!gSystem->ProcessEvents()){}; 

}
