#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <map>
#include <string>
#include "TClass.h"
#include "TH1F.h"
#include "TBrowser.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TString.h"
#include "StvDebug.h"
int StvDebug::mgDebug=1; //0=no debug, 1=Normal, 2=count is on
int StvDebug::mgRecov=1;
int StvDebug::mgCheck=1;

typedef std::map<std::string, TH1*>   myDebMap_t;
typedef myDebMap_t::const_iterator myDebIter_t;
static  myDebMap_t  myDebMap;

typedef std::map<std::string, TCanvas*>   myCanMap_t;
typedef myCanMap_t::const_iterator myCanIter_t;
myCanMap_t myCanMap;

//______________________________________________________________________________ 
int StvDebug::Break(int key)
{ static int kto=-2010;
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
//______________________________________________________________________________ 

class TH2Hack: public TH2F
{
public: 
enum {kBUFF=10000};
TH2Hack(const char *key);
int Fill(double x,double y);
double GetEntries() const;
void Save();
private:
int mEnt;
double mXLow;
double mXUpp;
double mYLow;
double mYUpp;
double buf[kBUFF][2];
};

//______________________________________________________________________________ 
TH2Hack::TH2Hack(const char *key):TH2F(key,key,100,0,0,100,0,0)
{
mEnt= 0;
mXLow =  3e33;
mXUpp = -3e33;
mYLow =  3e33;
mYUpp = -3e33;

}
//______________________________________________________________________________ 
int TH2Hack::Fill(double x,double y)
{
 mEnt++;
 if (mEnt<kBUFF+1) 	{
   if (mXLow>x) mXLow=x;
   if (mYLow>y) mYLow=y;
   if (mXUpp<x) mXUpp=x;
   if (mYUpp<y) mYUpp=y;
   buf[mEnt-1][0]=x; buf[mEnt-1][1]=y; 
   return 0;
 } else if (mEnt==kBUFF+1) 	{Save();}
 return TH2F::Fill(x,y);
}
//______________________________________________________________________________ 
void TH2Hack::Save()
{
  enum {kMORE=2};
  if(TH2F::GetEntries()) return;
  double xlow = 0.5*(mXLow+mXUpp) -  0.5*(mXUpp-mXLow)*kMORE;
  double xupp = 0.5*(mXLow+mXUpp) +  0.5*(mXUpp-mXLow)*kMORE;
  double ylow = 0.5*(mYLow+mYUpp) -  0.5*(mYUpp-mYLow)*kMORE;
  double yupp = 0.5*(mYLow+mYUpp) +  0.5*(mYUpp-mYLow)*kMORE;

  GetXaxis()->Set(100,xlow,xupp);
  GetYaxis()->Set(100,ylow,yupp);
  for (int l=0;l<mEnt;l++) {TH2F::Fill(buf[l][0],buf[l][1]);}
  mEnt=kBUFF+2;
}
//______________________________________________________________________________ 
double TH2Hack::GetEntries() const
{
  if (mEnt<=kBUFF) ((TH2Hack*)this)->Save();
  return TH2F::GetEntries();
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
void StvDebug::Count(const char *key,double valx,double valy)
{
  if (mgDebug<2) return;
  TH1 *&h = myDebMap[key];
  if (!h) { h = new TH2Hack(key);}
  h->Fill(valx,valy);
}
//______________________________________________________________________________ 
void StvDebug::Sumary()
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
