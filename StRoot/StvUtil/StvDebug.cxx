#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <map>
#include <string>
#include "TH1F.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TString.h"
#include "StvDebug.h"
int StvDebug::mgDebug=1; //0=no debug, 1=Normal, 2=count is on
int StvDebug::mgRecov=1;
int StvDebug::mgCheck=1;

typedef std::map<std::string, TH1F*>   myDebMap_t;
typedef myDebMap_t::const_iterator myDebIter_t;
static  myDebMap_t  myDebMap;

typedef std::map<std::string, TCanvas*>   myCanMap_t;
typedef myCanMap_t::const_iterator myCanIter_t;
myCanMap_t myCanMap;

typedef std::map<std::string, int>   myIntMap_t;
myIntMap_t myIntMap;


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
void StvDebug::Count(const char *key,double val)
{
  if (mgDebug<2) return;
  TH1F *&h = myDebMap[key];
  if (!h) { h = new TH1F(key,key,100,0.,0.);}
  h->Fill(val);
}
//______________________________________________________________________________ 
void StvDebug::Sumary()
{
  printf("StvDebug::Sumary()\n");
  TH1 *H[4];
  int nH = 0,n=0;
  for (myDebIter_t iter = myDebMap.begin();iter != myDebMap.end();++iter) {
    TH1F *h = (*iter).second; n++;
    int nEnt = h->GetEntries();
    double mean = h->GetMean();
    double rms  = h->GetRMS();
    printf("%2d - %12s:\t %5d %g(+-%g)\n",n,h->GetName(),nEnt,mean,rms);
    if (rms<=0) continue;
    if (nH==4) {Draw(nH,H);nH=0;}
    H[nH++] = h;
  }
  if (nH) Draw(nH,H);
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
int &StvDebug::Flag(const char *key)
{
  return myIntMap[key];
}
