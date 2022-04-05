#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <map>
#include <string>
#include "TClass.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
#include "TString.h"

#include "StiDebug.h"
#include "TMath.h"
#include "TROOT.h"
#include "TObjArray.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
static int myReady=0;
typedef std::map<std::string, int>   myTallyMap_t;
typedef myTallyMap_t::const_iterator myTallyIter_t;
static  myTallyMap_t  mgTally;


int StiDebug::mgDebug=1; //0=no debug, 1=Normal, 2=count is on
int StiDebug::mgRecov=1;
int StiDebug::mgCheck=1;

typedef std::map<std::string, TH1*>   myDebMap_t;
typedef myDebMap_t::const_iterator myDebIter_t;
static  myDebMap_t  myDebMap;

typedef std::map<std::string, TCanvas*>   myCanMap_t;
typedef myCanMap_t::const_iterator myCanIter_t;
myCanMap_t myCanMap;
//______________________________________________________________________________
int StiDebug::Debug()
{
return mgDebug;
}
//______________________________________________________________________________
void StiDebug::Init()
{
  if (gROOT->IsBatch()) return;
}
//______________________________________________________________________________
int  StiDebug::iFlag(const char *flagName,int dflt)
{
  const char *val = gSystem->Getenv(flagName);
  if (!val) return dflt;
  printf("\nStiDebug::iFlag: %s = %s\n\n",flagName,val);

  return atoi(val);
}

//______________________________________________________________________________
double StiDebug::dFlag(const char *flagName, double dflt)
{
  const char *val = gSystem->Getenv(flagName);
  if (!val) return dflt;
  printf("\nStiDebug::dFlag: %s = %s\n\n",flagName,val);

  return atof(val);
}
//______________________________________________________________________________
void StiDebug::tally(const char *name,int val)
{
  if (mgDebug<2) return;
   mgTally[name] += val;
}
//______________________________________________________________________________
void StiDebug::Finish()
{
//VP  Sumary();
}
//______________________________________________________________________________
void StiDebug::Break(int kase)
{
static int myBreak=-2005;
if (kase!=myBreak) return;
if (kase!=-2005)
  printf("*** Break(%d) ***\n",kase);
}		
//______________________________________________________________________________
void StiDebug::FpeOn()
{
    gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
    printf("*** Float Point Exception is ON ***\n");

}
//______________________________________________________________________________
void StiDebug::show(StiKalmanTrack *kt)
{
//  lev=0  draw all nodes
//  lev=1  draw hit nodes
//  lev=2  draw hits only 



static TCanvas *myCanvas=0;
static TGraph  *graph[3][3] = {{0,0,0},{0,0,0},{0,0,0}};
//        P  G         P  G
  float X[3][3][100],Y[3][3][100];
  int   N[3][3];

  if (!myCanvas) {myCanvas = new TCanvas("C1","",600,800);
                  myCanvas->Divide(1,3);}
  if(kt) { 
    double curv = 0,xPrev=0,yPrev=0; int nCurv = 0;

    for (int i=0;i<9;i++) {delete graph[0][i];graph[0][i]=0;}

    StiKTNBidirectionalIterator it;
    for (int ig=0;ig<3;ig++) {
      int n=0;
      double s = 0;
      xPrev = 3e33;
      for (it=kt->begin();it!=kt->end();++it) {
	StiKalmanTrackNode *node = &(*it);
        if (!node->isValid()) 	continue;
//	S calculation common based on node x,y for both hit and node
        double xNode = node->x_g();
        double yNode = node->y_g();
	if (xPrev<3e33) {
	  double ds = sqrt(pow(xNode-xPrev,2)+pow(yNode-yPrev,2));
          double si = 0.5*ds*curv; if (si>0.99) si=0.99;
          if (si>0.01) ds = 2*asin(si)/curv;
	  s += ds;
        }
        xPrev = xNode;
        yPrev = yNode;

	StiHit *hit = node->getHit();
	if (hit &&  node->getChi2()>1000)	hit=0;
        if (ig==2 && !hit) 	continue;
        if (ig==0) { curv += node->getCurvature(); nCurv++; continue;}
	if (ig==1) {//draw nodes
          X[0][ig][n] = node->x_g();
          Y[0][ig][n] = node->y_g();
          Y[2][ig][n] = node->z_g();
	} else {//draw hits only
          X[0][ig][n] = hit->x_g();
          Y[0][ig][n] = hit->y_g();
          Y[2][ig][n] = hit->z_g();
	}

	if (n) {
          float xh = X[0][ig][n]-X[0][ig][0];
          float yh = Y[0][ig][n]-Y[0][ig][0];
	  float rh = xh*xh+yh*yh+1E-10;
	  X[1][ig][n-1] = xh/rh;
	  Y[1][ig][n-1] = yh/rh;
	}
	X[2][ig][n]=s;
	n++;
      }//end for nodes
      if (ig==0) { curv=fabs(curv)/nCurv; continue;}
      N[0][ig] = n;
      N[1][ig] = n-1;
      N[2][ig] = n;
    }//end for ig
    
    for (int ip=0;ip<3;ip++) {
      double xMin=999,xMax=-999,yMin=999,yMax=-999;
      for (int ig=1;ig<3;ig++) {
        for (int j=0;j<N[ip][ig];j++) {
           double x = X[ip][ig][j];
	   if (xMin> x) xMin = x;
	   if (xMax< x) xMax = x;
           double y = Y[ip][ig][j];
	   if (yMin> y) yMin = y;
	   if (yMax< y) yMax = y;
        }
      }
      X[ip][0][0] = xMin; Y[ip][0][0] = yMin;
      X[ip][0][1] = xMin; Y[ip][0][1] = yMax;
      X[ip][0][2] = xMax; Y[ip][0][2] = yMin;
      X[ip][0][3] = xMax; Y[ip][0][3] = yMax;
      N[ip][0] = 4;
    }
static const char *opt[]={"AP","Same CP","Same *"};  
    for (int ip=0;ip<3;ip++) {
      for (int ig =0;ig<3;ig++) {
        graph[ip][ig]  = new TGraph(N[ip][ig]  , X[ip][ig], Y[ip][ig]);
        if(ig==2) graph[ip][ig]->SetMarkerColor(kRed);
        myCanvas->cd(ip+1); graph[ip][ig]->Draw(opt[ig]);
      }//end for ig
    }//end ip

  }//end if


  if (!myCanvas) return;
  myCanvas->Modified();
  myCanvas->Update();
  myReady = 2005;
  while(!gSystem->ProcessEvents()){}; 
}  

//______________________________________________________________________________
StiAux::StiAux():TDataSet("StiAux")
{
   fN=0;
}
//______________________________________________________________________________
StiAux_t* StiAux::Get(int id) const
{
  if (id>fN) return 0;
  return ((StiAux_t*)(fArr.GetArray())) + id-1;
}
//______________________________________________________________________________
int StiAux::AddIt(StiAux_t *add)
{
  int n = fArr.GetSize();
  fN++;
  if (int(fN*sizeof(StiAux_t))>n) fArr.Set((n+100)*2);
  memcpy(Get(fN),add,sizeof(StiAux_t));
  return fN;
}
//______________________________________________________________________________
void StiAux::PrintIt(int id)
{
static const char* tit[] = {
"xnl[3]=","","", "xhl[3]=","","",
"ca=", "rho=",
"nYY=", "nZZ=",
"hYY=", "hZZ=",
"xng[3]=","","", "xhg[3]=","","",
"psi=", "dip=","chi2=",0};

  float *aux = (float*)Get(id);
  printf("%d4 - ",id);
  for (int i=0;tit[i];i++) { printf("%s%g ",tit[i],aux[i]);}
  printf("\n");
    
}    
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,double val)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,100,0.,0.);}
  h->Fill(val);
}
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,const char *val)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,0,0.,0.);}
  h->Fill(val,1.);
}
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,double val,double left,double rite)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH1F(key,key,100,left,rite);}
  h->Fill(val);
}
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,double valx, double valy
                                    ,double leftx,double ritex
				    ,double lefty,double ritey)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,leftx,ritex,100,lefty,ritey);}
  h->Fill(valx,valy);
}
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,double valx,double valy)
{
  if (mgDebug<2) return;
  TH1 *&h = (TH1*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,0.,0.,100,0,0);}
  h->Fill(valx,valy);
}
//______________________________________________________________________________ 
void StiDebug::Count(const char *key,const char *valx,double valy)
{
  if (mgDebug<2) return;
  TH2 *&h = (TH2*&)myDebMap[key];
  if (!h) { h = new TH2F(key,key,100,0.,0.,100,0,0);}
  h->Fill(valx,valy,1.);
}
//______________________________________________________________________________ 
void StiDebug::Sumary(int nHist)
{
  if (!nHist) nHist=4;
  printf("StiDebug::Sumary()\n");

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
    H[0]=h;Draw(1,H);
  }

  if (!n) return;
  while(!gSystem->ProcessEvents()){}; 

}

//______________________________________________________________________________ 
void StiDebug::Draw(int nH,TH1** H)
{ 
static int nCall=0; nCall++;

  TString ts("C_"); ts += H[0]->GetName();
  TCanvas *&C = myCanMap[ts.Data()];
  if (!C) C = new TCanvas(ts.Data(),ts.Data(),600,800);
  C->Clear();C->Divide(1,nH);
  for (int i=0;i<nH;i++) { C->cd(i+1); H[i]->Draw(); }

  C->Modified();C->Update();
}


