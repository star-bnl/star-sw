#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "StiDebug.h"
#include "TMath.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
#include "TThread.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
static int myReady=0;
//______________________________________________________________________________
void StiDebug::Break(int kase)
{
static int myBreak=-2005;
if (kase!=myBreak) return;
  printf("*** Break(%d) ***\n",kase);
}		
//______________________________________________________________________________
void StiDebug::showTh(StiKalmanTrack* kt)
{
  static TThread *myThread=0;
  delete myThread;
  TThread::VoidRtnFunc_t fun = (TThread::VoidRtnFunc_t)&show;
  myThread = new TThread(fun,kt);
  myReady=0;
  while(!gSystem->ProcessEvents() || myReady){}; 
  
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
    double curv = 0,xPrev,yPrev; int nCurv = 0;
    for (int i=0;i<9;i++) {delete graph[0][i];graph[0][i]=0;}
    StiKTNBidirectionalIterator it;
    for (int ig=0;ig<3;ig++) {
      int n=0;
      double s = 0;
      xPrev = 3e33;
      for (it=kt->begin();it!=kt->end();++it) {
	StiKalmanTrackNode *node = &(*it);
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
	  float rh = xh*xh+yh*yh;
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

ClassImp(StiAux)
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



