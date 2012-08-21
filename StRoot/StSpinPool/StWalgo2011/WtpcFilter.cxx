// $Id: WtpcFilter.cxx,v 1.5 2012/08/21 17:40:09 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT

#include <assert.h>
#include <TObjArray.h>
#include <StMessMgr.h>
#include <TH2.h>
#include <TList.h>
#include <TLine.h>
#include <TMath.h>

#include <StMuDSTMaker/COMMON/StMuTrack.h>
//----------------------------
//------- Sector dependent TPC track filter
//----------------------------

#include "WtpcFilter.h"
#include "WanaConst.h"

//--------------------------------------
//--------------------------------------
WtpcFilter::WtpcFilter() {
  secID=0;
  HList=0;
  name="fixMe";
  par_nFitPts=-1;
}


//--------------------------------------
//--------------------------------------
void
WtpcFilter::init(const char *core, int sec, TObjArray *HListX, bool barrel=true) {
  HList=HListX;
  secID=sec;
  name=core; name+=secID; name+="_";
  assert(HList); 
  assert(secID);
  assert(par_nFitPts>0);
  
  LOG_INFO<<Form("::InitTPC filter (sec=%d) done, name=%s=  nFit>%d, hitFrac>%.2f Rin<%.1fcm, Rout>%.1fcm",secID,name.Data(),par_nFitPts, par_nHitFrac, par_Rmin, par_Rmax)<<endm;

  initHistos(barrel);
}

//--------------------------------------
//--------------------------------------
bool
WtpcFilter::accept( const StMuTrack  *prTr){
  hA[0]->Fill("inp",1.);

  hA[2]->Fill(prTr->nHitsFit());
  if(prTr->nHitsFit()<=par_nFitPts) return false;
 
  hA[0]->Fill("nHit",1.);
  float hitFrac=1.*prTr->nHitsFit()/prTr->nHitsPoss();
  hA[3]->Fill(hitFrac);
  if(hitFrac<par_nHitFrac)  return false;

  hA[0]->Fill("Hfrac",1.);
  StThreeVectorF ri=prTr->globalTrack()->firstPoint();
  hA[4]->Fill(ri.perp());
  if(ri.perp()>par_Rmin)  return false;

  hA[0]->Fill("Rin",1.);
  StThreeVectorF ro=prTr->globalTrack()->lastPoint();
  hA[5]->Fill(ro.perp());
  if(ro.perp()<par_Rmax) return false;


  // accepted .......
  hA[0]->Fill("Rout",1.);
  hA[1]->Fill(ro.pseudoRapidity(),ro.phi());
  float dedx=prTr->dEdx()*1e6;
  hA[6]->Fill(prTr->p().mag(),dedx);
  return true;
}


//--------------------------------------
//--------------------------------------
int
WtpcFilter::getTpcSec(float phiRad, float etaDet){ // finds TPC sector for hit(phi,eta)   
  const float PI=TMath::Pi();
  int sec=0;
  float phi=phiRad/PI*180; // now in degrees
  if (etaDet>0) { // West TPC
    float x=75-phi;
    while(x<0) x+=360;
    sec=1+(int)( x/30.);
  } else {
    float x=phi-105;
    while(x<0) x+=360;
    sec=13+(int)( x/30.);
  }
  //  printf("phi/deg=%.1f, x=%.1f\n",phi,x);
  assert(sec>0);
  assert(sec<=mxTpcSec);
  return sec;
}


//--------------------------------------
//--------------------------------------
void
WtpcFilter::initHistos(bool barrel) {
  int myCol=2+ secID%6;

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;TH1 *h; 

  TString sufix=", TPC sec="; sufix+=secID;
  int nCase=6;
  hA[0]=h=new TH1F(name+"Stat","track counter"+sufix+"; Cases",nCase,0,nCase);
  h->SetLineColor(myCol );
  char key[][200]={"inp","nHit","Hfrac","Rin","Rout"};
  for(int i=0;i<5;i++) h->Fill(key[i],0.); // preset the order of keys

  if(barrel) {
    hA[1]=h=new TH2F(name+"Tr2D1","lastHit, accepted"+sufix+"; detector eta ; detector phi (rad)",100,-1.1,1.1,200,-3.2,3.2);
    h->SetLineColor(myCol );
  }
  else {
    hA[1]=h=new TH2F(name+"Tr2D1","lastHit, accepted"+sufix+"; detector eta ; detector phi (rad)",100,-0.2,1.8,200,-3.2,3.2);
    h->SetLineColor(myCol );
  }
  
  hA[2]=h=new TH1F(name+"TrNfit","prim tr nFitP"+sufix+"; nFitPoints",50,0,50);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nFitPts,0,par_nFitPts,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);


  hA[3]=h=new TH1F(name+"TrFitFrac","prim tr nFitFrac"+sufix+"; nFit/nPoss ",50,0,1.1);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_nHitFrac,0,par_nHitFrac,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[4]=h=new TH1F(name+"TrRxyIn","prim tr 1st hit  filter"+sufix+"; Rxy (cm)",60,50,170.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_Rmin,0,par_Rmin,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[5]=h=new TH1F(name+"TrRxyOut","prim tr last hit filter"+sufix+"; Rxy (cm)",60,100,220.);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_Rmax,0,par_Rmax,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  

  hA[6]=h=new TH2F(name+"TrdEdX"," dEdX vs. P, accpted "+sufix+"; track P (GeV); dE/dx (keV)",20,0,10,100,0,10);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  
  //  HList->ls();
  //  LOG_INFO<<Form("TpcFilter-%s::initHistos done",name.Data())<<endm;

}
