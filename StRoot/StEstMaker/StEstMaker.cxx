/***************************************************************************
 *
 * $Id: StEstMaker.cxx,v 1.1 2000/12/07 11:14:21 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Main methods for StEstMaker
 *
 ***************************************************************************
 *
 * $Log: StEstMaker.cxx,v $
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker.h"
#include "StMemoryInfo.hh"

ClassImp(StEstMaker)

StEstMaker::StEstMaker(const char* name):StMaker(name) {
}

Int_t StEstMaker::Init(){
  int i,j;

  cout<<"**** StEstMaker::StEstMaker() ****Init"<<endl;

  // creating the egr_par table.
  m_egr_egrpar = new St_egr_egrpar("egr_egrpar",1); {
    egr_egrpar_st row;
    //
    memset(&row,0,m_egr_egrpar->GetRowSize());
    row.debug[0]         =          1; // flags for debug printing ;
    row.debug[1]         =          0;
    row.debug[2]         =          0;
    row.debug[3]         =          0;
    row.debug[4]         =          0;
    row.debug[5]         =          0;
    row.debug[6]         =          0;
    row.debug[7]         =          0;
    row.debug[8]         =          0;
    row.minfit   =          2; // min no. of points on track ;
    row.mxtry    =         10; // max no. of attempts to fit ;
    row.useglobal = 2; // set if to usematching to be used ;
    row.usetpc   =  1; // set if TPC used in refit ;
    row.useemc   =          0; // set if EMC used in refit ;
    row.usesvt   =          0; // set if SVT used in refit ;
    row.usetof   =          0; // set if TOF used in refit ;
    row.usevert  =          0; // Set if primary vertex used in refit ;
    row.prob[0]  =        -200; // probability cut in fit ;
    row.prob[1]  =        -100;
    //    row.prob[0]  =        10; // probability cut in fit ;
    //    row.prob[1]  =        10;
//     row.prob[0]  =        -200; // probability cut in fit ;
    //     row.prob[1]  =        -100;
    row.svtchicut        =  0; // SVT chi2 cut for adding SVT-only tracks ;
    m_egr_egrpar->AddAt(&row,0);
  }
  
  AddRunCont(m_egr_egrpar);
 
  mPass = 0;
  mNPass = 5;

 // ideal tracking: mIdealTracking = 1
  mIdealTracking = 0;

  mParams = new StEstParams*[mNPass];
  for (i=0;i<mNPass;i++) mParams[i] = new StEstParams;

  // default settings, may be overridden by SetParams:
  for (i=0;i<mNPass;i++) {
    mParams[i]->debug = 2;
   // Cant have ideal without MC info
    if( mIdealTracking &&  mParams[i]->debug <2 ) mParams[i]->debug = 2;
    for (j=0;j<4;j++) {
      mParams[i]->nneighbours[j] = 2;
      mParams[i]->ntotbranch[j] = 6;
      mParams[i]->onoff[j] = 1;
      mParams[i]->share[j] = 5;
    }
    //    mParams[i]->onoff[3] = 0;
    mParams[i]->nbranch[3] = 3;
    mParams[i]->nbranch[2] = 2;
    mParams[i]->nbranch[1] = 1;
    mParams[i]->nbranch[0] = 1;

    mParams[i]->maxtpchits=50;
    mParams[i]->maxsvthits=8;    
    mParams[i]->maxbranches=100;     

    mParams[i]->phibin = 5;
    mParams[i]->zbin = 2;
    mParams[i]->nphibins = 72;
    mParams[i]->nzbins = 36;
    
    mParams[i]->lrad[0][0] = 6.125;
    mParams[i]->lrad[0][1] = 7.185;
    mParams[i]->lrad[1][0] = 10.185;
    mParams[i]->lrad[1][1] = 11.075;
    mParams[i]->lrad[2][0] = 13.995;
    mParams[i]->lrad[2][1] = 14.935;
    mParams[i]->lrad[3][0] = 23;
    mParams[i]->lrad[3][1] = 23;
  }	
    
  mParams[4]->ptmin = 0.1;
  mParams[4]->ptmax = 0.2;
  mParams[3]->ptmin = 0.2;
  mParams[3]->ptmax = 0.4;
  mParams[2]->ptmin = 0.4;
  mParams[2]->ptmax = 0.7;
  mParams[1]->ptmin = 0.7;
  mParams[1]->ptmax = 1.0;  
  mParams[0]->ptmin = 1.0;
  mParams[0]->ptmax = 100;

  mParams[0]->geomcutl[3] = 1.0;
  mParams[0]->geomcutl[2] = 0.5;
  mParams[0]->geomcutl[1] = 0.2;
  mParams[0]->geomcutl[0] = 0.2;
  mParams[0]->geomcutw[3] = 1.0;
  mParams[0]->geomcutw[2] = 0.5;
  mParams[0]->geomcutw[1] = 0.2;
  mParams[0]->geomcutw[0] = 0.2;

  mParams[1]->geomcutl[3] = 1.0;
  mParams[1]->geomcutl[2] = 0.5;
  mParams[1]->geomcutl[1] = 0.2;
  mParams[1]->geomcutl[0] = 0.2;
  mParams[1]->geomcutw[3] = 1.0;
  mParams[1]->geomcutw[2] = 0.5;
  mParams[1]->geomcutw[1] = 0.2;
  mParams[1]->geomcutw[0] = 0.2;
  
  mParams[2]->geomcutl[3] = 1.5;
  mParams[2]->geomcutl[2] = 0.5;
  mParams[2]->geomcutl[1] = 0.2;
  mParams[2]->geomcutl[0] = 0.2;
  mParams[2]->geomcutw[3] = 1.5;
  mParams[2]->geomcutw[2] = 0.5;
  mParams[2]->geomcutw[1] = 0.2;
  mParams[2]->geomcutw[0] = 0.2;
  
  mParams[3]->geomcutl[3] = 3.0;
  mParams[3]->geomcutl[2] = 0.5;
  mParams[3]->geomcutl[1] = 0.2;
  mParams[3]->geomcutl[0] = 0.2;
  mParams[3]->geomcutw[3] = 3.0;
  mParams[3]->geomcutw[2] = 0.5;
  mParams[3]->geomcutw[1] = 0.2;
  mParams[3]->geomcutw[0] = 0.2;
  
  mParams[4]->geomcutl[3] = 5.;
  mParams[4]->geomcutl[2] = 0.5;
  mParams[4]->geomcutl[1] = 0.2;
  mParams[4]->geomcutl[0] = 0.2;
  mParams[4]->geomcutw[3] = 5.;
  mParams[4]->geomcutw[2] = 0.5;
  mParams[4]->geomcutw[1] = 0.2;
  mParams[4]->geomcutw[0] = 0.2;

  mIndexGeom = new StEstIndexGeom(mParams[0]->nphibins, mParams[0]->nzbins);

  // superpass settings

  mSuperPass = 0;
  mNSuperPass = 1;
  mSegments = new StEstSegments*[mNSuperPass];
  for (i=0;i<mNSuperPass;i++) mSegments[i] = new StEstSegments;

  mSegments[0]->chisqcut = 300;
  mSegments[0]->minhits=4;
  mSegments[0]->rminTPC=500;
  mSegments[0]->minTPChits=0;
  mSegments[0]->slay[3]=2;
  mSegments[0]->slay[2]=2;
  mSegments[0]->slay[1]=2;
  mSegments[0]->slay[0]=2;
  
//   mSegments[1]->chisqcut = 100;
//   mSegments[1]->minhits=3;
//   mSegments[1]->rminTPC=500;
//   mSegments[1]->minTPChits=0;
//   mSegments[1]->slay[3]=1;
//   mSegments[1]->slay[2]=1;
//   mSegments[1]->slay[1]=1;
//   mSegments[1]->slay[0]=1;

//   mSegments[2]->chisqcut = 30;
//   mSegments[2]->minhits=1;
//   mSegments[2]->rminTPC=500;
//   mSegments[2]->minTPChits=0;
//   mSegments[2]->slay[3]=1;
//   mSegments[2]->slay[2]=1;
//   mSegments[2]->slay[1]=1;
//   mSegments[2]->slay[0]=1;
  

//   mSegments[3]->chisqcut = 30;
//   mSegments[3]->minhits=2;
//   mSegments[3]->rminTPC=500;
//   mSegments[3]->minTPChits=0;
//   mSegments[3]->slay[3]=1;
//   mSegments[3]->slay[2]=1;
//   mSegments[3]->slay[1]=1;
//   mSegments[3]->slay[0]=1;

//   mSegments[4]->chisqcut = 40;
//   mSegments[4]->minhits=2;
//   mSegments[4]->rminTPC=100;
//   mSegments[4]->minTPChits=0;
//   mSegments[4]->slay[3]=1;
//   mSegments[4]->slay[2]=1;
//   mSegments[4]->slay[1]=1;
//   mSegments[4]->slay[0]=1;
  
  

  // histogram definitions
  disthitip0   = new TH1F("disthitip0","TrackProj-PerfHit distance in lay0",100,0.,5.);
  disthitip1   = new TH1F("disthitip1","TrackProj-PerfHit distance in lay1",100,0.,5.);
  disthitip2   = new TH1F("disthitip2","TrackProj-PerfHit distance in lay2",100,0.,5.);
  disthitip3   = new TH1F("disthitip3","TrackProj-PerfHit distance in lay3",100,0.,5.);
  disthitip3_p0   = new TH1F("disthitip3_p0","TrackProj-PerfHit distance in lay3-pass0",100,0.,5.);
  disthitip3_p1   = new TH1F("disthitip3_p1","TrackProj-PerfHit distance in lay3-pass1",100,0.,5.);
  disthitip3_p2   = new TH1F("disthitip3_p2","TrackProj-PerfHit distance in lay3-pass2",100,0.,5.);
  disthitip3_p3   = new TH1F("disthitip3_p3","TrackProj-PerfHit distance in lay3-pass3",100,0.,5.);
  disthitip3_p4   = new TH1F("disthitip3_p4","TrackProj-PerfHit distance in lay3-pass4",100,0.,5.);
  chisqib0   = new TH1F("chisqib0","Chisq of ideal branch in lay0",100,0.,5000.);
  chisqib1   = new TH1F("chisqib1","Chisq of ideal branch in lay1",100,0.,5000.);
  chisqib2   = new TH1F("chisqib2","Chisq of ideal branch in lay2",100,0.,5000.);
  chisqib3   = new TH1F("chisqib3","Chisq of ideal branch in lay3",100,0.,5000.);
  disthitip3_p = new TH1F("disthitip3_p","PrimTrackProj-PerfHit distance in lay3",100,0.,5.);
  disthitip3_p_p0 = new TH1F("disthitip3_p_p0","PrimTrackProj-PerfHit distance in lay3-pass0",100,0.,5.);
  disthitip3_p_p1 = new TH1F("disthitip3_p_p1","PrimTrackProj-PerfHit distance in lay3-pass1",100,0.,5.);
  disthitip3_p_p2 = new TH1F("disthitip3_p_p2","PrimTrackProj-PerfHit distance in lay3-pass2",100,0.,5.);
  disthitip3_p_p3 = new TH1F("disthitip3_p_p3","PrimTrackProj-PerfHit distance in lay3-pass3",100,0.,5.);
  disthitip3_p_p4 = new TH1F("disthitip3_p_p4","PrimTrackProj-PerfHit distance in lay3-pass4",100,0.,5.);
  disthitip3_s = new TH1F("disthitip3_s","SecoTrackProj-PerfHit distance in lay3",100,0.,5.);
  disthitip3_s_p0 = new TH1F("disthitip3_s_p0","SecoTrackProj-PerfHit distance in lay3-pass0",100,0.,5.);
  disthitip3_s_p1 = new TH1F("disthitip3_s_p1","SecoTrackProj-PerfHit distance in lay3-pass1",100,0.,5.);
  disthitip3_s_p2 = new TH1F("disthitip3_s_p2","SecoTrackProj-PerfHit distance in lay3-pass2",100,0.,5.);
  disthitip3_s_p3 = new TH1F("disthitip3_s_p3","SecoTrackProj-PerfHit distance in lay3-pass3",100,0.,5.);
  disthitip3_s_p4 = new TH1F("disthitip3_s_p4","SecoTrackProj-PerfHit distance in lay3-pass4",100,0.,5.);
  disthitip0w  = new TH1F("disthitip0w","TrackProj-PerfHit distance (circ.) in lay0",100,0.,1.);
  disthitip1w  = new TH1F("disthitip1w","TrackProj-PerfHit distance (circ.) in lay1",100,0.,1.);
  disthitip2w  = new TH1F("disthitip2w","TrackProj-PerfHit distance (circ.) in lay2",100,0.,1.);
  disthitip3w  = new TH1F("disthitip3w","TrackProj-PerfHit distance (circ.) in lay3",100,0.,1.);
  disthitip0l  = new TH1F("disthitip0l","TrackProj-PerfHit distance (line.) in lay0",100,0.,1.);
  disthitip1l  = new TH1F("disthitip1l","TrackProj-PerfHit distance (line.) in lay1",100,0.,1.);
  disthitip2l  = new TH1F("disthitip2l","TrackProj-PerfHit distance (line.) in lay2",100,0.,1.);
  disthitip3l  = new TH1F("disthitip3l","TrackProj-PerfHit distance (line.) in lay3",100,0.,1.);
  disthitipchisq  = new TH1F("disthitipchisq","ideal track chisq",100,0.,100.);
  disthitipchisql  = new TH1F("disthitipchisql","ideal track chisq",100,0.,100.);
  disthitipchisqw  = new TH1F("disthitipchisqw","ideal track chisq",100,0.,100.);
  dca_all  = new TH1F("dca_all","dca of all tracks",100,0.,10.);
  dca_pri  = new TH1F("dca_pri","dca of pri tracks",100,0.,10.);
  dca_sec  = new TH1F("dca_sec","dca of sec tracks",100,0.,10.);
  Initfitstatus = new TH1F("Initfitstatus","fit status",16,-10.5,5.5);
  
  Location3=  new TH2F("Location3","location vs pass in lay3",5,-0.5,4.5,15,-4.5,10.5);
  Location2=  new TH2F("Location2","location vs pass in lay2",5,-0.5,4.5,15,-4.5,10.5);
  Location1=  new TH2F("Location1","location vs pass in lay1",5,-0.5,4.5,15,-4.5,10.5);
  Location0=  new TH2F("Location0","location vs pass in lay0",5,-0.5,4.5,15,-4.5,10.5);

  IdealPattern= new TH1F("IdealPattern","ideal pattern",20,-0.5,19.5);
  IdealNHits= new TH1F("IdealNHits","ideal number of hits (one/layer)",20,-0.5,19.5);
  
  mChiLin = new TH1F("mChiLin","Chisq linear",100,0,10);
  mChiCir = new TH1F("mChiCir","Chisq circular",100,0,10);


  efficiency = new TH1D("eff","pt vs efficiency",50,0,1.5);
  idealpt    = new TH1D("idealpt","pt distri. of ideal tracks",30,0,1.5);
  idealpt_p    = new TH1D("idealpt_p","pt distri. of ideal tracks (prim)",30,0,1.5);
  idealpt_s    = new TH1D("idealpt_s","pt distri. of ideal tracks (seco)",30,0,1.5);
  Elidealpt    = new TH1D("Elidealpt","pt distri. of ideal electron tracks",30,0,1.5);
  Elidealpt_p    = new TH1D("Elidealpt_p","pt distri. of ideal electron tracks (prim)",30,0,1.5);
  Elidealpt_s    = new TH1D("Elidealpt_s","pt distri. of ideal electron tracks (seco)",30,0,1.5);
  Muidealpt    = new TH1D("Muidealpt","pt distri. of ideal muon tracks",30,0,1.5);
  Muidealpt_p    = new TH1D("Muidealpt_p","pt distri. of ideal muon tracks (prim)",30,0,1.5);
  Muidealpt_s    = new TH1D("Muidealpt_s","pt distri. of ideal muon tracks (seco)",30,0,1.5);
  Piidealpt    = new TH1D("Piidealpt","pt distri. of ideal pion tracks",30,0,1.5);
  Piidealpt_p    = new TH1D("Piidealpt_p","pt distri. of ideal pion tracks (prim)",30,0,1.5);
  Piidealpt_s    = new TH1D("Piidealpt_s","pt distri. of ideal pion tracks (seco)",30,0,1.5);
  Kaidealpt    = new TH1D("Kaidealpt","pt distri. of ideal kaon tracks",30,0,1.5);
  Kaidealpt_p    = new TH1D("Kaidealpt_p","pt distri. of ideal kaon tracks (prim)",30,0,1.5);
  Kaidealpt_s    = new TH1D("Kaidealpt_s","pt distri. of ideal kaon tracks (seco)",30,0,1.5);
  Pridealpt    = new TH1D("Pridealpt","pt distri. of ideal proton tracks",30,0,1.5);
  Pridealpt_p    = new TH1D("Pridealpt_p","pt distri. of ideal proton tracks (prim)",30,0,1.5);
  Pridealpt_s    = new TH1D("Pridealpt_s","pt distri. of ideal proton tracks (seco)",30,0,1.5);
  idealntpc  = new TH1D("idealntpc","ntpc distribution of ideal tracks",50,0,50);
  idealrtpc  = new TH1D("idealrtpc","rtpc distribution of ideal tracks",50,40,250);
  goodpt     = new TH1D("goodpt","pt distri. of good tracks",30,0,1.5);
  goodpt_p     = new TH1D("goodpt_p","pt distri. of good tracks (prim)",30,0,1.5);
  goodpt_s     = new TH1D("goodpt_s","pt distri. of good tracks (seco)",30,0,1.5);
  Elgoodpt     = new TH1D("Elgoodpt","pt distri. of good electron tracks",30,0,1.5);
  Elgoodpt_p     = new TH1D("Elgoodpt_p","pt distri. of good electron tracks (prim)",30,0,1.5);
  Elgoodpt_s     = new TH1D("Elgoodpt_s","pt distri. of good electron tracks (seco)",30,0,1.5);
  Mugoodpt     = new TH1D("Mugoodpt","pt distri. of good muon tracks",30,0,1.5);
  Mugoodpt_p     = new TH1D("Mugoodpt_p","pt distri. of good muon tracks (prim)",30,0,1.5);
  Mugoodpt_s     = new TH1D("Mugoodpt_s","pt distri. of good muon tracks (seco)",30,0,1.5);
  Pigoodpt     = new TH1D("Pigoodpt","pt distri. of good pion tracks",30,0,1.5);
  Pigoodpt_p     = new TH1D("Pigoodpt_p","pt distri. of good pion tracks (prim)",30,0,1.5);
  Pigoodpt_s     = new TH1D("Pigoodpt_s","pt distri. of good pion tracks (seco)",30,0,1.5);
  Kagoodpt     = new TH1D("Kagoodpt","pt distri. of good kaon tracks",30,0,1.5);
  Kagoodpt_p     = new TH1D("Kagoodpt_p","pt distri. of good kaon tracks (prim)",30,0,1.5);
  Kagoodpt_s     = new TH1D("Kagoodpt_s","pt distri. of good kaon tracks (seco)",30,0,1.5);
  Prgoodpt     = new TH1D("Prgoodpt","pt distri. of good proton tracks",30,0,1.5);
  Prgoodpt_p     = new TH1D("Prgoodpt_p","pt distri. of good proton tracks (prim)",30,0,1.5);
  Prgoodpt_s     = new TH1D("Prgoodpt_s","pt distri. of good proton tracks (seco)",30,0,1.5);
  badpt      = new TH1D("badpt","pt distri. of bad tracks",30,0,1.5);
  badpt_p      = new TH1D("badpt_p","pt distri. of bad tracks (prim)",30,0,1.5);
  badpt_s      = new TH1D("badpt_s","pt distri. of bad tracks (seco)",30,0,1.5);
  Elbadpt     = new TH1D("Elbadpt","pt distri. of bad electron tracks",30,0,1.5);
  Elbadpt_p     = new TH1D("Elbadpt_p","pt distri. of bad electron tracks (prim)",30,0,1.5);
  Elbadpt_s     = new TH1D("Elbadpt_s","pt distri. of bad electron tracks (seco)",30,0,1.5);
  Mubadpt     = new TH1D("Mubadpt","pt distri. of bad muon tracks",30,0,1.5);
  Mubadpt_p     = new TH1D("Mubadpt_p","pt distri. of bad muon tracks (prim)",30,0,1.5);
  Mubadpt_s     = new TH1D("Mubadpt_s","pt distri. of bad muon tracks (seco)",30,0,1.5);
  Pibadpt     = new TH1D("Pibadpt","pt distri. of bad pion tracks",30,0,1.5);
  Pibadpt_p     = new TH1D("Pibadpt_p","pt distri. of bad pion tracks (prim)",30,0,1.5);
  Pibadpt_s     = new TH1D("Pibadpt_s","pt distri. of bad pion tracks (seco)",30,0,1.5);
  Kabadpt     = new TH1D("Kabadpt","pt distri. of bad kaon tracks",30,0,1.5);
  Kabadpt_p     = new TH1D("Kabadpt_p","pt distri. of bad kaon tracks (prim)",30,0,1.5);
  Kabadpt_s     = new TH1D("Kabadpt_s","pt distri. of bad kaon tracks (seco)",30,0,1.5);
  Prbadpt     = new TH1D("Prbadpt","pt distri. of bad proton tracks",30,0,1.5);
  Prbadpt_p     = new TH1D("Prbadpt_p","pt distri. of bad proton tracks (prim)",30,0,1.5);
  Prbadpt_s     = new TH1D("Prbadpt_s","pt distri. of bad proton tracks (seco)",30,0,1.5);

  goodchi   = new TH1D("goodchi", "chisq of good branches", 100, 0, 100000.);
  badchi    = new TH1D("badchi",  "chisq of bad branches", 100, 0, 100000.);

  badStep    = new TH1D("badStep",  "step of bad branches", 41, -0.5, 40.5);
  goodStep    = new TH1D("goodStep",  "step of good branches", 41, -0.5, 40.5);
  
  goodmaxdist = new TH1D("goodmaxdist", "max dist for good branches", 100, 0, 5);
  badmaxdist = new TH1D("badmaxdist", "max dist for bad branches", 100, 0, 5);

  idealphi     = new TH1D("idealphi","phi distribution of ideal tracks",200,-2.,2.);
  goodphi     = new TH1D("goodphi","phi distribution of good tracks",200,-2.,2.);
  badphi     = new TH1D("badphi","phi distribution of bad tracks",200,-2.,2.);

  cout<<"**** StEstMaker::StEstMaker() **** INIT STOP"<<endl;
  return kStOK;

}

StEstMaker::~StEstMaker() {}


void StEstMaker::SetGeomParams(double parl[4][5], double parw[4][5]) {
  for (int i=0; i<4; i++) {
    for (int j=0; j<5; j++) {
      mParams[i]->geomcutl[j] = parl[i][j];
      mParams[i]->geomcutw[j] = parw[i][j];
    }
  }
}


void StEstMaker::ReadTables() {
  
  St_ObjectSet*   data     = (St_ObjectSet *)GetDataSet("bfc/.make");
  St_DataSetIter* dataiter = new St_DataSetIter(data);
  St_svg_geom*    geom     = (St_svg_geom *)dataiter->FindByName("geom");
  svg_geom_st*    svtgeom  = geom->GetTable();
  //St_DataSet *svtgeom = dataiter.Find("params/svt/svgpars/geom");
  
  cout << "TESTUJEMY............... "<<(*svtgeom).id<<" ******"<<endl;

}


Int_t StEstMaker::Finish() {
  cout<<"StEstMaker::Finish() ****START****"<<endl;
  return StMaker::Finish();
  cout<<"StEstMaker::Finish() ****STOP****"<<endl;
}

Int_t StEstMaker::Make() {

  long TrackDeadBeforeSelection,TrackDeadAfterSelection;
  long TrackDeadBeforeSegment,TrackDeadAfterSegment;
  long TrackDeadBeforeBest,TrackDeadAfterBest;
  long TrackDeadBeforeRemoveSharing,TrackDeadAfterRemoveSharing;
  long NTrackPresented,NTrackFormed;
  long NTrackPresentedGood;
  int OneIsGood;
  long i,j;
  long ihita[4],ihitaold[4];
  long ihitb[4],ihitbshared[4];
  ofstream sortie("out.log",ios::out);

  StMemoryInfo *info = StMemoryInfo::instance();
  info->snapshot();
  info->print();
  
  Setup();

  if( mParams[mPass]->debug>0){
    SetupMc();
    cout<<"Building the ideal branches for all the tracks"<<endl;
    BuildIdealBranches();
    cout<<"Building the findable branches for all the tracks"<<endl;
    BuildFindableBranches();
  }

  if (!mTrack) return 1;
  if(mParams[mPass]->debug>0)
    cout<<"StEstMaker::Make() ****START****"<<endl;
  // Printing out the tracking conditions
  if(mParams[mPass]->debug>0) PrintSettings();
  int slay,onoffmatrix,nminhit;
  
  for (i=0;i<4;i++) ihita[i]=0;
  for (i=0;i<mNSvtHit;i++) {
    if (mSvtHit[i]->GetFlag()==0) ihita[mSvtHit[i]->GetWafer()->GetLayer()]++;
  }
  cout<<"Hit density (0123) :";
  cout<<"\t"<<ihita[0]<<"\t\t"<<ihita[1]<<"\t\t"<<ihita[2]<<"\t\t"<<ihita[3]<<endl;
  for(mSuperPass=0; mSuperPass<mNSuperPass; mSuperPass++) {
    FlagTPCTracksSP(mSuperPass);
    onoffmatrix=0;
    for (j=0;j<4;j++) 
      if (mSegments[mSuperPass]->slay[j]==2 && mParams[0]->onoff[j]!=0) 
	onoffmatrix |= int(pow(2,j));
    nminhit=mSegments[mSuperPass]->minhits;
    for (mPass=0;mPass<mNPass;mPass++) {
      cout<<"Super = "<<mSuperPass<<" Pass = "<<mPass<<endl;
      // locally mark the tracks which should be considered
      NTrackPresented=0;
      NTrackPresentedGood=0;
      for (i=0;i<mNTrack;i++) {
	mTrack[i]->SetDoIt(1);
	if(mTrack[i]->mTPCTrack->mPt<=mParams[mPass]->ptmin || 
	   mTrack[i]->mTPCTrack->mPt>mParams[mPass]->ptmax ||
	   mTrack[i]->mTPCTrack->GetFlag()<0 || 
	   mTrack[i]->mTPCTrack->GetFlagSP()<0 || 
	   mTrack[i]->GetFlag()!=0) mTrack[i]->SetDoIt(0);
	else {
	  NTrackPresented++;
	  if (onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	      mTrack[i]->GetIdealPattern()>=nminhit)
	  NTrackPresentedGood++;
	}
      }
      for (slay=3;slay>=0;slay--) {
	if(mParams[mPass]->onoff[slay])  Tracking(slay);
	TrackDeadBeforeSegment=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit) {
	      TrackDeadBeforeSegment++;
	      for (j=0;j<mTrack[i]->GetNBranches();j++)
		if (mTrack[i]->GetBranch(j)->GetIsGoodOld()==1) {
		  //		  cout<<"******** Track_id="<<mTrack[i]->mTPCTrack->GetId()<<" Killed HitPosition="<<mTrack[i]->GetBranch(j)->mHitPosition<<endl;
		  //		  PrintTrackDetails(mTrack[i]->mTPCTrack->GetId());
		}	      
	      
	    }
	  }

	ChooseSegment(mSuperPass,slay);
	
	TrackDeadAfterSegment=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit) {
	      TrackDeadAfterSegment++;
	    }
	  }
	cout<<"slay "<<slay<<" TrackDead (b/a) ChooseSegment = "<<TrackDeadBeforeSegment<<"  "<<TrackDeadAfterSegment<<endl;

	TrackDeadBeforeSelection=0;
	TrackDeadAfterSelection=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1) {
	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadBeforeSelection++;

	    ChooseBestNBranches(mTrack[i], slay);

	    OneIsGood=0;
	    for (j=0;j<mTrack[i]->GetNBranches();j++)
	      if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	    if (OneIsGood==0 && 
		onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
		mTrack[i]->GetIdealPattern()>=nminhit)
	      TrackDeadAfterSelection++;
	  }	
	cout<<"slay "<<slay<<" TrackDead (b/a) ChooseBestNBranches = "<<TrackDeadBeforeSelection<<"  "<<TrackDeadAfterSelection<<endl;
	
      }// for (slay=3..... 

      //      if(mParams[mPass]->debug>0) 
      //	cout << "End of ChooseSegment for mSuperPass# "<<mSuperPass<<"  Pass# "<<mPass<<endl;
      
      // choosing best branches for each track and killing the rest
      TrackDeadBeforeBest=0;
      for (i=0;i<mNTrack;i++) 
	if (mTrack[i]->GetDoIt()==1) {
	  OneIsGood=0;
	  for (j=0;j<mTrack[i]->GetNBranches();j++)
	    if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	  if (OneIsGood==0 && 
	      onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	      mTrack[i]->GetIdealPattern()>=nminhit)
	    TrackDeadBeforeBest++;
	}
      for (i=0;i<mNTrack;i++) {
	if(mTrack[i]->GetDoIt()==1){
	  ChooseBestBranch(mTrack[i], mSuperPass);    
	}
      }
      TrackDeadAfterBest=0;
      for (i=0;i<mNTrack;i++) 
	if (mTrack[i]->GetDoIt()==1) {
	  OneIsGood=0;
	  for (j=0;j<mTrack[i]->GetNBranches();j++)
	    if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	  if (OneIsGood==0 && 
	      onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	      mTrack[i]->GetIdealPattern()>=nminhit)
	    TrackDeadAfterBest++;
	}
      cout<<"--> TrackDead (b/a) ChooseBest = "<<TrackDeadBeforeBest<<"  "<<TrackDeadAfterBest<<endl;
      //      if(mParams[mPass]->debug>0) 
      //	cout << "End of ChooseBestBranch for mSuperPass# "<<mSuperPass<<"  Pass# "<<mPass<<endl;
      
      // The hit sharing was here before...
      
      NTrackFormed=0;
	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1 && mTrack[i]->GetFlag()==1) NTrackFormed++;
      cout<<"Number of tracks presented/should be formed/formed during the pass : "
	  <<NTrackPresented<<"/"<<NTrackPresentedGood<<"/"<<NTrackFormed<<endl;

      // checking consistency of the track <-> branch <-> hit connections
      TrackDebug();
      for (i=0;i<4;i++) {
	ihitaold[i]=ihita[i];
	ihita[i]=0;
      }
      for (i=0;i<mNSvtHit;i++) {
	if (mSvtHit[i]->GetFlag()==0) ihita[mSvtHit[i]->GetWafer()->GetLayer()]++;
      }
      cout<<"Hit density (0123) :";
      cout<<"\t"<<ihita[0]<<"\t"<<ihitaold[0]-ihita[0];
      cout<<"\t"<<ihita[1]<<"\t"<<ihitaold[1]-ihita[1];
      cout<<"\t"<<ihita[2]<<"\t"<<ihitaold[2]-ihita[2];
      cout<<"\t"<<ihita[3]<<"\t"<<ihitaold[3]-ihita[3]<<endl;
      // We need to flag the track process in the current pass 
      // in order to be considered by the method applied (FinishFlag...)
      // at the end of the superpasses.
      	for (i=0;i<mNTrack;i++) 
	  if (mTrack[i]->GetDoIt()==1)  mTrack[i]->SetDone(1);
    } // for (mPass=0.......
    mPass--;
    // studing the hit sharing.
    cout<<"Studying the hit sharing"<<endl;
    for (i=0;i<4;i++) {
      ihitb[i]=0;
      ihitbshared[i]=0;
    }
    for (i=0;i<mNSvtHit;i++)       
      if (mSvtHit[i]->GetNBranch()!=0) {
	ihitb[mSvtHit[i]->GetWafer()->GetLayer()]++;
	if (mSvtHit[i]->GetNShare()>1) 
	  ihitbshared[mSvtHit[i]->GetWafer()->GetLayer()]++;
      }
    
    cout<<"Hits used (0123) :";
    cout<<"\t"<<ihitb[0];
    cout<<"\t"<<ihitb[1];
    cout<<"\t"<<ihitb[2];
    cout<<"\t"<<ihitb[3]<<endl;
    cout<<"Hits shared (0123) :";
    cout<<"\t"<<ihitbshared[0];
    cout<<"\t"<<ihitbshared[1];
    cout<<"\t"<<ihitbshared[2];
    cout<<"\t"<<ihitbshared[3]<<endl;
    
    // choosing best branches for each hit
    TrackDeadBeforeRemoveSharing=0;
    for (i=0;i<mNTrack;i++) 
      if (mTrack[i]->GetDone()==1) {
	OneIsGood=0;
	for (j=0;j<mTrack[i]->GetNBranches();j++)
	  if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	if (OneIsGood==0 && 
	    onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	    mTrack[i]->GetIdealPattern()>=nminhit)
	  TrackDeadBeforeRemoveSharing++;
      }
    RemoveHitSharing();
    
    TrackDeadAfterRemoveSharing=0;
    for (i=0;i<mNTrack;i++) 
      if (mTrack[i]->GetDone()==1) {
	OneIsGood=0;
	for (j=0;j<mTrack[i]->GetNBranches();j++)
	  if (mTrack[i]->GetBranch(j)->GetIsGood()==1) OneIsGood=1;
	if (OneIsGood==0 && 
	    onoffmatrix == (mTrack[i]->GetIdealPattern()& onoffmatrix) &&
	    mTrack[i]->GetIdealPattern()>=nminhit)
	  TrackDeadAfterRemoveSharing++;
      }
    cout<<"--> TrackDead (b/a) RemoveSharing = "<<TrackDeadBeforeRemoveSharing<<"  "<<TrackDeadAfterRemoveSharing<<endl;
    

    // flagging the tracks which we assume as found
    FinishFlag();
    // reinitialize the branch helix of the track which are dropped
    ReInitializeHelix();

    // Doing the evaluation for the superpass.
    if(mParams[0]->debug>0) Eval(onoffmatrix,nminhit);
    //      Eval(0,1);
  }// for(mSuperPass...


  EsttoGlobtrk();

  if(mParams[0]->debug>0) {
    cout<<"StEstMaker::Make() ****STOP****"<<endl;
  }  

  info->snapshot();
  info->print();
  
  CleanUp();

  info->snapshot();
  info->print();
  
  return kStOK;
} 

void StEstMaker::TrackDebug() {
  
  long i,j,k,l,brmax,ok,tok;
  StEstBranch *branch;
  StEstHit *hit;
  StEstTrack *track;
  
  if(mParams[0]->debug>2) 
    cout << "StEstMaker::TrackDebug **** START ****   Number of tracks:  "<<mNTrack<<endl;
  tok=0;
  for (i=0;i<mNTrack; i++) {
    track = mTrack[i];
    ok=0;
    for (k=0;k<track->GetNBranches();k++) 
      if (track->GetBranch(k)->mTrack != track) {
	if(mParams[0]->debug>3) 
	  cout << "no proper track "<<track->mTPCTrack->mId<<" in branch "<<track->GetBranch(k)<<endl;
	tok=1;
      }
  }
  if (tok==0) 
    if(mParams[0]->debug>2) 
      cout << "Tracks in branches - OK"<<endl;

  tok=0;
  for (i=0;i<mNTrack; i++) {
    for (j=0;j<mTrack[i]->GetNBranches();j++) {
      branch = mTrack[i]->GetBranch(j);
      for (k=0;k<branch->GetNHits();k++) {
	hit = branch->GetHit(k);
	brmax= hit->GetNBranch();
	ok=0;
	for (l=0;l<brmax;l++) 
	  if (hit->GetBranch(l) == branch) ok++;
	if (ok==0) {
	  if(mParams[0]->debug>3) {  
	    cout << "no proper branch "<<branch<<" in hit "<<hit->mId<<endl;
	    cout << "   track #"<<i<<"  nbr_hit= "<<brmax<<endl;
	  }
	  for (l=0;l<brmax;l++) {
	    if(mParams[0]->debug>3)
	      cout << "   hit->GetBranch("<<l<<")= "<<hit->GetBranch(l)<<endl;
	  }
	}
	if (ok>1) 
	  if(mParams[0]->debug>3) cout << "branch "<<branch<<" is "<<ok<<" times in hit "<<hit->mId<<endl;
	if (ok!=1) tok=1;
      }
    }
  }
  if (tok==0) 
    if(mParams[0]->debug>2) 
      cout << "Branches in hits - OK"<<endl;

  tok=0;
  for (i=0;i<mNSvtHit;i++) {
    hit = mSvtHit[i];
    if (hit == NULL) cout <<"HIT NULL!!!"<<endl;

    for (j=0;j<hit->GetNBranch();j++) {
      ok=0;
      branch= hit->GetBranch(j);
      if (branch == NULL) cout <<"BRANCH NULL!!! in hit "<<hit->mId<<endl;

      for (k=0;k<branch->GetNHits();k++) 
	if (branch->GetHit(k)==hit) ok++;
      if (ok==0) {
	if(mParams[0]->debug>3) {
	  cout << "no proper hit "<<hit<<" in branch "<<branch<<endl;
	  cout << "   hit#"<<i<<" mId="<<hit->mId<< " NBranch="<<hit->GetNBranch()<<" j="<<j<<endl;
	}
      }
      if (ok>1) 
	if(mParams[0]->debug>3) 
	  cout << "hit "<<hit->mId<<" is "<<ok<<" times in branch "<<branch<<endl; 
      if (ok!=1) tok=1;
    }
  }
  if (tok==0) 
    if(mParams[0]->debug>2) cout << "Hits in branches - OK"<<endl;

  if(mParams[0]->debug>2) 
    cout << "StEstMaker::TrackDebug **** STOP ****"<<endl;
}
  

void StEstMaker::PrintSettings() {
  // Print the tracking parameter settings.

  long j,k;
  ofstream sortie("out.log",ios::app);

  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"\t\t\t SETTINGS OF THE Pt PASSES\t\t\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"number of passes "<<mNPass;
  for (j=0;j<mNPass;j++) cout<<"\t| "<<j;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"ptmin\t\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ptmin;
  cout<<"\t|"<<endl;
  cout<<"ptmax\t\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ptmax;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" lin. geom. cut";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->geomcutl[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" cir. geom. cut";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->geomcutw[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"layer 0123 on(1)/off(0)";
  for (j=0;j<mNPass;j++) {
    cout<<"\t| ";
    for (k=0;k<4;k++) cout<<mParams[j]->onoff[k];
  }
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" ring of neighb.";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nneighbours[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" branching";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nbranch[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" total branching";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ntotbranch[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" hit sharing";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->share[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"max tpc hits in tracks";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxtpchits;
  cout<<"\t|"<<endl;
  cout<<"max svt hits in tracks";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxsvthits;
  cout<<"\t|"<<endl;
  cout<<"max number of branches";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxbranches;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"phi bin size\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->phibin;
  cout<<"\t|"<<endl;
  cout<<"z bin size\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->zbin;
  cout<<"\t|"<<endl;
  cout<<"number of phi bins";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nphibins;
  cout<<"\t|"<<endl;
  cout<<"number of z bins";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nzbins;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"\t\t SETTINGS OF THE SEGMENT SUPERPASSES\t\t\t\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"number of superpasses "<<mNSuperPass;
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<j;
  cout<<"\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"chisq cut\t";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->chisqcut;
  cout<<"\t|"<<endl;
  cout<<"min TPC hits\t";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->minTPChits;
  cout<<"\t|"<<endl;
  cout<<"TPCtrack max orig.(x,y)";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->rminTPC;
  cout<<"\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"min SVT/SSD hits  ";
  for (j=0;j<mNSuperPass;j++) {
    cout<<"\t| ";
    cout<<mSegments[j]->minhits;
    }
  cout<<"\t|"<<endl;
  cout<<"hit in layer 0123";
  for (j=0;j<mNSuperPass;j++)
    {
    cout<<"\t| ";
    for (k=0;k<4;k++) cout<<mSegments[j]->slay[k];
    }
  cout<<"\t|"<<endl;
  cout<<"requiered (2) possible(1) excluded (0) \t\t\t\t\t\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;

  if (sortie) {
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"\t\t\t SETTINGS OF THE Pt PASSES\t\t\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"number of passes "<<mNPass;
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<j;
  sortie<<"\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"ptmin\t\t";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->ptmin;
  sortie<<"\t|"<<endl;
  sortie<<"ptmax\t\t";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->ptmax;
  sortie<<"\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" lin. geom. cut";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->geomcutl[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" cir. geom. cut";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->geomcutw[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"layer 0123 on(1)/off(0)";
  for (j=0;j<mNPass;j++) {
    sortie<<"\t| ";
    for (k=0;k<4;k++) sortie<<mParams[j]->onoff[k];
  }
  sortie<<"\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" ring of neighb.";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->nneighbours[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" branching";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->nbranch[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" total branching";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->ntotbranch[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      sortie<<"layer "<<k<<" hit sharing";
      for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->share[k];
      sortie<<"\t|"<<endl;
    }
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"max tpc hits in tracks";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->maxtpchits;
  sortie<<"\t|"<<endl;
  sortie<<"max svt hits in tracks";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->maxsvthits;
  sortie<<"\t|"<<endl;
  sortie<<"max number of branches";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->maxbranches;
  sortie<<"\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<"phi bin size\t";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->phibin;
  sortie<<"\t|"<<endl;
  sortie<<"z bin size\t";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->zbin;
  sortie<<"\t|"<<endl;
  sortie<<"number of phi bins";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->nphibins;
  sortie<<"\t|"<<endl;
  sortie<<"number of z bins";
  for (j=0;j<mNPass;j++) sortie<<"\t| "<<mParams[j]->nzbins;
  sortie<<"\t|"<<endl;
  sortie<<"-----------------------------------------------------------------"<<endl;
  sortie<<endl;
  sortie<<"-------------------------------------------------------------------------------"<<endl;
  sortie<<"\t\t SETTINGS OF THE SEGMENT SUPERPASSES\t\t\t\t|"<<endl;
  sortie<<"-------------------------------------------------------------------------------"<<endl;
  sortie<<"number of superpasses "<<mNSuperPass;
  for (j=0;j<mNSuperPass;j++) sortie<<"\t| "<<j;
  sortie<<"\t|"<<endl;
  sortie<<"-------------------------------------------------------------------------------"<<endl;
  sortie<<"chisq cut\t";
  for (j=0;j<mNSuperPass;j++) sortie<<"\t| "<<mSegments[j]->chisqcut;
  sortie<<"\t|"<<endl;
  sortie<<"min TPC hits\t";
  for (j=0;j<mNSuperPass;j++) sortie<<"\t| "<<mSegments[j]->minTPChits;
  sortie<<"\t|"<<endl;
  sortie<<"TPCtrack max orig.(x,y)";
  for (j=0;j<mNSuperPass;j++) sortie<<"\t| "<<mSegments[j]->rminTPC;
  sortie<<"\t|"<<endl;
  sortie<<"-------------------------------------------------------------------------------"<<endl;
  sortie<<"min SVT/SSD hits  ";
  for (j=0;j<mNSuperPass;j++) {
    sortie<<"\t| ";
    sortie<<mSegments[j]->minhits;
    }
  sortie<<"\t|"<<endl;
  sortie<<"hit in layer 0123";
  for (j=0;j<mNSuperPass;j++)
    {
    sortie<<"\t| ";
    for (k=0;k<4;k++) sortie<<mSegments[j]->slay[k];
    }
  sortie<<"\t|"<<endl;
  sortie<<"requiered (2) possible(1) excluded (0) \t\t\t\t\t\t|"<<endl;
  sortie<<"-------------------------------------------------------------------------------"<<endl;
  }

}

void StEstMaker::BuildIdealBranches() {

  int nseg,matrix,l[4],idealnhits;
  int iret,flaglog[8];
  int fitstatus;
  long i,j,slay,mcid,nsvthit;
  long slay_found;
  long IsolatedTPCTracks,AssociatedTPCTracks;
  double dist,distw,distl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;

  IsolatedTPCTracks=0;
  AssociatedTPCTracks=0;
  for (i=0;i<8;i++) flaglog[i]=0;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->GetFlag()>0) {
      if (mTrack[i]->mTPCTrack->mPt>mParams[mNPass-1]->ptmin && mTrack[i]->mTPCTrack->mPt<mParams[0]->ptmax) {
	mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
	nseg=0;
	for (j=0;j<mNTrack;j++)
	  if (mTrack[i]->mTPCTrack->GetFlag()>0 && Eval_id_mctrk2est_Track[mTrack[j]->mTPCTrack->GetMcId()]==mcid)
	    nseg++;
	// get the ideal branch pattern= sum(2^layer)
	// and the ideal number of hits (counting one/layer)
	j=0;
	matrix=0;
	l[0]=0;
	l[1]=0;
	l[2]=0;
	l[3]=0;
	idealnhits=0;
	while (Eval_mchits[mcid][j]!=NULL && j<10) {
	  matrix= matrix | int(pow(2,Eval_mchits[mcid][j]->GetWafer()->GetLayer()));
	  if (l[Eval_mchits[mcid][j]->GetWafer()->GetLayer()]==0) 
	    l[Eval_mchits[mcid][j]->GetWafer()->GetLayer()]++;
	  j++;
	}
	nsvthit=j;
	
	for (j=0;j<4;j++) idealnhits=idealnhits+l[j];
	
	if (matrix==0) IsolatedTPCTracks++;
	if (matrix!=0) AssociatedTPCTracks++;
	mTrack[i]->SetIdealPattern(matrix);
	IdealPattern->Fill((float) matrix);
	mTrack[i]->SetIdealNHits(idealnhits);
	IdealNHits->Fill((float) idealnhits);

	branch = new StEstBranch(NULL, long(mParams[0]->maxsvthits));
	if (branch==NULL)
	  cerr << "ERROR StEstMaker::BuildIdealBranches branch==NULL" << endl;
	else { // the perfect branch has been created
	  branch->JoinTrack(mTrack[i],1);
	  StThreeVector<double> temp(mVertex->GetGlobX()->x(),
				     mVertex->GetGlobX()->y(),
				     mVertex->GetGlobX()->z());
	  dca=mTrack[i]->mTPCTrack->GetHelix()->distance(temp); 
	  // copy of the tpc helix.
	  StHelix *helix_for_idealbranch = new StHelix(*mTrack[i]->mTPCTrack->GetHelix());
	  branch->SetHelix(helix_for_idealbranch);


      	  if (dca<3.) {
   	    iret=RefitBranch2(branch,NULL,-1,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }	  
    	  else {
    	    iret=RefitBranch2(branch,NULL,-1,0.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }
	  for (slay=3;slay>=0;slay--) {
	    if(mParams[mPass]->onoff[slay]) {
	      j=0;
	      while (Eval_mchits[mcid][j]!=NULL && j<10) {      
		if (Eval_mchits[mcid][j]->GetWafer()->GetLayer()==slay){
		  XWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetX()->x());
		  XWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetX()->y());
		  XWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetX()->z());
		  NWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetN()->x());
		  NWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetN()->y());
		  NWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetN()->z());
		  sd=branch->GetHelix()->pathLength(XWaf,NWaf);
		  if (sd<1000) {
		    Proj=branch->GetHelix()->at(sd);
		    dist=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			      (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()) +
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    distw=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			       (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()));
		    distl=sqrt((Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			       (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    
		    if (slay==0 && matrix==15) {
		      disthitip0->Fill((float) dist);   
		      disthitip0w->Fill((float) distw);   
		      disthitip0l->Fill((float) distl);   
		    }
		    if (slay==1 && matrix==15) {
		      disthitip1->Fill((float) dist);   
		      disthitip1w->Fill((float) distw);   
		      disthitip1l->Fill((float) distl);   
		    }
		    if (slay==2 && matrix==15) {
		      disthitip2->Fill((float) dist);   
		      disthitip2w->Fill((float) distw);   
		      disthitip2l->Fill((float) distl);   
		    }
		    if (slay==3 && matrix==15) {
		      dca_all->Fill((float) dca);
		      if (mTPCTrack[i]->mType==1) dca_pri->Fill((float) dca);
		      if (mTPCTrack[i]->mType==2) dca_sec->Fill((float) dca);
		      disthitip3->Fill((float) dist);   
		      disthitip3w->Fill((float) distw);   
		      disthitip3l->Fill((float) distl);   
		      if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p->Fill((float) dist); 
		      if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s->Fill((float) dist); 
		      if (mTrack[i]->mTPCTrack->mPt>mParams[0]->ptmin &&
			  mTrack[i]->mTPCTrack->mPt<=mParams[0]->ptmax) {
			disthitip3_p0->Fill((float) dist);   
			if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p_p0->Fill((float) dist); 
			if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s_p0->Fill((float) dist); 
		      }
		      if (mNPass>1 && mTrack[i]->mTPCTrack->mPt>mParams[1]->ptmin &&
			  mTrack[i]->mTPCTrack->mPt<=mParams[1]->ptmax) {
			disthitip3_p1->Fill((float) dist);   
			if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p_p1->Fill((float) dist); 
			if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s_p1->Fill((float) dist); 
		      }
		      if (mNPass>2 && mTrack[i]->mTPCTrack->mPt>mParams[2]->ptmin &&
			  mTrack[i]->mTPCTrack->mPt<=mParams[2]->ptmax) {
			disthitip3_p2->Fill((float) dist);   
			if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p_p2->Fill((float) dist); 
			if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s_p2->Fill((float) dist); 
		      }
		      if (mNPass>3 && mTrack[i]->mTPCTrack->mPt>mParams[3]->ptmin &&
			  mTrack[i]->mTPCTrack->mPt<=mParams[3]->ptmax) {
			disthitip3_p3->Fill((float) dist);   
			if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p_p3->Fill((float) dist); 
			if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s_p3->Fill((float) dist); 
		      }
		      if (mNPass>4 && mTrack[i]->mTPCTrack->mPt>mParams[4]->ptmin &&
			  mTrack[i]->mTPCTrack->mPt<=mParams[4]->ptmax) {
			disthitip3_p4->Fill((float) dist);   
			if (mTrack[i]->mTPCTrack->mType==1) disthitip3_p_p4->Fill((float) dist); 
			if (mTrack[i]->mTPCTrack->mType==2) disthitip3_s_p4->Fill((float) dist); 
		      }
		    }
		    branch->AddHit(Eval_mchits[mcid][j],dist);
		    RefitBranch2(branch,NULL,-1,0,&fitstatus);
		    if (slay==3 && matrix==15) 
		      chisqib3->Fill((float) mTrack[i]->mIdealBranch->GetChiSq());   
		    if (slay==2 && matrix==15) 
		      chisqib2->Fill((float) mTrack[i]->mIdealBranch->GetChiSq());   
		    if (slay==1 && matrix==15) 
		      chisqib1->Fill((float) mTrack[i]->mIdealBranch->GetChiSq());   
		    if (slay==0 && matrix==15) 
		      chisqib0->Fill((float) mTrack[i]->mIdealBranch->GetChiSq());   
		  }
		}
		j++;
	      }
	    }
	  }
	  if (matrix!=0) {
	    disthitipchisq->Fill((float) mTrack[i]->mIdealBranch->GetChiSq()-mTrack[i]->mTPCTrack->mChiSq);
	    disthitipchisql->Fill((float) mTrack[i]->mIdealBranch->GetChiSqLin()-mTrack[i]->mTPCTrack->mChiSqLin);
	    disthitipchisqw->Fill((float) mTrack[i]->mIdealBranch->GetChiSqCir()-mTrack[i]->mTPCTrack->mChiSqCir);
	  }
	}
      }
    }
  }
  cout<<"End of Build Ideal Branches"<<endl;
  cout<<"Number of TPC Tracks without SVT/SSD hits :"<<IsolatedTPCTracks<<endl;
  cout<<"Number of TPC Tracks with    SVT/SSD hits :"<<AssociatedTPCTracks<<endl;
  cout<<"Fit status : ";
  for (i=0;i<8;i++) cout<<flaglog[i]<<" ";
  cout<<endl;
}


void StEstMaker::BuildFindableBranches() {

  int matrix;
  int iret,flaglog[8];
  int fitstatus;
  long i,j,slay,mcid;
  long CorrectPass;
  double dist,distw,distl,distsl,sd;
  double dca;
  StEstBranch *branch;
  StThreeVector<double> Proj;
  StThreeVector<double> XWaf;
  StThreeVector<double> NWaf;

  for (i=0;i<8;i++) flaglog[i]=0;
  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->GetFlag()>0) {
      if (mTrack[i]->mTPCTrack->mPt>mParams[mNPass-1]->ptmin && mTrack[i]->mTPCTrack->mPt<mParams[0]->ptmax) {

	CorrectPass=mNPass-1;
 	for (j=0;j<mNPass;j++)
 	  if (mTrack[i]->mTPCTrack->mPt>mParams[j]->ptmin && 
 	      mTrack[i]->mTPCTrack->mPt<mParams[j]->ptmax) CorrectPass=j;
	mcid = Eval_id_mctrk2est_Track[mTrack[i]->mTPCTrack->GetMcId()];
	branch = new StEstBranch(NULL, long(mParams[0]->maxsvthits));
	if (branch==NULL)
	  cerr << "ERROR StEstMaker::BuildFindableBranches branch==NULL" << endl;
	else { // the perfect branch has been created
	  branch->JoinTrack(mTrack[i],2);
	  StThreeVector<double> temp(mVertex->GetGlobX()->x(),
				     mVertex->GetGlobX()->y(),
				     mVertex->GetGlobX()->z());
	  dca=mTrack[i]->mTPCTrack->GetHelix()->distance(temp); 
	  // copy of the tpc helix.
	  StHelix *helix_for_findablebranch = new StHelix(*mTrack[i]->mTPCTrack->GetHelix());
	  branch->SetHelix(helix_for_findablebranch);


      	  if (dca<3.) {
   	    iret=RefitBranch2(branch,NULL,-1,1.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }	  
    	  else {
    	    iret=RefitBranch2(branch,NULL,-1,0.,&fitstatus);
	    if (iret!=1)  flaglog[7]++;
	    else {
	      if (fitstatus==1) flaglog[0]++;
	      else flaglog[abs(fitstatus)]++;
	    }
	  }
	  for (slay=3;slay>=0;slay--) {
	    if(mParams[mPass]->onoff[slay]) {
	      j=0;
	      while (Eval_mchits[mcid][j]!=NULL && j<10) {      
		if (Eval_mchits[mcid][j]->GetWafer()->GetLayer()==slay){
		  XWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetX()->x());
		  XWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetX()->y());
		  XWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetX()->z());
		  NWaf.setX(Eval_mchits[mcid][j]->GetWafer()->GetN()->x());
		  NWaf.setY(Eval_mchits[mcid][j]->GetWafer()->GetN()->y());
		  NWaf.setZ(Eval_mchits[mcid][j]->GetWafer()->GetN()->z());
		  sd=branch->GetHelix()->pathLength(XWaf,NWaf);

		  if (sd<1000) {
		    Proj=branch->GetHelix()->at(sd);
		    dist=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			      (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			      (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()) +
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			      (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    distw=sqrt((Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x())*
			       (Proj.x()-Eval_mchits[mcid][j]->GetGlobX()->x()) + 
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y())*
			       (Proj.y()-Eval_mchits[mcid][j]->GetGlobX()->y()));
		    distl=sqrt((Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z())*
			       (Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z()));
		    distsl=Proj.z()-Eval_mchits[mcid][j]->GetGlobX()->z();
		    if (distl<mParams[CorrectPass]->geomcutl[slay] && 
			distw<mParams[CorrectPass]->geomcutw[slay]) {
		      branch->AddHit(Eval_mchits[mcid][j],dist);
		      RefitBranch2(branch,NULL,-1,0,&fitstatus);
		    }
		  }
		  else dist=999.;
		}
		j++;
	      }

	    }
	  }
	}
	matrix=0;
	for (j=0;j<branch->GetNHits();j++) 
	  matrix= matrix | int(pow(2,branch->GetHit(j)->GetWafer()->GetLayer()));
	mTrack[i]->SetFindablePattern(matrix);
	mTrack[i]->SetFindableNHits(branch->GetNHits());
      }
    }
  }
  cout<<"End of Build Findable Branches"<<endl;
  cout<<"Fit status : ";
  for (i=0;i<8;i++) cout<<flaglog[i]<<" ";
  cout<<endl;
}


void StEstMaker::PrintTrackDetails(int trackid) {

  long i,lm,lm2;

  for (i=0;i<mNTrack;i++) {
    if (mTrack[i]->mTPCTrack->mId==trackid) {
      cout<<"------------------------------------------------------------"<<endl;
      cout<<"Track id "<<trackid
	  <<" mcid="<<mTrack[i]->mTPCTrack->GetMcId()
	  <<" type="<<mTrack[i]->mTPCTrack->mType
	  <<" Pt(tpc)="<<mTrack[i]->mTPCTrack->mPt
	  <<" Nhits(tpc)="<<mTrack[i]->mTPCTrack->GetNHits()
	  <<" R(tpc)="<<mTrack[i]->mTPCTrack->GetR()<<endl;
      cout<<"Flag= "<<mTrack[i]->GetFlag()
	  <<" Pid= "<<mTrack[i]->mTPCTrack->GetPid()
	  <<" ParentPid= "<<mTrack[i]->mTPCTrack->mParentPid 
	  <<" ParentMcId= "<<mTrack[i]->mTPCTrack->mParentMcId<<endl; 
      cout<<"mcid shared with track id : ";
      for (lm=0;lm<mNTPCTrack;lm++) {
	if (mTPCTrack[lm]->GetMcId()==mTrack[i]->mTPCTrack->GetMcId()&&mTPCTrack[lm]->GetId()!=trackid)
	  cout<<mTPCTrack[lm]->GetId()
	      <<" (pt="<<mTPCTrack[lm]->mPt
	      <<" nhits="<<mTPCTrack[lm]->GetNHits()
	      <<" r="<<mTPCTrack[lm]->GetR()<<") ";
      }
      cout<<endl;
      cout<<"Ideal Pattern : "<<mTrack[i]->GetIdealPattern()<<" IdealNHits : "<<mTrack[i]->GetIdealNHits()<<endl;
      if (mTrack[i]->mIdealBranch) {
	cout<<"Ideal branch : "<<mTrack[i]->mIdealBranch->GetNHits()<<" hit_id (dist) : ";
	for (lm=0;lm<mTrack[i]->mIdealBranch->GetNHits();lm++) 
	  cout<<mTrack[i]->mIdealBranch->GetHit(lm)->GetId()<<" ("
	      <<mTrack[i]->mIdealBranch->GetDist(lm)<<") ";
	cout<<endl;
	cout<<"Ideal branch :  wafer_id for the hits : ";
	for (lm=0;lm<mTrack[i]->mIdealBranch->GetNHits();lm++) 
	  cout<<mTrack[i]->mIdealBranch->GetHit(lm)->GetWafer()->GetId()<<" ";
	cout<<endl;
      }
      else cout<<"No Ideal branch"<<endl;
      cout<<"Findable Pattern : "<<mTrack[i]->GetFindablePattern()
	  <<" FindableNHits : "<<mTrack[i]->GetFindableNHits()<<endl;
      if (mTrack[i]->mFindableBranch) {
	cout<<"Findable branch : "<<mTrack[i]->mFindableBranch->GetNHits()<<" hit_id wafer_id sharing flag dist : "<<endl;
	for (lm=0;lm<mTrack[i]->mFindableBranch->GetNHits();lm++) {
	  cout<<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetId()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetWafer()->GetId()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetNShare()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetHit(lm)->GetFlag()
	      <<"\t"<<mTrack[i]->mFindableBranch->GetDist(lm);
	  for (lm2=0;lm2<mTrack[i]->mFindableBranch->GetHit(lm)->GetNBranch();lm2++)
	    cout<<" "<<mTrack[i]->mFindableBranch->GetHit(lm)->GetBranch(lm2)->GetTrack()->mTPCTrack->GetId();
	  cout<<endl;
	}
      }
      else cout<<"No Findable branch"<<endl;
      cout<<"From the tracking : Nbranches="<<mTrack[i]->GetNBranches()<<endl;
      for (lm=0;lm<mTrack[i]->GetNBranches();lm++) {
	cout<<"br,good,goodold,nhit,nfit,fitstatus,chisq,hit_id,waf_id,dist : "<<lm
	    <<" "<<mTrack[i]->GetBranch(lm)->GetIsGood()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetIsGoodOld()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetNHits()
	    <<" "<<mTrack[i]->GetBranch(lm)->GetNFit()
	    <<" "<<mTrack[i]->GetBranch(lm)->mLastFitStatus
	    <<" "<<mTrack[i]->GetBranch(lm)->GetChiSq()<<endl;
        for (lm2=0;lm2<mTrack[i]->GetBranch(lm)->GetNHits();lm2++) {
          cout<<mTrack[i]->GetBranch(lm)->GetHit(lm2)->mId<<"\t"
	      <<mTrack[i]->GetBranch(lm)->GetHit(lm2)->GetWafer()->GetId()<<"\t"
	      <<mTrack[i]->GetBranch(lm)->GetDist(lm2)
	      <<" ("<<mTrack[i]->GetBranch(lm)->GetHit(lm2)->GetNBranch()<<") "<<endl;
        }
      }
      cout<<"------------------------------------------------------------"<<endl;
    }
  }
}

void StEstMaker::CheckConsistency() {
  // Method to do a consistency double check between the branch list of the hits
  // and the hit list of the branches
  // We check first that all branches referenced in the branch list of a hit possess the
  // hit in their hit list.
  // Then we check that all branches that possess the hit in their list are referenced in 
  // branch list of the hit.
  // The check is done for all the hits with DebugLevel>0

  long i,j,k,l,m;
  int TheHitIsFound,TheBranchIsFound;
  cout<<"Consistency check 1 : all branches in the hit listes possess the hit"<<endl;
  for (i=0;i<mNSvtHit;i++) {
    if (mSvtHit[i]->GetDebugLevel()>0) {
      for (j=0;j<mSvtHit[i]->GetNBranch();j++) {
	for (k=0;k<mNTrack;k++){
	  for (l=0;l<mTrack[k]->GetNBranches();l++) {
	    if (mTrack[k]->GetBranch(l)==mSvtHit[i]->GetBranch(j)) {
	      TheHitIsFound=0;
	      for (m=0;m<mTrack[k]->GetBranch(l)->GetNHits();m++) {
		if (mTrack[k]->GetBranch(l)->GetHit(m)==mSvtHit[i]) TheHitIsFound=1;
	      }
	      if (TheHitIsFound==0) {
		cout<<"-----------------> ERROR : the hit "<<mSvtHit[i];
		cout<<" id : "<<mSvtHit[i]->GetId();
		cout<<" is not in the list of the branch "<<mSvtHit[i]->GetBranch(j);
		cout<<" from the track "<<mTrack[k];
		cout<<" id : "<<mTrack[k]->mTPCTrack->GetId()<<endl;
	      }
	    }
	  }
	}						    
      }
    }
  }
  cout<<"Consistency check 2 : all hits in the branch listes possess the branch"<<endl;
  for (k=0;k<mNTrack;k++){
    for (l=0;l<mTrack[k]->GetNBranches();l++) {
      for (m=0;m<mTrack[k]->GetBranch(l)->GetNHits();m++) {
	if (mTrack[k]->GetBranch(l)->GetHit(m)->GetDebugLevel()>0) {
	  for (i=0;i<mNSvtHit;i++) {
	    if (mSvtHit[i]==mTrack[k]->GetBranch(l)->GetHit(m)) {
	      TheBranchIsFound=0;
	      for (j=0;j<mSvtHit[i]->GetNBranch();j++) {
		if (mSvtHit[i]->GetBranch(j)==mTrack[k]->GetBranch(l)) TheBranchIsFound=1;
	      }
	      if (TheBranchIsFound==0) {
		cout<<"-----------------> ERROR : the branch "<<mTrack[k]->GetBranch(l);
		cout<<" from the track "<<mTrack[k];
		cout<<" id : "<<mTrack[k]->mTPCTrack->GetId();
		cout<<" is not in the list of the hit "<<mSvtHit[i];
		cout<<" id : "<<mSvtHit[i]->GetId()<<endl;
	      }
	    }
	  }
	}
      }
    }
  }
}


void StEstMaker::CleanUp(){

  delete [] mIndexWaf;
  delete [] mTPCTrack;
  delete [] mTrack;
  delete [] mSvtHit;
}
