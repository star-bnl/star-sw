// $Id: StXTrakMaker.cxx,v 1.3 2016/06/01 01:06:33 perev Exp $
/// \File StXTrakMaker.cxx
/// \author V.Perev 2016
//
/*!

\class StXTrakMaker

A maker StXTrakMaker is a auxiliary maker for Sti/StiCA/Stv packages.
<br>
Main tasks:
<ul>
<li> Xtend/prolong StTrack to far detector;
<li> Save produced data into StEvent.
</ul>
*/
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "TSystem.h"
#include "TCernLib.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StTrack.h"
#include "StPrimaryTrack.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#include "StContainers.h"
#include "StVertex.h"
#include "StThreeVectorD.hh"

#include "StXTrakMaker.h"
#include "StXTrak.h"
#include "TGeoManager.h"
#include "TGeoSwim.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoMaterial.h"
#include "TFile.h"

#include "Sti/StiElossCalculator.h"
#include "StvUtil/StvELossTrak.h"
#include "StiUtilities/StiDebug.h"
#include "StEvent/StEventSummary.h"

ClassImp(StXTrakMaker)

#include <map>
std::map<TString,int> myMap;


//_____________________________________________________________________________
StXTrakMaker::StXTrakMaker(const Char_t *name) :
    StMaker(name)

{
}

//_____________________________________________________________________________
StXTrakMaker::~StXTrakMaker()
{
  cout <<"StXTrakMaker::~StXTrakMaker() -I- Started/Done"<<endl;
}

//_____________________________________________________________________________
void StXTrakMaker::Clear(const char*)
{
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StXTrakMaker::Finish()
{
  printf("\\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n");
  for (auto myMapIt = myMap.begin();myMapIt!=myMap.end();++myMapIt) {
    const char *pat = (*myMapIt).first.Data();
    int num = (*myMapIt).second;
    printf ("%d  %s\n",num,pat);
  }
  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StXTrakMaker::Init()
{
  mSwim = new TGeoSwim;
  TGeoSwimMag  *myMag  = new MyMag;
  TGeoSwimLoss *myLoss = new MyLoss;
  mSwim->Set(myMag,myLoss,0);
  mSwim->Set(400.,-400.,400.,5.);
  mELoss=0;mMyMag=0;
//   mELoss = new StvELossTrak;
//   mELoss->Reset(1);
//   mMyMag = new MyMag;

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StXTrakMaker::Make()
{
static const double kRxyMax=500;
static const char* farDets[]={
"BTOF","BBCA","CALB","ECAL","ETTV",
"FGTM","FBOX","FSCE","HCMO","MUTD",
"PHMD","VPDD","ZCAL",
0};

static const double EC = 2.99792458e-3;

  auto *myMag = mSwim->GetMag();
  double B[3];
  static double B00=0;
  if (B00<=0) { 
    double pos[3]={0};
    (*myMag)(pos,B);
    B00=B[2];
  }

  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (!event) return kStWarn;
  const StVertex *vtx=0;
  double VTX[3];
  const double *myPos,*myDir;
  double curv,P,Pt,p,pt,dPtot,rxy;
  double pos[3],dir[3],POS[3],DIR[3];
  const StEventSummary *summ = event->summary();
  double magSumm = summ->magneticField()*EC; if (magSumm){};
  double cosTh,cosTh2,inLen,otLen,dP,lenTofIn,lenTofOt,lenStTr,lenExt;
  int iend=0,nFound=0;
  const StSPtrVecTrackNode& nodes= event->trackNodes();
  int nNodes = nodes.size();
  for (int iNode = 0;iNode <nNodes; iNode++) {
    vtx = 0;
    double maxStep = 0;
    lenExt = 0;
    const StTrackNode *node = nodes[iNode];
    const StTrack *track = node->track(primary);
    int iprim = track!=0;
    if (!iprim) continue;
    if (!track) { track = node->track(global);}
    assert(track);
    lenStTr = track->length();

    if (!iprim) { //it is global track. Length is incorrect. Fix it.
      const StTrackGeometry* geo = track->geometry();
      assert(geo);
      int h = geo->helicity();
      curv = geo->curvature(); if (h<0) curv = -curv;
      StThreeVectorD stpos = geo->origin();
      StThreeVectorD stmom = geo->momentum();
      myPos = stpos.xyz();
      myDir = stmom.xyz();
      THelixTrack th(myPos,myDir,curv); 
      th.Backward();
      lenStTr+=th.Path(0.,0.);
    } else {
      vtx = ((const StPrimaryTrack*)track)->vertex();
      memcpy(VTX,StThreeVectorD(vtx->position()).xyz(),sizeof(VTX));



    }
    dPtot = 0;
    nFound = 0;
    const StTrackGeometry* geo = track->outerGeometry();
    assert(geo);
    int h = geo->helicity();
    int CHARGE = geo->charge();
    double CURV = geo->curvature(); if (h<0) CURV = -CURV;
    if (fabs(CURV )>1./200) continue;    
    StThreeVectorD STPOS = geo->origin();
    StThreeVectorD STMOM = geo->momentum();
    StThreeVectorD STDIR(STMOM);
    STDIR = STDIR.unit();
    cosTh = STDIR.perp();
    STDIR*=1./cosTh;
    THelixTrack hlxBeg(STPOS.xyz(),STMOM.xyz(),CURV);
    hlxBeg.Backward();
    lenTofIn = hlxBeg.Path(VTX);
    hlxBeg.Backward();
    StThreeVectorD stpos(STPOS),stmom(STMOM);
    myPos = stpos.xyz();
    myDir = STDIR.xyz();
    P  = STMOM.mag(); p = P;
    Pt = STMOM.perp(); pt = Pt;
//     double Eta = stmom.pseudoRapidity();
//     double Phi = stmom.phi()*57.;
    memcpy(pos,myPos,sizeof(pos));
    memcpy(dir,myDir,sizeof(dir));
    memcpy(POS,myPos,sizeof(pos));
    memcpy(DIR,myDir,sizeof(dir));
    double rxyPre = 0;
    int found = 0,oldFound = 0;

//   (*mMyMag)(pos,B);
     (*myMag)(pos,B);
//		Loop over volumes
    int iVol = 0;
    for ( iVol=0; iVol<1000;iVol++) {
      curv = -B[2]*CHARGE/pt;
      if (!iVol) {
	if (curv*CURV<0) {curv = -curv; CHARGE=-CHARGE;}
	assert(iVol || curv*CURV>0);
      }

      const TGeoMaterial *gmate=0;
      TString path;
      double maxLen = 5;

//	Loop inside one volume with big ELoss

      do {
        mSwim->Set(pos,dir,curv);
        gmate = mSwim->GetMate();
        if (!gmate) {iend = 11; break;}
	iend = mSwim->Swim(maxLen);
	if (iend>1) {printf("End=%d\n",iend); break; }
        path = mSwim->GetPath();
	inLen = mSwim->GetLen(0);
	otLen = mSwim->GetLen(1);
        dP = 0;
#if 0	
        dP = (*myLoss)(gmate,p,otLen);
#endif
#ifdef StvELoss
        mELoss->Reset(1);
        mELoss->Set(gmate,p);


#endif
#ifdef StiELoss
      double A = gmate->GetA(),Z=gmate->GetZ(),D=gmate->GetDensity();
//    double X0=gmate->GetRadLen();
      StiElossCalculator ecal(Z/A,0,A,Z,D);
      double beta2 = p*p/(p*p+PiMASS*PiMASS);
      double dedx  = ecal.calculate(1., PiMASS,beta2);
      dP = dedx*otLen*sqrt(1+pow(PiMASS/p,2));
#endif
#ifdef StvELoss
       mELoss->Add(otLen);
       dP = mELoss->PLoss(p);
#endif

    }while(0);

//		We got new position
    if (iend) StiDebug::Count("End:Rxy",rxy,iend);
    if (iend) break;
    myPos = mSwim->GetPos(1);
    myDir = mSwim->GetDir(1);
    rxy = sqrt(myPos[0]*myPos[0]+myPos[1]*myPos[1]);
    memcpy(pos,myPos,sizeof(pos));
    memcpy(dir,myDir,sizeof(dir));
    StiDebug::Count("StepSize:Rxy",rxy,otLen);
    double dca = hlxBeg.Dca(pos);
    cosTh2 = (1.-dir[2])*(1.+dir[2]);
    cosTh = sqrt(cosTh2);
    for (int j=0;j<3;j++){ dir[j]/=cosTh;}
    double dpt = pt/p*dP;
    pt -= dpt;
    if (pt<1e-2) { iend = 1313; break; }
    dPtot += dP;
    lenExt += otLen;
    double lenxy2 = cosTh2*otLen*otLen;
    double lenxy = cosTh*otLen;

    (*myMag)(pos,B);
    double dc = -B[2]*CHARGE/pt -curv;
    double dphi = dc*lenxy /2.;
    double dh   = dc*lenxy2/6.;

    curv +=dc; p -= dP;
    assert(p>0);
    pos[0]+=-dir[1]*dh;
    pos[1]+= dir[0]*dh;
    double cosPhi = dir[0]; if (cosPhi){}
    dir[0] += -dir[1]*dphi;
    dir[1] +=  dir[0]*dphi;
    rxy = sqrt(pos[0]*pos[0]+pos[1]*pos[1]);
    if (rxy<400 && fabs(B[2]/B00)>1e-3) {
      StiDebug::Count("Br/Bz:Rxy",rxy   ,sqrt(B[0]*B[0]+B[1]*B[1])/B[2]);
      StiDebug::Count("Br/Bz:Z"  ,pos[2],sqrt(B[0]*B[0]+B[1]*B[1])/B[2]);
      StiDebug::Count("Bz:Rxy)"    ,rxy,B[2]/B00);
    }
    dP = mSwim->GetPLoss();
    dPtot += dP;
static int pri=0;
if (pri)
      printf("%6d - Rxy = %g(%g %g) \tName=%s/%s\n",iVol,rxy,inLen,dP/p
            ,mSwim->GetNode(1)->GetName()
            ,path.Data());

      if (rxy>kRxyMax) 	break;
      if (rxy<rxyPre )  break;
      found = 0;
      for (int j=0;farDets[j];j++) {
        if (path.Index(farDets[j])<0) continue;
        found = j+1; break;
      }
      rxyPre = rxy;
      if (found && found!=oldFound)     {
	    oldFound = found; nFound++;
	    THelixTrack hlx(STPOS.xyz(),STMOM.xyz(),CURV);
	    double dist = (stpos-StThreeVectorD(pos)).mag();
	    hlx.Move(dist);
	    dca = hlx.Dca(pos);


	    lenTofOt = hlx.Path(pos)+dist;
	    if (iprim) {
              int i = path.Index("RefSys");
              if (i>=0) {path.Remove(0,i+9);}
              TString ts;ts += int(rxy);
	      ts+="|"; ts +=path;
              myMap[ts]++;

	      StiDebug::Count("DetIdx",found);
	      StiDebug::Count("PriDca:Z"  ,pos[2] , dca);
	      StiDebug::Count("PriDca:Rxy",rxy    , dca);
	      StiDebug::Count("PriDca:Ptinv",1./pt, dca);
	      StiDebug::Count("PriDca:PLoss",dPtot, dca);
	      StiDebug::Count("PriY:X",pos[0], pos[1]);
	      StiDebug::Count("PriRxy:Z",pos[2], rxy);
	      StiDebug::Count("PriPLoss:Z",pos[2],dPtot);
	      StiDebug::Count("PriPLoss:Ptin",1./pt, dPtot);
	      StiDebug::Count("LenInnRes:Z",pos[2],lenTofIn-lenStTr);

              ts="PriDca:Z_";  ts+=farDets[found-1];
	      StiDebug::Count(ts.Data(),pos[2], dca);
              ts="PriDca:Rxy_";  ts+=farDets[found-1];
	      StiDebug::Count(ts.Data(),rxy, dca);

              ts="LenOutRes:Z_";
	      StiDebug::Count(ts.Data(),pos[2],lenTofOt-lenExt);
	      ts+=farDets[found-1]; 
	      StiDebug::Count(ts.Data(),pos[2],lenTofOt-lenExt);
              ts="LenOutRes:Rxy_";ts+=farDets[found-1]; 
	      StiDebug::Count(ts.Data(),rxy,lenTofOt-lenExt);
              ts="PLoss:Rxy_";ts+=farDets[found-1]; 
	      StiDebug::Count(ts.Data(),rxy,dPtot);
              ts="PLoss:Z_";ts+=farDets[found-1]; 
	      StiDebug::Count(ts.Data(),pos[2],dPtot);



	    } else {
	    }


	}
  }//End iVol

}//End stnodes
  return kStOK;
}

