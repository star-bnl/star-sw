// $Id: StXTrakMaker.cxx,v 1.10 2018/10/06 23:42:55 perev Exp $
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
#include "TVector3.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StEvent/StTrack.h"
#include "StEvent/StPrimaryTrack.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StExtGeometry.h"
#include "StEvent/StContainers.h"
#include "StEvent/StVertex.h"
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
//#include "StvUtil/StvELossTrak.h"
#include "StiUtilities/StiDebug.h"
#include "StEvent/StEventSummary.h"

ClassImp(StXTrakMaker)


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
  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StXTrakMaker::Init()
{
  mSwim = new StXTrak;
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StXTrakMaker::Make()
{
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
  double rxy,dca,*pos;
  const StEventSummary *summ = event->summary();if(summ){};
  double lenStTr=0,lenOutHlx=0,lenInnHlx=0,lenExt=0;
  const StSPtrVecTrackNode& nodes= event->trackNodes();
  int nNodes = nodes.size();
  for (int iNode = 0;iNode <nNodes; iNode++) {
    mSwim->Clear();
//  ===================================================
    StTrackNode *node = nodes[iNode];
    StTrack *track = node->track(primary);
    int iprim = track!=0;
    if (!iprim) continue;
    if (!track) { track = node->track(global);}
    assert(track);
    lenStTr = track->length();

    const StTrackGeometry* geo1st = track->geometry();
    assert(geo1st);
    StThreeVectorD priPOS = geo1st->origin();
    StThreeVectorD priMOM = geo1st->momentum();
    int            priCHARGE = geo1st->charge();
    auto h = geo1st->helicity();
    double priCURV = geo1st->curvature(); if (h<0) priCURV = -priCURV;
    if (fabs(priCURV  )>1./90) continue;    
    THelixTrack priHLX(priPOS.xyz(),priMOM.xyz(),priCURV);

    mSwim->Set1stPoint(priCHARGE,priPOS.xyz(),priMOM.xyz());
//  ===================================================
    const StTrackGeometry* geo2nd = track->outerGeometry();
    assert(geo2nd);
    int CHARGE = geo2nd->charge();
    h = geo2nd->helicity();
    double CURV = geo2nd->curvature(); if (h<0) CURV = -CURV;
    if (fabs(CURV )>1./90) continue;    
    StThreeVectorD STPOS = geo2nd->origin();
    StThreeVectorD STMOM = geo2nd->momentum();
    mSwim->Set2ndPoint(CHARGE,STPOS.xyz(),STMOM.xyz());
    double lenSimp = mSwim->GetLen(2);
    mSwim->SetLen2nd(lenStTr);
//  ===================================================
    THelixTrack HLX(STPOS.xyz(),STMOM.xyz(),CURV);
    TString found;

    while(1) {// Loop along the track

      int	iEnd = mSwim->Next();
  //  ===================================================

      if (iEnd) break;
      if (found == mSwim->GetName()) continue;
      found = mSwim->GetName();
      auto &aux = mSwim->GetAux();
      pos = aux.mPos;
      lenExt = aux.mLen-lenStTr;
      rxy = TVector3(pos).Perp();
      dca = HLX.Dca(pos);
      lenOutHlx = HLX.Path(pos);
      lenInnHlx = fabs(HLX.Path(0.,0.));

      StExtGeometry *xg = new StExtGeometry;
      xg->setName(found.Data());
      double pars[6];
      pars[StExtGeometry::kPhi ] = atan2(pos[1],pos[0]);
      pars[StExtGeometry::kZ   ] = pos[2];
      pars[StExtGeometry::kPsi ] = atan2(aux.mMom[1],aux.mMom[0]);
      pars[StExtGeometry::kPti ] = aux.mPti;
      pars[StExtGeometry::kTan ] = atan2(aux.mMom[2],aux.mPt);
      pars[StExtGeometry::kCurv] = aux.mCurv;
      xg->setLength(aux.mLen);
      xg->set(rxy,pars,0);
      track->addExtGeometry(xg);


      
      TString ts;
      if (iprim) {
	ts ="PriDca:Z."; ts+=found;
	StiDebug::Count(ts  ,pos[2] , 		dca);
	ts ="PriDca:Rxy."; ts+=found;
	StiDebug::Count(ts,rxy    , 		dca);
	ts ="PriDca:Ptinv."; ts+=found;
	StiDebug::Count(ts,aux.mPti, 		dca);
	ts ="PriDca:PLoss."; ts+=found;
	StiDebug::Count(ts,aux.mPLoss, 	dca);
	ts ="PriY:X."; 
	StiDebug::Count(ts,pos[0], pos[1]);
	ts ="PriRxy:Z.";
	StiDebug::Count(ts,pos[2], rxy);
	ts ="PriPLoss:Z."; ts+=found;
	StiDebug::Count(ts,pos[2],aux.mPLoss);
	ts ="PriPLoss:Pti."; ts+=found;
	StiDebug::Count(ts,aux.mPti, aux.mPLoss);
	ts ="LenOutRes:Z."; ts+=found;
	StiDebug::Count(ts,pos[2],lenExt-lenOutHlx);
	ts ="LenInnRes1:Z.";
	StiDebug::Count(ts,pos[2],(lenSimp-lenStTr));
	ts ="LenInnRes2:Z.";
	StiDebug::Count(ts,pos[2],(lenInnHlx-lenStTr));
	StiDebug::Count("Detectors",found.Data());

        if (found.Contains("TOF")) {
	  double myTime = mSwim->GetTimeF();
	  StiDebug::Count("myTOFTime",myTime*1e6);
          double beta = (lenSimp+lenOutHlx)/myTime/TMath::C();
          if(beta<1) {
            double p = mSwim->GetAux(1).mP;
	    double mass = p*sqrt((1.-beta)*(1+beta))/beta-mSwim->GetMass();
	    StiDebug::Count("DeltaMass_1",p,mass);
            p = mSwim->GetAux(2).mP;
	    mass = p*sqrt((1.-beta)*(1+beta))/beta-mSwim->GetMass();
	    StiDebug::Count("DeltaMass_2",p,mass);
          }          

	}
	

      } else {//global track
      }
    }// end along the track
  }//End stnodes
  return kStOK;
}

