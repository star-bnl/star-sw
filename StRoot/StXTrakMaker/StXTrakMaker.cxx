// $Id: StXTrakMaker.cxx,v 1.5 2016/07/26 01:06:47 perev Exp $
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
  double lenStTr,lenHlx,lenExt;
  const StSPtrVecTrackNode& nodes= event->trackNodes();
  int nNodes = nodes.size();
  for (int iNode = 0;iNode <nNodes; iNode++) {
    mSwim->Clear();
//  ===================================================
    const StTrackNode *node = nodes[iNode];
    const StTrack *track = node->track(primary);
    int iprim = track!=0;
    if (!iprim) continue;
    if (!track) { track = node->track(global);}
    assert(track);
    lenStTr = track->length();

    const StTrackGeometry* geo1st = track->geometry();
    assert(geo1st);
    StThreeVectorD stpos = geo1st->origin();
    StThreeVectorD stmom = geo1st->momentum();
    int           charge = geo1st->charge();
    double curv = geo1st->curvature(); 
    if (fabs(curv )>1./200) continue;    

    mSwim->Set1stPoint(charge,stpos.xyz(),stmom.xyz());
//  ===================================================
    const StTrackGeometry* geo2nd = track->outerGeometry();
    assert(geo2nd);
    int CHARGE = geo2nd->charge();
    auto h = geo2nd->helicity();
    double CURV = geo2nd->curvature(); if (h<0) CURV = -CURV;
    if (fabs(CURV )>1./200) continue;    
    StThreeVectorD STPOS = geo2nd->origin();
    StThreeVectorD STMOM = geo2nd->momentum();
    mSwim->Set2ndPoint(CHARGE,STPOS.xyz(),STMOM.xyz());
    mSwim->SetLen2nd(lenStTr);
//  ===================================================
    THelixTrack HLX(STPOS.xyz(),STMOM.xyz(),CURV);
    stpos = STPOS;
    stmom = STMOM;
    TString found;

    while(1) {// Loop along the track

      int	iEnd = mSwim->Next();
  //  ===================================================

      if (iEnd) break;
      if (found == mSwim->GetName()) continue;
      found = mSwim->GetName();
      auto &aux = mSwim->GetAux();
      pos = aux.mPos;
      lenExt = aux.mLen;
      rxy = TVector3(pos).Perp();
      dca = HLX.Dca(pos);
      lenHlx = HLX.Path(pos)+mSwim->GetAux(1).mLen;
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
	ts ="LenInnRes:Z."; ts+=found;
	StiDebug::Count(ts,pos[2],lenExt-lenHlx);
	StiDebug::Count("Detectors",found.Data());

      } else {//global track
      }
    }// end along the track
  }//End stnodes
  return kStOK;
}

