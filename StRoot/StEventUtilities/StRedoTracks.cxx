//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StRedoTracks redoes the space charge correction                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StRedoTracks.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StTpcDb/StTpcDb.h"
#include "St_db_Maker/St_db_Maker.h"


ClassImp(StRedoTracks)
  
//_____________________________________________________________________________
StRedoTracks::StRedoTracks(const char *name):StMaker(name),
 m_ExB(0), redo(kTRUE) {}
//_____________________________________________________________________________
StRedoTracks::~StRedoTracks() {}
//_____________________________________________________________________________
Int_t StRedoTracks::Init(){
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StRedoTracks::Make(){
  int option = 0;
  if (!m_ExB) {
    TDataSet *RunLog = GetDataBase("RunLog");
    if (!RunLog) gMessMgr->Warning("StRedoTracks: No RunLog found.");
    m_ExB = new StMagUtilities(gStTpcDb,RunLog,option);
  }

  StEvent* event = (StEvent*) GetInputDS("StEvent");
  if (!event) {
    gMessMgr->Warning("StRedoTracks: no StEvent; skipping event.");
    return kStWarn;
  }

  unsigned int i,j,k;
  float x[3],p[3],x_new[3],p_new[3];
  StTrackGeometry* triGeom = 0;
  StThreeVectorD ooo;
  StTrackType typ;
  Prime typ2;

  StPrimaryVertex* pvtx = event->primaryVertex();
  if (!pvtx) return kStOk;
  ooo = pvtx->position();
  StSPtrVecTrackNode& theNodes = event->trackNodes();

  // Assign an error to the primary vertex for primary track fits
  UInt_t nPrims = pvtx->numberOfDaughters();
  Float_t pv_err = TMath::Sqrt(0.0004 + (0.11/nPrims));

  for (i=0; i<theNodes.size(); i++) {
    typ = global; typ2 = IsGlobal;
    Bool_t iterate = kTRUE;
    while (iterate) {
      for (j=0; j<theNodes[i]->entries(typ); j++) {
        StTrack* tri = theNodes[i]->track(typ,j);
        const StTrackTopologyMap& map = tri->topologyMap();
        for (k=0; k<2; k++) {
          if (k) triGeom = tri->outerGeometry();
          else triGeom = tri->geometry();

          StThreeVectorF xvec = triGeom->origin();
          if (!(xvec.x() || xvec.y() || xvec.z())) continue;
          StThreeVectorF pvec = triGeom->momentum();
          if (!(pvec.x() || pvec.y())) continue;

          float oldPt = pvec.perp();
          if (oldPt < 0.0001) continue;
          p[0] = pvec.x();
          p[1] = pvec.y();
          p[2] = pvec.z();
          x[0] = xvec.x();
          x[1] = xvec.y();
          x[2] = xvec.z();

          if (!redo) continue;
          m_ExB->FixSpaceChargeDistortion(triGeom->charge(),x,p,
            typ2,x_new,p_new,map.data(0),map.data(1),pv_err);

          StThreeVectorF npvec(p_new);
          StThreeVectorF nxvec(x_new);
          float newPt = npvec.perp();
          float inv_newPt = 1.0/newPt;
          float psi = TMath::ACos(npvec.x()*inv_newPt);
          if (npvec.y() < 0) psi = TMath::TwoPi() - psi;     // psi: 0...2pi

          triGeom->setMomentum(npvec);
          triGeom->setOrigin(nxvec);
          triGeom->setCurvature(triGeom->curvature()*pvec.perp()*inv_newPt);
          triGeom->setDipAngle(TMath::ATan(npvec.z()*inv_newPt));
          triGeom->setPsi(psi);
      
        }
      } // loop over j tracks
      if (typ == global) { typ = primary; typ2 = IsPrimary; }
      else iterate = kFALSE;
    } // loop over track types
  } // loop over i Nodes

 
  return kStOK;
}
//_____________________________________________________________________________
// $Id: StRedoTracks.cxx,v 1.1 2003/06/28 00:46:15 genevb Exp $
// $Log: StRedoTracks.cxx,v $
// Revision 1.1  2003/06/28 00:46:15  genevb
// Introduction of StRedoTracks class
//
//
