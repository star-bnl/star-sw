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
#include "StTpcDb/StTpcDbMaker.h"
#include "St_db_Maker/St_db_Maker.h"


ClassImp(StRedoTracks)
  
//_____________________________________________________________________________
StRedoTracks::StRedoTracks(const char *name, StTpcDbMaker* mkr):StMaker(name),
 m_ExB(0), tpcDbMaker(mkr), redo(kTRUE) {
}
//_____________________________________________________________________________
StRedoTracks::~StRedoTracks() {}
//_____________________________________________________________________________
Int_t StRedoTracks::Init(){
  // Find StTpcDbMaker
  if (!tpcDbMaker) {
    StMakerIter iter(GetParentChain());
    StMaker* mkr;
    while ((mkr = iter.NextMaker())) {
      if (mkr->IsA() == StTpcDbMaker::Class()) {
        tpcDbMaker = (StTpcDbMaker*) mkr;
        break;
      }
    }
    if (!tpcDbMaker) gMessMgr->Warning("StRedoTracks: No StTpcDbMaker found.");
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StRedoTracks::Make(){
  // Get instance of StMagUtilities
  int option = 0;
  if (!m_ExB) {
#ifdef __NEW_MagUtilities__
    m_ExB = new StMagUtilities(tpcDbMaker->tpcDbInterface(),option);
#else
    TDataSet *RunLog = GetDataBase("RunLog/MagFactor");
    if (!RunLog) gMessMgr->Warning("StRedoTracks: No RunLog/MagFactor found.");
    m_ExB = new StMagUtilities(tpcDbMaker->tpcDbInterface(),RunLog,option);
#endif
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
  // Currently set to 200 microns for infinite tracks, goes as 1/::sqrt(Ntracks)
  UInt_t nPrims = pvtx->numberOfDaughters();
  Float_t pv_err = TMath::Sqrt(0.0004 + (0.11/nPrims));

  for (i=0; i<theNodes.size(); i++) {
    typ = global; typ2 = IsGlobal;
    Bool_t iterate = kTRUE;
    while (iterate) {
      for (j=0; j<theNodes[i]->entries(typ); j++) {
        StTrack* tri = (StTrack *) theNodes[i]->track(typ,j);
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
// $Id: StRedoTracks.cxx,v 1.7 2018/06/29 17:21:24 perev Exp $
// $Log: StRedoTracks.cxx,v $
// Revision 1.7  2018/06/29 17:21:24  perev
// Irakli_Jun29
//
// Revision 1.6  2012/11/07 23:27:55  fisyak
// Add place holder for new StMagUtilities
//
// Revision 1.5  2012/10/01 17:50:06  genevb
// Reduce some overhead DB queries by being more specific about needed tables
//
// Revision 1.4  2003/09/02 17:58:09  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2003/07/02 15:32:38  genevb
// Remove StTpcDb.so dependence
//
// Revision 1.1  2003/06/28 00:46:15  genevb
// Introduction of StRedoTracks class
//
//
