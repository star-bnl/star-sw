// $Id: StEventQAMaker.cxx,v 2.9 2001/05/01 15:17:36 genevb Exp $
// $Log: StEventQAMaker.cxx,v $
// Revision 2.9  2001/05/01 15:17:36  genevb
// Execute EMC code only if EMC libs loaded
//
// Revision 2.8  2001/04/30 19:09:27  genevb
// Catch missing EMC info
//
// Revision 2.7  2001/04/28 22:05:13  genevb
// Added EMC histograms
//
// Revision 2.6  2001/04/25 21:35:25  genevb
// Added V0 phi distributions
//
// Revision 2.5  2001/04/24 22:53:51  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.4  2001/04/24 21:33:05  genevb
// Use det_id to identify detectors, and some cleanup
//
// Revision 2.3  2000/12/08 18:37:22  genevb
// Change kTofPatchId->kTofId
//
// Revision 2.2  2000/09/08 18:55:53  lansdell
// turned on FTPC primary track histograms
//
// Revision 2.1  2000/09/01 16:59:02  genevb
// Change for V0 plots
//
// Revision 2.0  2000/08/25 16:02:39  genevb
// New revision: new structure, multiplicity classes
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StEventQAMaker class for QA Histograms using StEvent                 //
//     adapted from St_QA_Maker                                          //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include "PhysicalConstants.h"
#include <math.h>
#include "TROOT.h"
#include "TMath.h"
#include "SystemOfUnits.h"
#include "StQABookHist.h"
#include "TH1.h"
#include "TH2.h"
//#include "TSpectrum.h"
#include "StEventQAMaker.h"
#include "StEventTypes.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StDbUtilities/StCoordinates.hh"
#include "HitHistograms.h"
#include "StTpcDb/StTpcDb.h"
#include "StarClassLibrary/StTimer.hh"
#include "StMessMgr.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcUtil/StEmcMath.h"

static StEmcGeom* emcGeom[4];

ClassImp(StEventQAMaker)

//_____________________________________________________________________________
StEventQAMaker::StEventQAMaker(const char *name, const char *title) :
 StQAMakerBase(name,title,"StE") {
  
}

//_____________________________________________________________________________
Int_t StEventQAMaker::Finish() {

  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StEventQAMaker::Init() {

// StEventQAMaker - Init; book histograms and set defaults for member functions

  mHitHist = new HitHistograms("QaDedxAllSectors","dE/dx for all TPC sectors",100,0.,1.e-5,2);
  if ((gROOT->GetClass("StEmcMath")) && (gROOT->GetClass("StEmcGeom"))) {
    for(Int_t i=0; i<4; i++) {emcGeom[i] = new StEmcGeom(i+1);}
  }
  return StQAMakerBase::Init();
}

//_____________________________________________________________________________
Int_t StEventQAMaker::Make() {
// StEventQAMaker - Make; fill histograms
  
  event = (StEvent *)GetInputDS("StEvent");
  if (event) {
    if (firstEvent) {
      if (event->info()->type() == "NONE") {
        histsSet = 1;
      } else {
        // process Monte Carlo events
        histsSet = 0;
      }
      BookHist();
    }
    // only process if a primary vertex exists !!!
    if (event->primaryVertex()) {
      multiplicity = event->trackNodes().size();
      mNullPrimVtx->Fill(1);
      if (event->info()->type() == "NONE") {
        histsSet = 1;
      } else {
        // process Monte Carlo events
	histsSet = 0;
      }
      return StQAMakerBase::Make();
    } else {
      gMessMgr->Warning("StEventQAMaker::Make(): no primary vertex found!");
      mNullPrimVtx->Fill(-1);
      return kStOk;
    }
  } else {
    gMessMgr->Error("StEventQAMaker::Make(): no event found!");
    return kStErr;
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistEvSum() {
  // Fill histograms for event summary

  //PrintInfo();
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling event summary histograms ");

  StEventSummary *event_summary = event->summary();
  if (event_summary) {
    Float_t trk_tot =   event_summary->numberOfTracks();
    Float_t trk_good =  event_summary->numberOfGoodTracks();
    Float_t trk_plus =  event_summary->numberOfGoodTracks(positive);
    Float_t trk_minus = event_summary->numberOfGoodTracks(negative);

    m_trk_tot_gd->Fill(trk_good/trk_tot,multClass); 
    m_glb_trk_tot->Fill(trk_tot,multClass);
    m_glb_trk_tot_sm->Fill(trk_tot,multClass);
    m_glb_trk_plusminus->Fill(trk_plus/trk_minus,multClass);
    m_glb_trk_plusminus_sm->Fill(trk_plus/trk_minus,multClass);
    m_glb_trk_prim->Fill(event_summary->numberOfGoodPrimaryTracks(),multClass);
    m_glb_trk_prim_sm->Fill(event_summary->numberOfGoodPrimaryTracks(),multClass);
    m_vert_total->Fill(event_summary->numberOfVertices(),multClass);
    m_vert_total_sm->Fill(event_summary->numberOfVertices(),multClass);
    m_mean_pt->Fill(event_summary->meanPt(),multClass);
    m_mean_pt_sm->Fill(event_summary->meanPt(),multClass);
    m_mean_eta->Fill(event_summary->meanEta(),multClass);
    m_rms_eta->Fill(event_summary->rmsEta(),multClass);

    m_prim_vrtr->Fill(event_summary->primaryVertexPosition().perp(),multClass);
    m_prim_vrtx0->Fill(event_summary->primaryVertexPosition()[0],multClass);
    m_prim_vrtx1->Fill(event_summary->primaryVertexPosition()[1],multClass);
    m_prim_vrtx2->Fill(event_summary->primaryVertexPosition()[2],multClass);
  }

  if (event->softwareMonitor()) {
    StTpcSoftwareMonitor *tpcMon = event->softwareMonitor()->tpc();
    Float_t tpcChgWest=0;
    Float_t tpcChgEast=0;
    for (UInt_t i=0; i<24; i++) {
      if (i<12)
	tpcChgWest += tpcMon->chrg_tpc_in[i]+tpcMon->chrg_tpc_out[i];
      else
	tpcChgEast += tpcMon->chrg_tpc_in[i]+tpcMon->chrg_tpc_out[i];
    }
    m_glb_trk_chg->Fill(tpcChgEast/tpcChgWest,multClass);
  }
}

//-----------------------------------------------------------------
void StEventQAMaker::MakeHistGlob() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling global track histograms ");

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t cnttrk=0;
  Int_t cnttrkg=0;

  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *globtrk = theNodes[i]->track(global);
    if (!globtrk) continue;
    cnttrk += theNodes[i]->entries(global);
    hists->m_globtrk_iflag->Fill(globtrk->flag());
    if (globtrk->flag()>0) {
      StTrackGeometry* geom = globtrk->geometry();
      StTrackFitTraits& fTraits = globtrk->fitTraits();
      StTrackDetectorInfo* detInfo = globtrk->detectorInfo();
      const StTrackTopologyMap& map=globtrk->topologyMap();

      cnttrkg++;
      Float_t pT = -999.;
      pT = geom->momentum().perp();
      Float_t lmevpt = TMath::Log10(pT*1000.0);
      Float_t theta = TMath::ASin(1.) - geom->dipAngle();
      Float_t thetad = theta/degree;
      Float_t eta = geom->momentum().pseudoRapidity();
      Float_t gmom = abs(geom->momentum());
      Float_t lmevmom = TMath::Log10(gmom*1000.0);
      Float_t chisq0 = fTraits.chi2(0);
      Float_t chisq1 = fTraits.chi2(1);
      Float_t nfitntot = (Float_t(fTraits.numberOfFitPoints())) /
	                 (Float_t(detInfo->numberOfPoints()));
      Float_t nfitnmax = (Float_t(fTraits.numberOfFitPoints())) /
                         (Float_t(globtrk->numberOfPossiblePoints()));
      const StThreeVectorF& firstPoint = detInfo->firstPoint();
      const StThreeVectorF& origin = geom->origin();
      StThreeVectorF dif = firstPoint - origin;
      Float_t radf = firstPoint.perp();

      Float_t logImpact = TMath::Log10(globtrk->impactParameter());
      Float_t logCurvature = TMath::Log10(geom->curvature());

      // pathLength(double x,double y) should return path length at
      // DCA in the xy-plane to a given point
      double S = geom->helix().pathLength(0,0);
      StThreeVectorD dcaToBeam = geom->helix().at(S);
      // these histogram additions are for Lanny's evr QA histograms
      hists->m_dcaToBeamXY->Fill(dcaToBeam.x(),dcaToBeam.y());
      hists->m_dcaToBeamZ1->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ2->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ3->Fill(dcaToBeam.z());
      hists->m_zDcaTanl->Fill(dcaToBeam.z(),TMath::Tan(geom->dipAngle()));
      hists->m_zDcaZf->Fill(dcaToBeam.z(),firstPoint.z());
      hists->m_zDcaPsi->Fill(dcaToBeam.z(),geom->psi()/degree);
      if (origin.phi() < 0)
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),360+origin.phi()/degree);
      else
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),origin.phi()/degree);

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 

      // check if the track has hits in a detector -CPL
      if (map.hasHitInDetector(kUnknownId)) hists->m_det_id->Fill(kUnknownId);
      if (map.hasHitInDetector(kTpcId)) hists->m_det_id->Fill(kTpcId);
      if (map.hasHitInDetector(kSvtId)) hists->m_det_id->Fill(kSvtId);
      if (map.hasHitInDetector(kRichId)) hists->m_det_id->Fill(kRichId);
      if (map.hasHitInDetector(kFtpcWestId)) hists->m_det_id->Fill(kFtpcWestId);
      if (map.hasHitInDetector(kFtpcEastId)) hists->m_det_id->Fill(kFtpcEastId);
      if (map.hasHitInDetector(kTofId)) hists->m_det_id->Fill(kTofId);
      if (map.hasHitInDetector(kCtbId)) hists->m_det_id->Fill(kCtbId);
      if (map.hasHitInDetector(kSsdId)) hists->m_det_id->Fill(kSsdId);
      if (map.hasHitInDetector(kBarrelEmcTowerId)) hists->m_det_id->Fill(kBarrelEmcTowerId);
      if (map.hasHitInDetector(kBarrelEmcPreShowerId)) hists->m_det_id->Fill(kBarrelEmcPreShowerId);
      if (map.hasHitInDetector(kBarrelSmdEtaStripId)) hists->m_det_id->Fill(kBarrelSmdEtaStripId);
      if (map.hasHitInDetector(kBarrelSmdPhiStripId)) hists->m_det_id->Fill(kBarrelSmdPhiStripId);
      if (map.hasHitInDetector(kEndcapEmcTowerId)) hists->m_det_id->Fill(kEndcapEmcTowerId);
      if (map.hasHitInDetector(kEndcapEmcPreShowerId)) hists->m_det_id->Fill(kEndcapEmcPreShowerId);
      if (map.hasHitInDetector(kEndcapSmdUStripId)) hists->m_det_id->Fill(kEndcapSmdUStripId);
      if (map.hasHitInDetector(kEndcapSmdVStripId)) hists->m_det_id->Fill(kEndcapSmdVStripId);
      if (map.hasHitInDetector(kZdcWestId)) hists->m_det_id->Fill(kZdcWestId);
      if (map.hasHitInDetector(kZdcEastId)) hists->m_det_id->Fill(kZdcEastId);
      if (map.hasHitInDetector(kMwpcWestId)) hists->m_det_id->Fill(kMwpcWestId);
      if (map.hasHitInDetector(kMwpcEastId)) hists->m_det_id->Fill(kMwpcEastId);
      if (map.hasHitInDetector(kTpcSsdId)) hists->m_det_id->Fill(kTpcSsdId);
      if (map.hasHitInDetector(kTpcSvtId)) hists->m_det_id->Fill(kTpcSvtId);
      if (map.hasHitInDetector(kTpcSsdSvtId)) hists->m_det_id->Fill(kTpcSsdSvtId);
      if (map.hasHitInDetector(kSsdSvtId)) hists->m_det_id->Fill(kSsdSvtId);

      // calculate the probability of a fit being correct
      // number of degrees of freedom = fitpoints-5 (5 params constrain track)
      Double_t ndf = 2*fTraits.numberOfFitPoints()-5;
      Double_t probability = TMath::Prob(chisq0*ndf,ndf);
      hists->m_globtrk_fit_prob->Fill(probability);

// now fill all TPC histograms ------------------------------------------------
      if (map.trackTpcOnly()) {

// these are TPC only
	// m_glb_f0 uses hist class StMultiH1F
        hists->m_glb_f0->Fill(dif.x(),0.);
        hists->m_glb_f0->Fill(dif.y(),1.);
        hists->m_glb_f0->Fill(dif.z(),2.);

        hists->m_glb_xf0->Fill(dif.x());
        hists->m_glb_yf0->Fill(dif.y());
        hists->m_glb_zf0->Fill(dif.z());
        hists->m_glb_impactT->Fill(logImpact);
        hists->m_glb_impactrT->Fill(globtrk->impactParameter());

	// TPC padrow histogram
	StTpcCoordinateTransform transformer(gStTpcDb);
	StGlobalCoordinate globalHitPosition(firstPoint);
	StTpcPadCoordinate padCoord;
	transformer(globalHitPosition,padCoord);
        hists->m_glb_padfT->Fill(padCoord.row());

// these are TPC & FTPC
        hists->m_pointT->Fill(detInfo->numberOfPoints());
        hists->m_max_pointT->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointT->Fill(fTraits.numberOfFitPoints());
        hists->m_glb_chargeT->Fill(geom->charge());

        hists->m_glb_r0T->Fill(origin.perp());
	if (origin.phi() < 0)
	  hists->m_glb_phi0T->Fill(360+origin.phi()/degree);
	else
	  hists->m_glb_phi0T->Fill(origin.phi()/degree);
        hists->m_glb_z0T->Fill(origin.z());
        hists->m_glb_curvT->Fill(logCurvature);

        hists->m_glb_xfT->Fill(firstPoint.x());
        hists->m_glb_yfT->Fill(firstPoint.y());
        hists->m_glb_zfT->Fill(firstPoint.z());
        hists->m_glb_radfT->Fill(radf);
        hists->m_glb_ratioT->Fill(nfitntot);
        hists->m_glb_ratiomT->Fill(nfitnmax);
        hists->m_psiT->Fill(geom->psi()/degree);
        hists->m_tanlT->Fill(TMath::Tan(geom->dipAngle()));
        hists->m_glb_thetaT->Fill(thetad);
        hists->m_etaT->Fill(eta);
        hists->m_pTT->Fill(pT);
        hists->m_momT->Fill(gmom);
        hists->m_lengthT->Fill(globtrk->length());
        hists->m_chisq0T->Fill(chisq0);
        hists->m_chisq1T->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_globtrk_xf_yfT->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthT->Fill(eta,globtrk->length());
        hists->m_npoint_lengthT->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthT->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));

// these are TPC only
        hists->m_pT_eta_recT->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  hists->m_tanl_zfT->Fill(firstPoint.z() -
			   event->primaryVertex()->position().z(),
			   Float_t(TMath::Tan(geom->dipAngle())));
	}
        hists->m_mom_trklengthT->Fill(globtrk->length(),lmevmom);
        hists->m_chisq0_momT->Fill(lmevmom,chisq0);
        hists->m_chisq1_momT->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaT->Fill(eta,chisq0);
        hists->m_chisq1_etaT->Fill(eta,chisq1);
        hists->m_chisq0_dipT->Fill(TMath::Tan(geom->dipAngle()),chisq0);
        hists->m_chisq1_dipT->Fill(TMath::Tan(geom->dipAngle()),chisq1);
        hists->m_chisq0_zfT->Fill(firstPoint.z(),chisq0);
        hists->m_chisq1_zfT->Fill(firstPoint.z(),chisq1);
	if (origin.phi() < 0)
	  hists->m_chisq0_phiT->Fill(360+origin.phi()/degree,chisq0);
	else
	  hists->m_chisq0_phiT->Fill(origin.phi()/degree,chisq0);
        hists->m_nfptonpt_momT->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaT->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CPL
	Float_t phi_deg;
	if (origin.phi() < 0)
	  phi_deg = 360+origin.phi()/degree;
	else
	  phi_deg = origin.phi()/degree;
	Float_t psi_deg = geom->psi()/degree;
        hists->m_psi_phiT->Fill(phi_deg,psi_deg);
      }

// now fill all TPC+SVT histograms --------------------------------------------

      else if (map.trackTpcSvt()) {

        hists->m_glb_f0TS->Fill(dif.x(),0.);
        hists->m_glb_f0TS->Fill(dif.y(),1.);
        hists->m_glb_f0TS->Fill(dif.z(),2.);

        hists->m_glb_xf0TS->Fill(dif.x());
        hists->m_glb_yf0TS->Fill(dif.y());
        hists->m_glb_zf0TS->Fill(dif.z());
        hists->m_glb_impactTS->Fill(logImpact);
        hists->m_glb_impactrTS->Fill(globtrk->impactParameter());

        hists->m_pointTS->Fill(detInfo->numberOfPoints());
        hists->m_max_pointTS->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointTS->Fill(fTraits.numberOfFitPoints());
        hists->m_glb_chargeTS->Fill(geom->charge());

        hists->m_glb_r0TS->Fill(origin.perp());
	if (origin.phi() < 0)
	  hists->m_glb_phi0TS->Fill(360+origin.phi()/degree);
	else
	  hists->m_glb_phi0TS->Fill(origin.phi()/degree);
        hists->m_glb_z0TS->Fill(origin.z());
        hists->m_glb_curvTS->Fill(logCurvature);

        hists->m_glb_xfTS->Fill(firstPoint.x());
        hists->m_glb_yfTS->Fill(firstPoint.y());
        hists->m_glb_zfTS->Fill(firstPoint.z());
        hists->m_glb_radfTS->Fill(radf);
        hists->m_glb_ratioTS->Fill(nfitntot);
        hists->m_glb_ratiomTS->Fill(nfitnmax);
        hists->m_psiTS->Fill(geom->psi()/degree);
        hists->m_tanlTS->Fill(TMath::Tan(geom->dipAngle()));
        hists->m_glb_thetaTS->Fill(thetad);
        hists->m_etaTS->Fill(eta);
        hists->m_pTTS->Fill(pT);
        hists->m_momTS->Fill(gmom);
        hists->m_lengthTS->Fill(globtrk->length());
        hists->m_chisq0TS->Fill(chisq0);
        hists->m_chisq1TS->Fill(chisq1);
        hists->m_globtrk_xf_yfTS->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthTS->Fill(eta,globtrk->length());
        hists->m_npoint_lengthTS->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthTS->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));

        hists->m_pT_eta_recTS->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  hists->m_tanl_zfTS->Fill(firstPoint.z() -
			    event->primaryVertex()->position().z(),
			    Float_t(TMath::Tan(geom->dipAngle())));
	}
        hists->m_mom_trklengthTS->Fill(globtrk->length(),lmevmom);
        hists->m_chisq0_momTS->Fill(lmevmom,chisq0);
        hists->m_chisq1_momTS->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaTS->Fill(eta,chisq0);
        hists->m_chisq1_etaTS->Fill(eta,chisq1);
        hists->m_chisq0_dipTS->Fill(TMath::Tan(geom->dipAngle()),chisq0);
        hists->m_chisq1_dipTS->Fill(TMath::Tan(geom->dipAngle()),chisq1);
        hists->m_chisq0_zfTS->Fill(firstPoint.z(),chisq0);
        hists->m_chisq1_zfTS->Fill(firstPoint.z(),chisq1);
	if (origin.phi() < 0)
	  hists->m_chisq0_phiTS->Fill(360+origin.phi()/degree,chisq0);
	else
	  hists->m_chisq0_phiTS->Fill(origin.phi()/degree,chisq0);

        hists->m_nfptonpt_momTS->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaTS->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CPL
	Float_t phi_deg;
	if (origin.phi() < 0)
	  phi_deg = 360+origin.phi()/degree;
	else
	  phi_deg = origin.phi()/degree;
	Float_t psi_deg = geom->psi()/degree;
        hists->m_psi_phiTS->Fill(phi_deg,psi_deg);
      }

// now fill all FTPC East histograms ------------------------------------------
      else if (map.trackFtpcEast()) {

// these are TPC & FTPC
        hists->m_pointFE->Fill(detInfo->numberOfPoints());
        hists->m_max_pointFE->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointFE->Fill(fTraits.numberOfFitPoints());
        hists->m_glb_chargeFE->Fill(geom->charge());
        hists->m_glb_xfFE->Fill(firstPoint.x());
        hists->m_glb_yfFE->Fill(firstPoint.y());
        hists->m_glb_zfFE->Fill(firstPoint.z());
        hists->m_glb_radfFE->Fill(radf);
        hists->m_glb_ratioFE->Fill(nfitntot);
        hists->m_glb_ratiomFE->Fill(nfitnmax);
        hists->m_psiFE->Fill(geom->psi()/degree);
        hists->m_etaFE->Fill(eta);
        hists->m_pTFE->Fill(pT);
        hists->m_momFE->Fill(gmom);
        hists->m_lengthFE->Fill(globtrk->length());
        hists->m_chisq0FE->Fill(chisq0);
        hists->m_chisq1FE->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_pT_eta_recFE->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFE->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthFE->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFE->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthFE->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));

      }
// now fill all FTPC West histograms ------------------------------------------
      else if (map.trackFtpcWest()) {

// these are TPC & FTPC
        hists->m_pointFW->Fill(detInfo->numberOfPoints());
        hists->m_max_pointFW->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointFW->Fill(fTraits.numberOfFitPoints());
        hists->m_glb_chargeFW->Fill(geom->charge());
        hists->m_glb_xfFW->Fill(firstPoint.x());
        hists->m_glb_yfFW->Fill(firstPoint.y());
        hists->m_glb_zfFW->Fill(firstPoint.z());
        hists->m_glb_radfFW->Fill(radf);
        hists->m_glb_ratioFW->Fill(nfitntot);
        hists->m_glb_ratiomFW->Fill(nfitnmax);
        hists->m_psiFW->Fill(geom->psi()/degree);
        hists->m_etaFW->Fill(eta);
        hists->m_pTFW->Fill(pT);
        hists->m_momFW->Fill(gmom);
        hists->m_lengthFW->Fill(globtrk->length());
        hists->m_chisq0FW->Fill(chisq0);
        hists->m_chisq1FW->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_pT_eta_recFW->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFW->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthFW->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFW->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthFW->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));

      }
    }
  }
  hists->m_globtrk_tot->Fill(cnttrk);
  hists->m_globtrk_tot_sm->Fill(cnttrk);
  hists->m_globtrk_good->Fill(cnttrkg);
  hists->m_globtrk_good_sm->Fill(cnttrkg);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistDE() {
  // histograms filled in MakeHistPID() method
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPrim() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling primary track histograms ");

  Int_t cnttrk=0;
  Int_t cnttrkg=0;

  StPrimaryVertex *primVtx = event->primaryVertex();
  UInt_t daughters=0;
  UInt_t currentNumber=0;
  if (primVtx) {
    for (UInt_t v=0; v<event->numberOfPrimaryVertices(); v++) {
      currentNumber = event->primaryVertex(v)->numberOfDaughters();
      if (currentNumber > daughters) {
	daughters = currentNumber;
	primVtx = event->primaryVertex(v);
      }
    }
  }
  
  if (primVtx) {
    cnttrk = primVtx->numberOfDaughters();
    hists->m_primtrk_tot->Fill(cnttrk);
    hists->m_primtrk_tot_sm->Fill(cnttrk);

    for (UInt_t i=0; i<primVtx->numberOfDaughters(); i++) {
      StTrack *primtrk = primVtx->daughter(i);
      hists->m_primtrk_iflag->Fill(primtrk->flag());

      if (primtrk->flag()>0) {
        StTrackGeometry* geom = primtrk->geometry();
        StTrackFitTraits& fTraits = primtrk->fitTraits();
        StTrackDetectorInfo* detInfo = primtrk->detectorInfo();
        const StTrackTopologyMap& map=primtrk->topologyMap();

        cnttrkg++;
	Float_t pT = -999.;
	pT = geom->momentum().perp();
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - geom->dipAngle();
	Float_t thetad = theta/degree;
	Float_t eta   = geom->momentum().pseudoRapidity();
	Float_t gmom = abs(geom->momentum());
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = fTraits.chi2(0);
	Float_t chisq1 = fTraits.chi2(1);
	Float_t nfitnmax = (Float_t(fTraits.numberOfFitPoints())) /
	                   (Float_t(primtrk->numberOfPossiblePoints()));
        Float_t nfitntot = (Float_t(fTraits.numberOfFitPoints()))/
	                   (Float_t(detInfo->numberOfPoints()));

	Float_t logImpact = TMath::Log10(primtrk->impactParameter());
	Float_t logCurvature = TMath::Log10(geom->curvature());

        const StThreeVectorF& firstPoint = detInfo->firstPoint();
        const StThreeVectorF& origin = geom->origin();

	// need to find position on helix closest to first point on track since
	// the primary vertex is used as the first point on helix for primary
	// tracks -CPL
	double s = geom->helix().
	           pathLength(firstPoint);
	StThreeVectorF dif = firstPoint -
	                     geom->helix().at(s);
        Float_t radf = firstPoint.perp();

	// check if the track has hits in a detector -CPL
	if (map.hasHitInDetector(kUnknownId)) hists->m_pdet_id->Fill(kUnknownId);
	if (map.hasHitInDetector(kTpcId)) hists->m_pdet_id->Fill(kTpcId);
	if (map.hasHitInDetector(kSvtId)) hists->m_pdet_id->Fill(kSvtId);
	if (map.hasHitInDetector(kRichId)) hists->m_pdet_id->Fill(kRichId);
	if (map.hasHitInDetector(kFtpcWestId)) hists->m_pdet_id->Fill(kFtpcWestId);
	if (map.hasHitInDetector(kFtpcEastId)) hists->m_pdet_id->Fill(kFtpcEastId);
	if (map.hasHitInDetector(kTofId)) hists->m_pdet_id->Fill(kTofId);
	if (map.hasHitInDetector(kCtbId)) hists->m_pdet_id->Fill(kCtbId);
	if (map.hasHitInDetector(kSsdId)) hists->m_pdet_id->Fill(kSsdId);
	if (map.hasHitInDetector(kBarrelEmcTowerId)) hists->m_pdet_id->Fill(kBarrelEmcTowerId);
	if (map.hasHitInDetector(kBarrelEmcPreShowerId)) hists->m_pdet_id->Fill(kBarrelEmcPreShowerId);
	if (map.hasHitInDetector(kBarrelSmdEtaStripId)) hists->m_pdet_id->Fill(kBarrelSmdEtaStripId);
	if (map.hasHitInDetector(kBarrelSmdPhiStripId)) hists->m_pdet_id->Fill(kBarrelSmdPhiStripId);
	if (map.hasHitInDetector(kEndcapEmcTowerId)) hists->m_pdet_id->Fill(kEndcapEmcTowerId);
	if (map.hasHitInDetector(kEndcapEmcPreShowerId)) hists->m_pdet_id->Fill(kEndcapEmcPreShowerId);
	if (map.hasHitInDetector(kEndcapSmdUStripId)) hists->m_pdet_id->Fill(kEndcapSmdUStripId);
	if (map.hasHitInDetector(kEndcapSmdVStripId)) hists->m_pdet_id->Fill(kEndcapSmdVStripId);
	if (map.hasHitInDetector(kZdcWestId)) hists->m_pdet_id->Fill(kZdcWestId);
	if (map.hasHitInDetector(kZdcEastId)) hists->m_pdet_id->Fill(kZdcEastId);
	if (map.hasHitInDetector(kMwpcWestId)) hists->m_pdet_id->Fill(kMwpcWestId);
	if (map.hasHitInDetector(kMwpcEastId)) hists->m_pdet_id->Fill(kMwpcEastId);
	if (map.hasHitInDetector(kTpcSsdId)) hists->m_pdet_id->Fill(kTpcSsdId);
	if (map.hasHitInDetector(kTpcSvtId)) hists->m_pdet_id->Fill(kTpcSvtId);
	if (map.hasHitInDetector(kTpcSsdSvtId)) hists->m_pdet_id->Fill(kTpcSsdSvtId);
	if (map.hasHitInDetector(kSsdSvtId)) hists->m_pdet_id->Fill(kSsdSvtId);

// now fill all TPC histograms ------------------------------------------------
        if (map.trackTpcOnly()) {

// these are TPC only
	  hists->m_prim_f0->Fill(dif.x(),0.);
	  hists->m_prim_f0->Fill(dif.y(),1.);
	  hists->m_prim_f0->Fill(dif.z(),2.);

	  hists->m_prim_xf0->Fill(dif.x());
	  hists->m_prim_yf0->Fill(dif.y());
	  hists->m_prim_zf0->Fill(dif.z());
	  hists->m_prim_impactT->Fill(logImpact);
	  hists->m_prim_impactrT->Fill(primtrk->impactParameter());

	  // TPC gains histograms
	  if (event->summary()) {
	    mHitHist->clear();
	    mHitHist->setTrack(primtrk);
	    mHitHist->setBField(event->summary()->magneticField());
	    mHitHist->findHits();
	    mHitHist->fillHistograms();
	  }

// these are TPC & FTPC
	  hists->m_ppointT->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointT->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointT->Fill(fTraits.numberOfFitPoints());
	  hists->m_prim_chargeT->Fill(geom->charge());

	  hists->m_prim_r0T->Fill(origin.perp());
	  if (origin.phi() < 0)
	    hists->m_prim_phi0T->Fill(360+origin.phi()/degree);
	  else
	    hists->m_prim_phi0T->Fill(origin.phi()/degree);
	  hists->m_prim_z0T->Fill(origin.z());
	  hists->m_prim_curvT->Fill(logCurvature);

	  hists->m_prim_xfT->Fill(firstPoint.x());
	  hists->m_prim_yfT->Fill(firstPoint.y());
	  hists->m_prim_zfT->Fill(firstPoint.z());
	  hists->m_prim_radfT->Fill(radf);
	  hists->m_prim_ratioT->Fill(nfitntot);
	  hists->m_prim_ratiomT->Fill(nfitnmax);
	  hists->m_ppsiT->Fill(geom->psi()/degree);
	  hists->m_ptanlT->Fill(TMath::Tan(geom->dipAngle()));
	  hists->m_prim_thetaT->Fill(thetad);
	  hists->m_petaT->Fill(eta);
	  hists->m_ppTT->Fill(pT);
	  hists->m_pmomT->Fill(gmom);
	  hists->m_plengthT->Fill(primtrk->length());
	  hists->m_pchisq0T->Fill(chisq0);
	  hists->m_pchisq1T->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_primtrk_xf_yfT->Fill(firstPoint.x(),
				 firstPoint.y());
	  hists->m_peta_trklengthT->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthT->Fill(primtrk->length(),
				  Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthT->Fill(primtrk->length(),
				  Float_t(fTraits.numberOfFitPoints()));

// these are TPC only
	  hists->m_ppT_eta_recT->Fill(eta,lmevpt);
	  hists->m_ptanl_zfT->Fill(firstPoint.z() -
			    event->primaryVertex()->position().z(),
			    Float_t(TMath::Tan(geom->dipAngle())));
	  hists->m_pmom_trklengthT->Fill(primtrk->length(),lmevmom);
	  hists->m_pchisq0_momT->Fill(lmevmom,chisq0);
	  hists->m_pchisq1_momT->Fill(lmevmom,chisq1);
	  hists->m_pchisq0_etaT->Fill(eta,chisq0);
	  hists->m_pchisq1_etaT->Fill(eta,chisq1);
	  hists->m_pchisq0_dipT->Fill(TMath::Tan(geom->dipAngle()),chisq0);
	  hists->m_pchisq1_dipT->Fill(TMath::Tan(geom->dipAngle()),chisq1);
	  hists->m_pchisq0_zfT->Fill(firstPoint.z(),chisq0);
	  hists->m_pchisq1_zfT->Fill(firstPoint.z(),chisq1);
	  hists->m_pnfptonpt_momT->Fill(lmevmom,nfitntot);
	  hists->m_pnfptonpt_etaT->Fill(eta,nfitntot);
	  // had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	  // for some strange reason... -CPL
	  Float_t phi_deg;
	  if (origin.phi() < 0)
	    phi_deg = 360+origin.phi()/degree;
	  else
	    phi_deg = origin.phi()/degree;
	  Float_t psi_deg = geom->psi()/degree;
	  hists->m_ppsi_phiT->Fill(phi_deg,psi_deg);
	}

// now fill all TPC+SVT histograms --------------------------------------------

        else if (map.trackTpcSvt()) {

	  hists->m_prim_f0TS->Fill(dif.x(),0.);
	  hists->m_prim_f0TS->Fill(dif.y(),1.);
	  hists->m_prim_f0TS->Fill(dif.z(),2.);

	  hists->m_prim_xf0TS->Fill(dif.x());
	  hists->m_prim_yf0TS->Fill(dif.y());
	  hists->m_prim_zf0TS->Fill(dif.z());
	  hists->m_prim_impactTS->Fill(logImpact);
	  hists->m_prim_impactrTS->Fill(primtrk->impactParameter());

	  hists->m_ppointTS->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointTS->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointTS->Fill(fTraits.numberOfFitPoints());
	  hists->m_prim_chargeTS->Fill(geom->charge());

	  hists->m_prim_r0TS->Fill(origin.perp());
	  if (origin.phi() < 0)
	    hists->m_prim_phi0TS->Fill(360+origin.phi()/degree);
	  else
	    hists->m_prim_phi0TS->Fill(origin.phi()/degree);
	  hists->m_prim_z0TS->Fill(origin.z());
	  hists->m_prim_curvTS->Fill(logCurvature);

	  hists->m_prim_xfTS->Fill(firstPoint.x());
	  hists->m_prim_yfTS->Fill(firstPoint.y());
	  hists->m_prim_zfTS->Fill(firstPoint.z());
	  hists->m_prim_radfTS->Fill(radf);
	  hists->m_prim_ratioTS->Fill(nfitntot);
	  hists->m_prim_ratiomTS->Fill(nfitnmax);
	  hists->m_ppsiTS->Fill(geom->psi()/degree);
	  hists->m_ptanlTS->Fill(TMath::Tan(geom->dipAngle()));
	  hists->m_prim_thetaTS->Fill(thetad);
	  hists->m_petaTS->Fill(eta);
	  hists->m_ppTTS->Fill(pT);
	  hists->m_pmomTS->Fill(gmom);
	  hists->m_plengthTS->Fill(primtrk->length());
	  hists->m_pchisq0TS->Fill(chisq0);
	  hists->m_pchisq1TS->Fill(chisq1);

	  hists->m_primtrk_xf_yfTS->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthTS->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthTS->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthTS->Fill(primtrk->length(),
				   Float_t(fTraits.numberOfFitPoints()));

	  hists->m_ppT_eta_recTS->Fill(eta,lmevpt);
	  hists->m_ptanl_zfTS->Fill(firstPoint.z() -
			     event->primaryVertex()->position().z(),
			     Float_t(TMath::Tan(geom->dipAngle())));
	  hists->m_pmom_trklengthTS->Fill(primtrk->length(),lmevmom);
	  hists->m_pchisq0_momTS->Fill(lmevmom,chisq0);
	  hists->m_pchisq1_momTS->Fill(lmevmom,chisq1);
	  hists->m_pchisq0_etaTS->Fill(eta,chisq0);
	  hists->m_pchisq1_etaTS->Fill(eta,chisq1);
	  hists->m_pchisq0_dipTS->Fill(TMath::Tan(geom->dipAngle()),chisq0);
	  hists->m_pchisq1_dipTS->Fill(TMath::Tan(geom->dipAngle()),chisq1);
	  hists->m_pchisq0_zfTS->Fill(firstPoint.z(),chisq0);
	  hists->m_pchisq1_zfTS->Fill(firstPoint.z(),chisq1);
	  hists->m_pnfptonpt_momTS->Fill(lmevmom,nfitntot);
	  hists->m_pnfptonpt_etaTS->Fill(eta,nfitntot);
	  // had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	  // for some strange reason... -CPL
	  Float_t phi_deg;
	  if (origin.phi() < 0)
	    phi_deg = 360+origin.phi()/degree;
	  else
	    phi_deg = origin.phi()/degree;
	  Float_t psi_deg = geom->psi()/degree;
	  hists->m_ppsi_phiTS->Fill(phi_deg,psi_deg);
	}

// now fill all FTPC East histograms ------------------------------------------
        else if (map.trackFtpcEast()) {

// these are TPC & FTPC
	  hists->m_ppointFE->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointFE->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointFE->Fill(fTraits.numberOfFitPoints());
	  hists->m_prim_chargeFE->Fill(geom->charge());
	  hists->m_prim_xfFE->Fill(firstPoint.x());
	  hists->m_prim_yfFE->Fill(firstPoint.y());
	  hists->m_prim_zfFE->Fill(firstPoint.z());
	  hists->m_prim_radfFE->Fill(radf);
	  hists->m_prim_ratioFE->Fill(nfitntot);
	  hists->m_prim_ratiomFE->Fill(nfitnmax);
	  hists->m_ppsiFE->Fill(geom->psi()/degree);
	  hists->m_petaFE->Fill(eta);
	  hists->m_ppTFE->Fill(pT);
	  hists->m_pmomFE->Fill(gmom);
	  hists->m_plengthFE->Fill(primtrk->length());
	  hists->m_pchisq0FE->Fill(chisq0);
	  hists->m_pchisq1FE->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFE->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFE->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthFE->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFE->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthFE->Fill(primtrk->length(),
				   Float_t(fTraits.numberOfFitPoints()));
	}

// now fill all FTPC West histograms ------------------------------------------
        else if (map.trackFtpcWest()) {

// these are TPC & FTPC
	  hists->m_ppointFW->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointFW->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointFW->Fill(fTraits.numberOfFitPoints());
	  hists->m_prim_chargeFW->Fill(geom->charge());
	  hists->m_prim_xfFW->Fill(firstPoint.x());
	  hists->m_prim_yfFW->Fill(firstPoint.y());
	  hists->m_prim_zfFW->Fill(firstPoint.z());
	  hists->m_prim_radfFW->Fill(radf);
	  hists->m_prim_ratioFW->Fill(nfitntot);
	  hists->m_prim_ratiomFW->Fill(nfitnmax);
	  hists->m_ppsiFW->Fill(geom->psi()/degree);
	  hists->m_petaFW->Fill(eta);
	  hists->m_ppTFW->Fill(pT);
	  hists->m_pmomFW->Fill(gmom);
	  hists->m_plengthFW->Fill(primtrk->length());
	  hists->m_pchisq0FW->Fill(chisq0);
	  hists->m_pchisq1FW->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFW->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFW->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthFW->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFW->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthFW->Fill(primtrk->length(),
				   Float_t(fTraits.numberOfFitPoints()));
	}
      }
    }
    hists->m_primtrk_good->Fill(cnttrkg);
    hists->m_primtrk_good_sm->Fill(cnttrkg);
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistGen() {

  //  StEvent does not have data corresponding to the DST particle table
  //  so this method is not used in StEventQAMaker.  However, this
  //  information can be found from StMcEvent.

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling particle histograms ");

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPID() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling dE/dx histograms ");

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t cntrows=0;
  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *theTrack = theNodes[i]->track(global);
    if (!theTrack) continue;
    cntrows++;
    StSPtrVecTrackPidTraits &trkPidTr = theTrack->pidTraits();
    StDedxPidTraits *dedxPidTr;

    for (unsigned int itrait=0; itrait<trkPidTr.size();itrait++) {
      dedxPidTr = 0;
      StTrackPidTraits *thisTrait = trkPidTr[itrait];
      dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);

      if (dedxPidTr && dedxPidTr->method() == kTruncatedMeanId) {
	int ndedx = dedxPidTr->numberOfPoints();
	double dedx = dedxPidTr->mean();
	double error = dedxPidTr->errorOnMean();
	double p = abs(theTrack->geometry()->momentum());
	if (dedxPidTr->detector() == kTpcId) {
	  hists->m_ndedxT->Fill(ndedx);
	  hists->m_dedx0T->Fill(dedx);
	  hists->m_dedx1T->Fill(error);
	  if (ndedx > 15) {
	    hists->m_p_dedx_rec->Fill((float)(p),(float)(dedx*1.e6));
	  }
	}
	if (dedxPidTr->detector() == kFtpcWestId) {
	  hists->m_ndedxFW->Fill(ndedx);
	  hists->m_dedx0FW->Fill(dedx);
	  hists->m_dedx1FW->Fill(error);
	}
	if (dedxPidTr->detector() == kFtpcEastId) {
	  hists->m_ndedxFE->Fill(ndedx);
	  hists->m_dedx0FE->Fill(dedx);
	  hists->m_dedx1FE->Fill(error);
	}
      }
    }
  }
  hists->m_ndedxr->Fill(cntrows);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistVertex() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling vertex histograms ");

  Float_t m_prmass2 = (proton_mass_c2*proton_mass_c2);
  Float_t m_pimass2 = (pion_minus_mass_c2*pion_minus_mass_c2);
  Float_t m_lamass2 = (lambda_mass_c2*lambda_mass_c2);

  // primary vertex
  StPrimaryVertex *primVtx = event->primaryVertex();
  UInt_t daughters=0;
  UInt_t currentNumber=0;

  if (primVtx) {
    for (UInt_t v=0; v<event->numberOfPrimaryVertices(); v++) {
      currentNumber = event->primaryVertex(v)->numberOfDaughters();
      if (currentNumber > daughters) {
	daughters = currentNumber;
	primVtx = event->primaryVertex(v);
      }
    }

    float z_svt = 999.;
    float z_tpc = -999.;
    for (UInt_t j=0; j<event->numberOfPrimaryVertices(); j++) {
      StPrimaryVertex *aPrimVtx = event->primaryVertex(j);

      if (aPrimVtx->flag() == 201) z_svt = aPrimVtx->position().z();
      if (aPrimVtx->flag() == 101) z_tpc = aPrimVtx->position().z();

      if (aPrimVtx == primVtx) {
        hists->m_pv_vtxid->Fill(primVtx->type());
	if (!isnan(double(primVtx->position().x())))
	  hists->m_pv_x->Fill(primVtx->position().x());
	if (!isnan(double(primVtx->position().y())))
	  hists->m_pv_y->Fill(primVtx->position().y());
	if (!isnan(double(primVtx->position().z())))
	  hists->m_pv_z->Fill(primVtx->position().z());
        hists->m_pv_pchi2->Fill(primVtx->chiSquared());
        hists->m_pv_r->Fill(primVtx->position().x()*primVtx->position().x() +
		     primVtx->position().y()*primVtx->position().y());
      }
      else {
        hists->m_v_vtxid->Fill(aPrimVtx->type());
	if (!isnan(double(aPrimVtx->position().x())))
	  hists->m_v_x->Fill(aPrimVtx->position().x());     
	if (!isnan(double(aPrimVtx->position().y())))
	  hists->m_v_y->Fill(aPrimVtx->position().y());     
	if (!isnan(double(aPrimVtx->position().z())))
	  hists->m_v_z->Fill(aPrimVtx->position().z());     
        hists->m_v_pchi2->Fill(aPrimVtx->chiSquared());
        hists->m_v_r->Fill(aPrimVtx->position().x()*aPrimVtx->position().x() +
		    aPrimVtx->position().y()*aPrimVtx->position().y());
      }
    }
    hists->m_vtx_z->Fill(z_tpc-z_svt);
  }

  // V0 vertices
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling dst_v0_vertex histograms ");

  StSPtrVecV0Vertex &v0Vtx = event->v0Vertices();
  hists->m_v0->Fill(v0Vtx.size());

//  static TH1F v0PhiHist("voph","v0 Phi Hist",36,0.,360.);
//  static TH1F v0PhiHist2("voph2","v0 Phi Hist2",36,180.,540.);
//  static TSpectrum v0PhiSpec;
//  v0PhiHist.Reset();
//  v0PhiHist2.Reset();

  for (UInt_t k=0; k<v0Vtx.size(); k++) {
    StV0Vertex *v0 = v0Vtx[k];
    if ((v0) && (v0->dcaParentToPrimaryVertex() >= 0.)) {
      Float_t e1a = pow(abs(v0->momentumOfDaughter(positive)),2);
      Float_t e2 = pow(abs(v0->momentumOfDaughter(negative)),2);
      Float_t e1 = e1a + m_prmass2;
      e2 += m_pimass2;
      e1 = TMath::Sqrt(e1);
      e2 = TMath::Sqrt(e2);
      Float_t p = pow(abs(v0->momentum()),2);
      Float_t inv_mass_la = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      e1 = e1a + m_pimass2;
      e1 = TMath::Sqrt(e1);
      Float_t inv_mass_k0 = TMath::Sqrt((e1+e2)*(e1+e2) - p);

      hists->m_ev0_lama_hist->Fill(inv_mass_la);
      hists->m_ev0_k0ma_hist->Fill(inv_mass_k0);

      hists->m_v_vtxid->Fill(v0->type());
      if (!isnan(double(v0->position().x())))
        hists->m_v_x->Fill(v0->position().x());     
      if (!isnan(double(v0->position().y())))
        hists->m_v_y->Fill(v0->position().y());     
      if (!isnan(double(v0->position().z())))
        hists->m_v_z->Fill(v0->position().z());     
      hists->m_v_pchi2->Fill(v0->chiSquared());
      hists->m_v_r->Fill(v0->position().x()*v0->position().x() +
		  v0->position().y()*v0->position().y());

      if (!(isnan(double(v0->position().x())) ||
            isnan(double(v0->position().y())))) {
        Float_t phi = atan2(v0->position().y() - primVtx->position().y(),
	                    v0->position().x() - primVtx->position().x())
                       * 180./M_PI;
        if (phi<0.) phi += 360.;
        hists->m_vtx_phi_dist->Fill(phi);
//        v0PhiHist.Fill(phi);
//        if (phi<180.) phi += 360.;
//        v0PhiHist2.Fill(phi);
      }
    }
  }

  /*
  if (v0PhiHist.GetEntries()>=144) {
    for (Float_t ssig=2.; ssig<100.; ssig+=2.) {
      Int_t npeaks = v0PhiSpec.Search(&v0PhiHist,
                   (Double_t) (ssig/v0PhiHist.GetBinWidth(0)));
      Float_t* pks = v0PhiSpec.GetPositionX();
      for (Int_t ipeak=0; ipeak<npeaks; ipeak++) {
        if ((pks[ipeak]>=90.)&&(pks[ipeak]<270.)) {
          hists->m_vtx_phi_dist->Fill(pks[ipeak], ssig);
        }
      }

      npeaks = v0PhiSpec.Search(&v0PhiHist2,
                   (Double_t) (ssig/v0PhiHist2.GetBinWidth(0)));
      pks = v0PhiSpec.GetPositionX();
      for (Int_t ipeak=0; ipeak<npeaks; ipeak++) {
        if ((pks[ipeak]>=270.)&&(pks[ipeak]<450.)) {
          Float_t phi2 = pks[ipeak];
          if (phi2>360.) phi2 -= 360.;
          hists->m_vtx_phi_dist->Fill(phi2, ssig);
	}
      }
    }
  }
  */

  // Xi vertices
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling dst_xi_vertex histograms ");

  StSPtrVecXiVertex &xiVtx = event->xiVertices();
  hists->m_xi_tot->Fill(xiVtx.size());

  for (UInt_t l=0; l<xiVtx.size(); l++) {
    StXiVertex *xi = xiVtx[l];
    if (xi) {
      const StThreeVectorF& pMom = xi->momentumOfBachelor();
      StThreeVectorF lMom = xi->momentumOfV0();
      StThreeVectorF xMom = lMom + pMom;
      Float_t pP2 = pMom.mag2();
      Float_t pL2 = lMom.mag2();
      Float_t pX2 = xMom.mag2();
      Float_t epi = sqrt(pP2 + m_pimass2);
      Float_t ela = sqrt(pL2 + m_lamass2);
      Float_t eXi = ela + epi;
      Float_t inv_mass_xi = sqrt(eXi*eXi - pX2);

      hists->m_xi_ma_hist->Fill(inv_mass_xi);

      hists->m_v_vtxid->Fill(xi->type());
      if (!isnan(double(xi->position().x())))
        hists->m_v_x->Fill(xi->position().x());     
      if (!isnan(double(xi->position().y())))
        hists->m_v_y->Fill(xi->position().y());     
      if (!isnan(double(xi->position().z())))
        hists->m_v_z->Fill(xi->position().z());     
      hists->m_v_pchi2->Fill(xi->chiSquared());
      hists->m_v_r->Fill(xi->position().x()*xi->position().x() +
		  xi->position().y()*xi->position().y());
    }
  }

  // Kink vertices
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling kink histograms ");

  StSPtrVecKinkVertex &kinkVtx = event->kinkVertices();
  hists->m_kink_tot->Fill(kinkVtx.size());

  for (UInt_t m=0; m<kinkVtx.size(); m++) {
    StKinkVertex *kink = kinkVtx[m];
    if (kink) {
      //hists->m_v_detid->Fill(kink->det_id); 
      hists->m_v_vtxid->Fill(kink->type());
      if (!isnan(double(kink->position().x())))
        hists->m_v_x->Fill(kink->position().x());     
      if (!isnan(double(kink->position().y())))
        hists->m_v_y->Fill(kink->position().y());     
      if (!isnan(double(kink->position().z())))
        hists->m_v_z->Fill(kink->position().z());     
      hists->m_v_pchi2->Fill(kink->chiSquared());
      hists->m_v_r->Fill(kink->position().x()*kink->position().x() +
		  kink->position().y()*kink->position().y());
    }
  }

  UInt_t cntrows = 0;
  cntrows = event->numberOfPrimaryVertices() + v0Vtx.size() +
            xiVtx.size() + kinkVtx.size(); //this gives 3 less than the DSTs!!
                                           //->needs to be fixed !!!
  hists->m_v_num->Fill(cntrows);
  hists->m_v_num_sm->Fill(cntrows);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPoint() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling point histograms ");

  StTpcHitCollection *tpcHits = event->tpcHitCollection();
  StSvtHitCollection *svtHits = event->svtHitCollection();
  StFtpcHitCollection *ftpcHits = event->ftpcHitCollection();
  StSsdHitCollection *ssdHits = event->ssdHitCollection();

  ULong_t totalHits = 0;
  ULong_t ftpcHitsE = 0;
  ULong_t ftpcHitsW = 0;

  if (tpcHits) {
    // z-dist of hits
    for (UInt_t i=0; i<24; i++)
      for (UInt_t j=0; j<45; j++)
	for (UInt_t k=0; k<tpcHits->sector(i)->padrow(j)->hits().size(); k++)
	  hists->m_z_hits->Fill(tpcHits->sector(i)->padrow(j)->hits()[k]->position().z());
    hists->m_pnt_tpc->Fill(tpcHits->numberOfHits());
    totalHits += tpcHits->numberOfHits();
  }
  if (svtHits) {
    hists->m_pnt_svt->Fill(svtHits->numberOfHits());
    totalHits += svtHits->numberOfHits();
  }
  if (ftpcHits) {
    // StFtpcHitCollection doesn't differentiate between W and E FTPCs
    // so it is up to the user to check this via plane number -CPL
    for (UInt_t i=0; i<20; i++) {
      if (i<10)
	ftpcHitsW += ftpcHits->plane(i)->numberOfHits();
      else
	ftpcHitsE += ftpcHits->plane(i)->numberOfHits();
    }
    hists->m_pnt_ftpcW->Fill(ftpcHitsW);
    hists->m_pnt_ftpcE->Fill(ftpcHitsE);
    totalHits += ftpcHits->numberOfHits();
  }
  if (ssdHits) {
    hists->m_pnt_ssd->Fill(ssdHits->numberOfHits());
    totalHits += ssdHits->numberOfHits();
  }
  hists->m_pnt_tot->Fill(totalHits);
  hists->m_pnt_tot_med->Fill(totalHits);
  hists->m_pnt_tot_sm->Fill(totalHits);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistRich() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling Rich histograms ");

  if (event->softwareMonitor()->rich())
    hists->m_rich_tot->Fill(event->softwareMonitor()->rich()->mult_rich_tot);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistEMC() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling EMC histograms ");

  StEmcCollection* emccol = event->emcCollection();
  cout <<"emccol = "<<emccol<<"\n";
  if (!emccol) return;

  //Get vertex
  StPrimaryVertex* pvert = event->primaryVertex(0);

  UInt_t i;

  for(i=0; i<4; i++){
    Int_t det = i+1;
    StDetectorId id = StEmcMath::detectorId(det);
    StEmcDetector* detector=emccol->detector(id);

    Float_t energy=0.0; // Energy for whole detector
    UInt_t  nh=0;         // Hits for whole detectors
    for(UInt_t j=1;j<121;j++){
      StEmcModule* module = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=module->hits();
        
      Int_t m,e,s,adc;
      Float_t eta,phi,E;
      nh += rawHit.size();
      for(UInt_t k=0;k<rawHit.size();k++){
        m   = rawHit[k]->module();
        e   = rawHit[k]->eta();
        s   = rawHit[k]->sub();
        if (s == -1) s = 1; // case of smde
        adc = rawHit[k]->adc();
        E   = rawHit[k]->energy();
        emcGeom[i]->getEta(m, e, eta); 
        emcGeom[i]->getPhi(m, s, phi);
        hists->m_emc_hits[i]->Fill(eta,phi); 
        hists->m_emc_energy2D[i]->Fill(eta,phi,E); 
        hists->m_emc_adc[i]->Fill(float(adc)); 
        hists->m_emc_energy[i]->Fill(E);
        energy += E;
       }
     }
     if(nh)     hists->m_emc_nhit->Fill(log10(Double_t(nh)), Float_t(det));
     if(energy) hists->m_emc_etot->Fill(log10(Double_t(energy)), Float_t(det));
  }
  
  for(i=0; i<4; i++) {  
    Int_t det = i+1, nh;
    StDetectorId id = StEmcMath::detectorId(det);
    StEmcDetector* detector = emccol->detector(id);
    StSPtrVecEmcCluster& cluster = detector->cluster()->clusters();

    hists->m_emc_ncl->Fill(log10(Double_t(cluster.size())),(Float_t)det);
    Float_t Etot=0.0, eta, phi, sigEta, sigPhi, eCl;
    for(UInt_t j=0;j<cluster.size();j++){
      nh     = cluster[j]->nHits();
      eCl    = cluster[j]->energy();
      eta    = cluster[j]->eta();
      sigEta = cluster[j]->sigmaEta();
      phi    = cluster[j]->phi();
      sigPhi = cluster[j]->sigmaPhi();
      if(sigEta > 0)   hists->m_emc_sig_e->Fill(sigEta, Axis_t(det));          
      if(sigPhi > 0.0) hists->m_emc_sig_p->Fill(sigPhi, Axis_t(det));

      hists->m_emc_cl[det-1]->Fill(Axis_t(eta), Axis_t(phi));
      hists->m_emc_energyCl[det-1]->Fill(Axis_t(eta), Axis_t(phi), eCl);
      hists->m_emc_HitsInCl[det-1]->Fill(Axis_t(nh));
      hists->m_emc_EnergyCl[det-1]->Fill(Axis_t(eCl));
      hists->m_emc_EtaInCl[det-1]->Fill(Axis_t(eta));
      hists->m_emc_PhiInCl[det-1]->Fill(Axis_t(phi));
      Etot  += eCl;
    }
    hists->m_emc_etotCl->Fill(log10(Etot), Axis_t(det));
  }      

  // Get the hists from StEmcPoints

  StSPtrVecEmcPoint& pointvec = emccol->barrelPoints();
 
  Int_t Point_Mult[4];
  for(i=0;i<4;i++) {Point_Mult[i]=0;}
  
  for(i=0;i<pointvec.size();i++) {

    StEmcPoint *point = (StEmcPoint*) pointvec[i];

    //const StThreeVectorF & posP = point->position();
    const StThreeVectorF & sizeP = point->size();

    Float_t eta=0.;
    Float_t phi=0.;
    if (pvert) {
      eta=StEmcMath::eta(point,(StMeasuredPoint*)pvert);
      phi=StEmcMath::phi(point,(StMeasuredPoint*)pvert);
    }

    // Get category (ncat) for this point
   
    Float_t EnergyDet[4];
    for(UInt_t ie=0;ie<4;ie++) {EnergyDet[ie]=0.0;}

    for(Int_t j=0;j<4;j++) {
      StDetectorId detid = static_cast<StDetectorId>(j+kBarrelEmcTowerId);
      EnergyDet[j] = point->energyInDetector(detid);
    }
    Int_t ncat=0;

    if(EnergyDet[2]==0 && EnergyDet[3] ==0) {
      ncat=0;
    } else if(EnergyDet[2]>0 && EnergyDet[3] ==0) {
      ncat=1;
    } else if(EnergyDet[2]==0 && EnergyDet[3]>0) {
      ncat=2;
    } else{
      ncat=3;
    }

    //Fill the hists
    Float_t energy=point->energy();
    Float_t sigmaeta=sizeP.x();
    Float_t sigmaphi=sizeP.y();
    Float_t trackmom=point->chiSquare();
    Float_t deltaeta=point->deltaEta();
    Float_t deltaphi=point->deltaPhi();
    if (ncat>3) ncat=3;
    Point_Mult[ncat]++;
    if (energy>0) hists->m_emc_point_energy[ncat]->Fill(energy);
    if (pvert) {
      hists->m_emc_point_eta[ncat]->Fill(eta);
      hists->m_emc_point_phi[ncat]->Fill(phi);
    }
    hists->m_emc_point_sigeta[ncat]->Fill(sigmaeta);
    hists->m_emc_point_sigphi[ncat]->Fill(sigmaphi);
    hists->m_emc_point_flag->Fill(Float_t(ncat+1));
    if (trackmom>0) {
      hists->m_emc_point_trmom[ncat]->Fill(trackmom);
      hists->m_emc_point_deleta[ncat]->Fill(deltaeta);
      hists->m_emc_point_delphi[ncat]->Fill(deltaphi);
    }
  }
  for(i=0;i<4;i++) {hists->m_emc_points[i]->Fill(Float_t(Point_Mult[i]));}

}

//_____________________________________________________________________________

void StEventQAMaker::MakeHistEval() {

  // requires StMcEvent
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling Eval histograms ");

}

//_____________________________________________________________________________
