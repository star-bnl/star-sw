// $Id: StEventQAMaker.cxx,v 2.0 2000/08/25 16:02:39 genevb Exp $
// $Log: StEventQAMaker.cxx,v $
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
#include "TMath.h"
#include "SystemOfUnits.h"
#include "StQABookHist.h"
#include "TH1.h"
#include "TH2.h"
#include "StEventQAMaker.h"
#include "StEventTypes.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StDbUtilities/StCoordinates.hh"
#include "HitHistograms.h"
#include "StTpcDb/StTpcDb.h"
#include "StarClassLibrary/StTimer.hh"
#include "StMessMgr.h"

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
      cnttrkg++;
      Float_t pT = -999.;
      pT = globtrk->geometry()->momentum().perp();
      Float_t lmevpt = TMath::Log10(pT*1000.0);
      Float_t theta = TMath::ASin(1.) - globtrk->geometry()->dipAngle();
      Float_t thetad = theta/degree;
      Float_t eta = globtrk->geometry()->momentum().pseudoRapidity();
      Float_t gmom = abs(globtrk->geometry()->momentum());
      Float_t lmevmom = TMath::Log10(gmom*1000.0);
      Float_t chisq0 = globtrk->fitTraits().chi2(0);
      Float_t chisq1 = globtrk->fitTraits().chi2(1);
      Float_t nfitntot = (Float_t(globtrk->fitTraits().numberOfFitPoints())) /
	                 (Float_t(globtrk->detectorInfo()->numberOfPoints()));
      Float_t nfitnmax = (Float_t(globtrk->fitTraits().numberOfFitPoints())) /
                         (Float_t(globtrk->numberOfPossiblePoints()));
      StThreeVectorF dif = globtrk->detectorInfo()->firstPoint() -
	                   globtrk->geometry()->origin();
      Float_t radf = globtrk->detectorInfo()->firstPoint().perp();

      Float_t logImpact = TMath::Log10(globtrk->impactParameter());
      Float_t logCurvature = TMath::Log10(globtrk->geometry()->curvature());

      // pathLength(double x,double y) should return path length at
      // DCA in the xy-plane to a given point
      double S = globtrk->geometry()->helix().pathLength(0,0);
      StThreeVectorD dcaToBeam = globtrk->geometry()->helix().at(S);
      // these histogram additions are for Lanny's evr QA histograms
      hists->m_dcaToBeamXY->Fill(dcaToBeam.x(),dcaToBeam.y());
      hists->m_dcaToBeamZ1->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ2->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ3->Fill(dcaToBeam.z());
      hists->m_zDcaTanl->Fill(dcaToBeam.z(),TMath::Tan(globtrk->geometry()->dipAngle()));
      hists->m_zDcaZf->Fill(dcaToBeam.z(),globtrk->detectorInfo()->firstPoint().z());
      hists->m_zDcaPsi->Fill(dcaToBeam.z(),globtrk->geometry()->psi()/degree);
      if (globtrk->geometry()->origin().phi() < 0)
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),360+globtrk->geometry()->origin().phi()/degree);
      else
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),globtrk->geometry()->origin().phi()/degree);

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 

      // check if the track has hits in a detector -CPL
      if (globtrk->topologyMap().numberOfHits(kUnknownId)>0) hists->m_det_id->Fill(kUnknownId);
      if (globtrk->topologyMap().numberOfHits(kTpcId)>0) hists->m_det_id->Fill(kTpcId);
      if (globtrk->topologyMap().numberOfHits(kSvtId)>0) hists->m_det_id->Fill(kSvtId);
      if (globtrk->topologyMap().numberOfHits(kRichId)>0) hists->m_det_id->Fill(kRichId);
      if (globtrk->topologyMap().numberOfHits(kFtpcWestId)>0) hists->m_det_id->Fill(kFtpcWestId);
      if (globtrk->topologyMap().numberOfHits(kFtpcEastId)>0) hists->m_det_id->Fill(kFtpcEastId);
      if (globtrk->topologyMap().numberOfHits(kTofPatchId)>0) hists->m_det_id->Fill(kTofPatchId);
      if (globtrk->topologyMap().numberOfHits(kCtbId)>0) hists->m_det_id->Fill(kCtbId);
      if (globtrk->topologyMap().numberOfHits(kSsdId)>0) hists->m_det_id->Fill(kSsdId);
      if (globtrk->topologyMap().numberOfHits(kBarrelEmcTowerId)>0) hists->m_det_id->Fill(kBarrelEmcTowerId);
      if (globtrk->topologyMap().numberOfHits(kBarrelEmcPreShowerId)>0) hists->m_det_id->Fill(kBarrelEmcPreShowerId);
      if (globtrk->topologyMap().numberOfHits(kBarrelSmdEtaStripId)>0) hists->m_det_id->Fill(kBarrelSmdEtaStripId);
      if (globtrk->topologyMap().numberOfHits(kBarrelSmdPhiStripId)>0) hists->m_det_id->Fill(kBarrelSmdPhiStripId);
      if (globtrk->topologyMap().numberOfHits(kEndcapEmcTowerId)>0) hists->m_det_id->Fill(kEndcapEmcTowerId);
      if (globtrk->topologyMap().numberOfHits(kEndcapEmcPreShowerId)>0) hists->m_det_id->Fill(kEndcapEmcPreShowerId);
      if (globtrk->topologyMap().numberOfHits(kEndcapSmdUStripId)>0) hists->m_det_id->Fill(kEndcapSmdUStripId);
      if (globtrk->topologyMap().numberOfHits(kEndcapSmdVStripId)>0) hists->m_det_id->Fill(kEndcapSmdVStripId);
      if (globtrk->topologyMap().numberOfHits(kZdcWestId)>0) hists->m_det_id->Fill(kZdcWestId);
      if (globtrk->topologyMap().numberOfHits(kZdcEastId)>0) hists->m_det_id->Fill(kZdcEastId);
      if (globtrk->topologyMap().numberOfHits(kMwpcWestId)>0) hists->m_det_id->Fill(kMwpcWestId);
      if (globtrk->topologyMap().numberOfHits(kMwpcEastId)>0) hists->m_det_id->Fill(kMwpcEastId);
      if (globtrk->topologyMap().numberOfHits(kTpcSsdId)>0) hists->m_det_id->Fill(kTpcSsdId);
      if (globtrk->topologyMap().numberOfHits(kTpcSvtId)>0) hists->m_det_id->Fill(kTpcSvtId);
      if (globtrk->topologyMap().numberOfHits(kTpcSsdSvtId)>0) hists->m_det_id->Fill(kTpcSsdSvtId);
      if (globtrk->topologyMap().numberOfHits(kSsdSvtId)>0) hists->m_det_id->Fill(kSsdSvtId);

      // calculate the probability of a fit being correct
      // number of degrees of freedom = fitpoints-5 (5 params constrain track)
      Double_t ndf = 2*globtrk->fitTraits().numberOfFitPoints()-5;
      Double_t probability = TMath::Prob(chisq0*ndf,ndf);
      hists->m_globtrk_fit_prob->Fill(probability);

// now fill all TPC histograms ------------------------------------------------
      if (globtrk->flag()>=100 && globtrk->flag()<200) {

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
	StGlobalCoordinate globalHitPosition(globtrk->detectorInfo()->firstPoint());
	StTpcPadCoordinate padCoord;
	transformer(globalHitPosition,padCoord);
        hists->m_glb_padfT->Fill(padCoord.row());

// these are TPC & FTPC
        hists->m_pointT->Fill(globtrk->detectorInfo()->numberOfPoints());
        hists->m_max_pointT->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointT->Fill(globtrk->fitTraits().numberOfFitPoints());
        hists->m_glb_chargeT->Fill(globtrk->geometry()->charge());

        hists->m_glb_r0T->Fill(globtrk->geometry()->origin().perp());
	if (globtrk->geometry()->origin().phi() < 0)
	  hists->m_glb_phi0T->Fill(360+globtrk->geometry()->origin().phi()/degree);
	else
	  hists->m_glb_phi0T->Fill(globtrk->geometry()->origin().phi()/degree);
        hists->m_glb_z0T->Fill(globtrk->geometry()->origin().z());
        hists->m_glb_curvT->Fill(logCurvature);

        hists->m_glb_rfT->Fill(globtrk->detectorInfo()->firstPoint().perp());
        hists->m_glb_xfT->Fill(globtrk->detectorInfo()->firstPoint().x());
        hists->m_glb_yfT->Fill(globtrk->detectorInfo()->firstPoint().y());
        hists->m_glb_zfT->Fill(globtrk->detectorInfo()->firstPoint().z());
        hists->m_glb_radfT->Fill(radf);
        hists->m_glb_ratioT->Fill(nfitntot);
        hists->m_glb_ratiomT->Fill(nfitnmax);
        hists->m_psiT->Fill(globtrk->geometry()->psi()/degree);
        hists->m_tanlT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()));
        hists->m_glb_thetaT->Fill(thetad);
        hists->m_etaT->Fill(eta);
        hists->m_pTT->Fill(pT);
        hists->m_momT->Fill(gmom);
        hists->m_lengthT->Fill(globtrk->length());
        hists->m_chisq0T->Fill(chisq0);
        hists->m_chisq1T->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_globtrk_xf_yfT->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
        hists->m_eta_trklengthT->Fill(eta,globtrk->length());
        hists->m_npoint_lengthT->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
        hists->m_fpoint_lengthT->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

// these are TPC only
        hists->m_pT_eta_recT->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  hists->m_tanl_zfT->Fill(globtrk->detectorInfo()->firstPoint().z() -
			   event->primaryVertex()->position().z(),
			   Float_t(TMath::Tan(globtrk->geometry()->dipAngle())));
	}
        hists->m_mom_trklengthT->Fill(globtrk->length(),lmevmom);
        hists->m_chisq0_momT->Fill(lmevmom,chisq0);
        hists->m_chisq1_momT->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaT->Fill(eta,chisq0);
        hists->m_chisq1_etaT->Fill(eta,chisq1);
        hists->m_chisq0_dipT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq0);
        hists->m_chisq1_dipT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq1);
        hists->m_chisq0_zfT->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq0);
        hists->m_chisq1_zfT->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq1);
	if (globtrk->geometry()->origin().phi() < 0)
	  hists->m_chisq0_phiT->Fill(360+globtrk->geometry()->origin().phi()/degree,chisq0);
	else
	  hists->m_chisq0_phiT->Fill(globtrk->geometry()->origin().phi()/degree,chisq0);
        hists->m_nfptonpt_momT->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaT->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CPL
	Float_t phi_deg;
	if (globtrk->geometry()->origin().phi() < 0)
	  phi_deg = 360+globtrk->geometry()->origin().phi()/degree;
	else
	  phi_deg = globtrk->geometry()->origin().phi()/degree;
	Float_t psi_deg = globtrk->geometry()->psi()/degree;
        hists->m_psi_phiT->Fill(phi_deg,psi_deg);
      }

// now fill all TPC+SVT histograms --------------------------------------------

      if (globtrk->flag()>=500 && globtrk->flag()<600 ) {

        hists->m_glb_f0TS->Fill(dif.x(),0.);
        hists->m_glb_f0TS->Fill(dif.y(),1.);
        hists->m_glb_f0TS->Fill(dif.z(),2.);

        hists->m_glb_xf0TS->Fill(dif.x());
        hists->m_glb_yf0TS->Fill(dif.y());
        hists->m_glb_zf0TS->Fill(dif.z());
        hists->m_glb_impactTS->Fill(logImpact);
        hists->m_glb_impactrTS->Fill(globtrk->impactParameter());

        hists->m_pointTS->Fill(globtrk->detectorInfo()->numberOfPoints());
        hists->m_max_pointTS->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointTS->Fill(globtrk->fitTraits().numberOfFitPoints());
        hists->m_glb_chargeTS->Fill(globtrk->geometry()->charge());

        hists->m_glb_r0TS->Fill(globtrk->geometry()->origin().perp());
	if (globtrk->geometry()->origin().phi() < 0)
	  hists->m_glb_phi0TS->Fill(360+globtrk->geometry()->origin().phi()/degree);
	else
	  hists->m_glb_phi0TS->Fill(globtrk->geometry()->origin().phi()/degree);
        hists->m_glb_z0TS->Fill(globtrk->geometry()->origin().z());
        hists->m_glb_curvTS->Fill(logCurvature);

        hists->m_glb_rfTS->Fill(globtrk->detectorInfo()->firstPoint().perp());
        hists->m_glb_xfTS->Fill(globtrk->detectorInfo()->firstPoint().x());
        hists->m_glb_yfTS->Fill(globtrk->detectorInfo()->firstPoint().y());
        hists->m_glb_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z());
        hists->m_glb_radfTS->Fill(radf);
        hists->m_glb_ratioTS->Fill(nfitntot);
        hists->m_glb_ratiomTS->Fill(nfitnmax);
        hists->m_psiTS->Fill(globtrk->geometry()->psi()/degree);
        hists->m_tanlTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()));
        hists->m_glb_thetaTS->Fill(thetad);
        hists->m_etaTS->Fill(eta);
        hists->m_pTTS->Fill(pT);
        hists->m_momTS->Fill(gmom);
        hists->m_lengthTS->Fill(globtrk->length());
        hists->m_chisq0TS->Fill(chisq0);
        hists->m_chisq1TS->Fill(chisq1);
        hists->m_globtrk_xf_yfTS->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
        hists->m_eta_trklengthTS->Fill(eta,globtrk->length());
        hists->m_npoint_lengthTS->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
        hists->m_fpoint_lengthTS->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

        hists->m_pT_eta_recTS->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  hists->m_tanl_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z() -
			    event->primaryVertex()->position().z(),
			    Float_t(TMath::Tan(globtrk->geometry()->dipAngle())));
	}
        hists->m_mom_trklengthTS->Fill(globtrk->length(),lmevmom);
        hists->m_chisq0_momTS->Fill(lmevmom,chisq0);
        hists->m_chisq1_momTS->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaTS->Fill(eta,chisq0);
        hists->m_chisq1_etaTS->Fill(eta,chisq1);
        hists->m_chisq0_dipTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq0);
        hists->m_chisq1_dipTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq1);
        hists->m_chisq0_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq0);
        hists->m_chisq1_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq1);
	if (globtrk->geometry()->origin().phi() < 0)
	  hists->m_chisq0_phiTS->Fill(360+globtrk->geometry()->origin().phi()/degree,chisq0);
	else
	  hists->m_chisq0_phiTS->Fill(globtrk->geometry()->origin().phi()/degree,chisq0);

        hists->m_nfptonpt_momTS->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaTS->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CPL
	Float_t phi_deg;
	if (globtrk->geometry()->origin().phi() < 0)
	  phi_deg = 360+globtrk->geometry()->origin().phi()/degree;
	else
	  phi_deg = globtrk->geometry()->origin().phi()/degree;
	Float_t psi_deg = globtrk->geometry()->psi()/degree;
        hists->m_psi_phiTS->Fill(phi_deg,psi_deg);
      }

// now fill all FTPC East histograms ------------------------------------------
      if (globtrk->flag()>=700 && globtrk->flag()<800 && globtrk->topologyMap().numberOfHits(kFtpcEastId)>0) {

// these are TPC & FTPC
        hists->m_pointFE->Fill(globtrk->detectorInfo()->numberOfPoints());
        hists->m_max_pointFE->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointFE->Fill(globtrk->fitTraits().numberOfFitPoints());
        hists->m_glb_chargeFE->Fill(globtrk->geometry()->charge());
        hists->m_glb_rfFE->Fill(globtrk->detectorInfo()->firstPoint().perp());
        hists->m_glb_xfFE->Fill(globtrk->detectorInfo()->firstPoint().x());
        hists->m_glb_yfFE->Fill(globtrk->detectorInfo()->firstPoint().y());
        hists->m_glb_zfFE->Fill(globtrk->detectorInfo()->firstPoint().z());
        hists->m_glb_radfFE->Fill(radf);
        hists->m_glb_ratioFE->Fill(nfitntot);
        hists->m_glb_ratiomFE->Fill(nfitnmax);
        hists->m_psiFE->Fill(globtrk->geometry()->psi()/degree);
        hists->m_etaFE->Fill(eta);
        hists->m_pTFE->Fill(pT);
        hists->m_momFE->Fill(gmom);
        hists->m_lengthFE->Fill(globtrk->length());
        hists->m_chisq0FE->Fill(chisq0);
        hists->m_chisq1FE->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_pT_eta_recFE->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFE->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
        hists->m_eta_trklengthFE->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFE->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
        hists->m_fpoint_lengthFE->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

      }
// now fill all FTPC West histograms ------------------------------------------
      if (globtrk->flag()>=700 && globtrk->flag()<800 && globtrk->topologyMap().numberOfHits(kFtpcWestId)>0) {

// these are TPC & FTPC
        hists->m_pointFW->Fill(globtrk->detectorInfo()->numberOfPoints());
        hists->m_max_pointFW->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointFW->Fill(globtrk->fitTraits().numberOfFitPoints());
        hists->m_glb_chargeFW->Fill(globtrk->geometry()->charge());
        hists->m_glb_rfFW->Fill(globtrk->detectorInfo()->firstPoint().perp());
        hists->m_glb_xfFW->Fill(globtrk->detectorInfo()->firstPoint().x());
        hists->m_glb_yfFW->Fill(globtrk->detectorInfo()->firstPoint().y());
        hists->m_glb_zfFW->Fill(globtrk->detectorInfo()->firstPoint().z());
        hists->m_glb_radfFW->Fill(radf);
        hists->m_glb_ratioFW->Fill(nfitntot);
        hists->m_glb_ratiomFW->Fill(nfitnmax);
        hists->m_psiFW->Fill(globtrk->geometry()->psi()/degree);
        hists->m_etaFW->Fill(eta);
        hists->m_pTFW->Fill(pT);
        hists->m_momFW->Fill(gmom);
        hists->m_lengthFW->Fill(globtrk->length());
        hists->m_chisq0FW->Fill(chisq0);
        hists->m_chisq1FW->Fill(chisq1);

// these are for TPC & FTPC
        hists->m_pT_eta_recFW->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFW->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
        hists->m_eta_trklengthFW->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFW->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
        hists->m_fpoint_lengthFW->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

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
        cnttrkg++;
	Float_t pT = -999.;
	pT = primtrk->geometry()->momentum().perp();
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - primtrk->geometry()->dipAngle();
	Float_t thetad = theta/degree;
	Float_t eta   = primtrk->geometry()->momentum().pseudoRapidity();
	Float_t gmom = abs(primtrk->geometry()->momentum());
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = primtrk->fitTraits().chi2(0);
	Float_t chisq1 = primtrk->fitTraits().chi2(1);
	Float_t nfitnmax = (Float_t(primtrk->fitTraits().numberOfFitPoints())) /
	                   (Float_t(primtrk->numberOfPossiblePoints()));
        Float_t nfitntot = (Float_t(primtrk->fitTraits().numberOfFitPoints()))/
	                   (Float_t(primtrk->detectorInfo()->numberOfPoints()));

	Float_t logImpact = TMath::Log10(primtrk->impactParameter());
	Float_t logCurvature = TMath::Log10(primtrk->geometry()->curvature());


	// need to find position on helix closest to first point on track since
	// the primary vertex is used as the first point on helix for primary
	// tracks -CPL
	double s = primtrk->geometry()->helix().
	           pathLength(primtrk->detectorInfo()->firstPoint());
	StThreeVectorF dif = primtrk->detectorInfo()->firstPoint() -
	                     primtrk->geometry()->helix().at(s);
        Float_t radf = primtrk->detectorInfo()->firstPoint().perp();

	// check if the track has hits in a detector -CPL
	if (primtrk->topologyMap().numberOfHits(kUnknownId)>0) hists->m_pdet_id->Fill(kUnknownId);
	if (primtrk->topologyMap().numberOfHits(kTpcId)>0) hists->m_pdet_id->Fill(kTpcId);
	if (primtrk->topologyMap().numberOfHits(kSvtId)>0) hists->m_pdet_id->Fill(kSvtId);
	if (primtrk->topologyMap().numberOfHits(kRichId)>0) hists->m_pdet_id->Fill(kRichId);
	if (primtrk->topologyMap().numberOfHits(kFtpcWestId)>0) hists->m_pdet_id->Fill(kFtpcWestId);
	if (primtrk->topologyMap().numberOfHits(kFtpcEastId)>0) hists->m_pdet_id->Fill(kFtpcEastId);
	if (primtrk->topologyMap().numberOfHits(kTofPatchId)>0) hists->m_pdet_id->Fill(kTofPatchId);
	if (primtrk->topologyMap().numberOfHits(kCtbId)>0) hists->m_pdet_id->Fill(kCtbId);
	if (primtrk->topologyMap().numberOfHits(kSsdId)>0) hists->m_pdet_id->Fill(kSsdId);
	if (primtrk->topologyMap().numberOfHits(kBarrelEmcTowerId)>0) hists->m_pdet_id->Fill(kBarrelEmcTowerId);
	if (primtrk->topologyMap().numberOfHits(kBarrelEmcPreShowerId)>0) hists->m_pdet_id->Fill(kBarrelEmcPreShowerId);
	if (primtrk->topologyMap().numberOfHits(kBarrelSmdEtaStripId)>0) hists->m_pdet_id->Fill(kBarrelSmdEtaStripId);
	if (primtrk->topologyMap().numberOfHits(kBarrelSmdPhiStripId)>0) hists->m_pdet_id->Fill(kBarrelSmdPhiStripId);
	if (primtrk->topologyMap().numberOfHits(kEndcapEmcTowerId)>0) hists->m_pdet_id->Fill(kEndcapEmcTowerId);
	if (primtrk->topologyMap().numberOfHits(kEndcapEmcPreShowerId)>0) hists->m_pdet_id->Fill(kEndcapEmcPreShowerId);
	if (primtrk->topologyMap().numberOfHits(kEndcapSmdUStripId)>0) hists->m_pdet_id->Fill(kEndcapSmdUStripId);
	if (primtrk->topologyMap().numberOfHits(kEndcapSmdVStripId)>0) hists->m_pdet_id->Fill(kEndcapSmdVStripId);
	if (primtrk->topologyMap().numberOfHits(kZdcWestId)>0) hists->m_pdet_id->Fill(kZdcWestId);
	if (primtrk->topologyMap().numberOfHits(kZdcEastId)>0) hists->m_pdet_id->Fill(kZdcEastId);
	if (primtrk->topologyMap().numberOfHits(kMwpcWestId)>0) hists->m_pdet_id->Fill(kMwpcWestId);
	if (primtrk->topologyMap().numberOfHits(kMwpcEastId)>0) hists->m_pdet_id->Fill(kMwpcEastId);
	if (primtrk->topologyMap().numberOfHits(kTpcSsdId)>0) hists->m_pdet_id->Fill(kTpcSsdId);
	if (primtrk->topologyMap().numberOfHits(kTpcSvtId)>0) hists->m_pdet_id->Fill(kTpcSvtId);
	if (primtrk->topologyMap().numberOfHits(kTpcSsdSvtId)>0) hists->m_pdet_id->Fill(kTpcSsdSvtId);
	if (primtrk->topologyMap().numberOfHits(kSsdSvtId)>0) hists->m_pdet_id->Fill(kSsdSvtId);

// now fill all TPC histograms ------------------------------------------------
	if (primtrk->flag()>=300 && primtrk->flag()<400) {

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
	  hists->m_ppointT->Fill(primtrk->detectorInfo()->numberOfPoints());
	  hists->m_pmax_pointT->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointT->Fill(primtrk->fitTraits().numberOfFitPoints());
	  hists->m_prim_chargeT->Fill(primtrk->geometry()->charge());

	  hists->m_prim_r0T->Fill(primtrk->geometry()->origin().perp());
	  if (primtrk->geometry()->origin().phi() < 0)
	    hists->m_prim_phi0T->Fill(360+primtrk->geometry()->origin().phi()/degree);
	  else
	    hists->m_prim_phi0T->Fill(primtrk->geometry()->origin().phi()/degree);
	  hists->m_prim_z0T->Fill(primtrk->geometry()->origin().z());
	  hists->m_prim_curvT->Fill(logCurvature);

	  hists->m_prim_xfT->Fill(primtrk->detectorInfo()->firstPoint().x());
	  hists->m_prim_yfT->Fill(primtrk->detectorInfo()->firstPoint().y());
	  hists->m_prim_zfT->Fill(primtrk->detectorInfo()->firstPoint().z());
	  hists->m_prim_radfT->Fill(radf);
	  hists->m_prim_ratioT->Fill(nfitntot);
	  hists->m_prim_ratiomT->Fill(nfitnmax);
	  hists->m_ppsiT->Fill(primtrk->geometry()->psi()/degree);
	  hists->m_ptanlT->Fill(TMath::Tan(primtrk->geometry()->dipAngle()));
	  hists->m_prim_thetaT->Fill(thetad);
	  hists->m_petaT->Fill(eta);
	  hists->m_ppTT->Fill(pT);
	  hists->m_pmomT->Fill(gmom);
	  hists->m_plengthT->Fill(primtrk->length());
	  hists->m_pchisq0T->Fill(chisq0);
	  hists->m_pchisq1T->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_primtrk_xf_yfT->Fill(primtrk->detectorInfo()->firstPoint().x(),
				 primtrk->detectorInfo()->firstPoint().y());
	  hists->m_peta_trklengthT->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthT->Fill(primtrk->length(),
				  Float_t(primtrk->detectorInfo()->numberOfPoints()));
	  hists->m_pfpoint_lengthT->Fill(primtrk->length(),
				  Float_t(primtrk->fitTraits().numberOfFitPoints()));

// these are TPC only
	  hists->m_ppT_eta_recT->Fill(eta,lmevpt);
	  hists->m_ptanl_zfT->Fill(primtrk->detectorInfo()->firstPoint().z() -
			    event->primaryVertex()->position().z(),
			    Float_t(TMath::Tan(primtrk->geometry()->dipAngle())));
	  hists->m_pmom_trklengthT->Fill(primtrk->length(),lmevmom);
	  hists->m_pchisq0_momT->Fill(lmevmom,chisq0);
	  hists->m_pchisq1_momT->Fill(lmevmom,chisq1);
	  hists->m_pchisq0_etaT->Fill(eta,chisq0);
	  hists->m_pchisq1_etaT->Fill(eta,chisq1);
	  hists->m_pchisq0_dipT->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq0);
	  hists->m_pchisq1_dipT->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq1);
	  hists->m_pchisq0_zfT->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq0);
	  hists->m_pchisq1_zfT->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq1);
	  hists->m_pnfptonpt_momT->Fill(lmevmom,nfitntot);
	  hists->m_pnfptonpt_etaT->Fill(eta,nfitntot);
	  // had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	  // for some strange reason... -CPL
	  Float_t phi_deg;
	  if (primtrk->geometry()->origin().phi() < 0)
	    phi_deg = 360+primtrk->geometry()->origin().phi()/degree;
	  else
	    phi_deg = primtrk->geometry()->origin().phi()/degree;
	  Float_t psi_deg = primtrk->geometry()->psi()/degree;
	  hists->m_ppsi_phiT->Fill(phi_deg,psi_deg);
	}

// now fill all TPC+SVT histograms --------------------------------------------

	if (primtrk->flag()>=600 && primtrk->flag()<700) {

	  hists->m_prim_f0TS->Fill(dif.x(),0.);
	  hists->m_prim_f0TS->Fill(dif.y(),1.);
	  hists->m_prim_f0TS->Fill(dif.z(),2.);

	  hists->m_prim_xf0TS->Fill(dif.x());
	  hists->m_prim_yf0TS->Fill(dif.y());
	  hists->m_prim_zf0TS->Fill(dif.z());
	  hists->m_prim_impactTS->Fill(logImpact);
	  hists->m_prim_impactrTS->Fill(primtrk->impactParameter());

	  hists->m_ppointTS->Fill(primtrk->detectorInfo()->numberOfPoints());
	  hists->m_pmax_pointTS->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointTS->Fill(primtrk->fitTraits().numberOfFitPoints());
	  hists->m_prim_chargeTS->Fill(primtrk->geometry()->charge());

	  hists->m_prim_r0TS->Fill(primtrk->geometry()->origin().perp());
	  if (primtrk->geometry()->origin().phi() < 0)
	    hists->m_prim_phi0TS->Fill(360+primtrk->geometry()->origin().phi()/degree);
	  else
	    hists->m_prim_phi0TS->Fill(primtrk->geometry()->origin().phi()/degree);
	  hists->m_prim_z0TS->Fill(primtrk->geometry()->origin().z());
	  hists->m_prim_curvTS->Fill(logCurvature);

	  hists->m_prim_xfTS->Fill(primtrk->detectorInfo()->firstPoint().x());
	  hists->m_prim_yfTS->Fill(primtrk->detectorInfo()->firstPoint().y());
	  hists->m_prim_zfTS->Fill(primtrk->detectorInfo()->firstPoint().z());
	  hists->m_prim_radfTS->Fill(radf);
	  hists->m_prim_ratioTS->Fill(nfitntot);
	  hists->m_prim_ratiomTS->Fill(nfitnmax);
	  hists->m_ppsiTS->Fill(primtrk->geometry()->psi()/degree);
	  hists->m_ptanlTS->Fill(TMath::Tan(primtrk->geometry()->dipAngle()));
	  hists->m_prim_thetaTS->Fill(thetad);
	  hists->m_petaTS->Fill(eta);
	  hists->m_ppTTS->Fill(pT);
	  hists->m_pmomTS->Fill(gmom);
	  hists->m_plengthTS->Fill(primtrk->length());
	  hists->m_pchisq0TS->Fill(chisq0);
	  hists->m_pchisq1TS->Fill(chisq1);

	  hists->m_primtrk_xf_yfTS->Fill(primtrk->detectorInfo()->firstPoint().x(),
				  primtrk->detectorInfo()->firstPoint().y());
	  hists->m_peta_trklengthTS->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthTS->Fill(primtrk->length(),
				   Float_t(primtrk->detectorInfo()->numberOfPoints()));
	  hists->m_pfpoint_lengthTS->Fill(primtrk->length(),
				   Float_t(primtrk->fitTraits().numberOfFitPoints()));

	  hists->m_ppT_eta_recTS->Fill(eta,lmevpt);
	  hists->m_ptanl_zfTS->Fill(primtrk->detectorInfo()->firstPoint().z() -
			     event->primaryVertex()->position().z(),
			     Float_t(TMath::Tan(primtrk->geometry()->dipAngle())));
	  hists->m_pmom_trklengthTS->Fill(primtrk->length(),lmevmom);
	  hists->m_pchisq0_momTS->Fill(lmevmom,chisq0);
	  hists->m_pchisq1_momTS->Fill(lmevmom,chisq1);
	  hists->m_pchisq0_etaTS->Fill(eta,chisq0);
	  hists->m_pchisq1_etaTS->Fill(eta,chisq1);
	  hists->m_pchisq0_dipTS->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq0);
	  hists->m_pchisq1_dipTS->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq1);
	  hists->m_pchisq0_zfTS->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq0);
	  hists->m_pchisq1_zfTS->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq1);
	  hists->m_pnfptonpt_momTS->Fill(lmevmom,nfitntot);
	  hists->m_pnfptonpt_etaTS->Fill(eta,nfitntot);
	  // had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	  // for some strange reason... -CPL
	  Float_t phi_deg;
	  if (primtrk->geometry()->origin().phi() < 0)
	    phi_deg = 360+primtrk->geometry()->origin().phi()/degree;
	  else
	    phi_deg = primtrk->geometry()->origin().phi()/degree;
	  Float_t psi_deg = primtrk->geometry()->psi()/degree;
	  hists->m_ppsi_phiTS->Fill(phi_deg,psi_deg);
	}

/* The following are for the FTPC, which doesn't do primary tracking yet.
// now fill all FTPC East histograms ------------------------------------------
	if (primtrk->flag()>=700 && primtrk->flag()<800 && primtrk->topologyMap().numberOfHits(kFtpcEastId)>0) {

// these are TPC & FTPC
	  hists->m_ppointFE->Fill(primtrk->detectorInfo()->numberOfPoints());
	  hists->m_pmax_pointFE->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointFE->Fill(primtrk->fitTraits().numberOfFitPoints());
	  hists->m_prim_chargeFE->Fill(primtrk->geometry()->charge());
	  hists->m_prim_xfFE->Fill(primtrk->detectorInfo()->firstPoint().x());
	  hists->m_prim_yfFE->Fill(primtrk->detectorInfo()->firstPoint().y());
	  hists->m_prim_zfFE->Fill(primtrk->detectorInfo()->firstPoint().z());
	  hists->m_prim_radfFE->Fill(radf);
	  hists->m_prim_ratioFE->Fill(nfitntot);
	  hists->m_prim_ratiomFE->Fill(nfitnmax);
	  hists->m_ppsiFE->Fill(primtrk->geometry()->psi()/degree);
	  hists->m_petaFE->Fill(eta);
	  hists->m_ppTFE->Fill(pT);
	  hists->m_pmomFE->Fill(gmom);
	  hists->m_plengthFE->Fill(primtrk->length());
	  hists->m_pchisq0FE->Fill(chisq0);
	  hists->m_pchisq1FE->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFE->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFE->Fill(primtrk->detectorInfo()->firstPoint().x(),
				  primtrk->detectorInfo()->firstPoint().y());
	  hists->m_peta_trklengthFE->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFE->Fill(primtrk->length(),
				   Float_t(primtrk->detectorInfo()->numberOfPoints()));
	  hists->m_pfpoint_lengthFE->Fill(primtrk->length(),
				   Float_t(primtrk->fitTraits().numberOfFitPoints()));
	}

// now fill all FTPC West histograms ------------------------------------------
	if (primtrk->flag()>=700 && primtrk->flag()<800 && primtrk->topologyMap().numberOfHits(kFtpcWestId)>0) {

// these are TPC & FTPC
	  hists->m_ppointFW->Fill(primtrk->detectorInfo()->numberOfPoints());
	  hists->m_pmax_pointFW->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_pfit_pointFW->Fill(primtrk->fitTraits().numberOfFitPoints());
	  hists->m_prim_chargeFW->Fill(primtrk->geometry()->charge());
	  hists->m_prim_xfFW->Fill(primtrk->detectorInfo()->firstPoint().x());
	  hists->m_prim_yfFW->Fill(primtrk->detectorInfo()->firstPoint().y());
	  hists->m_prim_zfFW->Fill(primtrk->detectorInfo()->firstPoint().z());
	  hists->m_prim_radfFW->Fill(radf);
	  hists->m_prim_ratioFW->Fill(nfitntot);
	  hists->m_prim_ratiomFW->Fill(nfitnmax);
	  hists->m_ppsiFW->Fill(primtrk->geometry()->psi()/degree);
	  hists->m_petaFW->Fill(eta);
	  hists->m_ppTFW->Fill(pT);
	  hists->m_pmomFW->Fill(gmom);
	  hists->m_plengthFW->Fill(primtrk->length());
	  hists->m_pchisq0FW->Fill(chisq0);
	  hists->m_pchisq1FW->Fill(chisq1);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFW->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFW->Fill(primtrk->detectorInfo()->firstPoint().x(),
				  primtrk->detectorInfo()->firstPoint().y());
	  hists->m_peta_trklengthFW->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFW->Fill(primtrk->length(),
				   Float_t(primtrk->detectorInfo()->numberOfPoints()));
	  hists->m_pfpoint_lengthFW->Fill(primtrk->length(),
				   Float_t(primtrk->fitTraits().numberOfFitPoints()));
	}
*/
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

  for (UInt_t k=0; k<v0Vtx.size(); k++) {
    StV0Vertex *v0 = v0Vtx[k];
    if (v0) {
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
    }
  }

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

void StEventQAMaker::MakeHistEval() {

  // requires StMcEvent
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling Eval histograms ");

}

//_____________________________________________________________________________
