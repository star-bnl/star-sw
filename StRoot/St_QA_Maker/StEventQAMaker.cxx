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
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StDbUtilities/StCoordinates.hh"
// include this because it's not in StCoordinates.hh yet
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "HitHistograms.h"
#include "StTpcDb/StTpcDb.h"
#include "StarClassLibrary/StTimer.hh"
#include "StMessMgr.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcUtil/StEmcMath.h"
#include "StarClassLibrary/BetheBloch.h"

static StEmcGeom* emcGeom[4];

// These are the mean z positions of the FTPC padrows (1-20).
// The width of each padrow in z is 2 cm.
static float ftpcPadrowZPos[20] = {162.75,171.25,184.05,192.55,205.35,
				   213.85,226.65,235.15,247.95,256.45,
				   -162.75,-171.25,-184.05,-192.55,-205.35,
				   -213.85,-226.65,-235.15,-247.95,-256.45};

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
  return StQAMakerBase::Init();
}


//_____________________________________________________________________________
/// StEventQAMaker - InitRun; Book histograms, set defaults for member functions
Int_t StEventQAMaker::InitRun(int runnumber) {

  if(! mHitHist){
    mHitHist = new HitHistograms("QaDedxAllSectors","dE/dx for all TPC sectors",
                                 100,0.,1.e-5,2,this);
  }
  
  if ((gROOT->GetClass("StEmcMath")) && (gROOT->GetClass("StEmcGeom"))) {
    for(Int_t i=0; i<4; i++) {emcGeom[i] = StEmcGeom::getEmcGeom(i+1);} // 3-oct-2001 by PAI
  }
  return kStOK;
}

//_____________________________________________________________________________
/// StEventQAMaker - Make; fill histograms
Int_t StEventQAMaker::Make() {
  
  n_prim_good = 0;
  n_glob_good = 0;

  event = (StEvent *)GetInputDS("StEvent");
  if (event) {
    Bool_t realData = (event->info()->type() == "NONE");
    if (firstEvent) {
      if (realData) {
        histsSet = 1;
      } else {
        // process Monte Carlo events
        histsSet = 0;
      }
      BookHistTrigger();
    }
    UInt_t tword = 0;
    Bool_t doEvent = kTRUE;
    StTrigger* l0Trig = event->l0Trigger();
    Int_t run_num = event->runId();
    Int_t run_year = run_num/1000000;       // Determine run year from run #
    if ((l0Trig) && (run_year != 1)) {      // Don't use year 1 trigger word
      if (realData) doEvent = kFALSE;
      tword = l0Trig->triggerWord();
      if (tword) {
        if ((tword >= 0x1000) && (tword < 0x1100)) {
          mTrigWord->Fill(1.); // "MinBias"
	  doEvent = kTRUE;
        } else if ((tword >= 0x1100) && (tword < 0x1200)) {
          mTrigWord->Fill(2.); // "Central"
	  doEvent = kTRUE;
        } else if ((tword >= 0x1200) && (tword < 0x2000)) {
          mTrigWord->Fill(3.); // "Other Physics"
	  doEvent = kTRUE;
        } else if ((tword >= 0x2000) && (tword < 0x3000)) {
          mTrigWord->Fill(4.); // "pp Physics"
	  doEvent = kTRUE;
          if ((firstEvent) && (histsSet==1)) histsSet = 2;
        } else if (tword == 0xF200) {
          mTrigWord->Fill(7.); // "Laser"
        } else {
          mTrigWord->Fill(8.); // "Other"
        }
        for (int bitn=0; bitn<32; bitn++) {
          if (tword>>(bitn) & 1U)
            mTrigBits->Fill((Float_t) bitn);
        }
      } else {
        if (realData)
          gMessMgr->Warning("StEventQAMaker::Make(): trigger word=0 !!!!!");
      }
    } else { // No trigger info or year 1 data!
      if (run_year != 1)
        gMessMgr->Warning("StEventQAMaker::Make(): No trigger info...processing anyhow");
    }
    if (!doEvent) {
      gMessMgr->Message() << "StEventQAMaker::Make(): "
        << "skipping because trigger word=" << tword << endm;
      return kStOk;
    }
    if (firstEvent) BookHist();
    // only process if a primary vertex exists !!!
    if (event->primaryVertex()) {
      multiplicity = event->trackNodes().size();
      int makeStat = StQAMakerBase::Make();
      mNullPrimVtx->Fill(1);
      if ((histsSet == 1 && multiplicity >= 50)
	  || histsSet == 2)
	hists->mNullPrimVtxMult->Fill(1);
      return makeStat;
    } else {
      gMessMgr->Warning("StEventQAMaker::Make(): no primary vertex found!");
      mNullPrimVtx->Fill(-1);
      if ((histsSet == 1 && multiplicity >= 50)
	  || histsSet == 2)
	hists->mNullPrimVtxMult->Fill(-1);
      return kStOk;
    }
  } else {
    gMessMgr->Error("StEventQAMaker::Make(): no event found!");
    return kStErr;
  }
}


//_____________________________________________________________________________
/// Fill histograms for event summary
void StEventQAMaker::MakeHistEvSum() {

  //PrintInfo();
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling software monitor histograms ");

  if (event->softwareMonitor()) {
    StTpcSoftwareMonitor *tpcMon = event->softwareMonitor()->tpc();
    StFtpcSoftwareMonitor *ftpcMon = event->softwareMonitor()->ftpc();
    Float_t tpcChgWest=0;
    Float_t tpcChgEast=0;
    for (UInt_t i=0; i<24; i++) {
      if (i<12)
	tpcChgWest += tpcMon->chrg_tpc_in[i]+tpcMon->chrg_tpc_out[i];
      else
	tpcChgEast += tpcMon->chrg_tpc_in[i]+tpcMon->chrg_tpc_out[i];
    }
    m_glb_trk_chg->Fill(tpcChgEast/tpcChgWest,multClass);
    m_glb_trk_chgF->Fill(ftpcMon->chrg_ftpc_tot[0]/ftpcMon->chrg_ftpc_tot[1],multClass);
  }
}


//-----------------------------------------------------------------
void StEventQAMaker::MakeHistGlob() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling global track histograms ");

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t cnttrk=0;
  Int_t cnttrkT=0;
  Int_t cnttrkTS=0;
  Int_t cnttrkg=0;
  Int_t cnttrkgT=0;
  Int_t cnttrkgTS=0;
  Int_t cnttrkgTTS=0;
  Int_t cnttrkgFE=0;
  Int_t cnttrkgFW=0;

  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *globtrk = theNodes[i]->track(global);
    if (!globtrk) continue;
    cnttrk += theNodes[i]->entries(global);
    hists->m_globtrk_iflag->Fill(globtrk->flag());
    const StTrackTopologyMap& map=globtrk->topologyMap();
    if (map.trackTpcOnly()) cnttrkT++;
    if (map.trackTpcSvt()) cnttrkTS++;
    if (globtrk->flag()>0) {
      StTrackGeometry* geom = globtrk->geometry();
      StTrackFitTraits& fTraits = globtrk->fitTraits();
      StTrackDetectorInfo* detInfo = globtrk->detectorInfo();

      n_glob_good++;
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
      const StThreeVectorF& lastPoint = detInfo->lastPoint();
      const StThreeVectorF& origin = geom->origin();
      // get the helix position closest to the first point on track
      double sFirst = geom->helix().pathLength(firstPoint);
      // get the helix position closest to the last point on track
      double sLast = geom->helix().pathLength(lastPoint);

      StThreeVectorF dif = firstPoint - geom->helix().at(sFirst);
      StThreeVectorF difl = lastPoint - geom->helix().at(sLast);
      Float_t xcenter = geom->helix().xcenter();
      Float_t ycenter = geom->helix().ycenter();
      Float_t rcircle = 1./geom->helix().curvature();
      Float_t centerOfCircleToFP = sqrt(pow(xcenter-firstPoint.x(),2) +
					pow(ycenter-firstPoint.y(),2));
      Float_t centerOfCircleToLP = sqrt(pow(xcenter-lastPoint.x(),2) +
					pow(ycenter-lastPoint.y(),2));
      Float_t azimdif = dif.perp();
      if (rcircle<centerOfCircleToFP) azimdif *= -1.;
      Float_t azimdifl = difl.perp();
      if (rcircle<centerOfCircleToLP) azimdifl *= -1.;
      Float_t radf = firstPoint.perp();

      Float_t logImpact = TMath::Log10(globtrk->impactParameter());
      Float_t logCurvature = TMath::Log10(geom->curvature());

      // pathLength(double x,double y) should return path length at
      // DCA in the xy-plane to a given point
      double S = geom->helix().pathLength(0,0);
      StThreeVectorD dcaToBeam = geom->helix().at(S);

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

      // these histogram additions are for Lanny's evr QA histograms
      hists->m_dcaToBeamXY->Fill(dcaToBeam.x(),dcaToBeam.y());
      hists->m_dcaToBeamZ1->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ2->Fill(dcaToBeam.z());
      hists->m_dcaToBeamZ3->Fill(dcaToBeam.z());
      if (map.trackTpcOnly()) {
	hists->m_zDcaTanl->Fill(dcaToBeam.z(),TMath::Tan(geom->dipAngle()));
	hists->m_zDcaZf->Fill(dcaToBeam.z(),firstPoint.z());
      }
      if (map.trackTpcSvt())
	hists->m_zDcaTanl->Fill(dcaToBeam.z(),TMath::Tan(geom->dipAngle()));
      if (map.trackTpcSvt() && radf>40)
	hists->m_zDcaZf->Fill(dcaToBeam.z(),firstPoint.z());
      hists->m_zDcaPsi->Fill(dcaToBeam.z(),geom->psi()/degree);
      if (origin.phi() < 0)
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),360+origin.phi()/degree);
      else
        hists->m_zDcaPhi0->Fill(dcaToBeam.z(),origin.phi()/degree);

      // calculate the probability of a fit being correct
      // number of degrees of freedom = fitpoints-5 (5 params constrain track)
      Double_t ndf = 2*fTraits.numberOfFitPoints()-5;
      Double_t probability = TMath::Prob(chisq0*ndf,(int) ndf);
      hists->m_globtrk_fit_prob->Fill(probability);

      // now fill all TPC histograms ------------------------------------------------
      if (map.trackTpcOnly()) {

	cnttrkgT++;
	cnttrkgTTS++;
	// these are TPC only
	// m_glb_f0 uses hist class StMultiH1F
        hists->m_glb_f0->Fill(dif.x(),0.);
        hists->m_glb_f0->Fill(dif.y(),1.);
        hists->m_glb_f0->Fill(dif.z(),2.);

        hists->m_glb_xf0->Fill(dif.x());
        hists->m_glb_yf0->Fill(dif.y());
        hists->m_glb_zf0->Fill(dif.z());
	hists->m_glb_rzf0->Fill(azimdif,0.);
        hists->m_glb_rzf0->Fill(dif.z(),1.);
	hists->m_glb_rzl0->Fill(azimdifl,0.);
        hists->m_glb_rzl0->Fill(difl.z(),1.);
        hists->m_glb_impactT->Fill(logImpact);
        hists->m_glb_impactrT->Fill(globtrk->impactParameter());
        hists->m_glb_impactTTS->Fill(logImpact,1.);
        hists->m_glb_impactrTTS->Fill(globtrk->impactParameter(),1.);

	// TPC padrow histogram
	StTpcCoordinateTransform transformer(gStTpcDb);
	StGlobalCoordinate globalHitPosition(firstPoint);
	StTpcPadCoordinate padCoord;
	transformer(globalHitPosition,padCoord);
        hists->m_glb_padfT->Fill(padCoord.row());

        hists->m_pointT->Fill(detInfo->numberOfPoints());
        hists->m_max_pointT->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointT->Fill(fTraits.numberOfFitPoints());
        hists->m_fit_pointTTS->Fill(fTraits.numberOfFitPoints(),1.);
        hists->m_glb_chargeT->Fill(geom->charge());

        hists->m_glb_r0T->Fill(origin.perp());
	if (origin.phi() < 0)
	  hists->m_glb_phi0T->Fill(360+origin.phi()/degree);
	else
	  hists->m_glb_phi0T->Fill(origin.phi()/degree);

	if (firstPoint.z() < 0) {
	  hists->m_glb_padfTEW->Fill(padCoord.row(),0.);
	  if (firstPoint.phi() < 0)
	    hists->m_glb_phifT->Fill(360+firstPoint.phi()/degree,0.);
	  else
	    hists->m_glb_phifT->Fill(firstPoint.phi()/degree,0.);
	}
	else if (firstPoint.z() > 0) {
	  hists->m_glb_padfTEW->Fill(padCoord.row(),1.);
	  if (firstPoint.phi() < 0)
	    hists->m_glb_phifT->Fill(360+firstPoint.phi()/degree,1.);
	  else
	    hists->m_glb_phifT->Fill(firstPoint.phi()/degree,1.);
	}

        hists->m_glb_z0T->Fill(origin.z());
        hists->m_glb_curvT->Fill(logCurvature);

        hists->m_glb_xfT->Fill(firstPoint.x());
        hists->m_glb_yfT->Fill(firstPoint.y());
        hists->m_glb_zfT->Fill(firstPoint.z());
        hists->m_glb_radfT->Fill(radf);
        hists->m_glb_ratioT->Fill(nfitntot);
        hists->m_glb_ratioTTS->Fill(nfitntot,1.);
        hists->m_glb_ratiomT->Fill(nfitnmax);
        hists->m_glb_ratiomTTS->Fill(nfitnmax,1.);
        hists->m_psiT->Fill(geom->psi()/degree);
        hists->m_psiTTS->Fill(geom->psi()/degree,1.);
        hists->m_tanlT->Fill(TMath::Tan(geom->dipAngle()));
        hists->m_glb_thetaT->Fill(thetad);
        hists->m_etaT->Fill(eta);
        hists->m_etaTTS->Fill(eta,1.);
        hists->m_pTT->Fill(pT);
        hists->m_pTTTS->Fill(lmevpt,1.);
        hists->m_momT->Fill(gmom);
        hists->m_lengthT->Fill(globtrk->length());
        hists->m_chisq0T->Fill(chisq0);
        hists->m_chisq0TTS->Fill(chisq0,1.);
        hists->m_chisq1T->Fill(chisq1);
        hists->m_chisq1TTS->Fill(chisq1,1.);

	if (firstPoint.z()<0)
	  hists->m_globtrk_xf_yfTE->Fill(firstPoint.x(),
					 firstPoint.y());
	else
	  hists->m_globtrk_xf_yfTW->Fill(firstPoint.x(),
					 firstPoint.y());
        hists->m_eta_trklengthT->Fill(eta,globtrk->length());
        hists->m_npoint_lengthT->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthT->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));
        hists->m_fpoint_lengthTTS->Fill(globtrk->length(),
					Float_t(fTraits.numberOfFitPoints()));

        hists->m_pT_eta_recT->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  Float_t denom = 2*rcircle*
	    asin(sqrt((pow(firstPoint.x()-event->primaryVertex()->position().x(),2))+
		      (pow(firstPoint.y()-event->primaryVertex()->position().y(),2)))/
		 (2*rcircle));
	  hists->m_tanl_zfT->Fill((firstPoint.z() -
				   event->primaryVertex()->position().z())/denom,
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

	cnttrkgTS++;
	cnttrkgTTS++;

        hists->m_glb_f0TS->Fill(dif.x(),0.);
        hists->m_glb_f0TS->Fill(dif.y(),1.);
        hists->m_glb_f0TS->Fill(dif.z(),2.);

        hists->m_glb_xf0TS->Fill(dif.x());
        hists->m_glb_yf0TS->Fill(dif.y());
        hists->m_glb_zf0TS->Fill(dif.z());
	hists->m_glb_rzf0TS->Fill(azimdif,0.);
        hists->m_glb_rzf0TS->Fill(dif.z(),1.);
	hists->m_glb_rzl0TS->Fill(azimdifl,0.);
        hists->m_glb_rzl0TS->Fill(difl.z(),1.);
        hists->m_glb_impactTS->Fill(logImpact);
        hists->m_glb_impactrTS->Fill(globtrk->impactParameter());
        hists->m_glb_impactTTS->Fill(logImpact,0.);
        hists->m_glb_impactrTTS->Fill(globtrk->impactParameter(),0.);

	// SVT barrel histogram - causes segmentation violation currently
	//   => use m_glb_radfTS for now. -CPL
	//StSvtCoordinateTransform transformer;
	//StGlobalCoordinate globalHitPosition(firstPoint);
	//StSvtLocalCoordinate layerCoord;
	//transformer(globalHitPosition,layerCoord);
        //hists->m_glb_layerfTS->Fill(layerCoord.layer())

        hists->m_pointTS->Fill(detInfo->numberOfPoints());
        hists->m_max_pointTS->Fill(globtrk->numberOfPossiblePoints());
        hists->m_fit_pointTS->Fill(fTraits.numberOfFitPoints());
        hists->m_fit_pointTTS->Fill(fTraits.numberOfFitPoints(),0.);
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
        if (radf<40) {
	  hists->m_glb_zfTS->Fill(firstPoint.z());
	  if (firstPoint.phi() < 0)
	    hists->m_glb_phifTS->Fill(360+firstPoint.phi()/degree);
	  else
	    hists->m_glb_phifTS->Fill(firstPoint.phi()/degree);
	}

        hists->m_glb_radfTS->Fill(radf);
        hists->m_glb_ratioTS->Fill(nfitntot);
        hists->m_glb_ratioTTS->Fill(nfitntot,0.);
        hists->m_glb_ratiomTS->Fill(nfitnmax);
        hists->m_glb_ratiomTTS->Fill(nfitnmax,0.);
        hists->m_psiTS->Fill(geom->psi()/degree);
        hists->m_psiTTS->Fill(geom->psi()/degree,0.);
        hists->m_tanlTS->Fill(TMath::Tan(geom->dipAngle()));
        hists->m_glb_thetaTS->Fill(thetad);
        hists->m_etaTS->Fill(eta);
        hists->m_etaTTS->Fill(eta,0.);
        hists->m_pTTS->Fill(pT);
        hists->m_pTTTS->Fill(lmevpt,0.);
        hists->m_momTS->Fill(gmom);
        hists->m_lengthTS->Fill(globtrk->length());
        hists->m_chisq0TS->Fill(chisq0);
        hists->m_chisq0TTS->Fill(chisq0,0.);
        hists->m_chisq1TS->Fill(chisq1);
        hists->m_chisq1TTS->Fill(chisq1,0.);
        hists->m_globtrk_xf_yfTS->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthTS->Fill(eta,globtrk->length());
        hists->m_npoint_lengthTS->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
        hists->m_fpoint_lengthTS->Fill(globtrk->length(),
			       Float_t(fTraits.numberOfFitPoints()));
        hists->m_fpoint_lengthTTS->Fill(globtrk->length(),
					Float_t(fTraits.numberOfFitPoints()));

        hists->m_pT_eta_recTS->Fill(eta,lmevpt);
	if (event->primaryVertex()) {
	  Float_t denom = 2*rcircle*
	    asin(sqrt((pow(firstPoint.x()-event->primaryVertex()->position().x(),2))+
		      (pow(firstPoint.y()-event->primaryVertex()->position().y(),2)))/
		 (2*rcircle));
	  if (radf>40) {
	    hists->m_tanl_zfT->Fill((firstPoint.z() -
				     event->primaryVertex()->position().z())/denom,
				    Float_t(TMath::Tan(geom->dipAngle())));
	  }
	  if (radf<40) {
	    hists->m_tanl_zfTS->Fill((firstPoint.z() -
				     event->primaryVertex()->position().z())/denom,
				     Float_t(TMath::Tan(geom->dipAngle())));
	  }
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

	cnttrkgFE++;

	// east and west in same histogram
        hists->m_pointF->Fill(detInfo->numberOfPoints(),0.);
        hists->m_max_pointF->Fill(globtrk->numberOfPossiblePoints(),0.);
        hists->m_glb_chargeF->Fill(geom->charge(),0.);
        hists->m_glb_xfF->Fill(firstPoint.x(),0.);
        hists->m_glb_yfF->Fill(firstPoint.y(),0.);
        hists->m_glb_zfF->Fill(firstPoint.z(),0.);
        hists->m_glb_radfF->Fill(radf,0.);
        hists->m_glb_ratiomF->Fill(nfitnmax,0.);
        hists->m_psiF->Fill(geom->psi()/degree,0.);
        hists->m_etaF->Fill(fabs(eta),0.);
        hists->m_pTF->Fill(pT,0.);
        hists->m_momF->Fill(gmom,0.);
        hists->m_lengthF->Fill(globtrk->length(),0.);
        hists->m_chisq0F->Fill(chisq0,0.);
        hists->m_chisq1F->Fill(chisq1,0.);
        hists->m_glb_impactF->Fill(logImpact,0.);
        hists->m_glb_impactrF->Fill(globtrk->impactParameter(),0.);

	// FTPC plane histogram - there are no FTPC transformation utilities yet.
	//   => use m_glb_zfF for now.  -CPL
	//StFtpcCoordinateTransform transformer();
	//StGlobalCoordinate globalHitPosition(firstPoint);
	//StFtpcLocalCoordinate planeCoord;
	//transformer(globalHitPosition,planeCoord);
        //hists->m_glb_planefF->Fill(planeCoord.plane());  

	if (fabs(firstPoint.z()-ftpcPadrowZPos[10])<=1)
	  hists->m_glb_planefF->Fill(11,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[11])<=1)
	  hists->m_glb_planefF->Fill(12,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[12])<=1)
	  hists->m_glb_planefF->Fill(13,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[13])<=1)
	  hists->m_glb_planefF->Fill(14,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[14])<=1)
	  hists->m_glb_planefF->Fill(15,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[15])<=1)
	  hists->m_glb_planefF->Fill(16,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[16])<=1)
	  hists->m_glb_planefF->Fill(17,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[17])<=1)
	  hists->m_glb_planefF->Fill(18,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[18])<=1)
	  hists->m_glb_planefF->Fill(19,0.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[19])<=1)
	  hists->m_glb_planefF->Fill(20,0.);

	// east and west in separate histograms
        hists->m_pointFE->Fill(detInfo->numberOfPoints());
        hists->m_max_pointFE->Fill(globtrk->numberOfPossiblePoints());
        hists->m_glb_chargeFE->Fill(geom->charge());
        hists->m_glb_xfFE->Fill(firstPoint.x());
        hists->m_glb_yfFE->Fill(firstPoint.y());
        hists->m_glb_zfFE->Fill(firstPoint.z());
        hists->m_glb_radfFE->Fill(radf);
        hists->m_glb_ratiomFE->Fill(nfitnmax);
        hists->m_psiFE->Fill(geom->psi()/degree);
        hists->m_etaFE->Fill(eta);
        hists->m_pTFE->Fill(pT);
        hists->m_momFE->Fill(gmom);
        hists->m_lengthFE->Fill(globtrk->length());
        hists->m_chisq0FE->Fill(chisq0);
        hists->m_chisq1FE->Fill(chisq1);

        hists->m_pT_eta_recFE->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFE->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthFE->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFE->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
      }
      // now fill all FTPC West histograms ------------------------------------------
      else if (map.trackFtpcWest()) {

	cnttrkgFW++;

	// east and west in same histogram
        hists->m_pointF->Fill(detInfo->numberOfPoints(),1.);
        hists->m_max_pointF->Fill(globtrk->numberOfPossiblePoints(),1.);
        hists->m_glb_chargeF->Fill(geom->charge(),1.);
        hists->m_glb_xfF->Fill(firstPoint.x(),1.);
        hists->m_glb_yfF->Fill(firstPoint.y(),1.);
        hists->m_glb_zfF->Fill(firstPoint.z(),1.);
        hists->m_glb_radfF->Fill(radf,1.);
        hists->m_glb_ratiomF->Fill(nfitnmax,1.);
        hists->m_psiF->Fill(geom->psi()/degree,1.);
        hists->m_etaF->Fill(fabs(eta),1.);
        hists->m_pTF->Fill(pT,1.);
        hists->m_momF->Fill(gmom,1.);
        hists->m_lengthF->Fill(globtrk->length(),1.);
        hists->m_chisq0F->Fill(chisq0,1.);
        hists->m_chisq1F->Fill(chisq1,1.);
        hists->m_glb_impactF->Fill(logImpact,1.);
        hists->m_glb_impactrF->Fill(globtrk->impactParameter(),1.);

	if (fabs(firstPoint.z()-ftpcPadrowZPos[0])<=1)
	  hists->m_glb_planefF->Fill(1,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[1])<=1)
	  hists->m_glb_planefF->Fill(2,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[2])<=1)
	  hists->m_glb_planefF->Fill(3,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[3])<=1)
	  hists->m_glb_planefF->Fill(4,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[4])<=1)
	  hists->m_glb_planefF->Fill(5,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[5])<=1)
	  hists->m_glb_planefF->Fill(6,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[6])<=1)
	  hists->m_glb_planefF->Fill(7,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[7])<=1)
	  hists->m_glb_planefF->Fill(8,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[8])<=1)
	  hists->m_glb_planefF->Fill(9,1.);
	if (fabs(firstPoint.z()-ftpcPadrowZPos[9])<=1)
	  hists->m_glb_planefF->Fill(10,1.);

	// east and west in separate histograms
        hists->m_pointFW->Fill(detInfo->numberOfPoints());
        hists->m_max_pointFW->Fill(globtrk->numberOfPossiblePoints());
        hists->m_glb_chargeFW->Fill(geom->charge());
        hists->m_glb_xfFW->Fill(firstPoint.x());
        hists->m_glb_yfFW->Fill(firstPoint.y());
        hists->m_glb_zfFW->Fill(firstPoint.z());
        hists->m_glb_radfFW->Fill(radf);
        hists->m_glb_ratiomFW->Fill(nfitnmax);
        hists->m_psiFW->Fill(geom->psi()/degree);
        hists->m_etaFW->Fill(eta);
        hists->m_pTFW->Fill(pT);
        hists->m_momFW->Fill(gmom);
        hists->m_lengthFW->Fill(globtrk->length());
        hists->m_chisq0FW->Fill(chisq0);
        hists->m_chisq1FW->Fill(chisq1);

        hists->m_pT_eta_recFW->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFW->Fill(firstPoint.x(),
			       firstPoint.y());
        hists->m_eta_trklengthFW->Fill(eta,globtrk->length());
        hists->m_npoint_lengthFW->Fill(globtrk->length(),
	      		       Float_t(detInfo->numberOfPoints()));
      }
    }
  }
  hists->m_globtrk_tot->Fill(cnttrk); 
  hists->m_globtrk_good->Fill(cnttrkg);
  hists->m_globtrk_good_sm->Fill(cnttrkg);
  hists->m_globtrk_good_tot->Fill((Float_t)cnttrkgT/(Float_t)cnttrkT,1.);
  hists->m_globtrk_good_tot->Fill((Float_t)cnttrkgTS/(Float_t)cnttrkTS,0.);
  hists->m_globtrk_goodTTS->Fill(cnttrkgTTS);
  hists->m_globtrk_goodF->Fill(cnttrkgFE,cnttrkgFW);
}

//_____________________________________________________________________________
/// histograms filled in MakeHistPID() method
void StEventQAMaker::MakeHistDE() {

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPrim() {

  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling primary track histograms ");

  Int_t cnttrk=0;
  Int_t cnttrkg=0;
  Int_t cnttrkgT=0;
  Int_t cnttrkgTS=0;
  Int_t cnttrkgFE=0;
  Int_t cnttrkgFW=0; 
  Int_t pTcnttrkgFE=0;
  Int_t pTcnttrkgFW=0; 
  Float_t mean_ptT=0;
  Float_t mean_ptTS=0;
  Float_t mean_ptFE=0;
  Float_t mean_ptFW=0;
  Float_t mean_etaT=0;
  Float_t mean_etaTS=0;
  Float_t mean_etaFE=0;
  Float_t mean_etaFW=0;

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
	// due to variation on "kalman fitting" of primary tracks
	// we want to look at the hit residuals using the outerGeometry()
	// helix parameters (parameters at last point on track)
	StTrackGeometry* outerGeom = primtrk->outerGeometry();
        StTrackFitTraits& fTraits = primtrk->fitTraits();
        StTrackDetectorInfo* detInfo = primtrk->detectorInfo();
        const StTrackTopologyMap& map=primtrk->topologyMap();

	StTrack *gtrack = primtrk->node()->track(global);
	StTrackFitTraits& gfTraits = gtrack->fitTraits();
	Int_t nhit_prim_fit = fTraits.numberOfFitPoints();
	Int_t nhit_glob_fit = gfTraits.numberOfFitPoints();
	hists->m_primglob_fit->Fill((Float_t)nhit_prim_fit/(Float_t)nhit_glob_fit);

	n_prim_good++;
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
	const StThreeVectorF& lastPoint = detInfo->lastPoint();
	const StThreeVectorF& origin = geom->origin();
	//const StThreeVectorF& outerOrigin = outerGeom->origin();

	// need to find position on helix closest to first point on track since
	// the primary vertex is used as the first point on helix for primary
	// tracks -CPL
	double sFirst = geom->helix().pathLength(firstPoint);
	// get the helix position closest to the last point on track
	double sLast = geom->helix().pathLength(lastPoint);

	StThreeVectorF dif = firstPoint - geom->helix().at(sFirst);
	StThreeVectorF difl = lastPoint - geom->helix().at(sLast);

	Float_t xcenter = geom->helix().xcenter();
	Float_t ycenter = geom->helix().ycenter();
	Float_t rcircle = 1./geom->helix().curvature();
	Float_t centerOfCircleToFP = sqrt(pow(xcenter-firstPoint.x(),2) +
					  pow(ycenter-firstPoint.y(),2));
	Float_t centerOfCircleToLP = sqrt(pow(xcenter-lastPoint.x(),2) +
					  pow(ycenter-lastPoint.y(),2));
	Float_t azimdif = dif.perp();
	if (rcircle<centerOfCircleToFP) azimdif *= -1.;
	Float_t azimdifl = difl.perp();
	if (rcircle<centerOfCircleToLP) azimdifl *= -1.;

	// get the same information as above but from the outerGeometry()
	// ... this is so we can look at the hit residuals using the helix
	// parameters at the last point on a track
	double sFirstOuter = outerGeom->helix().pathLength(firstPoint);
	double sLastOuter = outerGeom->helix().pathLength(lastPoint);
	StThreeVectorF outerDif = firstPoint - outerGeom->helix().at(sFirstOuter);
	StThreeVectorF outerDifl = lastPoint - outerGeom->helix().at(sLastOuter);
	Float_t outerXcenter = outerGeom->helix().xcenter();
	Float_t outerYcenter = outerGeom->helix().ycenter();
	Float_t outerRcircle = 1./outerGeom->helix().curvature();
	Float_t outerCenterOfCircleToFP = sqrt(pow(outerXcenter-firstPoint.x(),2) +
					       pow(outerYcenter-firstPoint.y(),2));
	Float_t outerCenterOfCircleToLP = sqrt(pow(outerXcenter-lastPoint.x(),2) +
					       pow(outerYcenter-lastPoint.y(),2));
	Float_t outerAzimdif = outerDif.perp();
	if (outerRcircle<outerCenterOfCircleToFP) outerAzimdif *= -1.;
	Float_t outerAzimdifl = outerDifl.perp();
	if (outerRcircle<outerCenterOfCircleToLP) outerAzimdifl *= -1.;

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

	  cnttrkgT++;
	  mean_ptT += geom->momentum().perp();
	  mean_etaT += eta;
// these are TPC only
	  hists->m_prim_f0->Fill(outerDif.x(),0.);
	  hists->m_prim_f0->Fill(outerDif.y(),1.);
	  hists->m_prim_f0->Fill(outerDif.z(),2.);

	  hists->m_prim_xf0->Fill(outerDif.x());
	  hists->m_prim_yf0->Fill(outerDif.y());
	  hists->m_prim_zf0->Fill(outerDif.z());
	  hists->m_prim_rzf0->Fill(outerAzimdif,0.);
	  hists->m_prim_rzf0->Fill(outerDif.z(),1.);
	  hists->m_prim_rzl0->Fill(outerAzimdifl,0.);
	  hists->m_prim_rzl0->Fill(outerDifl.z(),1.);
	  hists->m_prim_impactT->Fill(logImpact);
	  hists->m_prim_impactrT->Fill(primtrk->impactParameter());
	  hists->m_prim_impactTTS->Fill(logImpact,1.);
	  hists->m_prim_impactrTTS->Fill(primtrk->impactParameter(),1.);

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
	  hists->m_ppsiTTS->Fill(geom->psi()/degree,1.);
	  hists->m_ptanlT->Fill(TMath::Tan(geom->dipAngle()));
	  hists->m_prim_thetaT->Fill(thetad);
	  hists->m_petaT->Fill(eta);
	  hists->m_petaTTS->Fill(eta,1.);
	  hists->m_ppTT->Fill(pT);
	  hists->m_ppTTTS->Fill(pT,1.);
	  hists->m_pmomT->Fill(gmom);
	  hists->m_plengthT->Fill(primtrk->length());
	  hists->m_pchisq0T->Fill(chisq0);
	  hists->m_pchisq1T->Fill(chisq1);
	  hists->m_pchisq0TTS->Fill(chisq0,1.);
	  hists->m_pchisq1TTS->Fill(chisq1,1.);

// these are for TPC & FTPC
	  if (firstPoint.z()<0)
	    hists->m_primtrk_xf_yfTE->Fill(firstPoint.x(),
					   firstPoint.y());
	  else
	    hists->m_primtrk_xf_yfTW->Fill(firstPoint.x(),
					   firstPoint.y());
	  hists->m_peta_trklengthT->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthT->Fill(primtrk->length(),
				  Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthT->Fill(primtrk->length(),
					 Float_t(fTraits.numberOfFitPoints()));
	  hists->m_pfpoint_lengthTTS->Fill(primtrk->length(),
					   Float_t(fTraits.numberOfFitPoints()));

// these are TPC only
	  hists->m_ppT_eta_recT->Fill(eta,lmevpt);
	  Float_t denom = 2*rcircle*
	    asin(sqrt((pow(firstPoint.x()-event->primaryVertex()->position().x(),2))+
		      (pow(firstPoint.y()-event->primaryVertex()->position().y(),2)))/
		 (2*rcircle));
	  hists->m_ptanl_zfT->Fill((firstPoint.z() -
				    event->primaryVertex()->position().z())/denom,
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

	  cnttrkgTS++;
	  mean_ptTS += geom->momentum().perp();
	  mean_etaTS += eta;

	  hists->m_prim_f0TS->Fill(outerDif.x(),0.);
	  hists->m_prim_f0TS->Fill(outerDif.y(),1.);
	  hists->m_prim_f0TS->Fill(outerDif.z(),2.);

	  hists->m_prim_xf0TS->Fill(outerDif.x());
	  hists->m_prim_yf0TS->Fill(outerDif.y());
	  hists->m_prim_zf0TS->Fill(outerDif.z());
	  hists->m_prim_rzf0TS->Fill(outerAzimdif,0.);
	  hists->m_prim_rzf0TS->Fill(outerDif.z(),1.);
	  hists->m_prim_rzl0TS->Fill(outerAzimdifl,0.);
	  hists->m_prim_rzl0TS->Fill(outerDifl.z(),1.);
	  hists->m_prim_impactTS->Fill(logImpact);
	  hists->m_prim_impactrTS->Fill(primtrk->impactParameter());
	  hists->m_prim_impactTTS->Fill(logImpact,0.);
	  hists->m_prim_impactrTTS->Fill(primtrk->impactParameter(),0.);

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
	  hists->m_ppsiTTS->Fill(geom->psi()/degree,0.);
	  hists->m_ptanlTS->Fill(TMath::Tan(geom->dipAngle()));
	  hists->m_prim_thetaTS->Fill(thetad);
	  hists->m_petaTS->Fill(eta);
	  hists->m_petaTTS->Fill(eta,0.);
	  hists->m_ppTTS->Fill(pT);
	  hists->m_ppTTTS->Fill(pT,0.);
	  hists->m_pmomTS->Fill(gmom);
	  hists->m_plengthTS->Fill(primtrk->length());
	  hists->m_pchisq0TS->Fill(chisq0);
	  hists->m_pchisq1TS->Fill(chisq1);
	  hists->m_pchisq0TTS->Fill(chisq0,0.);
	  hists->m_pchisq1TTS->Fill(chisq1,0.);

	  hists->m_primtrk_xf_yfTS->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthTS->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthTS->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	  hists->m_pfpoint_lengthTS->Fill(primtrk->length(),
					  Float_t(fTraits.numberOfFitPoints()));
	  hists->m_pfpoint_lengthTTS->Fill(primtrk->length(),
					   Float_t(fTraits.numberOfFitPoints()));

	  hists->m_ppT_eta_recTS->Fill(eta,lmevpt);
	  Float_t denom = 2*rcircle*
	    asin(sqrt((pow(firstPoint.x()-event->primaryVertex()->position().x(),2))+
		      (pow(firstPoint.y()-event->primaryVertex()->position().y(),2)))/
		 (2*rcircle));
	  if (radf>40) hists->m_ptanl_zfT->
			 Fill((firstPoint.z() - event->primaryVertex()->position().z())/denom,
			      Float_t(TMath::Tan(geom->dipAngle())));
	  if (radf<40) hists->m_ptanl_zfTS->
			 Fill((firstPoint.z() - event->primaryVertex()->position().z())/denom,
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

          if ( pT<2 ){
             mean_ptFE += geom->momentum().perp();
             pTcnttrkgFE++;
          }
	  cnttrkgFE++;
	  mean_etaFE += eta;
// these are TPC & FTPC
	  // east and west in same histogram
	  hists->m_ppointF->Fill(detInfo->numberOfPoints(),0.);
	  hists->m_pmax_pointF->Fill(primtrk->numberOfPossiblePoints(),0.);
	  hists->m_prim_chargeF->Fill(geom->charge(),0.);
	  hists->m_prim_xfF->Fill(firstPoint.x(),0.);
	  hists->m_prim_yfF->Fill(firstPoint.y(),0.);
	  hists->m_prim_zfF->Fill(firstPoint.z(),0.);
	  hists->m_prim_radfF->Fill(radf,0.);
	  hists->m_prim_ratiomF->Fill(nfitnmax,0.);
	  hists->m_ppsiF->Fill(geom->psi()/degree,0.);
	  hists->m_petaF->Fill(fabs(eta),0.);
	  hists->m_ppTF->Fill(pT,0.);
	  hists->m_pmomF->Fill(gmom,0.);
	  hists->m_plengthF->Fill(primtrk->length(),0.);
	  hists->m_pchisq0F->Fill(chisq0,0.);	
	  hists->m_prim_impactF->Fill(logImpact,0.);
	  hists->m_prim_impactrF->Fill(primtrk->impactParameter(),0.);
	  // east and west in separate histograms
	  hists->m_ppointFE->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointFE->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_prim_chargeFE->Fill(geom->charge());
	  hists->m_prim_xfFE->Fill(firstPoint.x());
	  hists->m_prim_yfFE->Fill(firstPoint.y());
	  hists->m_prim_zfFE->Fill(firstPoint.z());
	  hists->m_prim_radfFE->Fill(radf);
	  hists->m_prim_ratiomFE->Fill(nfitnmax);
	  hists->m_ppsiFE->Fill(geom->psi()/degree);
	  hists->m_petaFE->Fill(eta);
	  hists->m_ppTFE->Fill(pT);
	  hists->m_pmomFE->Fill(gmom);
	  hists->m_plengthFE->Fill(primtrk->length());
	  hists->m_pchisq0FE->Fill(chisq0);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFE->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFE->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthFE->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFE->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	}

// now fill all FTPC West histograms ------------------------------------------
        else if (map.trackFtpcWest()) {

          if ( pT<2 ){
             mean_ptFW += geom->momentum().perp();
             pTcnttrkgFW++;
          }
	  cnttrkgFW++;
	  mean_etaFW += eta;
// these are TPC & FTPC
	  // east and west in same histogram
	  hists->m_ppointF->Fill(detInfo->numberOfPoints(),1.);
	  hists->m_pmax_pointF->Fill(primtrk->numberOfPossiblePoints(),1.);
	  hists->m_prim_chargeF->Fill(geom->charge(),1.);
	  hists->m_prim_xfF->Fill(firstPoint.x(),1.);
	  hists->m_prim_yfF->Fill(firstPoint.y(),1.);
	  hists->m_prim_zfF->Fill(firstPoint.z(),1.);
	  hists->m_prim_radfF->Fill(radf,1.);
	  hists->m_prim_ratiomF->Fill(nfitnmax,1.);
	  hists->m_ppsiF->Fill(geom->psi()/degree,1.);
	  hists->m_petaF->Fill(fabs(eta),1.);
	  hists->m_ppTF->Fill(pT,1.);
	  hists->m_pmomF->Fill(gmom,1.);
	  hists->m_plengthF->Fill(primtrk->length(),1.);
	  hists->m_pchisq0F->Fill(chisq0,1.);
	  hists->m_prim_impactF->Fill(logImpact,1.);
	  hists->m_prim_impactrF->Fill(primtrk->impactParameter(),1.);
	  // east and west in separate histograms
	  hists->m_ppointFW->Fill(detInfo->numberOfPoints());
	  hists->m_pmax_pointFW->Fill(primtrk->numberOfPossiblePoints());
	  hists->m_prim_chargeFW->Fill(geom->charge());
	  hists->m_prim_xfFW->Fill(firstPoint.x());
	  hists->m_prim_yfFW->Fill(firstPoint.y());
	  hists->m_prim_zfFW->Fill(firstPoint.z());
	  hists->m_prim_radfFW->Fill(radf);
	  hists->m_prim_ratiomFW->Fill(nfitnmax);
	  hists->m_ppsiFW->Fill(geom->psi()/degree);
	  hists->m_petaFW->Fill(eta);
	  hists->m_ppTFW->Fill(pT);
	  hists->m_pmomFW->Fill(gmom);
	  hists->m_plengthFW->Fill(primtrk->length());
	  hists->m_pchisq0FW->Fill(chisq0);

// these are for TPC & FTPC
	  hists->m_ppT_eta_recFW->Fill(eta,lmevpt);
	  hists->m_primtrk_xf_yfFW->Fill(firstPoint.x(),
				  firstPoint.y());
	  hists->m_peta_trklengthFW->Fill(eta,primtrk->length());
	  hists->m_pnpoint_lengthFW->Fill(primtrk->length(),
				   Float_t(detInfo->numberOfPoints()));
	}
      }
    }
    hists->m_primtrk_good->Fill(cnttrkg);
    hists->m_primtrk_good_sm->Fill(cnttrkg);
    hists->m_primtrk_goodTTS->Fill(cnttrkgT+cnttrkgTS);
    hists->m_primtrk_goodF->Fill(cnttrkgFE,cnttrkgFW);
  }
  mean_ptT /= cnttrkgT;
  mean_ptTS /= cnttrkgTS;
  mean_ptFE /= pTcnttrkgFE;
  mean_ptFW /= pTcnttrkgFW;
  mean_etaT /= cnttrkgT;
  mean_etaTS /= cnttrkgTS;
  mean_etaFE /= cnttrkgFE;
  mean_etaFW /= cnttrkgFW;
  hists->m_primtrk_meanptTTS->Fill(mean_ptTS,0.);
  hists->m_primtrk_meanptTTS->Fill(mean_ptT,1.);
  hists->m_primtrk_meanptF->Fill(mean_ptFE,0.);
  hists->m_primtrk_meanptF->Fill(mean_ptFW,1.);
  hists->m_primtrk_meanetaTTS->Fill(mean_etaTS,0.);
  hists->m_primtrk_meanetaTTS->Fill(mean_etaT,1.);
  hists->m_primtrk_meanetaF->Fill(fabs(mean_etaFE),0.);
  hists->m_primtrk_meanetaF->Fill(fabs(mean_etaFW),1.);

  // MakeHistPrim() must be called after MakeHistGlob for the following to work
  hists->m_primglob_good->Fill((Float_t)n_prim_good/(Float_t)n_glob_good);
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
	double trackLength = dedxPidTr->length();
	if (dedxPidTr->detector() == kTpcId) {
	  // using BetheBloch::Sirrf method for curve normalized to 2.4 keV/cm
	  Float_t pionExpectedBB = BetheBloch::Sirrf(theTrack->geometry()->momentum().mag()/
						     pion_minus_mass_c2,trackLength);
	  pionExpectedBB *= 1.e-6;
	  hists->m_dedxTTS->Fill(dedx/pionExpectedBB);
	  hists->m_ndedxT->Fill(ndedx);
	  hists->m_dedx0T->Fill(dedx);
	  hists->m_dedx1T->Fill(error);
	  if (ndedx > 15) {
	    hists->m_p_dedx_rec->Fill((float)(p),(float)(dedx*1.e6));
	  }
	}
	if (dedxPidTr->detector() == kTpcSvtId) {
	  // using BetheBloch::Sirrf method for curve normalized to 2.4 keV/cm
	  Float_t pionExpectedBB = BetheBloch::Sirrf(theTrack->geometry()->momentum().mag()/
						     pion_minus_mass_c2,trackLength);
	  pionExpectedBB *= 1.e-6;
	  hists->m_dedxTTS->Fill(dedx/pionExpectedBB);
	}
	if (dedxPidTr->detector() == kFtpcWestId) {
	  // east and west in same histogram
	  hists->m_ndedxF->Fill(ndedx,1.);
	  hists->m_dedx0F->Fill(dedx,1.);
	  // east and west in separate histograms
	  hists->m_ndedxFW->Fill(ndedx);
	  hists->m_dedx0FW->Fill(dedx);
	}
	if (dedxPidTr->detector() == kFtpcEastId) {
	  // east and west in same histogram
	  hists->m_ndedxF->Fill(ndedx,0.);
	  hists->m_dedx0F->Fill(dedx,0.);
	  // east and west in separate histograms
	  hists->m_ndedxFE->Fill(ndedx);
	  hists->m_dedx0FE->Fill(dedx);
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
	hists->m_pv_xy->Fill(primVtx->position().x(),primVtx->position().y());
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
	hists->m_vtx_z_dist->Fill(v0->position().z() - primVtx->position().z());
	Float_t r_dist = sqrt(pow(v0->position().x()-primVtx->position().x(),2)+
			      pow(v0->position().y()-primVtx->position().y(),2));
	hists->m_vtx_r_dist->Fill(r_dist);
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
    // z and phi dist of hits
    for (UInt_t i=0; i<tpcHits->numberOfSectors(); i++)
      for (UInt_t j=0; j<tpcHits->sector(i)->numberOfPadrows(); j++)
	for (UInt_t k=0; k<tpcHits->sector(i)->padrow(j)->hits().size(); k++) {
	  Float_t x  = tpcHits->sector(i)->padrow(j)->hits()[k]->position().x();
	  Float_t y  = tpcHits->sector(i)->padrow(j)->hits()[k]->position().y();
	  Float_t z  = tpcHits->sector(i)->padrow(j)->hits()[k]->position().z();
	  Float_t phi = tpcHits->sector(i)->padrow(j)->hits()[k]->position().phi();
	  hists->m_z_hits->Fill(z);
	  if (z<0) {
	    if (phi<0)
	      hists->m_pnt_phiT->Fill(360+phi/degree,0.);
	    else
	      hists->m_pnt_phiT->Fill(phi/degree,0.);
	    hists->m_pnt_padrowT->Fill(j+1,0.); // physical padrow numbering starts at 1
	    hists->m_pnt_xyTE->Fill(x,y);
	  }
	  else {
	    if (phi<0)
	      hists->m_pnt_phiT->Fill(360+phi/degree,1.);
	    else
	      hists->m_pnt_phiT->Fill(phi/degree,1.);
	    hists->m_pnt_padrowT->Fill(j+1,1.); // physical padrow numbering starts at 1
	    hists->m_pnt_xyTW->Fill(x,y);
	  }
	}
    hists->m_pnt_tpc->Fill(tpcHits->numberOfHits());
    totalHits += tpcHits->numberOfHits();
  }
  if (svtHits) {
    ULong_t totalSvtHits = 0;
    for (UInt_t i=0; i<svtHits->numberOfBarrels(); i++) {
      StSvtBarrelHitCollection* svtbarrel = svtHits->barrel(i);
      for (UInt_t j=0; j<svtbarrel->numberOfLadders(); j++) {
        StSvtLadderHitCollection* svtladder = svtbarrel->ladder(j);
	for (UInt_t k=0; k<svtladder->numberOfWafers(); k++) {
          StSPtrVecSvtHit& svtwaferhits = svtladder->wafer(k)->hits();
	  for (UInt_t l=0; l<svtwaferhits.size(); l++) {
	    StSvtHit* svthit = svtwaferhits[l];
	    if (svthit->flag() < 4) {
	      Float_t z = svthit->position().z();
	      Float_t phi = svthit->position().phi();
	      hists->m_pnt_zS->Fill(z);
	      if (phi<0)
	        hists->m_pnt_phiS->Fill(360+phi/degree);
	      else
	        hists->m_pnt_phiS->Fill(phi/degree);
	      hists->m_pnt_barrelS->Fill(i+1); // physical barrel numbering starts at 1
              totalSvtHits++;
            }
	  }
        }
      }
    }
    // totalSvtHits = svtHits->numberOfHits();
    hists->m_pnt_svt->Fill(totalSvtHits);
    totalHits += totalSvtHits;
  }
  if (ftpcHits) {
    // StFtpcHitCollection doesn't differentiate between W and E FTPCs
    // so it is up to the user to check this via plane number -CPL
    for (UInt_t i=0; i<ftpcHits->numberOfPlanes(); i++) {
      for (UInt_t j=0; j<ftpcHits->plane(i)->numberOfSectors(); j++)
	for (UInt_t k=0; k<ftpcHits->plane(i)->sector(j)->hits().size(); k++) {
          Float_t x  = ftpcHits->plane(i)->sector(j)->hits()[k]->position().x();
          Float_t y  = ftpcHits->plane(i)->sector(j)->hits()[k]->position().y();
	  if (i<10) {
	    hists->m_pnt_planeF->Fill(i+1,1.); // physical numbering starts at 1
            hists->m_pnt_xyFW->Fill(x,y);
          }
	  else {
	    hists->m_pnt_planeF->Fill(i+1,0.); // physical numbering starts at 1
            hists->m_pnt_xyFE->Fill(x,y);
          }
	}
      if (i<10)
	ftpcHitsW += ftpcHits->plane(i)->numberOfHits();
      else
	ftpcHitsE += ftpcHits->plane(i)->numberOfHits();
    }
    // east and west in same histogram
    hists->m_pnt_ftpc->Fill(ftpcHitsE,0.);
    hists->m_pnt_ftpc->Fill(ftpcHitsW,1.);
    // east and west in separate histograms
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
  //cout <<"emccol = "<<emccol<<"\n";
  if (!emccol) return;

  //Get vertex
  StPrimaryVertex* pvert = event->primaryVertex(0);

  UInt_t i;
  
  //if (Debug()) 
  //  gMessMgr->Info(" *** in StEventQAMaker - filling EMC HITS ");

  for(i=0; i<4; i++){
    Int_t det = i+1;
    StDetectorId id = StEmcMath::detectorId(det);
    StEmcDetector* detector=emccol->detector(id);
    if(detector)
    {
      Float_t energy=0.0; // Energy for whole detector
      UInt_t  nh=0;         // Hits for whole detectors
      for(UInt_t j=1;j<121;j++){
        StEmcModule* module = detector->module(j);
        if(module)
        {
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
     }
     if(nh>0)     hists->m_emc_nhit->Fill(log10(Double_t(nh)), Float_t(det));
     if(energy>0) hists->m_emc_etot->Fill(log10(Double_t(energy)), Float_t(det));
     }
  }
  
  //if (Debug()) 
  //  gMessMgr->Info(" *** in StEventQAMaker - filling EMC Clusters ");
  
  for(i=0; i<4; i++) {  
    Int_t det = i+1, nh;
    StDetectorId id = StEmcMath::detectorId(det);
    StEmcDetector* detector = emccol->detector(id);
    if(detector)
    {
      StEmcClusterCollection* clusters=detector->cluster();
      if(clusters)
      {
        StSPtrVecEmcCluster& cluster = clusters->clusters();

        if(cluster.size()>0)
        {
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
      }
    }
  }      

  // Get the hists from StEmcPoints
  //if (Debug()) 
  //  gMessMgr->Info(" *** in StEventQAMaker - filling EMC Points ");

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
  
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - Finished filling EMC histograms ");

}
//_____________________________________________________________________________
void StEventQAMaker::MakeHistEval() {

  // requires StMcEvent
  if (Debug()) 
    gMessMgr->Info(" *** in StEventQAMaker - filling Eval histograms ");

  if (!(gROOT->GetClass("StMcEventMaker"))) return;
  StMcEvent* mcEvent = 0;
  StMcEventMaker* mcEventMaker = (StMcEventMaker*) GetMaker("StMcEvent");
  if (mcEventMaker) mcEvent = mcEventMaker->currentMcEvent();
  StMcVertex* mcprimaryVertex = mcEvent->primaryVertex();
  StPrimaryVertex* primaryVertex = event->primaryVertex();
  if ((primaryVertex) && (mcprimaryVertex)) {
    Float_t geantX = mcprimaryVertex->position().x();
    Float_t geantY = mcprimaryVertex->position().y();
    Float_t geantZ = mcprimaryVertex->position().z();
    Float_t recoX = primaryVertex->position().x();
    Float_t recoY = primaryVertex->position().y();
    Float_t recoZ = primaryVertex->position().z();
    hists->m_geant_reco_pvtx_x->Fill(geantX-recoX);
    hists->m_geant_reco_pvtx_y->Fill(geantY-recoY);
    hists->m_geant_reco_pvtx_z->Fill(geantZ-recoZ);
    hists->m_geant_reco_vtx_z_z->Fill(geantZ-recoZ,recoZ);    
  }

}
//_____________________________________________________________________________
void StEventQAMaker::MakeHistBBC() {

 Int_t i;
 StTriggerDetectorCollection* trig = event->triggerDetectorCollection();
 if (!trig) return;
 StBbcTriggerDetector& bbc = trig->bbc();

 for (i=0; i<32; i++)
 {
  hists->m_bbc_adc[i/8]->Fill(bbc.adc(i),i%8);
  hists->m_bbc_tdc[i/8]->Fill(bbc.tdc(i),i%8);
 }

}
//_____________________________________________________________________________
void StEventQAMaker::MakeHistFPD() {

 Int_t i;
 StFpdCollection* fpd = event->fpdCollection();
 if (!fpd) return;
 unsigned short* dfpd = fpd->adc();
 if (!dfpd) return;

 for (i=0; i<16; i++)
 {
  hists->m_fpd_top[i/8]->Fill((float) dfpd[i],i%8);
  hists->m_fpd_bottom[i/8]->Fill((float) dfpd[i+16],i%8);
  hists->m_fpd_south[i/8]->Fill((float) dfpd[i+32],i%8);
  if (i<12) hists->m_fpd_north[i/6]->Fill((float) dfpd[i+48],i%6);
 }

 hists->m_fpd_sums[0]->Fill((float) fpd->sumAdcTop());
 hists->m_fpd_sums[1]->Fill((float) fpd->sumAdcBottom());
 hists->m_fpd_sums[2]->Fill((float) fpd->sumAdcSouth());
 hists->m_fpd_sums[3]->Fill((float) fpd->sumAdcNorth());
 hists->m_fpd_sums[4]->Fill((float) fpd->sumAdcSmdX());
 hists->m_fpd_sums[5]->Fill((float) fpd->sumAdcSmdY());
 hists->m_fpd_sums[6]->Fill((float) fpd->sumAdcPreShower1());
 hists->m_fpd_sums[7]->Fill((float) fpd->sumAdcPreShower2());

}

//_____________________________________________________________________________
// $Id: StEventQAMaker.cxx,v 2.37 2002/04/23 01:59:55 genevb Exp $
// $Log: StEventQAMaker.cxx,v $
// Revision 2.37  2002/04/23 01:59:55  genevb
// Addition of BBC/FPD histos
//
// Revision 2.36  2002/04/03 21:13:11  lansdell
// primary track first, last point residuals now use outerGeometry() for helix parameters
//
// Revision 2.35  2002/02/23 00:31:26  lansdell
// bug fix: primary vertex check histograms for a multiplicity class did not reflect the correct number of good events
//
// Revision 2.34  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 2.33  2002/02/10 16:48:28  jeromel
// Attempt to prevent re-creation of mHitHist.
//
// Revision 2.32  2002/02/05 22:27:30  jeromel
// Modifications from David H. Int() -> InitRun().
//
// Revision 2.31  2001/12/28 09:19:12  genevb
// Adjustments for pp running
//
// Revision 2.30  2001/12/20 03:11:07  genevb
// pp trigger words 0x2XXX
//
// Revision 2.29  2001/11/20 21:53:45  lansdell
// added x-y dist of hits, tpc east&west histos
//
// Revision 2.28  2001/11/02 21:57:44  genevb
// Fix mistake in trigger word histogram
//
// Revision 2.27  2001/11/02 20:50:03  genevb
// Changed histogram ranges for momenta
//
// Revision 2.26  2001/10/31 22:08:40  suaide
// fixed EMC histograms
//
// Revision 2.25  2001/10/24 20:11:49  genevb
// Fixed trigger issue for year 1
//
// Revision 2.24  2001/10/15 16:15:02  pavlinov
// Clenup EMC stuff for production
//
// Revision 2.23  2001/09/10 18:00:12  genevb
// Another trigger word
//
// Revision 2.22  2001/09/01 14:24:40  genevb
// Allow trigger word=0 for MC data
//
// Revision 2.21  2001/08/31 21:29:50  genevb
// Check if trigger info exists
//
// Revision 2.20  2001/08/29 20:45:15  genevb
// Trigger word histos
//
// Revision 2.19  2001/08/23 17:57:36  genevb
// Added SVT hit flag
//
// Revision 2.18  2001/08/07 07:51:27  lansdell
// primvtx check for different multiplicities crashed for MC data, now fixed
//
// Revision 2.17  2001/08/03 20:33:55  lansdell
// added primvtx check histos for different multiplicities; separated x-y plot of first point on track, tpc into east and west histos
//
// Revision 2.16  2001/07/31 23:21:42  lansdell
// added last point, hit-helix histos
//
// Revision 2.15  2001/05/25 17:46:59  lansdell
// commented out unnecessary emccol cout statement
//
// Revision 2.14  2001/05/25 16:31:20  lansdell
// more updates to qa shift histograms
//
// Revision 2.13  2001/05/24 01:48:13  lansdell
// qa_shift histograms updated
//
// Revision 2.12  2001/05/23 00:14:52  lansdell
// more changes for qa_shift histograms
//
// Revision 2.11  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.10  2001/05/02 16:10:46  lansdell
// changed some histogram limits
//
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
