// $Id: StEventQAMaker.cxx,v 1.23 2000/01/31 22:15:24 kathy Exp $
// $Log: StEventQAMaker.cxx,v $
// Revision 1.23  2000/01/31 22:15:24  kathy
// added Gene's code to make mass plot for Xi's in table and StEvent versions
//
// Revision 1.22  2000/01/14 23:14:23  lansdell
// for primtrk, now find residuals correctly
//
// Revision 1.21  2000/01/12 19:09:33  lansdell
// fixed FTPC hits indexing bug
//
// Revision 1.20  2000/01/12 02:05:27  lansdell
// fixed minor for-loop problem
//
// Revision 1.19  2000/01/11 23:50:51  lansdell
// now filling FTPC hits histograms
//
// Revision 1.18  2000/01/11 00:10:08  lansdell
// totalled hits from detectors; made code for xdif, ydif, zdif, radf more readable (search for dif and radf)
//
// Revision 1.17  2000/01/08 03:27:34  lansdell
// fixed nfit/nmax ratio in Tab version; separated hits by detector; changed vertex histograms to allow for events with 0 vertices
//
// Revision 1.16  2000/01/07 20:35:00  kathy
// make some corrections to filling hist; add point hist for each det separately
//
// Revision 1.15  2000/01/07 01:02:55  lansdell
// fixed theta histogram and filled psi vs phi
//
// Revision 1.14  2000/01/05 23:20:50  lansdell
// changed sigma() to errorOnMean() for dedx histos
//
// Revision 1.13  1999/12/16 22:14:03  lansdell
// corrected how I get r0 and phi0 for global tracks
//
// Revision 1.12  1999/12/16 04:27:34  lansdell
// histogram of psi values now in degrees instead of radians
//
// Revision 1.11  1999/12/16 03:56:19  lansdell
// mirrored Kathy's changes in St_QA_Maker.cxx: separated tpc and tpc+svt histograms for global tracks using StEvent; added r0,phi0,z0,curvature histograms for global tracks in the tpc
//
// Revision 1.10  1999/12/14 18:33:21  kathy
// removed 4 ftpc histograms as per Janet's request
//
// Revision 1.9  1999/12/13 20:08:37  lansdell
// added pt vs eta in ftpc histogram to match table QA changes; updated logy scale histograms
//
// Revision 1.8  1999/12/08 03:07:20  lansdell
// made corresponding tpc/ftpc split in dedx histograms for StEventQAMaker
//
// Revision 1.7  1999/12/07 23:14:17  kathy
// fix primary vtx histograms for dst tables; split apart the ftpc and tpc in the dedx histograms
//
// Revision 1.6  1999/12/07 18:56:00  lansdell
// added a few more histograms and updated the default list of logy plots
//
// Revision 1.5  1999/12/06 22:25:05  kathy
// split apart the tpc and ftpc (east & west) histograms for the globtrk table; had to add characters to end of each histogram pointer to differentiate the different ones; updated the default list of hist to be plotted with logy scale
//
// Revision 1.4  1999/12/02 19:38:50  lansdell
// some more histograms are filled now, but still more to go!
//
// Revision 1.3  1999/11/23 19:00:50  lansdell
// Reorganized Make() and include files (Gene)
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StEventQAMaker class for QA Histograms using StEvent                 //
//     adapted from St_QA_Maker                                          //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "SystemOfUnits.h"
#include "TH1.h"
#include "TH2.h"
#include "StEventQAMaker.h"
#include "StEventTypes.h"

ClassImp(StEventQAMaker)

//_____________________________________________________________________________
StEventQAMaker::StEventQAMaker(const char *name, const char *title):StQABookHist(name,title,"StE") {
  
}

//_____________________________________________________________________________
StEventQAMaker::~StEventQAMaker() {

}

//_____________________________________________________________________________
Int_t StEventQAMaker::Finish() {

  return StMaker::Finish();
}

//_____________________________________________________________________________
Int_t StEventQAMaker::Init() {

// StEventQAMaker - Init; book histograms and set defaults for member functions

  return StQABookHist::Init();
}

//_____________________________________________________________________________
Int_t StEventQAMaker::Make() {
// StEventQAMaker - Make; fill histograms
  
  event = (StEvent *)GetInputDS("StEvent");
  if (event) {
    return StQABookHist::Make();
  } else {
    cout << "Error in StEventQAMaker::Make(): no event found!" << endl;
    return kStErr;
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistEvSum() {
  //  PrintInfo();
  // Fill histograms for event summary

  StEventSummary *event_summary = event->summary();
  if (event_summary) {
    Float_t trk_tot =   event_summary->numberOfTracks();
    Float_t trk_good =  event_summary->numberOfGoodTracks();
    Float_t trk_plus =  event_summary->numberOfGoodTracks(positive);
    Float_t trk_minus = event_summary->numberOfGoodTracks(negative);

    m_trk_tot_gd->Fill(trk_good/trk_tot); 
    m_glb_trk_tot->Fill(trk_tot);
    m_glb_trk_plusminus->Fill(trk_plus/trk_minus);
    m_vert_total->Fill(event_summary->numberOfVertices());
    m_glb_trk_prim->Fill(event_summary->numberOfGoodPrimaryTracks());

    m_mean_pt->Fill(event_summary->meanPt());
    m_mean_eta->Fill(event_summary->meanEta());
    m_rms_eta->Fill(event_summary->rmsEta());

    if(!isnan((double)(event_summary->primaryVertexPosition()[0])))
      m_prim_vrtx0->Fill(event_summary->primaryVertexPosition()[0]);
    if(!isnan((double)(event_summary->primaryVertexPosition()[1])))
      m_prim_vrtx1->Fill(event_summary->primaryVertexPosition()[1]);
    if(!isnan((double)(event_summary->primaryVertexPosition()[2])))
      m_prim_vrtx2->Fill(event_summary->primaryVertexPosition()[2]);
  }
}

//-----------------------------------------------------------------
void StEventQAMaker::MakeHistGlob() {

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t cnttrk=0;
  Int_t cnttrkg=0;

  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *globtrk = theNodes[i]->track(global);
    if (!globtrk) continue;
    cnttrk += theNodes[i]->entries(global);
    m_globtrk_iflag->Fill(globtrk->flag());
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

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 

      for (UInt_t k=0; k<globtrk->pidTraits().size(); k++)
	m_det_id->Fill(globtrk->pidTraits()[k]->detector());

// now fill all TPC histograms ------------------------------------------------
      if (globtrk->flag()>100 && globtrk->flag()<200) {

// these are TPC only
	m_glb_xf0->Fill(dif.x());
	m_glb_yf0->Fill(dif.y());
	m_glb_zf0->Fill(dif.z());
	m_glb_impactT->Fill(globtrk->impactParameter());

// these are TPC & FTPC
	m_pointT->Fill(globtrk->detectorInfo()->numberOfPoints());
	m_max_pointT->Fill(globtrk->numberOfPossiblePoints());
	m_fit_pointT->Fill(globtrk->fitTraits().numberOfFitPoints());
	m_glb_chargeT->Fill(globtrk->geometry()->charge());

	m_glb_r0T->Fill(globtrk->geometry()->origin().perp());
	m_glb_phi0T->Fill(globtrk->geometry()->origin().phi()/degree);
	m_glb_z0T->Fill(globtrk->geometry()->origin().z());
	m_glb_curvT->Fill(globtrk->geometry()->curvature());

	m_glb_xfT->Fill(globtrk->detectorInfo()->firstPoint().x());
	m_glb_yfT->Fill(globtrk->detectorInfo()->firstPoint().y());
	m_glb_zfT->Fill(globtrk->detectorInfo()->firstPoint().z());
	m_glb_radfT->Fill(radf);
	m_glb_ratioT->Fill(nfitntot);
        m_glb_ratiomT->Fill(nfitnmax);
	m_psiT->Fill(globtrk->geometry()->psi()/degree);
	m_tanlT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()));
	m_glb_thetaT->Fill(thetad);
	m_etaT->Fill(eta);
	m_pTT->Fill(pT);
	m_momT->Fill(gmom);
	m_lengthT->Fill(globtrk->length());
	m_chisq0T->Fill(chisq0);
	m_chisq1T->Fill(chisq1);

// these are for TPC & FTPC
	m_globtrk_xf_yfT->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
	m_eta_trklengthT->Fill(eta,globtrk->length());
	m_npoint_lengthT->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
	m_fpoint_lengthT->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

// these are TPC only
	m_pT_eta_recT->Fill(eta,lmevpt);
	m_tanl_zfT->Fill(globtrk->detectorInfo()->firstPoint().z(),
		         Float_t(TMath::Tan(globtrk->geometry()->dipAngle())));
	m_mom_trklengthT->Fill(globtrk->length(),lmevmom);
	m_chisq0_momT->Fill(lmevmom,chisq0);
	m_chisq1_momT->Fill(lmevmom,chisq1);
	m_chisq0_etaT->Fill(eta,chisq0);
	m_chisq1_etaT->Fill(eta,chisq1);
	m_chisq0_dipT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq0);
	m_chisq1_dipT->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq1);
	m_chisq0_zfT->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq0);
	m_chisq1_zfT->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq1);
	m_nfptonpt_momT->Fill(lmevmom,nfitntot);
	m_nfptonpt_etaT->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CL
	Float_t psi_deg = globtrk->geometry()->origin().phi()/degree;
	Float_t phi_deg = globtrk->geometry()->psi()/degree;
	m_psi_phiT->Fill(phi_deg,psi_deg);
      }

// now fill all TPC+SVT histograms --------------------------------------------

      if (globtrk->flag()>500 && globtrk->flag()<600 ) {

        m_glb_xf0TS->Fill(dif.x());
        m_glb_yf0TS->Fill(dif.y());
        m_glb_zf0TS->Fill(dif.z());
	m_glb_impactTS->Fill(globtrk->impactParameter());

	m_pointTS->Fill(globtrk->detectorInfo()->numberOfPoints());
	m_max_pointTS->Fill(globtrk->numberOfPossiblePoints());
	m_fit_pointTS->Fill(globtrk->fitTraits().numberOfFitPoints());
	m_glb_chargeTS->Fill(globtrk->geometry()->charge());

	m_glb_r0TS->Fill(globtrk->geometry()->origin().perp());
	m_glb_phi0TS->Fill(globtrk->geometry()->origin().phi()/degree);
	m_glb_z0TS->Fill(globtrk->geometry()->origin().z());
	m_glb_curvTS->Fill(globtrk->geometry()->curvature());

	m_glb_xfTS->Fill(globtrk->detectorInfo()->firstPoint().x());
	m_glb_yfTS->Fill(globtrk->detectorInfo()->firstPoint().y());
	m_glb_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z());
	m_glb_radfTS->Fill(radf);
	m_glb_ratioTS->Fill(nfitntot);
        m_glb_ratiomTS->Fill(nfitnmax);
	m_psiTS->Fill(globtrk->geometry()->psi()/degree);
	m_tanlTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()));
	m_glb_thetaTS->Fill(thetad);
	m_etaTS->Fill(eta);
	m_pTTS->Fill(pT);
	m_momTS->Fill(gmom);
	m_lengthTS->Fill(globtrk->length());
	m_chisq0TS->Fill(chisq0);
	m_chisq1TS->Fill(chisq1);

	m_globtrk_xf_yfTS->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
	m_eta_trklengthTS->Fill(eta,globtrk->length());
	m_npoint_lengthTS->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
	m_fpoint_lengthTS->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

	m_pT_eta_recTS->Fill(eta,lmevpt);
	m_tanl_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z(),
		         Float_t(TMath::Tan(globtrk->geometry()->dipAngle())));
	m_mom_trklengthTS->Fill(globtrk->length(),lmevmom);
	m_chisq0_momTS->Fill(lmevmom,chisq0);
	m_chisq1_momTS->Fill(lmevmom,chisq1);
	m_chisq0_etaTS->Fill(eta,chisq0);
	m_chisq1_etaTS->Fill(eta,chisq1);
	m_chisq0_dipTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq0);
	m_chisq1_dipTS->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq1);
	m_chisq0_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq0);
	m_chisq1_zfTS->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq1);
	m_nfptonpt_momTS->Fill(lmevmom,nfitntot);
	m_nfptonpt_etaTS->Fill(eta,nfitntot);
	// had to make psi_deg and phi_deg b/c ROOT won't compile otherwise
	// for some strange reason... -CL
	Float_t psi_deg = globtrk->geometry()->origin().phi()/degree;
	Float_t phi_deg = globtrk->geometry()->psi()/degree;
	m_psi_phiTS->Fill(phi_deg,psi_deg);
      }

// now fill all FTPC East histograms ------------------------------------------
      if (globtrk->flag()>700 && globtrk->flag()<800 && globtrk->pidTraits()[0]->detector()==5) {             // didn't loop over pidTraits vector this time
                               // -> should I have? -CL
// these are TPC & FTPC
	m_pointFE->Fill(globtrk->detectorInfo()->numberOfPoints());
	m_max_pointFE->Fill(globtrk->numberOfPossiblePoints());
	m_fit_pointFE->Fill(globtrk->fitTraits().numberOfFitPoints());
	m_glb_chargeFE->Fill(globtrk->geometry()->charge());
	m_glb_xfFE->Fill(globtrk->detectorInfo()->firstPoint().x());
	m_glb_yfFE->Fill(globtrk->detectorInfo()->firstPoint().y());
	m_glb_zfFE->Fill(globtrk->detectorInfo()->firstPoint().z());
	m_glb_radfFE->Fill(radf);
	m_glb_ratioFE->Fill(nfitntot);
        m_glb_ratiomFE->Fill(nfitnmax);
	m_psiFE->Fill(globtrk->geometry()->psi()/degree);
	m_etaFE->Fill(eta);
	m_pTFE->Fill(pT);
	m_momFE->Fill(gmom);
	m_lengthFE->Fill(globtrk->length());
	m_chisq0FE->Fill(chisq0);
	m_chisq1FE->Fill(chisq1);

// these are for TPC & FTPC
	m_pT_eta_recFE->Fill(eta,lmevpt);
	m_globtrk_xf_yfFE->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
	m_eta_trklengthFE->Fill(eta,globtrk->length());
	m_npoint_lengthFE->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
	m_fpoint_lengthFE->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

      }
// now fill all FTPC West histograms ------------------------------------------
      if (globtrk->flag()>700 && globtrk->flag()<800 && globtrk->pidTraits()[0]->detector()==4) {             // didn't loop over pidTraits vector this time
                               // -> should I have? -CL
// these are TPC & FTPC
	m_pointFW->Fill(globtrk->detectorInfo()->numberOfPoints());
	m_max_pointFW->Fill(globtrk->numberOfPossiblePoints());
	m_fit_pointFW->Fill(globtrk->fitTraits().numberOfFitPoints());
	m_glb_chargeFW->Fill(globtrk->geometry()->charge());
	m_glb_xfFW->Fill(globtrk->detectorInfo()->firstPoint().x());
	m_glb_yfFW->Fill(globtrk->detectorInfo()->firstPoint().y());
	m_glb_zfFW->Fill(globtrk->detectorInfo()->firstPoint().z());
	m_glb_radfFW->Fill(radf);
	m_glb_ratioFW->Fill(nfitntot);
        m_glb_ratiomFW->Fill(nfitnmax);
	m_psiFW->Fill(globtrk->geometry()->psi()/degree);
	m_etaFW->Fill(eta);
	m_pTFW->Fill(pT);
	m_momFW->Fill(gmom);
	m_lengthFW->Fill(globtrk->length());
	m_chisq0FW->Fill(chisq0);
	m_chisq1FW->Fill(chisq1);

// these are for TPC & FTPC
	m_pT_eta_recFW->Fill(eta,lmevpt);
	m_globtrk_xf_yfFW->Fill(globtrk->detectorInfo()->firstPoint().x(),
			       globtrk->detectorInfo()->firstPoint().y());
	m_eta_trklengthFW->Fill(eta,globtrk->length());
	m_npoint_lengthFW->Fill(globtrk->length(),
	      		       Float_t(globtrk->detectorInfo()->numberOfPoints()));
	m_fpoint_lengthFW->Fill(globtrk->length(),
			       Float_t(globtrk->fitTraits().numberOfFitPoints()));

      }
    }
  }
  m_globtrk_tot->Fill(cnttrk);
  m_globtrk_good->Fill(cnttrkg);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistDE() {
  // Fill histograms for dE/dx

  Int_t cntrows=0;
  StSPtrVecTrackNode &theNodes = event->trackNodes();

  for (UInt_t i=0; i<theNodes.size(); i++) {
    cntrows++;
    StTrack *theTrack = theNodes[i]->track(0);
    if (!theTrack) continue;
    StSPtrVecTrackPidTraits &trkPidTr = theTrack->pidTraits();
    if (trkPidTr.size() > 0) {

      //  should use dynamic_cast, but will crash in root4star (why?) -CL
      //StDedxPidTraits *dedxPidTr = dynamic_cast<StDedxPidTraits*>(trkPidTr[0]);
      StDedxPidTraits *dedxPidTr = (StDedxPidTraits*)(trkPidTr[0]);
      if (dedxPidTr) {
	if (trkPidTr[0]->detector()==1) {
	  m_ndedxT->Fill(dedxPidTr->numberOfPoints());
	  m_dedx0T->Fill(dedxPidTr->mean());
	  m_dedx1T->Fill(dedxPidTr->errorOnMean());
	}
	if (trkPidTr[0]->detector()==4) {
	  m_ndedxFW->Fill(dedxPidTr->numberOfPoints());
	  m_dedx0FW->Fill(dedxPidTr->mean());
	  m_dedx1FW->Fill(dedxPidTr->errorOnMean());
	}
	if (trkPidTr[0]->detector()==5) {
	  m_ndedxFE->Fill(dedxPidTr->numberOfPoints());
	  m_dedx0FE->Fill(dedxPidTr->mean());
	  m_dedx1FE->Fill(dedxPidTr->errorOnMean());
	}
      }
    }
  }
  m_ndedxr->Fill(cntrows);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPrim() {

  Int_t cnttrk=0;
  Int_t cnttrkg=0;

  StPrimaryVertex *primVtx = event->primaryVertex();
  UInt_t daughters=0;
  UInt_t currentNumber=0;
  for (UInt_t v=0; v<event->numberOfPrimaryVertices(); v++) {
    currentNumber = event->primaryVertex(v)->numberOfDaughters();
    if (currentNumber > daughters) {
      daughters = currentNumber;
      primVtx = event->primaryVertex(v);
    }
  }
  
  if (primVtx) {
    cnttrk = primVtx->numberOfDaughters();
    m_primtrk_tot->Fill(cnttrk);

    for (UInt_t i=0; i<primVtx->numberOfDaughters(); i++) {
      StTrack *primtrk = primVtx->daughter(i);
      m_primtrk_iflag->Fill(primtrk->flag());

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
        Float_t nfitntot = (Float_t(primtrk->fitTraits().numberOfFitPoints()))/
	                   (Float_t(primtrk->detectorInfo()->numberOfPoints()));
	// need to find position on helix closest to first point on track since
	// the primary vertex is used as the first point on helix for primary
	// tracks -CL
	double s = primtrk->geometry()->helix().
	           pathLength(primtrk->detectorInfo()->firstPoint());
	StThreeVectorF dif = primtrk->detectorInfo()->firstPoint() -
	                     primtrk->geometry()->helix().at(s);
        Float_t radf = primtrk->detectorInfo()->firstPoint().perp();

	for (UInt_t k=0; k<primtrk->pidTraits().size(); k++)
	  m_pdet_id->Fill(primtrk->pidTraits()[k]->detector());
	m_ppoint->Fill(primtrk->detectorInfo()->numberOfPoints());
	m_pmax_point->Fill(primtrk->numberOfPossiblePoints());
	m_pfit_point->Fill(primtrk->fitTraits().numberOfFitPoints());
        m_prim_charge->Fill(primtrk->geometry()->charge());
        m_prim_xf->Fill(primtrk->detectorInfo()->firstPoint().x());
        m_prim_yf->Fill(primtrk->detectorInfo()->firstPoint().y());
        m_prim_zf->Fill(primtrk->detectorInfo()->firstPoint().z());
        m_prim_xf0->Fill(dif.x());
        m_prim_yf0->Fill(dif.y());
        m_prim_zf0->Fill(dif.z());
        m_prim_radf->Fill(radf);
        m_prim_ratio->Fill(nfitntot);
	m_ppsi->Fill(primtrk->geometry()->psi()/degree);
        m_ptanl->Fill(TMath::Tan(primtrk->geometry()->dipAngle()));
        m_prim_theta->Fill(thetad);
	m_peta->Fill(eta);
	m_ppT->Fill(pT);
        m_pmom->Fill(gmom);
	m_plength->Fill(primtrk->length());
        m_prim_impact->Fill(primtrk->impactParameter());
       	m_pchisq0->Fill(chisq0);
	m_pchisq1->Fill(chisq1);

	m_ppT_eta_rec->Fill(eta,lmevpt);
        m_primtrk_xf_yf->Fill(primtrk->detectorInfo()->firstPoint().x(),
			      primtrk->detectorInfo()->firstPoint().y());
        m_ptanl_zf->Fill(primtrk->detectorInfo()->firstPoint().z(),
			 Float_t(TMath::Tan(primtrk->geometry()->dipAngle())));
	m_pmom_trklength->Fill(primtrk->length(),lmevmom);
        m_peta_trklength->Fill(eta,primtrk->length());
	m_pnpoint_length->Fill(primtrk->length(),Float_t(primtrk->
			       detectorInfo()->numberOfPoints()));
	m_pfpoint_length->Fill(primtrk->length(),Float_t(primtrk->
			       fitTraits().numberOfFitPoints()));
	m_pchisq0_mom->Fill(lmevmom,chisq0);
	m_pchisq1_mom->Fill(lmevmom,chisq1);
	m_pchisq0_eta->Fill(eta,chisq0);
	m_pchisq1_eta->Fill(eta,chisq1);
	m_pchisq0_dip->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq0);
	m_pchisq1_dip->Fill(TMath::Tan(primtrk->geometry()->dipAngle()),chisq1);
	m_pchisq0_zf->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq0);
	m_pchisq1_zf->Fill(primtrk->detectorInfo()->firstPoint().z(),chisq1);
        m_pnfptonpt_mom->Fill(lmevmom,nfitntot);
        m_pnfptonpt_eta->Fill(eta,nfitntot);
      }
    }
    m_primtrk_good->Fill(cnttrkg);
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistGen() {

  //  StEvent does not have data corresponding to the DST particle table
  //  so this method is not used in StEventQAMaker.  However, this
  //  information can be found from StMcEvent.
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling particle histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t nchgpart = 0;
  Int_t totpart = 0;

  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *theTrack = theNodes[i]->track(global);  // need primary too? -CL
    if (!theTrack) continue;
    totpart++;
    StParticleDefinition *part = theTrack->fitTraits().pidHypothesis();

    //  select only particles which can be detected
    //  in the STAR detector. Here we restrict us to/
    //  the most common species.

    if (part == StElectron::instance() || part == StMuonMinus::instance() ||
	part == StPionPlus::instance() || part == StKaonPlus::instance() ||
	part == StProton::instance()) {
      nchgpart++;
      Double_t pT = theTrack->geometry()->momentum().perp();
      Float_t eta = theTrack->geometry()->momentum().pseudoRapidity();

      m_H_pT_eta_gen->Fill(eta, (Float_t) pT);
      m_H_pT_gen->Fill((Float_t) pT);
      m_H_eta_gen->Fill(eta);
      //m_H_vtxx->Fill(p->vhep[0]);
      //m_H_vtxy->Fill(p->vhep[1]);
      //m_H_vtxz->Fill(p->vhep[2]);
    }
  }
  m_H_npart->Fill(totpart);
  m_H_ncpart->Fill(nchgpart);
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistV0() {

  if (Debug()) cout << " *** in StEventQAMaker - filling dst_v0_vertex histograms " << endl;

  StSPtrVecV0Vertex &v0vertices = event->v0Vertices();

  Int_t cntrows=0;
  cntrows = v0vertices.size();
  m_v0->Fill(cntrows);

  if (v0vertices.size() > 0) {
    Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
    Float_t m_pimass2 = (pion_minus_mass_c2*pion_minus_mass_c2);

    for (UInt_t k=0; k<v0vertices.size(); k++) {
      StV0Vertex *v0 = v0vertices[k];
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
	m_ev0_lama_hist->Fill(inv_mass_la);
	m_ev0_k0ma_hist->Fill(inv_mass_k0);
      }
    }
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPID() {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling PID histograms " << endl;
  
  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  StPrimaryVertex *primVtx = event->primaryVertex();
  UInt_t daughters=0;
  UInt_t currentNumber=0;
  for (UInt_t v=0; v<event->numberOfPrimaryVertices(); v++) {
    currentNumber = event->primaryVertex(v)->numberOfDaughters();
    if (currentNumber > daughters) {
      daughters = currentNumber;
      primVtx = event->primaryVertex(v);
    }
  }

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *theTrack = theNodes[i]->track(0);
    if (!theTrack) continue;
    StTrackPidTraits *trkPidTr = theTrack->pidTraits()[0];
    if (trkPidTr) {
      //  should use dynamic_cast, but will crash in root4star (why?) -CL
      //StDedxPidTraits *dedxPidTr = dynamic_cast<StDedxPidTraits*>(trkPidTr);
      StDedxPidTraits *dedxPidTr = (StDedxPidTraits*)(trkPidTr);
      if (dedxPidTr) {
	Float_t p = 
      }
    }
  }

  
  if (primVtx) {
    for (UInt_t i=0; i<primVtx->numberOfDaughters(); i++) {
      StTrack *primtrk = primVtx->daughter(i);

      if (primtrk->flag()>0) {
*/
/* ===== Old DST table-based code... -CL

  // spectra-PID diagnostic histograms
  St_dst_track      *primtrk     = (St_dst_track     *) dstI["primtrk"];
  St_dst_dedx       *dst_dedx    = (St_dst_dedx *) dstI["dst_dedx"];
  
  if (dst_dedx && primtrk) {
    dst_dedx_st  *d   = dst_dedx->GetTable();
    dst_track_st  *trk   = primtrk->GetTable();
    Int_t no_of_tracks  = primtrk->GetNRows();
    // loop over dedx entries
    for (Int_t l = 0; l < dst_dedx->GetNRows(); l++,d++){
      Float_t dedx_m = d->dedx[0];
      Int_t igl = d->id_track;
      Int_t igl_use = igl - 1;
      // this is bad style, since it assumes the global track has not been sorted
      // it works for now
      if (igl_use >= 0 && igl_use < no_of_tracks) {
	dst_track_st  *t = trk + igl_use ;
	if (t->iflag>0) {
	  Float_t invpt = t->invpt;
	  Float_t pT = 9999.;
	  if (invpt) pT = 1./TMath::Abs(invpt);
	  Float_t pz = pT*t->tanl;
	  Float_t  p = TMath::Sqrt(pT*pT+pz*pz);
	  Float_t x0 = t->x_first[0];
	  Float_t y0 = t->x_first[1];
	  
	  if (d->det_id==1 && d->ndedx >15 ) { 
	    m_p_dedx_rec->Fill(p,(float)(dedx_m*1e6)); // change from GeV/cm to keV/cm
	  }
	}
      }
    }
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistVertex() {

  if (Debug()) cout << " *** in StEventQAMaker - filling vertex histograms " << endl;

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

    for (UInt_t j=0; j<event->numberOfPrimaryVertices(); j++) {
      StPrimaryVertex *aPrimVtx = event->primaryVertex(j);
      if (aPrimVtx == primVtx) {
	//m_pv_detid->Fill(primVtx->det_id);
	m_pv_vtxid->Fill(primVtx->type());
	if (!isnan(double(primVtx->position().x())))
	  m_pv_x->Fill(primVtx->position().x());
	if (!isnan(double(primVtx->position().y())))
	  m_pv_y->Fill(primVtx->position().y());
	if (!isnan(double(primVtx->position().z())))
	  m_pv_z->Fill(primVtx->position().z());
	m_pv_pchi2->Fill(primVtx->chiSquared());
      }
      else {
	//m_v_detid->Fill(aPrimVtx->det_id); 
	m_v_vtxid->Fill(aPrimVtx->type());
	if (!isnan(double(aPrimVtx->position().x())))
	  m_v_x->Fill(aPrimVtx->position().x());     
	if (!isnan(double(aPrimVtx->position().y())))
	  m_v_y->Fill(aPrimVtx->position().y());     
	if (!isnan(double(aPrimVtx->position().z())))
	  m_v_z->Fill(aPrimVtx->position().z());     
	m_v_pchi2->Fill(aPrimVtx->chiSquared()); 
      }
    }
  }

  StSPtrVecV0Vertex &v0Vtx = event->v0Vertices();

  for (UInt_t k=0; k<v0Vtx.size(); k++) {
    StV0Vertex *v0 = v0Vtx[k];
    if (v0) {
      //m_v_detid->Fill(v0->det_id); 
      m_v_vtxid->Fill(v0->type());
      if (!isnan(double(v0->position().x())))
	m_v_x->Fill(v0->position().x());     
      if (!isnan(double(v0->position().y())))
	m_v_y->Fill(v0->position().y());     
      if (!isnan(double(v0->position().z())))
	m_v_z->Fill(v0->position().z());     
      m_v_pchi2->Fill(v0->chiSquared()); 
    }
  }

  StSPtrVecXiVertex &xiVtx = event->xiVertices();

  for (UInt_t l=0; l<xiVtx.size(); l++) {
    StXiVertex *xi = xiVtx[l];
    if (xi) {
      //m_v_detid->Fill(xi->det_id); 
      m_v_vtxid->Fill(xi->type());
      if (!isnan(double(xi->position().x())))
	m_v_x->Fill(xi->position().x());     
      if (!isnan(double(xi->position().y())))
	m_v_y->Fill(xi->position().y());     
      if (!isnan(double(xi->position().z())))
	m_v_z->Fill(xi->position().z());     
      m_v_pchi2->Fill(xi->chiSquared()); 
    }
  }

  StSPtrVecKinkVertex &kinkVtx = event->kinkVertices();

  for (UInt_t m=0; m<kinkVtx.size(); m++) {
    StKinkVertex *kink = kinkVtx[m];
    if (kink) {
      //m_v_detid->Fill(kink->det_id); 
      m_v_vtxid->Fill(kink->type());
      if (!isnan(double(kink->position().x())))
	m_v_x->Fill(kink->position().x());     
      if (!isnan(double(kink->position().y())))
	m_v_y->Fill(kink->position().y());     
      if (!isnan(double(kink->position().z())))
	m_v_z->Fill(kink->position().z());     
      m_v_pchi2->Fill(kink->chiSquared()); 
    }
  }
  UInt_t cntrows = 0;
  cntrows = event->numberOfPrimaryVertices() + v0Vtx.size() +
            xiVtx.size() + kinkVtx.size(); //this gives 3 less than the DSTs!!
                                           //->needs to be fixed !!!
  m_v_num->Fill(cntrows);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistXi()
{
  if (Debug()) cout << 
   " *** in StEventQAMaker - filling dst_xi_vertex histograms " << endl;

  StSPtrVecXiVertex &xi = event->xiVertices();
  Int_t cntrows=0;
  cntrows = xi.size();
  m_xi_tot->Fill(cntrows);

  Float_t m_lamass2 = (lambda_mass_c2*lambda_mass_c2);
  Float_t m_pimass2 = (pion_minus_mass_c2*pion_minus_mass_c2);

  for (Int_t k=0; k<cntrows; k++) {
    StXiVertex *vertex = xi[k];
    const StThreeVectorF& pMom = vertex->momentumOfBachelor();
    StThreeVectorF lMom = vertex->momentumOfV0();
    StThreeVectorF xMom = lMom + pMom;
    Float_t pP2 = pMom.mag2();
    Float_t pL2 = lMom.mag2();
    Float_t pX2 = xMom.mag2();
    Float_t epi = sqrt(pP2 + m_pimass2);
    Float_t ela = sqrt(pL2 + m_lamass2);
    Float_t eXi = ela + epi;
  }

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPoint() {

  if (Debug()) cout << " *** in StEventQAMaker - filling point histograms " << endl;

  StTpcHitCollection *tpcHits = event->tpcHitCollection();
  StSvtHitCollection *svtHits = event->svtHitCollection();
  StFtpcHitCollection *ftpcHits = event->ftpcHitCollection();
  StSsdHitCollection *ssdHits = event->ssdHitCollection();

  ULong_t totalHits = 0;
  ULong_t ftpcHitsE = 0;
  ULong_t ftpcHitsW = 0;

  if (tpcHits) {
    m_pnt_tpc->Fill(tpcHits->numberOfHits());
    totalHits += tpcHits->numberOfHits();
  }
  if (svtHits) {
    m_pnt_svt->Fill(svtHits->numberOfHits());
    totalHits += svtHits->numberOfHits();
  }
  if (ftpcHits) {
    // it's better to do this with det_id, but need to code -CL
    for (UInt_t i=0; i<10; i++)
      ftpcHitsW += ftpcHits->plane(i)->numberOfHits();
    for (UInt_t j=10; j<20; j++)
      ftpcHitsE += ftpcHits->plane(j)->numberOfHits();

    m_pnt_ftpcW->Fill(ftpcHitsW);
    m_pnt_ftpcE->Fill(ftpcHitsE);
    totalHits += ftpcHitsW + ftpcHitsE;
  }
  if (ssdHits) {
    m_pnt_ssd->Fill(ssdHits->numberOfHits());
    totalHits += ssdHits->numberOfHits();
  }
  m_pnt_tot->Fill(totalHits);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistKink() {

  if (Debug()) cout << " *** in StEventQAMaker - filling kink histograms " << endl;

  StSPtrVecKinkVertex &kink = event->kinkVertices();
  Int_t cntrows=0;
  cntrows = kink.size();
  m_kink_tot->Fill(cntrows);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistL3() {

  if (Debug()) cout << " *** in StEventQAMaker - filling L3 histograms " << endl;

  m_l3_tot->Fill(event->softwareMonitor()->l3()->nTotalTracks);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistV0Eval() {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling ev0_eval histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL
  // This table does not seem to exist in the new StEvent. -CL

  St_DataSetIter dstI(dst);           

  St_ev0_eval *pt = (St_ev0_eval*) dstI["ev0_eval"];
  if (pt) {
    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_v0eval_tot->Fill(cntrows);
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistRich() {

  if (Debug()) cout << " *** in StEventQAMaker - filling Rich histograms " << endl;

  m_rich_tot->Fill(event->softwareMonitor()->rich()->mult_rich_tot);
}

//_____________________________________________________________________________
