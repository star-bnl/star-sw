//  
//  
//
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// StEventQAMaker class  - reads from StEvent and fills histograms           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "SystemOfUnits.h"
#include "StEventQAMaker.h"
#include "StChain.h"
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
  
 // Call methods to fill histograms
  StEvent *event = (StEvent *)GetInputDS("StEvent");
  
  // histograms from table event_summary
  MakeHistEvSum(event);
  // histograms from table globtrk
  MakeHistGlob(event);
  // histograms from table dst_dedx
  MakeHistDE(event);
  // histograms from table primtrk
  MakeHistPrim(event);
  // histograms from table particle
  MakeHistGen(event);  
  // histograms from table dst_v0_vertex
  MakeHistV0(event);
  // histograms from table primtrk & dst_dedx
  MakeHistPID(event);
  // histograms from table dst_vertex
  MakeHistVertex(event);
  // histograms from table dst_xi_vertex
  MakeHistXi(event);
  // histograms from table point
  MakeHistPoint(event);
  // histograms from table kinkVertex
  MakeHistKink(event);
  // histograms from table l3Track
  MakeHistL3(event);
  // histograms from table ev0_eval
  MakeHistV0Eval(event);
  // histograms from table g2t_rch_hit
  MakeHistRich(event);
  
  return kStOK;
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistEvSum(StEvent *event) {
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

    m_mean_pt->Fill(event_summary->meanPt());
    m_mean_eta->Fill(event_summary->meanEta());
    m_rms_eta->Fill(event_summary->rmsEta());

    if(!isnan((double)(event_summary->primaryVertexPosition()[0])))
      m_prim_vrtx0->Fill(event_summary->primaryVertexPosition()[0]);
    if(!isnan((double)(event_summary->primaryVertexPosition()[1])))
      m_prim_vrtx1->Fill(event_summary->primaryVertexPosition()[1]);
    if(!isnan((double)(event_summary->primaryVertexPosition()[2])))
      m_prim_vrtx2->Fill(event_summary->primaryVertexPosition()[2]);

// not in 99i tables
    m_glb_trk_prim->Fill(event_summary->numberOfGoodPrimaryTracks());
//      m_T_average->Fill(tt->T_average);    
//      m_vert_V0->Fill(tt->n_vert_V0);
//      m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 

  }
}

//-----------------------------------------------------------------
void StEventQAMaker::MakeHistGlob(StEvent *event) {

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
      pT = TMath::Abs(globtrk->geometry()->momentum().perp());
      Float_t lmevpt = TMath::Log10(pT*1000.0);
      Float_t theta = TMath::ASin(1.) - globtrk->geometry()->dipAngle();
      Float_t eta = globtrk->geometry()->momentum().pseudoRapidity();
      //      Float_t gmom  = pT/TMath::Sin(theta);
      Float_t gmom = abs(globtrk->geometry()->momentum());
      Float_t lmevmom = TMath::Log10(gmom*1000.0);
      Float_t chisq0 = globtrk->fitTraits().chi2(0);
      Float_t chisq1 = globtrk->fitTraits().chi2(1);
      Float_t nfitntot = (Float_t(globtrk->fitTraits().numberOfFitPoints())) /
	                 (Float_t(globtrk->detectorInfo()->numberOfPoints()));
      Float_t xdif = (globtrk->detectorInfo()->firstPoint().x()) -
	             (globtrk->geometry()->origin().x());
      Float_t ydif = (globtrk->detectorInfo()->firstPoint().y()) -
	             (globtrk->geometry()->origin().y());
      Float_t zdif = (globtrk->detectorInfo()->firstPoint().z()) -
	             (globtrk->geometry()->origin().z());
      Float_t radf = TMath::Power((globtrk->detectorInfo()->firstPoint().x()),
				  2) +
	             TMath::Power((globtrk->detectorInfo()->firstPoint().y()),
				  2);
      radf = TMath::Sqrt(radf);

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here (juts plotted)

      for (UInt_t k=0; k<globtrk->pidTraits().size(); k++)
	m_det_id->Fill(globtrk->pidTraits()[k]->detector());
      m_point->Fill(globtrk->detectorInfo()->numberOfPoints());
      m_max_point->Fill(globtrk->numberOfPossiblePoints());
      m_fit_point->Fill(globtrk->fitTraits().numberOfFitPoints());
      m_glb_charge->Fill(globtrk->geometry()->charge());
      m_glb_xf->Fill(globtrk->detectorInfo()->firstPoint().x());
      m_glb_yf->Fill(globtrk->detectorInfo()->firstPoint().y());
      m_glb_zf->Fill(globtrk->detectorInfo()->firstPoint().z());
      m_glb_xf0->Fill(xdif);
      m_glb_yf0->Fill(ydif);
      m_glb_zf0->Fill(zdif);
      m_glb_radf->Fill(radf);
      m_glb_ratio->Fill(nfitntot);
        
      //originally t->psi... but psi()=t->psi*degree in StEvent -CL
      m_psi->Fill(globtrk->geometry()->psi());

      //originally was t->tanl -CL
      m_tanl->Fill(TMath::Tan(globtrk->geometry()->dipAngle()));
      m_glb_theta->Fill(theta);
      m_eta->Fill(eta);
      m_pT->Fill(pT);
      m_mom->Fill(gmom);
      m_length->Fill(globtrk->length());
      m_glb_impact->Fill(globtrk->impactParameter());

      m_chisq0->Fill(chisq0);
      m_chisq1->Fill(chisq1);

      m_pT_eta_rec->Fill(eta,lmevpt);
      m_globtrk_xf_yf->Fill(globtrk->detectorInfo()->firstPoint().x(),
			    globtrk->detectorInfo()->firstPoint().y());
      m_tanl_zf->Fill(globtrk->detectorInfo()->firstPoint().z(),
		      Float_t(TMath::Tan(globtrk->geometry()->dipAngle())));
      m_mom_trklength->Fill(globtrk->length(),lmevmom);
      m_eta_trklength->Fill(eta,globtrk->length());
      m_npoint_length->Fill(globtrk->length(),
			    Float_t(globtrk->detectorInfo()->numberOfPoints()));
      m_fpoint_length->Fill(globtrk->length(),
			    Float_t(globtrk->fitTraits().numberOfFitPoints()));
      m_chisq0_mom->Fill(lmevmom,chisq0);
      m_chisq1_mom->Fill(lmevmom,chisq1);
      m_chisq0_eta->Fill(eta,chisq0);
      m_chisq1_eta->Fill(eta,chisq1);
      m_chisq0_dip->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq0);
      m_chisq1_dip->Fill(TMath::Tan(globtrk->geometry()->dipAngle()),chisq1);
      m_chisq0_zf->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq0);
      m_chisq1_zf->Fill(globtrk->detectorInfo()->firstPoint().z(),chisq1);
      m_nfptonpt_mom->Fill(lmevmom,nfitntot);
      m_nfptonpt_eta->Fill(eta,nfitntot);
    }
  }
  m_globtrk_tot->Fill(cnttrk);
  m_globtrk_good->Fill(cnttrkg);
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistDE(StEvent *event) {
  // Fill histograms for dE/dx
/*
  St_DataSetIter dstI(dst);
  
  St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];

  if(dst_dedx) {

    Int_t cntrows=0;
    cntrows = dst_dedx->GetNRows();
    m_ndedxr->Fill(cntrows);

    dst_dedx_st *d = dst_dedx->GetTable();
    for (Int_t i = 0; i < dst_dedx->GetNRows(); i++,d++) {
      m_ndedx->Fill(d->ndedx);
      m_dedx0->Fill(d->dedx[0]*1e6);
      m_dedx1->Fill(d->dedx[1]*1e6);
    }
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPrim(StEvent *event) {

  StPrimaryVertex *primVtx = event->primaryVertex();

  if (primVtx) {
    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = primVtx->numberOfDaughters();
    m_primtrk_tot->Fill(cnttrk);

    for (UInt_t i=0; i<primVtx->numberOfDaughters(); i++) {
      StTrack *primtrk = primVtx->daughter(i);
      m_primtrk_iflag->Fill(primtrk->flag());

      if (primtrk->flag()>0) {
        cnttrkg++;
	Float_t pT = -999.;
	pT = TMath::Abs(primtrk->geometry()->momentum().perp());
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - primtrk->geometry()->dipAngle();
	Float_t eta   = primtrk->geometry()->momentum().pseudoRapidity();
	//	Float_t gmom  = pT/TMath::Sin(theta);
	Float_t gmom = abs(primtrk->geometry()->momentum());
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = primtrk->fitTraits().chi2(0);
	Float_t chisq1 = primtrk->fitTraits().chi2(1);
        Float_t nfitntot = (Float_t(primtrk->fitTraits().numberOfFitPoints()))/
	                   (Float_t(primtrk->detectorInfo()->numberOfPoints()));
        Float_t xdif = (primtrk->detectorInfo()->firstPoint().x()) -
	               (primtrk->geometry()->origin().x());
        Float_t ydif = (primtrk->detectorInfo()->firstPoint().y()) -
	               (primtrk->geometry()->origin().y());
        Float_t zdif = (primtrk->detectorInfo()->firstPoint().z()) -
	               (primtrk->geometry()->origin().z());
        Float_t radf = TMath::Power((primtrk->detectorInfo()->firstPoint().x())
				    ,2) + 
                       TMath::Power((primtrk->detectorInfo()->firstPoint().y())
				    ,2);
	radf = TMath::Sqrt(radf); 

	for (UInt_t k=0; k<primtrk->pidTraits().size(); k++)
	  m_pdet_id->Fill(primtrk->pidTraits()[k]->detector());
	m_ppoint->Fill(primtrk->detectorInfo()->numberOfPoints());
	m_pmax_point->Fill(primtrk->numberOfPossiblePoints());
	m_pfit_point->Fill(primtrk->fitTraits().numberOfFitPoints());
        m_prim_charge->Fill(primtrk->geometry()->charge());
        m_prim_xf->Fill(primtrk->detectorInfo()->firstPoint().x());
        m_prim_yf->Fill(primtrk->detectorInfo()->firstPoint().y());
        m_prim_zf->Fill(primtrk->detectorInfo()->firstPoint().z());
        m_prim_xf0->Fill(xdif);
        m_prim_yf0->Fill(ydif);
        m_prim_zf0->Fill(zdif);
        m_prim_radf->Fill(radf);
        m_prim_ratio->Fill(nfitntot);

	// originally t->psi... but psi()=t->psi*degree in StEvent -CL
	m_ppsi->Fill(primtrk->geometry()->psi());

	// originally was t->tanl -CL
        m_ptanl->Fill(TMath::Tan(primtrk->geometry()->dipAngle()));
        m_prim_theta->Fill(theta);
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
void StEventQAMaker::MakeHistGen(StEvent *event) {

/*
  if (Debug()) cout << " *** in StEventQAMaker - filling particle histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  St_DataSetIter dstI(dst);
  
  St_particle   *part     = (St_particle  *) dstI["particle"];
  if (!part) part = (St_particle  *) DataSet("geant/particle");
  if (part){
    particle_st *p = part->GetTable();
    Int_t nchgpart=0;
    Int_t totpart=0;
    for (Int_t l=0; l < part->GetNRows(); l++, p++){
      //
      //  select only particles which can be detected
      //  in the STAR detector. Here we restrict us to/
      //  the most common species.
      //
      if(l!=0){                        // first row of table is header, so skip it!
	if (p->isthep == 1) {            // select good status only
	  totpart++;
	  if (TMath::Abs(p->idhep) == 11   ||       // electrons
	      TMath::Abs(p->idhep) == 13   ||       // muon
	      TMath::Abs(p->idhep) == 211  ||       // pion
	      TMath::Abs(p->idhep) == 321  ||       // kaon
	      TMath::Abs(p->idhep) == 2212) {       // proton/
	    
	    nchgpart++;	    
	    Double_t px = p->phep[0];
	    Double_t py = p->phep[1];
	    Double_t pz = p->phep[2];
	    Double_t pT    =  TMath::Sqrt(px*px+py*py);
	    Double_t theta =  TMath::ATan2( pT, pz );
	    Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
	    m_H_pT_eta_gen->Fill(eta, (Float_t) pT);
	    m_H_pT_gen->Fill((Float_t) pT);
	    m_H_eta_gen->Fill(eta);
	    m_H_vtxx->Fill(p->vhep[0]);
	    m_H_vtxy->Fill(p->vhep[1]);
	    m_H_vtxz->Fill(p->vhep[2]);
	  }
	}
      }
    }
    m_H_npart->Fill(totpart);
    m_H_ncpart->Fill(nchgpart);
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistV0(StEvent *event) {

  if (Debug()) cout << " *** in StEventQAMaker - filling dst_v0_vertex histograms " << endl;

  StSPtrVecV0Vertex &v0vertices = event->v0Vertices();

  if (v0vertices.size() > 0) {
    Int_t cntrows=0;
    cntrows = v0vertices.size();  //Is there a better way of doing this? -CL
    m_v0->Fill(cntrows);

    Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
    Float_t m_pimass2 = (0.139567*0.139567);

    for (UInt_t k=0; k<v0vertices.size(); k++) {
      StV0Vertex *v0 = v0vertices[k];
      cntrows += v0->numberOfDaughters();

      Float_t e1a = abs(v0->momentumOfDaughter(positive));
      Float_t e2 = abs(v0->momentumOfDaughter(negative));
      Float_t e1 = e1a + m_prmass2;
      e2 += m_pimass2;
      e1 = TMath::Sqrt(e1);
      e2 = TMath::Sqrt(e2);
      Float_t p = abs(v0->momentum());
      Float_t inv_mass_la = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      e1 = e1a + m_pimass2;
      e1 = TMath::Sqrt(e1);
      Float_t inv_mass_k0 = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      m_ev0_lama_hist->Fill(inv_mass_la);
      m_ev0_k0ma_hist->Fill(inv_mass_k0);   
    }
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPID(StEvent *event) {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling PID histograms " << endl;
  
  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

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
void StEventQAMaker::MakeHistVertex(StEvent *event) {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling vertex histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  //Which set of vertices should I look at here? Prim,V0,Xi,Kink, or ALL? -CL
  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];
  
  if (vertex) {

    Int_t cntrows=0;
    cntrows = vertex->GetNRows();
    m_v_num->Fill(cntrows);

    dst_vertex_st  *t   = vertex->GetTable();
    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      //         if (t->iflag>0) {  
      if (i==0){                           // plot of primary vertex only
	m_pv_detid->Fill(t->det_id); 
	m_pv_vtxid->Fill(t->vtx_id);
	if (!isnan(double(t->x))) m_pv_x->Fill(t->x);     
	if (!isnan(double(t->y))) m_pv_y->Fill(t->y);     
	if (!isnan(double(t->z))) m_pv_z->Fill(t->z);     
	m_pv_pchi2->Fill(t->chisq[0]);
      }
      m_v_detid->Fill(t->det_id); 
      m_v_vtxid->Fill(t->vtx_id);
      if (!isnan(double(t->x))) m_v_x->Fill(t->x);     
      if (!isnan(double(t->y))) m_v_y->Fill(t->y);     
      if (!isnan(double(t->z))) m_v_z->Fill(t->z);     
      m_v_pchi2->Fill(t->chisq[0]); 
      // }
    }
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistXi(StEvent *event) {

  if (Debug()) cout << " *** in StEventQAMaker - filling dst_xi_vertex histograms " << endl;

  StSPtrVecXiVertex &xi = event->xiVertices();
  if (xi.size() > 0) {
    Int_t cntrows=0;
    cntrows = xi.size();  //Is there a better way of getting this? -CL
    m_xi_tot->Fill(cntrows);
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPoint(StEvent *event) {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling point histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  St_DataSetIter dstI(dst);           

  St_dst_point *pt = (St_dst_point*) dstI["point"];
  if (pt) {
    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_pnt_tot->Fill(cntrows);
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistKink(StEvent *event) {

  if (Debug()) cout << " *** in StEventQAMaker - filling kink histograms " << endl;

  StSPtrVecKinkVertex &kink = event->kinkVertices();
  if (kink.size() > 0) {
    Int_t cntrows=0;
    cntrows = kink.size();   // Is there a better way of doing this? -CL
    m_kink_tot->Fill(cntrows);
  }
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistL3(StEvent *event) {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling L3 histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL

  St_DataSetIter dstI(dst);           

  St_tpt_track *pt = (St_tpt_track*) dstI["l3Track"];
  if (pt) {
    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_l3_tot->Fill(cntrows);
  }
*/
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistV0Eval(StEvent *event) {
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
void StEventQAMaker::MakeHistRich(StEvent *event) {
/*
  if (Debug()) cout << " *** in StEventQAMaker - filling Rich histograms " << endl;

  // THIS IS NOT FINISHED AND WILL NOT COMPILE YET! -CL
  // This table does not seem to exist in the new StEvent. -CL

  St_DataSetIter dstI(dst);           

  St_g2t_rch_hit *pt = (St_g2t_rch_hit*) dstI["g2t_rch_hit"];
  if (pt) {
    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_rich_tot->Fill(cntrows);
  }
*/
}

//_____________________________________________________________________________
