// $Id: St_QA_Maker.cxx,v 2.5 2001/04/25 21:35:26 genevb Exp $
// $Log: St_QA_Maker.cxx,v $
// Revision 2.5  2001/04/25 21:35:26  genevb
// Added V0 phi distributions
//
// Revision 2.4  2001/04/24 22:53:51  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.3  2001/04/24 19:59:08  genevb
// Use det_id to identify detectors
//
// Revision 2.2  2000/09/08 18:55:54  lansdell
// turned on FTPC primary track histograms
//
// Revision 2.1  2000/09/01 16:59:03  genevb
// Change for V0 plots
//
// Revision 2.0  2000/08/25 16:02:41  genevb
// New revision: new structure, multiplicity classes
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  St_QA_Maker class for QA Histograms using dst tables                 //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "SystemOfUnits.h"
#include "TH1.h"
#include "TH2.h"
#include "St_DataSetIter.h"
#include "St_QA_Maker.h"
#include "StQABookHist.h"
#include "StDetectorId.h"

#include "StVertexId.h"
#include "StDetectorDefinitions.h"

// tables  on DST
#include "tables/St_dst_event_summary_Table.h" // event_summary (1 row)
#include "tables/St_event_header_Table.h"  // event_header (1 row)
#include "tables/St_dst_track_Table.h"         // 3 tables: globtrk,globtrk2,primtrk
#include "tables/St_dst_vertex_Table.h"        // vertex
#include "tables/St_dst_point_Table.h"         // point
#include "tables/St_dst_v0_vertex_Table.h"     // dst_v0_vertex
#include "tables/St_dst_xi_vertex_Table.h"     // dst_xi_vertex
#include "tables/St_dst_dedx_Table.h"          // dst_dedx
#include "tables/St_dst_TrgDet_Table.h"        // TrgDet (1 row)
#include "tables/St_dst_tkf_vertex_Table.h"    // kinkVertex
#include "tables/St_particle_Table.h"          // particle
#include "tables/St_g2t_rch_hit_Table.h"       // g2t_rch_hit
#include "tables/St_tpt_track_Table.h"         // l3Track

#include "tables/St_dst_mon_soft_tpc_Table.h"  // TPC software monitor

// tables  from geant
#include "tables/St_g2t_vertex_Table.h"


// Spiros added following line on 10jan00
// this routine is from pams/global/egr
extern "C" {float prop_one_track( float * ,  float * , float * );}


ClassImp(St_QA_Maker)
  
//_____________________________________________________________________________
St_QA_Maker::St_QA_Maker(const char *name, const char *title) :
 StQAMakerBase(name,title,"Tab"){

}
//_____________________________________________________________________________

Int_t St_QA_Maker::Finish() {

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Init(){
// St_QA_Maker - Init; book histograms and set defaults for member functions

  return StQAMakerBase::Init();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Make(){
// St_QA_Maker - Make; fill histograms
    
  dst = GetDataSet("dst");
  bool foundPrimVtx = kFALSE;

  if (dst) {
    St_DataSetIter dstI(dst);           
    if (firstEvent) {
      St_event_header* evHeader = (St_event_header*) dstI["event_header"];
      if (evHeader) {
        event_header_st* evh = evHeader->GetTable();
        if (evh) {
          if (!strcmp(evh->event_type,"NONE")) {
            histsSet = 1;
          } else {
            // process Monte Carlo events
            histsSet = 0;
          }
        }
      }
      BookHist();
    }
    // check that primary vertex exists !!!
    St_dst_vertex *vertex = (St_dst_vertex *) dstI["vertex"];
    if (vertex) {
      dst_vertex_st *tt = vertex->GetTable();
      for (Int_t i=0; i<vertex->GetNRows(); i++, tt++) {
	if (tt->iflag==1 && tt->vtx_id==kEventVtxId) {
	  foundPrimVtx = kTRUE;
	  mNullPrimVtx->Fill(1);
	  St_dst_track* gtracks = (St_dst_track*) dstI["globtrk"];
	  multiplicity = gtracks->GetNRows();
	  return StQAMakerBase::Make();
	}
      }
    }
  }
  if (foundPrimVtx == kFALSE) {
    cout << "Error in St_QA_Maker::Make(): no primary vertex found!" << endl;
    mNullPrimVtx->Fill(-1);
    return kStOk;
  }
  else {
    cout << "Error in St_QA_Maker::Make(): no dst dataset found!" << endl;
    return kStWarn;
  }
}
//_____________________________________________________________________________

void St_QA_Maker::MakeHistEvSum(){
  //  PrintInfo();
  // Fill histograms for event summary
  St_DataSetIter dstI(dst);         
  
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dstI["event_summary"];
  if (event_summary) {
    dst_event_summary_st  *tt = event_summary->GetTable();

    for (Int_t j = 0; j < event_summary->GetNRows(); j++,tt++) {
      Float_t trk_tot =   tt->glb_trk_tot;
      Float_t trk_good =  tt->glb_trk_good;
      Float_t trk_plus =  tt->glb_trk_plus;
      Float_t trk_minus = tt->glb_trk_minus;

      m_trk_tot_gd->Fill(trk_good/trk_tot,multClass); 
      m_glb_trk_tot->Fill(tt->glb_trk_tot,multClass);
      m_glb_trk_tot_sm->Fill(tt->glb_trk_tot,multClass);
      m_glb_trk_plusminus->Fill(trk_plus/trk_minus,multClass);
      m_glb_trk_plusminus_sm->Fill(trk_plus/trk_minus,multClass);
      m_glb_trk_prim->Fill(tt->glb_trk_prim,multClass);
      m_glb_trk_prim_sm->Fill(tt->glb_trk_prim,multClass);
      m_vert_total->Fill(tt->n_vert_total,multClass);
      m_vert_total_sm->Fill(tt->n_vert_total,multClass);
      m_mean_pt->Fill(tt->mean_pt,multClass);
      m_mean_pt_sm->Fill(tt->mean_pt,multClass);
      m_mean_eta->Fill(tt->mean_eta,multClass);
      m_rms_eta->Fill(tt->rms_eta,multClass);

      if(!isnan((double)(tt->prim_vrtx[0])) &&
	 !isnan((double)(tt->prim_vrtx[1])) &&
	 !isnan((double)(tt->prim_vrtx[2]))) {
	Float_t radius = sqrt(tt->prim_vrtx[0]*tt->prim_vrtx[0]+
			      tt->prim_vrtx[1]*tt->prim_vrtx[1]);
        m_prim_vrtr->Fill(radius,multClass);
        m_prim_vrtx0->Fill(tt->prim_vrtx[0],multClass);
        m_prim_vrtx1->Fill(tt->prim_vrtx[1],multClass);
        m_prim_vrtx2->Fill(tt->prim_vrtx[2],multClass);
      }
    }
  }

  St_dst_mon_soft_tpc *tpcSoftMon = (St_dst_mon_soft_tpc *) dstI["mon_soft_tpc"];
  if (tpcSoftMon) {
    dst_mon_soft_tpc_st *t = tpcSoftMon->GetTable();
    Float_t tpcChgWest=0;
    Float_t tpcChgEast=0;
    for (int i=0; i<24; i++) {
      if (i<12)
	tpcChgWest += t->chrg_tpc_in[i]+t->chrg_tpc_out[i];
      else
	tpcChgEast += t->chrg_tpc_in[i]+t->chrg_tpc_out[i];
    }
    m_glb_trk_chg->Fill(tpcChgEast/tpcChgWest,multClass);
  }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeHistGlob(){

  St_DataSetIter dstI(dst);           

  Float_t primVtxZ = 0;
  St_dst_vertex *vertex = (St_dst_vertex *) dstI["vertex"];
  if (vertex) {
    dst_vertex_st *tt = vertex->GetTable();
    for (Int_t i=0; i<vertex->GetNRows(); i++, tt++)
      if (tt->iflag==1 && tt->vtx_id==kEventVtxId)
	if (!isnan(double(tt->z)))
	  primVtxZ = tt->z;
  }

  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    dst_track_st  *t   = globtrk->GetTable();

    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = globtrk->GetNRows();
    hists->m_globtrk_tot->Fill(cnttrk);
    hists->m_globtrk_tot_sm->Fill(cnttrk);

    for (Int_t i = 0; i < globtrk->GetNRows(); i++,t++){

      hists->m_globtrk_iflag->Fill(t->iflag);

      if (t->iflag>0) {
        cnttrkg++;
// n_point,n_fit_point,n_max_point is packed on dst tables:
//   n_point = 1*tpc_pnt + 1000*svt_pnt + 10000*ssd_pnt
//   - must unpack and add together to get total #pnt
       Int_t ssdpnt = 0;
       Int_t  hold1 = 0;
       Int_t svtpnt = 0;
       Int_t tpcpnt = 0;
       Int_t trkpnt = 0;

       Int_t ssdfpnt = 0;
       Int_t  holdf1 = 0;
       Int_t svtfpnt = 0;
       Int_t tpcfpnt = 0;
       Int_t trkfpnt = 0;

       Int_t ssdmpnt = 0;
       Int_t  holdm1 = 0;
       Int_t svtmpnt = 0;
       Int_t tpcmpnt = 0;
       Int_t trkmpnt = 0;

 
        ssdpnt = (t->n_point)/10000;
         hold1 = ((t->n_point)%10000);
        svtpnt = hold1/1000;
        tpcpnt = (hold1%1000);
        trkpnt = tpcpnt+svtpnt+ssdpnt;

        ssdfpnt = (t->n_fit_point)/10000;
         holdf1 = ((t->n_fit_point)%10000);
        svtfpnt = holdf1/1000;
        tpcfpnt = (holdf1%1000);
        trkfpnt = tpcfpnt+svtfpnt+ssdfpnt;

        ssdmpnt = (t->n_max_point)/10000;
         holdm1 = ((t->n_max_point)%10000);
        svtmpnt = holdm1/1000;
        tpcmpnt = (holdm1%1000);
        trkmpnt = tpcmpnt+svtmpnt+ssdmpnt;

	//if (cnttrkg<2){
        //  cout << "   trk # " << i << endl;
        //  cout << " n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
        //                 " n_max = " << t->n_max_point << endl;
        //  cout << "  iflag = " << t->iflag << " det-id = " << t->det_id << endl;
        //  cout << "    #ssd   =  " << ssdpnt <<
	//             " #hold1 =  " << hold1  <<         
	//             " #svt   =  " << svtpnt <<
        //             " #tpc   =  " << tpcpnt <<
	// 	     " #tot   =  " << trkpnt << endl; 
        //  cout << "    #ssdf   = " << ssdfpnt <<
	//             " #hold1f = " << holdf1  <<         
	//             " #svtf   = " << svtfpnt <<
        //             " #tpcf   = " << tpcfpnt <<
	// 	     " #totf   = " << trkfpnt << endl; 
        //  cout << "    #ssdm   = " << ssdmpnt <<
	//             " #hold1m = " << holdm1  <<         
	//             " #svtm   = " << svtmpnt <<
        //             " #tpcm   = " << tpcmpnt <<
	// 	     " #totm   = " << trkmpnt << endl; 
	//	}

	//if (t->n_point<0){
        //  cout << "   trk # " << i << endl;
        //  cout << "!!! n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
        //                 " n_max = " << t->n_max_point << endl;
        //  cout << "  iflag = " << t->iflag << " det-id = " << t->det_id << endl;
        //  cout << "    #ssd    = " << ssdpnt <<
	//             " #hold1  = " << hold1  <<         
	//             " #ssd    = " << ssdpnt <<
	//             " #svt    = " << svtpnt <<
        //             " #tpc    = " << tpcpnt <<
	// 	     " #tot    = " << trkpnt << endl; 
        //  cout << "    #ssdf   = " << ssdfpnt <<
	//             " #hold1f = " << holdf1  <<         
	//             " #svtf   = " << svtfpnt <<
        //             " #tpcf   = " << tpcfpnt <<
	// 	     " #totf   = " << trkfpnt << endl; 
        //  cout << "    #ssdm   = " << ssdmpnt <<
	//             " #hold1m = " << holdm1  <<         
	//             " #svtm   = " << svtmpnt <<
        //             " #tpcm   = " << tpcmpnt <<
	// 	     " #totm   = " << trkmpnt << endl; 
	//       	}

	//if (t->n_point<1000 && cnttrkg<50){
        // cout << " trk # " << i << endl;
        // cout << "ccc n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
        //                 " n_max = " << t->n_max_point << endl;
        //  cout << "  iflag = " << t->iflag << " det-id = " << t->det_id << endl;
          //cout << "    #ssd    = " << ssdpnt <<
       	  //           " #hold1  = " << hold1  <<         
       	  //           " #ssd    = " << ssdpnt <<
       	  //           " #svt    = " << svtpnt <<
          //           " #tpc    = " << tpcpnt <<
	  // 	     " #tot    = " << trkpnt << endl; 
          //cout << "    #ssdf   = " << ssdfpnt <<
       	  //           " #hold1f = " << holdf1  <<         
       	  //           " #svtf   = " << svtfpnt <<
          //           " #tpcf   = " << tpcfpnt <<
	  // 	     " #totf   = " << trkfpnt << endl; 
          //cout << "    #ssdm   = " << ssdmpnt <<
       	  //           " #hold1m = " << holdm1  <<         
       	  //           " #svtm   = " << svtmpnt <<
          //           " #tpcm   = " << tpcmpnt <<
	  // 	     " #totm   = " << trkmpnt << endl; 
	//}


    	//if (t->n_point < t->n_fit_point){
        //  cout << "   trk # " << i << endl;
        //  cout << "*** n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
        //                 " n_max = " << t->n_max_point << endl;
        //  cout << "  iflag = " << t->iflag << " det-id = " << t->det_id << endl;
        //  cout << "    #ssd    = " << ssdpnt <<
	//            " #hold1  = " << hold1  <<         
	//             " #ssd    = " << ssdpnt <<
	//             " #svt    = " << svtpnt <<
        //             " #tpc    = " << tpcpnt <<
	// 	     " #tot    = " << trkpnt << endl; 
        //  cout << "    #ssdf   = " << ssdfpnt <<
	//             " #hold1f = " << holdf1  <<         
	//             " #svtf   = " << svtfpnt <<
        //             " #tpcf   = " << tpcfpnt <<
	// 	     " #totf   = " << trkfpnt << endl; 
        //  cout << "    #ssdm   = " << ssdmpnt <<
	//             " #hold1m = " << holdm1  <<         
	//             " #svtm   = " << svtmpnt <<
        //             " #tpcm   = " << tpcmpnt <<
	// 	     " #totm   = " << trkmpnt << endl; 
	//	}

	//if (trkfpnt > 55){
        //  cout << "   trk # " << i << endl;
        //  cout << "*** n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
        //                 " n_max = " << t->n_max_point << endl;
        //  cout << "  iflag = " << t->iflag << " det-id = " << t->det_id << endl;
        //  cout << "    #ssd    = " << ssdpnt <<
	//            " #hold1  = " << hold1  <<         
	//             " #ssd    = " << ssdpnt <<
	//             " #svt    = " << svtpnt <<
        //             " #tpc    = " << tpcpnt <<
	// 	     " #tot    = " << trkpnt << endl; 
        //  cout << "    #ssdf   = " << ssdfpnt <<
	//             " #hold1f = " << holdf1  <<         
	//             " #svtf   = " << svtfpnt <<
        //             " #tpcf   = " << tpcfpnt <<
	// 	     " #totf   = " << trkfpnt << endl; 
        //  cout << "    #ssdm   = " << ssdmpnt <<
	//             " #hold1m = " << holdm1  <<         
	//             " #svtm   = " << svtmpnt <<
        //             " #tpcm   = " << tpcmpnt <<
	// 	     " #totm   = " << trkmpnt << endl; 
	//			}


	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta  = TMath::ASin(1.) - TMath::ATan(t->tanl);
        Float_t thetad = theta *(360./twopi);
	Float_t eta    =-TMath::Log(TMath::Tan(theta/2.));
	Float_t gmom   = pT/TMath::Sin(theta);
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
        Float_t nfitntot = (Float_t(trkfpnt))/(Float_t(trkpnt));
        Float_t nfitnmax = (Float_t(trkfpnt))/(Float_t(trkmpnt));
        Float_t x0s  =  t->r0 * TMath::Cos(t->phi0*degree);
        Float_t y0s  =  t->r0 * TMath::Sin(t->phi0*degree);
        Float_t xdif =  (t->x_first[0])-x0s;
        Float_t ydif =  (t->x_first[1])-y0s;
        Float_t zdif =  (t->x_first[2])-(t->z0);
        Float_t radf = TMath::Power((t->x_first[0]),2) + 
                       TMath::Power((t->x_first[1]),2);
                radf = TMath::Sqrt(radf); 

        Float_t logImpact = TMath::Log10(t->impact); 
        Float_t logCurvature = TMath::Log10(t->curvature); 


// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 
		
         hists->m_det_id->Fill(t->det_id);

        switch (t->det_id) {
//  now fill all TPC histograms ------------------------------------------------
        case (kTpcId) : {

// these are tpc only
        hists->m_glb_f0->Fill(xdif,0.);
        hists->m_glb_f0->Fill(ydif,1.);
        hists->m_glb_f0->Fill(zdif,2.);

        hists->m_glb_xf0->Fill(xdif);
        hists->m_glb_yf0->Fill(ydif);
        hists->m_glb_zf0->Fill(zdif);
        hists->m_glb_impactT->Fill(logImpact);
        hists->m_glb_impactrT->Fill(t->impact);

	// need padrow histogram -CPL
	
// these are tpc & ftpc
        hists->m_pointT->Fill(trkpnt);
        hists->m_max_pointT->Fill(trkmpnt);
        hists->m_fit_pointT->Fill(trkfpnt);
        hists->m_glb_chargeT->Fill(t->icharge);
        hists->m_glb_r0T->Fill(t->r0);
        hists->m_glb_phi0T->Fill(t->phi0);
        hists->m_glb_z0T->Fill(t->z0);
        hists->m_glb_curvT->Fill(logCurvature);
        hists->m_glb_xfT->Fill(t->x_first[0]);
        hists->m_glb_yfT->Fill(t->x_first[1]);
        hists->m_glb_zfT->Fill(t->x_first[2]);
        hists->m_glb_radfT->Fill(radf);
        hists->m_glb_ratioT->Fill(nfitntot);
        hists->m_glb_ratiomT->Fill(nfitnmax);
        hists->m_psiT->Fill(t->psi);
        hists->m_tanlT->Fill(t->tanl);
        hists->m_glb_thetaT->Fill(thetad);
        hists->m_etaT->Fill(eta);
        hists->m_pTT->Fill(pT);
        hists->m_momT->Fill(gmom);
        hists->m_lengthT->Fill(t->length);
        hists->m_chisq0T->Fill(chisq0);
        hists->m_chisq1T->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_globtrk_xf_yfT->Fill(t->x_first[0],t->x_first[1]);
        hists->m_eta_trklengthT->Fill(eta,t->length);
        hists->m_npoint_lengthT->Fill(t->length,Float_t(trkpnt));
        hists->m_fpoint_lengthT->Fill(t->length,Float_t(trkfpnt));
	
// these are tpc only
        hists->m_pT_eta_recT->Fill(eta,lmevpt);
        hists->m_tanl_zfT->Fill(t->x_first[2]-primVtxZ,t->tanl);
        hists->m_mom_trklengthT->Fill(t->length,lmevmom);
        hists->m_chisq0_momT->Fill(lmevmom,chisq0);
        hists->m_chisq1_momT->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaT->Fill(eta,chisq0);
        hists->m_chisq1_etaT->Fill(eta,chisq1);
        hists->m_chisq0_dipT->Fill(t->tanl,chisq0);
        hists->m_chisq1_dipT->Fill(t->tanl,chisq1);
        hists->m_chisq0_zfT->Fill(t->x_first[2],chisq0);
        hists->m_chisq1_zfT->Fill(t->x_first[2],chisq1);
        hists->m_chisq0_phiT->Fill(t->phi0,chisq0);
        hists->m_nfptonpt_momT->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaT->Fill(eta,nfitntot);
        hists->m_psi_phiT->Fill(t->phi0,t->psi);
        break; }


//  now fill all TPC+SVT histograms ------------------------------------------------
        case (kSvtId) :
        case (kTpcSsdId) :
        case (kTpcSvtId) :
        case (kTpcSsdSvtId) :
        case (kSsdSvtId) : {

	// use multihist class StMultiH1F
        hists->m_glb_f0TS->Fill(xdif,0);
        hists->m_glb_f0TS->Fill(ydif,1);
        hists->m_glb_f0TS->Fill(zdif,2);

        hists->m_glb_xf0TS->Fill(xdif);
        hists->m_glb_yf0TS->Fill(ydif);
        hists->m_glb_zf0TS->Fill(zdif);
        hists->m_glb_impactTS->Fill(logImpact);
        hists->m_glb_impactrTS->Fill(t->impact);
	
        hists->m_pointTS->Fill(trkpnt);
        hists->m_max_pointTS->Fill(trkmpnt);
        hists->m_fit_pointTS->Fill(trkfpnt);
        hists->m_glb_chargeTS->Fill(t->icharge);
        hists->m_glb_r0TS->Fill(t->r0);
        hists->m_glb_phi0TS->Fill(t->phi0);
        hists->m_glb_z0TS->Fill(t->z0);
        hists->m_glb_curvTS->Fill(logCurvature);
        hists->m_glb_xfTS->Fill(t->x_first[0]);
        hists->m_glb_yfTS->Fill(t->x_first[1]);
        hists->m_glb_zfTS->Fill(t->x_first[2]);
        hists->m_glb_radfTS->Fill(radf);
        hists->m_glb_ratioTS->Fill(nfitntot);
        hists->m_glb_ratiomTS->Fill(nfitnmax);
        hists->m_psiTS->Fill(t->psi);
        hists->m_tanlTS->Fill(t->tanl);
        hists->m_glb_thetaTS->Fill(thetad);
        hists->m_etaTS->Fill(eta);
        hists->m_pTTS->Fill(pT);
        hists->m_momTS->Fill(gmom);
        hists->m_lengthTS->Fill(t->length);
        hists->m_chisq0TS->Fill(chisq0);
        hists->m_chisq1TS->Fill(chisq1);
	
        hists->m_globtrk_xf_yfTS->Fill(t->x_first[0],t->x_first[1]);
        hists->m_eta_trklengthTS->Fill(eta,t->length);
        hists->m_npoint_lengthTS->Fill(t->length,Float_t(trkpnt));
        hists->m_fpoint_lengthTS->Fill(t->length,Float_t(trkfpnt));
	
        hists->m_pT_eta_recTS->Fill(eta,lmevpt);
        hists->m_tanl_zfTS->Fill(t->x_first[2],t->tanl);
        hists->m_mom_trklengthTS->Fill(t->length,lmevmom);
        hists->m_chisq0_momTS->Fill(lmevmom,chisq0);
        hists->m_chisq1_momTS->Fill(lmevmom,chisq1);
        hists->m_chisq0_etaTS->Fill(eta,chisq0);
        hists->m_chisq1_etaTS->Fill(eta,chisq1);
        hists->m_chisq0_dipTS->Fill(t->tanl,chisq0);
        hists->m_chisq1_dipTS->Fill(t->tanl,chisq1);
        hists->m_chisq0_zfTS->Fill(t->x_first[2],chisq0);
        hists->m_chisq1_zfTS->Fill(t->x_first[2],chisq1);
        hists->m_chisq0_phiTS->Fill(t->phi0,chisq0);
        hists->m_nfptonpt_momTS->Fill(lmevmom,nfitntot);
        hists->m_nfptonpt_etaTS->Fill(eta,nfitntot);
        hists->m_psi_phiTS->Fill(t->phi0,t->psi);
        break; }

//  now fill all FTPC East histograms ------------------------------------------------
        case (kFtpcEastId) : {
	
// these are tpc & ftpc
        hists->m_pointFE->Fill(trkpnt);
        hists->m_max_pointFE->Fill(trkmpnt);
        hists->m_fit_pointFE->Fill(trkfpnt);
        hists->m_glb_chargeFE->Fill(t->icharge);
        hists->m_glb_xfFE->Fill(t->x_first[0]);
        hists->m_glb_yfFE->Fill(t->x_first[1]);
        hists->m_glb_zfFE->Fill(t->x_first[2]);
        hists->m_glb_radfFE->Fill(radf);
        hists->m_glb_ratioFE->Fill(nfitntot);
        hists->m_glb_ratiomFE->Fill(nfitnmax);
        hists->m_psiFE->Fill(t->psi);
        hists->m_etaFE->Fill(eta);
        hists->m_pTFE->Fill(pT);
        hists->m_momFE->Fill(gmom);
        hists->m_lengthFE->Fill(t->length);
        hists->m_chisq0FE->Fill(chisq0);
        hists->m_chisq1FE->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_pT_eta_recFE->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFE->Fill(t->x_first[0],t->x_first[1]);
        hists->m_eta_trklengthFE->Fill(eta,t->length);
        hists->m_npoint_lengthFE->Fill(t->length,Float_t(trkpnt));
        hists->m_fpoint_lengthFE->Fill(t->length,Float_t(trkfpnt));	

        break; }

//  now fill all FTPC West histograms ------------------------------------------------
        case (kFtpcWestId) : {

// these are tpc & ftpc
        hists->m_pointFW->Fill(trkpnt);
        hists->m_max_pointFW->Fill(trkmpnt);
        hists->m_fit_pointFW->Fill(trkfpnt);
        hists->m_glb_chargeFW->Fill(t->icharge);
        hists->m_glb_xfFW->Fill(t->x_first[0]);
        hists->m_glb_yfFW->Fill(t->x_first[1]);
        hists->m_glb_zfFW->Fill(t->x_first[2]);
        hists->m_glb_radfFW->Fill(radf);
        hists->m_glb_ratioFW->Fill(nfitntot);
        hists->m_glb_ratiomFW->Fill(nfitnmax);
        hists->m_psiFW->Fill(t->psi);
        hists->m_etaFW->Fill(eta);
        hists->m_pTFW->Fill(pT);
        hists->m_momFW->Fill(gmom);
        hists->m_lengthFW->Fill(t->length);
        hists->m_chisq0FW->Fill(chisq0);
        hists->m_chisq1FW->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_pT_eta_recFW->Fill(eta,lmevpt);
        hists->m_globtrk_xf_yfFW->Fill(t->x_first[0],t->x_first[1]);
        hists->m_eta_trklengthFW->Fill(eta,t->length);
        hists->m_npoint_lengthFW->Fill(t->length,Float_t(trkpnt));
        hists->m_fpoint_lengthFW->Fill(t->length,Float_t(trkfpnt));	        
        break; }
       }
      }
    }
    hists->m_globtrk_good->Fill(cnttrkg);
    hists->m_globtrk_good_sm->Fill(cnttrkg);
  }       
}

//_____________________________________________________________________________

void St_QA_Maker::MakeHistDE() {
  // Fill histograms for dE/dx
  
  St_DataSetIter dstI(dst);
  
  St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];

  if(dst_dedx) {

    Int_t cntrows=0;
    cntrows = dst_dedx->GetNRows();
    hists->m_ndedxr->Fill(cntrows);

    dst_dedx_st *d = dst_dedx->GetTable();
    for (Int_t i = 0; i < dst_dedx->GetNRows(); i++,d++) {
        if (d->det_id==1) {      
         hists->m_ndedxT->Fill(d->ndedx);
         hists->m_dedx0T->Fill(d->dedx[0]);
         hists->m_dedx1T->Fill(d->dedx[1]);
        }
        if (d->det_id==4) {      
         hists->m_ndedxFW->Fill(d->ndedx);
         hists->m_dedx0FW->Fill(d->dedx[0]);
         hists->m_dedx1FW->Fill(d->dedx[1]);
        }
        if (d->det_id==5) {      
         hists->m_ndedxFE->Fill(d->ndedx);
         hists->m_dedx0FE->Fill(d->dedx[0]);
         hists->m_dedx1FE->Fill(d->dedx[1]);
        }
    }
  }
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistPrim(){

// Spiros added the following line on 10jan00
  float gtrack[8],target[2],ptrack[3];

  St_DataSetIter dstI(dst);           

  Float_t primVtxZ = 0;
  St_dst_vertex *vertex = (St_dst_vertex *) dstI["vertex"];
  if (vertex) {
    dst_vertex_st *tt = vertex->GetTable();
    for (Int_t i=0; i<vertex->GetNRows(); i++, tt++)
      if (tt->iflag==1 && tt->vtx_id==kEventVtxId)
	if (!isnan(double(tt->z)))
	  primVtxZ = tt->z;
  }

  St_dst_track *primtrk = (St_dst_track *) dstI["primtrk"];
  if (primtrk) {
    dst_track_st  *t   = primtrk->GetTable();

    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = primtrk->GetNRows();
    hists->m_primtrk_tot->Fill(cnttrk);
    hists->m_primtrk_tot_sm->Fill(cnttrk);

    for (Int_t i = 0; i < primtrk->GetNRows(); i++,t++){

      hists->m_primtrk_iflag->Fill(t->iflag);

      if (t->iflag>0) {
        cnttrkg++;

// n_point,n_fit_point,n_max_point is packed on dst tables:
//   n_point = 1*tpc_pnt + 1000*svt_pnt + 10000*ssd_pnt
//   - must unpack and add together to get total #pnt
       Int_t ssdpnt = 0;
       Int_t  hold1 = 0;
       Int_t svtpnt = 0;
       Int_t tpcpnt = 0;
       Int_t trkpnt = 0;

       Int_t ssdfpnt = 0;
       Int_t  holdf1 = 0;
       Int_t svtfpnt = 0;
       Int_t tpcfpnt = 0;
       Int_t trkfpnt = 0;

       Int_t ssdmpnt = 0;
       Int_t  holdm1 = 0;
       Int_t svtmpnt = 0;
       Int_t tpcmpnt = 0;
       Int_t trkmpnt = 0;

 
        ssdpnt = (t->n_point)/10000;
         hold1 = ((t->n_point)%10000);
        svtpnt = hold1/1000;
        tpcpnt = (hold1%1000);
        trkpnt = tpcpnt+svtpnt+ssdpnt;

        ssdfpnt = (t->n_fit_point)/10000;
         holdf1 = ((t->n_fit_point)%10000);
        svtfpnt = holdf1/1000;
        tpcfpnt = (holdf1%1000);
        trkfpnt = tpcfpnt+svtfpnt+ssdfpnt;

        ssdmpnt = (t->n_max_point)/10000;
         holdm1 = ((t->n_max_point)%10000);
        svtmpnt = holdm1/1000;
        tpcmpnt = (holdm1%1000);
        trkmpnt = tpcmpnt+svtmpnt+ssdmpnt;

	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - TMath::ATan(t->tanl);
        Float_t thetad = theta *(360./twopi);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t gmom  = pT/TMath::Sin(theta);
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
        Float_t nfitntot = (Float_t(trkfpnt))/(Float_t(trkpnt));
        Float_t nfitnmax = (Float_t(trkfpnt))/(Float_t(trkmpnt));
        Float_t logImpact = TMath::Log10(t->impact); 
        Float_t logCurvature = TMath::Log10(t->curvature); 



// Spiros' modifications start - 10jan00
        target[0] = t->x_first[0];
        target[1] = t->x_first[1];
        gtrack[0] = t->r0 * TMath::Cos(t->phi0*degree);
        gtrack[1] = t->r0 * TMath::Sin(t->phi0*degree);
        gtrack[2] = t->z0;
        gtrack[3] = t->psi;
        gtrack[4] = t->tanl;
        gtrack[5] = (float) t->icharge;
        gtrack[6] = t->invpt;
        gtrack[7] = t->curvature;
        Float_t mytst = prop_one_track( gtrack, target, ptrack);
        if (mytst != 5) {
         cout << " !! error from prop_one_track !!, iret = " << mytst << endl;
        }
        Float_t x0s  =  ptrack[0];
        Float_t y0s  =  ptrack[1];
        Float_t xdif =  (t->x_first[0])-x0s;
        Float_t ydif =  (t->x_first[1])-y0s;
        Float_t zdif = (t->x_first[2]) - (ptrack[2]);
// Spiros' modifications end

        Float_t radf = TMath::Power((t->x_first[0]),2) + 
                       TMath::Power((t->x_first[1]),2);
                radf = TMath::Sqrt(radf); 


// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 
		
         hists->m_pdet_id->Fill(t->det_id);

        switch (t->det_id) {
//  now fill all TPC histograms ------------------------------------------------
        case (kTpcId) : {

// these are tpc only
        hists->m_prim_f0->Fill(xdif,0);
        hists->m_prim_f0->Fill(ydif,1);
        hists->m_prim_f0->Fill(zdif,2);

        hists->m_prim_xf0->Fill(xdif);
        hists->m_prim_yf0->Fill(ydif);
        hists->m_prim_zf0->Fill(zdif);
        hists->m_prim_impactT->Fill(logImpact);
        hists->m_prim_impactrT->Fill(t->impact);
	
// these are tpc & ftpc
        hists->m_ppointT->Fill(trkpnt);
        hists->m_pmax_pointT->Fill(trkmpnt);
        hists->m_pfit_pointT->Fill(trkfpnt);
        hists->m_prim_chargeT->Fill(t->icharge);
        hists->m_prim_r0T->Fill(t->r0);
        hists->m_prim_phi0T->Fill(t->phi0);
        hists->m_prim_z0T->Fill(t->z0);
        hists->m_prim_curvT->Fill(logCurvature);
        hists->m_prim_xfT->Fill(t->x_first[0]);
        hists->m_prim_yfT->Fill(t->x_first[1]);
        hists->m_prim_zfT->Fill(t->x_first[2]);
        hists->m_prim_radfT->Fill(radf);
        hists->m_prim_ratioT->Fill(nfitntot);
        hists->m_prim_ratiomT->Fill(nfitnmax);
        hists->m_ppsiT->Fill(t->psi);
        hists->m_ptanlT->Fill(t->tanl);
        hists->m_prim_thetaT->Fill(thetad);
        hists->m_petaT->Fill(eta);
        hists->m_ppTT->Fill(pT);
        hists->m_pmomT->Fill(gmom);
        hists->m_plengthT->Fill(t->length);
        hists->m_pchisq0T->Fill(chisq0);
        hists->m_pchisq1T->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_primtrk_xf_yfT->Fill(t->x_first[0],t->x_first[1]);
        hists->m_peta_trklengthT->Fill(eta,t->length);
        hists->m_pnpoint_lengthT->Fill(t->length,Float_t(trkpnt));
        hists->m_pfpoint_lengthT->Fill(t->length,Float_t(trkfpnt));
	
// these are tpc only
        hists->m_ppT_eta_recT->Fill(eta,lmevpt);
        hists->m_ptanl_zfT->Fill(t->x_first[2]-primVtxZ,t->tanl);
        hists->m_pmom_trklengthT->Fill(t->length,lmevmom);
        hists->m_pchisq0_momT->Fill(lmevmom,chisq0);
        hists->m_pchisq1_momT->Fill(lmevmom,chisq1);
        hists->m_pchisq0_etaT->Fill(eta,chisq0);
        hists->m_pchisq1_etaT->Fill(eta,chisq1);
        hists->m_pchisq0_dipT->Fill(t->tanl,chisq0);
        hists->m_pchisq1_dipT->Fill(t->tanl,chisq1);
        hists->m_pchisq0_zfT->Fill(t->x_first[2],chisq0);
        hists->m_pchisq1_zfT->Fill(t->x_first[2],chisq1);
        hists->m_pnfptonpt_momT->Fill(lmevmom,nfitntot);
        hists->m_pnfptonpt_etaT->Fill(eta,nfitntot);
        hists->m_ppsi_phiT->Fill(t->phi0,t->psi);
        break; }


//  now fill all TPC+SVT histograms ------------------------------------------------
        case (kSvtId) :
        case (kTpcSsdId) :
        case (kTpcSvtId) :
        case (kTpcSsdSvtId) :
        case (kSsdSvtId) : {

        hists->m_prim_f0TS->Fill(xdif,0);
        hists->m_prim_f0TS->Fill(ydif,1);
        hists->m_prim_f0TS->Fill(zdif,2);

        hists->m_prim_xf0TS->Fill(xdif);
        hists->m_prim_yf0TS->Fill(ydif);
        hists->m_prim_zf0TS->Fill(zdif);
        hists->m_prim_impactTS->Fill(logImpact);
        hists->m_prim_impactrTS->Fill(t->impact);
	
        hists->m_ppointTS->Fill(trkpnt);
        hists->m_pmax_pointTS->Fill(trkmpnt);
        hists->m_pfit_pointTS->Fill(trkfpnt);
        hists->m_prim_chargeTS->Fill(t->icharge);
        hists->m_prim_r0TS->Fill(t->r0);
        hists->m_prim_phi0TS->Fill(t->phi0);
        hists->m_prim_z0TS->Fill(t->z0);
        hists->m_prim_curvTS->Fill(logCurvature);
        hists->m_prim_xfTS->Fill(t->x_first[0]);
        hists->m_prim_yfTS->Fill(t->x_first[1]);
        hists->m_prim_zfTS->Fill(t->x_first[2]);
        hists->m_prim_radfTS->Fill(radf);
        hists->m_prim_ratioTS->Fill(nfitntot);
        hists->m_prim_ratiomTS->Fill(nfitnmax);
        hists->m_ppsiTS->Fill(t->psi);
        hists->m_ptanlTS->Fill(t->tanl);
        hists->m_prim_thetaTS->Fill(thetad);
        hists->m_petaTS->Fill(eta);
        hists->m_ppTTS->Fill(pT);
        hists->m_pmomTS->Fill(gmom);
        hists->m_plengthTS->Fill(t->length);
        hists->m_pchisq0TS->Fill(chisq0);
        hists->m_pchisq1TS->Fill(chisq1);
	
        hists->m_primtrk_xf_yfTS->Fill(t->x_first[0],t->x_first[1]);
        hists->m_peta_trklengthTS->Fill(eta,t->length);
        hists->m_pnpoint_lengthTS->Fill(t->length,Float_t(trkpnt));
        hists->m_pfpoint_lengthTS->Fill(t->length,Float_t(trkfpnt));
	
        hists->m_ppT_eta_recTS->Fill(eta,lmevpt);
        hists->m_ptanl_zfTS->Fill(t->x_first[2]-primVtxZ,t->tanl);
        hists->m_pmom_trklengthTS->Fill(t->length,lmevmom);
        hists->m_pchisq0_momTS->Fill(lmevmom,chisq0);
        hists->m_pchisq1_momTS->Fill(lmevmom,chisq1);
        hists->m_pchisq0_etaTS->Fill(eta,chisq0);
        hists->m_pchisq1_etaTS->Fill(eta,chisq1);
        hists->m_pchisq0_dipTS->Fill(t->tanl,chisq0);
        hists->m_pchisq1_dipTS->Fill(t->tanl,chisq1);
        hists->m_pchisq0_zfTS->Fill(t->x_first[2],chisq0);
        hists->m_pchisq1_zfTS->Fill(t->x_first[2],chisq1);
        hists->m_pnfptonpt_momTS->Fill(lmevmom,nfitntot);
        hists->m_pnfptonpt_etaTS->Fill(eta,nfitntot);
        hists->m_ppsi_phiTS->Fill(t->phi0,t->psi);
        break; }

//  now fill all FTPC East histograms ------------------------------------------------
        case (kFtpcEastId) : {
	
// these are tpc & ftpc
        hists->m_ppointFE->Fill(trkpnt);
        hists->m_pmax_pointFE->Fill(trkmpnt);
        hists->m_pfit_pointFE->Fill(trkfpnt);
        hists->m_prim_chargeFE->Fill(t->icharge);
        hists->m_prim_xfFE->Fill(t->x_first[0]);
        hists->m_prim_yfFE->Fill(t->x_first[1]);
        hists->m_prim_zfFE->Fill(t->x_first[2]);
        hists->m_prim_radfFE->Fill(radf);
        hists->m_prim_ratioFE->Fill(nfitntot);
        hists->m_prim_ratiomFE->Fill(nfitnmax);
        hists->m_ppsiFE->Fill(t->psi);
        hists->m_petaFE->Fill(eta);
        hists->m_ppTFE->Fill(pT);
        hists->m_pmomFE->Fill(gmom);
        hists->m_plengthFE->Fill(t->length);
        hists->m_pchisq0FE->Fill(chisq0);
        hists->m_pchisq1FE->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_ppT_eta_recFE->Fill(eta,lmevpt);
        hists->m_primtrk_xf_yfFE->Fill(t->x_first[0],t->x_first[1]);
        hists->m_peta_trklengthFE->Fill(eta,t->length);
        hists->m_pnpoint_lengthFE->Fill(t->length,Float_t(trkpnt));
        hists->m_pfpoint_lengthFE->Fill(t->length,Float_t(trkfpnt));	

        break; }

//  now fill all FTPC West histograms ------------------------------------------------
        case (kFtpcWestId) : {

// these are tpc & ftpc
        hists->m_ppointFW->Fill(trkpnt);
        hists->m_pmax_pointFW->Fill(trkmpnt);
        hists->m_pfit_pointFW->Fill(trkfpnt);
        hists->m_prim_chargeFW->Fill(t->icharge);
        hists->m_prim_xfFW->Fill(t->x_first[0]);
        hists->m_prim_yfFW->Fill(t->x_first[1]);
        hists->m_prim_zfFW->Fill(t->x_first[2]);
        hists->m_prim_radfFW->Fill(radf);
        hists->m_prim_ratioFW->Fill(nfitntot);
        hists->m_prim_ratiomFW->Fill(nfitnmax);
        hists->m_ppsiFW->Fill(t->psi);
        hists->m_petaFW->Fill(eta);
        hists->m_ppTFW->Fill(pT);
        hists->m_pmomFW->Fill(gmom);
        hists->m_plengthFW->Fill(t->length);
        hists->m_pchisq0FW->Fill(chisq0);
        hists->m_pchisq1FW->Fill(chisq1);
	
// these are for tpc & ftpc
        hists->m_ppT_eta_recFW->Fill(eta,lmevpt);
        hists->m_primtrk_xf_yfFW->Fill(t->x_first[0],t->x_first[1]);
        hists->m_peta_trklengthFW->Fill(eta,t->length);
        hists->m_pnpoint_lengthFW->Fill(t->length,Float_t(trkpnt));
        hists->m_pfpoint_lengthFW->Fill(t->length,Float_t(trkfpnt));	        
        break; }
       }
      }
    }
    hists->m_primtrk_good->Fill(cnttrkg);
    hists->m_primtrk_good_sm->Fill(cnttrkg);
  }
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistGen(){
  if (Debug()) cout << " *** in St_QA_Maker - filling particle histograms " << endl;
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

            Float_t Glmevpt = TMath::Log10(pT*1000.0);

	    hists->m_H_pT_eta_gen->Fill(eta, Glmevpt);
	    hists->m_H_pT_gen->Fill((Float_t) pT);
	    if (fabs(eta) < 1)
	      hists->m_H_pT_genT->Fill((Float_t) pT);
	    hists->m_H_eta_gen->Fill(eta);
	    hists->m_H_vtxx->Fill(p->vhep[0]);
	    hists->m_H_vtxy->Fill(p->vhep[1]);
	    hists->m_H_vtxz->Fill(p->vhep[2]);
	  }
	}
      }
    }
    hists->m_H_npart->Fill(totpart);
    hists->m_H_npart_sm->Fill(totpart);
    hists->m_H_ncpart->Fill(nchgpart);
    hists->m_H_ncpart_sm->Fill(nchgpart);
  }
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistPID(){
  if (Debug()) cout << " *** in St_QA_Maker - filling PID histograms " << endl;
  
  St_DataSetIter dstI(dst);        
  
  // spectra-PID diagnostic histograms
  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  St_dst_dedx  *dst_dedx    = (St_dst_dedx *) dstI["dst_dedx"];
  
  if (dst_dedx && globtrk) {
    dst_dedx_st  *dedx   = dst_dedx->GetTable();
    dst_track_st  *trk   = globtrk->GetTable();
    dst_track_st *start_trk = trk;

    //    Int_t no_of_tracks  =  globtrk->GetNRows();
    //    Int_t no_of_dedx    =  dst_dedx->GetNRows();

    // loop over dedx entries
    for (Int_t l = 0; l < dst_dedx->GetNRows(); l++,dedx++){
      if (dedx->det_id == 1 && dedx->method == 1) {
        Float_t dedx_m = dedx->dedx[0];
        Int_t igl = dedx->id_track;
        Int_t igl_check;
      // loop over tracks till this is found
        trk = start_trk;
        for (Int_t t = 0; t < globtrk->GetNRows(); t++,trk++) {
	  igl_check = trk->id;
          if (igl == igl_check) break;
	}
        if (igl==trk->id) {
	  if (trk->iflag>0) {
	    Float_t pT = -999.;
	    pT = 1./TMath::Abs(trk->invpt);
	    Float_t theta = TMath::ASin(1.) - TMath::ATan(trk->tanl);
	    Float_t gmom  = pT/TMath::Sin(theta);
	    // Float_t invpt = trk->invpt;
	    // Float_t pT = 9999.;
	    // if (invpt) pT = 1./TMath::Abs(invpt);
	    // Float_t pz = pT*trk->tanl;
	    // Float_t  p = TMath::Sqrt(pT*pT+pz*pz);

	    if (dedx->ndedx >15 ) { 
	      hists->m_p_dedx_rec->Fill(gmom,dedx_m*1.e6); 
	    // change from GeV/cm to keV/cm
	    }
	  }
	}
      }
    }
  }
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistVertex(){

  Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
  Float_t m_pimass2 = (pion_minus_mass_c2*pion_minus_mass_c2);

  if (Debug()) cout << " *** in St_QA_Maker - filling vertex histograms " << endl;
  St_DataSetIter dstI(dst);
  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];
  
  if (vertex) {
    float z_svt = 999.;
    float z_tpc = -999.;
    float pvtx_x = 500.;
    float pvtx_y = 500.;
    float pvtx_z = 500.;

    hists->m_v_num->Fill(vertex->GetNRows());
    hists->m_v_num_sm->Fill(vertex->GetNRows());
    dst_vertex_st  *t   = vertex->GetTable();

    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      if (t->iflag == 201) z_svt = t->z;
      else if (t->iflag == 101) z_tpc = t->z;
      else if (t->iflag == 1 && t->vtx_id == kEventVtxId) { 
	// plot of primary vertex only
        hists->m_pv_vtxid->Fill(t->vtx_id);
	if (!isnan(double(t->x))) {
	  hists->m_pv_x->Fill(t->x);
	  pvtx_x = t->x;
	}
	if (!isnan(double(t->y))) {
	  hists->m_pv_y->Fill(t->y);
	  pvtx_y = t->y;
	}
	if (!isnan(double(t->z))) {
	  hists->m_pv_z->Fill(t->z);
	  pvtx_z = t->z;
	}
        hists->m_pv_pchi2->Fill(t->chisq[0]);
        hists->m_pv_r->Fill(t->x*t->x + t->y*t->y);
      }
      if (!(t->iflag == 1 && t->vtx_id == kEventVtxId)) { 
	// plot of 2ndary verticex only
        hists->m_v_vtxid->Fill(t->vtx_id);
	if (!isnan(double(t->x))) hists->m_v_x->Fill(t->x);     
	if (!isnan(double(t->y))) hists->m_v_y->Fill(t->y);     
	if (!isnan(double(t->z))) hists->m_v_z->Fill(t->z);     
        hists->m_v_pchi2->Fill(t->chisq[0]); 
        hists->m_v_r->Fill(t->x*t->x + t->y*t->y);
      }
    }

    if (!((pvtx_x == 500.) || (pvtx_y == 500.))) {
      t   = vertex->GetTable();
      for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
        if (t->vtx_id == kV0VtxId) {
           Float_t phi = atan2(t->y - pvtx_y, t->x - pvtx_x) * 180./M_PI;
           if (phi<0.) phi += 360.;
	   hists->m_vtx_phi_dist->Fill(phi);
        }
      }
    }
    hists->m_vtx_z->Fill(z_tpc-z_svt);
  }

  // V0 vertices
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_v0_vertex histograms " << endl;
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) dstI["dst_v0_vertex"];

  if (dst_v0_vertex) {
    dst_v0_vertex_st *v0 = dst_v0_vertex->GetTable();
    hists->m_v0->Fill(dst_v0_vertex->GetNRows());

    for (Int_t k=0; k<dst_v0_vertex->GetNRows(); k++, v0++){
      if (v0->dcav0 < 0.) continue;
      Float_t e1a = v0->pos_px*v0->pos_px +  v0->pos_py*v0->pos_py
	+ v0->pos_pz*v0->pos_pz;
      Float_t e2 = v0->neg_px*v0->neg_px +  v0->neg_py*v0->neg_py
	+ v0->neg_pz*v0->neg_pz;
      Float_t e1 = e1a + m_prmass2;  
      e2 += m_pimass2;
      e1 = TMath::Sqrt(e1);
      e2 = TMath::Sqrt(e2);
      Float_t p = (v0->neg_px+v0->pos_px)*(v0->neg_px+v0->pos_px)
	+  (v0->neg_py+v0->pos_py)*(v0->neg_py+v0->pos_py)
	+ (v0->neg_pz+v0->pos_pz)*(v0->neg_pz+v0->pos_pz);
      Float_t inv_mass_la = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      e1 = e1a + m_pimass2;
      e1 = TMath::Sqrt(e1);
      Float_t inv_mass_k0 = TMath::Sqrt((e1+e2)*(e1+e2) - p);

      hists->m_ev0_lama_hist->Fill(inv_mass_la);
      hists->m_ev0_k0ma_hist->Fill(inv_mass_k0);   

    }
  }

  // Xi vertices
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_xi_vertex histograms " << endl;
  St_dst_xi_vertex *dst_xi = (St_dst_xi_vertex*) dstI["dst_xi_vertex"];

  if (dst_xi) {
    hists->m_xi_tot->Fill(dst_xi->GetNRows());
    St_dst_v0_vertex *dst_v0 = (St_dst_v0_vertex*) dstI["dst_v0_vertex"];
    if (!dst_v0) {
      cout << "Error! No V0 table found for Xi's.";
    }
    else {
      dst_xi_vertex_st *xi = dst_xi->GetTable();
      dst_v0_vertex_st *v0sav = dst_v0->GetTable();
      Int_t v0last = dst_v0->GetNRows() - 1;
      dst_v0_vertex_st *v0end = dst_v0->GetTable(v0last);
      Float_t m_lamass2 = (lambda_mass_c2*lambda_mass_c2);
      Float_t m_pimass2 = (pion_minus_mass_c2*pion_minus_mass_c2);

      for (Int_t k=0; k<dst_xi->GetNRows(); k++, xi++){
	dst_v0_vertex_st *v0 = v0sav;
	
	while ((v0 != v0end) && (v0->id != xi->id_v0)) v0++;
	if (v0->id != xi->id_v0) {
	  cout << "Error! V0 associated with Xi not found." << endl;
	  break;
	}

	Float_t px = v0->pos_px+v0->neg_px;
	Float_t py = v0->pos_py+v0->neg_py;
	Float_t pz = v0->pos_pz+v0->neg_pz;
	Float_t e1 = px*px + py*py + pz*pz;
	e1 +=  m_lamass2;
	Float_t e2 = xi->px_b*xi->px_b +  xi->py_b*xi->py_b
	  + xi->pz_b*xi->pz_b;
	e2 += m_pimass2;
	e1 = sqrt(e1);
	e2 = sqrt(e2);
	Float_t e3 = e1 + e2;
	px += xi->px_b;
	py += xi->py_b;
	pz += xi->pz_b;
	Float_t psq =  px*px + py*py + pz*pz;
	Float_t inv_mass_xi = sqrt(e3*e3 - psq);

        hists->m_xi_ma_hist->Fill(inv_mass_xi);
      }
    }
  }

  // Kink vertices
  if (Debug()) cout << " *** in St_QA_Maker - filling kink histograms " << endl;
  St_dst_tkf_vertex *pt = (St_dst_tkf_vertex*) dstI["kinkVertex"];

  if (pt) {
    hists->m_kink_tot->Fill(pt->GetNRows());
  }

}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistPoint(){
  if (Debug()) cout << " *** in St_QA_Maker - filling point histograms " << endl;


  St_DataSetIter dstI(dst);           

  St_dst_point *pt = (St_dst_point*) dstI["point"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
      hists->m_pnt_tot->Fill(cntrows);
      hists->m_pnt_tot_med->Fill(cntrows);
      hists->m_pnt_tot_sm->Fill(cntrows);

    dst_point_st  *t   = pt->GetTable();

    Int_t hitsTpc=0;
    Int_t hitsSvt=0;
    Int_t hitsFtpcW=0;
    Int_t hitsFtpcE=0;
    Int_t hitsSsd=0;

    Int_t id = 0;
    for (Int_t i = 0; i < pt->GetNRows(); i++,t++){

// unpack detector ID value:
      id = (t->hw_position) & 15;

      hists->m_pnt_id->Fill(id);

      if (id==kTpcIdentifier)           {hitsTpc++;}
      else if (id==kSvtIdentifier)      {hitsSvt++;}
      else if (id==kFtpcWestIdentifier) {hitsFtpcW++;}
      else if (id==kFtpcEastIdentifier) {hitsFtpcE++;}
      else if (id==kSsdIdentifier)      {hitsSsd++;}

    }

      hists->m_pnt_tpc->Fill(hitsTpc);
      hists->m_pnt_svt->Fill(hitsSvt);
      hists->m_pnt_ftpcW->Fill(hitsFtpcW);
      hists->m_pnt_ftpcE->Fill(hitsFtpcE);
      hists->m_pnt_ssd->Fill(hitsSsd);

  }

}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistRich(){
  if (Debug()) cout << " *** in St_QA_Maker - filling Rich histograms " << endl;
  // RICH information is written directly to StEvent now.
}


//_____________________________________________________________________________
void St_QA_Maker::MakeHistEval(){
  if (Debug()) cout << " *** in St_QA_Maker - filling geant,reco eval histograms " << endl;


// -- get geant vtx ----------------------------------------------------
  

  St_DataSet *geant;      //! Pointer to current dataset - geant
  geant = GetDataSet("geant");
  if( !geant ){ 
     cout << " St_QA_Maker::Make - No pointer to GEANT DataSet \n" << endl; 
     return;
  }

  St_DataSetIter geantI(geant);

   St_g2t_vertex *geantVertex=(St_g2t_vertex *) geant->Find("g2t_vertex"); 
  if( !geantVertex ) { 
      cout << "St_QA_Maker::MakeHistEval - NULL pointer to St_g2t_vertex table\n"<< endl;
      return; 
  } 
  if( geantVertex->GetNRows()<=0) {
      cout << " St_QA_Maker::MakeHistEval - empty St_g2t_vertex table\n" << endl; 
      return; 
  } 

  g2t_vertex_st *gvt=geantVertex->GetTable();
  Float_t geantX, geantY, geantZ; 
  geantX = gvt->ge_x[0];
  geantY = gvt->ge_x[1];
  geantZ = gvt->ge_x[2];


// get reco vtx ----------------------------------------------------
  St_DataSetIter dstI(dst);

  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];



  if (vertex) {
    Float_t recoX, recoY, recoZ;
    recoX = recoY = recoZ = 200.;
    dst_vertex_st  *t   = vertex->GetTable();
    if (vertex->GetNRows() <= 0) {
      cout << " St_QA_Maker::MakeHistEval - empty St_dst_vertex table\n" << endl;
      return;
    }
    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      if (t->iflag==1 && t->vtx_id==kEventVtxId){       
        recoX = t->x;
        recoY = t->y;
        recoZ = t->z;
      }
    }
// fill geant,reco comparison histograms -----------------------------------
    hists->m_geant_reco_pvtx_x->Fill(geantX-recoX);
    hists->m_geant_reco_pvtx_y->Fill(geantY-recoY);
    hists->m_geant_reco_pvtx_z->Fill(geantZ-recoZ);
    hists->m_geant_reco_vtx_z_z->Fill(geantZ-recoZ,recoZ);
  }
}

//_____________________________________________________________________________

