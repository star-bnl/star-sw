// $Id: St_QA_Maker.cxx,v 1.72 1999/12/16 19:52:37 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
// Revision 1.72  1999/12/16 19:52:37  kathy
// fix hist titles in QABookHist; unpack n_point,n_fit_point,n_max_point correctly for globtrk table - must still fix for primtrk table - in St_QA_Maker
//
// Revision 1.71  1999/12/15 20:32:18  kathy
// separated the tpc and tpc+svt histograms for globtrk table; had to book and fill new histograms, add histograms to default logy list AND had to change what values of iflag I cut on for filling each different type of track in makehistglob method
//
// Revision 1.70  1999/12/15 18:31:05  kathy
// added 4 new histogram to globtrk for tpc - r0,phi0,z0,curvature; also put 3 of these in default logY list; also changed scale on iflag hist. for globtrk & primtrk
//
// Revision 1.69  1999/12/15 17:17:33  kathy
// changed the dedx histograms to the scale GeV/cm - which is the scale in the dst table
//
// Revision 1.68  1999/12/14 18:33:24  kathy
// removed 4 ftpc histograms as per Janet's request
//
// Revision 1.67  1999/12/12 23:09:47  kathy
// add pt vs eta in ftpc histogram as per Janet
//
// Revision 1.66  1999/12/08 22:58:18  kathy
// changed histogram limits and made names smaller
//
// Revision 1.65  1999/12/07 23:14:18  kathy
// fix primary vtx histograms for dst tables; split apart the ftpc and tpc in the dedx histograms
//
// Revision 1.64  1999/12/06 22:25:05  kathy
// split apart the tpc and ftpc (east & west) histograms for the globtrk table; had to add characters to end of each histogram pointer to differentiate the different ones; updated the default list of hist to be plotted with logy scale
//
// Revision 1.63  1999/11/23 19:00:51  lansdell
// Reorganized Make() and include files (Gene)
//
// Revision 1.62  1999/11/22 22:46:41  lansdell
// update to identify histogram method used (StEvent or DST tables) by Gene; StEventQAMaker code partially completed (run bfcread_dst_EventQAhist.C)
//
// Revision 1.61  1999/11/19 22:44:43  kathy
// took histogram booking out of St_QA_Maker as per Thomas' request and put it into separate class StQABookHist which can now be used also by Curtis' class to book histograms - thanks for your help Gene!
//
// Revision 1.60  1999/11/18 22:48:42  kathy
// remove commented out lines
//
// Revision 1.59  1999/11/18 22:34:11  kathy
// removed some histograms of variables that no longer exist and change some limits
//
// Revision 1.58  1999/11/09 20:37:33  fisyak
// Correct tables
//
// Revision 1.57  1999/11/05 15:25:38  kathy
// fix hist limits for detector id hist; small updates to documentation
//
// Revision 1.56  1999/09/30 21:48:39  kathy
// fix for phi0 being in degrees in globtrk
//
// Revision 1.55  1999/09/29 16:46:29  kathy
// changed code so it would compile in .dev due to changes in DST tables - I even used cons instead of makel - wow! - I just changed variables or commented out some histograms that use now-non-existant variables so it would compile - later I will go through and redefine histograms as needed
//
// Revision 1.54  1999/09/23 20:08:37  kathy
// fix some histogram limits
//
// Revision 1.53  1999/09/23 18:54:09  kathy
// fix some histogram limits, add about 10 histograms - just so we know number rows in each table - had to include some more tables to do this
//
// Revision 1.52  1999/09/23 16:04:33  kathy
// change paths for include files to standard way according to Yuri's request
//
// Revision 1.51  1999/09/21 15:05:36  kathy
// comment out unneccessary method: SetPntrToHistUtil because now I'm making it totally independent of the histograms printing at the end - also put in doc directory and html file - basically empty now
//
// Revision 1.50  1999/09/20 20:22:19  kathy
// oops - one more fix to make sure that QA_Maker doesn't depend on StHistUtil
//
// Revision 1.49  1999/09/20 20:12:17  kathy
// moved the histogram utility methods out of St_QA_Maker and into StHistUtil because they can really be used by any Maker and associated histograms
//
// Revision 1.48  1999/09/02 21:47:23  kathy
// changed code so that it uses TMath functions so will compile on HP
//
// Revision 1.47  1999/07/23 17:26:36  kathy
// changes to histogram limits
//
// Revision 1.46  1999/07/17 01:51:19  kathy
// changed limits and titles of some histograms
//
// Revision 1.45  1999/07/15 13:57:37  perev
// cleanup
//
// Revision 1.44  1999/07/14 23:22:58  kathy
// a lot of changes to hist limits and fixes to titles and added a few new hist
//
// Revision 1.43  1999/07/12 16:39:33  kathy
// hopefully last change for globtrk,event_summary and primtrk histograms
//
// Revision 1.42  1999/07/11 23:32:00  fisyak
// St_dst_TriggerDetectors => St_dst_TrgDet
//
// Revision 1.41  1999/07/09 23:04:03  kathy
// hopefully getting to final round of fixes to globtrk and primtrk histograms
//
// Revision 1.40  1999/07/09 13:14:17  kathy
// now have put in new primtrk histograms to match the globtrk ones
//
// Revision 1.38  1999/07/08 22:20:57  kathy
// updated limits on hist
//
// Revision 1.37  1999/07/07 21:23:16  kathy
// fixed log scales
//
// Revision 1.36  1999/07/07 16:58:32  kathy
// put log scales on some histograms
//
// Revision 1.35  1999/07/02 21:56:56  kathy
// update for tables which exist in 99f AND put in changes to event summary and globtrk histogram sets requested by offline analysis meeting
//
// Revision 1.34  1999/06/30 20:35:35  kathy
// now have 2D histograms being plotted with box plots instead of scatter plots
//
// Revision 1.33  1999/06/17 18:25:32  kathy
// fix so writes out blank canvas
//
// Revision 1.32  1999/06/15 14:44:52  kathy
// fix St_QA_Maker
//
// Revision 1.30  1999/06/11 20:05:51  kathy
// put in method FindHists to find the histogram directory, since it can be in different places depending on how/where you make the histograms
//
// Revision 1.29  1999/05/10 20:03:54  kathy
// add new member function ExamineLogYList and RemoveFromLogYList
//
// Revision 1.28  1999/05/10 17:16:16  kathy
// added new member function SetDefaultLogYList and implemented and tested
//
// Revision 1.27  1999/05/07 20:20:53  kathy
// now set logy on when hist name is in loglist
//
// Revision 1.26  1999/05/07 17:18:29  kathy
// new method AddToLogYList implemented and tested on solaris
//
// Revision 1.25  1999/05/06 12:48:44  fisyak
// Add search geant in search path for particle tables
//
// Revision 1.24  1999/05/05 19:35:52  kathy
// add new method ListHists and clean up
//
// Revision 1.23  1999/04/28 18:39:29  kathy
// removed check of two different directory for GetDataSet because the infrastructure code should take care of this and not the Makers
//
// Revision 1.22  1999/04/27 21:05:29  kathy
// clean up comments
//
// Revision 1.21  1999/04/23 14:04:07  kathy
// just cleaning up comments
//
// Revision 1.20  1999/04/21 20:19:18  kathy
// put in comments and cleaned up - works for mdc2 dst in dev now
//
// Revision 1.19  1999/04/20 01:16:59  fisyak
// Add check on. no of tracks in dE/dX
//
// Revision 1.18  1999/04/19 20:33:42  didenko
// uncommented MakeHistGen fuction
//
// Revision 1.17  1999/04/19 18:07:57  didenko
// QA_Maker for new scheme DST
//
// Revision 1.16  1999/03/11 23:14:49  fisyak
// Victor scheme
// 
// Revision 1.15  1999/03/11 21:13:13  kathy
// update to hist limits
//
// Revision 1.14  1999/03/09 16:30:23  fine
// Workqround of the St_io_Maker bug
//
// Revision 1.13  1999/03/07 19:26:15  fine
// QA->SetPostScriptFile(psFile) has been introduced
//
// Revision 1.12  1999/03/07 16:53:32  fine
// New method DrawHists
//
// Revision 1.11  1999/03/05 21:19:37  kathy
// added new histograms
//
// Revision 1.10  1999/03/03 23:34:29  kathy
// fixes to histograms
//
// Revision 1.9  1999/02/26 18:42:33  kathy
// added vertex histograms
//
// Revision 1.8  1999/02/26 17:24:42  kathy
// fix histograms
//
// Revision 1.7  1999/02/25 21:11:56  kathy
// fix histograms
//
// Revision 1.6  1999/02/25 19:25:39  kathy
// fix up histograms
//
// Revision 1.5  1999/02/24 21:15:02  kathy
// fixed histograms and added a few new ones
//
// Revision 1.4  1999/02/23 22:22:22  kathy
// changes to histograms: titles changed so they'll be in order and redundant ones removed
//
// Revision 1.3  1999/02/22 21:27:17  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.2  1999/02/20 00:24:48  kathy
// fixed some of the histograms
//
// Revision 1.1  1999/02/08 19:28:23  didenko
// fixed directory level
//
// Revision 1.4  1999/01/22 22:53:14  didenko
// maker to fill QA histograms
//
// Revision 1.3  1999/01/22 22:19:57  didenko
// maker to fill QA histograms
//
// Revision 1.2  1998/12/21 19:43:17  fisyak
// Move ROOT includes to non system
//
// Revision 1.1  1998/11/01 16:42:25  fisyak
// dst analysis
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
#include "tables/St_ev0_eval_Table.h"          // ev0_eval
#include "tables/St_tpt_track_Table.h"         // l3Track

ClassImp(St_QA_Maker)
  
//_____________________________________________________________________________
  St_QA_Maker::St_QA_Maker(const char *name, const char *title) : StQABookHist(name,title,"Tab"){

}
//_____________________________________________________________________________

St_QA_Maker::~St_QA_Maker(){

}

//_____________________________________________________________________________

Int_t St_QA_Maker::Finish() {

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Init(){
// St_QA_Maker - Init; book histograms and set defaults for member functions

  return StQABookHist::Init();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Make(){
// St_QA_Maker - Make; fill histograms
    
  dst = GetDataSet("dst");
  if (dst) {
    return StQABookHist::Make();
  } else {
    cout << "Error in St_QA_Maker::Make(): no dst dataset found!" << endl;
    return kStErr;
  }
}
//_____________________________________________________________________________


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

      m_trk_tot_gd->Fill(trk_good/trk_tot); 
      m_glb_trk_tot->Fill(tt->glb_trk_tot);
      m_glb_trk_plusminus->Fill(trk_plus/trk_minus);
      m_vert_total->Fill(tt->n_vert_total);
      m_glb_trk_prim->Fill(tt->glb_trk_prim);

      m_mean_pt->Fill(tt->mean_pt);
      m_mean_eta->Fill(tt->mean_eta);
      m_rms_eta->Fill(tt->rms_eta);

      if(!isnan((double)(tt->prim_vrtx[0])))  m_prim_vrtx0->Fill(tt->prim_vrtx[0]);
      if(!isnan((double)(tt->prim_vrtx[1])))  m_prim_vrtx1->Fill(tt->prim_vrtx[1]);
      if(!isnan((double)(tt->prim_vrtx[2])))  m_prim_vrtx2->Fill(tt->prim_vrtx[2]);
      
    }
  }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeHistGlob(){

  St_DataSetIter dstI(dst);           

  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    dst_track_st  *t   = globtrk->GetTable();

    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = globtrk->GetNRows();
    m_globtrk_tot->Fill(cnttrk);

    for (Int_t i = 0; i < globtrk->GetNRows(); i++,t++){

      m_globtrk_iflag->Fill(t->iflag);

      if (t->iflag>0) {
        cnttrkg++;
// n_point,n_fit_point,n_max_point is packed on dst tables:
//   n_point = 1*tpc_pnt + 1000*svt_pnt + 10000*ssd_pnt
//   - must unpack and add together to get total #pnt
       Int_t ssdpnt = 0;
       Int_t  hold1 = 0;
       Int_t svtpnt = 0;
       Int_t  hold2 = 0;
       Int_t tpcpnt = 0;
       Int_t trkpnt = 0;

       Int_t ssdfpnt = 0;
       Int_t  holdf1 = 0;
       Int_t svtfpnt = 0;
       Int_t  holdf2 = 0;
       Int_t tpcfpnt = 0;
       Int_t trkfpnt = 0;

       Int_t ssdmpnt = 0;
       Int_t  holdm1 = 0;
       Int_t svtmpnt = 0;
       Int_t  holdm2 = 0;
       Int_t tpcmpnt = 0;
       Int_t trkmpnt = 0;

       //if (cnttrkg<10){
       //   cout << "Before: n_point = " << t->n_point << " n_fit = " << t->n_fit_point << 
       //                  " n_max = " << t->n_max_point << endl;
       //	}
        ssdpnt = (t->n_point)/10000;
         hold1 = ((t->n_point)%10000);
        svtpnt = hold1/1000;
         hold2 = (hold1%1000);
        tpcpnt = hold2;
        trkpnt = tpcpnt+svtpnt+ssdpnt;

        ssdfpnt = (t->n_fit_point)/10000;
         holdf1 = ((t->n_fit_point)%10000);
        svtfpnt = holdf1/1000;
         holdf2 = (holdf1%1000);
        tpcfpnt = holdf2;
        trkfpnt = tpcfpnt+svtfpnt+ssdfpnt;

        ssdmpnt = (t->n_max_point)/10000;
         holdm1 = ((t->n_max_point)%10000);
        svtmpnt = holdm1/1000;
         holdm2 = (holdm1%1000);
        tpcmpnt = holdm2;
        trkmpnt = tpcmpnt+svtmpnt+ssdmpnt;

	//        if (cnttrkg<10){
        //  cout << "After:  #ssd   = " << ssdpnt <<
	//        " #hold1 = " << hold1  <<         
	//        " #svt   = " << svtpnt <<
	//        " #hold2 = " << hold2  << 
        //        " #tpc   = " << tpcpnt <<
	// 	" #tot   = " << trkpnt << endl; 
        //}    
        //if (cnttrkg<10){
        //  cout << "After:  #ssdf   = " << ssdfpnt <<
	//        " #hold1f = " << holdf1  <<         
	//        " #svtf   = " << svtfpnt <<
	//        " #hold2f = " << holdf2  << 
        //        " #tpcf   = " << tpcfpnt <<
	// 	" #totf   = " << trkfpnt << endl; 
        //}    
        //if (cnttrkg<10){
        //  cout << "After:  #ssdm   = " << ssdmpnt <<
	//        " #hold1m = " << holdm1  <<         
	//        " #svtm   = " << svtmpnt <<
	//        " #hold2m = " << holdm2  << 
        //        " #tpcm   = " << tpcmpnt <<
	// 	" #totm   = " << trkmpnt << endl; 
        //}
    
	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - TMath::ATan(t->tanl);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t gmom  = pT/TMath::Sin(theta);
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
	//	Float_t degoffree = t->n_fit_point;
	//Float_t chisq0_p = chisq0/(degoffree-3);
	//Float_t chisq1_p = chisq1/(degoffree-2);
        Float_t nfitntot = (Float_t(trkfpnt))/(Float_t(trkpnt));
        Float_t nfitnmax = (Float_t(trkmpnt))/(Float_t(trkpnt));
        Float_t x0s  =  t->r0 * TMath::Cos(t->phi0*degree);
        Float_t y0s  =  t->r0 * TMath::Sin(t->phi0*degree);
        Float_t xdif =  (t->x_first[0])-x0s;
        Float_t ydif =  (t->x_first[1])-y0s;
        Float_t zdif =  (t->x_first[2])-(t->z0);
        Float_t radf = TMath::Power((t->x_first[0]),2) + 
                       TMath::Power((t->x_first[1]),2);
                radf = TMath::Sqrt(radf); 

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here 
		
 	m_det_id->Fill(t->det_id);

//  now fill all TPC histograms ------------------------------------------------
        if (t->iflag>100 && t->iflag<200 ) {

// these are tpc only
        m_glb_xf0->Fill(xdif);
        m_glb_yf0->Fill(ydif);
        m_glb_zf0->Fill(zdif);
        m_glb_impactT->Fill(t->impact);
	
// these are tpc & ftpc
	m_pointT->Fill(trkpnt);
	m_max_pointT->Fill(trkmpnt);
	m_fit_pointT->Fill(trkfpnt);
        m_glb_chargeT->Fill(t->icharge);
        m_glb_r0T->Fill(t->r0);
        m_glb_phi0T->Fill(t->phi0);
        m_glb_z0T->Fill(t->z0);
        m_glb_curvT->Fill(t->curvature);
        m_glb_xfT->Fill(t->x_first[0]);
        m_glb_yfT->Fill(t->x_first[1]);
        m_glb_zfT->Fill(t->x_first[2]);
        m_glb_radfT->Fill(radf);
        m_glb_ratioT->Fill(nfitntot);
        m_glb_ratiomT->Fill(nfitnmax);
	m_psiT->Fill(t->psi);
        m_tanlT->Fill(t->tanl);
        m_glb_thetaT->Fill(theta);
	m_etaT->Fill(eta);
	m_pTT->Fill(pT);
        m_momT->Fill(gmom);
	m_lengthT->Fill(t->length);
	m_chisq0T->Fill(chisq0);
	m_chisq1T->Fill(chisq1);
	
// these are for tpc & ftpc
        m_globtrk_xf_yfT->Fill(t->x_first[0],t->x_first[1]);
        m_eta_trklengthT->Fill(eta,t->length);
	m_npoint_lengthT->Fill(t->length,Float_t(trkpnt));
	m_fpoint_lengthT->Fill(t->length,Float_t(trkfpnt));
	
// these are tpc only
	m_pT_eta_recT->Fill(eta,lmevpt);
        m_tanl_zfT->Fill(t->x_first[2],t->tanl);
	m_mom_trklengthT->Fill(t->length,lmevmom);
	m_chisq0_momT->Fill(lmevmom,chisq0);
	m_chisq1_momT->Fill(lmevmom,chisq1);
	m_chisq0_etaT->Fill(eta,chisq0);
	m_chisq1_etaT->Fill(eta,chisq1);
	m_chisq0_dipT->Fill(t->tanl,chisq0);
	m_chisq1_dipT->Fill(t->tanl,chisq1);
	m_chisq0_zfT->Fill(t->x_first[2],chisq0);
	m_chisq1_zfT->Fill(t->x_first[2],chisq1);
        m_nfptonpt_momT->Fill(lmevmom,nfitntot);
        m_nfptonpt_etaT->Fill(eta,nfitntot);
        }


//  now fill all TPC+SVT histograms ------------------------------------------------
        if (t->iflag>500 && t->iflag<600 ) {

        m_glb_xf0TS->Fill(xdif);
        m_glb_yf0TS->Fill(ydif);
        m_glb_zf0TS->Fill(zdif);
        m_glb_impactTS->Fill(t->impact);
	
	m_pointTS->Fill(trkpnt);
	m_max_pointTS->Fill(trkmpnt);
	m_fit_pointTS->Fill(trkfpnt);
        m_glb_chargeTS->Fill(t->icharge);
        m_glb_r0TS->Fill(t->r0);
        m_glb_phi0TS->Fill(t->phi0);
        m_glb_z0TS->Fill(t->z0);
        m_glb_curvTS->Fill(t->curvature);
        m_glb_xfTS->Fill(t->x_first[0]);
        m_glb_yfTS->Fill(t->x_first[1]);
        m_glb_zfTS->Fill(t->x_first[2]);
        m_glb_radfTS->Fill(radf);
        m_glb_ratioTS->Fill(nfitntot);
        m_glb_ratiomTS->Fill(nfitnmax);
	m_psiTS->Fill(t->psi);
        m_tanlTS->Fill(t->tanl);
        m_glb_thetaTS->Fill(theta);
	m_etaTS->Fill(eta);
	m_pTTS->Fill(pT);
        m_momTS->Fill(gmom);
	m_lengthTS->Fill(t->length);
	m_chisq0TS->Fill(chisq0);
	m_chisq1TS->Fill(chisq1);
	
        m_globtrk_xf_yfTS->Fill(t->x_first[0],t->x_first[1]);
        m_eta_trklengthTS->Fill(eta,t->length);
	m_npoint_lengthTS->Fill(t->length,Float_t(trkpnt));
	m_fpoint_lengthTS->Fill(t->length,Float_t(trkfpnt));
	
	m_pT_eta_recTS->Fill(eta,lmevpt);
        m_tanl_zfTS->Fill(t->x_first[2],t->tanl);
	m_mom_trklengthTS->Fill(t->length,lmevmom);
	m_chisq0_momTS->Fill(lmevmom,chisq0);
	m_chisq1_momTS->Fill(lmevmom,chisq1);
	m_chisq0_etaTS->Fill(eta,chisq0);
	m_chisq1_etaTS->Fill(eta,chisq1);
	m_chisq0_dipTS->Fill(t->tanl,chisq0);
	m_chisq1_dipTS->Fill(t->tanl,chisq1);
	m_chisq0_zfTS->Fill(t->x_first[2],chisq0);
	m_chisq1_zfTS->Fill(t->x_first[2],chisq1);
        m_nfptonpt_momTS->Fill(lmevmom,nfitntot);
        m_nfptonpt_etaTS->Fill(eta,nfitntot);

        }

//  now fill all FTPC East histograms ------------------------------------------------
        if (t->iflag>700 && t->iflag<800 && t->det_id==5) {
	
// these are tpc & ftpc
	m_pointFE->Fill(trkpnt);
	m_max_pointFE->Fill(trkmpnt);
	m_fit_pointFE->Fill(trkfpnt);
        m_glb_chargeFE->Fill(t->icharge);
        m_glb_xfFE->Fill(t->x_first[0]);
        m_glb_yfFE->Fill(t->x_first[1]);
        m_glb_zfFE->Fill(t->x_first[2]);
        m_glb_radfFE->Fill(radf);
        m_glb_ratioFE->Fill(nfitntot);
        m_glb_ratiomFE->Fill(nfitnmax);
	m_psiFE->Fill(t->psi);
	m_etaFE->Fill(eta);
	m_pTFE->Fill(pT);
        m_momFE->Fill(gmom);
	m_lengthFE->Fill(t->length);
	m_chisq0FE->Fill(chisq0);
	m_chisq1FE->Fill(chisq1);
	
// these are for tpc & ftpc
	m_pT_eta_recFE->Fill(eta,lmevpt);
        m_globtrk_xf_yfFE->Fill(t->x_first[0],t->x_first[1]);
        m_eta_trklengthFE->Fill(eta,t->length);
	m_npoint_lengthFE->Fill(t->length,Float_t(trkpnt));
	m_fpoint_lengthFE->Fill(t->length,Float_t(trkfpnt));	

        }

//  now fill all FTPC West histograms ------------------------------------------------
        if (t->iflag>700 && t->iflag<800 && t->det_id==4) {

// these are tpc & ftpc
	m_pointFW->Fill(trkpnt);
	m_max_pointFW->Fill(trkmpnt);
	m_fit_pointFW->Fill(trkfpnt);
        m_glb_chargeFW->Fill(t->icharge);
        m_glb_xfFW->Fill(t->x_first[0]);
        m_glb_yfFW->Fill(t->x_first[1]);
        m_glb_zfFW->Fill(t->x_first[2]);
        m_glb_radfFW->Fill(radf);
        m_glb_ratioFW->Fill(nfitntot);
        m_glb_ratiomFW->Fill(nfitnmax);
	m_psiFW->Fill(t->psi);
	m_etaFW->Fill(eta);
	m_pTFW->Fill(pT);
        m_momFW->Fill(gmom);
	m_lengthFW->Fill(t->length);
	m_chisq0FW->Fill(chisq0);
	m_chisq1FW->Fill(chisq1);
	
// these are for tpc & ftpc
	m_pT_eta_recFW->Fill(eta,lmevpt);
        m_globtrk_xf_yfFW->Fill(t->x_first[0],t->x_first[1]);
        m_eta_trklengthFW->Fill(eta,t->length);
	m_npoint_lengthFW->Fill(t->length,Float_t(trkpnt));
	m_fpoint_lengthFW->Fill(t->length,Float_t(trkfpnt));	        
        }

      }
    }
    m_globtrk_good->Fill(cnttrkg);
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
    m_ndedxr->Fill(cntrows);

    dst_dedx_st *d = dst_dedx->GetTable();
    for (Int_t i = 0; i < dst_dedx->GetNRows(); i++,d++) {
        if (d->det_id==1) {      
         m_ndedxT->Fill(d->ndedx);
         m_dedx0T->Fill(d->dedx[0]);
         m_dedx1T->Fill(d->dedx[1]);
        }
        if (d->det_id==4) {      
         m_ndedxFW->Fill(d->ndedx);
         m_dedx0FW->Fill(d->dedx[0]);
         m_dedx1FW->Fill(d->dedx[1]);
        }
        if (d->det_id==5) {      
         m_ndedxFE->Fill(d->ndedx);
         m_dedx0FE->Fill(d->dedx[0]);
         m_dedx1FE->Fill(d->dedx[1]);
        }
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistPrim(){

  St_DataSetIter dstI(dst);           

  St_dst_track *primtrk = (St_dst_track *) dstI["primtrk"];
  if (primtrk) {
    dst_track_st  *t   = primtrk->GetTable();

    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = primtrk->GetNRows();
    m_primtrk_tot->Fill(cnttrk);

    for (Int_t i = 0; i < primtrk->GetNRows(); i++,t++){

      m_primtrk_iflag->Fill(t->iflag);

      if (t->iflag>0) {
        cnttrkg++;
	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - TMath::ATan(t->tanl);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t gmom  = pT/TMath::Sin(theta);
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
	//Float_t degoffree = t->n_fit_point;
	//Float_t chisq0_p = chisq0/(degoffree-3);
	//Float_t chisq1_p = chisq1/(degoffree-2);
        Float_t nfitntot = (Float_t(t->n_fit_point))/(Float_t(t->n_point));
        Float_t x0s  =  t->r0 * TMath::Cos(t->phi0*degree);
        Float_t y0s  =  t->r0 * TMath::Sin(t->phi0*degree);
        Float_t xdif =  (t->x_first[0])-x0s;
        Float_t ydif =  (t->x_first[1])-y0s;
        Float_t zdif = (t->x_first[2]) - (t->z0);
        Float_t radf = TMath::Power((t->x_first[0]),2) + 
                       TMath::Power((t->x_first[1]),2);
                radf = TMath::Sqrt(radf); 

 	m_pdet_id->Fill(t->det_id);
	m_ppoint->Fill(t->n_point);
	m_pmax_point->Fill(t->n_max_point);
	m_pfit_point->Fill(t->n_fit_point);
        m_prim_charge->Fill(t->icharge);
        m_prim_xf->Fill(t->x_first[0]);
        m_prim_yf->Fill(t->x_first[1]);
        m_prim_zf->Fill(t->x_first[2]);
        m_prim_xf0->Fill(xdif);
        m_prim_yf0->Fill(ydif);
        m_prim_zf0->Fill(zdif);
        m_prim_radf->Fill(radf);
        m_prim_ratio->Fill(nfitntot);
	m_ppsi->Fill(t->psi);
        m_ptanl->Fill(t->tanl);
        m_prim_theta->Fill(theta);
	m_peta->Fill(eta);
	m_ppT->Fill(pT);
        m_pmom->Fill(gmom);
	m_plength->Fill(t->length);
        m_prim_impact->Fill(t->impact);
       	m_pchisq0->Fill(chisq0);
	m_pchisq1->Fill(chisq1);

	m_ppT_eta_rec->Fill(eta,lmevpt);
        m_primtrk_xf_yf->Fill(t->x_first[0],t->x_first[1]);
        m_ptanl_zf->Fill(t->x_first[2],t->tanl);
	m_pmom_trklength->Fill(t->length,lmevmom);
        m_peta_trklength->Fill(eta,t->length);
	m_pnpoint_length->Fill(t->length,Float_t(t->n_point));
	m_pfpoint_length->Fill(t->length,Float_t(t->n_fit_point));
	m_pchisq0_mom->Fill(lmevmom,chisq0);
	m_pchisq1_mom->Fill(lmevmom,chisq1);
	m_pchisq0_eta->Fill(eta,chisq0);
	m_pchisq1_eta->Fill(eta,chisq1);
	m_pchisq0_dip->Fill(t->tanl,chisq0);
	m_pchisq1_dip->Fill(t->tanl,chisq1);
	m_pchisq0_zf->Fill(t->x_first[2],chisq0);
	m_pchisq1_zf->Fill(t->x_first[2],chisq1);
        m_pnfptonpt_mom->Fill(lmevmom,nfitntot);
        m_pnfptonpt_eta->Fill(eta,nfitntot);

      }
    }
    m_primtrk_good->Fill(cnttrkg);
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

	    m_H_pT_eta_gen->Fill(eta, Glmevpt);
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
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistV0(){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_v0_vertex histograms " << endl;

  St_DataSetIter dstI(dst);         
  
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) dstI["dst_v0_vertex"];

  if (dst_v0_vertex) {
    dst_v0_vertex_st *v0 = dst_v0_vertex->GetTable();

    Int_t cntrows=0;
    cntrows = dst_v0_vertex->GetNRows();
    m_v0->Fill(cntrows);

    Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
    Float_t m_pimass2 = (0.139567*0.139567);
    for (Int_t k=0; k<dst_v0_vertex->GetNRows(); k++, v0++){
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
      m_ev0_lama_hist->Fill(inv_mass_la);
      m_ev0_k0ma_hist->Fill(inv_mass_k0);   
    }
  }
}

//_____________________________________________________________________________

void St_QA_Maker::MakeHistPID(){
  if (Debug()) cout << " *** in St_QA_Maker - filling PID histograms " << endl;
  
  St_DataSetIter dstI(dst);        
  
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
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistVertex(){
  if (Debug()) cout << " *** in St_QA_Maker - filling vertex histograms " << endl;

  St_DataSetIter dstI(dst);

  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];
  
  if (vertex) {

    Int_t cntrows=0;
    cntrows = vertex->GetNRows();
    m_v_num->Fill(cntrows);

    dst_vertex_st  *t   = vertex->GetTable();
    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      //         if (t->iflag>0) {  
      if (t->iflag==1 && t->vtx_id==1){                           // plot of primary vertex only
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
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistXi(){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_xi_vertex histograms " << endl;
    
  St_DataSetIter dstI(dst);           

  St_dst_xi_vertex *xi = (St_dst_xi_vertex*) dstI["dst_xi_vertex"];
  if (xi) {

    Int_t cntrows=0;
    cntrows = xi->GetNRows();
    m_xi_tot->Fill(cntrows);

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
    m_pnt_tot->Fill(cntrows);

  }

}


//_____________________________________________________________________________
void St_QA_Maker::MakeHistKink(){
  if (Debug()) cout << " *** in St_QA_Maker - filling kink histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_dst_tkf_vertex *pt = (St_dst_tkf_vertex*) dstI["kinkVertex"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_kink_tot->Fill(cntrows);

  }

}


//_____________________________________________________________________________
void St_QA_Maker::MakeHistL3(){
  if (Debug()) cout << " *** in St_QA_Maker - filling L3 histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_tpt_track *pt = (St_tpt_track*) dstI["l3Track"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_l3_tot->Fill(cntrows);

  }

}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistV0Eval(){
  if (Debug()) cout << " *** in St_QA_Maker - filling ev0_eval histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_ev0_eval *pt = (St_ev0_eval*) dstI["ev0_eval"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_v0eval_tot->Fill(cntrows);

  }

}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistRich(){
  if (Debug()) cout << " *** in St_QA_Maker - filling Rich histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_g2t_rch_hit *pt = (St_g2t_rch_hit*) dstI["g2t_rch_hit"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_rich_tot->Fill(cntrows);

  }

}


//_____________________________________________________________________________



