// $Id: St_QA_Maker.cxx,v 1.51 1999/09/21 15:05:36 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
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
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// St_QA_Maker class for Makers (evr + egr + ev0 + ev0_eval + event_summary  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "TStyle.h"
#include "TCanvas.h"
#include "TObjString.h"
#include "TPostScript.h"
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"

#include "St_QA_Maker.h"
#include "StHistUtil.h"
#include "StChain.h"
#include "St_DataSetIter.h"

// tables currently in 99e,99f
#include "St_dst_event_summary_Table.h"
#include "St_dst_event_header_Table.h"
#include "St_dst_track_Table.h"   // globtrk,globtrk2,primtrk
#include "St_dst_track_aux_Table.h" //primtrk_aux
#include "St_dst_vertex_Table.h"
#include "St_dst_point_Table.h"
#include "St_dst_v0_vertex_Table.h"
#include "St_dst_xi_vertex_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_particle_Table.h"
#include "St_dst_TrgDet_Table.h"
#include "St_g2t_rch_hit_Table.h"
#include "St_dst_monitor_soft_Table.h"


const Int_t   St_QA_Maker::nxpT = 50;
const Int_t   St_QA_Maker::nyeta = 50;
const Float_t St_QA_Maker::xminpT = 0.0;
const Float_t St_QA_Maker::xmaxpT = 5.0;
const Float_t St_QA_Maker::ymineta = -2.0;
const Float_t St_QA_Maker::ymaxeta =  2.0;

const Int_t St_QA_Maker::nchisq = 50;
const Int_t St_QA_Maker::nmass  = 40;
const Int_t St_QA_Maker::ntau   = 40;
const Int_t St_QA_Maker::ndedx  = 50;
const Int_t St_QA_Maker::npnt   = 50;
const Int_t St_QA_Maker::nleng  = 50;
const Int_t St_QA_Maker::npsi   = 36;
const Int_t St_QA_Maker::knpsi  = 42;
const Int_t St_QA_Maker::ntrk   = 50;
const Int_t St_QA_Maker::nvrt   = 100;
const Int_t St_QA_Maker::nmnpt  = 50;
const Int_t St_QA_Maker::nmneta = 50;
const Int_t St_QA_Maker::nxyz   = 50;
const Int_t St_QA_Maker::knyeta = 60;
const Int_t St_QA_Maker::knid   = 10;
const Int_t St_QA_Maker::cnp   = 50;
const Int_t St_QA_Maker::cndedx = 50;    

const Float_t St_QA_Maker::kminnid  = 0.0;
const Float_t St_QA_Maker::kmaxnid  = 10.0;
const Float_t St_QA_Maker::minpsi   = 0.0;
const Float_t St_QA_Maker::kminpsi  = -60.0;
const Float_t St_QA_Maker::maxpsi   = 360.0;
const Float_t St_QA_Maker::minchisq = 0.;
const Float_t St_QA_Maker::maxchisq = 10.0;
const Float_t St_QA_Maker::minmass  = 0.0;
const Float_t St_QA_Maker::maxmass  = 2.0;
const Float_t St_QA_Maker::mindedx  = 0.0;
const Float_t St_QA_Maker::maxdedx  = 0.0005*1e6; // in keV/cm
const Float_t St_QA_Maker::minpnt   = 0.0;
const Float_t St_QA_Maker::maxpnt   = 50.0;
const Float_t St_QA_Maker::minleng  = 0.0;
const Float_t St_QA_Maker::maxleng  = 200.0;
const Float_t St_QA_Maker::mintau   = 0.0;
const Float_t St_QA_Maker::maxtau   = 20.0;
const Float_t St_QA_Maker::mintrk   = 0.0;
const Float_t St_QA_Maker::maxtrk   = 8000.0;
const Float_t St_QA_Maker::minvrt   = 2000.0;
const Float_t St_QA_Maker::maxvrt   = 4000.0;
const Float_t St_QA_Maker::minmpt   = 0.0;
const Float_t St_QA_Maker::maxmpt   = 2.0;
const Float_t St_QA_Maker::minmeta  = -0.2;
const Float_t St_QA_Maker::maxmeta  = 0.2;
const Float_t St_QA_Maker::kmineta  = -3.0;
const Float_t St_QA_Maker::kmaxeta  = 3.0;
const Float_t St_QA_Maker::minxyz   = 0.0;
const Float_t St_QA_Maker::maxxyz   = 50.0;
const Float_t St_QA_Maker::cminp = 0.0;
const Float_t St_QA_Maker::cmaxp = 2.0;
const Float_t St_QA_Maker::cmindedx = 0.0;
const Float_t St_QA_Maker::cmaxdedx =  0.1e-04*1e6; // change from GeV to keV per cm

ClassImp(St_QA_Maker)
  
//_____________________________________________________________________________
  St_QA_Maker::St_QA_Maker(const char *name, const char *title):StMaker(name,title)
{

// St_QA_Maker - constructor
//  - zero all pointers defined in the header file

// for method MakeEvSum - from table event_summary
  m_trk_tot_gd = 0;       //! number of good global tracks divided by total
  m_glb_trk_tot=0;        //! # tracks total from globtrk
  m_glb_trk_plusminus=0;  //! # trks pos/neg. 
  m_glb_trk_prim=0;        //! # trks from primaries
  m_vert_total=0;    //! total number of vertices
  m_vert_V0=0;       //! number of V0 vertices
  m_mean_pt=0;       //! mean pt value
  m_mean_eta=0;      //! mean eta value 
  m_rms_eta=0;       //! rms eta value 
  m_T_average=0;     //! mean Temp
  m_prim_vrtx0=0;    //! primary vrtx x position
  m_prim_vrtx1=0;    //! primary vrtx y position
  m_prim_vrtx2=0;    //! primary vrtx z position
  m_vrtx_chisq=0;    //! primary vrtx chisq
  
// for method MakeGlob - from table globtrk

  m_globtrk_tot=0;
  m_globtrk_good=0;
  m_globtrk_iflag=0;
  m_det_id=0;
  m_point=0;
  m_max_point=0;
  m_fit_point=0;
  m_glb_charge=0;
  m_glb_xf0=0;
  m_glb_xf=0;
  m_glb_yf0=0;
  m_glb_yf=0;     
  m_glb_zf0=0;     
  m_glb_zf=0;     
  m_psi=0;        
  m_tanl=0;       
  m_glb_theta=0;  
  m_eta=0;        
  m_pT=0;
  m_mom=0;        
  m_chisq0=0;     
  m_chisq1=0;     
  m_length=0;     
  m_glb_impact=0; 
  m_glb_ndf=0;    

  m_pT_eta_rec = 0;
  m_globtrk_xf_yf = 0;
  m_tanl_zf  = 0;
  m_mom_trklength = 0;
  m_eta_trklength = 0;
  m_npoint_length = 0;		  
  m_fpoint_length = 0;
  m_chisq0_mom = 0;
  m_chisq1_mom = 0;
  m_chisq0_eta = 0;
  m_chisq1_eta = 0;
  m_chisq0_dip = 0;
  m_chisq1_dip = 0;
  m_chisq0_zf = 0;
  m_chisq1_zf = 0;
  m_nfptonpt_mom = 0;
  m_nfptonpt_eta = 0;

  
// for method MakeDE - from table dst_dedx
  m_ndedx=0;         //! number of point to find dE/dx
  m_dedx0=0;         //! dE/dx [0]
  m_dedx1=0;         //! dE/dx [1] 
  
// for method MakeHistPrim - from table primtrk

  m_primtrk_tot=0;
  m_primtrk_good=0;
  m_primtrk_iflag=0;
  m_pdet_id=0;
  m_ppoint=0;
  m_pmax_point=0;
  m_pfit_point=0;
  m_prim_charge=0;
  m_prim_xf0=0;
  m_prim_xf=0;
  m_prim_yf0=0;
  m_prim_yf=0;     
  m_prim_zf0=0;     
  m_prim_zf=0;     
  m_ppsi=0;        
  m_ptanl=0;       
  m_prim_theta=0;  
  m_peta=0;        
  m_ppT=0;
  m_pmom=0;        
  m_pchisq0=0;     
  m_pchisq1=0;     
  m_plength=0;     
  m_prim_impact=0; 
  m_prim_ndf=0;    

  m_ppT_eta_rec = 0;
  m_primtrk_xf_yf = 0;
  m_ptanl_zf  = 0;
  m_pmom_trklength = 0;
  m_peta_trklength = 0;
  m_pnpoint_length = 0;		  
  m_pfpoint_length = 0;
  m_pchisq0_mom = 0;
  m_pchisq1_mom = 0;
  m_pchisq0_eta = 0;
  m_pchisq1_eta = 0;
  m_pchisq0_dip = 0;
  m_pchisq1_dip = 0;
  m_pchisq0_zf = 0;
  m_pchisq1_zf = 0;
  m_pnfptonpt_mom = 0;
  m_pnfptonpt_eta = 0;


// for method MakeHistGen - from table particle
  m_H_pT_eta_gen=0; //! pT versus eta Spectra for generated
  m_H_pT_gen=0;     //! pT Spectra for generated
  m_H_eta_gen=0;    //! eta Spectra for generated
  m_H_vtxx=0;       //! production vertex (mm)
  m_H_vtxy=0;       //! production vertex (mm)
  m_H_vtxz=0;       //! production vertex (mm)
  m_H_npart=0;      //! total num particles generated
  m_H_ncpart=0;     //! number of charged e,mu,proton,kaon,pion
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist=0; //! Lambda mass
  m_ev0_k0ma_hist=0; //! K0 mass
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  m_p_dedx_rec=0;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  m_v_detid=0; //! detector id where vertex was found 
  m_v_vtxid=0; //! vertex type
  m_v_x=0;     //! vertex coordinates in
  m_v_y=0;     //!  STAR reference 
  m_v_z=0;     //!   system
  m_v_pchi2=0; //! P(chi^2,ndf) of vertex fit
  
  m_pv_detid=0; //! row1-detector id where vertex was found 
  m_pv_vtxid=0; //! row1-vertex type
  m_pv_x=0;     //! row1-vertex coordinates in
  m_pv_y=0;     //!  STAR reference 
  m_pv_z=0;     //!   system
  m_pv_pchi2=0; //! row1-P(chi^2,ndf) of vertex fit
  

  // for method MakeHistXi

}
//_____________________________________________________________________________

St_QA_Maker::~St_QA_Maker(){

// St_QA_Maker - destructor
//  SafeDelete(m_QACanvas);
//  if (m_ListOfLog) {
//  m_ListOfLog->Delete();
//  SafeDelete(m_ListOfLog);
//}

}


//_____________________________________________________________________________

Int_t St_QA_Maker::Finish() {

// St_QA_Maker - Finish, Draw histograms if SetDraw true
//  Use DrawHists method from StHistUtil
//    - must have pointer to StHistUtil from using SetHistUtil member function
//  if (drawinit)  
//  m_PntrToHistUtil->DrawHists();

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Init(){
// St_QA_Maker - Init; book histograms and set defaults for member functions
    
//book histograms --------------
  BookHistEvSum();
  BookHistGlob();
  BookHistPrim();
  BookHistDE();
  BookHistGen();
  BookHistV0();
  BookHistPID();
  BookHistVertex();
  BookHistXi();

//  Set default values for all methods:
//  SetDraw(kFALSE);

  //  m_PntrToHistUtil->SetHistsNamesDraw();
  //m_PntrToHistUtil->SetZones();
  //m_PntrToHistUtil->SetPaperSize();
  //m_PntrToHistUtil->SetPostScriptFile(); 
  //m_PntrToHistUtil->SetDefaultLogYList();

  return StMaker::Init();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Make(){
// St_QA_Maker - Make; fill histograms
  
  // Call methods to fill histograms
  
  St_DataSet *dst = GetDataSet("dst");  
  
  // histograms from table event_summary
  MakeHistEvSum(dst);
  // histograms from table globtrk
  MakeHistGlob(dst);
  // histograms from table dst_dedx
  MakeHistDE(dst);
  // histograms from table primtrk
  MakeHistPrim(dst);
  // histograms from table particle
  MakeHistGen(dst);  
  // histograms from table dst_v0_vertex
  MakeHistV0(dst);
  // histograms from table primtrk & dst_dedx
  MakeHistPID(dst);
  // histograms from table dst_vertex
  MakeHistVertex(dst);
  MakeHistXi(dst);
  
  return kStOK;
}
//_____________________________________________________________________________

void St_QA_Maker::BookHistEvSum(){
  
 // for method MakeEvSum - from table event_summary
  m_trk_tot_gd    = new TH1F("QaEvsumTrkGoodDTotal","evsum: num good track over total",
                             50,0.9,1.1);
    m_trk_tot_gd->SetXTitle("number of good/total tracks");
  m_glb_trk_tot   = new TH1F("QaEvsumTrkTot","evsum: num tracks total ",
                             ntrk, 0., 10000.);
  m_glb_trk_plusminus  = new TH1F("QaEvsumPlusMinusTrk", "evsum: num pos. over neg trks",
                             ntrk,0.8,1.4);
  m_glb_trk_prim    = new TH1F("QaEvsumTrkPrim","evsum: num good tracks from primaries ",
                             ntrk, mintrk, maxtrk);
	  
  m_vert_total = new TH1F("QaEvsumVertTot", "evsum: total num of vertices",100,0.,5000.);
  m_vert_V0    = new TH1F("QaEvsumVertV0", "evsum: num V0 vertices",100,0.,5000.); 
 
  m_mean_pt    = new TH1F("QaEvsumMeanPt",   "evsum: mean pt", nmnpt, 0., 2.0);
  m_mean_eta   = new TH1F("QaEvsumMeanEta",  "evsum: mean eta", nmneta, -0.25,0.25);
  m_rms_eta    = new TH1F("QaEvsumRmsEta",   "evsum: rms eta", nmneta, -2.5,2.5);
  m_T_average  = new TH1F("QaEvsumMeanTemp", "evsum: mean Temp", nmneta, 0., 0.5);
  m_prim_vrtx0 = new TH1F("QaEvsumPrimVertX","evsum: X of primary vertex", 40, -1.,1.);
  m_prim_vrtx1 = new TH1F("QaEvsumPrimVertY","evsum: Y of primary vertex", 40,-1.,1.);
  m_prim_vrtx2 = new TH1F("QaEvsumPrimVertZ","evsum: Z of primary vertex", nxyz,-50., 50.);
  m_vrtx_chisq = new TH1F("QaEvsumVrtxChisq","evsum: chisq of primary vertex",nchisq, 0., 10.); 
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistGlob(){
  
// for method MakeGlob - from table globtrk

// 1D
  m_globtrk_tot   = new TH1F("QaGlobtrkTot",  "globtrk: tot # tracks",40,0.,10000.);
  m_globtrk_iflag = new TH1F("QaGlobtrkFlag", "globtrk: iflag ",200,-999.,1001.);

  m_globtrk_good  = new TH1F("QaGlobtrkGood", "globtrk: tot # good tracks",40,0.,10000.);  
  m_det_id     = new TH1F("QaGlobtrkDetId",   "globtrk: Detector ID for tracks",11,-0.5,10.5);
  m_point      = new TH1F("QaGlobtrkNPnt",    "globtrk: N points on track", 50, 0.,50.);
  m_max_point  = new TH1F("QaGlobtrkNPntMax", "globtrk: N max points on track", 50, 0.,50.);
  m_fit_point  = new TH1F("QaGlobtrkNPntFit", "globtrk: N fit points on track", 50, 0.,50.);
  m_glb_charge = new TH1F("QaGlobtrkChrg",    "globtrk: charge ", 20,-2.,2.);
  m_glb_xf     = new TH1F("QaGlobtrkXf",      "globtrk: x of first hit on trk", 50,-200.,200.);
  m_glb_xf0    = new TH1F("QaGlobtrkXf0",     "globtrk: x of first hit - on helix at start",50,-20.,20.);
  m_glb_yf     = new TH1F("QaGlobtrkYf",      "globtrk: y of first hit on trk", 50,-200.,200.);
  m_glb_yf0    = new TH1F("QaGlobtrkYf0",     "globtrk: y of first hit - on helix at start",50,-20.,20.);
  m_glb_zf     = new TH1F("QaGlobtrkZf",      "globtrk: z of first hit on trk", 50,-250.,250.);
  m_glb_zf0    = new TH1F("QaGlobtrkZf0",     "globtrk: z of first hit - on helix at start",50,-20.,20.);
  m_glb_radf   = new TH1F("QaGlobtrkR",       "globtrk: radial position of first tpc hit", 50,0.,250.);
  m_glb_ratio  = new TH1F("QaGlobtrkRnf",     "globtrk: ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_psi        = new TH1F("QaGlobtrkPsi",     "globtrk: psi distribution", 36, 0.,360.);
  m_tanl       = new TH1F("QaGlobtrkTanl",    "globtrk: tanl distribution",32,-4.,4.);
  m_glb_theta  = new TH1F("QaGlobtrkTheta",   "globtrk: theta distribution",20,0.,4.);
  m_eta        = new TH1F("QaGlobtrkEta",     "globtrk: eta distribution",60,-3.0,3.0);
  m_pT         = new TH1F("QaGlobtrkPt",      "globtrk: pT distribution",50,0.,5.);
  m_mom        = new TH1F("QaGlobtrkP",       "globtrk: momentum distribution",50,0.,5.);
  m_chisq0     = new TH1F("QaGlobtrkChisq0",  "globtrk: chisq0 - xy", 50, 0.,15.);
  m_chisq1     = new TH1F("QaGlobtrkChisq1",  "globtrk: chisq1 - z", 50, 0.,15.);
  m_length     = new TH1F("QaGlobtrkLength",  "globtrk: track length", 50,0.,300.);
  m_glb_impact = new TH1F("QaGlobtrkImpact",  "globtrk: impact param from prim vtx ", 50,0.,500.);
  m_glb_ndf    = new TH1F("QaGlobtrkNdof",    "globtrk: num deg of freedom", 100,0.,100.);


// 2D
  m_pT_eta_rec = new TH2F("QaGlobtrkPtVsEta","globtrk: log pT versus eta", 20,-2.,2.,40,1.,4.);
    m_pT_eta_rec->SetXTitle("eta");
    m_pT_eta_rec->SetYTitle(" log pT (MeV)");

  m_globtrk_xf_yf = new TH2F("QaGlobtrkXfYf","globtrk: Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yf->SetXTitle("x first");
    m_globtrk_xf_yf->SetYTitle("y first");

  m_tanl_zf = new TH2F("QaGlobtrkTanlzf","globtrk: tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_tanl_zf->SetXTitle("zfirst");
    m_tanl_zf->SetYTitle("tanl");


  m_mom_trklength = new TH2F("QaGlobtrkPVsTrkLength","globtrk: log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_mom_trklength->SetXTitle("trk length");  
    m_mom_trklength->SetYTitle("log P (MeV)");

  m_eta_trklength = new TH2F("QaGlobtrkLengthVEta","globtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklength->SetXTitle("eta");
    m_eta_trklength->SetYTitle("length");


  m_npoint_length = new TH2F("QaGlobtrkNPntLength","globtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_npoint_length->SetXTitle("length");
    m_npoint_length->SetYTitle("Npoints");

  m_fpoint_length = new TH2F("QaGlobtrkFitPntLength","globtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_fpoint_length->SetXTitle("length");
    m_fpoint_length->SetYTitle("Npoints");

  m_chisq0_mom = new TH2F("QaGlobtrkChi0Mom","globtrk: Chisq0 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq0_mom->SetXTitle("log P (MeV)");
    m_chisq0_mom->SetYTitle("chisq0") ;

  m_chisq1_mom = new TH2F("QaGlobtrkChi1Mom","globtrk: Chisq1 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq1_mom->SetXTitle("log P (MeV)");
    m_chisq1_mom->SetYTitle("chisq1");


  m_chisq0_eta = new TH2F("QaGlobtrkChi0Eta","globtrk: Chisq0 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq0_eta->SetXTitle("eta");
    m_chisq0_eta->SetYTitle("chisq0");

  m_chisq1_eta = new TH2F("QaGlobtrkChi1Eta","globtrk: Chisq1 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq1_eta->SetXTitle("eta");
    m_chisq1_eta->SetYTitle("chisq1");


  m_chisq0_dip = new TH2F("QaGlobtrkChi0Tanl","globtrk: Chisq0 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq0_dip->SetXTitle("dip angle");
    m_chisq0_dip->SetYTitle("chisq0");

  m_chisq1_dip = new TH2F("QaGlobtrkChi1Tanl","globtrk: Chisq1 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq1_dip->SetXTitle("dip angle");
    m_chisq1_dip->SetYTitle("chisq1");

  m_chisq0_zf = new TH2F("QaGlobtrkChi0zf","globtrk: Chisq0 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq0_zf->SetXTitle("zfirst");
    m_chisq0_zf->SetYTitle("chisq0");

  m_chisq1_zf = new TH2F("QaGlobtrkChi1zf","globtrk: Chisq1 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq1_zf->SetXTitle("zfirst");
    m_chisq1_zf->SetYTitle("chisq1");

  m_nfptonpt_mom = new TH2F("QaGlobtrkRPntMom","globtrk: ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_nfptonpt_mom->SetXTitle("log P (MeV)");
     m_nfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_eta = new TH2F("QaGlobtrkRPntEta","globtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_nfptonpt_eta->SetXTitle("eta");
     m_nfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");


}


//____________________________________________________
void St_QA_Maker::BookHistPrim(){

// for method MakeHistPrim - from table primtrk

// 1D
  m_primtrk_tot   = new TH1F("QaPrimtrkTot",  "primtrk: tot # tracks",40,0.,10000.);
  m_primtrk_iflag = new TH1F("QaPrimtrkFlag", "primtrk: iflag ",200,-999.,1001.);

  m_primtrk_good  = new TH1F("QaPrimtrkGood",  "primtrk: tot # good tracks",40,0.,10000.);  
  m_pdet_id     = new TH1F("QaPrimtrkDetId",   "primtrk: Detector ID for tracks",11,-0.5,10.5);
  m_ppoint      = new TH1F("QaPrimtrkNPnt",    "primtrk: N points on track", 50, 0.,50.);
  m_pmax_point  = new TH1F("QaPrimtrkNPntMax", "primtrk: N max points on track", 50, 0.,50.);
  m_pfit_point  = new TH1F("QaPrimtrkNPntFit", "primtrk: N fit points on track", 50, 0.,50.);
  m_prim_charge = new TH1F("QaPrimtrkChrg",    "primtrk: charge ", 20,-2.,2.);
  m_prim_xf     = new TH1F("QaPrimtrkXf",      "primtrk: x of first hit on trk ", 50,-200.,200.);
  m_prim_xf0    = new TH1F("QaPrimtrkXf0",     "primtrk: x of first hit - on helix at start",50,-200.,200.);
  m_prim_yf     = new TH1F("QaPrimtrkYf",      "primtrk: y of first hit on trk", 50,-200.,200.);
  m_prim_yf0    = new TH1F("QaPrimtrkYf0",     "primtrk: y of first hit - on helix at start",50,-200.,200.);
  m_prim_zf     = new TH1F("QaPrimtrkZf",      "primtrk: z of first hit on trk", 50,-200.,200.);
  m_prim_zf0    = new TH1F("QaPrimtrkZf0",     "primtrk: z of first hit - on helix at start",50,-200.,200.);
  m_prim_radf   = new TH1F("QaPrimtrkR",   "primtrk: radial position of first tpc hit",50,0.,250.);
  m_prim_ratio  = new TH1F("QaPrimtrkRnf",     "primtrk: ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_ppsi        = new TH1F("QaPrimtrkPsi",     "primtrk: psi distribution", 36, 0.,360.);
  m_ptanl       = new TH1F("QaPrimtrkTanl",    "primtrk: tanl distribution",32,-4.,4.);
  m_prim_theta  = new TH1F("QaPrimtrkTheta",   "primtrk: theta distribution",20,0.,4.);
  m_peta        = new TH1F("QaPrimtrkEta",     "primtrk: eta distribution",60,-3.0,3.0);
  m_ppT         = new TH1F("QaPrimtrkPt",      "primtrk: pT distribution",50,0.,5.);
  m_pmom        = new TH1F("QaPrimtrkP",       "primtrk: momentum distribution",50,0.,5.);
  m_pchisq0     = new TH1F("QaPrimtrkChisq0",  "primtrk: chisq0 - xy", 50, 0.,15.);
  m_pchisq1     = new TH1F("QaPrimtrkChisq1",  "primtrk: chisq1 - z", 50, 0.,15.);
  m_plength     = new TH1F("QaPrimtrkLength",  "primtrk: track length", 50,0.,300.);
  m_prim_impact = new TH1F("QaPrimtrkImpact",  "primtrk: impact param from prim vtx ", 50,0.,500.);
  m_prim_ndf    = new TH1F("QaPrimtrkNdof",    "primtrk: num deg of freedom", 100, 0.,100.);


// 2D
  m_ppT_eta_rec = new TH2F("QaPrimtrkPtVsEta","primtrk: log pT versus eta", 20,-2.,2.,40,1.,4.);
    m_ppT_eta_rec->SetXTitle("eta");
    m_ppT_eta_rec->SetYTitle(" log pT (MeV)");

  m_primtrk_xf_yf = new TH2F("QaPrimtrkXfYf","primtrk: Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yf->SetXTitle("x first");
    m_primtrk_xf_yf->SetYTitle("y first");


  m_ptanl_zf = new TH2F("QaPrimtrkTanlzf","primtrk: tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_ptanl_zf->SetXTitle("zfirst");
    m_ptanl_zf->SetYTitle("tanl");


  m_pmom_trklength = new TH2F("QaPrimtrkPVsTrkLength","primtrk: log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_pmom_trklength->SetXTitle("trk length");  
    m_pmom_trklength->SetYTitle("log P (MeV)");

  m_peta_trklength = new TH2F("QaPrimtrkLengthVEta","primtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_peta_trklength->SetXTitle("eta");
    m_peta_trklength->SetYTitle("length");


  m_pnpoint_length = new TH2F("QaPrimtrkNPntLength","primtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pnpoint_length->SetXTitle("length");
    m_pnpoint_length->SetYTitle("Npoints");

  m_pfpoint_length = new TH2F("QaPrimtrkFitPntLength","primtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pfpoint_length->SetXTitle("length");
    m_pfpoint_length->SetYTitle("Npoints");

  m_pchisq0_mom = new TH2F("QaPrimtrkChi0Mom","primtrk: Chisq0 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq0_mom->SetXTitle("log P (MeV)");
    m_pchisq0_mom->SetYTitle("chisq0") ;

  m_pchisq1_mom = new TH2F("QaPrimtrkChi1Mom","primtrk: Chisq1 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq1_mom->SetXTitle("log P (MeV)");
    m_pchisq1_mom->SetYTitle("chisq1");


  m_pchisq0_eta = new TH2F("QaPrimtrkChi0Eta","primtrk: Chisq0 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq0_eta->SetXTitle("eta");
    m_pchisq0_eta->SetYTitle("chisq0");

  m_pchisq1_eta = new TH2F("QaPrimtrkChi1Eta","primtrk: Chisq1 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq1_eta->SetXTitle("eta");
    m_pchisq1_eta->SetYTitle("chisq1");

  m_pchisq0_dip = new TH2F("QaPrimtrkChi0Tanl","primtrk: Chisq0 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq0_dip->SetXTitle("dip angle");
    m_pchisq0_dip->SetYTitle("chisq0");

  m_pchisq1_dip = new TH2F("QaPrimtrkChi1Tanl","primtrk: Chisq1 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq1_dip->SetXTitle("dip angle");
    m_pchisq1_dip->SetYTitle("chisq1");

  m_pchisq0_zf = new TH2F("QaPrimtrkChi0zf","primtrk: Chisq0 vs zfirst",20,-250.,250.,25,0.,5.);
    m_pchisq0_zf->SetXTitle("zfirst");
    m_pchisq0_zf->SetYTitle("chisq0");

  m_pchisq1_zf = new TH2F("QaPrimtrkChi1zf","primtrk: Chisq1 vs zfirst",20,-250.,250.,25,0.,7.);
    m_pchisq1_zf->SetXTitle("zfirst");
    m_pchisq1_zf->SetYTitle("chisq1");

  m_pnfptonpt_mom = new TH2F("QaPrimtrkRPntMom","primtrk: ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_pnfptonpt_mom->SetXTitle("log P (MeV)");
     m_pnfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_pnfptonpt_eta = new TH2F("QaPrimtrkRPntEta","primtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_pnfptonpt_eta->SetXTitle("eta");
     m_pnfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");

}
 
//_____________________________________________________________________________
void St_QA_Maker::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedx   = new TH1F("QaDstDedxNdedx", "dedx: number of point to define dE/dx", 50,0., 50.);  
  m_dedx0   = new TH1F("QaDstDedxDedx0","dedx: dE/dx[0]", ndedx, mindedx, maxdedx/10.);
  m_dedx1   = new TH1F("QaDstDedxDedx1","dedx: dE/dx[1]", ndedx, mindedx, maxdedx);
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistGen(){
  // for MakeHistGen - from table particle
  m_H_npart   = new TH1F("QaParticleNumPart","total num particles (generated)",100,0.,30000.);
  m_H_ncpart  = new TH1F("QaParticleNumChgPart","num chg (e,mu,pi,K,p) part (generated)",100,0.,20000.);
  m_H_pT_gen  = new TH1F("QaParticlePt","charged: pt (generated)",nxpT,xminpT,xmaxpT);
  m_H_eta_gen = new TH1F("QaParticleEta","charged:eta (generated)",nyeta,-4.,4.);
  m_H_pT_eta_gen = new TH2F("QaParticlePtVsEta","charged:pT versus eta (generated)",
			    nyeta,kmineta,kmaxeta,nxpT,xminpT,xmaxpT);
  m_H_pT_eta_gen->SetXTitle("eta");
  m_H_pT_eta_gen->SetYTitle("pT (GeV)");
  m_H_vtxx    = new TH1F("QaParticleVtxX","Generator prod vertex x (mm)",50,-100.,100.);
  m_H_vtxy    = new TH1F("QaParticleVtxY","Generator prod vertex y (mm)",50,-100.,100.);
  m_H_vtxz    = new TH1F("QaParticleVtxZ","Generator prod vertex z (mm)",50,-500.,500.);
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistV0(){
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist  = new TH1F("QaDstV0VertexLambdaMass","dst_v0_vertex: Lambda mass",50,1.05,1.15);
  m_ev0_k0ma_hist  = new TH1F("QaDstV0VertexK0Mass","dst_v0_vertex: k0 mass",50,.4,.6);
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistPID(){
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  
  m_p_dedx_rec = new TH2F("QaPidPrimtrkDstdedxPVsDedx","primtrk-dst_dedx: p vs dedx (reconstructed)",
			  cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistVertex(){
  // for MakeHistVertex - from table dst_vertex
  
  
  m_v_detid = new TH1F("QaVertexDetId"," vertex: Detector ID ",100,0.,100.);
  m_v_vtxid = new TH1F("QaVertexVtxId"," vertex: Vertex ID ",10,0.,10.);
  m_v_x     = new TH1F("QaVertexX"," vertex: x ",50,-25.,25.);
  m_v_y     = new TH1F("QaVertexY"," vertex: y ",50,-25.,25.);
  m_v_z     = new TH1F("QaVertexZ"," vertex: z ",50,-50.,50.);
  m_v_pchi2 = new TH1F("QaVertexChisq"," vertex: chisq/dof ",50,0.,5.);
  
  m_pv_detid = new TH1F("QaVertexPrDetId"," vertex,prim: Detector ID ",40,0.,40.);
  m_pv_vtxid = new TH1F("QaVertexPrVtxId"," vertex,prim: Vertex ID ",10,0.,10.);
  m_pv_x     = new TH1F("QaVertexPrX"," vertex,prim: x ",50,-1.,1.);
  m_pv_y     = new TH1F("QaVertexPrY"," vertex,prim: y ",50,-1.,1.);
  m_pv_z     = new TH1F("QaVertexPrZ"," vertex,prim: z ",50,-50.,50.);
  m_pv_pchi2 = new TH1F("QaVertexPrChisq"," vertex,prim: chisq/dof ",50,0.,5.);
  
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistXi(){
  
}


//_____________________________________________________________________________
void St_QA_Maker::MakeHistEvSum(St_DataSet *dst){
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
      m_glb_trk_prim->Fill(tt->glb_trk_prim);
      
      m_vert_total->Fill(tt->n_vert_total);
      m_vert_V0->Fill(tt->n_vert_V0);


      m_mean_pt->Fill(tt->mean_pt);
      m_mean_eta->Fill(tt->mean_eta);
      m_rms_eta->Fill(tt->rms_eta);
      m_T_average->Fill(tt->T_average);

      if(!isnan((double)(tt->prim_vrtx[0])))  m_prim_vrtx0->Fill(tt->prim_vrtx[0]);
      if(!isnan((double)(tt->prim_vrtx[1])))  m_prim_vrtx1->Fill(tt->prim_vrtx[1]);
      if(!isnan((double)(tt->prim_vrtx[2])))  m_prim_vrtx2->Fill(tt->prim_vrtx[2]);

      m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 

    }
  }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeHistGlob(St_DataSet *dst){

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
	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = TMath::Log10(pT*1000.0);
	Float_t theta = TMath::ASin(1.) - TMath::ATan(t->tanl);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t gmom  = pT/TMath::Sin(theta);
        Float_t lmevmom = TMath::Log10(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
	Float_t degoffree = t->n_fit_point;
	Float_t chisq0_p = chisq0/(degoffree-3);
	Float_t chisq1_p = chisq1/(degoffree-2);
        Float_t nfitntot = (Float_t(t->n_fit_point))/(Float_t(t->n_point));
        Float_t xdif =  (t->x_first[0])-(t->x0);
        Float_t ydif =  (t->x_first[1])-(t->y0);
        Float_t zdif =  (t->x_first[2])-(t->z0);
        Float_t radf = TMath::Power((t->x_first[0]),2) + 
                       TMath::Power((t->x_first[1]),2);
                radf = TMath::Sqrt(radf); 

// from Lanny on 2 Jul 1999 9:56:03
//1. x0,y0,z0 are coordinates on the helix at the starting point, which
//   should be close to the first TPC hit position assigned to the track.
//   The latter, different quantity is in x_first[3].

// from Helen on 14 Jul 1999 - she now fills chisq0,1 with chisq/dof
// so it doesn't need to be calculated here (juts plotted)

 	m_det_id->Fill(t->det_id);
	m_point->Fill(t->n_point);
	m_max_point->Fill(t->n_max_point);
	m_fit_point->Fill(t->n_fit_point);
        m_glb_charge->Fill(t->icharge);
        m_glb_xf->Fill(t->x_first[0]);
        m_glb_yf->Fill(t->x_first[1]);
        m_glb_zf->Fill(t->x_first[2]);
        m_glb_xf0->Fill(xdif);
        m_glb_yf0->Fill(ydif);
        m_glb_zf0->Fill(zdif);
        m_glb_radf->Fill(radf);
        m_glb_ratio->Fill(nfitntot);
	m_psi->Fill(t->psi);
        m_tanl->Fill(t->tanl);
        m_glb_theta->Fill(theta);
	m_eta->Fill(eta);
	m_pT->Fill(pT);
        m_mom->Fill(gmom);
	m_length->Fill(t->length);
        m_glb_impact->Fill(t->impact);
        m_glb_ndf->Fill(t->ndegf);
	m_chisq0->Fill(chisq0  );
	m_chisq1->Fill(chisq1  );

	m_pT_eta_rec->Fill(eta,lmevpt);
        m_globtrk_xf_yf->Fill(t->x_first[0],t->x_first[1]);
        m_tanl_zf->Fill(t->x_first[2],t->tanl);
	m_mom_trklength->Fill(t->length,lmevmom);
        m_eta_trklength->Fill(eta,t->length);
	m_npoint_length->Fill(t->length,Float_t(t->n_point));
	m_fpoint_length->Fill(t->length,Float_t(t->n_fit_point));
	m_chisq0_mom->Fill(lmevmom,chisq0  );
	m_chisq1_mom->Fill(lmevmom,chisq1  );
	m_chisq0_eta->Fill(eta,chisq0  );
	m_chisq1_eta->Fill(eta,chisq1  );
	m_chisq0_dip->Fill(t->tanl,chisq0  );
	m_chisq1_dip->Fill(t->tanl,chisq1  );
	m_chisq0_zf->Fill(t->x_first[2],chisq0  );
	m_chisq1_zf->Fill(t->x_first[2],chisq1  );
        m_nfptonpt_mom->Fill(lmevmom,nfitntot);
        m_nfptonpt_eta->Fill(eta,nfitntot);

      }
    }
    m_globtrk_good->Fill(cnttrkg);
  }       
}

//_____________________________________________________________________________

void St_QA_Maker::MakeHistDE(St_DataSet *dst) {
  // Fill histograms for dE/dx
  
  St_DataSetIter dstI(dst);
  
  
  St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];
  if(dst_dedx) {
    dst_dedx_st *d = dst_dedx->GetTable();
    for (Int_t i = 0; i < dst_dedx->GetNRows(); i++,d++) {
      m_ndedx->Fill(d->ndedx);
      m_dedx0->Fill(d->dedx[0]*1e6);
      m_dedx1->Fill(d->dedx[1]*1e6);
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistPrim(St_DataSet *dst){

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
	Float_t degoffree = t->n_fit_point;
	Float_t chisq0_p = chisq0/(degoffree-3);
	Float_t chisq1_p = chisq1/(degoffree-2);
        Float_t nfitntot = (Float_t(t->n_fit_point))/(Float_t(t->n_point));
        Float_t xdif = (t->x_first[0]) - (t->x0);
        Float_t ydif = (t->x_first[1]) - (t->y0);
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
        m_prim_ndf->Fill(t->ndegf);
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


void St_QA_Maker::MakeHistGen(St_DataSet *dst){
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
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistV0(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_v0_vertex histograms " << endl;
  St_DataSetIter dstI(dst);         
  
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) dstI["dst_v0_vertex"];
  if (dst_v0_vertex) {
    dst_v0_vertex_st *v0 = dst_v0_vertex->GetTable();
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

void St_QA_Maker::MakeHistPID(St_DataSet *dst){
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


void St_QA_Maker::MakeHistVertex(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling vertex histograms " << endl;
  St_DataSetIter dstI(dst);
  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];
  
  if (vertex) {
    dst_vertex_st  *t   = vertex->GetTable();
    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      //         if (t->iflag>0) {  
      if (i==0){                           // plot of primary vertex only
	m_pv_detid->Fill(t->det_id); 
	m_pv_vtxid->Fill(t->vtx_id);
	if (!isnan(double(t->x))) m_pv_x->Fill(t->x);     
	if (!isnan(double(t->y))) m_pv_y->Fill(t->y);     
	if (!isnan(double(t->z))) m_pv_z->Fill(t->z);     
	m_pv_pchi2->Fill(t->pchi2);
      }
      m_v_detid->Fill(t->det_id); 
      m_v_vtxid->Fill(t->vtx_id);
      if (!isnan(double(t->x))) m_v_x->Fill(t->x);     
      if (!isnan(double(t->y))) m_v_y->Fill(t->y);     
      if (!isnan(double(t->z))) m_v_z->Fill(t->z);     
      m_v_pchi2->Fill(t->pchi2); 
      // }
    }
  }
}
//_____________________________________________________________________________
void St_QA_Maker::MakeHistXi(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_xi_vertex histograms " << endl;
}


