// $Id: StQABookHist.cxx,v 1.19 1999/12/16 23:12:03 kathy Exp $ 
// $Log: StQABookHist.cxx,v $
// Revision 1.19  1999/12/16 23:12:03  kathy
// fixed list of default histograms, had wrong title and added a few for tables and stevent; rescaled some histograms and fixed titles in booking
//
// Revision 1.18  1999/12/16 19:52:36  kathy
// fix hist titles in QABookHist; unpack n_point,n_fit_point,n_max_point correctly for globtrk table - must still fix for primtrk table - in St_QA_Maker
//
// Revision 1.17  1999/12/15 20:32:17  kathy
// separated the tpc and tpc+svt histograms for globtrk table; had to book and fill new histograms, add histograms to default logy list AND had to change what values of iflag I cut on for filling each different type of track in makehistglob method
//
// Revision 1.16  1999/12/15 18:31:05  kathy
// added 4 new histogram to globtrk for tpc - r0,phi0,z0,curvature; also put 3 of these in default logY list; also changed scale on iflag hist. for globtrk & primtrk
//
// Revision 1.15  1999/12/15 17:17:32  kathy
// changed the dedx histograms to the scale GeV/cm - which is the scale in the dst table
//
// Revision 1.14  1999/12/14 18:33:23  kathy
// removed 4 ftpc histograms as per Janet's request
//
// Revision 1.13  1999/12/12 23:09:47  kathy
// add pt vs eta in ftpc histogram as per Janet
//
// Revision 1.12  1999/12/12 17:17:24  kathy
// fixed limits on ftpc  histograms
//
// Revision 1.11  1999/12/10 22:28:19  kathy
// fix limits of evsum histogram
//
// Revision 1.10  1999/12/10 18:11:18  kathy
// changed limits of some histograms so we can at least see all the data; changed booking order for globtrk histograms
//
// Revision 1.9  1999/12/10 17:38:24  kathy
// now reprint canvas on each page of postscript output file; also changed some histogram limits
//
// Revision 1.8  1999/12/08 22:58:17  kathy
// changed histogram limits and made names smaller
//
// Revision 1.7  1999/12/07 23:14:18  kathy
// fix primary vtx histograms for dst tables; split apart the ftpc and tpc in the dedx histograms
//
// Revision 1.6  1999/12/07 16:50:43  kathy
// fix limits on histograms
//
// Revision 1.5  1999/12/06 22:25:05  kathy
// split apart the tpc and ftpc (east & west) histograms for the globtrk table; had to add characters to end of each histogram pointer to differentiate the different ones; updated the default list of hist to be plotted with logy scale
//
// Revision 1.4  1999/11/29 21:50:36  kathy
// remove St_QATestTables_Maker class - not used anywhere; remove SetDraw method from StQABookHist method - not needed
//
// Revision 1.3  1999/11/23 19:00:51  lansdell
// Reorganized Make() and include files (Gene)
//
// Revision 1.2  1999/11/22 22:46:41  lansdell
// update to identify histogram method used (StEvent or DST tables) by Gene; StEventQAMaker code partially completed (run bfcread_dst_EventQAhist.C)
//
// Revision 1.1  1999/11/19 22:44:42  kathy
// took histogram booking out of St_QA_Maker as per Thomas' request and put it into separate class StQABookHist which can now be used also by Curtis' class to book histograms - thanks for your help Gene!
// 
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQABookHist abstract base class for QA Histogram Makers             //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "TH1.h"
#include "TH2.h"
#include "StQABookHist.h"

const Int_t   StQABookHist::nxpT = 50;
const Int_t   StQABookHist::nyeta = 50;
const Float_t StQABookHist::xminpT = 0.0;
const Float_t StQABookHist::xmaxpT = 5.0;
//const Float_t StQABookHist::ymineta = -2.0;
//const Float_t StQABookHist::ymaxeta =  2.0;

const Int_t StQABookHist::nchisq = 50;
//const Int_t StQABookHist::nmass  = 40;
//const Int_t StQABookHist::ntau   = 40;
const Int_t StQABookHist::ndedx  = 50;
const Int_t StQABookHist::npnt   = 50;
const Int_t StQABookHist::nleng  = 50;
const Int_t StQABookHist::npsi   = 36;
const Int_t StQABookHist::knpsi  = 42;
const Int_t StQABookHist::ntrk   = 50;
const Int_t StQABookHist::nvrt   = 100;
const Int_t StQABookHist::nmnpt  = 50;
const Int_t StQABookHist::nmneta = 50;
const Int_t StQABookHist::nxyz   = 50;
const Int_t StQABookHist::knyeta = 60;
const Int_t StQABookHist::knid   = 10;
const Int_t StQABookHist::cnp   = 50;
const Int_t StQABookHist::cndedx = 50;    

const Float_t StQABookHist::kminnid  = 0.0;
const Float_t StQABookHist::kmaxnid  = 10.0;
const Float_t StQABookHist::minpsi   = 0.0;
const Float_t StQABookHist::kminpsi  = -60.0;
const Float_t StQABookHist::maxpsi   = 360.0;
const Float_t StQABookHist::minchisq = 0.;
const Float_t StQABookHist::maxchisq = 10.0;
const Float_t StQABookHist::minmass  = 0.0;
const Float_t StQABookHist::maxmass  = 2.0;
const Float_t StQABookHist::minpnt   = 0.0;
const Float_t StQABookHist::maxpnt   = 50.0;
const Float_t StQABookHist::minleng  = 0.0;
const Float_t StQABookHist::maxleng  = 200.0;
const Float_t StQABookHist::mintau   = 0.0;
const Float_t StQABookHist::maxtau   = 20.0;
const Float_t StQABookHist::mintrk   = 0.0;
const Float_t StQABookHist::maxtrk   = 8000.0;
const Float_t StQABookHist::minvrt   = 2000.0;
const Float_t StQABookHist::maxvrt   = 4000.0;
const Float_t StQABookHist::minmpt   = 0.0;
const Float_t StQABookHist::maxmpt   = 2.0;
const Float_t StQABookHist::minmeta  = -0.2;
const Float_t StQABookHist::maxmeta  = 0.2;
const Float_t StQABookHist::kmineta  = -3.0;
const Float_t StQABookHist::kmaxeta  = 3.0;
const Float_t StQABookHist::minxyz   = 0.0;
const Float_t StQABookHist::maxxyz   = 50.0;
const Float_t StQABookHist::cminp = 0.0;
const Float_t StQABookHist::cmaxp = 2.0;
const Float_t StQABookHist::cmindedx = 0.0;
const Float_t StQABookHist::cmaxdedx =  0.1e-04*1e6; // change from GeV to keV per cm

ClassImp(StQABookHist)
  
//_____________________________________________________________________________
StQABookHist::StQABookHist(const char *name, const char *title, const char* type)
     : StMaker(name,title), QAHistType(type) {

//  - zero all pointers defined in the header file

// for method MakeEvSum - from table event_summary
  m_trk_tot_gd = 0;       //! number of good global tracks divided by total
  m_glb_trk_tot=0;        //! # tracks total from globtrk
  m_glb_trk_plusminus=0;  //! # trks pos/neg. 
  m_glb_trk_prim=0;        //! # trks from primaries
  m_vert_total=0;    //! total number of vertices
  //  m_vert_V0=0;       //! number of V0 vertices
  m_mean_pt=0;       //! mean pt value
  m_mean_eta=0;      //! mean eta value 
  m_rms_eta=0;       //! rms eta value 
  //  m_T_average=0;     //! mean Temp
  m_prim_vrtx0=0;    //! primary vrtx x position
  m_prim_vrtx1=0;    //! primary vrtx y position
  m_prim_vrtx2=0;    //! primary vrtx z position
  //  m_vrtx_chisq=0;    //! primary vrtx chisq
  
// for method MakeGlob - from table globtrk

  m_globtrk_tot=0;
  m_globtrk_good=0;
  m_globtrk_iflag=0;
  m_det_id=0;
  m_pointT=0;
  m_pointFE=0;
  m_pointFW=0;
  m_max_pointT=0;
  m_max_pointFE=0;
  m_max_pointFW=0;
  m_fit_pointT=0;
  m_fit_pointFE=0;
  m_fit_pointFW=0;
  m_glb_ratioT=0;
  m_glb_ratioFE=0;
  m_glb_ratioFW=0;
  m_glb_ratiomT=0;
  m_glb_ratiomFE=0;
  m_glb_ratiomFW=0;
  m_glb_chargeT=0;
  m_glb_chargeFE=0;
  m_glb_chargeFW=0;
  m_glb_r0T=0;
  m_glb_phi0T=0;
  m_glb_z0T=0;
  m_glb_curvT=0;
  m_glb_xf0=0;
  m_glb_xfT=0;
  m_glb_xfFE=0;
  m_glb_xfFW=0;
  m_glb_yf0=0;
  m_glb_yfT=0;  
  m_glb_yfFE=0;  
  m_glb_yfFW=0;     
  m_glb_zf0=0;     
  m_glb_zfT=0; 
  m_glb_zfFE=0; 
  m_glb_zfFW=0;
  m_glb_radfT=0;
  m_glb_radfFE=0;
  m_glb_radfFW=0;
  m_psiT=0;  
  m_psiFE=0;  
  m_psiFW=0;        
  m_tanlT=0;   
  m_glb_thetaT=0;  
  m_etaT=0;    
  m_etaFE=0;    
  m_etaFW=0;        
  m_pTT=0;
  m_pTFE=0;
  m_pTFW=0;
  m_momT=0; 
  m_momFE=0; 
  m_momFW=0;        
  m_lengthT=0;  
  m_lengthFE=0;  
  m_lengthFW=0;  
  m_chisq0T=0;     
  m_chisq0FE=0;     
  m_chisq0FW=0;     
  m_chisq1T=0;
  m_chisq1FE=0;
  m_chisq1FW=0;     
  m_glb_impactT=0; 

  m_pointTS=0;        
  m_max_pointTS=0;    
  m_fit_pointTS=0;    
  m_glb_ratioTS=0;    
  m_glb_ratiomTS=0;   
  m_glb_chargeTS=0;   
  m_glb_r0TS=0;       
  m_glb_phi0TS=0;     
  m_glb_z0TS=0;       
  m_glb_curvTS=0;     
  m_glb_xfTS=0;       
  m_glb_yfTS=0;       
  m_glb_zfTS=0;       
  m_glb_xf0TS=0;      
  m_glb_yf0TS=0;      
  m_glb_zf0TS=0;      
  m_glb_radfTS=0;     
  m_psiTS=0;          
  m_tanlTS=0;         
  m_glb_thetaTS=0;    
  m_etaTS=0;          
  m_momTS=0;          
  m_pTTS=0;           
  m_lengthTS=0;       
  m_chisq0TS=0;       
  m_chisq1TS=0;       
  m_glb_impactTS=0;   

  m_pT_eta_recT = 0;
  m_pT_eta_recFE = 0;
  m_pT_eta_recFW = 0;
  m_globtrk_xf_yfT = 0;
  m_globtrk_xf_yfFE = 0;
  m_globtrk_xf_yfFW = 0;
  m_tanl_zfT  = 0;
  m_mom_trklengthT = 0;
  m_eta_trklengthT = 0;
  m_eta_trklengthFE = 0;
  m_eta_trklengthFW = 0;
  m_npoint_lengthT = 0;	
  m_npoint_lengthFE = 0;	
  m_npoint_lengthFW = 0;		  
  m_fpoint_lengthT = 0;
  m_fpoint_lengthFE = 0;
  m_fpoint_lengthFW = 0;
  m_chisq0_momT = 0;
  m_chisq1_momT = 0;
  m_chisq0_etaT = 0;
  m_chisq1_etaT = 0;
  m_chisq0_dipT = 0;
  m_chisq1_dipT = 0;
  m_chisq0_zfT = 0;
  m_chisq1_zfT = 0;
  m_nfptonpt_momT = 0;
  m_nfptonpt_etaT = 0;

  m_pT_eta_recTS= 0;
  m_globtrk_xf_yfTS = 0;
  m_tanl_zfTS  = 0;
  m_mom_trklengthTS = 0;
  m_eta_trklengthTS = 0;
  m_npoint_lengthTS = 0;	
  m_fpoint_lengthTS = 0;
  m_chisq0_momTS = 0;
  m_chisq1_momTS = 0;
  m_chisq0_etaTS = 0;
  m_chisq1_etaTS = 0;
  m_chisq0_dipTS = 0;
  m_chisq1_dipTS = 0;
  m_chisq0_zfTS = 0;
  m_chisq1_zfTS = 0;
  m_nfptonpt_momTS = 0;
  m_nfptonpt_etaTS = 0;

  
// for method MakeDE - from table dst_dedx
  m_ndedxr=0;        //! number of tracks with dedx info

  m_ndedxT=0;         //! number of point to find dE/dx
  m_dedx0T=0;         //! dE/dx [0] - mean
  m_dedx1T=0;         //! dE/dx [1] - sigma
  
  m_ndedxFE=0;         //! number of point to find dE/dx
  m_dedx0FE=0;         //! dE/dx [0] - mean
  m_dedx1FE=0;         //! dE/dx [1] - sigma
  
  m_ndedxFW=0;         //! number of point to find dE/dx
  m_dedx0FW=0;         //! dE/dx [0] - mean
  m_dedx1FW=0;         //! dE/dx [1] -sigma
  
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
  m_v0           =0; //! # v0 vertices
  m_ev0_lama_hist=0; //! Lambda mass
  m_ev0_k0ma_hist=0; //! K0 mass  

  // for MakeHistPID - from tables primtrk & dst_dedx 
  m_p_dedx_rec=0;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  m_v_num        =0; //! number of vertices
  m_v_detid=0; //! detector id where vertex was found 
  m_v_vtxid=0; //! vertex type
  m_v_x=0;     //! vertex coordinates in
  m_v_y=0;     //!  STAR reference 
  m_v_z=0;     //!   system
  m_v_pchi2=0; //! chisq per dof of vertex fit
  
  m_pv_detid=0; //! row1-detector id where vertex was found 
  m_pv_vtxid=0; //! row1-vertex type
  m_pv_x=0;     //! row1-vertex coordinates in
  m_pv_y=0;     //!  STAR reference 
  m_pv_z=0;     //!   system
  m_pv_pchi2=0; //! row1-chisq per dof of vertex fit
  

  // for method MakeHistXi
    m_xi_tot=0;   //! number of vertices
  
  // for method MakeHistPoint
    m_pnt_tot=0;   //! number of tpc hits
  
  // for method MakeHistKink
    m_kink_tot=0;   //! number of kinks
  
  // for method MakeHistL3
    m_l3_tot=0;   //! number of l3 tracks
  
  // for method MakeHistV0Eval
    m_v0eval_tot=0;   //! number of vertices
  
  // for method MakeHistRich
    m_rich_tot=0;   //! number of rich hits

}
//_____________________________________________________________________________
Int_t StQABookHist::Init(){
 
  cout << " In StQABookHist::Init  " << endl;

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
  BookHistPoint();
  BookHistKink();
  BookHistL3();
  BookHistV0Eval();
  BookHistRich();
  
  return StMaker::Init();

}
//_____________________________________________________________________________
Int_t StQABookHist::Make(){

  // Call methods to fill histograms
  
  // histograms from table event_summary
  MakeHistEvSum();
  // histograms from table globtrk
  MakeHistGlob();
  // histograms from table dst_dedx
  MakeHistDE();
  // histograms from table primtrk
  MakeHistPrim();
  // histograms from table particle
  MakeHistGen();  
  // histograms from table dst_v0_vertex
  MakeHistV0();
  // histograms from table primtrk & dst_dedx
  MakeHistPID();
  // histograms from table dst_vertex
  MakeHistVertex();
  // histograms from table dst_xi_vertex
  MakeHistXi();
  // histograms from table point
  MakeHistPoint();
  // histograms from table kinkVertex
  MakeHistKink();
  // histograms from table l3Track
  MakeHistL3();
  // histograms from table ev0_eval
  MakeHistV0Eval();
  // histograms from table g2t_rch_hit
  MakeHistRich();

  return kStOK;

}
//_____________________________________________________________________________
StQABookHist::~StQABookHist(){

}
//_____________________________________________________________________________
const char* StQABookHist::NameIt(const char* name) {

  return ((QAHistName=QAHistType) += name).Data();

}
//_____________________________________________________________________________
const char* StQABookHist::TitleIt(const char* name) {

  return (((QAHistTitle=QAHistType) += " ") += name).Data();

}
//_____________________________________________________________________________
TH1F* StQABookHist::QAH1F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup) {

  return (new TH1F(NameIt(name),TitleIt(title),nbinsx,xlow,xup));

}
//_____________________________________________________________________________
TH2F* StQABookHist::QAH2F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup,
   Int_t nbinsy, Axis_t ylow, Axis_t yup) {

  return (new TH2F(NameIt(name),TitleIt(title),nbinsx,xlow,xup,nbinsy,ylow,yup));

}
//_____________________________________________________________________________

void StQABookHist::BookHistEvSum(){
  
// for method MakeEvSum - from table event_summary

  m_trk_tot_gd    = QAH1F("QaEvsumTrkGoodDTotal",
    "evsum: num good track over total",  55,0.,1.1);
    m_trk_tot_gd->SetXTitle("number of good/total tracks");

  m_glb_trk_tot   = QAH1F("QaEvsumTrkTot","evsum: num tracks total ",
                             ntrk, 0., 10000.);
  m_glb_trk_plusminus  = QAH1F("QaEvsumPlusMinusTrk", "evsum: num pos. over neg trks",
                             ntrk,0.8,1.4);
  m_glb_trk_prim    = QAH1F("QaEvsumTrkPrim","evsum: num good tracks from primaries ",
                             ntrk, mintrk, maxtrk);
	  
  m_vert_total = QAH1F("QaEvsumVertTot", "evsum: total num of vertices",80,0.,8000.);
  //  m_vert_V0    = QAH1F("QaEvsumVertV0", "evsum: num V0 vertices",80,0.,8000.); 
 
  m_mean_pt    = QAH1F("QaEvsumMeanPt",   "evsum: mean pt", nmnpt, 0., 2.0);
  m_mean_eta   = QAH1F("QaEvsumMeanEta",  "evsum: mean eta", nmneta, -0.25,0.25);
  m_rms_eta    = QAH1F("QaEvsumRmsEta",   "evsum: rms eta", nmneta, -2.5,2.5);
  //  m_T_average  = QAH1F("QaEvsumMeanTemp", "evsum: mean Temp", nmneta, 0., 0.5);
  m_prim_vrtx0 = QAH1F("QaEvsumPrimVertX","evsum: X of primary vertex", 40, -1.,1.);
  m_prim_vrtx1 = QAH1F("QaEvsumPrimVertY","evsum: Y of primary vertex", 40,-1.,1.);
  m_prim_vrtx2 = QAH1F("QaEvsumPrimVertZ","evsum: Z of primary vertex", nxyz,-50., 50.);
  //  m_vrtx_chisq = QAH1F("QaEvsumVrtxChisq","evsum: chisq of primary vertex",nchisq, 0., 10.); 
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistGlob(){
  
// for method MakeGlob - from table globtrk

// 1D general

  m_globtrk_tot   = QAH1F("QaGtrkTot",  "globtrk: tot # tracks",40,0.,10000.);
  m_globtrk_iflag = QAH1F("QaGtrkFlag", "globtrk: iflag ",160,-799.,801.);

  m_globtrk_good  = QAH1F("QaGtrkGood", "globtrk: tot # good tracks",40,0.,10000.);  
  m_det_id        = QAH1F("QaGtrkDetId",   "globtrk: Detector ID for tracks",25,0.,25.);


// 1D tpc

  m_pointT      = QAH1F("QaGtrkNPntT",   "globtrk: N points on trk,tpc", 50, 0.,50.);
  m_max_pointT  = QAH1F("QaGtrkNPntMaxT","globtrk: N max pnts on trk, tpc", 50, 0.,100.);
  m_fit_pointT  = QAH1F("QaGtrkNPntFitT","globtrk: N fit pnts on trk, tpc", 50, 0.,50.);
  m_glb_ratioT  = QAH1F("QaGtrkRnfT",    "globtrk: ratio Nfit/tot pnt, tpc", 55, 0., 1.1);
  m_glb_ratiomT = QAH1F("QaGtrkRnmT",    "globtrk: ratio Nfit/max pnt, tpc", 55, 0., 1.1);
  m_glb_chargeT = QAH1F("QaGtrkChrgT",   "globtrk: charge, tpc ", 20,-2.,2.);
  m_glb_r0T     = QAH1F("QaGtrkR0T",     "globtrk: radius at start (cm), tpc ", 50,0.,200.);
  m_glb_phi0T   = QAH1F("QaGtrkPhi0T",   "globtrk: azimuth ang at start (deg), tpc ", 64,0.,360.);
  m_glb_z0T     = QAH1F("QaGtrkZ0T",     "globtrk: z-coord at start (cm), tpc ", 50, -300.,300.);
  m_glb_curvT   = QAH1F("QaGtrkCurvT",   "globtrk: curvature (1/cm), tpc ", 50,0.,.1);
  m_glb_xfT     = QAH1F("QaGtrkXfT",     "globtrk: x of first hit on trk, tpc", 50,-200.,200.);
  m_glb_xf0     = QAH1F("QaGtrkXf0",     "globtrk: x of first hit - on helix at start, tpc",50,-5.,5.);
  m_glb_yfT     = QAH1F("QaGtrkYfT",     "globtrk: y of first hit on trk, tpc", 50,-200.,200.);
  m_glb_yf0     = QAH1F("QaGtrkYf0",     "globtrk: y of first hit - on helix at start, tpc",50,-5.,5.);
  m_glb_zfT     = QAH1F("QaGtrkZfT",     "globtrk: z of first hit on trk, tpc", 50,-300.,300.);
  m_glb_zf0     = QAH1F("QaGtrkZf0",     "globtrk: z of first hit - on helix at start, tpc",50,-5.,5.);
  m_glb_radfT   = QAH1F("QaGtrkRT",      "globtrk: radial position of first hit, tpc", 50,0.,200.);
  m_lengthT     = QAH1F("QaGtrkLengthT", "globtrk: track length, tpc", 50,0.,300.);
  m_psiT        = QAH1F("QaGtrkPsiT",    "globtrk: psi, tpc", 64, 0.,360.);
  m_tanlT       = QAH1F("QaGtrkTanlT",   "globtrk: tanl, tpc",32,-4.,4.);
  m_glb_thetaT  = QAH1F("QaGtrkThetaT",  "globtrk: theta, tpc",20,0.,4.);
  m_etaT        = QAH1F("QaGtrkEtaT",    "globtrk: eta, tpc",60,-6.,6.);
  m_pTT         = QAH1F("QaGtrkPtT",     "globtrk: pT, tpc",50,0.,5.);
  m_momT        = QAH1F("QaGtrkPT",      "globtrk: momentum, tpc",50,0.,5.);
  m_chisq0T     = QAH1F("QaGtrkChisq0T", "globtrk: chisq0 - xy, tpc", 50, 0.,15.);
  m_chisq1T     = QAH1F("QaGtrkChisq1T", "globtrk: chisq1 - z, tpc", 50, 0.,15.);
  m_glb_impactT = QAH1F("QaGtrkImpactT", "globtrk: impact param from prim vtx, tpc  ", 50,0.,500.);



// 2D - tpc

  m_pT_eta_recT = QAH2F("QaGtrkPtVsEtaT","globtrk: log pT vs eta, tpc", 20,-2.,2.,40,1.,4.);
    m_pT_eta_recT->SetXTitle("eta");
    m_pT_eta_recT->SetYTitle(" log pT (MeV)");

  m_globtrk_xf_yfT = QAH2F("QaGtrkXfYfT",  "globtrk: Y vs X of first hit on trk, tpc", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yfT->SetXTitle("x first");
    m_globtrk_xf_yfT->SetYTitle("y first");


  m_tanl_zfT = QAH2F("QaGtrkTanlzf","globtrk: tanl(dip) versus zfirst, tpc",50,-250.,250.,60,-3.,3.);
    m_tanl_zfT->SetXTitle("zfirst");
    m_tanl_zfT->SetYTitle("tanl");

  m_mom_trklengthT = QAH2F("QaGtrkPVsTrkLength","globtrk: log mom vs trk length, tpc",
			     50,0.,250.,40,1.,4.);
    m_mom_trklengthT->SetXTitle("trk length");  
    m_mom_trklengthT->SetYTitle("log P (MeV)");

  m_eta_trklengthT = QAH2F("QaGtrkLengthVEtaT","globtrk: trk length vs eta, tpc",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklengthT->SetXTitle("eta");
    m_eta_trklengthT->SetYTitle("length");

  m_npoint_lengthT = QAH2F("QaGtrkNPntLengthT","globtrk: N pnts vs length, tpc",
			     25,0.,250.,25,0.,50.);
    m_npoint_lengthT->SetXTitle("trk length");
    m_npoint_lengthT->SetYTitle("Npoints on trk");

  m_fpoint_lengthT = QAH2F("QaGtrkFitPntLengthT","globtrk: N fit pnts vs length, tpc",
			     25,0.,250.,25,0.,50.);
    m_fpoint_lengthT->SetXTitle("trk length");
    m_fpoint_lengthT->SetYTitle("Npoints on trk");

  m_chisq0_momT = QAH2F("QaGtrkChi0MomT","globtrk: Chisq0 vs log mom, tpc",40,1.,4.,50,0.,10.);
    m_chisq0_momT->SetXTitle("log P (MeV)");
    m_chisq0_momT->SetYTitle("chisq0") ;

  m_chisq1_momT = QAH2F("QaGtrkChi1MomT","globtrk: Chisq1 vs log mom, tpc",40,1.,4.,50,0.,10.);
    m_chisq1_momT->SetXTitle("log P (MeV)");
    m_chisq1_momT->SetYTitle("chisq1");

  m_chisq0_etaT = QAH2F("QaGtrkChi0EtaT","globtrk: Chisq0 vs eta, tpc",20,-2.,2.,20,0.,10.);
    m_chisq0_etaT->SetXTitle("eta");
    m_chisq0_etaT->SetYTitle("chisq0");

  m_chisq1_etaT = QAH2F("QaGtrkChi1EtaT","globtrk: Chisq1 vs eta, tpc",20,-2.,2.,20,0.,10.);
    m_chisq1_etaT->SetXTitle("eta");
    m_chisq1_etaT->SetYTitle("chisq1");

  m_chisq0_dipT = QAH2F("QaGtrkChi0TanlT","globtrk: Chisq0 vs tanl(dip), tpc",20,-5.,5.,20,0.,10.);
    m_chisq0_dipT->SetXTitle("dip angle");
    m_chisq0_dipT->SetYTitle("chisq0");

  m_chisq1_dipT = QAH2F("QaGtrkChi1TanlT","globtrk: Chisq1 vs tanl(dip), tpc",20,-5.,5.,20,0.,10.);
    m_chisq1_dipT->SetXTitle("dip angle");
    m_chisq1_dipT->SetYTitle("chisq1");

  m_chisq0_zfT = QAH2F("QaGtrkChi0zfT","globtrk: Chisq0 vs zfirst, tpc",20,-250.,250.,20,0.,10.);
    m_chisq0_zfT->SetXTitle("zfirst");
    m_chisq0_zfT->SetYTitle("chisq0");

  m_chisq1_zfT = QAH2F("QaGtrkChi1zfT","globtrk: Chisq1 vs zfirst, tpc",20,-250.,250.,20,0.,10.);
    m_chisq1_zfT->SetXTitle("zfirst");
    m_chisq1_zfT->SetYTitle("chisq1");

  m_nfptonpt_momT = QAH2F("QaGtrkRPntMomT","globtrk: ratio Nfitpnt,Npnt vs log mom., tpc",40,1.,4.,50,0.,1.2005); 
     m_nfptonpt_momT->SetXTitle("log P (MeV)");
     m_nfptonpt_momT->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_etaT = QAH2F("QaGtrkRPntEtaT","globtrk: ratio Nfitpnt,Npnt vs Eta, tpc",40,-2.,2.,50,0.,1.2005); 
     m_nfptonpt_etaT->SetXTitle("eta");
     m_nfptonpt_etaT->SetYTitle("Ratio Nfitpnt/Npnt");


// 1D tpc + silicon (svt+ssd)

  m_pointTS      = QAH1F("QaGtrkNPntTS",   "globtrk: N points on trk,tpc+svt", 50, 0.,100.);
  m_max_pointTS  = QAH1F("QaGtrkNPntMaxTS","globtrk: N max pnts on trk, tpc+svt", 50, 0.,100.);
  m_fit_pointTS  = QAH1F("QaGtrkNPntFitTS","globtrk: N fit pnts on trk, tpc+svt", 50, 0.,100.);
  m_glb_ratioTS  = QAH1F("QaGtrkRnfTS",    "globtrk: ratio Nfit/tot pnt, tpc+svt", 55, 0., 1.1);
  m_glb_ratiomTS = QAH1F("QaGtrkRnmTS",    "globtrk: ratio Nfit/max pnt, tpc+svt", 55, 0., 1.1);
  m_glb_chargeTS = QAH1F("QaGtrkChrgTS",   "globtrk: charge, tpc+svt ", 20,-2.,2.);
  m_glb_r0TS     = QAH1F("QaGtrkR0TS",     "globtrk: radius at start (cm), tpc+svt", 50,0.,500.);
  m_glb_phi0TS   = QAH1F("QaGtrkPhi0TS",   "globtrk: azimuth ang at start (deg), tpc+svt", 36,0.,360.);
  m_glb_z0TS     = QAH1F("QaGtrkZ0TS",     "globtrk: z-coord at start (cm), tpc+svt", 50, -300.,300.);
  m_glb_curvTS   = QAH1F("QaGtrkCurvTS",   "globtrk: curvature (1/cm), tpc+svt", 50,0.,.1);
  m_glb_xfTS     = QAH1F("QaGtrkXfTS",     "globtrk: x of first hit on trk, tpc+svt", 50,-200.,200.);
  m_glb_xf0TS    = QAH1F("QaGtrkXf0TS",    "globtrk: x of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_yfTS     = QAH1F("QaGtrkYfTS",     "globtrk: y of first hit on trk, tpc+svt", 50,-200.,200.);
  m_glb_yf0TS    = QAH1F("QaGtrkTf0TS",    "globtrk: y of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_zfTS     = QAH1F("QaGtrkZfTS",     "globtrk: z of first hit on trk, tpc+svt", 50,-300.,300.);
  m_glb_zf0TS    = QAH1F("QaGtrkZf0TS",    "globtrk: z of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_radfTS   = QAH1F("QaGtrkRTS",      "globtrk: radial position of first hit, tpc+svt", 50,0.,200.);
  m_lengthTS     = QAH1F("QaGtrkLengthTS", "globtrk: track length, tpc+svt", 50,0.,300.);
  m_psiTS        = QAH1F("QaGtrkPsiTS",    "globtrk: psi, tpc+svt", 36, 0.,360.);
  m_tanlTS       = QAH1F("QaGtrkTanlTS",   "globtrk: tanl, tpc+svt",32,-4.,4.);
  m_glb_thetaTS  = QAH1F("QaGtrkThetaTS",  "globtrk: theta, tpc+svt",20,0.,4.);
  m_etaTS        = QAH1F("QaGtrkEtaTS",    "globtrk: eta, tpc+svt",60,-6.,6.);
  m_pTTS         = QAH1F("QaGtrkPtTS",     "globtrk: pT, tpc+svt",50,0.,5.);
  m_momTS        = QAH1F("QaGtrkPTS",      "globtrk: momentum, tpc+svt",50,0.,5.);
  m_chisq0TS     = QAH1F("QaGtrkChisq0TS", "globtrk: chisq0 - xy, tpc+svt", 50, 0.,15.);
  m_chisq1TS     = QAH1F("QaGtrkChisq1TS", "globtrk: chisq1 - z, tpc+svt", 50, 0.,15.);
  m_glb_impactTS = QAH1F("QaGtrkImpactTS", "globtrk: impact param from prim vtx, tpc+svt", 50,0.,500.);


// 2D - tpc + sillicon (svt + ssd)

  m_pT_eta_recTS = QAH2F("QaGtrkPtVsEtaTS","globtrk: log pT vs eta, tpc+svt", 20,-2.,2.,40,1.,4.);
    m_pT_eta_recTS->SetXTitle("eta");
    m_pT_eta_recTS->SetYTitle(" log pT (MeV)");

  m_globtrk_xf_yfTS = QAH2F("QaGtrkXfYfTS",  "globtrk: Y vs X of first hit on trk, tpc+svt", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yfTS->SetXTitle("x first");
    m_globtrk_xf_yfTS->SetYTitle("y first");


  m_tanl_zfTS = QAH2F("QaGtrkTanlzfTS","globtrk: tanl(dip) versus zfirst, tpc+svt",50,-250.,250.,60,-3.,3.);
    m_tanl_zfTS->SetXTitle("zfirst");
    m_tanl_zfTS->SetYTitle("tanl");

  m_mom_trklengthTS = QAH2F("QaGtrkPVsTrkLTS","globtrk: log mom vs trk length, tpc+svt",
			     50,0.,250.,40,1.,4.);
    m_mom_trklengthTS->SetXTitle("trk length");  
    m_mom_trklengthTS->SetYTitle("log P (MeV)");

  m_eta_trklengthTS = QAH2F("QaGtrkLVEtaTS","globtrk: trk length vs eta, tpc+svt",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklengthTS->SetXTitle("eta");
    m_eta_trklengthTS->SetYTitle("length");

  m_npoint_lengthTS = QAH2F("QaGtrkNPntLTS","globtrk: N pnts vs length, tpc+svt",
			     25,0.,250.,25,0.,50.);
    m_npoint_lengthTS->SetXTitle("trk length");
    m_npoint_lengthTS->SetYTitle("Npoints on trk");

  m_fpoint_lengthTS = QAH2F("QaGtrkFitPntLTS","globtrk: N fit pnts vs length, tpc+svt",
			     25,0.,250.,25,0.,50.);
    m_fpoint_lengthTS->SetXTitle("trk length");
    m_fpoint_lengthTS->SetYTitle("Npoints on trk");

  m_chisq0_momTS = QAH2F("QaGtrkChi0MomTS","globtrk: Chisq0 vs log mom, tpc+svt",40,1.,4.,50,0.,10.);
    m_chisq0_momTS->SetXTitle("log P (MeV)");
    m_chisq0_momTS->SetYTitle("chisq0") ;

  m_chisq1_momTS = QAH2F("QaGtrkChi1MomTS","globtrk: Chisq1 vs log mom, tpc+svt",40,1.,4.,50,0.,10.);
    m_chisq1_momTS->SetXTitle("log P (MeV)");
    m_chisq1_momTS->SetYTitle("chisq1");

  m_chisq0_etaTS = QAH2F("QaGtrkChi0EtaTS","globtrk: Chisq0 vs eta, tpc+svt",20,-2.,2.,20,0.,10.);
    m_chisq0_etaTS->SetXTitle("eta");
    m_chisq0_etaTS->SetYTitle("chisq0");

  m_chisq1_etaTS = QAH2F("QaGtrkChi1EtaTS","globtrk: Chisq1 vs eta, tpc+svt",20,-2.,2.,20,0.,10.);
    m_chisq1_etaTS->SetXTitle("eta");
    m_chisq1_etaTS->SetYTitle("chisq1");

  m_chisq0_dipTS = QAH2F("QaGtrkChi0TanlTS","globtrk: Chisq0 vs tanl(dip), tpc+svt",20,-5.,5.,20,0.,10.);
    m_chisq0_dipTS->SetXTitle("dip angle");
    m_chisq0_dipTS->SetYTitle("chisq0");

  m_chisq1_dipTS = QAH2F("QaGtrkChi1TanlTS","globtrk: Chisq1 vs tanl(dip), tpc+svt",20,-5.,5.,20,0.,10.);
    m_chisq1_dipTS->SetXTitle("dip angle");
    m_chisq1_dipTS->SetYTitle("chisq1");

  m_chisq0_zfTS = QAH2F("QaGtrkChi0zfTS","globtrk: Chisq0 vs zfirst, tpc+svt",20,-250.,250.,20,0.,10.);
    m_chisq0_zfTS->SetXTitle("zfirst");
    m_chisq0_zfTS->SetYTitle("chisq0");

  m_chisq1_zfTS = QAH2F("QaGtrkChi1zfTS","globtrk: Chisq1 vs zfirst, tpc+svt",20,-250.,250.,20,0.,10.);
    m_chisq1_zfTS->SetXTitle("zfirst");
    m_chisq1_zfTS->SetYTitle("chisq1");

  m_nfptonpt_momTS = QAH2F("QaGtrkRPntMomTS","globtrk: ratio Nfitpnt,Npnt vs log mom., tpc+svt",40,1.,4.,50,0.,1.2005); 
     m_nfptonpt_momTS->SetXTitle("log P (MeV)");
     m_nfptonpt_momTS->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_etaTS = QAH2F("QaGtrkRPntEtaTS","globtrk: ratio Nfitpnt,Npnt vs Eta, tpc+svt",40,-2.,2.,50,0.,1.2005); 
     m_nfptonpt_etaTS->SetXTitle("eta");
     m_nfptonpt_etaTS->SetYTitle("Ratio Nfitpnt/Npnt");


// 1D ftpc

  m_pointFE      = QAH1F("QaGtrkNPntFE",    "globtrk: N points on trk,ftpc east", 15, 0.,15.);
  m_pointFW      = QAH1F("QaGtrkNPntFW",    "globtrk: N points on trk,ftpc west", 15, 0.,15.);
  m_max_pointFE  = QAH1F("QaGtrkNPntMaxFE", "globtrk: N max pnts on trk, ftpc east", 15, 0.,15.);
  m_max_pointFW  = QAH1F("QaGtrkNPntMaxFW", "globtrk: N max pnts on trk, ftpc west", 15, 0.,15.);
  m_fit_pointFE  = QAH1F("QaGtrkNPntFitFE", "globtrk: N fit pnts on trk, ftpc east", 15, 0.,15.);
  m_fit_pointFW  = QAH1F("QaGtrkNPntFitFW", "globtrk: N fit pnts on trk, ftpc west", 15, 0.,15.);
  m_glb_ratioFE  = QAH1F("QaGtrkRnfFE",     "globtrk: ratio Nfit/tot pnt, ftpc east", 55, 0., 1.1);
  m_glb_ratioFW  = QAH1F("QaGtrkRnfFW",     "globtrk: ratio Nfit/tot pnt, ftpc west", 55, 0., 1.1);
  m_glb_ratiomFE = QAH1F("QaGtrkRnmFE",     "globtrk: ratio Nfit/max pnt, ftpc east", 55, 0., 1.1);
  m_glb_ratiomFW = QAH1F("QaGtrkRnmFW",     "globtrk: ratio Nfit/max pnt, ftpc west", 55, 0., 1.1);
  m_glb_chargeFE = QAH1F("QaGtrkChrgFE",    "globtrk: charge, ftpc east ", 20,-2.,2.);
  m_glb_chargeFW = QAH1F("QaGtrkChrgFW",    "globtrk: charge, ftpc west ", 20,-2.,2.);
  m_glb_xfFE     = QAH1F("QaGtrkXfFE",      "globtrk: x of first hit on trk, ftpc east", 50,-200.,200.);
  m_glb_xfFW     = QAH1F("QaGtrkXfFW",      "globtrk: x of first hit on trk, ftpc west", 50,-200.,200.);
  m_glb_yfFE     = QAH1F("QaGtrkYfFE",      "globtrk: y of first hit on trk, ftpc east", 50,-200.,200.);
  m_glb_yfFW     = QAH1F("QaGtrkYfFW",      "globtrk: y of first hit on trk, ftpc west", 50,-200.,200.);
  m_glb_zfFE     = QAH1F("QaGtrkZfFE",      "globtrk: z of first hit on trk, ftpc east", 50,-300.,300.);
  m_glb_zfFW     = QAH1F("QaGtrkZfFW",      "globtrk: z of first hit on trk, ftpc west", 50,-300.,300.);
  m_glb_radfFE   = QAH1F("QaGtrkRFE",       "globtrk: radial position of first hit, ftpc east", 40,0.,40.);
  m_glb_radfFW   = QAH1F("QaGtrkRFW",       "globtrk: radial position of first hit, ftpc west", 40,0.,40.);
  m_lengthFE     = QAH1F("QaGtrkLengthFE",  "globtrk: track length, ftpc east", 60,0.,120.);
  m_lengthFW     = QAH1F("QaGtrkLengthFW",  "globtrk: track length, ftpc west", 60,0.,120.);
  m_psiFE        = QAH1F("QaGtrkPsiFE",     "globtrk: psi, ftpc east", 36, 0.,360.);
  m_psiFW        = QAH1F("QaGtrkPsiFW",     "globtrk: psi, ftpc west", 36, 0.,360.);
  m_etaFE        = QAH1F("QaGtrkEtaFE",     "globtrk: eta, ftpc east",80,-8.,8.);
  m_etaFW        = QAH1F("QaGtrkEtaFW",     "globtrk: eta, ftpc west",80,-8.,8.);
  m_pTFE         = QAH1F("QaGtrkPtFE",      "globtrk: pT, ftpc east",50,0.,5.);
  m_pTFW         = QAH1F("QaGtrkPtFW",      "globtrk: pT, ftpc west",50,0.,5.);
  m_momFE        = QAH1F("QaGtrkPFE",       "globtrk: momentum, ftpc east ",50,0.,5.);
  m_momFW        = QAH1F("QaGtrkPFW",       "globtrk: momentum, ftpc west ",50,0.,5.);
  m_chisq0FE     = QAH1F("QaGtrkChisq0FE",  "globtrk: chisq0 - xy, ftpc east", 50, 0.,5000.);
  m_chisq0FW     = QAH1F("QaGtrkChisq0FW",  "globtrk: chisq0 - xy, ftpc west", 50, 0.,5000.);
  m_chisq1FE     = QAH1F("QaGtrkChisq1FE",  "globtrk: chisq1 - z, ftpc east", 50, 0.,500.);
  m_chisq1FW     = QAH1F("QaGtrkChisq1FW",  "globtrk: chisq1 - z, ftpc west", 50, 0.,500.);

// 2D - ftpc

   m_pT_eta_recFE = 
      QAH2F("QaGtrkPtVsEtaFE","globtrk: log pT vs eta, ftpcE",20,-5.,0.,40,1.,4.);
   m_pT_eta_recFW = 
      QAH2F("QaGtrkPtVsEtaFW","globtrk: log pT vs eta, ftpcW",20,0.,5.,40,1.,4.);

  m_globtrk_xf_yfFE = QAH2F("QaGtrkXfYfFE","globtrk: Y vs X of first hit on trk, ftpcE", 40,-40.,40.,40,-40.,40.);
    m_globtrk_xf_yfFE->SetXTitle("x first");
    m_globtrk_xf_yfFE->SetYTitle("y first");
  m_globtrk_xf_yfFW = QAH2F("QaGtrkXfYfFW","globtrk: Y vs X of first hit on trk, ftpcW", 40,-40.,40.,40,-40.,40.);
    m_globtrk_xf_yfFW->SetXTitle("x first");
    m_globtrk_xf_yfFW->SetYTitle("y first");

  m_eta_trklengthFE = QAH2F("QaGtrkLengthVEtaFE","globtrk: trk length vs eta, ftpcE",
			     25,-4.5,-2.,30,0.,120.);
    m_eta_trklengthFE->SetXTitle("eta");
    m_eta_trklengthFE->SetYTitle("length");
  m_eta_trklengthFW = QAH2F("QaGtrkLengthVEtaFW","globtrk: trk length vs eta, ftpcW",
			     25,2.,4.5,30,0.,120.);
    m_eta_trklengthFW->SetXTitle("eta");
    m_eta_trklengthFW->SetYTitle("length");

  m_npoint_lengthFE = QAH2F("QaGtrkNPntLengthFE","globtrk: N pnts vs length, ftpcE",
			     30,0.,120.,15,0.,15.);
    m_npoint_lengthFE->SetXTitle("trk length");
    m_npoint_lengthFE->SetYTitle("Npoints on trk");
  m_npoint_lengthFW = QAH2F("QaGtrkNPntLengthFW","globtrk: N pnts vs length, ftpcW",
			     30,0.,120.,15,0.,15.);
    m_npoint_lengthFW->SetXTitle("trk length");
    m_npoint_lengthFW->SetYTitle("Npoints on trk");

  m_fpoint_lengthFE = QAH2F("QaGtrkFitPntLengthFE","globtrk: N fit pnts vs length, ftpcE",
			     30,0.,120.,15,0.,15.);
    m_fpoint_lengthFE->SetXTitle("trk length");
    m_fpoint_lengthFE->SetYTitle("Npoints on trk");
  m_fpoint_lengthFW = QAH2F("QaGtrkFitPntLengthFW","globtrk: N fit pnts vs length, ftpcW",
			     30,0.,120.,15,0.,15.);
    m_fpoint_lengthFW->SetXTitle("trk length");
    m_fpoint_lengthFW->SetYTitle("Npoints on trk");

}
//____________________________________________________
void StQABookHist::BookHistPrim(){

// for method MakeHistPrim - from table primtrk

// 1D
  m_primtrk_tot   = QAH1F("QaPtrkTot",  "primtrk: tot # tracks",40,0.,10000.);
  m_primtrk_iflag = QAH1F("QaPtrkFlag", "primtrk: iflag ",160,-799.,801.);

  m_primtrk_good  = QAH1F("QaPtrkGood",  "primtrk: tot # good tracks",40,0.,10000.);  
  m_pdet_id     = QAH1F("QaPtrkDetId",   "primtrk: Detector ID for tracks",25,0.,25.);
  m_ppoint      = QAH1F("QaPtrkNPnt",    "primtrk: N points on track", 100, 0.,2000.);
  m_pmax_point  = QAH1F("QaPtrkNPntMax", "primtrk: N max points on track", 50, 0.,50.);
  m_pfit_point  = QAH1F("QaPtrkNPntFit", "primtrk: N fit points on track", 50, 0.,50.);
  m_prim_charge = QAH1F("QaPtrkChrg",    "primtrk: charge ", 20,-2.,2.);
  m_prim_xf     = QAH1F("QaPtrkXf",      "primtrk: x of first hit on trk ", 50,-200.,200.);
  m_prim_xf0    = QAH1F("QaPtrkXf0",     "primtrk: x of first hit - on helix at start",50,-200.,200.);
  m_prim_yf     = QAH1F("QaPtrkYf",      "primtrk: y of first hit on trk", 50,-200.,200.);
  m_prim_yf0    = QAH1F("QaPtrkYf0",     "primtrk: y of first hit - on helix at start",50,-200.,200.);
  m_prim_zf     = QAH1F("QaPtrkZf",      "primtrk: z of first hit on trk", 50,-200.,200.);
  m_prim_zf0    = QAH1F("QaPtrkZf0",     "primtrk: z of first hit - on helix at start",50,-200.,200.);
  m_prim_radf   = QAH1F("QaPtrkR",   "primtrk: radial position of first tpc hit",50,0.,250.);
  m_prim_ratio  = QAH1F("QaPtrkRnf",     "primtrk: ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_ppsi        = QAH1F("QaPtrkPsi",     "primtrk: psi distribution", 36, 0.,360.);
  m_ptanl       = QAH1F("QaPtrkTanl",    "primtrk: tanl distribution",32,-4.,4.);
  m_prim_theta  = QAH1F("QaPtrkTheta",   "primtrk: theta distribution",20,0.,4.);
  m_peta        = QAH1F("QaPtrkEta",     "primtrk: eta distribution",60,-3.0,3.0);
  m_ppT         = QAH1F("QaPtrkPt",      "primtrk: pT distribution",50,0.,5.);
  m_pmom        = QAH1F("QaPtrkP",       "primtrk: momentum distribution",50,0.,5.);
  m_pchisq0     = QAH1F("QaPtrkChisq0",  "primtrk: chisq0 - xy", 50, 0.,5.);
  m_pchisq1     = QAH1F("QaPtrkChisq1",  "primtrk: chisq1 - z", 50, 0.,5.);
  m_plength     = QAH1F("QaPtrkLength",  "primtrk: track length", 50,0.,300.);
  m_prim_impact = QAH1F("QaPtrkImpact",  "primtrk: impact param from prim vtx ", 50,0.,0.005);


// 2D
  m_ppT_eta_rec = QAH2F("QaPtrkPtVsEta","primtrk: log pT vs eta", 20,-2.,2.,40,1.,4.);
    m_ppT_eta_rec->SetXTitle("eta");
    m_ppT_eta_rec->SetYTitle(" log pT (MeV)");

  m_primtrk_xf_yf = QAH2F("QaPtrkXfYf","primtrk: Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yf->SetXTitle("x first");
    m_primtrk_xf_yf->SetYTitle("y first");


  m_ptanl_zf = QAH2F("QaPtrkTanlzf","primtrk: tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_ptanl_zf->SetXTitle("zfirst");
    m_ptanl_zf->SetYTitle("tanl");


  m_pmom_trklength = QAH2F("QaPtrkPVsTrkLength","primtrk: log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_pmom_trklength->SetXTitle("trk length");  
    m_pmom_trklength->SetYTitle("log P (MeV)");

  m_peta_trklength = QAH2F("QaPtrkLengthVEta","primtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_peta_trklength->SetXTitle("eta");
    m_peta_trklength->SetYTitle("length");


  m_pnpoint_length = QAH2F("QaPtrkNPntLength","primtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pnpoint_length->SetXTitle("length");
    m_pnpoint_length->SetYTitle("Npoints");

  m_pfpoint_length = QAH2F("QaPtrkFitPntLength","primtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pfpoint_length->SetXTitle("length");
    m_pfpoint_length->SetYTitle("Npoints");

  m_pchisq0_mom = QAH2F("QaPtrkChi0Mom","primtrk: Chisq0 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq0_mom->SetXTitle("log P (MeV)");
    m_pchisq0_mom->SetYTitle("chisq0") ;

  m_pchisq1_mom = QAH2F("QaPtrkChi1Mom","primtrk: Chisq1 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq1_mom->SetXTitle("log P (MeV)");
    m_pchisq1_mom->SetYTitle("chisq1");


  m_pchisq0_eta = QAH2F("QaPtrkChi0Eta","primtrk: Chisq0 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq0_eta->SetXTitle("eta");
    m_pchisq0_eta->SetYTitle("chisq0");

  m_pchisq1_eta = QAH2F("QaPtrkChi1Eta","primtrk: Chisq1 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq1_eta->SetXTitle("eta");
    m_pchisq1_eta->SetYTitle("chisq1");

  m_pchisq0_dip = QAH2F("QaPtrkChi0Tanl","primtrk: Chisq0 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq0_dip->SetXTitle("dip angle");
    m_pchisq0_dip->SetYTitle("chisq0");

  m_pchisq1_dip = QAH2F("QaPtrkChi1Tanl","primtrk: Chisq1 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq1_dip->SetXTitle("dip angle");
    m_pchisq1_dip->SetYTitle("chisq1");

  m_pchisq0_zf = QAH2F("QaPtrkChi0zf","primtrk: Chisq0 vs zfirst",20,-250.,250.,25,0.,5.);
    m_pchisq0_zf->SetXTitle("zfirst");
    m_pchisq0_zf->SetYTitle("chisq0");

  m_pchisq1_zf = QAH2F("QaPtrkChi1zf","primtrk: Chisq1 vs zfirst",20,-250.,250.,25,0.,7.);
    m_pchisq1_zf->SetXTitle("zfirst");
    m_pchisq1_zf->SetYTitle("chisq1");

  m_pnfptonpt_mom = QAH2F("QaPtrkRPntMom","primtrk: ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_pnfptonpt_mom->SetXTitle("log P (MeV)");
     m_pnfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_pnfptonpt_eta = QAH2F("QaPtrkRPntEta","primtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_pnfptonpt_eta->SetXTitle("eta");
     m_pnfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");

}
//_____________________________________________________________________________
void StQABookHist::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedxr  = QAH1F("QaDedxNum",     "dedx: number of tracks", 50,0., 10000.); 

  m_ndedxT   = QAH1F("QaDedxNdedxT", "dedx: number of point to define dE/dx, tpc", 50,0., 50.);  
  m_dedx0T   = QAH1F("QaDedxDedx0T", "dedx: dE/dx mean (GeV/cm), tpc", ndedx, 0., 0.0005);
  m_dedx1T   = QAH1F("QaDedxDedx1T", "dedx: dE/dx sigma, tpc", ndedx, 0., 0.005);
  
  m_ndedxFE   = QAH1F("QaDedxNdedxFE", "dedx: number of point to define dE/dx, ftpcE", 20,0., 20.);  
  m_dedx0FE   = QAH1F("QaDedxDedx0FE", "dedx: dE/dx mean (GeV/cm), ftpcE", ndedx,  0., 0.0005);
  m_dedx1FE   = QAH1F("QaDedxDedx1FE", "dedx: dE/dx sigma, ftpcE", ndedx,  0., 0.005);
  
  m_ndedxFW   = QAH1F("QaDedxNdedxFW", "dedx: number of point to define dE/dx, ftpcW", 20,0., 20.);  
  m_dedx0FW   = QAH1F("QaDedxDedx0FW", "dedx: dE/dx mean (GeV/cm), ftpcW", ndedx,  0., 0.0005);
  m_dedx1FW   = QAH1F("QaDedxDedx1FW", "dedx: dE/dx sigma, ftpcW", ndedx, 0., 0.005);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistGen(){

  // for MakeHistGen - from table particle
  m_H_npart   = QAH1F("QaEvgenNPart",      "particle:total num particles (generated)",100,0.,30000.);
  m_H_ncpart  = QAH1F("QaEvgenNChgPart",   "particle:num chg (e,mu,pi,K,p) part (generated)",100,0.,20000.);
  m_H_pT_gen  = QAH1F("QaEvgenPt",         "particle: charged pt (generated)",nxpT,xminpT,xmaxpT);
  m_H_eta_gen = QAH1F("QaEvgenEta",        "particle: charged eta (generated)",60,-6.,6.);
  m_H_pT_eta_gen = QAH2F("QaEvgenPtVsEta", "particle: charged pT versus eta (generated)",
			    20,-2.,2.,40,1.,4.);
  m_H_pT_eta_gen->SetXTitle("eta");
  m_H_pT_eta_gen->SetYTitle("pT (GeV)");
  m_H_vtxx    = QAH1F("QaEvgenVtxX","particle: Generator prod vertex x (mm)",50,-1.,1.);
  m_H_vtxy    = QAH1F("QaEvgenVtxY","particle: Generator prod vertex y (mm)",50,-1.,1.);
  m_H_vtxz    = QAH1F("QaEvgenVtxZ","particle: Generator prod vertex z (mm)",50,-5.,5.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistV0(){
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_v0             = QAH1F("QaV0Vtx","dst_v0_vertex: Number V0 found ",50,0.,10000.);
  m_ev0_lama_hist  = QAH1F("QaV0LambdaMass","dst_v0_vertex: Lambda mass",50,1.05,1.15);
  m_ev0_k0ma_hist  = QAH1F("QaV0K0Mass","dst_v0_vertex: k0 mass",50,.4,.6);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistPID(){
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  
  m_p_dedx_rec = QAH2F("QaPidPrimtrkDstdedxPVsDedx","PID: primtrk-dst_dedx,  p vs dedx (reconstructed)",
			  cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistVertex(){
  // for MakeHistVertex - from table dst_vertex
  
  
  m_v_num   = QAH1F("QaVtxNum"," vertex: num vertices ",50,0.,10000.);
  m_v_detid = QAH1F("QaVtxDetId"," vertex: Detector ID ",100,0.,10000.);
  m_v_vtxid = QAH1F("QaVtxVtxId"," vertex: Vertex ID ",10,0.,10.);
  m_v_x     = QAH1F("QaVtxX"," vertex: x ",50,-250.,250.);
  m_v_y     = QAH1F("QaVtxY"," vertex: y ",50,-250.,250.);
  m_v_z     = QAH1F("QaVtxZ"," vertex: z ",50,-250.,250.);
  m_v_pchi2 = QAH1F("QaVtxChisq"," vertex: chisq/dof ",50,0.,50.);
  
  m_pv_detid = QAH1F("QaVtxPrDetId"," vertex,prim: Detector ID ",40,0.,40.);
  m_pv_vtxid = QAH1F("QaVtxPrVtxId"," vertex,prim: Vertex ID ",10,0.,10.);
  m_pv_x     = QAH1F("QaVtxPrX"," vertex,prim: x ",50,-5.,5.);
  m_pv_y     = QAH1F("QaVtxPrY"," vertex,prim: y ",50,-5.,5.);
  m_pv_z     = QAH1F("QaVtxPrZ"," vertex,prim: z ",50,-50.,50.);
  m_pv_pchi2 = QAH1F("QaVtxPrChisq"," vertex,prim: chisq/dof ",40,0.,20.);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistXi(){

  m_xi_tot   = QAH1F("QaXiVtxTot",  "dst_xi_vertex: tot # vertices ",50,0.,2500.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistPoint(){

  m_pnt_tot   = QAH1F("QaPointTot",  "point: # tpc hits ",50,200000.,250000.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistKink(){

  m_kink_tot   = QAH1F("QaKinkTot",  "kinkVertex: # kinks ",25,0.,25.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistL3(){

  m_l3_tot   = QAH1F("QaL3Tot",  "l3Track: # tracks ",50,0.,10000.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistV0Eval(){

  m_v0eval_tot   = QAH1F("QaV0EvalTot",  "ev0_eval: # vertices ",50,0.,5000.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistRich(){

  m_rich_tot   = QAH1F("QaRichTot",  "g2t_rch_hit: multiplicity ",50,0.,2000.);

}
//_____________________________________________________________________________
