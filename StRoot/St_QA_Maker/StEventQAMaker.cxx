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
//#include "TStyle.h"
//#include "TCanvas.h"
//#include "TObjString.h"
//#include "TPostScript.h"
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "SystemOfUnits.h"

#include "StEventQAMaker.h"
#include "StChain.h"
#include "StEventTypes.h"                           // included b/c use methods

const Int_t   StEventQAMaker::nxpT = 50;
const Int_t   StEventQAMaker::nyeta = 50;
const Float_t StEventQAMaker::xminpT = 0.0;
const Float_t StEventQAMaker::xmaxpT = 5.0;
const Float_t StEventQAMaker::ymineta = -2.0;
const Float_t StEventQAMaker::ymaxeta =  2.0;

const Int_t StEventQAMaker::nchisq = 50;
const Int_t StEventQAMaker::nmass  = 40;
const Int_t StEventQAMaker::ntau   = 40;
const Int_t StEventQAMaker::ndedx  = 50;
const Int_t StEventQAMaker::npnt   = 50;
const Int_t StEventQAMaker::nleng  = 50;
const Int_t StEventQAMaker::npsi   = 36;
const Int_t StEventQAMaker::knpsi  = 42;
const Int_t StEventQAMaker::ntrk   = 50;
const Int_t StEventQAMaker::nvrt   = 100;
const Int_t StEventQAMaker::nmnpt  = 50;
const Int_t StEventQAMaker::nmneta = 50;
const Int_t StEventQAMaker::nxyz   = 50;
const Int_t StEventQAMaker::knyeta = 60;
const Int_t StEventQAMaker::knid   = 10;
const Int_t StEventQAMaker::cnp   = 50;
const Int_t StEventQAMaker::cndedx = 50;    

const Float_t StEventQAMaker::kminnid  = 0.0;
const Float_t StEventQAMaker::kmaxnid  = 10.0;
const Float_t StEventQAMaker::minpsi   = 0.0;
const Float_t StEventQAMaker::kminpsi  = -60.0;
const Float_t StEventQAMaker::maxpsi   = 360.0;
const Float_t StEventQAMaker::minchisq = 0.;
const Float_t StEventQAMaker::maxchisq = 10.0;
const Float_t StEventQAMaker::minmass  = 0.0;
const Float_t StEventQAMaker::maxmass  = 2.0;
const Float_t StEventQAMaker::mindedx  = 0.0;
const Float_t StEventQAMaker::maxdedx  = 0.0005*1e6; // in keV/cm
const Float_t StEventQAMaker::minpnt   = 0.0;
const Float_t StEventQAMaker::maxpnt   = 50.0;
const Float_t StEventQAMaker::minleng  = 0.0;
const Float_t StEventQAMaker::maxleng  = 200.0;
const Float_t StEventQAMaker::mintau   = 0.0;
const Float_t StEventQAMaker::maxtau   = 20.0;
const Float_t StEventQAMaker::mintrk   = 0.0;
const Float_t StEventQAMaker::maxtrk   = 8000.0;
const Float_t StEventQAMaker::minvrt   = 2000.0;
const Float_t StEventQAMaker::maxvrt   = 4000.0;
const Float_t StEventQAMaker::minmpt   = 0.0;
const Float_t StEventQAMaker::maxmpt   = 2.0;
const Float_t StEventQAMaker::minmeta  = -0.2;
const Float_t StEventQAMaker::maxmeta  = 0.2;
const Float_t StEventQAMaker::kmineta  = -3.0;
const Float_t StEventQAMaker::kmaxeta  = 3.0;
const Float_t StEventQAMaker::minxyz   = 0.0;
const Float_t StEventQAMaker::maxxyz   = 50.0;
const Float_t StEventQAMaker::cminp = 0.0;
const Float_t StEventQAMaker::cmaxp = 2.0;
const Float_t StEventQAMaker::cmindedx = 0.0;
const Float_t StEventQAMaker::cmaxdedx =  0.1e-04*1e6; // change from GeV to keV per cm

ClassImp(StEventQAMaker)
  
//_____________________________________________________________________________
  StEventQAMaker::StEventQAMaker(const char *name, const char *title):StMaker(name,title)
{

// StEventQAMaker - constructor
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
  m_ndedxr=0;        //! number of tracks with dedx info
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

StEventQAMaker::~StEventQAMaker(){

}


//_____________________________________________________________________________

Int_t StEventQAMaker::Finish() {

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t StEventQAMaker::Init(){
// StEventQAMaker - Init; book histograms and set defaults for member functions
    
//book histograms --------------
  BookHistEvSum();
  BookHistGlob();
/*
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
*/  

  return StMaker::Init();
}
//_____________________________________________________________________________

Int_t StEventQAMaker::Make(){
// StEventQAMaker - Make; fill histograms
  
 // Call methods to fill histograms

  // Use StEvent instead of St_DataSet directly -CL
  StEvent *event = (StEvent *)GetInputDS("StEvent");
  
  // histograms from table event_summary
  MakeHistEvSum(event);

  // histograms from table globtrk
  MakeHistGlob(event);

/*
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
*/

  
  return kStOK;
}
//_____________________________________________________________________________
void StEventQAMaker::BookHistEvSum(){
  
 // for method MakeEvSum - from table event_summary
  m_trk_tot_gd    = new TH1F("EQaEvsumTrkGoodDTotal","evsum(StE): num good track over total",
                             50,0.,1.0);
    m_trk_tot_gd->SetXTitle("number of good/total tracks");
  m_glb_trk_tot   = new TH1F("EQaEvsumTrkTot","evsum(StE): num tracks total ",
                             ntrk, 0., 10000.);
  m_glb_trk_plusminus  = new TH1F("EQaEvsumPlusMinusTrk", "evsum(StE): num pos. over neg trks",
                             ntrk,0.8,1.4);
  m_glb_trk_prim    = new TH1F("EQaEvsumTrkPrim","evsum(StE): num good tracks from primaries ",
                             ntrk, mintrk, maxtrk);
	  
  m_vert_total = new TH1F("EQaEvsumVertTot", "evsum(StE): total num of vertices",80,0.,8000.);
  m_vert_V0    = new TH1F("EQaEvsumVertV0", "evsum(StE): num V0 vertices",80,0.,8000.); 
 
  m_mean_pt    = new TH1F("EQaEvsumMeanPt",   "evsum(StE): mean pt", nmnpt, 0., 2.0);
  m_mean_eta   = new TH1F("EQaEvsumMeanEta",  "evsum(StE): mean eta", nmneta, -0.25,0.25);
  m_rms_eta    = new TH1F("EQaEvsumRmsEta",   "evsum(StE): rms eta", nmneta, -2.5,2.5);
  m_T_average  = new TH1F("EQaEvsumMeanTemp", "evsum(StE): mean Temp", nmneta, 0., 0.5);
  m_prim_vrtx0 = new TH1F("EQaEvsumPrimVertX","evsum(StE): X of primary vertex", 40, -1.,1.);
  m_prim_vrtx1 = new TH1F("EQaEvsumPrimVertY","evsum(StE): Y of primary vertex", 40,-1.,1.);
  m_prim_vrtx2 = new TH1F("EQaEvsumPrimVertZ","evsum(StE): Z of primary vertex", nxyz,-50., 50.);
  m_vrtx_chisq = new TH1F("EQaEvsumVrtxChisq","evsum(StE): chisq of primary vertex",nchisq, 0., 10.); 
  
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistGlob(){
  
// for method MakeGlob - from table globtrk

// 1D
  m_globtrk_tot   = new TH1F("EQaGlobtrkTot",  "globtrk(StE): tot # tracks",40,0.,10000.);
  m_globtrk_iflag = new TH1F("EQaGlobtrkFlag", "globtrk(StE): iflag ",200,-999.,1001.);

  m_globtrk_good  = new TH1F("EQaGlobtrkGood", "globtrk(StE): tot # good tracks",40,0.,10000.);  
  m_det_id     = new TH1F("EQaGlobtrkDetId",   "globtrk(StE): Detector ID for tracks",11,-0.5,10.5);
  m_point      = new TH1F("EQaGlobtrkNPnt",    "globtrk(StE): N points on track", 50, 0.,50.);
  m_max_point  = new TH1F("EQaGlobtrkNPntMax", "globtrk(StE): N max points on track", 50, 0.,100.);
  m_fit_point  = new TH1F("EQaGlobtrkNPntFit", "globtrk(StE): N fit points on track", 50, 0.,50.);
  m_glb_charge = new TH1F("EQaGlobtrkChrg",    "globtrk(StE): charge ", 20,-2.,2.);
  m_glb_xf     = new TH1F("EQaGlobtrkXf",      "globtrk(StE): x of first hit on trk", 50,-200.,200.);
  m_glb_xf0    = new TH1F("EQaGlobtrkXf0",     "globtrk(StE): x of first hit - on helix at start",50,-20.,20.);
  m_glb_yf     = new TH1F("EQaGlobtrkYf",      "globtrk(StE): y of first hit on trk", 50,-200.,200.);
  m_glb_yf0    = new TH1F("EQaGlobtrkYf0",     "globtrk(StE): y of first hit - on helix at start",50,-20.,20.);
  m_glb_zf     = new TH1F("EQaGlobtrkZf",      "globtrk(StE): z of first hit on trk", 50,-250.,250.);
  m_glb_zf0    = new TH1F("EQaGlobtrkZf0",     "globtrk(StE): z of first hit - on helix at start",50,-20.,20.);
  m_glb_radf   = new TH1F("EQaGlobtrkR",       "globtrk(StE): radial position of first tpc hit", 50,0.,250.);
  m_glb_ratio  = new TH1F("EQaGlobtrkRnf",     "globtrk(StE): ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_psi        = new TH1F("EQaGlobtrkPsi",     "globtrk(StE): psi distribution", 36, 0.,360.);
  m_tanl       = new TH1F("EQaGlobtrkTanl",    "globtrk(StE): tanl distribution",32,-4.,4.);
  m_glb_theta  = new TH1F("EQaGlobtrkTheta",   "globtrk(StE): theta distribution",20,0.,4.);
  m_eta        = new TH1F("EQaGlobtrkEta",     "globtrk(StE): eta distribution",60,-3.0,3.0);
  m_pT         = new TH1F("EQaGlobtrkPt",      "globtrk(StE): pT distribution",50,0.,5.);
  m_mom        = new TH1F("EQaGlobtrkP",       "globtrk(StE): momentum distribution",50,0.,5.);
  m_chisq0     = new TH1F("EQaGlobtrkChisq0",  "globtrk(StE): chisq0 - xy", 50, 0.,15.);
  m_chisq1     = new TH1F("EQaGlobtrkChisq1",  "globtrk(StE): chisq1 - z", 50, 0.,15.);
  m_length     = new TH1F("EQaGlobtrkLength",  "globtrk(StE): track length", 50,0.,300.);
  m_glb_impact = new TH1F("EQaGlobtrkImpact",  "globtrk(StE): impact param from prim vtx ", 50,0.,500.);
  m_glb_ndf    = new TH1F("EQaGlobtrkNdof",    "globtrk(StE): num deg of freedom", 100,0.,100.);


// 2D
  m_pT_eta_rec = new TH2F("EQaGlobtrkPtVsEta","globtrk(StE): log pT versus eta", 20,-2.,2.,40,1.,4.);
    m_pT_eta_rec->SetXTitle("eta");
    m_pT_eta_rec->SetYTitle(" log pT (MeV)");

  m_globtrk_xf_yf = new TH2F("EQaGlobtrkXfYf","globtrk(StE): Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yf->SetXTitle("x first");
    m_globtrk_xf_yf->SetYTitle("y first");

  m_tanl_zf = new TH2F("EQaGlobtrkTanlzf","globtrk(StE): tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_tanl_zf->SetXTitle("zfirst");
    m_tanl_zf->SetYTitle("tanl");


  m_mom_trklength = new TH2F("EQaGlobtrkPVsTrkLength","globtrk(StE): log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_mom_trklength->SetXTitle("trk length");  
    m_mom_trklength->SetYTitle("log P (MeV)");

  m_eta_trklength = new TH2F("EQaGlobtrkLengthVEta","globtrk(StE): trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklength->SetXTitle("eta");
    m_eta_trklength->SetYTitle("length");


  m_npoint_length = new TH2F("EQaGlobtrkNPntLength","globtrk(StE): N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_npoint_length->SetXTitle("length");
    m_npoint_length->SetYTitle("Npoints");

  m_fpoint_length = new TH2F("EQaGlobtrkFitPntLength","globtrk(StE): N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_fpoint_length->SetXTitle("length");
    m_fpoint_length->SetYTitle("Npoints");

  m_chisq0_mom = new TH2F("EQaGlobtrkChi0Mom","globtrk(StE): Chisq0 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq0_mom->SetXTitle("log P (MeV)");
    m_chisq0_mom->SetYTitle("chisq0") ;

  m_chisq1_mom = new TH2F("EQaGlobtrkChi1Mom","globtrk(StE): Chisq1 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq1_mom->SetXTitle("log P (MeV)");
    m_chisq1_mom->SetYTitle("chisq1");


  m_chisq0_eta = new TH2F("EQaGlobtrkChi0Eta","globtrk(StE): Chisq0 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq0_eta->SetXTitle("eta");
    m_chisq0_eta->SetYTitle("chisq0");

  m_chisq1_eta = new TH2F("EQaGlobtrkChi1Eta","globtrk(StE): Chisq1 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq1_eta->SetXTitle("eta");
    m_chisq1_eta->SetYTitle("chisq1");


  m_chisq0_dip = new TH2F("EQaGlobtrkChi0Tanl","globtrk(StE): Chisq0 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq0_dip->SetXTitle("dip angle");
    m_chisq0_dip->SetYTitle("chisq0");

  m_chisq1_dip = new TH2F("EQaGlobtrkChi1Tanl","globtrk(StE): Chisq1 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq1_dip->SetXTitle("dip angle");
    m_chisq1_dip->SetYTitle("chisq1");

  m_chisq0_zf = new TH2F("EQaGlobtrkChi0zf","globtrk(StE): Chisq0 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq0_zf->SetXTitle("zfirst");
    m_chisq0_zf->SetYTitle("chisq0");

  m_chisq1_zf = new TH2F("EQaGlobtrkChi1zf","globtrk(StE): Chisq1 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq1_zf->SetXTitle("zfirst");
    m_chisq1_zf->SetYTitle("chisq1");

  m_nfptonpt_mom = new TH2F("EQaGlobtrkRPntMom","globtrk(StE): ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_nfptonpt_mom->SetXTitle("log P (MeV)");
     m_nfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_eta = new TH2F("EQaGlobtrkRPntEta","globtrk(StE): ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_nfptonpt_eta->SetXTitle("eta");
     m_nfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");


}

/*
//____________________________________________________
void StEventQAMaker::BookHistPrim(){

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
  m_pchisq0     = new TH1F("QaPrimtrkChisq0",  "primtrk: chisq0 - xy", 50, 0.,5.);
  m_pchisq1     = new TH1F("QaPrimtrkChisq1",  "primtrk: chisq1 - z", 50, 0.,5.);
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
void StEventQAMaker::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedxr  = new TH1F("QaDedxNum", "dedx: number of tracks", 50,0., 10000.); 
  m_ndedx   = new TH1F("QaDedxNdedx", "dedx: number of point to define dE/dx", 50,0., 50.);  
  m_dedx0   = new TH1F("QaDedxDedx0","dedx: dE/dx[0]", ndedx, mindedx, maxdedx/10.);
  m_dedx1   = new TH1F("QaDedxDedx1","dedx: dE/dx[1]", ndedx, mindedx, maxdedx);
  
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistGen(){
  // for MakeHistGen - from table particle
  m_H_npart   = new TH1F("QaParticleNumPart","particle:total num particles (generated)",100,0.,30000.);
  m_H_ncpart  = new TH1F("QaParticleNumChgPart","particle:num chg (e,mu,pi,K,p) part (generated)",100,0.,20000.);
  m_H_pT_gen  = new TH1F("QaParticlePt","particle: charged pt (generated)",nxpT,xminpT,xmaxpT);
  m_H_eta_gen = new TH1F("QaParticleEta","particle: charged eta (generated)",nyeta,-5.,5.);
  m_H_pT_eta_gen = new TH2F("QaParticlePtVsEta","particle: charged pT versus eta (generated)",
			    nyeta,kmineta,kmaxeta,nxpT,xminpT,xmaxpT);
  m_H_pT_eta_gen->SetXTitle("eta");
  m_H_pT_eta_gen->SetYTitle("pT (GeV)");
  m_H_vtxx    = new TH1F("QaParticleVtxX","particle: Generator prod vertex x (mm)",50,-10.,10.);
  m_H_vtxy    = new TH1F("QaParticleVtxY","particle: Generator prod vertex y (mm)",50,-10.,10.);
  m_H_vtxz    = new TH1F("QaParticleVtxZ","particle: Generator prod vertex z (mm)",50,-50.,50.);
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistV0(){
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_v0             = new TH1F("QaV0Vtx","dst_v0_vertex: Number V0 found ",50,0.,10000.);
  m_ev0_lama_hist  = new TH1F("QaV0LambdaMass","dst_v0_vertex: Lambda mass",50,1.05,1.15);
  m_ev0_k0ma_hist  = new TH1F("QaV0K0Mass","dst_v0_vertex: k0 mass",50,.4,.6);
  
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistPID(){
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  
  m_p_dedx_rec = new TH2F("QaPidPrimtrkDstdedxPVsDedx","PID: primtrk-dst_dedx,  p vs dedx (reconstructed)",
			  cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistVertex(){
  // for MakeHistVertex - from table dst_vertex
  
  
  m_v_num   = new TH1F("QaVertexNum"," vertex: num vertices ",50,0.,10000.);
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
void StEventQAMaker::BookHistXi(){

  m_xi_tot   = new TH1F("QaXiVertexTot",  "dst_xi_vertex: tot # vertices ",50,0.,2500.);
  
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistPoint(){
  
  m_pnt_tot   = new TH1F("QaPointTot",  "point: # tpc hits ",50,200000.,250000.);
}

//_____________________________________________________________________________
void StEventQAMaker::BookHistKink(){
  
  m_kink_tot   = new TH1F("QaKinkTot",  "kinkVertex: # kinks ",25,0.,25.);

}

//_____________________________________________________________________________
void StEventQAMaker::BookHistL3(){
  
  m_l3_tot   = new TH1F("QaL3Tot",  "l3Track: # tracks ",50,0.,10000.);

}

//_____________________________________________________________________________
void StEventQAMaker::BookHistV0Eval(){
  
  m_v0eval_tot   = new TH1F("QaV0EvalTot",  "ev0_eval: # vertices ",50,0.,5000.);

}

//_____________________________________________________________________________
void StEventQAMaker::BookHistRich(){
  
  m_rich_tot   = new TH1F("QaRichTot",  "g2t_rch_hit: # vertices ",50,0.,2000.);

}
*/
//_____________________________________________________________________________
void StEventQAMaker::MakeHistEvSum(StEvent *event){
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
//      m_glb_trk_prim->Fill(tt->glb_trk_prim);
//      m_T_average->Fill(tt->T_average);    
//      m_vert_V0->Fill(tt->n_vert_V0);
//      m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 

  }
}

//-----------------------------------------------------------------
void StEventQAMaker::MakeHistGlob(StEvent *event){

  StSPtrVecTrackNode &theNodes = event->trackNodes();
  Int_t cnttrk=0;
  Int_t cnttrkg=0;

  //  cnttrk = theNodes.entries(global);
  //  m_globtrk_tot->Fill(cnttrk);
  for (UInt_t i=0; i<theNodes.size(); i++) {
    StTrack *globtrk = theNodes[i]->track(global);
    if (!globtrk) continue;
    cnttrk += theNodes[i]->entries(global);
    m_globtrk_tot->Fill(cnttrk);
    m_globtrk_iflag->Fill(globtrk->flag());
    if (globtrk->flag()>0) {
      cnttrkg++;
      Float_t pT = -999.;
      pT = 1./TMath::Abs(globtrk->geometry()->momentum().perp());
      Float_t lmevpt = TMath::Log10(pT*1000.0);
      Float_t theta = TMath::ASin(1.) - globtrk->geometry()->dipAngle();
      Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
      Float_t gmom  = pT/TMath::Sin(theta);
      Float_t lmevmom = TMath::Log10(gmom*1000.0);
      Float_t chisq0 = globtrk->fitTraits().chi2(0);
      Float_t chisq1 = globtrk->fitTraits().chi2(1);
      Float_t nfitntot = (Float_t(globtrk->fitTraits().numberOfFitPoints()))
	/ (Float_t(globtrk->detectorInfo()->numberOfPoints()));

      // Use globtrk->geometry()->origin()->x() and y() instead
      // of using r0*phi*degree for x0s and y0s -CL
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
  m_globtrk_good->Fill(cnttrkg);
}
/*
//_____________________________________________________________________________

void StEventQAMaker::MakeHistDE(St_DataSet *dst) {
  // Fill histograms for dE/dx
  
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
}

//_____________________________________________________________________________


void StEventQAMaker::MakeHistPrim(St_DataSet *dst){

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

// not in 99i tables
//   m_prim_ndf->Fill(t->ndegf);
//   Float_t xdif = (t->x_first[0]) - (t->x0);
//   Float_t ydif = (t->x_first[1]) - (t->y0);

      }
    }
    m_primtrk_good->Fill(cnttrkg);
  }       
}

//_____________________________________________________________________________


void StEventQAMaker::MakeHistGen(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling particle histograms " << endl;
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


void StEventQAMaker::MakeHistV0(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling dst_v0_vertex histograms " << endl;

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

void StEventQAMaker::MakeHistPID(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling PID histograms " << endl;
  
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


void StEventQAMaker::MakeHistVertex(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling vertex histograms " << endl;

  St_DataSetIter dstI(dst);

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
}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistXi(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling dst_xi_vertex histograms " << endl;
    
  St_DataSetIter dstI(dst);           

  St_dst_xi_vertex *xi = (St_dst_xi_vertex*) dstI["dst_xi_vertex"];
  if (xi) {

    Int_t cntrows=0;
    cntrows = xi->GetNRows();
    m_xi_tot->Fill(cntrows);

  }

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistPoint(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling point histograms " << endl;


  St_DataSetIter dstI(dst);           

  St_dst_point *pt = (St_dst_point*) dstI["point"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_pnt_tot->Fill(cntrows);

  }

}


//_____________________________________________________________________________
void StEventQAMaker::MakeHistKink(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling kink histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_dst_tkf_vertex *pt = (St_dst_tkf_vertex*) dstI["kinkVertex"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_kink_tot->Fill(cntrows);

  }

}


//_____________________________________________________________________________
void StEventQAMaker::MakeHistL3(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling L3 histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_tpt_track *pt = (St_tpt_track*) dstI["l3Track"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_l3_tot->Fill(cntrows);

  }

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistV0Eval(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling ev0_eval histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_ev0_eval *pt = (St_ev0_eval*) dstI["ev0_eval"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_v0eval_tot->Fill(cntrows);

  }

}

//_____________________________________________________________________________
void StEventQAMaker::MakeHistRich(St_DataSet *dst){
  if (Debug()) cout << " *** in StEventQAMaker - filling Rich histograms " << endl;

  St_DataSetIter dstI(dst);           

  St_g2t_rch_hit *pt = (St_g2t_rch_hit*) dstI["g2t_rch_hit"];
  if (pt) {

    Int_t cntrows=0;
    cntrows = pt->GetNRows();
    m_rich_tot->Fill(cntrows);

  }

}
*/
//_____________________________________________________________________________
