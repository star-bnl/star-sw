// $Id: StQABookHist.cxx,v 1.3 1999/11/23 19:00:51 lansdell Exp $ 
// $Log: StQABookHist.cxx,v $
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
const Float_t StQABookHist::ymineta = -2.0;
const Float_t StQABookHist::ymaxeta =  2.0;

const Int_t StQABookHist::nchisq = 50;
const Int_t StQABookHist::nmass  = 40;
const Int_t StQABookHist::ntau   = 40;
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
const Float_t StQABookHist::mindedx  = 0.0;
const Float_t StQABookHist::maxdedx  = 0.0005*1e6; // in keV/cm
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
 
  cout << "NOW BOOK HISTS! " << endl;

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
  //  m_trk_tot_gd    = QAH1F(   TString::TString("QaEvsumTrkGoodDTotal").Prepend(QAHistType).Data(),"evsum: num good track over total",  50,0.,1.0);

  m_trk_tot_gd    = QAH1F("QaEvsumTrkGoodDTotal",
    "evsum: num good track over total",  50,0.,1.0);
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

// 1D
  m_globtrk_tot   = QAH1F("QaGlobtrkTot",  "globtrk: tot # tracks",40,0.,10000.);
  m_globtrk_iflag = QAH1F("QaGlobtrkFlag", "globtrk: iflag ",200,-999.,1001.);

  m_globtrk_good  = QAH1F("QaGlobtrkGood", "globtrk: tot # good tracks",40,0.,10000.);  
  m_det_id     = QAH1F("QaGlobtrkDetId",   "globtrk: Detector ID for tracks",25,0.,25.);
  m_point      = QAH1F("QaGlobtrkNPnt",    "globtrk: N points on track", 50, 0.,50.);
  m_max_point  = QAH1F("QaGlobtrkNPntMax", "globtrk: N max points on track", 50, 0.,100.);
  m_fit_point  = QAH1F("QaGlobtrkNPntFit", "globtrk: N fit points on track", 50, 0.,50.);
  m_glb_charge = QAH1F("QaGlobtrkChrg",    "globtrk: charge ", 20,-2.,2.);
  m_glb_xf     = QAH1F("QaGlobtrkXf",      "globtrk: x of first hit on trk", 50,-200.,200.);
  m_glb_xf0    = QAH1F("QaGlobtrkXf0",     "globtrk: x of first hit - on helix at start",50,-20.,20.);
  m_glb_yf     = QAH1F("QaGlobtrkYf",      "globtrk: y of first hit on trk", 50,-200.,200.);
  m_glb_yf0    = QAH1F("QaGlobtrkYf0",     "globtrk: y of first hit - on helix at start",50,-20.,20.);
  m_glb_zf     = QAH1F("QaGlobtrkZf",      "globtrk: z of first hit on trk", 50,-250.,250.);
  m_glb_zf0    = QAH1F("QaGlobtrkZf0",     "globtrk: z of first hit - on helix at start",50,-20.,20.);
  m_glb_radf   = QAH1F("QaGlobtrkR",       "globtrk: radial position of first tpc hit", 50,0.,250.);
  m_glb_ratio  = QAH1F("QaGlobtrkRnf",     "globtrk: ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_psi        = QAH1F("QaGlobtrkPsi",     "globtrk: psi distribution", 36, 0.,360.);
  m_tanl       = QAH1F("QaGlobtrkTanl",    "globtrk: tanl distribution",32,-4.,4.);
  m_glb_theta  = QAH1F("QaGlobtrkTheta",   "globtrk: theta distribution",20,0.,4.);
  m_eta        = QAH1F("QaGlobtrkEta",     "globtrk: eta distribution",60,-3.0,3.0);
  m_pT         = QAH1F("QaGlobtrkPt",      "globtrk: pT distribution",50,0.,5.);
  m_mom        = QAH1F("QaGlobtrkP",       "globtrk: momentum distribution",50,0.,5.);
  m_chisq0     = QAH1F("QaGlobtrkChisq0",  "globtrk: chisq0 - xy", 50, 0.,15.);
  m_chisq1     = QAH1F("QaGlobtrkChisq1",  "globtrk: chisq1 - z", 50, 0.,15.);
  m_length     = QAH1F("QaGlobtrkLength",  "globtrk: track length", 50,0.,300.);
  m_glb_impact = QAH1F("QaGlobtrkImpact",  "globtrk: impact param from prim vtx ", 50,0.,500.);


// 2D
  m_pT_eta_rec = QAH2F("QaGlobtrkPtVsEta","globtrk: log pT versus eta", 20,-2.,2.,40,1.,4.);
    m_pT_eta_rec->SetXTitle("eta");
    m_pT_eta_rec->SetYTitle(" log pT (MeV)");

  m_globtrk_xf_yf = QAH2F("QaGlobtrkXfYf","globtrk: Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yf->SetXTitle("x first");
    m_globtrk_xf_yf->SetYTitle("y first");

  m_tanl_zf = QAH2F("QaGlobtrkTanlzf","globtrk: tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_tanl_zf->SetXTitle("zfirst");
    m_tanl_zf->SetYTitle("tanl");

  m_mom_trklength = QAH2F("QaGlobtrkPVsTrkLength","globtrk: log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_mom_trklength->SetXTitle("trk length");  
    m_mom_trklength->SetYTitle("log P (MeV)");

  m_eta_trklength = QAH2F("QaGlobtrkLengthVEta","globtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklength->SetXTitle("eta");
    m_eta_trklength->SetYTitle("length");

  m_npoint_length = QAH2F("QaGlobtrkNPntLength","globtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_npoint_length->SetXTitle("length");
    m_npoint_length->SetYTitle("Npoints");

  m_fpoint_length = QAH2F("QaGlobtrkFitPntLength","globtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_fpoint_length->SetXTitle("length");
    m_fpoint_length->SetYTitle("Npoints");

  m_chisq0_mom = QAH2F("QaGlobtrkChi0Mom","globtrk: Chisq0 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq0_mom->SetXTitle("log P (MeV)");
    m_chisq0_mom->SetYTitle("chisq0") ;

  m_chisq1_mom = QAH2F("QaGlobtrkChi1Mom","globtrk: Chisq1 vs log mom",40,1.,4.,50,0.,5.);
    m_chisq1_mom->SetXTitle("log P (MeV)");
    m_chisq1_mom->SetYTitle("chisq1");

  m_chisq0_eta = QAH2F("QaGlobtrkChi0Eta","globtrk: Chisq0 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq0_eta->SetXTitle("eta");
    m_chisq0_eta->SetYTitle("chisq0");

  m_chisq1_eta = QAH2F("QaGlobtrkChi1Eta","globtrk: Chisq1 vs eta",20,-2.,2.,20,0.,5.);
    m_chisq1_eta->SetXTitle("eta");
    m_chisq1_eta->SetYTitle("chisq1");

  m_chisq0_dip = QAH2F("QaGlobtrkChi0Tanl","globtrk: Chisq0 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq0_dip->SetXTitle("dip angle");
    m_chisq0_dip->SetYTitle("chisq0");

  m_chisq1_dip = QAH2F("QaGlobtrkChi1Tanl","globtrk: Chisq1 vs tanl(dip)",20,-5.,5.,20,0.,5.);
    m_chisq1_dip->SetXTitle("dip angle");
    m_chisq1_dip->SetYTitle("chisq1");

  m_chisq0_zf = QAH2F("QaGlobtrkChi0zf","globtrk: Chisq0 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq0_zf->SetXTitle("zfirst");
    m_chisq0_zf->SetYTitle("chisq0");

  m_chisq1_zf = QAH2F("QaGlobtrkChi1zf","globtrk: Chisq1 vs zfirst",20,-250.,250.,20,0.,5.);
    m_chisq1_zf->SetXTitle("zfirst");
    m_chisq1_zf->SetYTitle("chisq1");

  m_nfptonpt_mom = QAH2F("QaGlobtrkRPntMom","globtrk: ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_nfptonpt_mom->SetXTitle("log P (MeV)");
     m_nfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_eta = QAH2F("QaGlobtrkRPntEta","globtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_nfptonpt_eta->SetXTitle("eta");
     m_nfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");

}
//____________________________________________________
void StQABookHist::BookHistPrim(){

// for method MakeHistPrim - from table primtrk

// 1D
  m_primtrk_tot   = QAH1F("QaPrimtrkTot",  "primtrk: tot # tracks",40,0.,10000.);
  m_primtrk_iflag = QAH1F("QaPrimtrkFlag", "primtrk: iflag ",200,-999.,1001.);

  m_primtrk_good  = QAH1F("QaPrimtrkGood",  "primtrk: tot # good tracks",40,0.,10000.);  
  m_pdet_id     = QAH1F("QaPrimtrkDetId",   "primtrk: Detector ID for tracks",25,0.,25);
  m_ppoint      = QAH1F("QaPrimtrkNPnt",    "primtrk: N points on track", 50, 0.,50.);
  m_pmax_point  = QAH1F("QaPrimtrkNPntMax", "primtrk: N max points on track", 50, 0.,50.);
  m_pfit_point  = QAH1F("QaPrimtrkNPntFit", "primtrk: N fit points on track", 50, 0.,50.);
  m_prim_charge = QAH1F("QaPrimtrkChrg",    "primtrk: charge ", 20,-2.,2.);
  m_prim_xf     = QAH1F("QaPrimtrkXf",      "primtrk: x of first hit on trk ", 50,-200.,200.);
  m_prim_xf0    = QAH1F("QaPrimtrkXf0",     "primtrk: x of first hit - on helix at start",50,-200.,200.);
  m_prim_yf     = QAH1F("QaPrimtrkYf",      "primtrk: y of first hit on trk", 50,-200.,200.);
  m_prim_yf0    = QAH1F("QaPrimtrkYf0",     "primtrk: y of first hit - on helix at start",50,-200.,200.);
  m_prim_zf     = QAH1F("QaPrimtrkZf",      "primtrk: z of first hit on trk", 50,-200.,200.);
  m_prim_zf0    = QAH1F("QaPrimtrkZf0",     "primtrk: z of first hit - on helix at start",50,-200.,200.);
  m_prim_radf   = QAH1F("QaPrimtrkR",   "primtrk: radial position of first tpc hit",50,0.,250.);
  m_prim_ratio  = QAH1F("QaPrimtrkRnf",     "primtrk: ratio Nfitpnt over Npnt", 50, 0., 1.2005);
  m_ppsi        = QAH1F("QaPrimtrkPsi",     "primtrk: psi distribution", 36, 0.,360.);
  m_ptanl       = QAH1F("QaPrimtrkTanl",    "primtrk: tanl distribution",32,-4.,4.);
  m_prim_theta  = QAH1F("QaPrimtrkTheta",   "primtrk: theta distribution",20,0.,4.);
  m_peta        = QAH1F("QaPrimtrkEta",     "primtrk: eta distribution",60,-3.0,3.0);
  m_ppT         = QAH1F("QaPrimtrkPt",      "primtrk: pT distribution",50,0.,5.);
  m_pmom        = QAH1F("QaPrimtrkP",       "primtrk: momentum distribution",50,0.,5.);
  m_pchisq0     = QAH1F("QaPrimtrkChisq0",  "primtrk: chisq0 - xy", 50, 0.,5.);
  m_pchisq1     = QAH1F("QaPrimtrkChisq1",  "primtrk: chisq1 - z", 50, 0.,5.);
  m_plength     = QAH1F("QaPrimtrkLength",  "primtrk: track length", 50,0.,300.);
  m_prim_impact = QAH1F("QaPrimtrkImpact",  "primtrk: impact param from prim vtx ", 50,0.,5.);


// 2D
  m_ppT_eta_rec = QAH2F("QaPrimtrkPtVsEta","primtrk: log pT versus eta", 20,-2.,2.,40,1.,4.);
    m_ppT_eta_rec->SetXTitle("eta");
    m_ppT_eta_rec->SetYTitle(" log pT (MeV)");

  m_primtrk_xf_yf = QAH2F("QaPrimtrkXfYf","primtrk: Y vs X of first hit on trk", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yf->SetXTitle("x first");
    m_primtrk_xf_yf->SetYTitle("y first");


  m_ptanl_zf = QAH2F("QaPrimtrkTanlzf","primtrk: tanl(dip) versus zfirst",50,-250.,250.,60,-3.,3.);
    m_ptanl_zf->SetXTitle("zfirst");
    m_ptanl_zf->SetYTitle("tanl");


  m_pmom_trklength = QAH2F("QaPrimtrkPVsTrkLength","primtrk: log mom vs trk length",
			     50,0.,250.,40,1.,4.);
    m_pmom_trklength->SetXTitle("trk length");  
    m_pmom_trklength->SetYTitle("log P (MeV)");

  m_peta_trklength = QAH2F("QaPrimtrkLengthVEta","primtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_peta_trklength->SetXTitle("eta");
    m_peta_trklength->SetYTitle("length");


  m_pnpoint_length = QAH2F("QaPrimtrkNPntLength","primtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pnpoint_length->SetXTitle("length");
    m_pnpoint_length->SetYTitle("Npoints");

  m_pfpoint_length = QAH2F("QaPrimtrkFitPntLength","primtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_pfpoint_length->SetXTitle("length");
    m_pfpoint_length->SetYTitle("Npoints");

  m_pchisq0_mom = QAH2F("QaPrimtrkChi0Mom","primtrk: Chisq0 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq0_mom->SetXTitle("log P (MeV)");
    m_pchisq0_mom->SetYTitle("chisq0") ;

  m_pchisq1_mom = QAH2F("QaPrimtrkChi1Mom","primtrk: Chisq1 vs log mom",40,1.,4.,25,0.,5.);
    m_pchisq1_mom->SetXTitle("log P (MeV)");
    m_pchisq1_mom->SetYTitle("chisq1");


  m_pchisq0_eta = QAH2F("QaPrimtrkChi0Eta","primtrk: Chisq0 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq0_eta->SetXTitle("eta");
    m_pchisq0_eta->SetYTitle("chisq0");

  m_pchisq1_eta = QAH2F("QaPrimtrkChi1Eta","primtrk: Chisq1 vs eta",20,-2.,2.,25,0.,5.);
    m_pchisq1_eta->SetXTitle("eta");
    m_pchisq1_eta->SetYTitle("chisq1");

  m_pchisq0_dip = QAH2F("QaPrimtrkChi0Tanl","primtrk: Chisq0 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq0_dip->SetXTitle("dip angle");
    m_pchisq0_dip->SetYTitle("chisq0");

  m_pchisq1_dip = QAH2F("QaPrimtrkChi1Tanl","primtrk: Chisq1 vs tanl(dip)",20,-5.,5.,25,0.,5.);
    m_pchisq1_dip->SetXTitle("dip angle");
    m_pchisq1_dip->SetYTitle("chisq1");

  m_pchisq0_zf = QAH2F("QaPrimtrkChi0zf","primtrk: Chisq0 vs zfirst",20,-250.,250.,25,0.,5.);
    m_pchisq0_zf->SetXTitle("zfirst");
    m_pchisq0_zf->SetYTitle("chisq0");

  m_pchisq1_zf = QAH2F("QaPrimtrkChi1zf","primtrk: Chisq1 vs zfirst",20,-250.,250.,25,0.,7.);
    m_pchisq1_zf->SetXTitle("zfirst");
    m_pchisq1_zf->SetYTitle("chisq1");

  m_pnfptonpt_mom = QAH2F("QaPrimtrkRPntMom","primtrk: ratio Nfitpnt,Npnt vs log mom.",40,1.,4.,50,0.,1.2005); 
     m_pnfptonpt_mom->SetXTitle("log P (MeV)");
     m_pnfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_pnfptonpt_eta = QAH2F("QaPrimtrkRPntEta","primtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,50,0.,1.2005); 
     m_pnfptonpt_eta->SetXTitle("eta");
     m_pnfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");

}
//_____________________________________________________________________________
void StQABookHist::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedxr  = QAH1F("QaDedxNum", "dedx: number of tracks", 50,0., 10000.); 
  m_ndedx   = QAH1F("QaDedxNdedx", "dedx: number of point to define dE/dx", 50,0., 50.);  
  m_dedx0   = QAH1F("QaDedxDedx0","dedx: dE/dx[0]", ndedx, mindedx, maxdedx/10.);
  m_dedx1   = QAH1F("QaDedxDedx1","dedx: dE/dx[1]", ndedx, mindedx, maxdedx);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistGen(){

  // for MakeHistGen - from table particle
  m_H_npart   = QAH1F("QaParticleNumPart","particle:total num particles (generated)",100,0.,30000.);
  m_H_ncpart  = QAH1F("QaParticleNumChgPart","particle:num chg (e,mu,pi,K,p) part (generated)",100,0.,20000.);
  m_H_pT_gen  = QAH1F("QaParticlePt","particle: charged pt (generated)",nxpT,xminpT,xmaxpT);
  m_H_eta_gen = QAH1F("QaParticleEta","particle: charged eta (generated)",nyeta,-5.,5.);
  m_H_pT_eta_gen = QAH2F("QaParticlePtVsEta","particle: charged pT versus eta (generated)",
			    nyeta,kmineta,kmaxeta,nxpT,xminpT,xmaxpT);
  m_H_pT_eta_gen->SetXTitle("eta");
  m_H_pT_eta_gen->SetYTitle("pT (GeV)");
  m_H_vtxx    = QAH1F("QaParticleVtxX","particle: Generator prod vertex x (mm)",50,-10.,10.);
  m_H_vtxy    = QAH1F("QaParticleVtxY","particle: Generator prod vertex y (mm)",50,-10.,10.);
  m_H_vtxz    = QAH1F("QaParticleVtxZ","particle: Generator prod vertex z (mm)",50,-50.,50.);

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
  
  
  m_v_num   = QAH1F("QaVertexNum"," vertex: num vertices ",50,0.,10000.);
  m_v_detid = QAH1F("QaVertexDetId"," vertex: Detector ID ",100,0.,100.);
  m_v_vtxid = QAH1F("QaVertexVtxId"," vertex: Vertex ID ",10,0.,10.);
  m_v_x     = QAH1F("QaVertexX"," vertex: x ",50,-25.,25.);
  m_v_y     = QAH1F("QaVertexY"," vertex: y ",50,-25.,25.);
  m_v_z     = QAH1F("QaVertexZ"," vertex: z ",50,-50.,50.);
  m_v_pchi2 = QAH1F("QaVertexChisq"," vertex: chisq/dof ",50,0.,5.);
  
  m_pv_detid = QAH1F("QaVertexPrDetId"," vertex,prim: Detector ID ",40,0.,40.);
  m_pv_vtxid = QAH1F("QaVertexPrVtxId"," vertex,prim: Vertex ID ",10,0.,10.);
  m_pv_x     = QAH1F("QaVertexPrX"," vertex,prim: x ",50,-1.,1.);
  m_pv_y     = QAH1F("QaVertexPrY"," vertex,prim: y ",50,-1.,1.);
  m_pv_z     = QAH1F("QaVertexPrZ"," vertex,prim: z ",50,-50.,50.);
  m_pv_pchi2 = QAH1F("QaVertexPrChisq"," vertex,prim: chisq/dof ",50,0.,5.);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistXi(){

  m_xi_tot   = QAH1F("QaXiVertexTot",  "dst_xi_vertex: tot # vertices ",50,0.,2500.);

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

  m_rich_tot   = QAH1F("QaRichTot",  "g2t_rch_hit: # vertices ",50,0.,2000.);

}
//_____________________________________________________________________________
