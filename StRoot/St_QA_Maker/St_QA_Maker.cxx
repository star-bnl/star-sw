// $Id: St_QA_Maker.cxx,v 1.21 1999/04/23 14:04:07 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
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
#include "TPostScript.h"
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "St_QA_Maker.h"

#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"
#include "St_dst_track_Table.h"
#include "St_dst_v0_vertex_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_dst_event_summary_Table.h"
#include "St_dst_xi_vertex_Table.h"
#include "St_dst_vertex_Table.h"
#include "St_dst_tof_evt_Table.h"
#include "St_dst_tof_trk_Table.h"
#include "St_ems_hits_Table.h"


#include "StChain.h"
#include "St_DataSetIter.h"

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
const Int_t St_QA_Maker::nmneta = 40;
const Int_t St_QA_Maker::nxyz   = 50;
const Int_t St_QA_Maker::knyeta = 60;
const Int_t St_QA_Maker::knid   = 10;
const  Int_t St_QA_Maker::cnp   = 50;
const  Int_t St_QA_Maker::cndedx = 50;    

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
const Float_t St_QA_Maker::maxtrk   = 15000.0;
const Float_t St_QA_Maker::minvrt   = 0.0;
const Float_t St_QA_Maker::maxvrt   = 20000.0;
const Float_t St_QA_Maker::minmpt   = 0.0;
const Float_t St_QA_Maker::maxmpt   = 5.0;
const Float_t St_QA_Maker::minmeta  = -1.0;
const Float_t St_QA_Maker::maxmeta  = 1.0;
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

// First, zero all pointers defined in the header file
  
  // for method MakeEvSum - from table event_summary
  m_trk_tot_gd = 0;      //! number of good global tracks divided by total
  m_glb_trk_tot=0;       //! # tracks total from globtrk
  m_glb_trk_gd=0;        //! # tracks good from globtrk
  m_glb_trk_plusminus=0; //! # trks pos/neg. 
  
  m_vert_total=0;    //! total number of vertices
  m_vert_V0=0;       //! number of V0 vertices
  /*    m_vert_La=0;       //! number of La vertices  */
  /*    m_vert_Ala=0;      //! number of Ala vertices */
  /*    m_vert_K0=0;       //! number of K0 vertices */
  m_mean_pt=0;       //! mean pt value
  m_mean_eta=0;      //! mean eta value 
  m_prim_vrtx0=0;    //! primary vrtx x position
  m_prim_vrtx1=0;    //! primary vrtx y position
  m_prim_vrtx2=0;    //! primary vrtx z position
  m_vrtx_chisq=0;    //! primary vrtx chisq
  
  // for method MakeGlob - from table globtrk
  m_pT=0;            //! pT  reconstructed
  m_pT_fr=0;         //! pT  reconstructed - full range
  m_eta=0;           //! eta reconstructed
  m_pT_eta_rec=0;    //! pT versus eta Spectra for reconstructed
  m_mom_trklength=0; //! mom vs. trk length
  m_point=0;         //! number of points on the track
  m_fit_point=0;     //! number of track points used for fitting
  m_length=0;        //! length of track
  m_chisq0=0;        //! chi square [0]
  m_chisq1=0;        //! chi square [1]
  m_psi=0;           //! psi reconstructed
  m_det_id=0;        //! detector id of track
  m_npoint_length=0; //! num points vs length
  m_fpoint_length=0; //! num fit points vs length
  m_chisq0_mom=0;    //! chisq0 vs momentum
  m_chisq1_mom=0;    //! chisq1 vs momentum
  
  
  // for method MakeDE - from table dst_dedx
  m_ndedx=0;         //! number of point to find dE/dx
  m_dedx0=0;         //! dE/dx [0]
  m_dedx1=0;         //! dE/dx [1] 
  
  // for method MakeHistPrim - from table primtrk
  m_prim_pT=0;            //! pT  recostructed
  m_prim_eta=0;           //! eta recostructed
  m_prim_pT_eta_rec=0;    //! pT versus eta Spectra for reconstructed
  m_prim_tlength=0;       //! dst track length
  m_prim_chi2xd=0;        //! x chisq/degf
  m_prim_chi2yd=0;        //! y chisq/degf
  m_prim_point=0;         //! # points on track
  m_prim_fit_point=0;     //! # fitted points
  m_prim_psi=0;           //! psi angle_ 
  m_prim_det_id=0;        //! 
  m_prim_mom_trklength=0; //!
  m_prim_npoint_length=0; //!
  m_prim_fpoint_length=0; //!
  m_prim_chisq0_mom=0;    //!
  m_prim_chisq1_mom=0;    //!
  
  
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
  
  // for method MakeHistTofEvt
  m_te_ntpttrk=0;   //!no. of tpc tracks in event  
  m_te_nttetrk=0;   //!no. of tte tracks associated with tpt tracks
  m_te_ng2ttrk=0;   //!no. of g2t tracks associated with tpt/tte tracks
  m_te_nctfhit=0;   //!no. hits in g2t_tof_hit
  m_te_nexttrk=0;   //!no. decent tracks extrapolated
  m_te_ntoftrk=0;   //!no. of these that are kept
  m_te_ntrks=0;     //!no. decent tracks in the event
  m_te_ntrks_hit=0; //!no. of these with ctf hit via pointers
  m_te_ntrks_kee=0; //!no. of decent tracks extrapolated
  m_te_ntrks_tra=0; //!no. of these extrapolated to TOFp tray
  m_te_ntrks_mat=0; //!no. of these extrapolated to struck TOFp slat
  
  // for method MakeHistTofTrk
  m_tt_strk=0;   //! measured total length from target to hit
  m_tt_phitrk=0; //! phi of extrapolation of track to tof
  m_tt_stof=0;   //! geant's total length from target to hit 
  m_tt_phitof=0; //! phi of hit in tof from geant 
  m_tt_tof=0;    //! actual time of flight
  m_tt_adc=0;    //! ADC value for this slat
  
  // for method MakeHistEmsHitsBemc
  m_ehbe_hits1=0; //! bemc # hits detector 1
  m_ehbe_tnrg1=0; //! bemc tot energy detector 1
  m_ehbe_hits2=0; //! bemc # hits detector 2
  m_ehbe_tnrg2=0; //! bemc tot energy detector 2
  
  
  // for method MakeHistEmsHitsBsmd
  m_ehbs_hits3=0; //! bemc # hits detector 3
  m_ehbs_tnrg3=0; //! bemc tot energy detector 3
  m_ehbs_hits4=0; //! bemc # hits detector 4
  m_ehbs_tnrg4=0; //! bemc tot energy detector 4 
  
  // for method MakeHistXi

//  Now construct inline functions: 
  drawinit=kFALSE;
  SetDraw();
  SetHistsNames();
  SetZones();
  SetPaperSize();
  SetPostScriptFile(); // no PostScript file "by default"
}
//_____________________________________________________________________________
St_QA_Maker::~St_QA_Maker(){
  SafeDelete(m_QACanvas);
}
//_____________________________________________________________________________
Int_t St_QA_Maker::DrawHists() 
{
  // Plots the seleted  histograms abd generate the postscript file as well if any
  
  const Int_t numPads = m_PadColumns*m_PadRows;
  
  gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight);
  //  gStyle->SetOptStat(0);
  //   TCanvas *QACanvas = new TCanvas("Banner","Canvas Title",30*height,30*width);
  TPostScript *ps = 0;
  const Char_t *psfileName = m_PsFileName.Data();
  if (!m_PsFileName.IsNull()) ps = new TPostScript((char *)psfileName);  
  
  gStyle->SetOptStat(111111);
  SafeDelete(m_QACanvas);
  TCanvas *QACanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
  QACanvas->SetFillColor(19);
  QACanvas->SetBorderSize(2);
  
  QACanvas->Divide(m_PadColumns,m_PadRows);
  
  if (ps) ps->NewPage();
  const Char_t *firstHistName = m_FirstHistName.Data();
  const Char_t *lastHistName  = m_LastHistName.Data();
  TObject *obj = 0;
  TList *dirList = gDirectory->GetList();
  Int_t padCount = 0;
  //_____________________
  // Create an iterator
  TIter nextHist(dirList);
  Int_t histCounter = 0;
  Int_t histReadCounter = 0;
  Bool_t started = kFALSE;
  while (obj = nextHist()) {
    if (obj->InheritsFrom("TH1")) { 
      histReadCounter++;
      printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
      if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;
      if (started) {
	if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
	histCounter++;
	printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
	if (padCount == numPads) {
	  if (ps) ps->NewPage();
	  padCount=0;
	}
	QACanvas->cd(++padCount);
	
	obj->Draw();   
	if (gPad) gPad->Update();
      }
    }
  }
  if (ps) {
    ps->Close();
    delete ps;
  }
  return histCounter;
}

//_____________________________________________________________________________
Int_t St_QA_Maker::Finish() {
  if (drawinit) DrawHists();
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t St_QA_Maker::Init(){
  
  // Create tables
  // Create Histograms 
  
  //book histograms --------------
  
  BookHistEvSum();
  BookHistGlob();
  BookHistDE();
  BookHistPrim();
  BookHistGen();
  BookHistV0();
  BookHistPID();
  BookHistVertex();
  BookHistTofEvt();
  BookHistTofTrk();
  BookHistEmsHitsBemc();
  BookHistEmsHitsBsmd();
  BookHistXi();
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_QA_Maker::Make(){
  //  PrintInfo();
  
  // Call methods to fill histograms
  
  St_DataSet *dst = GetDataSet("dst/dst");
  St_DataSet *dst1 = GetDataSet("dst");
  if(!dst) dst = dst1;
  
  
  // histograms from table event_summary
  MakeHistEvSum(dst);
  // histograms from table globtrk
  MakeHistGlob(dst);
  // histograms from table dst_dedx
  MakeHistDE(dst);
  // histograms from table primtrk
  MakeHistPrim(dst);
  
  // histograms from table particle
  MakeHistGen(dst1);
  
  // histograms from table dst_v0_vertex
  MakeHistV0(dst);
  // histograms from table primtrk & dst_dedx
  MakeHistPID(dst);
  // histograms from table dst_vertex
  MakeHistVertex(dst);
  MakeHistTofEvt(dst);
  MakeHistTofTrk(dst);
  MakeHistEmsHitsBemc(dst);
  MakeHistEmsHitsBsmd(dst);
  MakeHistXi(dst);
  
  return kStOK;
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistEvSum(){
  
  // for method MakeEvSum - from table event_summary
  m_trk_tot_gd    = new TH1F("QaEvsumGlbTrkGoodDTotal","evsum: num good track over total",
                             55,0.,1.1);
  m_trk_tot_gd->SetXTitle("number of good/total tracks");
  m_glb_trk_gd    = new TH1F("QaEvsumGlbTrkGd","evsum: num good tracks ",
                             ntrk, mintrk, maxtrk);
  m_glb_trk_tot   = new TH1F("QaEvsumGlbTrkTot","evsum: num tracks total ",
                             ntrk, mintrk, maxtrk);
  m_glb_trk_plusminus  = new TH1F("QaEvsumPlusMinusTrk", "evsum: num pos. over neg trks",
				  ntrk, 0., 2.);  
  m_vert_total = new TH1F("QaEvsumVertTot", "evsum: total num of vertices",nvrt,minvrt,maxvrt);
  m_vert_V0    = new TH1F("QaEvsumVertV0", "evsum: num V0 vertices", nvrt,minvrt,maxvrt);
  //    m_vert_La    = new TH1F("QaEvsumVertLa", "number of La vertices", nvrt,minvrt,maxvrt);
  //    m_vert_Ala   = new TH1F("QaEvsumVertLb", "number of Lb vertices", nvrt,minvrt,maxvrt);
  //    m_vert_K0    = new TH1F("QaEvsumVertK0", "number of K0 vertices", nvrt,minvrt,maxvrt);   
  m_mean_pt    = new TH1F("QaEvsumMeanPt", "evsum: mean pt", nmnpt, 0., maxmpt);
  m_mean_eta   = new TH1F("QaEvsumMeanEta","evsum: mean eta", nmneta, minmeta, maxmeta);
  m_prim_vrtx0 = new TH1F("QaEvsumPrimVertX","evsum: X of primary vertex", 40, -2.,2.);
  m_prim_vrtx1 = new TH1F("QaEvsumPrimVertY","evsum: Y of primary vertex", 40,-2.,2.);
  m_prim_vrtx2 = new TH1F("QaEvsumPrimVertZ","evsum: Z of primary vertex", nxyz,-maxxyz, maxxyz);
  m_vrtx_chisq = new TH1F("QaEvsumVrtxChisq", "evsum: chisq of primary vertex",nchisq, 0., maxchisq); 
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistGlob(){
  
  // for method MakeGlob - from table globtrk
  m_pT         = new TH1F("QaGlobtrkPt","globtrk: pT distribution",nxpT,xminpT,5.);
  m_pT_fr      = new TH1F("QaGlobtrkPtfr","globtrk: pT distribution",nxpT,xminpT,10000.);
  m_eta        = new TH1F("QaGlobtrkEta","globtrk: eta distribution",knyeta,kmineta,kmaxeta);
  m_pT_eta_rec = new TH2F("QaGlobtrkPtVsEta","globtrk: pT versus eta (reconstructed)",
			  20,ymineta,ymaxeta,20,xminpT,xmaxpT);
  m_pT_eta_rec->SetXTitle("eta");
  m_pT_eta_rec->SetYTitle("pT (GeV)");
  m_mom_trklength = new TH2F("QaGlobtrkPVsTrkLength","globtrk: mom vs trk length",
			     50,0.,200.,20,0.,10.);
  m_mom_trklength->SetXTitle("length");
  m_mom_trklength->SetYTitle("mom");
  m_point      = new TH1F("QaGlobtrkNPoint","globtrk: N points on track", npnt, minpnt, maxpnt);
  m_fit_point  = new TH1F("QaGlobtrkNPointFit","globtrk: N fit points on track", npnt,minpnt, maxpnt);
  m_length     = new TH1F("QaGlobtrkLength","globtrk: track length", nleng,minleng,maxleng);
  m_npoint_length = new TH2F("QaGlobtrkNPntLength","globtrk: N points trk vs trk length",
			     25,minleng,maxleng,25,minpnt,maxpnt);
  m_fpoint_length = new TH2F("QaGlobtrkFitPntLength","globtrk: N fit points vs trk length",
			     25,minleng,maxleng,25,minpnt,maxpnt);
  m_chisq0     = new TH1F("QaGlobtrkChisq0P","globtrk: chisq[0]", nchisq, 0.,10.);
  m_chisq1     = new TH1F("QaGlobtrkChisq1P","globtrk: chisq[1]", nchisq, 0.,10.);
  m_psi        = new TH1F("QaGlobtrkPsi","globtrk: psi distribution", npsi, minpsi,maxpsi);
  m_det_id     = new TH1F("QaGlobtrkDetId","globtrk: Detector ID for tracks",11,-0.5,10.5);
  m_chisq0_mom = new TH2F("QaGlobtrkChi0Mom","globtrk: Chisq0 vs mom",20,0.,10.,20,0.,10.);
  m_chisq0_mom->SetXTitle("mom");
  m_chisq0_mom->SetYTitle("chisq0");
  m_chisq1_mom = new TH2F("QaGlobtrkChi1Mom","globtrk: Chisq1 vs mom",20,0.,10.,20,0.,10.);
  m_chisq1_mom->SetXTitle("mom");
  m_chisq1_mom->SetYTitle("chisq1");
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedx   = new TH1F("QaDstDedxNdedx", "number of point to define dE/dx", 50,
		       -0.5, 49.5);  
  m_dedx0   = new TH1F("QaDstDedxDedx0","dE/dx[0]", ndedx, mindedx, maxdedx/10.);
  m_dedx1   = new TH1F("QaDstDedxDedx1","dE/dx[1]", ndedx, mindedx, maxdedx);
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistPrim(){
  // for MakeHistPrim - from table primtrk
  m_prim_pT         = new TH1F("QaPrimtrkPt","primtrk: pT distribution",nxpT,xminpT,xmaxpT);
  m_prim_eta        = new TH1F("QaPrimtrkEta","primtrk: eta distribution",knyeta,kmineta,kmaxeta);
  m_prim_pT_eta_rec = new TH2F("QaPrimtrkPtVsEta","primtrk: pT versus eta (reconstructed)",
			       20,kmineta,kmaxeta,20,xminpT,xmaxpT);
  m_prim_pT_eta_rec->SetXTitle("eta");
  m_prim_pT_eta_rec->SetYTitle("pT (GeV)");
  m_prim_mom_trklength = new TH2F("QaPrimtrkPVsTrkLength","primtrk: mom vs trk length",
				  50,0.,250.,20,0.,10.);
  m_prim_mom_trklength->SetXTitle("length");
  m_prim_mom_trklength->SetYTitle("mom");
  m_prim_point     = new TH1F("QaPrimtrkNPoint","primtrk: N points on track", npnt, minpnt, maxpnt);
  m_prim_fit_point = new TH1F("QaPrimtrkNPointFit","primtrk: N fit points on track", npnt,minpnt, maxpnt);
  m_prim_tlength    = new TH1F("QaPrimtrkLength","primtrk: track length",100,0.,400.);
  m_prim_npoint_length = new TH2F("QaPrimtrkNPntLength","primtrk: N points vs trk length",
				  25,minleng,250.,25,minpnt,maxpnt);
  m_prim_fpoint_length = new TH2F("QaPrimtrkFitPntLength","primtrk: N fit points vs trk length",
				  25,minleng,250.,25,minpnt,maxpnt);
  
  m_prim_chi2xd     = new TH1F("QaPrimtrkChiXY","primtrk: chisqxy/degf",100,0.,10.);
  m_prim_chi2yd     = new TH1F("QaPrimtrkChiSZ","primtrk: chisqsz/degf",100,0.,10.);
  
  m_prim_psi       = new TH1F("QaPrimtrkPsi","primtrk: psi distribution", 60, 0.,360.);
  m_prim_det_id    = new TH1F("QaPrimtrkDetId","primtrk: Detector ID for tracks",knid,kminnid,kmaxnid);
  
  m_prim_chisq0_mom = new TH2F("QaPrimtrkChi0Mom","primtrk: Chisq0 vs mom",20,0.,10.,20,0.,10.);
  m_prim_chisq0_mom->SetXTitle("mom");
  m_prim_chisq0_mom->SetYTitle("chisq0");
  m_prim_chisq1_mom = new TH2F("QaPrimtrkChi1Mom","primtrk: Chisq1 vs mom",20,0.,10.,20,0.,10.);
  m_prim_chisq1_mom->SetXTitle("mom");
  m_prim_chisq1_mom->SetYTitle("chisq1");
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
void St_QA_Maker::BookHistTofEvt(){
  m_te_ntpttrk    = new TH1F("QaTofEvtNTptTrk","tof evt: num tpc trks",45,0.,15000.);
  m_te_nttetrk    = new TH1F("QaTofEvtNTteTrk","tof evt: num assoc tte trks",45,0.,15000.);
  m_te_ng2ttrk    = new TH1F("QaTofEvtNG2tTrk","tof evt: num assoc g2t trks",45,0.,45000.);
  m_te_nctfhit    = new TH1F("QaTofEvtNCtfHit","tof evt: num g2t_tof_hit",50,0.,200.);
  m_te_nexttrk    = new TH1F("QaTofEvtNExtTrk","tof evt: num decent trks extr.",50,0.,5000.);
  m_te_ntoftrk    = new TH1F("QaTofEvtNTofTrk","tof evt: num tof trks",50,0.,5000.);
  m_te_ntrks      = new TH1F("QaTofEvtNTrk",   "tof evt: num good trks",50,0.,10000.);
  m_te_ntrks_hit  = new TH1F("QaTofEvtNTrkHit","tof evt: num good trks w/ctf hit",20,0.,100.);
  m_te_ntrks_kee  = new TH1F("QaTofEvtNTrkKee","tof evt: num good trks extr.",50,0.,5000.);
  m_te_ntrks_tra  = new TH1F("QaTofEvtNTrkTra","tof evt: num good trks extr. to tofp",20,0.,100.);
  m_te_ntrks_mat  = new TH1F("QaTofEvtNTrkMat","tof evt: num good trks extr. to strk tofp",20,0.,100.);
  
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistTofTrk(){
  
  m_tt_strk   = new TH1F("QaTofTrkSTrk","tof trk: s of trk",50,200.,400.);
  m_tt_phitrk = new TH1F("QaTofTrkPhiTrk","tof trk: phi of trk",50,0.,6.28);
  m_tt_stof   = new TH1F("QaTofTrkSTof","tof trk: s of tof",50,200.,400.);
  m_tt_phitof = new TH1F("QaTofTrkPhiTof","tof trk: phi of tof",50,0.,6.28);
  m_tt_tof    = new TH1F("QaTofTrkTof","tof trk: tof value",128,0.,2048.);
  m_tt_adc    = new TH1F("QaTofTrkAdc","tof trk: adc value",128,0.,1024.);
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistEmsHitsBemc(){
  m_ehbe_hits1 = new TH1F("QaEmsBemcHits1","ems bemc: # hits det 1",50,0.,1000.);
  m_ehbe_tnrg1 = new TH1F("QaEmsBemcTotE1","ems bemc: uncorr geant energy det 1",50,0.,25.);
  m_ehbe_hits2 = new TH1F("QaEmsBemcHits2","ems bemc: # hits det 2",50,0.,1000.);
  m_ehbe_tnrg2 = new TH1F("QaEmsBemcTotE2","ems bemc: uncorr geant energy det 2",50,0.,0.01);
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistEmsHitsBsmd(){
  m_ehbs_hits3 = new TH1F("QaEmsBsmdHits3","ems bsmd: # hits det 3",50,0.,1000.);
  m_ehbs_tnrg3 = new TH1F("QaEmsBsmdTotE3","ems bsmd: uncorr geant energy det 3",50,0.,0.01);
  m_ehbs_hits4 = new TH1F("QaEmsBsmdHits4","ems bsmd: # hits det 4",50,0.,1000.);
  m_ehbs_tnrg4 = new TH1F("QaEmsBsmdTotE4","ems bsmd: uncorr geant energy det 4",50,0.,0.01);
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
      Float_t trk_tot = tt->glb_trk_tot;
      Float_t trk_good = tt->glb_trk_good;
      Float_t trk_plus = tt->glb_trk_plus;
      Float_t trk_minus = tt->glb_trk_minus;
      m_trk_tot_gd->Fill(trk_good/trk_tot); 
      m_glb_trk_gd->Fill(trk_good);
      m_glb_trk_tot->Fill(trk_tot);
      m_glb_trk_plusminus->Fill(trk_plus/trk_minus);
      
      
      m_vert_total->Fill(tt->n_vert_total);
      m_vert_V0->Fill(tt->n_vert_V0);
      //        m_vert_La->Fill(tt->n_vert_Lambda);
      //        m_vert_Ala->Fill(tt->n_vert_ALambda);
      //        m_vert_K0->Fill(tt->n_vert_K0);  
      m_mean_pt->Fill(tt->mean_pt);
      m_mean_eta->Fill(tt->mean_eta);
      if(!isnan((double)(tt->prim_vrtx[0])))  m_prim_vrtx0->Fill(tt->prim_vrtx[0]);
      if(!isnan((double)(tt->prim_vrtx[1])))  m_prim_vrtx1->Fill(tt->prim_vrtx[1]);
      if(!isnan((double)(tt->prim_vrtx[2])))  m_prim_vrtx2->Fill(tt->prim_vrtx[2]);
      m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 
    }
  }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeHistGlob(St_DataSet *dst){
  
  // Fill histograms for globtrk
  //  St_DataSet *dst = g_Chain->DataSet("dst");
  St_DataSetIter dstI(dst);         
  
  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    dst_track_st  *t   = globtrk->GetTable();
    for (Int_t i = 0; i < globtrk->GetNRows(); i++,t++){
      if (t->iflag>0) {
	Float_t pT = 9999.;
	if (t->invpt) pT = 1./TMath::Abs(t->invpt);
	Float_t theta = asin(1.) - atan(t->tanl);
	Float_t eta   =-log(tan(theta/2.));
	Float_t gmom  = pT/sin(theta); 
	m_mom_trklength->Fill(t->length,gmom);
	m_pT->Fill(pT);
	m_pT_fr->Fill(pT);
	m_eta->Fill(eta);
	m_pT_eta_rec->Fill(eta,pT);
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
	Float_t degoffree = t->n_fit_point;
	Float_t chisq0_p = chisq0/(degoffree-3);
	Float_t chisq1_p = chisq1/(degoffree-2);
	m_point->Fill(t->n_point);
	m_fit_point->Fill(t->n_fit_point); 
	m_chisq0->Fill(chisq0_p);
	m_chisq1->Fill(chisq1_p);
	m_length->Fill(t->length);
	m_npoint_length->Fill(t->length,float(t->n_point));
	m_fpoint_length->Fill(t->length,float(t->n_fit_point));
	m_psi->Fill(t->psi);
	m_det_id->Fill(t->det_id);
	m_chisq0_mom->Fill(gmom,chisq0_p);
	m_chisq1_mom->Fill(gmom,chisq1_p);
      }
    }
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
  if (Debug()) cout << " *** in St_QA_Maker - filling primtrk histograms " << endl;
  St_DataSetIter dstI(dst);
  St_dst_track      *primtrk     = (St_dst_track *) dstI["primtrk"];
  
  if (primtrk) {
    dst_track_st  *t   = primtrk->GetTable();
    for (Int_t i = 0; i < primtrk->GetNRows(); i++,t++){
      if (t->iflag>0) {
	Float_t pT = 9999.;
	if (t->invpt) pT = 1./TMath::Abs(t->invpt);
	Float_t theta = TMath::Pi()/2.0 - TMath::ATan(t->tanl);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t primmom  = pT/sin(theta); 
	m_prim_pT->Fill(pT);
	m_prim_eta->Fill(eta);
	m_prim_pT_eta_rec->Fill(eta,pT);
	m_prim_point->Fill(t->n_point);
	m_prim_fit_point->Fill(t->n_fit_point);
	m_prim_psi->Fill(t->psi);
	m_prim_det_id->Fill(t->det_id);
	m_prim_tlength->Fill(t->length);
	// Al histograms
	if (t->ndegf>0) {
	  m_prim_chi2xd->Fill(t->chisq[0]/((t->ndegf+5.)/2.-3.));  
	  m_prim_chi2yd->Fill(t->chisq[1]/((t->ndegf+5.)/2.-2.));  
	}
	m_prim_mom_trklength->Fill(t->length,primmom);
	m_prim_npoint_length->Fill(t->length,float(t->n_point));
	m_prim_fpoint_length->Fill(t->length,float(t->n_fit_point));
	m_prim_chisq0_mom->Fill(primmom,float(t->chisq[0]/((t->ndegf+5.)/2.-3.)));
	m_prim_chisq1_mom->Fill(primmom,float(t->chisq[0]/((t->ndegf+5.)/2.-3.)));
      }
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistGen(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling particle histograms " << endl;
  St_DataSetIter dstI(dst);
  
  St_particle   *part     = (St_particle  *) dstI["particle"];
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
	  if (abs(p->idhep) == 11   ||       // electrons
	      abs(p->idhep) == 13   ||       // muon
	      abs(p->idhep) == 211  ||       // pion
	      abs(p->idhep) == 321  ||       // kaon
	      abs(p->idhep) == 2212) {       // proton/
	    
	    nchgpart++;	    
	    Double_t px = p->phep[0];
	    Double_t py = p->phep[1];
	    Double_t pz = p->phep[2];
	    Double_t pT    =  TMath::Sqrt(px*px+py*py);
	    Double_t theta =  TMath::ATan2 ( pT, pz );
	    //        Double_t theta =  atan2 ( pT, pz );
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
	  Float_t  p = sqrt(pT*pT+pz*pz);
	  //     Float_t z0 = abs(t->x_first[2]);
	  Float_t x0 = t->x_first[0];
	  Float_t y0 = t->x_first[1];
	  //     Float_t r0 = sqrt(x0*x0+y0*y0);
	  
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
//_____________________________________________________________________________
void St_QA_Maker::MakeHistTofEvt(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_tof_evt histograms " << endl;
  //  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         
  
  St_dst_tof_evt *tofevt = (St_dst_tof_evt *) dstI["dst_tof_evt"];
  if (tofevt) {
    dst_tof_evt_st  *t   = tofevt->GetTable();
    for (Int_t i = 0; i < tofevt->GetNRows(); i++, t++){
      m_te_ntpttrk->Fill(t->n_tpttrk);
      m_te_nttetrk->Fill(t->n_ttetrk);
      m_te_ng2ttrk->Fill(t->n_g2ttrk);
      m_te_nctfhit->Fill(t->n_ctfhit);
      m_te_nexttrk->Fill(t->n_exttrk);
      m_te_ntoftrk->Fill(t->n_toftrk);
      m_te_ntrks->Fill(t->ntrks);
      m_te_ntrks_hit->Fill(t->ntrks_hit);
      m_te_ntrks_kee->Fill(t->ntrks_kee);
      m_te_ntrks_tra->Fill(t->ntrks_tra);
      m_te_ntrks_mat->Fill(t->ntrks_mat);
    }
  }
}
//_____________________________________________________________________________
void St_QA_Maker::MakeHistTofTrk(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_tof_trk histograms " << endl;
  //  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         
  
  St_dst_tof_trk *toftrk = (St_dst_tof_trk *) dstI["dst_tof_trk"];
  if (toftrk) {
    dst_tof_trk_st  *t   = toftrk->GetTable();
    for (Int_t i = 0; i < toftrk->GetNRows(); i++, t++){
      m_tt_strk->Fill(t->s_trk);
      m_tt_phitrk->Fill(t->phi_trk);
      m_tt_stof->Fill(t->s_tof);
      m_tt_phitof->Fill(t->phi_tof);
      m_tt_tof->Fill(t->tof);
      m_tt_adc->Fill(t->adc);
    }
  }
}

//_____________________________________________________________________________
void St_QA_Maker::MakeHistEmsHitsBemc(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_ems_bemc histograms " << endl;
  //  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         
  
  St_ems_hits *ems = (St_ems_hits *) dstI["ems_hits_bemc"];
  Int_t hits1 = 0;
  Int_t hits2 = 0;
  Float_t tote1=0.0;
  Float_t tote2=0.0;
  if (ems) {
    ems_hits_st  *t   = ems->GetTable();
    for (Int_t i = 0; i < ems->GetNRows(); i++, t++){
      if(t->det == 1){   
        hits1++;
        tote1+=t->energy;
      }
      if(t->det == 2){   
        hits2++;
        tote1+=t->energy;
      }
    }
    m_ehbe_hits1->Fill(hits1);
    m_ehbe_hits2->Fill(hits2);
    m_ehbe_tnrg1->Fill(tote1);
    m_ehbe_tnrg2->Fill(tote2);
  }
  
}
//_____________________________________________________________________________
void St_QA_Maker::MakeHistEmsHitsBsmd(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_ems_bsmd histograms " << endl;
  //  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         
  
  St_ems_hits *ems = (St_ems_hits *) dstI["ems_hits_bsmd"];
  Int_t hits3 = 0;
  Int_t hits4 = 0;
  Float_t tote3=0.0;
  Float_t tote4=0.0;
  if (ems) {
    ems_hits_st  *t   = ems->GetTable();
    for (Int_t i = 0; i < ems->GetNRows(); i++, t++){
      if(t->det == 3){   
        hits3++;
        tote3+=t->energy;
      }
      if(t->det == 4){   
        hits4++;
        tote4+=t->energy;
      }
    }
    m_ehbs_hits3->Fill(hits3);
    m_ehbs_hits4->Fill(hits4);
    m_ehbs_tnrg3->Fill(tote3);
    m_ehbs_tnrg4->Fill(tote4);
  }
}

//_____________________________________________________________________________


void St_QA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_QA_Maker.cxx,v 1.21 1999/04/23 14:04:07 kathy Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
