// $Id: St_QA_Maker.cxx,v 1.10 1999/03/03 23:34:29 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
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
#include "St_dst_vertex_Table.h"

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
    const Float_t St_QA_Maker::maxtrk   = 10000.0;
    const Float_t St_QA_Maker::minvrt   = 0.0;
    const Float_t St_QA_Maker::maxvrt   = 5000.0;
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
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_QA_Maker::~St_QA_Maker(){
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

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_QA_Maker::Make(){
//  PrintInfo();

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

  return kStOK;
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistEvSum(){

// for method MakeEvSum - from table event_summary
   m_trk_tot_gd    = new TH1F("QaEvsumGlbTrkGoodDTotal","number of good track over total",
                             55,0.,1.1);
     m_trk_tot_gd->SetXTitle("number of good/total tracks");
   m_glb_trk_gd    = new TH1F("QaEvsumGlbTrkGd","number of good tracks ",
                             ntrk, mintrk, maxtrk);
   m_glb_trk_tot   = new TH1F("QaEvsumGlbTrkTot","number of tracks total ",
                             ntrk, mintrk, maxtrk);
   m_glb_trk_plusminus  = new TH1F("QaEvsumPlusMinusTrk", "number of pos. over neg trks",
                                                     ntrk, 0., 2.);  
   m_vert_total = new TH1F("QaEvsumVertTot", "total number of vertices",500,
                                                      minvrt, maxvrt);
   m_vert_V0    = new TH1F("QaEvsumVertV0", "number of V0 vertices", nvrt,minvrt,5000.);
//    m_vert_La    = new TH1F("QaEvsumVertLa", "number of La vertices", nvrt,minvrt,maxvrt);
//    m_vert_Ala   = new TH1F("QaEvsumVertLb", "number of Lb vertices", nvrt,minvrt,maxvrt);
//    m_vert_K0    = new TH1F("QaEvsumVertK0", "number of K0 vertices", nvrt,minvrt,maxvrt);   
   m_mean_pt    = new TH1F("QaEvsumMeanPt", "mean pt", nmnpt, 0., maxmpt);
   m_mean_eta   = new TH1F("QaEvsumMeanEta","mean eta", nmneta, minmeta, maxmeta);
   m_prim_vrtx0 = new TH1F("QaEvsumPrimVertX","X of primary vertex", nxyz, -5.,5.);
   m_prim_vrtx1 = new TH1F("QaEvsumPrimVertY","Y of primary vertex", nxyz,-5.,5.);
   m_prim_vrtx2 = new TH1F("QaEvsumPrimVertZ","Z of primary vertex", nxyz,-maxxyz, maxxyz);
   m_vrtx_chisq = new TH1F("QaEvsumVrtxChisq", "chisq of primary vertex",
                                            nchisq, 0., maxchisq); 
     
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistGlob(){

// for method MakeGlob - from table globtrk
   m_pT         = new TH1F("QaGlobtrkPt","pT distribution",nxpT,xminpT,xmaxpT);
   m_eta        = new TH1F("QaGlobtrkEta","eta of track",knyeta,kmineta,kmaxeta);
   m_pT_eta_rec = new TH2F("QaGlobtrkPtVsEta","pT versus eta (reconstructed)",
                           20,ymineta,ymaxeta,20,xminpT,xmaxpT);
     m_pT_eta_rec->SetXTitle("eta");
     m_pT_eta_rec->SetYTitle("pT (GeV)");
   m_mom_trklength = new TH2F("QaGlobtrkPVsTrkLength","momentum versus track length",
                           50,0.,200.,20,0.,10.);
   m_point      = new TH1F("QaGlobtrkNPoint","N points on track", npnt, minpnt, maxpnt);
   m_fit_point  = new TH1F("QaGlobtrkNPointFit","N fit points on track", npnt,minpnt, maxpnt);
   m_length     = new TH1F("QaGlobtrkLength","Length of track", nleng,minleng,maxleng);
   m_npoint_length = new TH2F("QaGlobtrkNPntLength","num points on trk vs trk length",
                              25,minleng,maxleng,25,minpnt,maxpnt);
   m_fpoint_length = new TH2F("QaGlobtrkFitPntLength","num fit points on trk vs trk length",
                              25,minleng,maxleng,25,minpnt,maxpnt);
   m_chisq0     = new TH1F("QaGlobtrkChisq0P","chisq[0]", nchisq, 0.,10.);
   m_chisq1     = new TH1F("QaGlobtrkChisq1P","chisq[1]", nchisq, 0.,10.);
   m_psi        = new TH1F("QaGlobtrkPsi","psi distribution", npsi, minpsi,maxpsi);
   m_det_id     = new TH1F("QaGlobtrkDetId","Detector ID for tracks",11,-0.5,10.5);
   m_chisq0_mom = new TH2F("QaGlobtrkChi0Mom","Chisq0 vs mom",20,0.,10.,20,0.,10.);
   m_chisq1_mom = new TH2F("QaGlobtrkChi1Mom","Chisq1 vs mom",20,0.,10.,20,0.,10.);
      
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
  m_prim_tlength    = new TH1F("QaPrimtrkLength","primtrk: track length",100,0.,200.);
  m_prim_chi2xd     = new TH1F("QaPrimtrkChiXY","primtrk: chisqxy/degf",100,0.,10.);
  m_prim_chi2yd     = new TH1F("QaPrimtrkChiSZ","primtrk: chisqsz/degf",100,0.,10.);
  m_prim_point     = new TH1F("QaPrimtrkNPoint","N points on track", npnt, minpnt, maxpnt);
  m_prim_fit_point = new TH1F("QaPrimtrkNPointFit","N fit points on track", npnt,minpnt, maxpnt);
  m_prim_psi       = new TH1F("QaPrimtrkPsi","psi distribution", 60, 0.,360.);
  m_prim_det_id    = new TH1F("QaPrimtrkDetId","Detector ID for tracks",knid,kminnid,kmaxnid);

  m_prim_mom_trklength = new TH2F("QaPrimtrkPVsTrkLength","momentum versus track length",
                           50,0.,200.,20,0.,10.);
  m_prim_npoint_length = new TH2F("QaPrimtrkNPntLength","num points on trk vs trk length",
                              25,minleng,maxleng,25,minpnt,maxpnt);
  m_prim_fpoint_length = new TH2F("QaPrimtrkFitPntLength","num fit points on trk vs trk length",
                              25,minleng,maxleng,25,minpnt,maxpnt);
  m_prim_chisq0_mom = new TH2F("QaPrimtrkChi0Mom","Chisq0 vs mom",20,0.,10.,20,0.,10.);
  m_prim_chisq1_mom = new TH2F("QaPrimtrkChi1Mom","Chisq1 vs mom",20,0.,10.,20,0.,10.);

}


//_____________________________________________________________________________
void St_QA_Maker::BookHistGen(){
// for MakeHistGen - from table particle
  m_H_pT_eta_gen = new TH2F("QaParticlePtVsEta","charged:pT versus eta (generated)",
			  nyeta,kmineta,kmaxeta,nxpT,xminpT,xmaxpT);
    m_H_pT_eta_gen->SetXTitle("eta");
    m_H_pT_eta_gen->SetYTitle("pT (GeV)");
 m_H_pT_gen  = new TH1F("QaParticlePt","charged: pt (generated)",nxpT,xminpT,xmaxpT);
 m_H_eta_gen = new TH1F("QaParticleEta","charged:eta (generated)",nyeta,kmineta,kmaxeta);
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistV0(){

// for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist  = new TH1F("QaDstV0VertexLambdaMass","dst_v0_vertex: Lambda mass",50,1.05,1.25);
  m_ev0_k0ma_hist  = new TH1F("QaDstV0VertexK0Mass","dst_v0_vertex: k0 mass",50,.4,.6);

}

//_____________________________________________________________________________
void St_QA_Maker::BookHistPID(){

// for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie

  m_p_dedx_rec = new TH2F("QaPidPrimtrkDstdedxPVsDedx","primtrk-dst_dedx: p versus dedx (reconstructed)",
                           cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
    m_p_dedx_rec->SetYTitle("dedx");
    m_p_dedx_rec->SetXTitle("p (GeV)");

}

//_____________________________________________________________________________
void St_QA_Maker::BookHistVertex(){
// for MakeHistVertex - from table dst_vertex


    m_v_detid = new TH1F("QaVertexDetId"," vertex: Detector ID ",40,0.,40.);
    m_v_vtxid = new TH1F("QaVertexVtxId"," vertex: Vertex ID ",10,0.,10.);
    m_v_x     = new TH1F("QaVertexX"," vertex: x ",50,-25.,25.);
    m_v_y     = new TH1F("QaVertexY"," vertex: y ",50,-25.,25.);
    m_v_z     = new TH1F("QaVertexZ"," vertex: z ",50,-50.,50.);
    m_v_pchi2 = new TH1F("QaVertexChisq"," vertex: chisq/dof ",50,0.,5.);

}
//_____________________________________________________________________________
void St_QA_Maker::MakeHistEvSum(){
 //  PrintInfo();
 // Fill histograms for event summary
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         
   
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dstI["event_summary"];
  if (event_summary) {
      dst_event_summary_st  *it_m = event_summary->GetTable();
      for (Int_t j = 0; j < event_summary->GetNRows(); j++) {
       dst_event_summary_st *tt = it_m + j;  
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

void St_QA_Maker::MakeHistGlob(){

   // Fill histograms for globtrk
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         

  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    table_head_st *trk_h = globtrk->GetHeader();
    dst_track_st  *trk   = globtrk->GetTable();
    for (Int_t i = 0; i < globtrk->GetNRows(); i++){
      dst_track_st *t = trk + i;
      if (t->iflag>0) {
       Float_t pT = 9999.;
       if (t->invpt) pT = 1./TMath::Abs(t->invpt);
       Float_t theta = asin(1.) - atan(t->tanl);
       Float_t eta   =-log(tan(theta/2.));
       Float_t gmom  = pT/sin(theta); 
       m_mom_trklength->Fill(t->length,gmom);
       m_pT->Fill(pT);
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

 void St_QA_Maker::MakeHistDE() {
   // Fill histograms for dE/dx

  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);

  
   St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];
   if(dst_dedx) {
      dst_dedx_st *dedx = dst_dedx->GetTable();
      for (Int_t i = 0; i < dst_dedx->GetNRows(); i++) {
       dst_dedx_st *d = dedx + i;
       m_ndedx->Fill(d->ndedx);
       m_dedx0->Fill(d->dedx[0]*1e6);
       m_dedx1->Fill(d->dedx[1]*1e6);
      }
     }
   }

//_____________________________________________________________________________


void St_QA_Maker::MakeHistPrim(){
   cout << " *** in St_QA_Maker - filling primtrk histograms " << endl;
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);
  St_dst_track      *primtrk     = (St_dst_track *) dstI["primtrk"];

  if (primtrk) {
    table_head_st *trk_h = primtrk->GetHeader();
    dst_track_st  *trk   = primtrk->GetTable();
    for (Int_t i = 0; i < primtrk->GetNRows(); i++){
      dst_track_st *t = trk + i;
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


void St_QA_Maker::MakeHistGen(){
   cout << " *** in St_QA_Maker - filling particle histograms " << endl;
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);

  St_particle   *part     = (St_particle  *) dstI["particle"];
      if (part){
        particle_st *p = part->GetTable();
        for (Int_t l=0; l < part->GetNRows(); l++, p++){
          //
          //  select only particles which can be detected
          //  in the STAR detector. Here we restrict us to
	  //  the most common species.
	  //
          if (abs(p->idhep) == 11   ||     // electrons
              abs(p->idhep) == 13   ||     // muon
              abs(p->idhep) == 211  ||     // pion
              abs(p->idhep) == 321  ||     // kaon
              abs(p->idhep) == 2212) {     // proton
	    
	    if (p->isthep == 1) {
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
	    }
	  }
	}
      }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistV0(){
   cout << " *** in St_QA_Maker - filling dst_v0_vertex histograms " << endl;
  St_DataSet *dst = gStChain->DataSet("dst");
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


void St_QA_Maker::MakeHistPID(){
   cout << " *** in St_QA_Maker - filling PID histograms " << endl;

  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);        

  // spectra-PID diagnostic histograms
  St_dst_track      *primtrk     = (St_dst_track     *) dstI["primtrk"];
  St_dst_dedx       *dst_dedx    = (St_dst_dedx *) dstI["dst_dedx"];

  if (dst_dedx && primtrk) {
     	dst_dedx_st  *de   = dst_dedx->GetTable();
        dst_track_st  *trk   = primtrk->GetTable();
	// loop over dedx entries
        for (Int_t l = 0; l < dst_dedx->GetNRows(); l++){
	       dst_dedx_st *d = de + l;
               Float_t dedx_m = d->dedx[0];
               Int_t igl = d->id_track;
               Int_t igl_use = igl - 1;
    // this is bad style, since it assumes the global track has not been sorted
    // it works for now
               dst_track_st  *t = trk + igl_use ;
               Float_t invpt = t->invpt;
	       Float_t pT = 9999.;
	       if (invpt) pT = 1./TMath::Abs(invpt);
               Float_t pz = pT*t->tanl;
               Float_t  p = sqrt(pT*pT+pz*pz);
               Float_t z0 = abs(t->x_first[2]);
               Float_t x0 = t->x_first[0];
               Float_t y0 = t->x_first[1];
               Float_t r0 = sqrt(x0*x0+y0*y0);

	       if (d->det_id==1 && d->ndedx >15 ) { 
		    m_p_dedx_rec->Fill(p,(float)(dedx_m*1e6)); // change from GeV/cm to keV/cm
	       }
	}
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistVertex(){
   cout << " *** in St_QA_Maker - filling vertex histograms " << endl;
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);
  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];

  if (vertex) {
    table_head_st *trk_h = vertex->GetHeader();
    dst_vertex_st  *trk   = vertex->GetTable();
    for (Int_t i = 0; i < vertex->GetNRows(); i++){
      dst_vertex_st *t = trk + i;
      //         if (t->iflag>0) {            
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


void St_QA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_QA_Maker.cxx,v 1.10 1999/03/03 23:34:29 kathy Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
