// $Id: St_QA_Maker.cxx,v 1.4 1999/02/23 22:22:22 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
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
#include "TMath.h"
#include "St_QA_Maker.h"

#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"
#include "St_dst_track_Table.h"
#include "St_dst_v0_vertex_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_dst_event_summary_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"


   const Int_t   St_QA_Maker::nxpT = 50;
   const Int_t   St_QA_Maker::nyeta = 50;
   const Float_t St_QA_Maker::xminpT = 0.0;
   const Float_t St_QA_Maker::xmaxpT = 5.0;
   const Float_t St_QA_Maker::ymineta = -2.0;
   const Float_t St_QA_Maker::ymaxeta =  2.0;

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

    const Int_t nchisq = 50;
    const Int_t nmass  = 40;
    const Int_t ntau   = 40;
    const Int_t ndedx  = 50;
    const Int_t npnt   = 50;
    const Int_t nleng  = 50;
    const Int_t npsi   = 36;
    const Int_t ntrk   = 100;
    const Int_t nvrt   = 100;
    const Int_t nmnpt  = 50;
    const Int_t nmneta = 40;
    const Int_t nxyz   = 50;
    

    const Float_t minpsi   = 0.0;
    const Float_t maxpsi   = 360.0;
    const Float_t minchisq = 0.;
    const Float_t maxchisq = 50.0;
    const Float_t minmass  = 0.0;
    const Float_t maxmass  = 2.0;
    const Float_t mindedx  = 0.0000001;
    const Float_t maxdedx  = 0.0001;
    const Float_t minpnt   = 0.0;
    const Float_t maxpnt   = 50.0;
    const Float_t minleng  = 0.0;
    const Float_t maxleng  = 150.0;
    const Float_t mintau   = 0.0;
    const Float_t maxtau   = 20.0;
    const Float_t mintrk   = 0.0;
    const Float_t maxtrk   = 10000.0;
    const Float_t minvrt   = 0.0;
    const Float_t maxvrt   = 100.0;
    const Float_t minmpt   = 0.0;
    const Float_t maxmpt   = 5.0;
    const Float_t minmeta  = -1.0;
    const Float_t maxmeta  = 1.0;
    const Float_t minxyz   = 0.0;
    const Float_t maxxyz   = 50.0;
    

 //book histograms --------------

// for method MakeEvSum - from table event_summary
   m_trk_tot_gd = new TH2F("QA_evsum_glb_trk","number of good track versus total",
                             ntrk, mintrk, maxtrk, ntrk, mintrk, maxtrk);
     m_trk_tot_gd->SetXTitle("total number of tracks");
     m_trk_tot_gd->SetYTitle("number of good tracks");
   m_glb_trk_plus = new TH1F("QA_evsum_plus_trk", "number of positive tracks",
                                                     ntrk, mintrk, maxtrk);   
   m_glb_trk_minus = new TH1F("QA_evsum_minus_trk", "number of negative tracks",
                                                     ntrk, mintrk, maxtrk);
   m_trk_pls_mns = new TH2F("QA_evsum_trk_pls_mns", "number of (+) vs (-) trks",  
                                  ntrk, mintrk, maxtrk, ntrk, mintrk, maxtrk);
     m_trk_pls_mns->SetXTitle("number of (+) tracks");
     m_trk_pls_mns->SetYTitle("number of (-) tracks");


   m_vert_total = new TH1F("QA_evsum_vert_total", "total number of vertices",nvrt,
                                                      minvrt, maxvrt);
   m_vert_V0 = new TH1F("QA_evsum_vert_V0", "number of V0 vertices", nvrt,minvrt,maxvrt);
   m_vert_La = new TH1F("QA_evsum_vert_La", "number of La vertices", nvrt,minvrt,maxvrt);
   m_vert_Ala= new TH1F("QA_evsum_vert_Lb", "number of Lb vertices", nvrt,minvrt,maxvrt);
   m_vert_K0 = new TH1F("QA_evsum_vert_K0", "number of K0 vertices", nvrt,minvrt,maxvrt);   
   m_mean_pt = new TH1F("QA_evsum_mean_pt", "mean pt", nmnpt, minmpt, maxmpt);
   m_mean_eta = new TH1F("QA_evsum_mean_eta","mean eta", nmneta, minmeta, maxmeta);
   m_prim_vrtx0 = new TH1F("QA_evsum_prim_vrtx_X","X of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_prim_vrtx1 = new TH1F("QA_evsum_prim_vrtx_Y","Y of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_prim_vrtx2 = new TH1F("QA_evsum_prim_vrtx_Y","Y of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_vrtx_chisq = new TH1F("QA_evsum_vrtx_chisq", "chisq of primary vertex",
                                            nchisq, minchisq, maxchisq); 
     

// for method MakeGlob - from table globtrk
   m_pT    = new TH1F("QA_globtrk_pT_rec","pT distribution",nxpT,xminpT,xmaxpT);
   m_eta   = new TH1F("QA_globtrk_eta_rec","eta distribution",nyeta,ymineta,ymaxeta);
   m_pT_eta_rec = new TH2F("QA_globtrk_pT_eta_rec","pT versus eta (reconstructed)",
                           nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
     m_pT_eta_rec->SetXTitle("eta");
     m_pT_eta_rec->SetYTitle("pT (GeV)");
   m_point = new TH1F("QA_globtrk_npoint","N points on track", npnt, minpnt, maxpnt);
   m_fit_point = new TH1F("QA_globtrk_n_fit_point","N fit points on track", npnt,
                                                          minpnt, maxpnt);
   m_length  = new TH1F("QA_globtrk_length","Length of track", nleng,minleng,maxleng);
   m_chisq0  = new TH1F("QA_globtrk_chisq0_p","chisq[0]", nchisq, minchisq, maxchisq);
   m_chisq1  = new TH1F("QA_globtrk_chisq1_p","chisq[1]", nchisq, minchisq, maxchisq);
   m_psi   = new TH1F("QA_globtrk_phi_rec","azimutal distribution", npsi, minpsi,maxpsi);
      
   
// for method MakeDE - from table dst_dedx
   m_ndedx   = new TH1F("QA_dst_dedx_ndedx", "number of point to define dE/dx", npnt,
                                                          minpnt, maxpnt);  
   m_dedx0   = new TH1F("QA_dst_dedx_dedx0","dE/dx[0]", ndedx, mindedx, maxdedx);
   m_dedx1   = new TH1F("QA_dst_dedx_dedx1","dE/dx[1]", ndedx, mindedx, maxdedx);


// for MakeHistPrim - from table primtrk
  m_prim_pT   = new TH1F("QA_primtrk_pT","primtrk: pT distribution",nxpT,xminpT,xmaxpT);
  m_prim_eta  = new TH1F("QA_primtrk_eta","primtrk: eta distribution",nyeta,ymineta,ymaxeta);
  m_prim_pT_eta_rec = new TH2F("QA_primtrk_pT_eta_rec","primtrk: pT versus eta (reconstructed)",
			  nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
    m_prim_pT_eta_rec->SetXTitle("eta");
    m_prim_pT_eta_rec->SetYTitle("pT (GeV)");
  m_prim_tlength = new TH1F("QA_primtrk_tlength","primtrk: track length",100,0.,200.);
  m_prim_chi2xd  = new TH1F("QA_primtrk_chi2xd","primtrk: - x chisq/degf",100,0.,10.);
  m_prim_chi2yd  = new TH1F("QA_primtrk_chi2yd","primtrk: - y chisq/degf",100,0.,10.);


// for MakeHistGen - from table particle
  m_H_pT_eta_gen = new TH2F("QA_particle_pT_eta_gen","pT versus eta (generated)",
			  nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
    m_H_pT_eta_gen->SetXTitle("eta");
    m_H_pT_eta_gen->SetYTitle("pT (GeV)");


// for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist  = new TH1F("QA_dst_v0_vertex_lama","dst_v0_vertex: Lambda mass",50,1.05,1.25);
  m_ev0_k0ma_hist  = new TH1F("QA_dst_v0_vertex_k0ma","dst_v0_vertex: k0 mass",50,.4,.6);


// for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  Int_t cnp = 100;
  Int_t cndedx = 100;
  Float_t cminp = 0.0;
  Float_t cmaxp = 2.0;
  Float_t cmindedx = 0.0;
  Float_t cmaxdedx =  0.1e-04;

  m_p_dedx_rec = new TH2F("QA_primtrk_dst_dedx_p_dedx_rec","primtrk-dst_dedx: p versus dedx (reconstructed)",
                           cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
    m_p_dedx_rec->SetYTitle("dedx");
    m_p_dedx_rec->SetXTitle("p (GeV)");

// for method MakeV0 - from table dst_v0_vertex - doesn't work because using ev0out table
   //   m_pTv   = new TH1F("pTV_rec","V0 pT distribution",nxpT,xminpT,xmaxpT);
   //m_etav  = new TH1F("etaV_rec","V0 eta distribution",nyeta,ymineta,ymaxeta);
   //m_mass_la = new TH1F("mass_la","inv_mas_La", nmass, minmass, maxmass);
   //m_mass_lb = new TH1F("mass_lb","inv_mas_Lb", nmass, minmass, maxmass);
   //m_mass_k0 = new TH1F("mass_k0","inv_mas_k0", nmass, minmass, maxmass);
   //m_tau_la  = new TH1F("tau_la","time of life for La candidate", ntau,
   //                                                       mintau, maxtau);
   //m_tau_lb  = new TH1F("tau_lb","time of life for Lb candidate", ntau,
   //                                                       mintau, maxtau);
   //m_tau_k0  = new TH1F("tau_k0","time of life for_k0 candidate", ntau,
   //                                                       mintau, maxtau);  

// for method MakeGen - from table particle & table hepe_gent
   //m_pT_eta_gen = new TH2F("pT_eta_gen","pT versus eta (generated)",
   //                        nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
   //  m_pT_eta_gen->SetXTitle("eta");
   //  m_pT_eta_gen->SetYTitle("pT (GeV)");
   //m_pTK_gen   = new TH1F("pTK_gen","K0 pT distribution generated", 
   //                                                 nxpT,xminpT,xmaxpT);
   //m_etaK_gen  = new TH1F("etaK_gen","K0 eta distribution generated",
   //                                              nyeta,ymineta,ymaxeta);
   //m_pTL_gen   = new TH1F("pTL_gen","Lambda pT distribution generated",
   //                                                 nxpT,xminpT,xmaxpT);
   //m_etaL_gen  = new TH1F("etaL_gen","Lambda eta distribution generated",
   //                                              nyeta,ymineta,ymaxeta);
   //m_pTC_gen   = new TH1F("pTC_gen","Charged particles pT  generated", 
   //                                               nxpT,xminpT,xmaxpT);
   //m_etaC_gen  = new TH1F("etaC_gen","Charged particles eta  generated",
   //                                            nyeta,ymineta,ymaxeta);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_QA_Maker::Make(){
  //  PrintInfo();

// Call methods to fill histograms

// histograms from table event_summary
  MakeEvSum();
// histograms from table globtrk
  MakeGlob();
// histograms from table dst_dedx
  MakeDE();
// histograms from table primtrk
  MakeHistPrim();
// histograms from table particle
 MakeHistGen();
// histograms from table dst_v0_vertex
  MakeHistV0();
// histograms from table primtrk & dst_dedx
  MakeHistPID();
// histograms from table dst_v0_vertex - doesn't work
//  MakeV0();   
// histograms from table particle & hepe_gent - doesn't work
//  MakeGen();    

  return kStOK;
}
//_____________________________________________________________________________
void St_QA_Maker::MakeEvSum(){
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
       m_trk_tot_gd->Fill(trk_tot,trk_good); 
       m_glb_trk_plus->Fill(trk_plus);
       m_glb_trk_minus->Fill(trk_minus);
       m_trk_pls_mns->Fill(trk_plus,trk_minus);
       m_vert_total->Fill(tt->n_vert_total);
       m_vert_V0->Fill(tt->n_vert_V0);
       m_vert_La->Fill(tt->n_vert_Lambda);
       m_vert_Ala->Fill(tt->n_vert_ALambda);
       m_vert_K0->Fill(tt->n_vert_K0);  
       m_mean_pt->Fill(tt->mean_pt);
       m_mean_eta->Fill(tt->mean_eta);
       m_prim_vrtx0->Fill(tt->prim_vrtx[0]);
       m_prim_vrtx1->Fill(tt->prim_vrtx[1]);
       m_prim_vrtx2->Fill(tt->prim_vrtx[2]);
       m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 
      }
   }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeGlob(){

   // Fill histograms for globtrk
  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         

  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    table_head_st *trk_h = globtrk->GetHeader();
    dst_track_st  *trk   = globtrk->GetTable();
    for (Int_t i = 0; i < globtrk->GetNRows(); i++){
      dst_track_st *t = trk + i;
      Float_t pT = 9999.;
      if (t->invpt) pT = 1./TMath::Abs(t->invpt);
      Float_t theta = asin(1.) - atan(t->tanl);
      Float_t eta   =-log(tan(theta/2.));
      m_pT->Fill(pT);
      m_eta->Fill(eta);
      m_pT_eta_rec->Fill(eta,pT);
      Float_t chisq0 = t->chisq[0];
      Float_t chisq1 = t->chisq[1]; 
      Float_t pointk = t->n_point ;
      Float_t chisq0_p = chisq0/pointk ;
      Float_t chisq1_p = chisq1/pointk ;
      m_point->Fill(pointk);
      m_fit_point->Fill(t->n_fit_point); 
      m_chisq0->Fill(chisq0_p);
      m_chisq1->Fill(chisq1_p);
      m_length->Fill(t->length);
      m_psi->Fill(t->psi);
    }
  }       
}

//_____________________________________________________________________________

 void St_QA_Maker::MakeDE() {
   // Fill histograms for dE/dx

  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);

  
   St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];
   if(dst_dedx) {
      dst_dedx_st *dedx = dst_dedx->GetTable();
      for (Int_t i = 0; i < dst_dedx->GetNRows(); i++) {
       dst_dedx_st *d = dedx + i;
       m_ndedx->Fill(d->ndedx);
       m_dedx0->Fill(d->dedx[0]);
       m_dedx1->Fill(d->dedx[1]);
      }
     }
   }

//_____________________________________________________________________________


void St_QA_Maker::MakeGen() {
  //  Fill histograms for event generator

  St_DataSet *dst = gStChain->DataSet("dst");
  St_DataSetIter dstI(dst);         

   St_hepe_gent *hepev = (St_hepe_gent *) dstI["hepe_gent"];
    if (hepev) {
      table_head_st *t1_h = hepev->GetHeader();
      hepe_gent_st *particle = hepev->GetTable();
      for (Int_t l=0; l < hepev->GetNRows(); l++){
        hepe_gent_st *p = particle+l;
        if (p->isthep == 1) {
          Float_t px = p->phep[0];
          Float_t py = p->phep[1];
          Float_t pz = p->phep[2];
          Float_t pT    =  sqrt(px*px+py*py);
//        Double_t theta =  TMath::Atan2 ( pT, pz );
          Double_t theta =  atan2 ( pT, pz );
          Float_t  eta  = -log(tan(theta/2.));
          m_pT_eta_gen->Fill(eta,pT); 
	  if(p->idhep == 310 ) {
                m_pTK_gen->Fill(pT);
                m_etaK_gen->Fill(eta);
	     }
          if(abs(p->idhep) == 3122) {
                m_pTL_gen->Fill(pT);
                m_etaL_gen->Fill(eta);
	     }                 
          if(abs(p->idhep) == 211 || abs(p->idhep) == 321)
	     {
                m_pTC_gen->Fill(pT);
                m_etaC_gen->Fill(eta);
	     }
          if( abs(p->idhep) == 2212)
	    {
               m_pTC_gen->Fill(pT);
               m_etaC_gen->Fill(eta);
            }                   
	 }
      }
  }
   else {
     St_DataSet *evgen = gStChain->DataSet("geant");
      if (evgen) {
      St_DataSetIter local(evgen);
      local.Cd("Event");
      St_particle *pa = (St_particle *) local["particle"];
      if (pa){
        particle_st *particle = pa->GetTable();
        for (Int_t l=0; l < pa->GetNRows(); l++){
          particle_st *p = particle+l;
          if (p->isthep == 1) {
            Float_t px = p->phep[0];
            Float_t py = p->phep[1];
           Float_t pz = p->phep[2];
            Float_t pT    =  sqrt(px*px+py*py);
            Double_t theta =  atan2 ( pT, pz );
            Float_t  eta  = -log(tan(theta/2.));
            m_pT_eta_gen->Fill(eta,pT);
	  }
  	 }
      }
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
      Float_t pT = 9999.;
      if (t->invpt) pT = 1./TMath::Abs(t->invpt);
      Float_t theta = TMath::Pi() - TMath::ATan(t->tanl);
      Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
      m_prim_pT->Fill(pT);
      m_prim_eta->Fill(eta);
      m_prim_pT_eta_rec->Fill(eta,pT);
      // Al histograms
      m_prim_tlength->Fill(t->length);
      if (t->ndegf>0) {
        m_prim_chi2xd->Fill(t->chisq[0]/((t->ndegf+5.)/2.-3.));  
        m_prim_chi2yd->Fill(t->chisq[1]/((t->ndegf+5.)/2.-2.));  
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
          if (p->isthep == 1) {
            Double_t px = p->phep[0];
            Double_t py = p->phep[1];
            Double_t pz = p->phep[2];
            Double_t pT    =  TMath::Sqrt(px*px+py*py);
            Double_t theta =  TMath::ATan2 ( pT, pz );
	    //        Double_t theta =  atan2 ( pT, pz );
            Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
            m_H_pT_eta_gen->Fill(eta, (Float_t) pT);
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
		 m_p_dedx_rec->Fill(p,dedx_m);
	       }
	}
  }
}

//_____________________________________________________________________________

// doesn't work - tables have changed  - now use MakeHistV0 instead

void St_QA_Maker::MakeV0(){
   // Fill histograms for V0
  St_DataSet *global = gStChain->DataSet("global");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");

  St_dst_v0_vertex *ev0out = (St_dst_v0_vertex *) dst["dst_v0_vertex"];
   if(ev0out) {
       dst_v0_vertex_st *evopat = ev0out->GetTable();
       for (Int_t i = 0; i < ev0out->GetNRows(); i++){
       dst_v0_vertex_st *ev = evopat + i;
       //Float_t pvx = ev->px;
       //Float_t pvy = ev->py;
       //Float_t thetav = ev->theta;
       //Float_t pTv = sqrt(pvx*pvx + pvy*pvy);
       //Float_t etav = -log(tan(thetav/2.));
       //m_pTv->Fill(pTv);
       //m_etav->Fill(etav);
       //m_mass_la->Fill(ev->inv_mass_la) ;
       //m_mass_lb->Fill(ev->inv_mass_lb) ;
       //m_mass_k0->Fill(ev->inv_mass_k0) ;
       //m_tau_la->Fill(ev->tau_la);
       //m_tau_lb->Fill(ev->tau_lb);
       //m_tau_k0->Fill(ev->tau_k0); 
     }       
  }
}

//_____________________________________________________________________________


void St_QA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_QA_Maker.cxx,v 1.4 1999/02/23 22:22:22 kathy Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
