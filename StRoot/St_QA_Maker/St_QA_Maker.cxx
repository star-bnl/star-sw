// $Id: St_QA_Maker.cxx,v 1.1 1999/02/08 19:28:23 didenko Exp $
// $Log: St_QA_Maker.cxx,v $
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
#include "TMath.h"
#include "St_QA_Maker.h"
#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"
#include "St_dst_track_Table.h"
#include "St_ev0_aux_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_dst_event_summary_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"


   const Int_t St_QA_Maker::nxpT = 50;
   const Int_t St_QA_Maker::nyeta = 50;
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
    const Int_t nmass = 40;
    const Int_t ntau = 40;
    const Int_t ndedx = 50;
    const Int_t npnt = 50;
    const Int_t nleng = 50;
    const Int_t npsi = 36;
    const Int_t ntrk = 100;
    const Int_t nvrt = 100;
    const Int_t nmnpt = 50;
    const Int_t nmneta = 40;
    const Int_t nxyz = 50;
    

    const Float_t minpsi = 0.0;
    const Float_t maxpsi = 360.0;
    const Float_t minchisq =  0.;
    const Float_t maxchisq =  50.0;
    const Float_t minmass =  0.0;
    const Float_t maxmass =  2.0;
    const Float_t mindedx =  0.0000001;
    const Float_t maxdedx =  0.0001;
    const Float_t minpnt = 0.0;
    const Float_t maxpnt = 50.0;
    const Float_t minleng = 0.0;
    const Float_t maxleng = 150.0;
    const Float_t mintau = 0.0;
    const Float_t maxtau = 20.0;
    const Float_t mintrk = 0.0;
    const Float_t maxtrk = 10000.0;
    const Float_t minvrt = 0.0;
    const Float_t maxvrt = 100.0;
    const Float_t minmpt = 0.0;
    const Float_t maxmpt = 5.0;
    const Float_t minmeta = -1.0;
    const Float_t maxmeta = 1.0;
    const Float_t minxyz = 0.0;
    const Float_t maxxyz = 50.0;
      
   m_pT_eta_rec = new TH2F("pT_eta_rec","pT versus eta (reconstructed)",
                           nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
   m_pT_eta_rec->SetXTitle("eta");
   m_pT_eta_rec->SetYTitle("pT (GeV)");
   m_pT_eta_gen = new TH2F("pT_eta_gen","pT versus eta (generated)",
                           nyeta,ymineta,ymaxeta,nxpT,xminpT,xmaxpT);
   m_pT_eta_gen->SetXTitle("eta");
   m_pT_eta_gen->SetYTitle("pT (GeV)");
   m_trk_tot_gd = new TH2F("glb_trk","number of good track versus total",
                             ntrk, mintrk, maxtrk, ntrk, mintrk, maxtrk);
   m_trk_tot_gd->SetXTitle("total number of tracks");
   m_trk_tot_gd->SetYTitle("number of good tracks");
   m_trk_pls_mns = new TH2F("trk_pls_mns", "number of (+) tracks versusof (-)",  
                                  ntrk, mintrk, maxtrk, ntrk, mintrk, maxtrk);
   m_trk_pls_mns->SetXTitle("number of (+) tracks");
   m_trk_pls_mns->SetYTitle("number of (-) tracks");
      
   m_pT   = new TH1F("pT_rec","pT distribution",nxpT,xminpT,xmaxpT);
   m_eta  = new TH1F("eta_rec","eta distribution",nyeta,ymineta,ymaxeta);
   m_psi  = new TH1F("phi_rec","azimutal distribution", npsi, minpsi,maxpsi);
   m_pTv   = new TH1F("pTV_rec","V0 pT distribution",nxpT,xminpT,xmaxpT);
   m_etav  = new TH1F("etaV_rec","V0 eta distribution",nyeta,ymineta,ymaxeta);
   m_pTK_gen   = new TH1F("pTK_gen","K0 pT distribution generated", 
                                                    nxpT,xminpT,xmaxpT);
   m_etaK_gen  = new TH1F("etaK_gen","K0 eta distribution generated",
                                                 nyeta,ymineta,ymaxeta);
   m_pTL_gen   = new TH1F("pTL_gen","Lambda pT distribution generated",
                                                    nxpT,xminpT,xmaxpT);
   m_etaL_gen  = new TH1F("etaL_gen","Lambda eta distribution generated",
                                                 nyeta,ymineta,ymaxeta);
   m_pTC_gen   = new TH1F("pTC_gen","Charged particles pT  generated", 
                                                  nxpT,xminpT,xmaxpT);
   m_etaC_gen  = new TH1F("etaC_gen","Charged particles eta  generated",
                                               nyeta,ymineta,ymaxeta);
   m_point = new TH1F("npoint","N points on track", npnt, minpnt, maxpnt);
   m_fit_point = new TH1F("n_fit_point","N fit points on track", npnt,
                                                          minpnt, maxpnt);
   m_length = new TH1F("length","Length of track", nleng,minleng,maxleng);
   m_chisq0 = new TH1F("chisq0_p","chisg[0]", nchisq, minchisq, maxchisq);
   m_chisq1 = new TH1F("chisq1_p","chisq[1]", nchisq, minchisq, maxchisq);
   m_mass_la = new TH1F("mass_la","inv_mas_La", nmass, minmass, maxmass);
   m_mass_lb = new TH1F("mass_lb","inv_mas_Lb", nmass, minmass, maxmass);
   m_mass_k0 = new TH1F("mass_k0","inv_mas_k0", nmass, minmass, maxmass);
   m_tau_la = new TH1F("tau_la","time of life for La candidate", ntau,
                                                          mintau, maxtau);
   m_tau_lb = new TH1F("tau_lb","time of life for Lb candidate", ntau,
                                                          mintau, maxtau);
   m_tau_k0 = new TH1F("tau_k0","time of life for_k0 candidate", ntau,
                                                          mintau, maxtau);   
   m_ndedx  = new TH1F("ndedx", "number of point to define dE/dx", npnt,
                                                          minpnt, maxpnt);  
   m_dedx0 = new TH1F("dedx0","dE/dx[0]", ndedx, mindedx, maxdedx);
   m_dedx1 = new TH1F("dedx1","dE/dx[1]", ndedx, mindedx, maxdedx);
   m_glb_trk_plus = new TH1F("plus_trk", "number of positive tracks",
                                                     ntrk, mintrk, maxtrk);   
   m_glb_trk_minus = new TH1F("minus_trk", "number of negative tracks",
                                                     ntrk, mintrk, maxtrk);
   m_vert_total = new TH1F("vert_total", "total number of vertices",nvrt,
                                                      minvrt, maxvrt);
   m_vert_V0 = new TH1F("vert_V0", "number of V0 vertices", nvrt,minvrt,maxvrt);
   m_vert_La = new TH1F("vert_La", "number of La vertices", nvrt,minvrt,maxvrt);
   m_vert_Ala= new TH1F("vert_Lb", "number of Lb vertices", nvrt,minvrt,maxvrt);
   m_vert_K0 = new TH1F("vert_K0", "number of K0 vertices", nvrt,minvrt,maxvrt);   
   m_mean_pt = new TH1F("mean_pt", "mean pt", nmnpt, minmpt, maxmpt);
   m_mean_eta = new TH1F("mean_eta","mean eta", nmneta, minmeta, maxmeta);
   m_prim_vrtx0 = new TH1F("prim_vrtx_X","X of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_prim_vrtx1 = new TH1F("prim_vrtx_Y","Y of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_prim_vrtx2 = new TH1F("prim_vrtx_Y","Y of primary vertex", nxyz,
                                                        minxyz, maxxyz);
   m_vrtx_chisq = new TH1F("vrtx_chisq", "chisq of primary vertex",
                                            nchisq, minchisq, maxchisq); 
     
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_QA_Maker::Make(){
  //  PrintInfo();
  // Fill histograms for global tracks
  St_DataSet *global = gStChain->DataSet("global");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");
  St_dst_track *globtrk = (St_dst_track *) dst("globtrk");
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
    MakeV0();
    MakeDE();
  }
  MakeGen();
  MakeEvSum();
  return kStOK;
}
//_____________________________________________________________________________
void St_QA_Maker::MakeEvSum(){
 //  PrintInfo();
 // Fill histograms for event summary
  St_DataSet *global = gStChain->DataSet("run_summary");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");
   
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dst("event_summary");
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
//_____________________________________________________________________________

void St_QA_Maker::MakeV0(){
   // Fill histograms for V0
  St_DataSet *global = gStChain->DataSet("global");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");

   St_ev0_aux *ev0out = (St_ev0_aux *) dst("ev0out");
   if(ev0out) {
       ev0_aux_st *evopat = ev0out->GetTable();
       for (Int_t i = 0; i < ev0out->GetNRows(); i++){
       ev0_aux_st *ev = evopat + i;
       Float_t pvx = ev->px;
       Float_t pvy = ev->py;
       Float_t thetav = ev->theta;
       Float_t pTv = sqrt(pvx*pvx + pvy*pvy);
       Float_t etav = -log(tan(thetav/2.));
       m_pTv->Fill(pTv);
       m_etav->Fill(etav);
       m_mass_la->Fill(ev->inv_mass_la) ;
       m_mass_lb->Fill(ev->inv_mass_lb) ;
       m_mass_k0->Fill(ev->inv_mass_k0) ;
       m_tau_la->Fill(ev->tau_la);
       m_tau_lb->Fill(ev->tau_lb);
       m_tau_k0->Fill(ev->tau_k0); 
     }       
  }
}
//_____________________________________________________________________________
 void St_QA_Maker::MakeDE() {
   // Fill histograms for dE/dx

  St_DataSet *global = gStChain->DataSet("global");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");
  
   St_dst_dedx *dst_dedx = (St_dst_dedx *) dst("dst_dedx");
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

  St_DataSet *global = gStChain->DataSet("global");
  St_DataSetIter dst(global);         // data/global/dst
  dst.Cd("dst");

   St_hepe_gent *hepev = (St_hepe_gent *) dst("hepe_gent");
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
     St_DataSet *evgen = gStChain->DataSet("evgen");
      if (evgen) {
      St_DataSetIter local(evgen);
      St_particle *pa = (St_particle *) local("particle");
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

void St_QA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_QA_Maker.cxx,v 1.1 1999/02/08 19:28:23 didenko Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
