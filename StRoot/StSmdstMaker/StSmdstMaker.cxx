// $Id: StSmdstMaker.cxx,v 1.1 1999/04/14 15:10:35 genevb Exp $
// $Log: StSmdstMaker.cxx,v $
// Revision 1.1  1999/04/14 15:10:35  genevb
// Add StSmdstMaker source files
//
// Revision 1.2  1999/01/19 22:42:32  genevb
// update comments
//
//
// Revision 1.1 1999/01/18 12:46:19 genevb
// Strangeness microdst analysis
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSmdstMaker class for Makers                                      //
//                                                                      //
//   This Maker is for the STAR strangeness physics working             //
//   group's microdst analysis.                                         //
//                                                                      //
//   Built-in diagnostic histograms can be turned on with a call to     //
//   DoHistograms(n), where n is the number of events between screen    //
//   redraws (updates) of the histograms. If n is ommitted, then        //
//   updates occur for every event.                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "TROOT.h"

#include "StSmdstMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include <TH2.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <TStyle.h>
#include "StEventReaderMaker/StEventReaderMaker.h"
#include "StEvent/StEvent.hh"
#include "StEvent/StV0Vertex.hh"
#include "StEvent/StXiVertex.hh"
#include "math_constants.h"
#include "PhysicalConstants.h"
#include "TMath.h"
#include "StMagF.h"

ClassImp(StSmdstMaker)

//_____________________________________________________________________________
StSmdstMaker::StSmdstMaker(const char *name, const char *title):
   StMaker(name,title) {
//
// StSmdstMaker() constructor requires a name and title.
//
   draw_histos=kFALSE,
   counter = 0;
   update = 1;

   mMasspi2 = pion_plus_mass_c2 * pion_plus_mass_c2;
   mMasspr2 = proton_mass_c2 * proton_mass_c2;
   mMassla2 = lambda_mass_c2 * lambda_mass_c2;
   mMassk02 = kaon_0_short_mass_c2 * kaon_0_short_mass_c2;

}
//_____________________________________________________________________________
StSmdstMaker::~StSmdstMaker() {
}
//_____________________________________________________________________________
Int_t StSmdstMaker::Init() {
//
// Init() must create the smdst_v0cut table and then, if DoHistograms()
// has been called, book diagnostic histograms and define a canvas on
// which to display them.
//

   m_max_dca = 100.;
   m_max_bv0 = 100.;
   m_min_dv0 = 0.0;
   m_v0_maxlen = 20000;
   m_v0 = new smdst_v0_st();

// Create Histograms    
   if (draw_histos) {
      m_pt_alpha =
        new TH2F("pt_alpha","Armenteros Plot: Pt vs alpha",50,-1.25,1.25,60,0.,0.3);
      m_pt_alpha_real =
        new TH2F("pt_alpha_real","Pt vs alpha",50,-1.25,1.25,60,0.,0.3);
      m_pt_alpha_anti =
        new TH2F("pt_alpha_anti","Pt vs alpha",50,-1.25,1.25,60,0.,0.3);
      m_pt_alpha_eith =
        new TH2F("pt_alpha_eith","Pt vs alpha",50,-1.25,1.25,60,0.,0.3);
      m_pt_alpha->SetXTitle("alpha");
      m_pt_alpha->SetYTitle("Pt");
      m_pt_alpha->SetMarkerColor(0);
//      m_pt_alpha->SetMarkerColor(33);
      m_pt_alpha_real->SetMarkerColor(4);
      m_pt_alpha_real->SetMarkerStyle(7);
      m_pt_alpha_anti->SetMarkerColor(2);
      m_pt_alpha_anti->SetMarkerStyle(7);
      m_pt_alpha_eith->SetMarkerColor(6);
      m_pt_alpha_eith->SetMarkerStyle(7);
      m_k0_mass = new TH1F("k0_mass"," K0 Mass ",100,0.4,0.6);
      m_k0_mass->SetXTitle("Mass (GeV/c^2!)");
      m_k0_mass->SetFillColor(38);
      m_la_mass = new TH1F("la_mass"," Lambda Mass ",100,1.05,1.25);
      m_la_mass->SetXTitle("Mass (GeV/c^2!)");
      m_la_mass->SetFillColor(38);
      m_xi_mass = new TH1F("xi_mass"," Xi Mass ",50,1.1,1.5);
      m_xi_mass->SetXTitle("Mass (GeV/c^2!)");
      m_xi_mass->SetFillColor(38);
      m_om_mass = new TH1F("om_mass"," Omega Mass ",50,1.5,1.9);
      m_om_mass->SetXTitle("Mass (GeV/c^2!)");
      m_om_mass->SetFillColor(38);

      if (!StSmdstMaker::m_str1) {
         m_str1 =
            new TCanvas("str1","Strangeness V0 Histograms",20,10,500,660);
         m_str1->SetFillColor(46);
      }
      m_pad1 = new TPad("Pad1","Pad with armenteros",0.02,0.52,0.98,0.98,21);
      m_pad2 = new TPad("Pad2","Pad with Invariant k0 mass",0.02,0.02,0.49,0.50,21);
      m_pad3 = new TPad("Pad3","Pad with Invariant lambda mass",0.51,0.02,0.98,0.50,21);
      m_pad1->SetFillColor(10);
      m_pad2->SetFillColor(10);
      m_pad3->SetFillColor(10);
      m_pad1->Draw();
      m_pad2->Draw();
      m_pad3->Draw();

      if (!StSmdstMaker::m_str2) {
         m_str2 =
            new TCanvas("str2","Strangeness Xi Histograms",30,665,480,185);
         m_str2->SetFillColor(46);
      }
      m_pad4 = new TPad("Pad4","Pad with Invariant xi mass",0.02,0.02,0.48,0.98,21);
      m_pad5 = new TPad("Pad5","Pad with Invariant omega mass",0.52,0.02,0.98,0.98,21);
      m_pad4->SetFillColor(10);
      m_pad5->SetFillColor(10);
      m_pad4->Draw();
      m_pad5->Draw();

// Make Legend
      if (!m_legend1) {
         m_legend1 = new TPaveText(0.05,0.235,1.5,0.325,"br");
      }
      m_legend1->AddText("Legend ");
      m_legend1->AddText("Black Boxes  = all vertices ");
      m_legend1->AddText("Blue (Real)    = bn>2.0,bp>0.5,dv0>5");
      m_legend1->AddText("Red  (Anti)    = bn>0.5,bp>2.0,dv0>5");
      m_legend1->AddText("Pink (Either) = bn>2.0,bp>2.0,dv0>5");
      m_legend1->SetAllWith(" ","align",12);
      m_legend1->SetAllWith("=","font",20);
      m_legend1->SetAllWith("Legend","size",0.04);
      if (!m_legend2) {
         m_legend2 = new TPaveText(0.65,0.83,0.98,0.92,"option NDC");
      }
      m_legend2->AddText("From Blue and");
      m_legend2->AddText("Pink Above");
   }
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSmdstMaker::Make() {
//
// Make() calls FillV0Table to fill the output table. If DoHistograms()
// has been called, FillHistograms() is run for histograms.
//
   Int_t result = FillV0Table();
   if (!result && draw_histos) FillV0Histograms();
   FillXiHistograms();
   return result;
}
//_____________________________________________________________________________
Int_t StSmdstMaker::FillV0Table() {
//
// FillV0Table() creates output table, gets pointers to input tables, and
// then loops over all vertices to fill the output table.
//
   if (!m_DataSet->GetList())  {//if DataSet is empty fill it

   Long_t out=-1;
   Long_t nflag, pflag;
   Float_t ptotp2, ptotn2;
   Float_t eprp, eprn, epip, epin;
   Float_t pdotn, pt2, ptot2, ptot;
   Float_t mala, malb, mak0, ela, ek0;
   Float_t ppp, ppn, dv0, bf[3], x[3]={0.,0.,0.};
   Float_t dx, dy, dz;
   Float_t primX, primY, primZ;
  
   StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
   StEvent *ev = evMaker->event();
   if (!ev) return kStOK;
   StVertexIterator vertices = ev->vertexCollection()->begin();
   if ((*vertices)==0) {
     printf("StSmdstMaker: Warning - no vertices in event.\n");
     return kStOK;
   }

   St_smdst_v0 *smdst_v0  = new St_smdst_v0("smdst_v0",m_v0_maxlen);
   m_DataSet->Add(smdst_v0);

   agufld_(x,bf);
   bf[2] = bf[2]*0.1;
   if( bf[2] == 0. )
     printf("StSmdstMaker: Warning - magnetic field not loaded.\n");

   StVertex *primaryVertex = ev->primaryVertex();
   if (primaryVertex) {
     primaryVertex = *vertices;
     const StThreeVector<float>& primaryPos = primaryVertex->position();
     primX = primaryPos.x();
     primY = primaryPos.y();
     primZ = primaryPos.z();
   } else {
     printf("StSmdstMaker: Warning - no primary vertex, setting to (0,0,0).\n");
     primX = 0.;
     primY = 0.;
     primZ = 0.;
   }

   StGlobalTrack* nTrack;
   StGlobalTrack* pTrack;
   StTrackFitTraits* nFitTraits;
   StTrackFitTraits* pFitTraits;
   for (vertices  = ev->vertexCollection()->begin();
        vertices != ev->vertexCollection()->end(); vertices++) {
     if ( (*vertices)->type() != V0) continue;
     StV0Vertex *vertex = (StV0Vertex *) *vertices;

     StVecPtrGlobalTrack& vTracks = vertex->daughters();
     if( vTracks.size() > 1 ) {
       nTrack = vTracks[negativeTrack];
       pTrack = vTracks[positiveTrack];
       nFitTraits = &nTrack->fitTraits();
       pFitTraits = &pTrack->fitTraits();
       nflag = nFitTraits->qualityBitmap();
       pflag = pFitTraits->qualityBitmap();
     } else {
       nflag = 1;
       pflag = 1;
     }

     const StThreeVector<float>& pos = vertex->position();
     dx = pos.x() - primX;
     dy = pos.y() - primY;
     dz = pos.z() - primZ;
     dv0 = TMath::Sqrt( dx*dx + dy*dy + dz*dz );

     if (( vertex->dcaDaughters() <= m_max_dca ) &&
         ( vertex->dcaParentToPrimaryVertex() <= m_max_bv0 ) &&
         ( dv0 >= m_min_dv0 ) && (dv0 < 5000) &&
         ( pflag!=0 && ( pflag<200 | pflag>=300 )) &&
         ( nflag!=0 && ( nflag<200 | nflag>=300 ))) {

      
       if (++out == m_v0_maxlen) {
         printf("StSmdstMaker: Error - v0 table maxlen exceeded.\n");
         return kStErr;
       }

       const StThreeVector<float>& nMom = vertex->momentumOfDaughter(negativeTrack);
       const StThreeVector<float>& pMom = vertex->momentumOfDaughter(positiveTrack);
      
       ptotn2 = nMom.mag2();
       ptotp2 = pMom.mag2();
       pdotn  = nMom.dot(pMom);

       eprp = TMath::Sqrt(mMasspr2 + ptotp2);
       eprn = TMath::Sqrt(mMasspr2 + ptotn2);
       epip = TMath::Sqrt(mMasspi2 + ptotp2);
       epin = TMath::Sqrt(mMasspi2 + ptotn2);
       
       mala = TMath::Sqrt( mMasspr2 + mMasspi2 + 2*(eprp*epin - pdotn) );
       malb = TMath::Sqrt( mMasspr2 + mMasspi2 + 2*(epip*eprn - pdotn) );
       mak0 = TMath::Sqrt( mMasspi2 + mMasspi2 + 2*(epip*epin - pdotn) );

       StThreeVector<float> vMom = pMom + nMom;

       pt2   = vMom.perp2();
       ptot2 = vMom.mag2();
       ptot  = TMath::Sqrt(ptot2);

       ppp = ( ptotp2 + pdotn )/ptot;
       ppn = ( ptotn2 + pdotn )/ptot;

       ela = TMath::Sqrt( mMassla2 + ptot2 );
       ek0 = TMath::Sqrt( mMassk02 + ptot2 );

//       m_v0->id        = v0_vertex[in].id;
       m_v0->id        = 0;
       m_v0->run       = ev->runNumber();
       m_v0->event[0]  = (ev->id()).first;
       m_v0->event[1]  = (ev->id()).second;
//       m_v0->id_vertex = v0_vertex[in].id_vertex;
       m_v0->id_vertex = 0;
//       m_v0->pidp      = track[pkey].id_global_pid;
//       m_v0->pidn      = track[nkey].id_global_pid;
       m_v0->pidp      = 0;
       m_v0->pidn      = 0;
       m_v0->nflag     = nflag;
       m_v0->pflag     = pflag;
       if (vTracks.size() > 1) {
         m_v0->npn       = nFitTraits->numberOfFitPoints();
         m_v0->npp       = pFitTraits->numberOfFitPoints();
       } else {
         m_v0->npp       = 0;
         m_v0->npp       = 0;
       }
       m_v0->alpha     = (ppp - ppn)/(ppp + ppn);
       m_v0->ptarm     = TMath::Sqrt(fabs(ptotp2 - ppp*ppp));
       m_v0->bn        = vertex->dcaDaughterToPrimaryVertex(negativeTrack);
       m_v0->bp        = vertex->dcaDaughterToPrimaryVertex(positiveTrack);
       m_v0->dca       = vertex->dcaDaughters();
       m_v0->bv0       = vertex->dcaParentToPrimaryVertex();
       m_v0->mala      = mala;
       m_v0->malb      = malb;
       m_v0->mak0      = mak0;
       m_v0->pxn       = nMom.x();
       m_v0->pyn       = nMom.y();
       m_v0->pzn       = nMom.z();
       m_v0->pxp       = pMom.x();
       m_v0->pyp       = pMom.y();
       m_v0->pzp       = pMom.z();
       m_v0->px        = vMom.x();
       m_v0->py        = vMom.y();
       m_v0->pz        = vMom.z();
       m_v0->x         = pos.x();
       m_v0->y         = pos.y();
       m_v0->z         = pos.z();
       m_v0->dv0       = dv0;
//       m_v0->sagp      = track[pkey].length*track[pkey].length * 1.25 * 
//         C_D_CURVATURE*bf[2]*track[pkey].invpt;
//       m_v0->sagn      = track[nkey].length*track[nkey].length * 1.25 * 
//         C_D_CURVATURE*bf[2]*track[nkey].invpt;
       m_v0->sagn      = 0;
       m_v0->sagp      = 0;
       m_v0->rapla     = TMath::Log((ela+vMom.z())/TMath::Sqrt(mMassla2+pt2));
       m_v0->raplb     = m_v0->rapla;
       m_v0->rapk0     = TMath::Log((ek0+vMom.z())/TMath::Sqrt(mMassk02+pt2));
       m_v0->theta     = nMom.angle(pMom) * C_DEG_PER_RAD;

       smdst_v0->AddAt(m_v0,out);
     }
   }
  
  }
  
  return kStOK;
}
//_____________________________________________________________________________
void StSmdstMaker::FillV0Histograms() {
//
// FillV0Histograms() fills diagnostic V0 histograms for the Maker.
// The histograms are only redrawn once for every cycle of update events.
/* Begin_Html <center>
<img src="gif/smdst_histo.gif">
</center> End_Html */
//
  if (m_DataSet && update) {
    Int_t i,n;
    St_DataSetIter table(m_DataSet);
    St_smdst_v0 *smdst_v0 = (St_smdst_v0 *) table("smdst_v0");
    if (smdst_v0) {
      smdst_v0_st *v0table = smdst_v0->GetTable();
      n = smdst_v0->GetNRows();
      for (i=0; i<n; i++) {
        if (v0table[i].dv0>5. && v0table[i].bn>2.0 && v0table[i].bp>2.0) {
          m_pt_alpha_eith->Fill(v0table[i].alpha,v0table[i].ptarm);
          m_k0_mass->Fill(v0table[i].mak0);
          m_la_mass->Fill(v0table[i].mala);
        }
        else if (v0table[i].dv0>5. && v0table[i].bn>2.0 && v0table[i].bp>0.5) {
          m_pt_alpha_real->Fill(v0table[i].alpha,v0table[i].ptarm);
          m_k0_mass->Fill(v0table[i].mak0);
          m_la_mass->Fill(v0table[i].mala);
  	}
        else if (v0table[i].dv0>5. && v0table[i].bn>0.5 && v0table[i].bp>2.0) {
          m_pt_alpha_anti->Fill(v0table[i].alpha,v0table[i].ptarm);
	}
        m_pt_alpha->Fill(v0table[i].alpha,v0table[i].ptarm);
      }
    }
  }
  if (!update || (m_DataSet && (++counter%update) == 0)) {
    m_pad1->cd();
    gStyle->SetOptStat(10);
    m_pt_alpha->Draw();
//    m_pt_alpha->Draw("box");
    m_pt_alpha_real->Draw("same");
    m_pt_alpha_anti->Draw("same");
    m_pt_alpha_eith->Draw("same");
    m_legend1->Draw();
    m_pad2->cd();
    m_k0_mass->Draw();
    m_legend2->Draw();
    m_pad3->cd();
    m_la_mass->Draw();
    m_legend2->Draw();
    m_str1->Update();
    m_str1->cd();
  }
}
//_____________________________________________________________________________
void StSmdstMaker::FillXiHistograms() {
//
// FillXiHistograms() fills diagnostic Xi histograms for the Maker.
// The histograms are only redrawn once for every cycle of update events.
//
  Float_t ptotb2, ptotv2, bdotv;
  Float_t ek0b, epib, elav;
  Float_t maxi, maom;

  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  StEvent *ev = evMaker->event();
  if (!ev) return;
  StVertexIterator vertices = ev->vertexCollection()->begin();
  if ((*vertices)==0) {
    printf("StSmdstMaker: Warning - no vertices in event.\n");
    return;
  }

  if (m_DataSet && update) {
   for (vertices  = ev->vertexCollection()->begin();
        vertices != ev->vertexCollection()->end(); vertices++) {
     if ( (*vertices)->type() != Xi) continue;
     StXiVertex *vertex = (StXiVertex *) *vertices;

     StGlobalTrack* bTrack = vertex->bachelor();

     const StThreeVector<float>& bMom = vertex->momentumOfBachelor();
     const StThreeVector<float>& vMom = vertex->momentumOfV0();
      
     ptotb2 = bMom.mag2();
     ptotv2 = vMom.mag2();
     bdotv = vMom.dot(bMom);

     ek0b = TMath::Sqrt(mMassk02 + ptotb2);
     epib = TMath::Sqrt(mMasspi2 + ptotb2);
     elav = TMath::Sqrt(mMassla2 + ptotv2);
       
     maxi = TMath::Sqrt( mMassla2 + mMasspi2 + 2*(elav*epib - bdotv) );
     maom = TMath::Sqrt( mMassla2 + mMassk02 + 2*(elav*ek0b - bdotv) );

     m_xi_mass->Fill(maxi);
     m_om_mass->Fill(maom);

   }
  }
  if (!update || (m_DataSet && (counter%update) == 0)) {
    m_str2->cd();
    m_pad4->cd();
    m_xi_mass->Draw();
    m_pad5->cd();
    m_om_mass->Draw();
    m_str2->Update();
    m_str2->cd();
  }
}
//_____________________________________________________________________________
Int_t StSmdstMaker::Finish() {
//
// Finish() makes sure all histograms are up-to-date.
//
  if (draw_histos) {
    update = 0;
    FillV0Histograms();
    FillXiHistograms();
  }
  delete m_v0;
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StSmdstMaker::PrintInfo() {
//
// PrintInfo() prints information about the class to standard output.
//
  printf("**************************************************************\n");
  printf("* $Id: StSmdstMaker.cxx,v 1.1 1999/04/14 15:10:35 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  if (draw_histos) printf("* Strangeness Histograms are active\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
