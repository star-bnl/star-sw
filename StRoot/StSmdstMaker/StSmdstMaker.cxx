// $Id: StSmdstMaker.cxx,v 1.17 2000/01/04 22:08:41 fisyak Exp $
// $Log: StSmdstMaker.cxx,v $
// Revision 1.17  2000/01/04 22:08:41  fisyak
// replace agufld by gufld
//
// Revision 1.16  1999/12/07 23:23:55  genevb
// Fixed linux warnings
//
// Revision 1.15  1999/11/29 21:41:16  fisyak
// Remove StMagF header file from include area
//
// Revision 1.14  1999/11/16 23:01:19  genevb
// Modified for StEvent 2.0
//
// Revision 1.13  1999/09/24 01:23:23  fisyak
// Reduced Include Path
//
// Revision 1.12  1999/09/17 16:25:54  genevb
// handle missing events in Xi section
//
// Revision 1.11  1999/07/15 13:57:24  perev
// cleanup
//
// Revision 1.10  1999/07/08 23:03:58  genevb
// Now using StMessMgr, a couple small changes
//
// Revision 1.9  1999/06/28 00:07:57  fisyak
// Fix typo with size()
//
// Revision 1.8  1999/06/27 22:45:30  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.6  1999/06/22 19:06:15  genevb
// Fixed no vertex collection error for cascades too
//
// Revision 1.5  1999/06/22 15:16:50  genevb
// Typo fix
//
// Revision 1.4  1999/06/22 15:11:50  genevb
// Added hook for no vertex collection warning
//
// Revision 1.3  1999/04/14 22:05:06  genevb
// Comply with momentumOfV0 call
//
// Revision 1.2  1999/04/14 15:23:30  genevb
// Fixed file includes
//
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

#include "TROOT.h"

#include "StSmdstMaker.h"
#include "St_DataSetIter.h"
#include <TH2.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <TStyle.h>
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StTrack.h"
#include "math_constants.h"
#include "PhysicalConstants.h"
#include <math.h>
#include "StMessMgr.h"
#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}

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
   gMessMgr->Info("StSmdstMaker - now working.");
   Int_t result = FillV0Table();
   if (!result && draw_histos) {
     FillV0Histograms();
     FillXiHistograms();
   }
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
  
   StEvent* ev = (StEvent *) GetInputDS("StEvent");
   if (!ev) return kStOK; // If no event, we're done
   
   StSPtrVecV0Vertex& v0Vertices = ev->v0Vertices();
   size_t v0tot = v0Vertices.size();
   if (!v0tot) {
     gMessMgr->Warning("StSmdstMaker - no vertices in event.");
     return kStOK;
   }

   St_smdst_v0 *smdst_v0  = new St_smdst_v0("smdst_v0",m_v0_maxlen);
   m_DataSet->Add(smdst_v0);

   gufld_(x,bf);
   bf[2] = bf[2]*0.1;
   if( bf[2] == 0. )
     gMessMgr->Warning("StSmdstMaker - magnetic field not loaded.");

   StPrimaryVertex *primaryVertex = ev->primaryVertex();
   if (primaryVertex) {
     const StThreeVectorF& primaryPos = primaryVertex->position();
     primX = primaryPos.x();
     primY = primaryPos.y();
     primZ = primaryPos.z();
   } else {
     gMessMgr->Warning("StSmdstMaker - no primary vertex, setting to (0,0,0).");
     primX = 0.;
     primY = 0.;
     primZ = 0.;
   }

   StTrack* nTrack;
   StTrack* pTrack;
   for (size_t i=0; i<v0tot; i++) {
     StV0Vertex *vertex = v0Vertices[i];

     nTrack = vertex->daughter(negative);
     pTrack = vertex->daughter(positive);
     if (nTrack) {
       nflag = nTrack->flag();
     } else nflag = 1;
     if (pTrack) {
       pflag = pTrack->flag();
     } else pflag = 1;

     const StThreeVectorF& pos = vertex->position();
     dx = pos.x() - primX;
     dy = pos.y() - primY;
     dz = pos.z() - primZ;
     dv0 = sqrt( dx*dx + dy*dy + dz*dz );

     if (( vertex->dcaDaughters() <= m_max_dca ) &&
         ( vertex->dcaParentToPrimaryVertex() <= m_max_bv0 ) &&
         ( dv0 >= m_min_dv0 ) && (dv0 < 5000) &&
         ( pflag!=0 && ( pflag<200 | pflag>=300 )) &&
         ( nflag!=0 && ( nflag<200 | nflag>=300 ))) {

      
       if (++out == m_v0_maxlen) {
         gMessMgr->Error("StSmdstMaker - v0 table maxlen exceeded.");
         return kStErr;
       }

       const StThreeVectorF& nMom = vertex->momentumOfDaughter(negative);
       const StThreeVectorF& pMom = vertex->momentumOfDaughter(positive);
      
       ptotn2 = nMom.mag2();
       ptotp2 = pMom.mag2();
       pdotn  = nMom.dot(pMom);

       eprp = sqrt(mMasspr2 + ptotp2);
       eprn = sqrt(mMasspr2 + ptotn2);
       epip = sqrt(mMasspi2 + ptotp2);
       epin = sqrt(mMasspi2 + ptotn2);
       
       mala = sqrt( mMasspr2 + mMasspi2 + 2*(eprp*epin - pdotn) );
       malb = sqrt( mMasspr2 + mMasspi2 + 2*(epip*eprn - pdotn) );
       mak0 = sqrt( mMasspi2 + mMasspi2 + 2*(epip*epin - pdotn) );

       StThreeVectorF vMom = pMom + nMom;

       pt2   = vMom.perp2();
       ptot2 = vMom.mag2();
       ptot  = sqrt(ptot2);

       ppp = ( ptotp2 + pdotn )/ptot;
       ppn = ( ptotn2 + pdotn )/ptot;

       ela = sqrt( mMassla2 + ptot2 );
       ek0 = sqrt( mMassk02 + ptot2 );

//       m_v0->id        = v0_vertex[in].id;
       m_v0->id        = 0;
       m_v0->run       = ev->runId();
//       m_v0->event[0]  = (ev->id()).first;
//       m_v0->event[1]  = (ev->id()).second;
       m_v0->event[0]  = ev->id();
       m_v0->event[1]  = 0;
//       m_v0->id_vertex = v0_vertex[in].id_vertex;
       m_v0->id_vertex = 0;
//       m_v0->pidp      = track[pkey].id_global_pid;
//       m_v0->pidn      = track[nkey].id_global_pid;
       m_v0->pidp      = 0;
       m_v0->pidn      = 0;
       m_v0->nflag     = nflag;
       m_v0->pflag     = pflag;
       if (nTrack) {
         m_v0->npn       = nTrack->fitTraits().numberOfFitPoints();
       } else m_v0->npn       = 0;
       if (pTrack) {
         m_v0->npp       = pTrack->fitTraits().numberOfFitPoints();
       } else m_v0->npp       = 0;
       m_v0->alpha     = (ppp - ppn)/(ppp + ppn);
       m_v0->ptarm     = sqrt(fabs(ptotp2 - ppp*ppp));
       m_v0->bn        = vertex->dcaDaughterToPrimaryVertex(negative);
       m_v0->bp        = vertex->dcaDaughterToPrimaryVertex(positive);
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
       m_v0->rapla     = log((ela+vMom.z())/sqrt(mMassla2+pt2));
       m_v0->raplb     = m_v0->rapla;
       m_v0->rapk0     = log((ek0+vMom.z())/sqrt(mMassk02+pt2));
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

  StEvent* ev = (StEvent *) GetInputDS("StEvent");
  if (!ev) return; // If no event, we're done

  StSPtrVecXiVertex& xiVertices = ev->xiVertices();
  size_t castot = xiVertices.size();
  if (!castot) {
    gMessMgr->Warning("StSmdstMaker - no vertices in event.");
    return;
  }

  if (m_DataSet && update) {
    for (size_t i=0; i<castot; i++) {
      StXiVertex *vertex = xiVertices[i];

      const StThreeVectorF& bMom = vertex->momentumOfBachelor();
      const StThreeVectorF vMom = vertex->momentumOfV0();
      
      ptotb2 = bMom.mag2();
      ptotv2 = vMom.mag2();
      bdotv = vMom.dot(bMom);

      ek0b = sqrt(mMassk02 + ptotb2);
      epib = sqrt(mMasspi2 + ptotb2);
      elav = sqrt(mMassla2 + ptotv2);

      maxi = sqrt( mMassla2 + mMasspi2 + 2*(elav*epib - bdotv) );
      maom = sqrt( mMassla2 + mMassk02 + 2*(elav*ek0b - bdotv) );

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
