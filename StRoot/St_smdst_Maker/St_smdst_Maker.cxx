// $Id: St_smdst_Maker.cxx,v 1.4 1999/04/02 17:57:45 fisyak Exp $
// $Log: St_smdst_Maker.cxx,v $
// Revision 1.4  1999/04/02 17:57:45  fisyak
// Add Gene's gif
//
// Revision 1.3  1999/03/12 22:10:37  perev
// New maker schema
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
// St_smdst_Maker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "St_smdst_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "strange/St_smdst2_am_Module.h"
#include "St_smdst_v0_Table.h"
#include <TH2.h>
#include <TCanvas.h>
#include <TPaveText.h>
#include <TStyle.h>
ClassImp(St_smdst_Maker)

//_____________________________________________________________________________
St_smdst_Maker::St_smdst_Maker(const char *name):
   StMaker(name) {
//
// St_smdst_Maker() constructor requires a name and title.
//
   m_smdst_v0cut = 0;
   draw_histos=kFALSE,
   counter = 0;
   update = 1;
}
//_____________________________________________________________________________
St_smdst_Maker::~St_smdst_Maker() {
}
//_____________________________________________________________________________
Int_t St_smdst_Maker::Init() {
//
// Init() must create the smdst_v0cut table and then, if DoHistograms()
// has been called, book diagnostic histograms and define a canvas on
// which to display them.
//

// Create tables
   if (!St_smdst_Maker::m_smdst_v0cut) {
     m_smdst_v0cut = new St_smdst_v0cut("smdst_v0cut",1);
   }
   smdst_v0cut_st smdst_v0cut;
   smdst_v0cut.max_dca = 100.;
   smdst_v0cut.max_bv0 = 100.;
   smdst_v0cut.min_dv0 = 0.0;
   m_smdst_v0cut->AddAt(&smdst_v0cut,0);

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
      m_pt_alpha->SetMarkerColor(33);
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

      if (!St_smdst_Maker::m_str1) {
         m_str1 =
            new TCanvas("str1","Strangeness Histograms",20,10,500,660);
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
Int_t St_smdst_Maker::Make() {
//
// Make() creates output tables, gets pointers to input tables, and
// then calls the smdst2_am physics analysis module. If DoHistograms()
// has been called, MakeHistograms() is run for diagnostic histograms.
//
   St_smdst_v0 *smdst_v0  = new St_smdst_v0("smdst_v0",10000);m_DataSet->Add(smdst_v0);
   St_smdst_index *vindex = new St_smdst_index("vindex",10000);m_DataSet->Add(vindex);
   St_smdst_index *tindex = new St_smdst_index("tindex",50000);m_DataSet->Add(tindex);

   St_DataSet *dst_set = GetDataSet("dst");
   if (!dst_set) {
     cout << "No dst maker defined" << endl;
     return 1;    
   }
   St_DataSetIter       local(dst_set);
   St_dst_event_header *dst_event_header = (St_dst_event_header *) local("event_header");
   St_dst_track *dst_track         = (St_dst_track     *) local("globtrk");
   St_dst_vertex *dst_vertex       = (St_dst_vertex    *) local("vertex");
   St_dst_v0_vertex *dst_v0_vertex = (St_dst_v0_vertex *) local("dst_v0_vertex");

   Int_t Res_smdst = smdst2_am (
                        dst_event_header,
                        dst_track,
                        dst_vertex,
                        dst_v0_vertex,
                        m_smdst_v0cut,
                        smdst_v0,
                        tindex,
                        vindex);
   if (Res_smdst != kSTAFCV_OK) {
     cout << "***** Problem with smdst2_am *****" << endl;
   }
   if (draw_histos) MakeHistograms();       // Strangeness Histograms
 return kStOK;
}
//_____________________________________________________________________________
void St_smdst_Maker::MakeHistograms() {
//
// MakeHistograms() fills diagnostic histograms for the Maker.
// The histograms are only redrawn once for every cycle of update events.
/* Begin_Html
<P ALIGN=CENTER> <img src="gif/smdst_histo.gif">  </P> 
End_Html */
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
    m_str1->cd();
    m_pad1->cd();
    gStyle->SetOptStat(10);
    m_pt_alpha->Draw("box");
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
    m_str1->cd();
    m_str1->Update();
  }
}
//_____________________________________________________________________________
Int_t St_smdst_Maker::Finish() {
//
// Finish() makes sure all histograms are up-to-date.
//
  if (draw_histos) {
    update = 0;
    MakeHistograms();
  }
  return StMaker::Finish();
}
//_____________________________________________________________________________
void St_smdst_Maker::PrintInfo() {
//
// PrintInfo() prints information about the class to standard output.
//
  printf("**************************************************************\n");
  printf("* $Id: St_smdst_Maker.cxx,v 1.4 1999/04/02 17:57:45 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  if (draw_histos) printf("* Strangeness Histograms are active\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
