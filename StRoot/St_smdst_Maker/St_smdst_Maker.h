// $Id: St_smdst_Maker.h,v 1.1 1999/01/18 22:59:48 fisyak Exp $
// $Log: St_smdst_Maker.h,v $
// Revision 1.1  1999/01/18 22:59:48  fisyak
// Add Gene Van Buren smdst
//
//
// Revision 1.1  1999/01/18 12:46:19 genevb
// Strangeness microdst analysis
//
#ifndef STAR_St_smdst_Maker
#define STAR_St_smdst_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_smdst_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#include "St_smdst_v0cut_Table.h"
#endif

class St_smdst_v0cut;
class TH1F;
class TH2F;
class TCanvas;
class TPad;
class TPaveText;

class St_smdst_Maker : public StMaker {
 private:
   Bool_t draw_histos;                 // Histogram on/off toggle
   Int_t update;                       // Frequency of histogram updates
   Int_t counter;                      // Event counter
 protected:
   St_smdst_v0cut *m_smdst_v0cut;      //!
   TH2F *m_pt_alpha;                   //!
   TH2F *m_pt_alpha_real;              //!
   TH2F *m_pt_alpha_anti;              //!
   TH2F *m_pt_alpha_eith;              //!
   TH1F *m_k0_mass;                    //!
   TH1F *m_la_mass;                    //!
   TCanvas *m_str1;                    //!
   TPad *m_pad1;                       //!
   TPad *m_pad2;                       //!
   TPad *m_pad3;                       //!
   TPaveText *m_legend1;               //!
   TPaveText *m_legend2;               //!
   virtual void   MakeHistograms();    // Fill diagnostic histograms
 public:
                  St_smdst_Maker(const char *name="smdst", const char *title="event/data/strange/smdst");
   virtual        ~St_smdst_Maker();
   virtual void   DoHistograms(Int_t n) { draw_histos = kTRUE; update = n; } // Turn on diagnostic histograms
   virtual void   DoHistograms() { DoHistograms(update); } // Turn on diagnostic histograms
   virtual Int_t  Finish();            // Finalize analysis
   virtual Int_t  Init();              // Initialize analysis
   virtual Int_t  Make();              // Run analysis
   virtual void   PrintInfo();         // Print class information
   ClassDef(St_smdst_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif

