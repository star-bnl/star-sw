// $Id: StSmdstMaker.h,v 1.2 1999/07/15 13:57:25 perev Exp $
// $Log: StSmdstMaker.h,v $
// Revision 1.2  1999/07/15 13:57:25  perev
// cleanup
//
// Revision 1.1  1999/04/14 15:10:36  genevb
// Add StSmdstMaker source files
//
// Revision 1.2  1999/01/19 22:42:32  genevb
// update comments
//
//
// Revision 1.1  1999/01/18 12:46:19 genevb
// Strangeness microdst analysis
//
#ifndef STAR_StSmdstMaker
#define STAR_StSmdstMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSmdstMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "St_smdst_v0_Table.h"

class St_smdst_v0cut;
class TH1F;
class TH2F;
class TCanvas;
class TPad;
class TPaveText;

class StSmdstMaker : public StMaker {
 private:
   Bool_t draw_histos;                 // Histogram on/off toggle
   Int_t update;                       // Frequency of histogram updates
   Int_t counter;                      // Event counter

   Double_t mMasspi2;
   Double_t mMasspr2;
   Double_t mMassla2;
   Double_t mMassk02;

 protected:
   Float_t m_max_dca;
   Float_t m_max_bv0;
   Float_t m_min_dv0;
   Long_t m_v0_maxlen;
   smdst_v0_st *m_v0;                  //!
   TH2F *m_pt_alpha;                   //!
   TH2F *m_pt_alpha_real;              //!
   TH2F *m_pt_alpha_anti;              //!
   TH2F *m_pt_alpha_eith;              //!
   TH1F *m_k0_mass;                    //!
   TH1F *m_la_mass;                    //!
   TH1F *m_xi_mass;                    //!
   TH1F *m_om_mass;                    //!
   TCanvas *m_str1;                    //!
   TCanvas *m_str2;                    //!
   TPad *m_pad1;                       //!
   TPad *m_pad2;                       //!
   TPad *m_pad3;                       //!
   TPad *m_pad4;                       //!
   TPad *m_pad5;                       //!
   TPaveText *m_legend1;               //!
   TPaveText *m_legend2;               //!
   virtual void   FillV0Histograms();  // Fill diagnostic V0 histograms
   virtual void   FillXiHistograms();  // Fill diagnostic Xi histograms
   virtual Int_t  FillV0Table();         // Fill output table

 public:
                  StSmdstMaker(const char *name="smdst", const char *title="event/data/strange/smdst");
   virtual        ~StSmdstMaker();
   virtual void   DoHistograms(Int_t n) { draw_histos = kTRUE; update = n; } // Turn on diagnostic histograms
   virtual void   DoHistograms() { DoHistograms(update); } // Turn on diagnostic histograms
   virtual Int_t  Finish();            // Finalize analysis
   virtual Int_t  Init();              // Initialize analysis
   virtual Int_t  Make();              // Run analysis
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSmdstMaker.h,v 1.2 1999/07/15 13:57:25 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StSmdstMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

