// $Id: St_QA_Maker.h,v 1.5 1999/02/25 19:25:39 kathy Exp $
// $Log: St_QA_Maker.h,v $
// Revision 1.5  1999/02/25 19:25:39  kathy
// fix up histograms
//
// Revision 1.4  1999/02/24 21:15:04  kathy
// fixed histograms and added a few new ones
//
// Revision 1.3  1999/02/23 22:22:22  kathy
// changes to histograms: titles changed so they'll be in order and redundant ones removed
//
// Revision 1.2  1999/02/22 21:27:18  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.1  1999/02/08 19:28:33  didenko
// fixed directory level
//
// Revision 1.3  1999/01/22 22:21:07  didenko
// header file for  QA maker
//
// Revision 1.2  1998/12/21 19:43:19  fisyak
// Move ROOT includes to non system
//
// Revision 1.1  1998/11/01 16:42:26  fisyak
// dst analysis
//
// Revision 1.4  1998/10/31 00:26:13  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.2  1998/09/08 22:43:11  fisyak
// Modify St_QA_Maker to account new calling sequence
//
// Revision 1.1  1998/08/18 14:06:07  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_QA_Maker
#define STAR_St_QA_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_QA_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#ifndef ROOT_TH1
#include "TH1.h"
#endif
#ifndef ROOT_TH2
#include "TH2.h"
#endif

class St_QA_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t m_VersionCVS = "$Id: St_QA_Maker.h,v 1.5 1999/02/25 19:25:39 kathy Exp $";
   // Histograms booking constants
   static const Int_t nxpT;
   static const Int_t nyeta;
   static const Float_t xminpT;
   static const Float_t xmaxpT;
   static const Float_t ymineta;
   static const Float_t ymaxeta;

     static const Int_t nchisq;
     static const Int_t nmass;
     static const Int_t ntau; 
     static const Int_t ndedx;  
     static const Int_t npnt;   
     static const Int_t nleng;  
     static const Int_t npsi;   
     static const Int_t knpsi;  
     static const Int_t ntrk;   
     static const Int_t nvrt;   
     static const Int_t nmnpt;  
     static const Int_t nmneta; 
     static const Int_t nxyz;   
     static const Int_t knyeta; 
     static const Int_t knid;   
     static const Int_t cnp; 
     static const Int_t cndedx; 

     static const Float_t kminnid;  
     static const Float_t kmaxnid; 
     static const Float_t minpsi;   
     static const Float_t kminpsi;  
     static const Float_t maxpsi;   
     static const Float_t minchisq; 
     static const Float_t maxchisq; 
     static const Float_t minmass;  
     static const Float_t maxmass;  
     static const Float_t mindedx;  
     static const Float_t maxdedx;  
     static const Float_t minpnt;   
     static const Float_t maxpnt;   
     static const Float_t minleng;  
     static const Float_t maxleng;  
     static const Float_t mintau;   
     static const Float_t maxtau;   
     static const Float_t mintrk;   
     static const Float_t maxtrk;   
     static const Float_t minvrt;   
     static const Float_t maxvrt;   
     static const Float_t minmpt;   
     static const Float_t maxmpt;   
     static const Float_t minmeta;  
     static const Float_t maxmeta;  
     static const Float_t kmineta;  
     static const Float_t kmaxeta;  
     static const Float_t minxyz;   
     static const Float_t maxxyz;   
     static const Float_t cminp; 
     static const Float_t cmaxp; 
     static const Float_t cmindedx; 
     static const Float_t cmaxdedx; 


 protected:

// for method MakeEvSum - from table event_summary
   TH2F     *m_trk_tot_gd;    // number of good global tracks versus total
   TH1F     *m_glb_trk_tot;   // # tracks total from globtrk
   TH1F     *m_glb_trk_gd;    // # tracks good from globtrk
   TH1F     *m_glb_trk_plus;  // number of (+) global tracks    
   TH1F     *m_glb_trk_minus; // number of (-) global tracks
   TH2F     *m_trk_pls_mns;   // number of (+) global tracks versus (-) 
   TH1F     *m_vert_total;    // total number of vertices
   TH1F     *m_vert_V0;       // number of V0 vertices
   TH1F     *m_vert_La;       // number of La vertices 
   TH1F     *m_vert_Ala;      // number of Ala vertices
   TH1F     *m_vert_K0;       // number of K0 vertices
   TH1F     *m_mean_pt;       // mean pt value
   TH1F     *m_mean_eta;      // mean eta value 
   TH1F     *m_prim_vrtx0;    // primary vrtx x position
   TH1F     *m_prim_vrtx1;    // primary vrtx y position
   TH1F     *m_prim_vrtx2;    // primary vrtx z position
   TH1F     *m_vrtx_chisq;    // primary vrtx chisq

// for method MakeGlob - from table globtrk
   TH1F     *m_pT;            // pT  recostructed
   TH1F     *m_eta;           // eta recostructed
   TH2F     *m_pT_eta_rec;    // pT versus eta Spectra for reconstructed
   TH1F     *m_point;         // number of points on the track
   TH1F     *m_fit_point;     // number of track points used for fitting
   TH1F     *m_length;        // length of track
   TH1F     *m_chisq0;        // chi square [0]
   TH1F     *m_chisq1;        // chi square [1]
   TH1F     *m_psi;           // psi reconstructed
   TH1F     *m_det_id;        // detector id of track 

// for method MakeDE - from table dst_dedx
   TH1F     *m_ndedx;         // number of point to find dE/dx
   TH1F     *m_dedx0;         // dE/dx [0]
   TH1F     *m_dedx1;         // dE/dx [1] 

// for method MakeHistPrim - from table primtrk
   TH1F     *m_prim_pT;          //! pT  recostructed
   TH1F     *m_prim_eta;         //! eta recostructed
   TH2F     *m_prim_pT_eta_rec;  //! pT versus eta Spectra for reconstructed
   TH1F     *m_prim_tlength;     //! dst track length
   TH1F     *m_prim_chi2xd;      //! x chisq/degf
   TH1F     *m_prim_chi2yd;      //! y chisq/degf
   TH1F     *m_prim_point;       //! # points on track
   TH1F     *m_prim_fit_point;   //! # fitted points
   TH1F     *m_prim_psi;         //! psi angle
   TH1F     *m_prim_det_id;      //! detector id


// for method MakeHistGen - from table particle
   TH2F     *m_H_pT_eta_gen;  //! pT versus eta Spectra for generated


// for MakeHistV0 - from table dst_v0_vertex
   TH1F     *m_ev0_lama_hist;//! Lambda mass
   TH1F     *m_ev0_k0ma_hist;//! K0 mass

// for MakeHistPID - from tables primtrk & dst_dedx 
   TH2F     *m_p_dedx_rec;   //! dedx vs p



//------------------------------------------------------------------------

 public: 
                  St_QA_Maker(const char *name="QA", const char *title="evet/QA");
   virtual       ~St_QA_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   MakeHistEvSum();
   virtual void   MakeHistGlob();
   virtual void   MakeHistDE();
   virtual void   MakeHistPrim();
   virtual void   MakeHistGen();
   virtual void   MakeHistV0();
   virtual void   MakeHistPID();
   virtual void   BookHistEvSum();
   virtual void   BookHistGlob();
   virtual void   BookHistDE();
   virtual void   BookHistPrim();
   virtual void   BookHistGen();
   virtual void   BookHistV0();
   virtual void   BookHistPID();
   virtual void   PrintInfo();
   ClassDef(St_QA_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
