//! $Id: St_QA_Maker.h,v 1.14 1999/04/21 20:19:19 kathy Exp $
//! $Log: St_QA_Maker.h,v $
//! Revision 1.14  1999/04/21 20:19:19  kathy
//! put in comments and cleaned up - works for mdc2 dst in dev now
//!
//! Revision 1.13  1999/04/20 01:16:59  fisyak
//! Add check on. no of tracks in dE/dX
//!
//! Revision 1.12  1999/03/11 21:13:14  kathy
//! update to hist limits
//!
//! Revision 1.11  1999/03/09 16:30:24  fine
//! Workqround of the St_io_Maker bug
//!
//! Revision 1.10  1999/03/07 19:26:16  fine
//! QA->SetPostScriptFile(psFile) has been introduced
//!
//! Revision 1.9  1999/03/07 16:53:33  fine
//! New method DrawHists
//!
//! Revision 1.8  1999/03/05 21:19:38  kathy
//! added new histograms
//!
//! Revision 1.7  1999/03/03 23:34:30  kathy
//! fixes to histograms
//!
//! Revision 1.6  1999/02/26 18:42:34  kathy
//! added vertex histograms
//
//! Revision 1.5  1999/02/25 19:25:39  kathy
//! fix up histograms
//
//! Revision 1.4  1999/02/24 21:15:04  kathy
//! fixed histograms and added a few new ones
//
//! Revision 1.3  1999/02/23 22:22:22  kathy
//! changes to histograms: titles changed so they'll be in order and redundant ones removed
//
//! Revision 1.2  1999/02/22 21:27:18  kathy
//! moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
//! Revision 1.1  1999/02/08 19:28:33  didenko
//! fixed directory level
//
//! Revision 1.3  1999/01/22 22:21:07  didenko
//! header file for  QA maker
//
//! Revision 1.2  1998/12/21 19:43:19  fisyak
//! Move ROOT includes to non system
//
//! Revision 1.1  1998/11/01 16:42:26  fisyak
//! dst analysis
//
//! Revision 1.4  1998/10/31 00:26:13  fisyak
//! Makers take care about branches
//
//! Revision 1.3  1998/10/06 18:00:34  perev
//! cleanup
//
//! Revision 1.2  1998/09/08 22:43:11  fisyak
//! Modify St_QA_Maker to account new calling sequence
//
//! Revision 1.1  1998/08/18 14:06:07  fisyak
//! Add to bfc dst
//
//! Revision 1.3  1998/08/10 02:32:07  fisyak
//! Clean up
//
//! Revision 1.2  1998/07/20 15:08:15  fisyak
//! Add tcl and tpt
//
#ifndef STAR_St_QA_Maker
#define STAR_St_QA_Maker

//////////////////////////////////////////////////////////////////////////
//!                                                                      //
//! St_QA_Maker virtual base class for Maker                            //
//!                                                                      //
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

class TCanvas;

class St_QA_Maker : public StMaker {
 private:
  Bool_t drawinit;
  //! static Char_t m_VersionCVS = "$Id: St_QA_Maker.h,v 1.14 1999/04/21 20:19:19 kathy Exp $";
  //! Histograms booking constants
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
  
  // Data-members to make up the output Canvases and Postscript files
  TCanvas       *m_QACanvas;       //!
  Int_t          m_PadColumns;     // Number of the columns (TPad's) on the single Canvas
  Int_t          m_PadRows;        // Number of the columns (TPad's) on the single Canvas
  
  Int_t          m_PaperWidth;     // Paper size in inch.
  Int_t          m_PaperHeight;    // Paper size in inch.
  
  TString        m_FirstHistName;
  TString        m_LastHistName;
  
  TString        m_PsFileName;     // Name of the PostScipt file to plot hist's out
  
  
 protected:
  
  // for method MakeEvSum - from table event_summary
  TH1F     *m_trk_tot_gd;    //! number of good global tracks divided by total
  TH1F     *m_glb_trk_tot;   //! # tracks total from globtrk
  TH1F     *m_glb_trk_gd;    //! # tracks good from globtrk
  TH1F     *m_glb_trk_plusminus; //! # trks pos/neg. 
  
  TH1F     *m_vert_total;    //! total number of vertices
  TH1F     *m_vert_V0;       //! number of V0 vertices
  /*    TH1F     *m_vert_La;       //! number of La vertices  */
  /*    TH1F     *m_vert_Ala;      //! number of Ala vertices */
  /*    TH1F     *m_vert_K0;       //! number of K0 vertices */
  TH1F     *m_mean_pt;       //! mean pt value
  TH1F     *m_mean_eta;      //! mean eta value 
  TH1F     *m_prim_vrtx0;    //! primary vrtx x position
  TH1F     *m_prim_vrtx1;    //! primary vrtx y position
  TH1F     *m_prim_vrtx2;    //! primary vrtx z position
  TH1F     *m_vrtx_chisq;    //! primary vrtx chisq
  
  // for method MakeGlob - from table globtrk
  TH1F     *m_pT;            //! pT  reconstructed
  TH1F     *m_pT_fr;         //! pT  reconstructed - full range
  TH1F     *m_eta;           //! eta reconstructed
  TH2F     *m_pT_eta_rec;    //! pT versus eta Spectra for reconstructed
  TH2F     *m_mom_trklength; //! mom vs. trk length
  TH1F     *m_point;         //! number of points on the track
  TH1F     *m_fit_point;     //! number of track points used for fitting
  TH1F     *m_length;        //! length of track
  TH1F     *m_chisq0;        //! chi square [0]
  TH1F     *m_chisq1;        //! chi square [1]
  TH1F     *m_psi;           //! psi reconstructed
  TH1F     *m_det_id;        //! detector id of track
  TH2F     *m_npoint_length; //! num points vs length
  TH2F     *m_fpoint_length; //! num fit points vs length
  TH2F     *m_chisq0_mom;    //! chisq0 vs momentum
  TH2F     *m_chisq1_mom;    //! chisq1 vs momentum
  
  
  // for method MakeDE - from table dst_dedx
  TH1F     *m_ndedx;         //! number of point to find dE/dx
  TH1F     *m_dedx0;         //! dE/dx [0]
  TH1F     *m_dedx1;         //! dE/dx [1] 
  
  // for method MakeHistPrim - from table primtrk
  TH1F     *m_prim_pT;          //! pT  recostructed
  TH1F     *m_prim_eta;         //! eta recostructed
  TH2F     *m_prim_pT_eta_rec;  //! pT versus eta Spectra for reconstructed
  TH1F     *m_prim_tlength;     //! dst track length
  TH1F     *m_prim_chi2xd;      //! x chisq/degf
  TH1F     *m_prim_chi2yd;      //! y chisq/degf
  TH1F     *m_prim_point;       //! # points on track
  TH1F     *m_prim_fit_point;   //! # fitted points
  TH1F     *m_prim_psi;         //! psi angle_ 
  TH1F     *m_prim_det_id;        //!
  TH2F     *m_prim_mom_trklength; //!
  TH2F     *m_prim_npoint_length; //!
  TH2F     *m_prim_fpoint_length; //!
  TH2F     *m_prim_chisq0_mom;    //!
  TH2F     *m_prim_chisq1_mom;    //!
  
  
  // for method MakeHistGen - from table particle
  TH2F     *m_H_pT_eta_gen;  //! pT versus eta Spectra for generated
  TH1F     *m_H_pT_gen;  //! pT Spectra for generated
  TH1F     *m_H_eta_gen;  //! eta Spectra for generated
  TH1F     *m_H_vtxx;     //! production vertex (mm)
  TH1F     *m_H_vtxy;     //! production vertex (mm)
  TH1F     *m_H_vtxz;     //! production vertex (mm)
  TH1F     *m_H_npart;    //! total num particles generated
  TH1F     *m_H_ncpart;   //! number of charged e,mu,proton,kaon,pion
  
  // for MakeHistV0 - from table dst_v0_vertex
  TH1F     *m_ev0_lama_hist; //! Lambda mass
  TH1F     *m_ev0_k0ma_hist; //! K0 mass
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  TH2F     *m_p_dedx_rec;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  TH1F     *m_v_detid; //! detector id where vertex was found 
  TH1F     *m_v_vtxid; //! vertex type
  TH1F     *m_v_x;     //! vertex coordinates in
  TH1F     *m_v_y;     //!  STAR reference 
  TH1F     *m_v_z;     //!   system
  TH1F     *m_v_pchi2; //! P(chi^2,ndf) of vertex fit
  
  TH1F     *m_pv_detid; //! row1-detector id where vertex was found 
  TH1F     *m_pv_vtxid; //! row1-vertex type
  TH1F     *m_pv_x;     //! row1-vertex coordinates in
  TH1F     *m_pv_y;     //!  STAR reference 
  TH1F     *m_pv_z;     //!   system
  TH1F     *m_pv_pchi2; //! row1-P(chi^2,ndf) of vertex fit
  
  // for method MakeHistTofEvt
  TH1F *m_te_ntpttrk;   //!no. of tpc tracks in event  
  TH1F *m_te_nttetrk;   //!no. of tte tracks associated with tpt tracks
  TH1F *m_te_ng2ttrk;   //!no. of g2t tracks associated with tpt/tte tracks
  TH1F *m_te_nctfhit;   //!no. hits in g2t_tof_hit
  TH1F *m_te_nexttrk;   //!no. decent tracks extrapolated
  TH1F *m_te_ntoftrk;   //!no. of these that are kept
  TH1F *m_te_ntrks;     //!no. decent tracks in the event
  TH1F *m_te_ntrks_hit; //!no. of these with ctf hit via pointers
  TH1F *m_te_ntrks_kee; //!no. of decent tracks extrapolated
  TH1F *m_te_ntrks_tra; //!no. of these extrapolated to TOFp tray
  TH1F *m_te_ntrks_mat; //!no. of these extrapolated to struck TOFp slat
  
  // for method MakeHistTofTrk
  TH1F *m_tt_strk;   //! measured total length from target to hit
  TH1F *m_tt_phitrk; //! phi of extrapolation of track to tof
  TH1F *m_tt_stof;   //! geant's total length from target to hit 
  TH1F *m_tt_phitof; //! phi of hit in tof from geant 
  TH1F *m_tt_tof;    //! actual time of flight
  TH1F *m_tt_adc;    //! ADC value for this slat
  
  // for method MakeHistEmsHitsBemc
  TH1F *m_ehbe_hits1; //! bemc # hits detector 1
  TH1F *m_ehbe_tnrg1; //! bemc tot energy detector 1
  TH1F *m_ehbe_hits2; //! bemc # hits detector 2
  TH1F *m_ehbe_tnrg2; //! bemc tot energy detector 2
  
  
  // for method MakeHistEmsHitsBsmd
  TH1F *m_ehbs_hits3; //! bemc # hits detector 3
  TH1F *m_ehbs_tnrg3; //! bemc tot energy detector 3
  TH1F *m_ehbs_hits4; //! bemc # hits detector 4
  TH1F *m_ehbs_tnrg4; //! bemc tot energy detector 4 
  
  
  // for method MakeHistXi
  
  
  
  //------------------------------------------------------------------------
  
 public: 
  St_QA_Maker(const char *name="QA", const char *title="evet/QA");
  virtual       ~St_QA_Maker();
  virtual Int_t  DrawHists();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();
  virtual void   MakeHistEvSum(St_DataSet *dst);
  virtual void   MakeHistGlob(St_DataSet *dst);
  virtual void   MakeHistDE(St_DataSet *dst);
  virtual void   MakeHistPrim(St_DataSet *dst);
  virtual void   MakeHistGen(St_DataSet *dst);
  virtual void   MakeHistV0(St_DataSet *dst);
  virtual void   MakeHistPID(St_DataSet *dst);
  virtual void   MakeHistVertex(St_DataSet *dst);
  virtual void   BookHistEvSum();
  virtual void   BookHistGlob();
  virtual void   BookHistDE();
  virtual void   BookHistPrim();
  virtual void   BookHistGen();
  virtual void   BookHistV0();
  virtual void   BookHistPID();
  virtual void   BookHistVertex();
  virtual void   BookHistTofEvt();
  virtual void   BookHistTofTrk();
  virtual void   BookHistEmsHitsBemc();
  virtual void   BookHistEmsHitsBsmd();
  virtual void   BookHistXi();
  virtual void   MakeHistTofEvt(St_DataSet *dst);
  virtual void   MakeHistTofTrk(St_DataSet *dst);
  virtual void   MakeHistEmsHitsBemc(St_DataSet *dst);
  virtual void   MakeHistEmsHitsBsmd(St_DataSet *dst);
  virtual void   MakeHistXi(St_DataSet *dst);
  virtual void   PrintInfo();
  virtual void   SetDraw(Bool_t drawFlag=kTRUE);
  virtual void   SetHistsNames(const Char_t *firstName="*", const Char_t *lastName="*");
  virtual void   SetZones(Int_t columns=2, Int_t rows=3);
  virtual void   SetPaperSize(Int_t width=20, Int_t height=27);
  virtual void   SetPostScriptFile(const Char_t *psFileName="");
  
  ClassDef(St_QA_Maker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
    
inline void St_QA_Maker::SetDraw(Bool_t drawFlag) { drawinit = drawFlag;}
inline void St_QA_Maker::SetHistsNames(const Char_t *firstName, const Char_t *lastName)
            { m_FirstHistName = firstName;  m_LastHistName  = lastName; }
inline void St_QA_Maker::SetZones(Int_t columns, Int_t rows){ m_PadColumns =columns; m_PadRows = rows;}
inline void St_QA_Maker::SetPaperSize(Int_t width, Int_t height)
            { m_PaperWidth = width; m_PaperHeight = height;}
inline void St_QA_Maker::SetPostScriptFile(const Char_t *psFileName){ m_PsFileName = psFileName;}
