//! $Id: St_QA_Maker.h,v 1.33 1999/09/29 16:46:32 kathy Exp $
//! $Log: St_QA_Maker.h,v $
//! Revision 1.33  1999/09/29 16:46:32  kathy
//! changed code so it would compile in .dev due to changes in DST tables - I even used cons instead of makel - wow! - I just changed variables or commented out some histograms that use now-non-existant variables so it would compile - later I will go through and redefine histograms as needed
//!
//! Revision 1.32  1999/09/23 18:54:11  kathy
//! fix some histogram limits, add about 10 histograms - just so we know number rows in each table - had to include some more tables to do this
//!
//! Revision 1.31  1999/09/21 15:05:38  kathy
//! comment out unneccessary method: SetPntrToHistUtil because now I'm making it totally independent of the histograms printing at the end - also put in doc directory and html file - basically empty now
//!
//! Revision 1.30  1999/09/20 20:12:19  kathy
//! moved the histogram utility methods out of St_QA_Maker and into StHistUtil because they can really be used by any Maker and associated histograms
//!
//! Revision 1.29  1999/07/17 01:51:21  kathy
//! changed limits and titles of some histograms
//!
//! Revision 1.28  1999/07/15 13:57:41  perev
//! cleanup
//!
//! Revision 1.27  1999/07/14 23:23:00  kathy
//! a lot of changes to hist limits and fixes to titles and added a few new hist
//!
//! Revision 1.26  1999/07/12 16:39:35  kathy
//! hopefully last change for globtrk,event_summary and primtrk histograms
//!
//! Revision 1.25  1999/07/09 23:04:07  kathy
//! hopefully getting to final round of fixes to globtrk and primtrk histograms
//!
//! Revision 1.24  1999/07/09 13:14:19  kathy
//! now have put in new primtrk histograms to match the globtrk ones
//!
//! Revision 1.23  1999/07/07 16:58:34  kathy
//! put log scales on some histograms
//!
//! Revision 1.22  1999/07/02 21:56:57  kathy
//! update for tables which exist in 99f AND put in changes to event summary and globtrk histogram sets requested by offline analysis meeting
//!
//! Revision 1.21  1999/06/15 14:44:57  kathy
//! fix St_QA_Maker
//!
//! Revision 1.19  1999/06/11 20:05:54  kathy
//! put in method FindHists to find the histogram directory, since it can be in different places depending on how/where you make the histograms
//!
//! Revision 1.18  1999/05/10 20:03:56  kathy
//! add new member function ExamineLogYList and RemoveFromLogYList
//!
//! Revision 1.17  1999/05/10 17:16:18  kathy
//! added new member function SetDefaultLogYList and implemented and tested
//!
//! Revision 1.16  1999/05/07 17:18:30  kathy
//! new method AddToLogYList implemented and tested on solaris
//!
//! Revision 1.15  1999/05/05 19:35:53  kathy
//! add new method ListHists and clean up
//!
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

#include "TList.h"
#include "TString.h"

// tell it that we're going to use method from StHistUtil class (but
// not using them in header file, so don't need to include StHistUtil.h
//class StHistUtil;

class St_QA_Maker : public StMaker {
 private:
  Bool_t drawinit;
//  StHistUtil *m_PntrToHistUtil;    //! pointer to an StHistUtil

  //! static Char_t m_VersionCVS = "$Id: St_QA_Maker.h,v 1.33 1999/09/29 16:46:32 kathy Exp $";
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
  
 protected:
  
  // for method MakeEvSum - from table event_summary
  TH1F     *m_trk_tot_gd;         //! number of good global tracks divided by total
  TH1F     *m_glb_trk_tot;        //! # tracks total from globtrk
  TH1F     *m_glb_trk_plusminus;  //! # trks pos/neg. 
  TH1F     *m_glb_trk_prim;       //! # trks from primaries
  TH1F     *m_vert_total;         //! total number of vertices
  TH1F     *m_vert_V0;            //! number of V0 vertices
  TH1F     *m_mean_pt;       //! mean pt value
  TH1F     *m_mean_eta;      //! mean eta value 
  TH1F     *m_rms_eta;       //! rms eta value 
  TH1F     *m_T_average;     //! mean Temp
  TH1F     *m_prim_vrtx0;    //! primary vrtx x position
  TH1F     *m_prim_vrtx1;    //! primary vrtx y position
  TH1F     *m_prim_vrtx2;    //! primary vrtx z position
  TH1F     *m_vrtx_chisq;    //! primary vrtx chisq
  
  // for method MakeGlob - from table globtrk
  TH1F     *m_globtrk_tot;   //! # tracks in table
  TH1F     *m_globtrk_good;  //! # tracks in table with iflag>0 
  TH1F     *m_globtrk_iflag; //! iflag value
  TH1F     *m_det_id;        //! detector id of track
  TH1F     *m_point;         //! number of points on the track
  TH1F     *m_max_point;     //! number of max possible track points
  TH1F     *m_fit_point;     //! number of track points used for fitting
  TH1F     *m_glb_charge;    //! particle charge in units of |e|
  TH1F     *m_glb_xf0;       //! x-coord. of first hit - at start of helix
  TH1F     *m_glb_yf0;       //! y-coord. of first hit - at start of helix
  TH1F     *m_glb_zf0;       //! z-coord. of first hit - at start of helix
  TH1F     *m_glb_xf;        //! x-coord. of first hit on trk
  TH1F     *m_glb_yf;        //! y-coord. of first hit on trk
  TH1F     *m_glb_zf;        //! z-coord. of first hit on trk
  TH1F     *m_glb_radf;      //! radial (xy) coordinate of first tpc hit
  TH1F     *m_glb_ratio;     //! ratio of n fit pnts over n pnts
  TH1F     *m_psi;           //! psi reconstructed
  TH1F     *m_tanl;          //! tan(dip) =pz/pt at start
  TH1F     *m_glb_theta;     //! theta - calculated
  TH1F     *m_eta;           //! eta reconstructed
  TH1F     *m_mom;           //! momentum reconstructed
  TH1F     *m_pT;            //! pT  reconstructed
  TH1F     *m_chisq0;        //! chi square [0]
  TH1F     *m_chisq1;        //! chi square [1]
  TH1F     *m_length;        //! length of track
  TH1F     *m_glb_impact;    //! impact parameter from primary vertex
  TH1F     *m_glb_ndf;       //! no. deg. of freedom for track fit.

  TH2F     *m_pT_eta_rec;    //! pT versus eta Spectra for reconstructed
  TH2F     *m_globtrk_xf_yf; //! Y vs X of first hit on trk
  TH2F     *m_tanl_zf;       //! tanl(dip angle) vs zfirst
  TH2F     *m_mom_trklength; //! mom vs. trk length
  TH2F     *m_eta_trklength; //! trk length vs. eta
  TH2F     *m_npoint_length; //! num points vs length
  TH2F     *m_fpoint_length; //! num fit points vs length
  TH2F     *m_chisq0_mom;    //! chisq0 vs momentum
  TH2F     *m_chisq1_mom;    //! chisq1 vs momentum
  TH2F     *m_chisq0_eta;    //! chisq0 vs eta
  TH2F     *m_chisq1_eta;    //! chisq1 vs eta
  TH2F     *m_chisq0_dip;    //! chisq0 vs dip angle
  TH2F     *m_chisq1_dip;    //! chisq1 vs dip angle
  TH2F     *m_chisq0_zf;     //! chisq0 vs zfirst - 
  TH2F     *m_chisq1_zf;     //! chisq1 vs zfirst - 
  TH2F     *m_nfptonpt_mom;  //! mom vs ratio of n fit pnts over n pnts
  TH2F     *m_nfptonpt_eta;  //! eta vs ratio of n fit pnts over n pnts

  
// for method MakeDE - from table dst_dedx
  
  TH1F     *m_ndedxr;        //! number of tracks with dedx info
  TH1F     *m_ndedx;         //! number of point to find dE/dx
  TH1F     *m_dedx0;         //! dE/dx [0]
  TH1F     *m_dedx1;         //! dE/dx [1] 
  

// for method MakeHistPrim - from table primtrk
  TH1F     *m_primtrk_tot;   //! # tracks in table
  TH1F     *m_primtrk_good;  //! # tracks in table with iflag>0 
  TH1F     *m_primtrk_iflag; //! iflag value
  TH1F     *m_pdet_id;       //! detector id of track
  TH1F     *m_ppoint;        //! number of points on the track
  TH1F     *m_pmax_point;    //! number of max possible track points
  TH1F     *m_pfit_point;    //! number of track points used for fitting
  TH1F     *m_prim_charge;   //! particle charge in units of |e|
  TH1F     *m_prim_xf0;      //! x-coord. of first hit - at start of helix
  TH1F     *m_prim_yf0;      //! y-coord. of first hit - at start of helix
  TH1F     *m_prim_zf0;      //! z-coord. of first hit - at start of helix
  TH1F     *m_prim_xf;        //! x-coord. of first hit on trk
  TH1F     *m_prim_yf;        //! y-coord. of first hit on trk
  TH1F     *m_prim_zf;        //! z-coord. of first hit on trk
  TH1F     *m_prim_radf;      //! radial (xy) coordinate of first tpc hit
  TH1F     *m_prim_ratio;     //! ratio of n fit pnts over n pnts
  TH1F     *m_ppsi;           //! psi reconstructed
  TH1F     *m_ptanl;          //! tan(dip) =pz/pt at start
  TH1F     *m_prim_theta;     //! theta - calculated
  TH1F     *m_peta;           //! eta reconstructed
  TH1F     *m_pmom;           //! momentum reconstructed
  TH1F     *m_ppT;            //! pT  reconstructed
  TH1F     *m_pchisq0;        //! chi square [0]
  TH1F     *m_pchisq1;        //! chi square [1]
  TH1F     *m_plength;        //! length of track
  TH1F     *m_prim_impact;    //! impact parameter from primary vertex
  TH1F     *m_prim_ndf;       //! no. deg. of freedom for track fit.

  TH2F     *m_ppT_eta_rec;    //! pT versus eta Spectra for reconstructed
  TH2F     *m_primtrk_xf_yf;  //! Y vs X of first hit on trk
  TH2F     *m_ptanl_zf;       //! tanl(dip angle) vs z coord of first tpc hit
  TH2F     *m_pmom_trklength; //! mom vs. trk length
  TH2F     *m_peta_trklength; //! trk length vs. eta
  TH2F     *m_pnpoint_length; //! num points vs length
  TH2F     *m_pfpoint_length; //! num fit points vs length
  TH2F     *m_pchisq0_mom;    //! chisq0 vs momentum
  TH2F     *m_pchisq1_mom;    //! chisq1 vs momentum
  TH2F     *m_pchisq0_eta;    //! chisq0 vs eta
  TH2F     *m_pchisq1_eta;    //! chisq1 vs eta
  TH2F     *m_pchisq0_dip;    //! chisq0 vs dip angle
  TH2F     *m_pchisq1_dip;    //! chisq1 vs dip angle
  TH2F     *m_pchisq0_zf;    //! chisq0 vs zfirst
  TH2F     *m_pchisq1_zf;    //! chisq1 vs zfirst
  TH2F     *m_pnfptonpt_mom;  //! mom vs ratio of n fit pnts over n pnts
  TH2F     *m_pnfptonpt_eta;  //! eta vs ratio of n fit pnts over n pnts


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
  TH1F     *m_v0;            //! # v0 vertices
  TH1F     *m_ev0_lama_hist; //! Lambda mass
  TH1F     *m_ev0_k0ma_hist; //! K0 mass
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  TH2F     *m_p_dedx_rec;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  TH1F     *m_v_num;   //! number of vertices
  TH1F     *m_v_detid; //! detector id where vertex was found 
  TH1F     *m_v_vtxid; //! vertex type
  TH1F     *m_v_x;     //! vertex coordinates in
  TH1F     *m_v_y;     //!  STAR reference 
  TH1F     *m_v_z;     //!   system
  TH1F     *m_v_pchi2; //! chisq per dof of vertex fit
  
  TH1F     *m_pv_detid; //! row1-detector id where vertex was found 
  TH1F     *m_pv_vtxid; //! row1-vertex type
  TH1F     *m_pv_x;     //! row1-vertex coordinates in
  TH1F     *m_pv_y;     //!  STAR reference 
  TH1F     *m_pv_z;     //!   system
  TH1F     *m_pv_pchi2; //! row1-chisq per dof of vertex fit
  
  
  // for method MakeHistXi
   TH1F     *m_xi_tot;   //! number of xi vertices
  
  // for method MakeHistPoint
   TH1F     *m_pnt_tot;   //! number of tpc hits
  
  // for method MakeHistKink
   TH1F     *m_kink_tot;   //! number of kinks
  
  // for method MakeHistL3
   TH1F     *m_l3_tot;   //! number of l3 tracks
  
  // for method MakeHistV0Eval
   TH1F     *m_v0eval_tot;   //! number of vertices
  
  // for method MakeHistRich
   TH1F     *m_rich_tot;   //! number of rich hits
  
  
  //------------------------------------------------------------------------
  
 public: 
  St_QA_Maker(const char *name="QA", const char *title="evet/QA");
  virtual       ~St_QA_Maker();
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
  virtual void   MakeHistXi(St_DataSet *dst);
  virtual void   MakeHistPoint(St_DataSet *dst);
  virtual void   MakeHistKink(St_DataSet *dst);
  virtual void   MakeHistL3(St_DataSet *dst);
  virtual void   MakeHistV0Eval(St_DataSet *dst);
  virtual void   MakeHistRich(St_DataSet *dst);
  virtual void   BookHistEvSum();
  virtual void   BookHistGlob();
  virtual void   BookHistDE();
  virtual void   BookHistPrim();
  virtual void   BookHistGen();
  virtual void   BookHistV0();
  virtual void   BookHistPID();
  virtual void   BookHistVertex();
  virtual void   BookHistXi();
  virtual void   BookHistPoint();
  virtual void   BookHistKink();
  virtual void   BookHistL3();
  virtual void   BookHistV0Eval();
  virtual void   BookHistRich();
  

  virtual void   SetDraw(Bool_t drawFlag=kTRUE);
//  virtual void   SetPntrToHistUtil(StHistUtil *m1);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_QA_Maker.h,v 1.33 1999/09/29 16:46:32 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(St_QA_Maker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
    
inline void St_QA_Maker::SetDraw(Bool_t drawFlag) 
                         { drawinit = drawFlag;}
//inline void St_QA_Maker::SetPntrToHistUtil(StHistUtil *m1) 
//                          {m_PntrToHistUtil = m1;}









