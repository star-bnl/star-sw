//! $Id: StQABookHist.h,v 1.1 1999/11/19 22:44:43 kathy Exp $ 
//! $Log: StQABookHist.h,v $
//! Revision 1.1  1999/11/19 22:44:43  kathy
//! took histogram booking out of St_QA_Maker as per Thomas' request and put it into separate class StQABookHist which can now be used also by Curtis' class to book histograms - thanks for your help Gene!
//!


#ifndef STAR_StQABookHist
#define STAR_StQABookHist

//////////////////////////////////////////////////////////////////////////
//!                                                                      //
//! 
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

class StQABookHist : public StMaker {
 public:

//! static Char_t m_VersionCVS = "$Id: StQABookHist.h,v 1.1 1999/11/19 22:44:43 kathy Exp $";

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
  
 public:
  
  // for method MakeEvSum - from table event_summary
  TH1F     *m_trk_tot_gd;         //! number of good global tracks divided by total
  TH1F     *m_glb_trk_tot;        //! # tracks total from globtrk
  TH1F     *m_glb_trk_plusminus;  //! # trks pos/neg. 
  TH1F     *m_glb_trk_prim;       //! # trks from primaries
  TH1F     *m_vert_total;         //! total number of vertices
  //  TH1F     *m_vert_V0;            //! number of V0 vertices
  TH1F     *m_mean_pt;       //! mean pt value
  TH1F     *m_mean_eta;      //! mean eta value 
  TH1F     *m_rms_eta;       //! rms eta value 
  //  TH1F     *m_T_average;     //! mean Temp
  TH1F     *m_prim_vrtx0;    //! primary vrtx x position
  TH1F     *m_prim_vrtx1;    //! primary vrtx y position
  TH1F     *m_prim_vrtx2;    //! primary vrtx z position
  //  TH1F     *m_vrtx_chisq;    //! primary vrtx chisq
  
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
  
   //   Char_t *QAHistType;  //! character string to prepend to each hist name

//------------------------------------------------------------------------
  
 public: 

  StQABookHist(const char *name, const char *title);
  virtual       ~StQABookHist();
  virtual Int_t  Init();
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
  

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQABookHist.h,v 1.1 1999/11/19 22:44:43 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StQABookHist, 1)   //needed for all code that will be used in CINT
    };
    
#endif
    







