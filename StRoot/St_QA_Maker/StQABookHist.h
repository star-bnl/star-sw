//! $Id: StQABookHist.h,v 1.20 2000/02/10 23:02:45 kathy Exp $ 
//! $Log: StQABookHist.h,v $
//! Revision 1.20  2000/02/10 23:02:45  kathy
//! changed limits on linear impact param hist; added new hist of detector id values for dst_point table
//!
//! Revision 1.19  2000/02/10 21:31:29  kathy
//! add another set of impact param hist so we can see them in linear scale too
//!
//! Revision 1.18  2000/02/07 19:49:06  kathy
//! removed L3 trigger histograms and methods that created them - this table is no longer standard on the DST; created methods BookHistEval and MakeHistEval for geant vs reco evaluation histograms; filled geant vs reco evaluation histograms for table-based data
//!
//! Revision 1.17  2000/02/04 19:53:57  kathy
//! added 2 more histograms - for med and small range of # hits in detector
//!
//! Revision 1.16  2000/02/03 22:02:31  kathy
//! adding histograms for Akio - needed smaller ranges of some of them for use by peripheral collisions group
//!
//! Revision 1.15  2000/01/31 22:15:25  kathy
//! added Gene's code to make mass plot for Xi's in table and StEvent versions
//!
//! Revision 1.14  2000/01/07 20:35:00  kathy
//! make some corrections to filling hist; add point hist for each det separately
//!
//! Revision 1.13  1999/12/17 22:11:33  kathy
//! add psi vs phi hist, change limits
//!
//! Revision 1.12  1999/12/15 20:32:17  kathy
//! separated the tpc and tpc+svt histograms for globtrk table; had to book and fill new histograms, add histograms to default logy list AND had to change what values of iflag I cut on for filling each different type of track in makehistglob method
//!
//! Revision 1.11  1999/12/15 18:31:05  kathy
//! added 4 new histogram to globtrk for tpc - r0,phi0,z0,curvature; also put 3 of these in default logY list; also changed scale on iflag hist. for globtrk & primtrk
//!
//! Revision 1.10  1999/12/15 17:17:33  kathy
//! changed the dedx histograms to the scale GeV/cm - which is the scale in the dst table
//!
//! Revision 1.9  1999/12/14 18:33:24  kathy
//! removed 4 ftpc histograms as per Janet's request
//!
//! Revision 1.8  1999/12/12 23:09:47  kathy
//! add pt vs eta in ftpc histogram as per Janet
//!
//! Revision 1.7  1999/12/08 22:58:17  kathy
//! changed histogram limits and made names smaller
//!
//! Revision 1.6  1999/12/07 23:14:18  kathy
//! fix primary vtx histograms for dst tables; split apart the ftpc and tpc in the dedx histograms
//!
//! Revision 1.5  1999/12/06 22:25:05  kathy
//! split apart the tpc and ftpc (east & west) histograms for the globtrk table; had to add characters to end of each histogram pointer to differentiate the different ones; updated the default list of hist to be plotted with logy scale
//!
//! Revision 1.4  1999/11/29 21:50:38  kathy
//! remove St_QATestTables_Maker class - not used anywhere; remove SetDraw method from StQABookHist method - not needed
//!
//! Revision 1.3  1999/11/23 19:00:51  lansdell
//! Reorganized Make() and include files (Gene)
//!
//! Revision 1.2  1999/11/22 22:46:41  lansdell
//! update to identify histogram method used (StEvent or DST tables) by Gene; StEventQAMaker code partially completed (run bfcread_dst_EventQAhist.C)
//!
//! Revision 1.1  1999/11/19 22:44:43  kathy
//! took histogram booking out of St_QA_Maker as per Thomas' request and put it into separate class StQABookHist which can now be used also by Curtis' class to book histograms - thanks for your help Gene!
//!
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQABookHist abstract base class for QA Histogram Makers             //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StQABookHist
#define STAR_StQABookHist

#include "StMaker.h"
#include "TString.h"
class TH1F;
class TH2F;

class StQABookHist : public StMaker {
 public:

//! static Char_t m_VersionCVS = "$Id: StQABookHist.h,v 1.20 2000/02/10 23:02:45 kathy Exp $";

//! Histograms booking constants
  static const Int_t nxpT;
  static const Int_t nyeta;
  static const Float_t xminpT;
  static const Float_t xmaxpT;
  //  static const Float_t ymineta;
  //  static const Float_t ymaxeta;
  
  static const Int_t nchisq;
  //  static const Int_t nmass;
  //  static const Int_t ntau; 
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
  
  // for method MakeEvSum - from table event_summary
  TH1F     *m_trk_tot_gd;         //! num of good trks over total - global
  TH1F     *m_glb_trk_tot;        //! # tracks total from globtrk
  TH1F     *m_glb_trk_tot_sm;     //! # tracks total from globtrk, small range
  TH1F     *m_glb_trk_plusminus;  //! # trks pos/neg. 
  TH1F     *m_glb_trk_plusminus_sm; //! # trks pos/neg., small range 
  TH1F     *m_glb_trk_prim;       //! # trks from primaries
  TH1F     *m_glb_trk_prim_sm;    //! # trks from primaries, small range
  TH1F     *m_vert_total;         //! total number of vertices
  TH1F     *m_vert_total_sm;      //! total number of vertices, small range
  TH1F     *m_mean_pt;            //! mean pt value
  TH1F     *m_mean_pt_sm;         //! mean pt value, small range
  TH1F     *m_mean_eta;           //! mean eta value 
  TH1F     *m_rms_eta;            //! rms eta value 
  TH1F     *m_prim_vrtx0;         //! primary vrtx x position
  TH1F     *m_prim_vrtx1;         //! primary vrtx y position
  TH1F     *m_prim_vrtx2;         //! primary vrtx z position

  
  // for method MakeGlob - from table globtrk
  TH1F     *m_globtrk_tot;    //! # tracks in table
  TH1F     *m_globtrk_tot_sm; //! # tracks in table, small range
  TH1F     *m_globtrk_good;   //! # tracks in table with iflag>0 
  TH1F     *m_globtrk_good_sm;//! # tracks in table with iflag>0,small range
  TH1F     *m_globtrk_iflag; //! iflag value
  TH1F     *m_det_id;        //! detector id of track

  TH1F     *m_pointT;        //! number of points on the track - tpc
  TH1F     *m_pointFE;       //! number of points on the track - ftpc east
  TH1F     *m_pointFW;       //! number of points on the track - ftpc west
  TH1F     *m_max_pointT;    //! number of max possible track points - tpc
  TH1F     *m_max_pointFE;   //! number of max possible track points - ftpc east
  TH1F     *m_max_pointFW;   //! number of max possible track points - ftpc west
  TH1F     *m_fit_pointT;    //! number of track points used for fitting - tpc
  TH1F     *m_fit_pointFE;   //! number of track points used for fitting - ftpc east
  TH1F     *m_fit_pointFW;   //! number of track points used for fitting - ftpc west
  TH1F     *m_glb_ratioT;    //! ratio of n fit pnts over tot n pnts - tpc
  TH1F     *m_glb_ratioFE;   //! ratio of n fit pnts over tot n pnts - ftpc east
  TH1F     *m_glb_ratioFW;   //! ratio of n fit pnts over tot n pnts - ftpc west
  TH1F     *m_glb_ratiomT;   //! ratio of n fit pnts over max n pnts - tpc
  TH1F     *m_glb_ratiomFE;  //! ratio of n fit pnts over max n pnts - ftpc east
  TH1F     *m_glb_ratiomFW;  //! ratio of n fit pnts over max n pnts - ftpc west
  TH1F     *m_glb_chargeT;   //! particle charge in units of |e| - tpc
  TH1F     *m_glb_chargeFE;  //! particle charge in units of |e| - ftpc east
  TH1F     *m_glb_chargeFW;  //! particle charge in units of |e| - ftpc west
  TH1F     *m_glb_r0T;       //! radius at start (cm), tpc 
  TH1F     *m_glb_phi0T;     //! azimuthal angle at start (deg), tpc
  TH1F     *m_glb_z0T;       //! z-coord at start (cm), tpc 
  TH1F     *m_glb_curvT;     //! curvature (1/cm), tpc
  TH1F     *m_glb_xfT;       //! x-coord. of first hit on trk, tpc
  TH1F     *m_glb_xfFE;      //! x-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_xfFW;      //! x-coord. of first hit on trk, ftpc west
  TH1F     *m_glb_yfT;       //! y-coord. of first hit on trk, tpc
  TH1F     *m_glb_yfFE;      //! y-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_yfFW;      //! y-coord. of first hit on trk, ftpc west
  TH1F     *m_glb_zfT;       //! z-coord. of first hit on trk, tpc
  TH1F     *m_glb_zfFE;      //! z-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_zfFW;      //! z-coord. of first hit on trk, ftpc west
  TH1F     *m_glb_xf0;       //! x-coord. of first hit - at start of helix
  TH1F     *m_glb_yf0;       //! y-coord. of first hit - at start of helix
  TH1F     *m_glb_zf0;       //! z-coord. of first hit - at start of helix
  TH1F     *m_glb_radfT;     //! radial (xy) coordinate of first hit, tpc
  TH1F     *m_glb_radfFE;    //! radial (xy) coordinate of first hit, ftpc east
  TH1F     *m_glb_radfFW;    //! radial (xy) coordinate of first hit, ftpc west
  TH1F     *m_psiT;          //! psi reconstructed, tpc
  TH1F     *m_psiFE;         //! psi reconstructed, ftpc east
  TH1F     *m_psiFW;         //! psi reconstructed, ftpc west
  TH1F     *m_tanlT;         //! tan(dip) =pz/pt at start, tpc
  TH1F     *m_glb_thetaT;    //! theta - tpc
  TH1F     *m_etaT;          //! eta, tpc
  TH1F     *m_etaFE;         //! eta, ftpc east
  TH1F     *m_etaFW;         //! eta, ftpc west
  TH1F     *m_momT;          //! momentum, tpc
  TH1F     *m_momFE;         //! momentum, ftpc east
  TH1F     *m_momFW;         //! momentum, ftpc west
  TH1F     *m_pTT;           //! pT, tpc
  TH1F     *m_pTFE;          //! pT, ftpc east
  TH1F     *m_pTFW;          //! pT, ftpc west
  TH1F     *m_lengthT;       //! length of track, tpc
  TH1F     *m_lengthFE;      //! length of track, ftpc east
  TH1F     *m_lengthFW;      //! length of track, ftpc west
  TH1F     *m_chisq0T;       //! chi square [0], tpc
  TH1F     *m_chisq0FE;      //! chi square [0], ftpc east
  TH1F     *m_chisq0FW;      //! chi square [0], ftpc west
  TH1F     *m_chisq1T;       //! chi square [1], tpc
  TH1F     *m_chisq1FE;      //! chi square [1], ftpc east
  TH1F     *m_chisq1FW;      //! chi square [1], ftpc west
  TH1F     *m_glb_impactT;   //! impact parameter from primary vertex, tpc
  TH1F     *m_glb_impactrT;  //! impact parameter from primary vertex, tpc

// TPC + SVT HISTOGRAMS - 1D
  TH1F     *m_pointTS;        //! number of points on the track - tpc+svt
  TH1F     *m_max_pointTS;    //! number of max possible track points - tpc+svt
  TH1F     *m_fit_pointTS;    //! number of track points used for fitting - tpc+svt
  TH1F     *m_glb_ratioTS;    //! ratio of n fit pnts over tot n pnts - tpc+svt
  TH1F     *m_glb_ratiomTS;   //! ratio of n fit pnts over max n pnts - tpc+svt
  TH1F     *m_glb_chargeTS;   //! particle charge in units of |e| - tpc+svt
  TH1F     *m_glb_r0TS;       //! radius at start (cm), tpc+svt
  TH1F     *m_glb_phi0TS;     //! azimuthal angle at start (deg), tpc+svt
  TH1F     *m_glb_z0TS;       //! z-coord at start (cm), tpc+svt
  TH1F     *m_glb_curvTS;     //! curvature (1/cm), tpc+svt
  TH1F     *m_glb_xfTS;       //! x-coord. of first hit on trk, tpc+svt
  TH1F     *m_glb_yfTS;       //! y-coord. of first hit on trk, tpc+svt
  TH1F     *m_glb_zfTS;       //! z-coord. of first hit on trk, tpc+svt
  TH1F     *m_glb_xf0TS;       //! x-coord. of first hit - at start of helix+svt
  TH1F     *m_glb_yf0TS;       //! y-coord. of first hit - at start of helix+svt
  TH1F     *m_glb_zf0TS;       //! z-coord. of first hit - at start of helix+svt
  TH1F     *m_glb_radfTS;     //! radial (xy) coordinate of first hit, tpc+svt
  TH1F     *m_psiTS;          //! psi reconstructed, tpc+svt
  TH1F     *m_tanlTS;         //! tan(dip) =pz/pt at start, tpc+svt
  TH1F     *m_glb_thetaTS;    //! theta - tpc+svt
  TH1F     *m_etaTS;          //! eta, tpc+svt
  TH1F     *m_momTS;          //! momentum, tpc+svt
  TH1F     *m_pTTS;           //! pT, tpc+svt
  TH1F     *m_lengthTS;       //! length of track, tpc+svt
  TH1F     *m_chisq0TS;       //! chi square [0], tpc+svt
  TH1F     *m_chisq1TS;       //! chi square [1], tpc+svt
  TH1F     *m_glb_impactTS;   //! impact parameter from primary vertex, tpc+svt
  TH1F     *m_glb_impactrTS;  //! impact parameter from primary vertex, tpc+svt


  TH2F     *m_pT_eta_recT;     //! pT versus eta, tpc
  TH2F     *m_pT_eta_recFE;    //! pT versus eta, ftpcE
  TH2F     *m_pT_eta_recFW;    //! pT versus eta, ftpcW
  TH2F     *m_globtrk_xf_yfT;  //! Y vs X of first hit on trk, tpc
  TH2F     *m_globtrk_xf_yfFE; //! Y vs X of first hit on trk, ftpc east
  TH2F     *m_globtrk_xf_yfFW; //! Y vs X of first hit on trk, ftpc west
  TH2F     *m_tanl_zfT;        //! tanl(dip angle) vs zfirst, tpc
  TH2F     *m_mom_trklengthT;  //! mom vs. trk length, tpc
  TH2F     *m_eta_trklengthT;  //! trk length vs. eta, tpc
  TH2F     *m_eta_trklengthFE; //! trk length vs. eta, ftpc east
  TH2F     *m_eta_trklengthFW; //! trk length vs. eta, ftpc west
  TH2F     *m_fpoint_lengthT;  //! num fit points vs length, tpc
  TH2F     *m_fpoint_lengthFE; //! num fit points vs length, ftpc east
  TH2F     *m_fpoint_lengthFW; //! num fit points vs length, ftpc west
  TH2F     *m_npoint_lengthT;  //! tot num points vs length, tpc
  TH2F     *m_npoint_lengthFE; //! tot num points vs length, ftpc east
  TH2F     *m_npoint_lengthFW; //! tot num points vs length, ftpc west
  TH2F     *m_chisq0_momT;     //! chisq0 vs momentum, tpc
  TH2F     *m_chisq1_momT;     //! chisq1 vs momentum, tpc
  TH2F     *m_chisq0_etaT;     //! chisq0 vs eta, tpc
  TH2F     *m_chisq1_etaT;     //! chisq1 vs eta, tpc
  TH2F     *m_chisq0_dipT;     //! chisq0 vs dip angle, tpc
  TH2F     *m_chisq1_dipT;     //! chisq1 vs dip angle, tpc
  TH2F     *m_chisq0_zfT;      //! chisq0 vs zfirst, tpc 
  TH2F     *m_chisq1_zfT;      //! chisq1 vs zfirst, tpc 
  TH2F     *m_nfptonpt_momT;   //! mom vs ratio of n fit pnts over n pnts, tpc
  TH2F     *m_nfptonpt_etaT;   //! eta vs ratio of n fit pnts over n pnts, tpc
  TH2F     *m_psi_phiT;        //! psi vs phi, tpc

// TPC + SVT HISTOGRAMS - 2D
  TH2F     *m_pT_eta_recTS;     //! pT versus eta, tpc+svt
  TH2F     *m_globtrk_xf_yfTS;  //! Y vs X of first hit on trk, tpc+svt
  TH2F     *m_tanl_zfTS;        //! tanl(dip angle) vs zfirst, tpc+svt
  TH2F     *m_mom_trklengthTS;  //! mom vs. trk length, tpc+svt
  TH2F     *m_eta_trklengthTS;  //! trk length vs. eta, tpc+svt
  TH2F     *m_fpoint_lengthTS;  //! num fit points vs length, tpc+svt
  TH2F     *m_npoint_lengthTS;  //! tot num points vs length, tpc+svt
  TH2F     *m_chisq0_momTS;     //! chisq0 vs momentum, tpc+svt
  TH2F     *m_chisq1_momTS;     //! chisq1 vs momentum, tpc+svt
  TH2F     *m_chisq0_etaTS;     //! chisq0 vs eta, tpc+svt
  TH2F     *m_chisq1_etaTS;     //! chisq1 vs eta, tpc+svt
  TH2F     *m_chisq0_dipTS;     //! chisq0 vs dip angle, tpc+svt
  TH2F     *m_chisq1_dipTS;     //! chisq1 vs dip angle, tpc+svt
  TH2F     *m_chisq0_zfTS;      //! chisq0 vs zfirst, tpc+svt
  TH2F     *m_chisq1_zfTS;      //! chisq1 vs zfirst, tpc+svt
  TH2F     *m_nfptonpt_momTS;   //! mom vs ratio of n fit pnts over n pnts, tpc+svt
  TH2F     *m_nfptonpt_etaTS;   //! eta vs ratio of n fit pnts over n pnts, tpc+svt
  TH2F     *m_psi_phiTS;        //! psi vs phi, tpc+svt
  
// for method MakeDE - from table dst_dedx
  
  TH1F     *m_ndedxr;        //! number of tracks with dedx info

  TH1F     *m_ndedxT;         //! number of point to find dE/dx, tpc
  TH1F     *m_dedx0T;         //! dE/dx [0], tpc
  TH1F     *m_dedx1T;         //! dE/dx [1], tpc

  TH1F     *m_ndedxFE;         //! number of point to find dE/dx, ftpcE
  TH1F     *m_dedx0FE;         //! dE/dx [0], ftpcE
  TH1F     *m_dedx1FE;         //! dE/dx [1], ftpcE

  TH1F     *m_ndedxFW;         //! number of point to find dE/dx, ftpcW
  TH1F     *m_dedx0FW;         //! dE/dx [0], ftpcW
  TH1F     *m_dedx1FW;         //! dE/dx [1], ftpcW

  

// for method MakeHistPrim - from table primtrk
  TH1F     *m_primtrk_tot;    //! # tracks in table
  TH1F     *m_primtrk_tot_sm; //! # tracks in table, small range
  TH1F     *m_primtrk_good;   //! # tracks in table with iflag>0
  TH1F     *m_primtrk_good_sm;//! # tracks in table with iflag>0, small range
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
  TH1F     *m_prim_impactr;   //! impact parameter from primary vertex

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
  TH1F     *m_H_npart_sm; //! total num particles generated, small rnage
  TH1F     *m_H_ncpart;   //! num chg e,mu,proton,kaon,pion
  TH1F     *m_H_ncpart_sm;//! num chg e,mu,proton,kaon,pion, small range
  
  // for MakeHistV0 - from table dst_v0_vertex
  TH1F     *m_v0;            //! # v0 vertices
  TH1F     *m_ev0_lama_hist; //! Lambda mass
  TH1F     *m_ev0_k0ma_hist; //! K0 mass
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  TH2F     *m_p_dedx_rec;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  TH1F     *m_v_num;   //! number of vertices
  TH1F     *m_v_num_sm;//! number of vertices,small range
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
   TH1F     *m_xi_tot;     //! number of xi vertices
   TH1F     *m_xi_ma_hist; //!  xi Mass

  
  // for method MakeHistPoint
   TH1F     *m_pnt_tot;     //! number of hits total
   TH1F     *m_pnt_tot_med; //! number of hits total, med range
   TH1F     *m_pnt_tot_sm;  //! number of hits total, small range
   TH1F     *m_pnt_id;      //! detector ID of the hit

   TH1F     *m_pnt_tpc;   //! number of hits tpc
   TH1F     *m_pnt_svt;   //! number of hits svt
   TH1F     *m_pnt_ssd;   //! number of hits ssd
   TH1F     *m_pnt_ftpcE;   //! number of hits ftpcE
   TH1F     *m_pnt_ftpcW;   //! number of hits ftpcW
  
  // for method MakeHistKink
   TH1F     *m_kink_tot;   //! number of kinks
  
  // for method MakeHistV0Eval
   TH1F     *m_v0eval_tot;   //! number of vertices
  
  // for method MakeHistRich
   TH1F     *m_rich_tot;   //! number of rich hits

  // for method MakeHistEval
   TH1F *m_geant_reco_pvtx_x;  //! prim vtx x, diff geant - reco
   TH1F *m_geant_reco_pvtx_y;  //! prim vtx y, diff geant - reco
   TH1F *m_geant_reco_pvtx_z;  //! prim vtx z, diff geant - reco
   TH2F *m_geant_reco_vtx_z_z; //! prim vtx z, diff geant - reco vs reco z

  

 protected:

//   Bool_t drawinit;

   TString QAHistType;   // character string to prepend to each hist name/title
   TString QAHistName;   // character string for each hist name
   TString QAHistTitle;  // character string for each hist title
   const char* NameIt(const char* name); // method for naming histograms
   const char* TitleIt(const char* name); // method for titling histograms
   TH1F* QAH1F(const Text_t* name, const Text_t* title,
               Int_t nbinsx, Axis_t xlow, Axis_t xup);
   TH2F* QAH2F(const Text_t* name, const Text_t* title,
               Int_t nbinsx, Axis_t xlow, Axis_t xup,
               Int_t nbinsy, Axis_t ylow, Axis_t yup); // method for 2d-hists


//------------------------------------------------------------------------
  
 public:

  StQABookHist(const char *name, const char *title, const char *type);
  virtual       ~StQABookHist();
  virtual Int_t  Init();
  virtual Int_t  Make();

 protected:

  virtual void   MakeHistEvSum() = 0;
  virtual void   MakeHistGlob() = 0;
  virtual void   MakeHistDE() = 0;
  virtual void   MakeHistPrim() = 0;
  virtual void   MakeHistGen() = 0;
  virtual void   MakeHistV0() = 0;
  virtual void   MakeHistPID() = 0;
  virtual void   MakeHistVertex() = 0;
  virtual void   MakeHistXi() = 0;
  virtual void   MakeHistPoint() = 0;
  virtual void   MakeHistKink() = 0;
  virtual void   MakeHistV0Eval() = 0;
  virtual void   MakeHistRich() = 0;
  virtual void   MakeHistEval() = 0;

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
  virtual void   BookHistV0Eval();
  virtual void   BookHistRich();
  virtual void   BookHistEval();

  
 public:


//  virtual void   SetDraw(Bool_t drawFlag=kTRUE) {drawinit = drawFlag;}

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQABookHist.h,v 1.20 2000/02/10 23:02:45 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StQABookHist, 1)   //needed for all code that will be used in CINT
    };
    
#endif
    







