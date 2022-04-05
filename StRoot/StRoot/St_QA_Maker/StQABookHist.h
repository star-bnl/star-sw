// $Id: StQABookHist.h,v 2.40 2019/03/14 02:31:53 genevb Exp $ 
// $Log: StQABookHist.h,v $
// Revision 2.40  2019/03/14 02:31:53  genevb
// Introduce iTPC plots
//
// Revision 2.39  2018/07/03 21:33:34  genevb
// Introduce EPD (code provided by J. Ewigleben)
//
// Revision 2.38  2016/05/13 22:04:49  genevb
// Address coverity findings: uninit vars, dead code, one PMD error, and one TOF error
//
// Revision 2.37  2015/07/17 19:09:03  genevb
// SSD copied for SST, and HFT histogams use SST now too
//
// Revision 2.36  2015/04/02 19:53:47  genevb
// TPC dE/dx changes: Bethe-Bloch => Bichsel, and tighter cuts against pile-up tracks
//
// Revision 2.35  2015/01/16 21:08:28  genevb
// Initial versions of HFT histograms
//
// Revision 2.34  2014/08/06 11:43:53  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.33  2014/07/22 20:39:28  genevb
// Add MTD to Offline QA
//
// Revision 2.32  2012/03/05 03:42:32  genevb
// Remove TPC XY dist, add TPC RPhi charge
//
// Revision 2.31  2012/02/09 03:01:05  genevb
// No FTPC histograms for Run 12+
//
// Revision 2.30  2011/05/31 21:35:50  genevb
// TPC request: add time bucket distribution of hits
//
// Revision 2.29  2009/03/19 01:08:08  genevb
// Show both xy and rphi TPC hit hists
//
// Revision 2.28  2007/11/30 05:38:50  genevb
// Changes for Run8: mostly silicon removal, TOF addition
//
// Revision 2.27  2007/04/25 18:35:57  genevb
// Additional SSD hists
//
// Revision 2.26  2007/04/24 00:33:59  genevb
// SSD hists
//
// Revision 2.25  2007/04/12 20:39:48  genevb
// Cleanup (removal) of CalibVtx, Nfitpnt, Chisq1, Rich, histograms
//
// Revision 2.24  2007/04/07 04:40:30  genevb
// Remove fit pnts/tot; retitle log as log10
//
// Revision 2.23  2007/03/13 18:46:29  genevb
// Added Svt Laser Diff
//
// Revision 2.22  2007/02/26 20:45:01  genevb
// SVT drift hist
//
// Revision 2.21  2005/01/27 05:28:25  genevb
// PMD changes
//
// Revision 2.20  2004/12/13 15:52:37  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.19  2004/10/04 16:40:42  genevb
// FTPC radial histos
//
// Revision 2.18  2004/04/23 23:15:30  genevb
// Added signedDCA (Impact) plots for globals
//
// Revision 2.17  2004/02/12 05:03:12  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.16  2004/01/10 01:10:18  genevb
// Preparations for Year 5, added some svt plots
//
// Revision 2.15  2003/02/20 20:09:54  genevb
// Several changes for new trigger scheme, dAu data
//
// Revision 2.14  2003/02/19 06:38:29  genevb
// Rework trigger and mult/event class sections
//
// Revision 2.13  2002/04/23 01:59:56  genevb
// Addition of BBC/FPD histos
//
// Revision 2.12  2002/02/12 18:42:00  genevb
// Additional FTPC histograms
//
// Revision 2.11  2002/01/21 22:09:24  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.10  2001/11/20 21:53:46  lansdell
// added x-y dist of hits, tpc east&west histos
//
// Revision 2.9  2001/08/03 20:33:55  lansdell
// added primvtx check histos for different multiplicities; separated x-y plot of first point on track, tpc into east and west histos
//
// Revision 2.8  2001/07/31 23:21:42  lansdell
// added last point, hit-helix histos
//
// Revision 2.7  2001/05/24 01:48:14  lansdell
// qa_shift histograms updated
//
// Revision 2.6  2001/05/23 00:14:53  lansdell
// more changes for qa_shift histograms
//
// Revision 2.5  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.4  2001/04/28 22:05:13  genevb
// Added EMC histograms
//
// Revision 2.3  2001/04/25 21:35:26  genevb
// Added V0 phi distributions
//
// Revision 2.2  2001/04/24 22:53:51  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.1  2000/09/08 18:55:54  lansdell
// turned on FTPC primary track histograms
//
// Revision 2.0  2000/08/25 16:02:41  genevb
// New revision: new structure, multiplicity classes
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQABookHist class for multiplicity-based QA histograms              //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StQABookHist
#define STAR_StQABookHist

#include "TString.h"
#include "TObject.h"
class TH1F;
class TH2F;
class TH3F;
class StMaker;

class StQABookHist : public TObject {

// ************************ Public Functions *****************************
 public:

  StQABookHist(const char *type);
  virtual       ~StQABookHist() {}
  virtual void   BookHist(Int_t hSet);

  const char*    HistType() {return QAHistType.Data();}

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQABookHist.h,v 2.40 2019/03/14 02:31:53 genevb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


// ******************** Histogram Booking Constants ************************
 protected:

  Int_t nxpT;
  Int_t nyeta;
  Float_t xminpT;
  Float_t xmaxpT;
  //  Float_t ymineta;
  //  Float_t ymaxeta;
  
  Int_t nchisq;
  //  Int_t nmass;
  //  Int_t ntau; 
  Int_t ndedx;  
  Int_t npnt;   
  Int_t nleng;  
  Int_t npsi;   
  Int_t knpsi;  
  Int_t nvrt;   
  Int_t knyeta; 
  Int_t knid;   
  Int_t cnp; 
  Int_t cndedx; 
  
  Float_t kminnid;  
  Float_t kmaxnid; 
  Float_t minpsi;   
  Float_t kminpsi;  
  Float_t maxpsi;   
  Float_t minchisq; 
  Float_t maxchisq; 
  Float_t minmass;  
  Float_t maxmass;  
  Float_t minpnt;   
  Float_t maxpnt;   
  Float_t minleng;  
  Float_t maxleng;  
  Float_t mintau;   
  Float_t maxtau;   
  Float_t mintrk;   
  Float_t maxtrk;   
  Float_t minvrt;   
  Float_t maxvrt;   
  Float_t minmpt;   
  Float_t maxmpt;   
  Float_t minmeta;  
  Float_t maxmeta;  
  Float_t kmineta;  
  Float_t kmaxeta;  
  Float_t minxyz;   
  Float_t maxxyz;   
  Float_t cminp; 
  Float_t cmaxp; 
  Float_t cmindedx; 
  Float_t cmaxdedx; 


// ************************ Histogram Pointers *****************************
 public:

  TH1F     *mNullPrimVtxClass;  //! primary vertex check (based on
                                //! event class for real data)
  
  // for method MakeGlob - from table globtrk
  TH1F     *m_globtrk_fit_prob; //!
  TH1F     *m_globtrk_tot;      //! # tracks in table
  TH1F     *m_globtrk_good;     //! # tracks in table with iflag>0 
  TH1F     *m_globtrk_good_sm;  //! # tracks in table with iflag>0,small range
  TH1F     *m_globtrk_goodTTS;  //! # tracks in table with iflag>0, tpc, tpc+svt
  TH2F     *m_globtrk_goodF;    //! # tracks in table with iflag>0, ftpc
  TH1F     *m_globtrk_iflag;    //! iflag value
  TH1F     *m_det_id;           //! detector id of track
  TH2F     *m_dcaToBeamXY;      //! xy-dca to beam axis
  TH1F     *m_dcaToBeamZ1;      //! z-dca to beam axis -210 to -105
  TH1F     *m_dcaToBeamZ2;      //! z-dca to beam axis -105 to 0
  TH1F     *m_dcaToBeamZ3;      //! z-dca to beam axis 0 to 105
  TH2F     *m_zDcaTanl;         //! z-dca vs tanl
  TH2F     *m_zDcaZf;           //! z-dca vs z-first
  TH2F     *m_zDcaPsi;          //! z-dca vs psi
  TH2F     *m_zDcaPhi0;         //! z-dca vs phi0

  TH1F     *m_pointT;        //! number of points on the track - tpc
  TH1F     *m_pointTS;       //! number of points on the track - tpc+svt
  TH2F     *m_pointF;        //! number of points on the track - ftpc
  TH1F     *m_pointFE;       //! number of points on the track - ftpc east
  TH1F     *m_pointFW;       //! number of points on the track - ftpc west
  TH1F     *m_max_pointT;    //! number of max possible track points - tpc
  TH1F     *m_max_pointTS;   //! number of max possible track points - tpc+svt
  TH2F     *m_max_pointF;    //! number of max possible track points - ftpc
  TH1F     *m_max_pointFE;   //! number of max possible track points - ftpc east
  TH1F     *m_max_pointFW;   //! number of max possible track points - ftpc west
  TH1F     *m_fit_pointT;    //! number of track points used for fitting - tpc
  TH1F     *m_fit_pointTS;   //! number of track points used for fitting - tpc+svt
  TH2F     *m_fit_pointTTS;  //! number of track points used for fitting - tpc,tpc+svt
  TH1F     *m_glb_sptsTS;  //! number of track pnts on svt - tpc+svt
  TH2F     *m_glb_ratiomTTS; //! ratio of n fit pnts over max n pnts - tpc,tpc+svt
  TH1F     *m_glb_ratiomT;   //! ratio of n fit pnts over max n pnts - tpc
  TH1F     *m_glb_ratiomTS;  //! ratio of n fit pnts over max n pnts - tpc+svt
  TH2F     *m_glb_ratiomF;   //! ratio of n fit pnts over max n pnts - ftpc
  TH1F     *m_glb_ratiomFE;  //! ratio of n fit pnts over max n pnts - ftpc east
  TH1F     *m_glb_ratiomFW;  //! ratio of n fit pnts over max n pnts - ftpc west
  TH1F     *m_glb_chargeT;   //! particle charge in units of |e| - tpc
  TH1F     *m_glb_chargeTS;  //! particle charge in units of |e| - tpc+svt
  TH2F     *m_glb_chargeF;   //! particle charge in units of |e| - ftpc
  TH1F     *m_glb_chargeFE;  //! particle charge in units of |e| - ftpc east
  TH1F     *m_glb_chargeFW;  //! particle charge in units of |e| - ftpc west
  TH1F     *m_glb_r0T;       //! radius at start (cm), tpc 
  TH1F     *m_glb_r0TS;      //! radius at start (cm), tpc+svt
  TH1F     *m_glb_phi0T;     //! azimuthal angle at start (deg), tpc
  TH1F     *m_glb_phi0TS;    //! azimuthal angle at start (deg), tpc+svt
  TH1F     *m_glb_z0T;       //! z-coord at start (cm), tpc 
  TH1F     *m_glb_z0TS;      //! z-coord at start (cm), tpc+svt
  TH1F     *m_glb_padfT;     //! padrow of first hit on trk, tpc
  TH2F     *m_glb_padfTEW;   //! padrow of first hit on trk, tpc east and west
  TH1F     *m_glb_curvT;     //! curvature (1/cm), tpc
  TH1F     *m_glb_curvTS;    //! curvature (1/cm), tpc+svt
  TH1F     *m_glb_xfT;       //! x-coord. of first hit on trk, tpc
  TH1F     *m_glb_xfTS;      //! x-coord. of first hit on trk, tpc+svt
  TH2F     *m_glb_xfF;       //! x-coord. of first hit on trk, ftpc
  TH1F     *m_glb_xfFE;      //! x-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_xfFW;      //! x-coord. of first hit on trk, ftpc west
  TH1F     *m_glb_yfT;       //! y-coord. of first hit on trk, tpc
  TH1F     *m_glb_yfTS;      //! y-coord. of first hit on trk, tpc+svt
  TH2F     *m_glb_yfF;       //! y-coord. of first hit on trk, ftpc
  TH1F     *m_glb_yfFE;      //! y-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_yfFW;      //! y-coord. of first hit on trk, ftpc west
  TH1F     *m_glb_zfT;       //! z-coord. of first hit on trk, tpc
  TH1F     *m_glb_zfTS;      //! z-coord. of first hit on trk, tpc+svt
  TH2F     *m_glb_zfF;       //! z-coord. of first hit on trk, ftpc
  TH1F     *m_glb_zfFE;      //! z-coord. of first hit on trk, ftpc east
  TH1F     *m_glb_zfFW;      //! z-coord. of first hit on trk, ftpc west
  TH2F     *m_glb_planefF;   //! plane of first hit on trk, ftpc
  TH1F     *m_glb_xf0;       //! x-coord. of first hit - at start of helix
  TH1F     *m_glb_xf0TS;     //! x-coord. of first hit - at start of helix+svt
  TH1F     *m_glb_yf0;       //! y-coord. of first hit - at start of helix
  TH1F     *m_glb_yf0TS;     //! y-coord. of first hit - at start of helix+svt
  TH1F     *m_glb_zf0;       //! z-coord. of first hit - at start of helix
  TH1F     *m_glb_zf0TS;     //! z-coord. of first hit - at start of helix+svt
  TH2F     *m_glb_rzf0;      //! transverse & longitudinal helix center of first hit - helix hit
  TH2F     *m_glb_rzl0;      //! transverse & longitudinal helix center of last hit - helix hit
  TH2F     *m_glb_rzf0TS;    //! transverse & longitudinal helix center of first hit - helix hit+svt
  TH2F     *m_glb_rzl0TS;    //! transverse & longitudinal helix center of last hit - helix hit+svt
  TH1F     *m_glb_radfT;     //! radial (xy) coordinate of first hit, tpc
  TH1F     *m_glb_radfTS;    //! radial (xy) coordinate of first hit, tpc+svt
  TH2F     *m_glb_phifT;     //! phi dist. of first point on trk, tpc
  TH1F     *m_glb_phifTS;    //! phi dist. of first point on trk, tpc+svt
  TH1F     *m_lengthTS;      //! length of track, tpc+svt
  TH1F     *m_chisq0TS;      //! chi square [0], tpc+svt
  TH2F     *m_glb_impactTS;  //! log impact parameter from primary vertex, tpc+svt
  TH2F     *m_glb_simpactTS; //! signed impact parameter from primary vertex, tpc+svt
  TH2F     *m_glb_impactTTS; //! log impact parameter from primary vertex, tpc,tpc+svt
  TH2F     *m_glb_impactF;   //! log impact parameter from primary vertex, ftpc
  TH1F     *m_glb_impactrTS; //! impact parameter from primary vertex, tpc+svt
  TH2F     *m_glb_impactrTTS;//! impact parameter from primary vertex, tpc,tpc+svt
  TH2F     *m_glb_impactrF;  //! impact parameter from primary vertex, ftpc
  TH2F     *m_glb_radfF;     //! radial (xy) coordinate of first hit, ftpc
  TH1F     *m_glb_radfFE;    //! radial (xy) coordinate of first hit, ftpc east
  TH1F     *m_glb_radfFW;    //! radial (xy) coordinate of first hit, ftpc west
  TH1F     *m_psiT;          //! psi reconstructed, tpc
  TH1F     *m_psiTS;         //! psi reconstructed, tpc+svt
  TH2F     *m_psiTTS;        //! psi reconstructed, tpc,tpc+svt
  TH2F     *m_psiF;          //! psi reconstructed, ftpc
  TH1F     *m_psiFE;         //! psi reconstructed, ftpc east
  TH1F     *m_psiFW;         //! psi reconstructed, ftpc west
  TH1F     *m_tanlT;         //! tan(dip) =pz/pt at start, tpc
  TH1F     *m_tanlTS;        //! tan(dip) =pz/pt at start, tpc+svt
  TH1F     *m_glb_thetaT;    //! theta - tpc
  TH1F     *m_glb_thetaTS;   //! theta - tpc+svt
  TH1F     *m_etaT;          //! eta, tpc
  TH1F     *m_etaTS;         //! eta, tpc+svt
  TH2F     *m_etaTTS;        //! eta, tpc,tpc+svt
  TH2F     *m_etaF;          //! eta, ftpc
  TH1F     *m_etaFE;         //! eta, ftpc east
  TH1F     *m_etaFW;         //! eta, ftpc west
  TH1F     *m_momT;          //! momentum, tpc
  TH1F     *m_momTS;         //! momentum, tpc+svt
  TH2F     *m_momF;          //! momentum, ftpc
  TH1F     *m_momFE;         //! momentum, ftpc east
  TH1F     *m_momFW;         //! momentum, ftpc west
  TH1F     *m_pTT;           //! pT, tpc
  TH1F     *m_pTTS;          //! pT, tpc+svt
  TH2F     *m_pTTTS;         //! pT, tpc,tpc+svt
  TH2F     *m_pTF;           //! pT, ftpc
  TH1F     *m_pTFE;          //! pT, ftpc east
  TH1F     *m_pTFW;          //! pT, ftpc west
  TH1F     *m_lengthT;       //! length of track, tpc
  TH2F     *m_lengthF;       //! length of track, ftpc
  TH1F     *m_lengthFE;      //! length of track, ftpc east
  TH1F     *m_lengthFW;      //! length of track, ftpc west
  TH1F     *m_chisq0T;       //! chi square [0], tpc
  TH2F     *m_chisq0TTS;     //! chi square [0], tpc,svt
  TH2F     *m_chisq0F;       //! chi square [0], ftpc
  TH1F     *m_chisq0FE;      //! chi square [0], ftpc east
  TH1F     *m_chisq0FW;      //! chi square [0], ftpc west
  TH2F     *m_chisq1F;       //! chi square [1], ftpc
  TH1F     *m_chisq1FE;      //! chi square [1], ftpc east
  TH1F     *m_chisq1FW;      //! chi square [1], ftpc west
  TH2F     *m_glb_impactT;   //! log impact parameter from primary vertex, tpc
  TH2F     *m_glb_simpactT;  //! signed impact parameter from primary vertex, tpc
  TH1F     *m_glb_impactrT;  //! impact parameter from primary vertex, tpc

  TH2F     *m_glb_f0;          //! overlayed hist of first point - helix point
  TH2F     *m_glb_f0TS;        //! overlayed hist of first point - helix point
  TH2F     *m_pT_eta_recT;     //! pT versus eta, tpc
  TH2F     *m_pT_eta_recTS;    //! pT versus eta, tpc+svt
  TH2F     *m_pT_eta_recFE;    //! pT versus eta, ftpcE
  TH2F     *m_pT_eta_recFW;    //! pT versus eta, ftpcW
  TH2F     *m_globtrk_xf_yfTE; //! Y vs X of first hit on trk, tpc east
  TH2F     *m_globtrk_xf_yfTW; //! Y vs X of first hit on trk, tpc west
  TH2F     *m_globtrk_xf_yfTS; //! Y vs X of first hit on trk, tpc+svt
  TH2F     *m_globtrk_xf_yfFE; //! Y vs X of first hit on trk, ftpc east
  TH2F     *m_globtrk_xf_yfFW; //! Y vs X of first hit on trk, ftpc west
  TH2F     *m_globtrk_padtimeFE;    //! padlength vs timelength of hits on track, ftpcE
  TH2F     *m_globtrk_padtimeFW;    //! padlength vs timelength of hits on track, ftpcW
  TH2F     *m_tanl_zfT;        //! tanl(dip angle) vs zfirst-zvtx, tpc
  TH2F     *m_tanl_zfTS;       //! tanl(dip angle) vs zfirst-zvtx, tpc+svt 
  TH2F     *m_mom_trklengthT;  //! mom vs. trk length, tpc
  TH2F     *m_mom_trklengthTS; //! mom vs. trk length, tpc+svt
  TH2F     *m_eta_trklengthT;  //! trk length vs. eta, tpc 
  TH2F     *m_eta_trklengthTS; //! trk length vs. eta, tpc+svt
  TH2F     *m_eta_trklengthFE; //! trk length vs. eta, ftpc east
  TH2F     *m_eta_trklengthFW; //! trk length vs. eta, ftpc west
  TH2F     *m_fpoint_lengthT;  //! num fit points vs length, tpc
  TH2F     *m_fpoint_lengthTS; //! num fit points vs length, tpc+svt
  TH2F     *m_fpoint_lengthTTS;//! num fit points vs length, tpc,tpc+svt
  TH2F     *m_npoint_lengthT;  //! tot num points vs length, tpc
  TH2F     *m_npoint_lengthTS; //! tot num points vs length, tpc+svt
  TH2F     *m_npoint_lengthFE; //! tot num points vs length, ftpc east
  TH2F     *m_npoint_lengthFW; //! tot num points vs length, ftpc west
  TH2F     *m_chisq0_momT;     //! chisq0 vs momentum, tpc
  TH2F     *m_chisq0_momTS;    //! chisq0 vs momentum, tpc+svt
  TH2F     *m_chisq0_etaT;     //! chisq0 vs eta, tpc
  TH2F     *m_chisq0_etaTS;    //! chisq0 vs eta, tpc+svt
  TH2F     *m_chisq0_dipT;     //! chisq0 vs dip angle, tpc
  TH2F     *m_chisq0_dipTS;    //! chisq0 vs dip angle, tpc+svt
  TH2F     *m_chisq0_zfT;      //! chisq0 vs zfirst, tpc 
  TH2F     *m_chisq0_zfTS;     //! chisq0 vs zfirst, tpc+svt
  TH2F     *m_chisq0_phiT;     //! chisq0 vs phi, tpc
  TH2F     *m_chisq0_phiTS;    //! chisq0 vs phi, tpc+svt
  TH2F     *m_psi_phiT;        //! psi vs phi, tpc
  TH2F     *m_psi_phiTS;       //! psi vs phi, tpc+svt

// for method MakeDE - from table dst_dedx
  TH1F     *m_ndedxr;        //! number of tracks with dedx info

  TH1F     *m_ndedxT;        //! number of point to find dE/dx, tpc
  TH1F     *m_dedx0T;        //! dE/dx [0], tpc
  TH1F     *m_dedx1T;        //! dE/dx [1], tpc
  TH1F     *m_dedxTTS;       //! <dE/dx>/(Bichsel <dE/dx>), tpc,tpc+svt
  // east and west on same plot
  TH2F     *m_ndedxF;        //! number of point to find dE/dx, ftpc
  TH2F     *m_dedx0F;        //! dE/dx [0], ftpc
  // east and west on separate plots
  TH1F     *m_ndedxFE;       //! number of point to find dE/dx, ftpcE
  TH1F     *m_dedx0FE;       //! dE/dx [0], ftpcE

  TH1F     *m_ndedxFW;       //! number of point to find dE/dx, ftpcW
  TH1F     *m_dedx0FW;       //! dE/dx [0], ftpcW

// for method MakeHistPrim - from table primtrk
  TH1F     *m_primtrk_tot;    //! # tracks in table
  TH1F     *m_primtrk_tot_sm; //! # tracks in table, small range
  TH1F     *m_primtrk_good;   //! # tracks in table with iflag>0
  TH1F     *m_primtrk_good_sm;//! # tracks in table with iflag>0, small range
  TH1F     *m_primtrk_goodTTS;//! # tracks in table with iflag>0, tpc,svt
  TH2F     *m_primtrk_goodF;  //! # tracks in table with iflag>0, ftpc
  TH1F     *m_primglob_good;  //! # trks w/ iflag>0, ratio prim/glob, all detectors
  TH1F     *m_primglob_fit;   //! # fit points ratio prim/glob, all detectors
  TH1F     *m_primtrk_iflag;  //! iflag value
  TH1F     *m_pdet_id;        //! detector id of track
  TH2F     *m_primtrk_meanptTTS;  //! <pT>, tpc, tpc+svt
  TH2F     *m_primtrk_meanptF;    //! <pT>, ftpc
  TH2F     *m_primtrk_meanetaTTS; //! <eta>, tpc, tpc+svt
  TH2F     *m_primtrk_meanetaF;   //! <eta>, ftpc

  TH1F     *m_ppointT;        //! number of points on the track - tpc
  TH1F     *m_ppointTS;       //! number of points on the track - tpc+svt
  TH2F     *m_ppointF;        //! number of points on the track - ftpc
  TH1F     *m_ppointFE;       //! number of points on the track - ftpc east
  TH1F     *m_ppointFW;       //! number of points on the track - ftpc west
  TH1F     *m_pmax_pointT;    //! number of max possible track points - tpc
  TH1F     *m_pmax_pointTS;   //! number of max possible track points - tpc+svt
  TH2F     *m_pmax_pointF;    //! number of max possible track points - ftpc
  TH1F     *m_pmax_pointFE;   //! number of max possible track points - ftpc east
  TH1F     *m_pmax_pointFW;   //! number of max possible track points - ftpc west
  TH1F     *m_pfit_pointT;    //! number of track points used for fitting - tpc
  TH1F     *m_pfit_pointTS;   //! number of track points used for fitting - tpc+svt
  TH2F     *m_prim_ratioF;    //! ratio of n fit pnts over tot n pnts - ftpc
  TH1F     *m_prim_ratioFE;   //! ratio of n fit pnts over tot n pnts - ftpc east
  TH1F     *m_prim_ratioFW;   //! ratio of n fit pnts over tot n pnts - ftpc west
  TH1F     *m_prim_ratiomT;   //! ratio of n fit pnts over max n pnts - tpc
  TH1F     *m_prim_ratiomTS;  //! ratio of n fit pnts over max n pnts - tpc+svt
  TH2F     *m_prim_ratiomF;   //! ratio of n fit pnts over max n pnts - ftpc
  TH1F     *m_prim_ratiomFE;  //! ratio of n fit pnts over max n pnts - ftpc east
  TH1F     *m_prim_ratiomFW;  //! ratio of n fit pnts over max n pnts - ftpc west
  TH1F     *m_prim_chargeT;   //! particle charge in units of |e| - tpc
  TH1F     *m_prim_chargeTS;  //! particle charge in units of |e| - tpc+svt
  TH2F     *m_prim_chargeF;   //! particle charge in units of |e| - ftpc
  TH1F     *m_prim_chargeFE;  //! particle charge in units of |e| - ftpc east
  TH1F     *m_prim_chargeFW;  //! particle charge in units of |e| - ftpc west
  TH1F     *m_prim_r0T;       //! radius at start (cm), tpc 
  TH1F     *m_prim_r0TS;      //! radius at start (cm), tpc+svt
  TH1F     *m_prim_phi0T;     //! azimuthal angle at start (deg), tpc
  TH1F     *m_prim_phi0TS;    //! azimuthal angle at start (deg), tpc+svt
  TH1F     *m_prim_z0T;       //! z-coord at start (cm), tpc 
  TH1F     *m_prim_z0TS;      //! z-coord at start (cm), tpc+svt
  TH1F     *m_prim_curvT;     //! curvature (1/cm), tpc
  TH1F     *m_prim_curvTS;    //! curvature (1/cm), tpc+svt
  TH1F     *m_prim_xfT;       //! x-coord. of first hit on trk, tpc
  TH1F     *m_prim_xfTS;      //! x-coord. of first hit on trk, tpc+svt
  TH2F     *m_prim_xfF;       //! x-coord. of first hit on trk, ftpc
  TH1F     *m_prim_xfFE;      //! x-coord. of first hit on trk, ftpc east
  TH1F     *m_prim_xfFW;      //! x-coord. of first hit on trk, ftpc west
  TH1F     *m_prim_yfT;       //! y-coord. of first hit on trk, tpc
  TH1F     *m_prim_yfTS;      //! y-coord. of first hit on trk, tpc+svt
  TH2F     *m_prim_yfF;       //! y-coord. of first hit on trk, ftpc
  TH1F     *m_prim_yfFE;      //! y-coord. of first hit on trk, ftpc east
  TH1F     *m_prim_yfFW;      //! y-coord. of first hit on trk, ftpc west
  TH1F     *m_prim_zfT;       //! z-coord. of first hit on trk, tpc
  TH1F     *m_prim_zfTS;      //! z-coord. of first hit on trk, tpc+svt
  TH2F     *m_prim_zfF;       //! z-coord. of first hit on trk, ftpc
  TH1F     *m_prim_zfFE;      //! z-coord. of first hit on trk, ftpc east
  TH1F     *m_prim_zfFW;      //! z-coord. of first hit on trk, ftpc west
  TH1F     *m_prim_xf0;       //! x-coord. of first hit - at start of helix
  TH1F     *m_prim_xf0TS;     //! x-coord. of first hit - at start of helix+svt
  TH1F     *m_prim_yf0;       //! y-coord. of first hit - at start of helix
  TH1F     *m_prim_yf0TS;     //! y-coord. of first hit - at start of helix+svt
  TH1F     *m_prim_zf0;       //! z-coord. of first hit - at start of helix
  TH1F     *m_prim_zf0TS;     //! z-coord. of first hit - at start of helix+svt
  TH2F     *m_prim_rzf0;      //! transverse & longitudinal helix center of first hit - helix hit
  TH2F     *m_prim_rzl0;      //! transverse & longitudinal helix center of last hit - helix hit
  TH2F     *m_prim_rzf0TS;    //! transverse & longitudinal helix center of first hit - helix hit+svt
  TH2F     *m_prim_rzl0TS;    //! transverse & longitudinal helix center of last hit - helix hit+svt
  TH1F     *m_prim_radfT;     //! radial (xy) coordinate of first hit, tpc
  TH1F     *m_prim_radfTS;    //! radial (xy) coordinate of first hit, tpc+svt
  TH2F     *m_prim_radfF;     //! radial (xy) coordinate of first hit, ftpc
  TH1F     *m_prim_radfFE;    //! radial (xy) coordinate of first hit, ftpc east
  TH1F     *m_prim_radfFW;    //! radial (xy) coordinate of first hit, ftpc west
  TH1F     *m_ppsiT;          //! psi reconstructed, tpc
  TH1F     *m_ppsiTS;         //! psi reconstructed, tpc+svt
  TH2F     *m_ppsiTTS;        //! psi reconstructed, tpc,tpc+svt
  TH2F     *m_ppsiF;          //! psi reconstructed, ftpc
  TH1F     *m_ppsiFE;         //! psi reconstructed, ftpc east
  TH1F     *m_ppsiFW;         //! psi reconstructed, ftpc west
  TH1F     *m_ptanlT;         //! tan(dip) =pz/pt at start, tpc
  TH1F     *m_ptanlTS;        //! tan(dip) =pz/pt at start, tpc+svt
  TH1F     *m_prim_thetaT;    //! theta - tpc
  TH1F     *m_prim_thetaTS;   //! theta - tpc+svt
  TH1F     *m_petaT;          //! eta, tpc
  TH1F     *m_petaTS;         //! eta, tpc+svt
  TH2F     *m_petaTTS;        //! eta, tpc,tpc+svt
  TH2F     *m_petaF;          //! eta, ftpc
  TH1F     *m_petaFE;         //! eta, ftpc east
  TH1F     *m_petaFW;         //! eta, ftpc west
  TH1F     *m_pmomT;          //! momentum, tpc
  TH1F     *m_pmomTS;         //! momentum, tpc+svt
  TH2F     *m_pmomF;          //! momentum, ftpc
  TH1F     *m_pmomFE;         //! momentum, ftpc east
  TH1F     *m_pmomFW;         //! momentum, ftpc west
  TH1F     *m_ppTT;           //! pT, tpc
  TH1F     *m_ppTTS;          //! pT, tpc+svt
  TH2F     *m_ppTTTS;         //! pT, tpc,tpc+svt
  TH2F     *m_ppTF;           //! pT, ftpc
  TH1F     *m_ppTFE;          //! pT, ftpc east
  TH1F     *m_ppTFW;          //! pT, ftpc west
  TH1F     *m_plengthT;       //! length of track, tpc
  TH1F     *m_plengthTS;      //! length of track, tpc+svt
  TH2F     *m_plengthF;       //! length of track, ftpc
  TH1F     *m_plengthFE;      //! length of track, ftpc east
  TH1F     *m_plengthFW;      //! length of track, ftpc west
  TH1F     *m_pchisq0T;       //! chi square [0], tpc
  TH1F     *m_pchisq0TS;      //! chi square [0], tpc+svt
  TH2F     *m_pchisq0TTS;     //! chi square [0], tpc,tpc+svt
  TH2F     *m_pchisq0F;       //! chi square [0], ftpc
  TH1F     *m_pchisq0FE;      //! chi square [0], ftpc east
  TH1F     *m_pchisq0FW;      //! chi square [0], ftpc west
  TH2F     *m_pchisq1F;       //! chi square [1], ftpc
  TH1F     *m_pchisq1FE;      //! chi square [1], ftpc east
  TH1F     *m_pchisq1FW;      //! chi square [1], ftpc west

  TH2F     *m_prim_f0;          //! overlayed hist of first point - helix point
  TH2F     *m_prim_f0TS;        //! overlayed hist of first point - helix point
  TH2F     *m_ppT_eta_recT;     //! pT versus eta, tpc
  TH2F     *m_ppT_eta_recTS;    //! pT versus eta, tpc+svt
  TH2F     *m_ppT_eta_recFE;    //! pT versus eta, ftpcE
  TH2F     *m_ppT_eta_recFW;    //! pT versus eta, ftpcW
  TH2F     *m_primtrk_xf_yfTE;  //! Y vs X of first hit on trk, tpc east
  TH2F     *m_primtrk_xf_yfTW;  //! Y vs X of first hit on trk, tpc west
  TH2F     *m_primtrk_xf_yfTS;  //! Y vs X of first hit on trk, tpc+svt
  TH2F     *m_primtrk_xf_yfFE;  //! Y vs X of first hit on trk, ftpc east
  TH2F     *m_primtrk_xf_yfFW;  //! Y vs X of first hit on trk, ftpc west
  TH2F     *m_ptanl_zfT;        //! tanl(dip angle) vs zfirst-zvtx, tpc
  TH2F     *m_ptanl_zfTS;       //! tanl(dip angle) vs zfirst-zvtx, tpc+svt
  TH2F     *m_pmom_trklengthT;  //! mom vs. trk length, tpc
  TH2F     *m_pmom_trklengthTS; //! mom vs. trk length, tpc+svt
  TH2F     *m_peta_trklengthT;  //! trk length vs. eta, tpc
  TH2F     *m_peta_trklengthTS; //! trk length vs. eta, tpc+svt
  TH2F     *m_peta_trklengthFE; //! trk length vs. eta, ftpc east
  TH2F     *m_peta_trklengthFW; //! trk length vs. eta, ftpc west
  TH2F     *m_pfpoint_lengthT;  //! num fit points vs length, tpc
  TH2F     *m_pfpoint_lengthTS; //! num fit points vs length, tpc+svt
  TH2F     *m_pfpoint_lengthTTS;//! num fit points vs length, tpc
  TH2F     *m_pnpoint_lengthT;  //! tot num points vs length, tpc
  TH2F     *m_pnpoint_lengthTS; //! tot num points vs length, tpc+svt
  TH2F     *m_pnpoint_lengthFE; //! tot num points vs length, ftpc east
  TH2F     *m_pnpoint_lengthFW; //! tot num points vs length, ftpc west
  TH2F     *m_pchisq0_momT;     //! chisq0 vs momentum, tpc
  TH2F     *m_pchisq0_momTS;    //! chisq0 vs momentum, tpc+svt
  TH2F     *m_pchisq0_etaT;     //! chisq0 vs eta, tpc
  TH2F     *m_pchisq0_etaTS;    //! chisq0 vs eta, tpc+svt
  TH2F     *m_pchisq0_dipT;     //! chisq0 vs dip angle, tpc
  TH2F     *m_pchisq0_dipTS;    //! chisq0 vs dip angle, tpc+svt
  TH2F     *m_pchisq0_zfT;      //! chisq0 vs zfirst, tpc 
  TH2F     *m_pchisq0_zfTS;     //! chisq0 vs zfirst, tpc+svt
  TH2F     *m_ppsi_phiT;        //! psi vs phi, tpc
  TH2F     *m_ppsi_phiTS;       //! psi vs phi, tpc+svt

// for MakeHistPID - from tables primtrk & dst_dedx 
  TH2F     *m_p_dedx_rec;   //! dedx vs p

// for method MakeHistVertex - from table dst_vertex
  TH1F     *m_v_num;   //! number of vertices
  TH1F     *m_v_num_sm;//! number of vertices,small range
  TH1F     *m_v_vtxid; //! vertex type
  TH1F     *m_v_x;     //! vertex coordinates in
  TH1F     *m_v_y;     //!  STAR reference 
  TH1F     *m_v_z;     //!   system
  TH1F     *m_v_pchi2; //! chisq per dof of vertex fit
  TH1F     *m_v_r;     //! radius to vertex

  TH1F     *m_pv_vtxid; //! row1-vertex type
  TH1F     *m_pv_x;     //! row1-vertex coordinates in
  TH1F     *m_pv_y;     //!  STAR reference 
  TH1F     *m_pv_z;     //!   system
  TH2F     *m_pv_xy;    //! x versus y
  TH1F     *m_pv_pchi2; //! row1-chisq per dof of vertex fit
  TH1F     *m_pv_r;     //! radius to primary vertex

  TH1F     *m_vtx_phi_dist;  //! azimuthal distribution of V0s relative to primVtx
  TH1F     *m_vtx_r_dist;  //! radial distribution of V0s relative to primVtx
  TH1F     *m_vtx_z_dist;  //! z distribution of V0s relative to primVtx

  // from table dst_v0_vertex
  TH1F     *m_v0;            //! # v0 vertices
  TH1F     *m_ev0_lama_hist; //! Lambda mass
  TH1F     *m_ev0_k0ma_hist; //! K0 mass
  
  // from table dst_xi_vertex
  TH1F     *m_xi_tot;     //! number of xi vertices
  TH1F     *m_xi_ma_hist; //!  xi Mass

  // from kinkVertex
  TH1F     *m_kink_tot;   //! number of kinks

  // for FTPC vertex
  TH2F     *m_vtx_FtpcEastTpc_xy; //! FtpcEast prim vtx - TPC prim vtx, x vs y
  TH1F     *m_vtx_FtpcEastTpc_z;  //! FtpcEast prim vtx z - TPC prim vtx z
  TH2F     *m_vtx_FtpcWestTpc_xy; //! FtpcWest prim vtx - TPC prim vtx, x vs y
  TH1F     *m_vtx_FtpcWestTpc_z;  //! FtpcWest prim vtx z - TPC prim vtx z
  
  
// for method MakeHistPoint
  TH1F     *m_z_hits;      //! z dist. of hits, tpc
  TH1F     *m_pnt_zS;      //! z dist. of hits, svt
  TH2F     *m_pnt_timeT;   //! time dist. of hits, tpc
  TH2F     *m_pnt_xyS;     //! xy dist. of hits, svt
  TH2F     *m_pnt_rpTW;    //! rphi dist. of hits, tpcW
  TH2F     *m_pnt_rpTE;    //! rphi dist. of hits, tpcE
  TH2F     *m_pnt_phiT;    //! phi dist. of hits, tpc
  TH1F     *m_pnt_phiS;    //! phi dist. of hits, svt
  TH2F     *m_pnt_padrowT; //! padrow dist. of hits, tpc
  TH1F     *m_pnt_barrelS; //! barrel dist. of hits, svt
  TH2F     *m_pnt_xyFE;    //! xy dist. of hits, ftpcE
  TH2F     *m_pnt_xyFW;    //! xy dist. of hits, ftpcW
  TH2F     *m_pnt_padtimeFE;    //! padlength vs timelength of hits, ftpcE
  TH2F     *m_pnt_padtimeFW;    //! padlength vs timelength of hits, ftpcW
  TH2F     *m_pnt_planeF;  //! plane dist. of hits, ftpc
  TH1F     *m_pnt_tot;     //! number of hits total
  TH1F     *m_pnt_tot_med; //! number of hits total, med range
  TH1F     *m_pnt_tot_sm;  //! number of hits total, small range
  TH1F     *m_pnt_id;      //! detector ID of the hit

  TH1F     *m_pnt_tpc;   //! number of hits tpc
  TH2F     *m_pnt_ftpc;    //! number of hits ftpc
  TH1F     *m_pnt_ftpcE;   //! number of hits ftpcE
  TH1F     *m_pnt_ftpcW;   //! number of hits ftpcW

  // for method MakeHistEval
  TH1F *m_geant_reco_pvtx_x;  //! prim vtx x, diff geant - reco
  TH1F *m_geant_reco_pvtx_y;  //! prim vtx y, diff geant - reco
  TH1F *m_geant_reco_pvtx_z;  //! prim vtx z, diff geant - reco
  TH2F *m_geant_reco_vtx_z_z; //! prim vtx z, diff geant - reco vs reco z

// for method MakeHistEMC - from EMC (StEvent only)
  // Hists for EMC hits
 
  TH2F *m_emc_nhit;            //!
  TH2F *m_emc_etot;            //!
  TH2F *m_emc_hits[4];         //!
  TH2F *m_emc_energy2D[4];     //!
  TH1F *m_emc_adc[4];          //!
  TH1F *m_emc_energy[4];       //!
 
  // Hists for EMC cluster finder
  TH2F *m_emc_ncl;             //!
  TH2F *m_emc_etotCl;          //!
  TH2F *m_emc_sig_e;           //!
  TH2F *m_emc_sig_p;           //!
  TH2F *m_emc_cl[4];           //!
  TH2F *m_emc_energyCl[4];     //!
  TH1F *m_emc_HitsInCl[4];     //!
  TH1F *m_emc_EnergyCl[4];     //!
  TH1F *m_emc_EtaInCl[4];      //!
  TH1F *m_emc_PhiInCl[4];      //!
 
  // Hists for Emc points
  TH1F *m_emc_point_energy[4]; //! Point Energy spectra
  TH1F *m_emc_point_eta[4];    //! Point Eta spectra
  TH1F *m_emc_point_phi[4];    //! Point Phi spectra
  TH1F *m_emc_point_sigeta[4]; //! Point SigmaEta spectra
  TH1F *m_emc_point_sigphi[4]; //! Point SigmaPhi spectra
  TH1F *m_emc_point_deleta[4]; //! Point DeltaEta spectra
  TH1F *m_emc_point_delphi[4]; //! Point DeltaPhi spectra
  TH1F *m_emc_point_trmom[4];  //! Point TrMom spectra
  TH1F *m_emc_points[4];       //! Emc Point multiplicity
  TH1F *m_emc_point_flag;      //! Point Flag spectra

  // Hists for BBC
  TH2F* m_bbc_adc[4];          //!
  TH2F* m_bbc_tdc[4];          //!

  // Hists for FPD
  TH2F* m_fpd_top[2];          //!
  TH2F* m_fpd_bottom[2];       //!
  TH2F* m_fpd_south[2];        //!
  TH2F* m_fpd_north[2];        //!
  TH1F* m_fpd_sums[8];         //!

  // Hists for PMD
  TH2F* m_pmd_sm_adc[12];      //!
  TH2F* m_pmd_sm_hit[12];      //!
  TH2F* m_pmd_chain_hit[24];   //!
  TH2F* m_pmd_chain_adc[24];   //!
  TH2F* m_pmd_total_hit;       //!
  TH2F* m_pmd_total_adc;       //!
  TH2F* m_cpv_total_hit;       //!
  TH2F* m_cpv_total_adc;       //!

  // Hists for SVT
  TH1F* m_pnt_svt;             //! number of hits svt
  TH2F* m_pnt_svtLaser;        //! laser spots in svt
  TH3F* m_pnt_svtLaserDiff;    //! diff of laser spots in svt
  TH2F* m_svt_loc;             //! SVT drift

  // Hists for SSD
  TH1F* m_pnt_ssd;             //! number of hits ssd
  TH1F* m_pnt_phiSSD;          //!
  TH2F* m_pnt_lwSSD;           //!
  TH1F* m_glb_ssd_phi;         //!
  TH1F* m_prim_ssd_phi;        //!
  TH2F* m_pnt_sizeSSD;         //!
  TH1F* m_pnt_eSSD;            //!

  // Hists for SST
  TH1F* m_pnt_sst;             //! number of hits sst
  TH1F* m_pnt_phiSST;          //!
  TH2F* m_pnt_lwSST;           //!
  TH1F* m_glb_sst_phi;         //!
  TH1F* m_prim_sst_phi;        //!
  TH2F* m_pnt_sizeSST;         //!
  TH1F* m_pnt_eSST;            //!

  // Hists for HFT
  // correlation plots: hits in HFT subsystems
  TH2F* m_nhit_Pxl_Ist;         //!     # of hits in PIXEL vs hits in IST
  TH2F* m_nhit_Pxl_Sst;         //!     # of hits in PIXEL vs hits in SST
  TH2F* m_nhit_Ist_Sst;         //!     # of hits in ITS vs hits in ITS
  TH1F* m_global_hft_hit;       //! HFT hits per global track
  TH1F* m_primary_hft_hit;      //!     HFT hits per primary track

  // Hists for PIXEL
  TH2F* m_pxl_hit_phi_z_Pxl1;           //!     # of hits vs phi vs z position, inner PIXEL layer
  TH2F* m_pxl_hit_phi_z_Pxl2;           //!     # of hits vs phi vs z position, outer PIXEL layer
  TH1F* m_pxl_hit_ladder;               //!     # of hits vs x ladder (integrated over ladder, per event)
  TH2F* m_pxl_hit_sector_sensor_Pxl1;   //!     # of hits vs sector vs sensor, inner PIXEL layer
  TH2F* m_pxl_hit_sector_sensor_Pxl2;   //!     # of hits vs sector vs sensor, outer PIXEL layer
  TH2F* m_pxl_nhit_Pxl1_tpc_mult;       //!     # of hits in inner PIXEL layer vs TPC track multiplicity
  TH2F* m_pxl_nhit_Pxl2_tpc_mult;       //!     # of hits in outer PIXEL layer vs TPC track multiplicity
  TH2F* m_pxl_nhit_Pxl1_tof_mult;       //!     # of hits in inner PIXEL layer vs TOF track multiplicity
  TH2F* m_pxl_nhit_Pxl2_tof_mult;       //!     # of hits in outer PIXEL layer vs TOF track multiplicity
  TH2F* m_pxl_nhit_Pxl1_Pxl2;           //!     # of hits in inner vs outer PIXEL layer
  TH1F* m_global_pxl_hit;               //!     # of PIXEL hits per global track
  TH1F* m_primary_pxl_hit;              //!     # of PIXEL hits per primary track

  // Hists for IST
  TH2F* m_ist_hit_phi_z;        //!     # of hits vs phi vs z position
  TH1F* m_ist_hit_ladder;       //!     # of hits vs x ladder (integrated over ladder, per event)
  TH2F* m_ist_hit_ladder_sensor;//!     # of hits vs ladder vs sensor
  TH2F* m_ist_nhit_tpc_mult;    //!     # of hits in IST vs TPC track multiplicity
  TH2F* m_ist_nhit_tof_mult;    //!     # of hits in IST vs TOF track multiplicity
  TH1F* m_global_ist_hit;       //!     # of IST hits per global track
  TH1F* m_primary_ist_hit;      //!     # of IST hits per primary track

  // X. Dong
  // Hists for TOF
  TH2F* m_tof_hit_tray;        //! # of hits vs tray # (int over modules)
  TH2F* m_tof_hit_module;      //! # of hits vs module # (int over trays, east side use -32 - -1)
  TH2F* m_tof_match_tray;      //! # of matched hits vs tray #
  TH2F* m_tof_match_module;    //! # of matched hits vs module #
  TH2F* m_tof_vpd_hit;         //! # of tof hits vs # of vpd hits
  TH2F* m_tof_vtx_z;           //! vertex z from vpd vs verex z from TPC
  TH2F* m_tof_PID;             //! TOF PID:  1/beta vs p

  TH1F *m_MtdNHits;            //!
  TH2F *m_MtdHitMap;           //!
  TH1F *m_MtdNMatchHits;       //!
  TH2F *m_MtdMatchHitMap;      //!

  // Hists for EPD
  TH2F* m_epd_adc[24];         //!
  TH2F* m_epd_tac[24];         //!

  // Hists for iTPC
  TH2F *m_TPC_ch_nrow[24];        //!
  TH2F *m_TPC_ch_time_inner[24];  //!
  TH2F *m_TPC_ch_time_outer[24];  //!
  TH2F *m_TPC_adc_sec_inner;      //!
  TH2F *m_TPC_adc_sec_outer;      //!


  // ********************** Members For Internal Use *************************
 protected:

  TString QAHistType; // character string to prepend to each hist name/title
  bool silHists;
  bool ftpHists;
  Int_t histsSet;

  virtual void   BookHistGlob();
  virtual void   BookHistDE();
  virtual void   BookHistPrim();
  virtual void   BookHistPID();
  virtual void   BookHistVertex();
  virtual void   BookHistPoint();
  virtual void   BookHistEMC();
  virtual void   BookHistEval();
  virtual void   BookHistBBC();
  virtual void   BookHistFPD();
  virtual void   BookHistPMD();
  virtual void   BookHistTOF();
  virtual void   BookHistMTD();
  virtual void   BookHistPXL();
  virtual void   BookHistHFT();
  virtual void   BookHistIST();
  virtual void   BookHistEPD();
  virtual void   BookHistiTPC();

  ClassDef(StQABookHist,0)
};

#endif
    







