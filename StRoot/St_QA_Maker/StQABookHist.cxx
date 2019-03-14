// $Id: StQABookHist.cxx,v 2.80 2019/03/14 02:31:53 genevb Exp $
// $Log: StQABookHist.cxx,v $
// Revision 2.80  2019/03/14 02:31:53  genevb
// Introduce iTPC plots
//
// Revision 2.79  2018/07/03 21:33:34  genevb
// Introduce EPD (code provided by J. Ewigleben)
//
// Revision 2.78  2018/05/02 21:07:40  genevb
// Initial accomodation for iTPC
//
// Revision 2.77  2017/02/25 03:24:30  genevb
// Run 17: remove HFT
//
// Revision 2.76  2016/05/25 03:59:44  genevb
// fix uninit members
//
// Revision 2.75  2016/05/13 22:04:49  genevb
// Address coverity findings: uninit vars, dead code, one PMD error, and one TOF error
//
// Revision 2.74  2016/02/19 03:52:15  genevb
// Expand track detector ID histograms
//
// Revision 2.73  2015/07/17 19:09:03  genevb
// SSD copied for SST, and HFT histogams use SST now too
//
// Revision 2.72  2015/04/02 19:53:47  genevb
// TPC dE/dx changes: Bethe-Bloch => Bichsel, and tighter cuts against pile-up tracks
//
// Revision 2.71  2015/01/16 21:08:28  genevb
// Initial versions of HFT histograms
//
// Revision 2.70  2014/07/22 20:39:28  genevb
// Add MTD to Offline QA
//
// Revision 2.69  2013/03/12 03:06:02  genevb
// Add FMS/FPD histograms for Run 13+
//
// Revision 2.68  2012/03/05 03:42:32  genevb
// Remove TPC XY dist, add TPC RPhi charge
//
// Revision 2.67  2012/02/09 03:01:05  genevb
// No FTPC histograms for Run 12+
//
// Revision 2.66  2012/02/08 22:10:35  genevb
// Updates for Run 12
//
// Revision 2.65  2011/05/31 21:35:49  genevb
// TPC request: add time bucket distribution of hits
//
// Revision 2.64  2009/03/19 01:08:08  genevb
// Show both xy and rphi TPC hit hists
//
// Revision 2.63  2009/03/13 19:27:24  genevb
// Now draw TPC xy hits in polar coords
//
// Revision 2.62  2009/03/09 16:34:54  genevb
// Tweak a couple histogram ranges
//
// Revision 2.61  2007/12/10 19:58:20  genevb
// Use log10 for number of secondary vertices
//
// Revision 2.60  2007/12/03 16:51:09  genevb
// Out-of-order statements for pre-run8 data
//
// Revision 2.59  2007/11/30 05:38:50  genevb
// Changes for Run8: mostly silicon removal, TOF addition
//
// Revision 2.58  2007/04/25 18:35:57  genevb
// Additional SSD hists
//
// Revision 2.57  2007/04/24 00:33:58  genevb
// SSD hists
//
// Revision 2.56  2007/04/12 20:39:48  genevb
// Cleanup (removal) of CalibVtx, Nfitpnt, Chisq1, Rich, histograms
//
// Revision 2.55  2007/04/07 04:40:30  genevb
// Remove fit pnts/tot; retitle log as log10
//
// Revision 2.54  2007/04/02 05:36:23  genevb
// PMD always in now
//
// Revision 2.53  2007/03/13 18:46:28  genevb
// Added Svt Laser Diff
//
// Revision 2.52  2007/02/26 20:45:01  genevb
// SVT drift hist
//
// Revision 2.51  2006/05/18 16:37:11  genevb
// Change FTPC hist ranges
//
// Revision 2.50  2005/02/22 19:38:39  genevb
// Do PMD hists only for year 4 and later (real data)
//
// Revision 2.49  2005/02/08 17:22:46  genevb
// PMD histo changes, handle estGlobal/ITTF tracks
//
// Revision 2.48  2005/01/27 05:28:25  genevb
// PMD changes
//
// Revision 2.47  2004/12/13 15:52:36  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.46  2004/10/04 16:40:42  genevb
// FTPC radial histos
//
// Revision 2.45  2004/04/23 23:15:29  genevb
// Added signedDCA (Impact) plots for globals
//
// Revision 2.44  2004/02/12 05:03:10  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.43  2004/01/27 20:08:14  genevb
// Modify SVT point hist
//
// Revision 2.42  2004/01/10 01:10:18  genevb
// Preparations for Year 5, added some svt plots
//
// Revision 2.41  2003/09/02 17:59:21  perev
// gcc 3.2 updates + WarnOff
//
// Revision 2.40  2003/04/19 00:17:49  genevb
// Updated for dAu/pp running
//
// Revision 2.39  2003/02/20 20:09:54  genevb
// Several changes for new trigger scheme, dAu data
//
// Revision 2.38  2003/02/19 06:38:29  genevb
// Rework trigger and mult/event class sections
//
// Revision 2.37  2003/02/15 21:59:08  genevb
// dedx lable
//
// Revision 2.36  2003/01/24 15:24:48  genevb
// Further dAu changes
//
// Revision 2.35  2003/01/23 20:53:11  genevb
// Additional dAu changes
//
// Revision 2.34  2003/01/23 04:03:18  jeromel
// Include fixed
//
// Revision 2.33  2003/01/13 19:09:43  genevb
// Minor FTPC histo changes
//
// Revision 2.32  2002/07/11 12:26:22  genevb
// Some FTPC histogram changes
//
// Revision 2.31  2002/05/29 13:54:30  genevb
// Some changes to FTPC chisq histos
//
// Revision 2.30  2002/04/23 01:59:56  genevb
// Addition of BBC/FPD histos
//
// Revision 2.29  2002/02/12 18:41:59  genevb
// Additional FTPC histograms
//
// Revision 2.28  2002/01/21 22:09:24  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.27  2001/12/28 09:19:13  genevb
// Adjustments for pp running
//
// Revision 2.26  2001/11/20 21:53:45  lansdell
// added x-y dist of hits, tpc east&west histos
//
// Revision 2.25  2001/11/13 21:17:56  lansdell
// changed limits on dedx error on mean, tpc
//
// Revision 2.24  2001/11/02 20:50:03  genevb
// Changed histogram ranges for momenta
//
// Revision 2.23  2001/10/25 20:45:12  lansdell
// changed ftpc # hits range to 25k
//
// Revision 2.22  2001/10/25 20:43:43  lansdell
// changed 2d ftpc globtrk histo ranges
//
// Revision 2.21  2001/10/10 05:00:19  genevb
// Changed ftpc point/track ranges
//
// Revision 2.20  2001/08/07 07:51:27  lansdell
// primvtx check for different multiplicities crashed for MC data, now fixed
//
// Revision 2.19  2001/08/03 23:31:59  lansdell
// added missing axis labels to 2d histos
//
// Revision 2.18  2001/08/03 20:33:55  lansdell
// added primvtx check histos for different multiplicities; separated x-y plot of first point on track, tpc into east and west histos
//
// Revision 2.17  2001/07/31 23:21:42  lansdell
// added last point, hit-helix histos
//
// Revision 2.16  2001/06/06 18:44:59  lansdell
// changed domain on some qa_shift histograms
//
// Revision 2.15  2001/05/29 23:23:06  lansdell
// removed impact param plots for FTPC from qa_shift list
//
// Revision 2.14  2001/05/25 17:20:09  lansdell
// squashed a small bug
//
// Revision 2.13  2001/05/25 16:31:21  lansdell
// more updates to qa shift histograms
//
// Revision 2.12  2001/05/24 01:48:13  lansdell
// qa_shift histograms updated
//
// Revision 2.11  2001/05/23 00:14:52  lansdell
// more changes for qa_shift histograms
//
// Revision 2.10  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.9  2001/05/02 16:10:47  lansdell
// changed some histogram limits
//
// Revision 2.8  2001/05/01 15:17:36  genevb
// Execute EMC code only if EMC libs loaded
//
// Revision 2.7  2001/04/28 22:05:13  genevb
// Added EMC histograms
//
// Revision 2.6  2001/04/26 20:45:19  lansdell
// changed some histogram ranges (TPC+SVT radius at start, impact param for primary tracks)
//
// Revision 2.5  2001/04/26 16:34:28  genevb
// Fixed some histogram ranges
//
// Revision 2.4  2001/04/25 21:35:26  genevb
// Added V0 phi distributions
//
// Revision 2.3  2001/04/24 22:53:51  lansdell
// Removed redundant radial position of first hit histograms
//
// Revision 2.2  2000/09/08 18:55:53  lansdell
// turned on FTPC primary track histograms
//
// Revision 2.1  2000/09/06 15:31:13  lansdell
// changed FTPC radial position of first hit histogram limits
//
// Revision 2.0  2000/08/25 16:02:40  genevb
// New revision: new structure, multiplicity classes
//
// 
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQABookHist class for multiplicity-based QA histograms              //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <math.h>
#include "QAH.h"
#include "StQABookHist.h"
#include "StQAMakerBase.h"
#include "StEmcUtil/others/StEmcMath.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "TROOT.h"


ClassImp(StQABookHist)
  
//_____________________________________________________________________________
StQABookHist::StQABookHist(const char* type) : QAHistType(type) {

  histsSet = StQA_Undef;
  silHists = kFALSE;
  ftpHists = kFALSE;

  int i = 0;

//  - Set all the histogram booking constants

  nxpT = 50;
  nyeta = 50;
  xminpT = 0.0;
  xmaxpT = 5.0;
//  ymineta = -2.0;
//  ymaxeta =  2.0;

  nchisq = 50;
//  nmass  = 40;
//  ntau   = 40;
  ndedx  = 50;
  npnt   = 50;
  nleng  = 50;
  npsi   = 36;
  knpsi  = 42;
  nvrt   = 100;
  knyeta = 60;
  knid   = 10;
  cnp   = 50;
  cndedx = 50;    

  kminnid  = 0.0;
  kmaxnid  = 10.0;
  minpsi   = 0.0;
  kminpsi  = -60.0;
  maxpsi   = 360.0;
  minchisq = 0.;
  maxchisq = 10.0;
  minmass  = 0.0;
  maxmass  = 2.0;
  minpnt   = 0.0;
  maxpnt   = 50.0;
  minleng  = 0.0;
  maxleng  = 200.0;
  mintau   = 0.0;
  maxtau   = 20.0;
  mintrk   = 0.0;
  maxtrk   = 8000.0;
  minvrt   = 2000.0;
  maxvrt   = 4000.0;
  minmpt   = 0.0;
  maxmpt   = 2.0;
  minmeta  = -0.2;
  maxmeta  = 0.2;
  kmineta  = -3.0;
  kmaxeta  = 3.0;
  minxyz   = 0.0;
  maxxyz   = 50.0;
  cminp = 0.0;
  cmaxp = 2.0;
  cmindedx = 0.0;
  cmaxdedx =  0.1e-04*1e6; // change from GeV to keV per cm


//  - Zero all histogram pointers

  mNullPrimVtxClass = 0;

// for method MakeGlob - from table globtrk
  m_globtrk_fit_prob=0;
  m_globtrk_tot=0;
  m_globtrk_good=0;
  m_globtrk_good_sm=0;
  m_globtrk_goodTTS=0;
  m_globtrk_goodF=0;
  m_globtrk_iflag=0;
  m_det_id=0;
  m_dcaToBeamXY=0;
  m_dcaToBeamZ1=0;
  m_dcaToBeamZ2=0;
  m_dcaToBeamZ3=0;
  m_zDcaTanl=0;
  m_zDcaZf=0;
  m_zDcaPsi=0;
  m_zDcaPhi0=0;

  m_pointT=0;
  m_pointF=0;
  m_pointFE=0;
  m_pointFW=0;
  m_max_pointT=0;
  m_max_pointF=0;
  m_max_pointFE=0;
  m_max_pointFW=0;
  m_fit_pointT=0;
  m_fit_pointTTS=0;
  m_glb_sptsTS=0;
  m_glb_ratiomTTS=0;
  m_glb_ratiomT=0;
  m_glb_ratiomF=0;
  m_glb_ratiomFE=0;
  m_glb_ratiomFW=0;
  m_glb_chargeT=0;
  m_glb_chargeF=0;
  m_glb_chargeFE=0;
  m_glb_chargeFW=0;
  m_glb_r0T=0;
  m_glb_phi0T=0;
  m_glb_z0T=0;
  m_glb_curvT=0;
  m_glb_padfT=0;  
  m_glb_padfTEW=0;  
  m_glb_xf0=0;
  m_glb_xfT=0;
  m_glb_xfF=0;
  m_glb_xfFE=0;
  m_glb_xfFW=0;
  m_glb_yf0=0;
  m_glb_yfT=0;  
  m_glb_yfF=0;
  m_glb_yfFE=0;  
  m_glb_yfFW=0;     
  m_glb_zf0=0;     
  m_glb_zfT=0; 
  m_glb_zfF=0;
  m_glb_zfFE=0; 
  m_glb_zfFW=0;
  m_glb_planefF=0;
  m_glb_f0=0;
  m_glb_rzf0=0;
  m_glb_rzl0=0;
  m_glb_radfT=0;
  m_glb_radfF=0;
  m_glb_radfFE=0;
  m_glb_radfFW=0;
  m_glb_phifT=0;
  m_glb_phifTS=0;
  m_psiT=0;  
  m_psiTTS=0;  
  m_psiF=0;
  m_psiFE=0;
  m_psiFW=0;
  m_tanlT=0;
  m_glb_thetaT=0;  
  m_etaT=0;    
  m_etaTTS=0;    
  m_etaF=0;
  m_etaFE=0;
  m_etaFW=0;
  m_pTT=0;
  m_pTTTS=0;
  m_pTF=0;
  m_pTFE=0;
  m_pTFW=0;
  m_momT=0; 
  m_momF=0;
  m_momFE=0; 
  m_momFW=0;        
  m_lengthT=0;  
  m_lengthF=0;
  m_lengthFE=0;  
  m_lengthFW=0;  
  m_chisq0T=0;     
  m_chisq0TTS=0;     
  m_chisq0F=0;
  m_chisq0FE=0;
  m_chisq0FW=0;
  m_chisq1F=0;
  m_chisq1FE=0;
  m_chisq1FW=0;
  m_glb_impactT=0; 
  m_glb_simpactT=0; 
  m_glb_impactrT=0; 
  m_glb_impactTTS=0; 
  m_glb_impactrTTS=0; 
  m_glb_impactF=0; 
  m_glb_impactrF=0; 

  m_pointTS=0;        
  m_max_pointTS=0;    
  m_fit_pointTS=0;    
  m_glb_ratiomTS=0;   
  m_glb_chargeTS=0;   
  m_glb_r0TS=0;       
  m_glb_phi0TS=0;     
  m_glb_z0TS=0;       
  m_glb_curvTS=0;     
  m_glb_xfTS=0;       
  m_glb_yfTS=0;       
  m_glb_zfTS=0;       
  m_glb_xf0TS=0;      
  m_glb_yf0TS=0;      
  m_glb_zf0TS=0;      
  m_glb_f0TS=0;      
  m_glb_rzf0TS=0;
  m_glb_rzl0TS=0;
  m_glb_radfTS=0;     
  m_psiTS=0;          
  m_tanlTS=0;         
  m_glb_thetaTS=0;    
  m_etaTS=0;          
  m_momTS=0;          
  m_pTTS=0;           
  m_lengthTS=0;       
  m_chisq0TS=0;       
  m_glb_impactTS=0;   
  m_glb_simpactTS=0;   
  m_glb_impactrTS=0;   

  m_pT_eta_recT = 0;
  m_pT_eta_recFE = 0;
  m_pT_eta_recFW = 0;
  m_globtrk_xf_yfTE = 0;
  m_globtrk_xf_yfTW = 0;
  m_globtrk_xf_yfFE = 0;
  m_globtrk_xf_yfFW = 0;
  m_globtrk_padtimeFE = 0;
  m_globtrk_padtimeFW = 0;
  m_tanl_zfT  = 0;
  m_mom_trklengthT = 0;
  m_eta_trklengthT = 0;
  m_eta_trklengthFE = 0;
  m_eta_trklengthFW = 0;
  m_npoint_lengthT = 0;	
  m_npoint_lengthFE = 0;	
  m_npoint_lengthFW = 0;		  
  m_fpoint_lengthT = 0;
  m_fpoint_lengthTTS = 0;
  m_chisq0_momT = 0;
  m_chisq0_etaT = 0;
  m_chisq0_dipT = 0;
  m_chisq0_zfT = 0;
  m_chisq0_phiT = 0;
  m_psi_phiT = 0;

  m_pT_eta_recTS= 0;
  m_globtrk_xf_yfTS = 0;
  m_tanl_zfTS  = 0;
  m_mom_trklengthTS = 0;
  m_eta_trklengthTS = 0;
  m_npoint_lengthTS = 0;	
  m_fpoint_lengthTS = 0;
  m_chisq0_momTS = 0;
  m_chisq0_etaTS = 0;
  m_chisq0_dipTS = 0;
  m_chisq0_zfTS = 0;
  m_chisq0_phiTS = 0;
  m_psi_phiTS = 0;

// for method MakeDE - from table dst_dedx
  m_ndedxr=0;        //! number of tracks with dedx info

  m_ndedxT=0;         //! number of point to find dE/dx
  m_dedx0T=0;         //! dE/dx [0] - mean
  m_dedx1T=0;         //! dE/dx [1] - sigma
  m_dedxTTS=0;        //! <dE/dx>/(Bichsel <dE/dx>), tpc, tpc+svt
  // east and west on same plot
  m_ndedxF=0;         //! number of point to find dE/dx
  m_dedx0F=0;         //! dE/dx [0] - mean
  // east and west on separate plots
  m_ndedxFE=0;         //! number of point to find dE/dx
  m_dedx0FE=0;         //! dE/dx [0] - mean
  
  m_ndedxFW=0;         //! number of point to find dE/dx
  m_dedx0FW=0;         //! dE/dx [0] - mean
  
// for method MakeHistPrim - from table primtrk
  m_primtrk_tot=0;
  m_primtrk_tot_sm=0;
  m_primtrk_good=0;
  m_primtrk_good_sm=0;
  m_primtrk_goodTTS=0;
  m_primtrk_goodF=0;
  m_primtrk_iflag=0;
  m_primglob_good=0;
  m_primglob_fit=0;
  m_pdet_id=0;
  m_primtrk_meanptTTS=0;
  m_primtrk_meanptF=0;
  m_primtrk_meanetaTTS=0;
  m_primtrk_meanetaF=0;

  m_ppointT=0;
  m_ppointF=0;
  m_ppointFE=0;
  m_ppointFW=0;
  m_pmax_pointT=0;
  m_pmax_pointF=0;
  m_pmax_pointFE=0;
  m_pmax_pointFW=0;
  m_pfit_pointT=0;
  m_prim_ratiomT=0;
  m_prim_ratioF=0;
  m_prim_ratioFE=0;
  m_prim_ratioFW=0;
  m_prim_ratiomF=0;
  m_prim_ratiomFE=0;
  m_prim_ratiomFW=0;
  m_prim_chargeT=0;
  m_prim_chargeF=0;
  m_prim_chargeFE=0;
  m_prim_chargeFW=0;
  m_prim_r0T=0;
  m_prim_phi0T=0;
  m_prim_z0T=0;
  m_prim_curvT=0;
  m_prim_xf0=0;
  m_prim_xfT=0;
  m_prim_xfF=0;
  m_prim_xfFE=0;
  m_prim_xfFW=0;
  m_prim_yf0=0;
  m_prim_yfT=0;  
  m_prim_yfF=0;
  m_prim_yfFE=0;
  m_prim_yfFW=0;
  m_prim_zf0=0;     
  m_prim_zfT=0; 
  m_prim_zfF=0;
  m_prim_zfFE=0;
  m_prim_zfFW=0;
  m_prim_f0=0;
  m_prim_rzf0=0;
  m_prim_rzl0=0;
  m_prim_radfT=0;
  m_prim_radfF=0;
  m_prim_radfFE=0;
  m_prim_radfFW=0;
  m_ppsiT=0;  
  m_ppsiTTS=0;  
  m_ppsiF=0;
  m_ppsiFE=0;
  m_ppsiFW=0;
  m_ptanlT=0;   
  m_prim_thetaT=0;  
  m_petaT=0;    
  m_petaTTS=0;    
  m_petaF=0;
  m_petaFE=0;
  m_petaFW=0;
  m_ppTT=0;
  m_ppTTTS=0;
  m_ppTF=0;
  m_ppTFE=0;
  m_ppTFW=0;
  m_pmomT=0; 
  m_pmomTS=0; 
  m_pmomF=0;
  m_pmomFE=0;
  m_pmomFW=0;
  m_plengthT=0;  
  m_plengthF=0;
  m_plengthFE=0;
  m_plengthFW=0;
  m_pchisq0T=0;     
  m_pchisq0TTS=0;     
  m_pchisq0F=0;
  m_pchisq0FE=0;
  m_pchisq0FW=0;
  m_pchisq1F=0;
  m_pchisq1FE=0;
  m_pchisq1FW=0;

  m_ppointTS=0;        
  m_pmax_pointTS=0;    
  m_pfit_pointTS=0;    
  m_prim_ratiomTS=0;   
  m_prim_chargeTS=0;   
  m_prim_r0TS=0;       
  m_prim_phi0TS=0;     
  m_prim_z0TS=0;       
  m_prim_curvTS=0;     
  m_prim_xfTS=0;       
  m_prim_yfTS=0;       
  m_prim_zfTS=0;       
  m_prim_xf0TS=0;      
  m_prim_yf0TS=0;
  m_prim_zf0TS=0;
  m_prim_f0TS=0;
  m_prim_rzf0TS=0;
  m_prim_rzl0TS=0;
  m_prim_radfTS=0;
  m_ppsiTS=0;          
  m_ptanlTS=0;         
  m_prim_thetaTS=0;    
  m_petaTS=0;          
  m_momTS=0;          
  m_ppTTS=0;           
  m_plengthTS=0;       
  m_pchisq0TS=0;       

  m_ppT_eta_recT = 0;
  m_ppT_eta_recFE = 0;
  m_ppT_eta_recFW = 0;
  m_primtrk_xf_yfTE = 0;
  m_primtrk_xf_yfTW = 0;
  m_primtrk_xf_yfFE = 0;
  m_primtrk_xf_yfFW = 0;
  m_ptanl_zfT  = 0;
  m_pmom_trklengthT = 0;
  m_peta_trklengthT = 0;
  m_peta_trklengthFE = 0;
  m_peta_trklengthFW = 0;
  m_pnpoint_lengthT = 0;	
  m_pnpoint_lengthFE = 0;	
  m_pnpoint_lengthFW = 0;		  
  m_pfpoint_lengthT = 0;
  m_pfpoint_lengthTTS = 0;
  m_pchisq0_momT = 0;
  m_pchisq0_etaT = 0;
  m_pchisq0_dipT = 0;
  m_pchisq0_zfT = 0;
  m_ppsi_phiT = 0;

  m_ppT_eta_recTS= 0;
  m_primtrk_xf_yfTS = 0;
  m_ptanl_zfTS  = 0;
  m_pmom_trklengthTS = 0;
  m_peta_trklengthTS = 0;
  m_pnpoint_lengthTS = 0;	
  m_pfpoint_lengthTS = 0;
  m_pchisq0_momTS = 0;
  m_pchisq0_etaTS = 0;
  m_pchisq0_dipTS = 0;
  m_pchisq0_zfTS = 0;
  m_ppsi_phiTS = 0;

  // for MakeHistPID - from tables primtrk & dst_dedx 
  m_p_dedx_rec=0;   //! dedx vs p
  
  // for method MakeHistVertex - from table dst_vertex
  m_v_num=0;   //! number of vertices
  m_v_num_sm=0;//! number of vertices,small range
  m_v_vtxid=0; //! vertex type
  m_v_x=0;     //! vertex coordinates in
  m_v_y=0;     //!  STAR reference 
  m_v_z=0;     //!   system
  m_v_pchi2=0; //! chisq per dof of vertex fit
  m_v_r=0;     //! radius to vertex
  
  m_pv_vtxid=0; //! row1-vertex type
  m_pv_x=0;     //! row1-vertex coordinates in
  m_pv_y=0;     //!  STAR reference 
  m_pv_z=0;     //!   system
  m_pv_xy=0;    //! x versus y scatter plot
  m_pv_pchi2=0; //! row1-chisq per dof of vertex fit
  m_pv_r=0;     //! radius to primary vertex

  m_vtx_phi_dist=0;  //! azimuthal distribution of V0s relative to primVtx
  m_vtx_r_dist=0;  //! radial distribution of V0s relative to primVtx
  m_vtx_z_dist=0;  //! z distribution of V0s relative to primVtx

  m_v0           =0; //! # v0 vertices
  m_ev0_lama_hist=0; //! Lambda mass
  m_ev0_k0ma_hist=0; //! K0 mass  
  m_xi_tot=0;        //! number of vertices
  m_xi_ma_hist=0;    //! Xi Mass
  m_kink_tot=0;      //! number of kinks
  
  m_vtx_FtpcEastTpc_xy=0; //! FtpcEast prim vtx - TPC prim vtx, x vs y
  m_vtx_FtpcEastTpc_z=0;  //! FtpcEast prim vtx z - TPC prim vtx z
  m_vtx_FtpcWestTpc_xy=0; //! FtpcWest prim vtx - TPC prim vtx, x vs y
  m_vtx_FtpcWestTpc_z=0;  //! FtpcWest prim vtx z - TPC prim vtx z
  

// for method MakeHistPoint
  m_z_hits=0;
  m_pnt_tot=0;     //! number of hits
  m_pnt_tot_med=0; //! number of hits, med range
  m_pnt_tot_sm=0;  //! number of hits, small range
  m_pnt_id=0;      //! detector id of the hit
  m_pnt_xyS=0;     //! xy dist. of hits, svt
  m_pnt_rpTW=0;    //! rphi dist. of hits, tpcW
  m_pnt_rpTE=0;    //! rphi dist. of hits, tpcE
  m_pnt_phiT=0;    //! phi dist. of hits, tpc
  m_pnt_padrowT=0; //! padrow dist. of hits, tpc
  m_pnt_timeT=0;   //! time bucket dist. of hits, tpc
  m_pnt_zS=0;      //! z dist. of hits, svt
  m_pnt_phiS=0;    //! phi dist. of hits, svt
  m_pnt_barrelS=0; //! barrel dist. of hits, svt
  m_pnt_planeF=0;  //! plane dist. of hits, ftpc
  m_pnt_tpc=0;     //! number of hits tpc
  m_pnt_xyFE=0;    //! xy dist. of hits, ftpcE
  m_pnt_xyFW=0;    //! xy dist. of hits, ftpcW
  m_pnt_padtimeFE=0;    //! padlength vs. timelength of hits, ftpcE
  m_pnt_padtimeFW=0;    //! padlength vs. timelength of hits, ftpcW
  // east and west on same plot
  m_pnt_ftpc=0;    //! number of hits ftpc
  // east and west on separate plots
  m_pnt_ftpcE=0;   //! number of hits ftpcE
  m_pnt_ftpcW=0;   //! number of hits ftpcW

// for method MakeHistEval  
  m_geant_reco_pvtx_x=0;  //! prim vtx x, diff geant - reco
  m_geant_reco_pvtx_y=0;  //! prim vtx y, diff geant - reco
  m_geant_reco_pvtx_z=0;  //! prim vtx z, diff geant - reco
  m_geant_reco_vtx_z_z=0; //! prim vtx z, diff geant - reco vs reco z

// for EMC hits
  m_emc_nhit=0;            //!
  m_emc_etot=0;            //!
  for (i=0; i<4; i++) {
    m_emc_hits[i]=0;         //!
    m_emc_energy2D[i]=0;     //!
    m_emc_adc[i]=0;          //!
    m_emc_energy[i]=0;       //!
  }

// for EMC cluster finder
  m_emc_ncl=0;             //!
  m_emc_etotCl=0;          //!
  m_emc_sig_e=0;           //!
  m_emc_sig_p=0;           //!
  for (i=0; i<4; i++) {
    m_emc_cl[i]=0;           //!
    m_emc_energyCl[i]=0;     //!
    m_emc_HitsInCl[i]=0;     //!
    m_emc_EnergyCl[i]=0;     //!
    m_emc_EtaInCl[i]=0;      //!
    m_emc_PhiInCl[i]=0;      //!
  }

// for EMC points
  for (i=0; i<4; i++) {
    m_emc_point_energy[i]=0; //! Point Energy spectra
    m_emc_point_eta[i]=0;    //! Point Eta spectra
    m_emc_point_phi[i]=0;    //! Point Phi spectra
    m_emc_point_sigeta[i]=0; //! Point SigmaEta spectra
    m_emc_point_sigphi[i]=0; //! Point SigmaPhi spectra
    m_emc_point_deleta[i]=0; //! Point DeltaEta spectra
    m_emc_point_delphi[i]=0; //! Point DeltaPhi spectra
    m_emc_point_trmom[i]=0;  //! Point TrMom spectra
    m_emc_points[i]=0;       //! Emc Point multiplicity
  }
  m_emc_point_flag=0;      //! Point Flag spectra

// for BBC
  for (i=0; i<4; i++) {
    m_bbc_adc[i] = 0;
    m_bbc_tdc[i] = 0;
  }

// for FPD
  for (i=0; i<2; i++) {
    m_fpd_top[i] = 0;
    m_fpd_bottom[i] = 0;
    m_fpd_south[i] = 0;
    m_fpd_north[i] = 0;
  }
  for (i=0; i<8; i++) m_fpd_sums[i] = 0;

// for PMD
  for (i=0; i<12; i++) {
    m_pmd_sm_adc[i] = 0;
    m_pmd_sm_hit[i] = 0;
  }
  for (i=0; i<24; i++) {
    m_pmd_chain_adc[i] = 0;
    m_pmd_chain_hit[i] = 0;
  }
  m_pmd_total_hit = 0;
  m_pmd_total_adc = 0;
  m_cpv_total_hit = 0;
  m_cpv_total_adc = 0;

// for SVT
  m_pnt_svt=0;     //! number of hits svt
  m_pnt_svtLaser=0;     //! laser spots in svt
  m_pnt_svtLaserDiff=0; //! diff of laser spots in svt
  m_svt_loc=0;     //! SVT drift

// for SSD
  m_pnt_ssd=0;     //! number of hits ssd
  m_pnt_phiSSD = 0;
  m_pnt_lwSSD = 0;
  m_glb_ssd_phi = 0;
  m_prim_ssd_phi = 0;
  m_pnt_sizeSSD = 0;
  m_pnt_eSSD = 0;

// for SST
  m_pnt_sst=0;     //! number of hits sst
  m_pnt_phiSST = 0;
  m_pnt_lwSST = 0;
  m_glb_sst_phi = 0;
  m_prim_sst_phi = 0;
  m_pnt_sizeSST = 0;
  m_pnt_eSST = 0;

  // HFT histograms
  // correlation plots: hits in HFT subsystems
  m_nhit_Pxl_Ist = 0;
  m_nhit_Pxl_Sst = 0;
  m_nhit_Ist_Sst = 0;
  m_global_hft_hit = 0;
  m_primary_hft_hit = 0;

  // Hists for PIXEL
  m_pxl_hit_phi_z_Pxl1 = 0;
  m_pxl_hit_phi_z_Pxl2 = 0;
  m_pxl_hit_ladder = 0;
  m_pxl_hit_sector_sensor_Pxl1 = 0;
  m_pxl_hit_sector_sensor_Pxl2 = 0;
  m_pxl_nhit_Pxl1_tpc_mult = 0;
  m_pxl_nhit_Pxl2_tpc_mult = 0;
  m_pxl_nhit_Pxl1_tof_mult = 0;
  m_pxl_nhit_Pxl2_tof_mult = 0;
  m_pxl_nhit_Pxl1_Pxl2 = 0;
  m_global_pxl_hit = 0;
  m_primary_pxl_hit = 0;

  // Hists for IST
  m_ist_hit_phi_z = 0;
  m_ist_hit_ladder = 0;
  m_ist_hit_ladder_sensor = 0;
  m_ist_nhit_tpc_mult = 0;
  m_ist_nhit_tof_mult = 0;
  m_global_ist_hit = 0;
  m_primary_ist_hit = 0;

// for TOF
  m_tof_hit_tray=0;        //! # of hits vs tray # (int over modules)
  m_tof_hit_module=0;      //! # of hits vs module # (int over trays, east side use -32 - -1)
  m_tof_match_tray=0;      //! # of matched hits vs tray #
  m_tof_match_module=0;    //! # of matched hits vs module #
  m_tof_vpd_hit=0;         //! # of tof hits vs # of vpd hits
  m_tof_vtx_z=0;           //! vertex z from vpd vs verex z from TPC
  m_tof_PID=0;             //! TOF PID:  1/beta vs p

// for MTD
  m_MtdNHits = 0;
  m_MtdHitMap = 0;
  m_MtdNMatchHits = 0;
  m_MtdMatchHitMap = 0;

// for EPD
  for (int i=0; i<24; i++) {
    m_epd_adc[i] = 0;
    m_epd_tac[i] = 0;
  }


// for iTPC
  for(int i = 0; i < 24; i++) { 
    m_TPC_ch_time_inner[i] = 0;
    m_TPC_ch_time_outer[i] = 0;
    m_TPC_ch_nrow[i] = 0;
  }
  m_TPC_adc_sec_inner = 0;
  m_TPC_adc_sec_outer = 0;


}
//_____________________________________________________________________________
void StQABookHist::BookHist(Int_t hSet){

  histsSet = hSet;

  QAH::preString = QAHistType;
//book histograms --------------
  if (histsSet != StQA_MC) {
    mNullPrimVtxClass = QAH::H1F("QaNullPrimVtxMult","event primary vertex check",40,-2,2);
    mNullPrimVtxClass->SetXTitle("has primary vertex? (yes = 1, no = -1)");
    mNullPrimVtxClass->SetYTitle("# of events");
  }
  if (histsSet<StQA_run8) silHists = kTRUE;
  if (histsSet<StQA_run12all) ftpHists = kTRUE;
  BookHistPoint();
  BookHistEMC();
  if (histsSet == StQA_AuAuOld) {
    BookHistBBC();
    BookHistFPD();
  }
  BookHistGlob();
  BookHistPrim();
  BookHistDE();
  BookHistPID();
  BookHistVertex();
  if (histsSet<StQA_run12all) BookHistPMD();
  if (histsSet==StQA_MC) BookHistEval();
  if (histsSet>=StQA_run8) BookHistTOF();
  if (histsSet>=StQA_run12all) BookHistMTD();
  if (histsSet>=StQA_run14 && histsSet<StQA_run17) {
    BookHistPXL();
    BookHistIST();
    BookHistHFT();
  }
  if (histsSet>=StQA_run18) {
    BookHistEPD();
    BookHistiTPC();
  }

  
}
//_____________________________________________________________________________
void StQABookHist::BookHistGlob(){
  
// for method MakeGlob - from table globtrk

// general

  m_globtrk_tot      = QAH::H1F("QaGtrkTot","globtrk: tot num tracks - all",
                            40,0.,12500.);
  m_globtrk_iflag    = QAH::H1F("QaGtrkFlag","globtrk: iflag - all ",200,-999.,1001.);
  m_globtrk_good     = QAH::H1F("QaGtrkGood","globtrk: tot good tracks - all",40,0.,10000.);
  m_globtrk_good_sm  = QAH::H1F("QaGtrkGoodsm","globtrk: tot good tracks - all",40,0.,1000.);
  if (silHists)
  m_globtrk_goodTTS  = QAH::H1F("QaGtrkGoodTTS","globtrk: tot good tracks - tpc,tpc+svt",150,0.,9000.);
  m_globtrk_goodF    = QAH::H2F("QaGtrkGoodF","globtrk: tot good tracks - ftpc",150,0.,1500.,150,0.,1500.);
  m_globtrk_goodF->SetXTitle("FTPC East");
  m_globtrk_goodF->SetYTitle("FTPC West");
  m_globtrk_fit_prob = QAH::H1F("QaGtrkFitProb","globtrk: prob. fit is correct",100,0,1.2);
  m_det_id           = QAH::H1F("QaGtrkDetId","globtrk: Detector ID good tracks - all",48,-0.5,47.5);
  m_dcaToBeamXY      = QAH::H2F("QaGtrkDcaBeamXY","globtrk: xy-DCA to Beam Axis (z=0)",80,-4,4,80,-4,4);
  m_dcaToBeamXY->SetXTitle("x");
  m_dcaToBeamXY->SetYTitle("y");
  m_dcaToBeamZ1      = QAH::H1F("QaGtrkDcaBeamZ1","globtrk: z-DCA to Beam Axis, coarse scale",100,-200,200);
  m_dcaToBeamZ2      = QAH::H1F("QaGtrkDcaBeamZ2","globtrk: z-DCA to Beam Axis",100,-50,50);
  m_dcaToBeamZ3      = QAH::H1F("QaGtrkDcaBeamZ3","globtrk: z-DCA to Beam Axis, near z=0",80,-20,20);
  m_zDcaTanl         = QAH::H2F("QaGtrkZdcaTanl","globtrk: z-DCA to Beam Axis vs tanl, tpc",100,-25,25,32,-4,4);
  m_zDcaTanl->SetXTitle("z-DCA");
  m_zDcaTanl->SetYTitle("tanl");
  m_zDcaZf           = QAH::H2F("QaGtrkZdcaZf","globtrk: z-DCA to Beam Axis vs z-first",100,-25,25,50,-300,300);
  m_zDcaZf->SetXTitle("z-DCA");
  m_zDcaZf->SetYTitle("z of first point");
  m_zDcaPsi          = QAH::H2F("QaGtrkZdcaPsi","globtrk: z-DCA to Beam Axis vs psi",100,-25,25,64,0,360);
  m_zDcaPsi->SetXTitle("z-DCA");
  m_zDcaPsi->SetYTitle("psi");
  m_zDcaPhi0         = QAH::H2F("QaGtrkZdcaPhi0","globtrk: z-DCA to Beam Axis vs azimuth (phi0) at start",80,-20,20,64,0,360);
  m_zDcaPhi0->SetXTitle("z-DCA");
  m_zDcaPhi0->SetYTitle("phi0");
  if (silHists) {
  m_glb_sptsTS       = QAH::H1F("QaGtrkSptsTS","globtrk: number of svt points, tpc+svt",4,0.,4.);
  m_glb_ratiomTTS    = QAH::MH1F("QaGtrkRnmfTTS","globtrk: ratio Nfit/max pnt, tpc,svt", 55,0.,1.1,2);
  m_glb_ratiomTTS->SetMinimum(10);
  m_glb_ratiomTTS->Rebin(0,"TPC+SVT");
  m_glb_ratiomTTS->Rebin(1,"TPC");
  m_glb_ratiomTTS->SetStats(kFALSE);
  m_psiTTS           = QAH::MH1F("QaGtrkPsiTTS", "globtrk: psi (deg), tpc,svt", 64, 0.,360.,2);
  m_psiTTS->Rebin(0,"TPC+SVT");
  m_psiTTS->Rebin(1,"TPC");
  m_psiTTS->SetStats(kFALSE);
  m_etaTTS           = QAH::MH1F("QaGtrkEtaTTS", "globtrk: eta, tpc,svt",40,-2.,2.,2);
  m_etaTTS->Rebin(0,"TPC+SVT");
  m_etaTTS->Rebin(1,"TPC");
  m_etaTTS->SetStats(kFALSE);
  m_pTTTS            = QAH::MH1F("QaGtrkPtTTS",  "globtrk: log10 pT, tpc,svt",50,-1.,4.,2);
  m_pTTTS->Rebin(0,"TPC+SVT");
  m_pTTTS->Rebin(1,"TPC");
  m_pTTTS->SetStats(kFALSE);
  m_pTTTS->SetXTitle("log10 pT (MeV)");
  m_chisq0TTS        = QAH::MH1F("QaGtrkChisq0TTS", "globtrk: chisq0, tpc,svt", 50, 0.,5.,2);
  m_chisq0TTS->Rebin(0,"TPC+SVT");
  m_chisq0TTS->Rebin(1,"TPC");
  m_chisq0TTS->SetStats(kFALSE);
  m_fit_pointTTS     = QAH::MH1F("QaGtrkNPntFitTTS","globtrk: N fit pnts on trk, tpc,svt",50,0.,50.,2);
  m_fit_pointTTS->Rebin(0,"TPC+SVT");
  m_fit_pointTTS->Rebin(1,"TPC");
  m_fit_pointTTS->SetStats(kFALSE);
  m_glb_impactTTS    = QAH::MH1F("QaGtrkImpactTTS", "globtrk: log10 impact param from prim vtx, tpc,svt",120,-3.,3.,2);
  m_glb_impactTTS->Rebin(0,"TPC+SVT");
  m_glb_impactTTS->Rebin(1,"TPC");
  m_glb_impactTTS->SetStats(kFALSE);
  m_glb_impactrTTS   = QAH::MH1F("QaGtrkImpactrTTS", "globtrk: impact param from prim vtx, tpc,svt",100,0.,300.,2);
  m_glb_impactrTTS->Rebin(0,"TPC+SVT");
  m_glb_impactrTTS->Rebin(1,"TPC");
  m_glb_impactrTTS->SetStats(kFALSE);
  m_fpoint_lengthTTS = QAH::H2F("QaGtrkFitPntLTTS","globtrk: N fit pnts vs length, tpc,tpc+svt",
				25,0.,250.,25,0.,50.);
  m_fpoint_lengthTTS->SetXTitle("trk length");
  m_fpoint_lengthTTS->SetYTitle("Npoints on trk");
  }

// 1D tpc

  m_pointT      = QAH::H1F("QaGtrkNPntT",   "globtrk: N points on trk,tpc", 81,-0.5, 80.5);
  m_max_pointT  = QAH::H1F("QaGtrkNPntMaxT","globtrk: N max pnts on trk, tpc", 76, -1, 151);
  m_fit_pointT  = QAH::H1F("QaGtrkNPntFitT","globtrk: N fit pnts on trk, tpc", 81, -0.5, 80.5);
  m_glb_ratiomT = QAH::H1F("QaGtrkRnmT",    "globtrk: ratio Nfit/max pnt, tpc", 55, 0., 1.1);
  m_glb_chargeT = QAH::H1F("QaGtrkChrgT",   "globtrk: charge, tpc ", 20,-2.,2.);
  m_glb_r0T     = QAH::H1F("QaGtrkR0T",     "globtrk: radius at start (cm), tpc ", 50,0.,200.);
    m_glb_r0T->SetMinimum(100);
  m_glb_phi0T   = QAH::H1F("QaGtrkPhi0T",   "globtrk: azimuth (phi) at start (deg,force 0,360), tpc ", 64,0.,360.);
  m_glb_z0T     = QAH::H1F("QaGtrkZ0T",     "globtrk: z-coord at start (cm), tpc ", 50, -300.,300.);
    m_glb_z0T->SetMinimum(500);
  m_glb_curvT   = QAH::H1F("QaGtrkCurvT",   "globtrk: log10 curvature (1/cm), tpc ", 80,-3.5,0.5);
  m_glb_padfTEW = QAH::MH1F("QaGtrkPadfTEW","globtrk: padrow of first hit on trk, tpc", 72,0.5,72.5,2);
  m_glb_padfTEW->Rebin(0,"East");
  m_glb_padfTEW->Rebin(1,"West");
  m_glb_padfTEW->SetStats(kFALSE);
  m_glb_padfT   = QAH::H1F("QaGtrkPadfT",   "globtrk: padrow of first hit on trk, tpc", 72,0.5,72.5);
  m_glb_f0      = QAH::MH1F("QaGtrkf0",     "globtrk: first point: hit-helix, tpc",60,-3.,3.,3);
  m_glb_f0->Rebin(0,"X");
  m_glb_f0->Rebin(1,"Y");
  m_glb_f0->Rebin(2,"Z");
  m_glb_f0->SetStats(kFALSE);
  m_glb_xfT     = QAH::H1F("QaGtrkXfT",     "globtrk: x of first hit on trk, tpc", 50,-200.,200.);
  m_glb_xf0     = QAH::H1F("QaGtrkXf0",     "globtrk: first point: x_hit - x_helix, tpc",60,-3.,3.);
  m_glb_yfT     = QAH::H1F("QaGtrkYfT",     "globtrk: y of first hit on trk, tpc", 50,-200.,200.);
  m_glb_yf0     = QAH::H1F("QaGtrkYf0",     "globtrk: first point: y_hit - y_helix, tpc",60,-3.,3.);
  m_glb_zfT     = QAH::H1F("QaGtrkZfT",     "globtrk: z of first hit on trk, tpc", 50,-300.,300.);
  m_glb_zf0     = QAH::H1F("QaGtrkZf0",     "globtrk: first point: z_hit - z_helix, tpc",60,-3.,3.);
  m_glb_radfT   = QAH::H1F("QaGtrkRT",      "globtrk: radial position of first hit, tpc", 50,0.,200.);
  m_glb_rzf0    = QAH::MH1F("QaGtrkRZf0",   "globtrk: first point: hit - helix, tpc",60,-3.,3.,2);
  m_glb_rzf0->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_glb_rzf0->Rebin(1,"z_{dif}");
  m_glb_rzf0->SetStats(kFALSE);
  m_glb_rzl0    = QAH::MH1F("QaGtrkRZl0",   "globtrk: last point: hit - helix, tpc",60,-3.,3.,2);
  m_glb_rzl0->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_glb_rzl0->Rebin(1,"z_{dif}");
  m_glb_rzl0->SetStats(kFALSE);
  m_glb_phifT   = QAH::MH1F("QaGtrkPhifT",  "globtrk: phi of first point on trk, tpc",64,0,360,2);
  m_glb_phifT->Rebin(0,"East");
  m_glb_phifT->Rebin(1,"West");
  m_glb_phifT->SetStats(kFALSE);
  m_lengthT     = QAH::H1F("QaGtrkLengthT", "globtrk: track length, tpc", 50,0.,300.);
  m_psiT        = QAH::H1F("QaGtrkPsiT",    "globtrk: psi, tpc (deg)", 64, 0.,360.);
  m_tanlT       = QAH::H1F("QaGtrkTanlT",   "globtrk: tanl, tpc",32,-4.,4.);
  m_glb_thetaT  = QAH::H1F("QaGtrkThetaT",  "globtrk: theta (degrees), tpc",36,0.,180.);
  m_etaT        = QAH::H1F("QaGtrkEtaT",    "globtrk: eta, tpc",40,-2.,2.);
  m_pTT         = QAH::H1F("QaGtrkPtT",     "globtrk: pT, tpc",50,0.,10.);
  m_momT        = QAH::H1F("QaGtrkPT",      "globtrk: momentum, tpc",50,0.,10.);
  m_chisq0T     = QAH::H1F("QaGtrkChisq0T", "globtrk: chisq0, tpc", 50, 0.,5.);
  m_glb_impactT = QAH::MH1F("QaGtrkImpactT", "globtrk: log10 impact param from prim vtx, tpc",
                            120,-3.0,3.0,3);
  m_glb_impactT->Rebin(0,"East");
  m_glb_impactT->Rebin(1,"West");
  m_glb_impactT->Rebin(2,"All");
  m_glb_impactT->SetStats(kFALSE);
  m_glb_simpactT = QAH::MH1F("QaGtrkSImpactT", "globtrk: signed impact param from prim vtx, tpc",
                            50,-0.8,0.8,3);
  m_glb_simpactT->Rebin(0,"East");
  m_glb_simpactT->Rebin(1,"West");
  m_glb_simpactT->Rebin(2,"All");
  m_glb_simpactT->SetStats(kFALSE);
  m_glb_impactrT = QAH::H1F("QaGtrkImpactrT", "globtrk: impact param from prim vtx, tpc",100,0.,300.);


// 2D - tpc

  m_pT_eta_recT = QAH::H2F("QaGtrkPtVsEtaT","globtrk: log10 pT vs eta, tpc", 20,-2.,2.,40,1.,4.);
    m_pT_eta_recT->SetXTitle("eta");
    m_pT_eta_recT->SetYTitle(" log10 pT (MeV)");

  m_globtrk_xf_yfTE = QAH::H2F("QaGtrkXfYfTE",  "globtrk: Y vs X of first hit on trk, tpc east", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yfTE->SetXTitle("x first");
    m_globtrk_xf_yfTE->SetYTitle("y first");

  m_globtrk_xf_yfTW = QAH::H2F("QaGtrkXfYfTW",  "globtrk: Y vs X of first hit on trk, tpc west", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yfTW->SetXTitle("x first");
    m_globtrk_xf_yfTW->SetYTitle("y first");

  m_tanl_zfT = QAH::H2F("QaGtrkTanlzf","globtrk: tanl(dip) vs. (zfirst-zvtx)/arc length, tpc,tpc+svt",60,-3.,3.,60,-3.,3.);
    m_tanl_zfT->SetXTitle("(zfirst-zvtx)/arc length");
    m_tanl_zfT->SetYTitle("tanl");

  m_mom_trklengthT = QAH::H2F("QaGtrkPVsTrkLength","globtrk: log10 mom vs trk length, tpc",
			     50,0.,250.,40,1.,4.);
    m_mom_trklengthT->SetXTitle("trk length");  
    m_mom_trklengthT->SetYTitle("log10 P (MeV)");

  m_eta_trklengthT = QAH::H2F("QaGtrkLengthVEtaT","globtrk: trk length vs eta, tpc",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklengthT->SetXTitle("eta");
    m_eta_trklengthT->SetYTitle("length");

  m_npoint_lengthT = QAH::H2F("QaGtrkNPntLengthT","globtrk: N pnts vs length, tpc",
			     25,0.,250.,40,1.,81.);
    m_npoint_lengthT->SetXTitle("trk length");
    m_npoint_lengthT->SetYTitle("Npoints on trk");

  m_fpoint_lengthT = QAH::H2F("QaGtrkFitPntLengthT","globtrk: N fit pnts vs length, tpc",
			     25,0.,250.,40,1.,81.);
    m_fpoint_lengthT->SetXTitle("trk length");
    m_fpoint_lengthT->SetYTitle("Npoints on trk");

  m_chisq0_momT = QAH::H2F("QaGtrkChi0MomT","globtrk: Chisq0 vs log10 mom, tpc",40,1.,4.,50,0.,5.);
    m_chisq0_momT->SetXTitle("log10 P (MeV)");
    m_chisq0_momT->SetYTitle("chisq0") ;

  m_chisq0_etaT = QAH::H2F("QaGtrkChi0EtaT","globtrk: Chisq0 vs eta, tpc",20,-2.,2.,20,0.,5.);
    m_chisq0_etaT->SetXTitle("eta");
    m_chisq0_etaT->SetYTitle("chisq0");

  m_chisq0_dipT = QAH::H2F("QaGtrkChi0TanlT","globtrk: Chisq0 vs tanl(dip), tpc",20,-3.2,3.2,20,0.,5.);
    m_chisq0_dipT->SetXTitle("dip angle");
    m_chisq0_dipT->SetYTitle("chisq0");

  m_chisq0_zfT = QAH::H2F("QaGtrkChi0zfT","globtrk: Chisq0 vs zfirst, tpc",20,-250.,250.,20,0.,5.);
    m_chisq0_zfT->SetXTitle("zfirst");
    m_chisq0_zfT->SetYTitle("chisq0");

  m_chisq0_phiT = QAH::H2F("QaGtrkChi0PhiT","globtrk: Chisq0 vs phi, tpc",20,0.,360,20,0.,5.);
    m_chisq0_phiT->SetXTitle("phi");
    m_chisq0_phiT->SetYTitle("chisq0");

  m_psi_phiT = QAH::H2F("QaGtrkPsiPhiT","globtrk: psi vs phi, tpc",36, 0.,360.,36,0.,360.);
     m_psi_phiT->SetXTitle("phi");
     m_psi_phiT->SetYTitle("psi");

  if (silHists) {

// 1D tpc + silicon (svt+ssd)

  m_pointTS      = QAH::H1F("QaGtrkNPntTS",   "globtrk: N points on trk,tpc+svt", 60, 0.,60.);
  m_max_pointTS  = QAH::H1F("QaGtrkNPntMaxTS","globtrk: N max pnts on trk, tpc+svt", 50, 0.,100.);
  m_fit_pointTS  = QAH::H1F("QaGtrkNPntFitTS","globtrk: N fit pnts on trk, tpc+svt", 60, 0.,60.);
  m_glb_ratiomTS = QAH::H1F("QaGtrkRnmTS",    "globtrk: ratio Nfit/max pnt, tpc+svt", 55, 0., 1.1);
  m_glb_chargeTS = QAH::H1F("QaGtrkChrgTS",   "globtrk: charge, tpc+svt ", 20,-2.,2.);
  m_glb_r0TS     = QAH::H1F("QaGtrkR0TS",     "globtrk: radius at start (cm), tpc+svt", 100,0.,25.);
    m_glb_r0TS->SetMinimum(100);
  m_glb_phi0TS   = QAH::H1F("QaGtrkPhi0TS",   "globtrk: azimuth (phi) at start (deg,force 0-360),tpc+svt", 64, 0.,360.);
  m_glb_z0TS     = QAH::H1F("QaGtrkZ0TS",     "globtrk: z-coord at start (cm), tpc+svt", 50, -50.,50.);
    m_glb_z0TS->SetMinimum(500);
  m_glb_curvTS   = QAH::H1F("QaGtrkCurvTS",   "globtrk: log10 curvature (1/cm), tpc+svt", 80,-3.5,0.5);
  m_glb_f0TS     = QAH::MH1F("QaGtrkf0TS",    "globtrk: first point: hit-helix, tpc+svt",50,-5.,5.,3);
  m_glb_f0TS->Rebin(0,"X");
  m_glb_f0TS->Rebin(1,"Y");
  m_glb_f0TS->Rebin(2,"Z");
  m_glb_f0TS->SetStats(kFALSE);
  m_glb_xfTS     = QAH::H1F("QaGtrkXfTS",     "globtrk: x of first hit on trk, tpc+svt", 50,-200.,200.);
  m_glb_xf0TS    = QAH::H1F("QaGtrkXf0TS",    "globtrk: x of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_yfTS     = QAH::H1F("QaGtrkYfTS",     "globtrk: y of first hit on trk, tpc+svt", 50,-200.,200.);
  m_glb_yf0TS    = QAH::H1F("QaGtrkYf0TS",    "globtrk: y of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_zfTS     = QAH::H1F("QaGtrkZfTS",     "globtrk: z of first hit on trk, svt", 50,-30.,30.);
  m_glb_zf0TS    = QAH::H1F("QaGtrkZf0TS",    "globtrk: z of first hit - on helix at start, tpc+svt",50,-5.,5.);
  m_glb_radfTS   = QAH::H1F("QaGtrkRTS",      "globtrk: radial position of first hit, tpc+svt",80,4.,200.);
  m_glb_rzf0TS   = QAH::MH1F("QaGtrkRZf0TS",  "globtrk: first point: hit - helix, tpc+svt",60,-3.,3.,2);
  m_glb_rzf0TS->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_glb_rzf0TS->Rebin(1,"z_{dif}");
  m_glb_rzf0TS->SetStats(kFALSE);
  m_glb_rzl0TS   = QAH::MH1F("QaGtrkRZl0TS",  "globtrk: last point: hit - helix, tpc+svt",60,-3.,3.,2);
  m_glb_rzl0TS->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_glb_rzl0TS->Rebin(1,"z_{dif}");
  m_glb_rzl0TS->SetStats(kFALSE);
  m_glb_phifTS   = QAH::H1F("QaGtrkPhifTS",   "globtrk: phi of first point on track, svt",64,0,360);
  m_glb_ssd_phi  = QAH::H1F("QaGtrkPhifSSD",  "globtrk: phi of ssd point (per event)",64,0,360);
  m_lengthTS     = QAH::H1F("QaGtrkLengthTS", "globtrk: track length, tpc+svt", 50,0.,300.);
  m_psiTS        = QAH::H1F("QaGtrkPsiTS",    "globtrk: psi, tpc+svt (deg) ", 64, 0.,360.);
  m_tanlTS       = QAH::H1F("QaGtrkTanlTS",   "globtrk: tanl, tpc+svt",32,-4.,4.);
  m_glb_thetaTS  = QAH::H1F("QaGtrkThetaTS",  "globtrk: theta (degrees), tpc+svt",36,0.,180.);
  m_etaTS        = QAH::H1F("QaGtrkEtaTS",    "globtrk: eta, tpc+svt",40,-2.,2.);
  m_pTTS         = QAH::H1F("QaGtrkPtTS",     "globtrk: pT, tpc+svt",50,0.,10.);
  m_momTS        = QAH::H1F("QaGtrkPTS",      "globtrk: momentum, tpc+svt",50,0.,10.);
  m_chisq0TS     = QAH::H1F("QaGtrkChisq0TS", "globtrk: chisq0, tpc+svt", 50, 0.,5.);
  m_glb_impactTS = QAH::MH1F("QaGtrkImpactTS", "globtrk: log10 impact param from prim vtx, tpc+svt",
                            120,-3.0,3.0,3);
  m_glb_impactTS->Rebin(0,"East");
  m_glb_impactTS->Rebin(1,"West");
  m_glb_impactTS->Rebin(2,"All");
  m_glb_impactTS->SetStats(kFALSE);
  m_glb_simpactTS = QAH::MH1F("QaGtrkSImpactTS", "globtrk: signed impact param from prim vtx, tpc+svt",
                            50,-0.8,0.8,3);
  m_glb_simpactTS->Rebin(0,"East");
  m_glb_simpactTS->Rebin(1,"West");
  m_glb_simpactTS->Rebin(2,"All");
  m_glb_simpactTS->SetStats(kFALSE);
  m_glb_impactrTS = QAH::H1F("QaGtrkImpactrTS", "globtrk: impact param from prim vtx, tpc+svt",
                            100,0.,30.);


// 2D - tpc + silicon (svt + ssd)

  m_pT_eta_recTS = QAH::H2F("QaGtrkPtVsEtaTS","globtrk: log10 pT vs eta, tpc+svt", 20,-2.,2.,40,1.,4.);
    m_pT_eta_recTS->SetXTitle("eta");
    m_pT_eta_recTS->SetYTitle(" log10 pT (MeV)");

  m_globtrk_xf_yfTS = QAH::H2F("QaGtrkXfYfTS",  "globtrk: Y vs X of first hit on trk, tpc+svt", 40,-200.,200.,40,-200.,200.);
    m_globtrk_xf_yfTS->SetXTitle("x first");
    m_globtrk_xf_yfTS->SetYTitle("y first");

  m_tanl_zfTS = QAH::H2F("QaGtrkTanlzfTS","globtrk: tanl(dip) versus (zfirst-zvtx)/arc length, svt",60,-3.,3.,60,-3.,3.);
    m_tanl_zfTS->SetXTitle("(zfirst-zvtx)/arc length");
    m_tanl_zfTS->SetYTitle("tanl");

  m_mom_trklengthTS = QAH::H2F("QaGtrkPVsTrkLTS","globtrk: log10 mom vs trk length, tpc+svt",50,0.,250.,40,1.,4.);
    m_mom_trklengthTS->SetXTitle("trk length");  
    m_mom_trklengthTS->SetYTitle("log10 P (MeV)");

  m_eta_trklengthTS = QAH::H2F("QaGtrkLVEtaTS","globtrk: trk length vs eta, tpc+svt",20,-2.,2.,50,0.,250.);
    m_eta_trklengthTS->SetXTitle("eta");
    m_eta_trklengthTS->SetYTitle("length");

  m_npoint_lengthTS = QAH::H2F("QaGtrkNPntLTS","globtrk: N pnts vs length, tpc+svt",25,0.,250.,25,0.,50.);
    m_npoint_lengthTS->SetXTitle("trk length");
    m_npoint_lengthTS->SetYTitle("Npoints on trk");

  m_fpoint_lengthTS = QAH::H2F("QaGtrkFitPntLTS","globtrk: N fit pnts vs length, tpc+svt",25,0.,250.,25,0.,50.);
    m_fpoint_lengthTS->SetXTitle("trk length");
    m_fpoint_lengthTS->SetYTitle("Npoints on trk");

  m_chisq0_momTS = QAH::H2F("QaGtrkChi0MomTS","globtrk: Chisq0 vs log10 mom, tpc+svt",40,1.,4.,50,0.,5.);
    m_chisq0_momTS->SetXTitle("log10 P (MeV)");
    m_chisq0_momTS->SetYTitle("chisq0") ;

  m_chisq0_etaTS = QAH::H2F("QaGtrkChi0EtaTS","globtrk: Chisq0 vs eta, tpc+svt",20,-2.,2.,20,0.,5.);
    m_chisq0_etaTS->SetXTitle("eta");
    m_chisq0_etaTS->SetYTitle("chisq0");

  m_chisq0_dipTS = QAH::H2F("QaGtrkChi0TanlTS","globtrk: Chisq0 vs tanl(dip), tpc+svt",20,-3.2,3.2,20,0.,5.);
    m_chisq0_dipTS->SetXTitle("dip angle");
    m_chisq0_dipTS->SetYTitle("chisq0");

  m_chisq0_zfTS = QAH::H2F("QaGtrkChi0zfTS","globtrk: Chisq0 vs zfirst, tpc+svt",20,-250.,250.,20,0.,5.);
    m_chisq0_zfTS->SetXTitle("zfirst");
    m_chisq0_zfTS->SetYTitle("chisq0");

  m_chisq0_phiTS = QAH::H2F("QaGtrkChi0PhiTS","globtrk: Chisq0 vs phi, tpc+svt",20,0.,360,20,0.,5.);
    m_chisq0_phiTS->SetXTitle("phi");
    m_chisq0_phiTS->SetYTitle("chisq0");

  m_psi_phiTS = QAH::H2F("QaGtrkPsiPhiTS","globtrk: psi vs phi, tpc+svt",36, 0.,360.,36,0.,360.);
     m_psi_phiTS->SetXTitle("phi");
     m_psi_phiTS->SetYTitle("psi");

  }

  if (ftpHists) { // FTPC hists

// 1D ftpc

  // both east (solid) and west(dashed) on same plot
  m_pointF      = QAH::MH1F("QaGtrkNPntF",    "globtrk: N points on trk,ftpc", 8,4.,12.,2);
  m_pointF->Rebin(0,"East");
  m_pointF->Rebin(1,"West");
  m_pointF->SetStats(kFALSE);
  m_max_pointF  = QAH::MH1F("QaGtrkNPntMaxF", "globtrk: N max pnts on trk, ftpc", 8,4.,12.,2);
  m_max_pointF->Rebin(0,"East");
  m_max_pointF->Rebin(1,"West");
  m_max_pointF->SetStats(kFALSE);
  m_glb_ratiomF = QAH::MH1F("QaGtrkRnmF",     "globtrk: ratio Nfit/max pnt, ftpc", 11,0.,1.1,2);
  m_glb_ratiomF->Rebin(0,"East");
  m_glb_ratiomF->Rebin(1,"West");
  m_glb_ratiomF->SetStats(kFALSE);
  m_glb_chargeF = QAH::MH1F("QaGtrkChrgF",    "globtrk: charge, ftpc", 20,-2.,2.,2);
  m_glb_chargeF->Rebin(0,"East");
  m_glb_chargeF->Rebin(1,"West");
  m_glb_chargeF->SetStats(kFALSE);
  m_glb_xfF     = QAH::MH1F("QaGtrkXfF",      "globtrk: x of first hit on trk, ftpc",70,-35.,35.,2);
  m_glb_xfF->Rebin(0,"East");
  m_glb_xfF->Rebin(1,"West");
  m_glb_xfF->SetStats(kFALSE);
  m_glb_yfF     = QAH::MH1F("QaGtrkYfF",      "globtrk: y of first hit on trk, ftpc",70,-35.,35.,2);
  m_glb_yfF->Rebin(0,"East");
  m_glb_yfF->Rebin(1,"West");
  m_glb_yfF->SetStats(kFALSE);
  m_glb_zfF     = QAH::MH1F("QaGtrkZfF",      "globtrk: z of first hit on trk, ftpc",75,-225.,225.,2);
  m_glb_zfF->Rebin(0,"East");
  m_glb_zfF->Rebin(1,"West");
  m_glb_zfF->SetStats(kFALSE);
  m_glb_planefF = QAH::MH1F("QaGtrkPlanefF",  "globtrk: plane of first hit on trk, ftpc",20,0.5,20.5,2);
  m_glb_planefF->Rebin(0,"East");
  m_glb_planefF->Rebin(1,"West");
  m_glb_planefF->SetStats(kFALSE);
  m_glb_radfF   = QAH::MH1F("QaGtrkRF",       "globtrk: radial position of first hit, ftpc",35,0.,35.,2);
  m_glb_radfF->Rebin(0,"East");
  m_glb_radfF->Rebin(1,"West");
  m_glb_radfF->SetStats(kFALSE);
  m_lengthF     = QAH::MH1F("QaGtrkLengthF",  "globtrk: track length, ftpc",60,0.,120.,2);
  m_lengthF->Rebin(0,"East");
  m_lengthF->Rebin(1,"West");
  m_lengthF->SetStats(kFALSE);
  m_psiF        = QAH::MH1F("QaGtrkPsiF",     "globtrk: psi, ftpc",90,0.,360.,2);
  m_psiF->Rebin(0,"East");
  m_psiF->Rebin(1,"West");
  m_psiF->SetStats(kFALSE);
  m_etaF        = QAH::MH1F("QaGtrkEtaF",     "globtrk: |eta|, ftpc",80,2,5,2);
  m_etaF->Rebin(0,"East");
  m_etaF->Rebin(1,"West");
  m_etaF->SetStats(kFALSE);
  m_pTF         = QAH::MH1F("QaGtrkPtF",      "globtrk: pT, ftpc",50,0.,10.,2);
  m_pTF->Rebin(0,"East");
  m_pTF->Rebin(1,"West");
  m_pTF->SetStats(kFALSE);
  m_momF        = QAH::MH1F("QaGtrkPF",       "globtrk: momentum, ftpc",50,0.,10.,2);
  m_momF->Rebin(0,"East");
  m_momF->Rebin(1,"West");
  m_momF->SetStats(kFALSE);
  m_chisq0F     = QAH::MH1F("QaGtrkChisq0F",  "globtrk: chi2/ndf x,y fit, ftpc",50,0.,5.,2);
  m_chisq0F->Rebin(0,"East");
  m_chisq0F->Rebin(1,"West");
  m_chisq0F->SetStats(kFALSE);
  m_chisq1F     = QAH::MH1F("QaGtrkChisq1F",  "globtrk: chi2/ndf r,z fit, ftpc",50,0.,5.,2);
  m_chisq1F->Rebin(0,"East");
  m_chisq1F->Rebin(1,"West");
  m_chisq1F->SetStats(kFALSE);
  m_glb_impactF = QAH::MH1F("QaGtrkImpactF", "globtrk: log10 impact param from prim vtx, ftpc",120,-3.0,3.,2);
  m_glb_impactF->Rebin(0,"East");
  m_glb_impactF->Rebin(1,"West");
  m_glb_impactF->SetStats(kFALSE);
  m_glb_impactrF = QAH::MH1F("QaGtrkImpactrF", "globtrk: impact param from prim vtx, ftpc",100,0.,10.,2);
  m_glb_impactrF->Rebin(0,"East");
  m_glb_impactrF->Rebin(1,"West");
  m_glb_impactrF->SetStats(kFALSE);
  // separate east and west plots
  m_pointFE      = QAH::H1F("QaGtrkNPntFE",    "globtrk: N points on trk,ftpc east", 8, 4.,12.);
  m_pointFW      = QAH::H1F("QaGtrkNPntFW",    "globtrk: N points on trk,ftpc west", 8, 4.,12.);
  m_max_pointFE  = QAH::H1F("QaGtrkNPntMaxFE", "globtrk: N max pnts on trk, ftpc east", 8, 4.,12.);
  m_max_pointFW  = QAH::H1F("QaGtrkNPntMaxFW", "globtrk: N max pnts on trk, ftpc west", 8, 4.,12.);
  m_glb_ratiomFE = QAH::H1F("QaGtrkRnmFE",     "globtrk: ratio Nfit/max pnt, ftpc east", 55, 0., 1.1);
  m_glb_ratiomFW = QAH::H1F("QaGtrkRnmFW",     "globtrk: ratio Nfit/max pnt, ftpc west", 55, 0., 1.1);
  m_glb_chargeFE = QAH::H1F("QaGtrkChrgFE",    "globtrk: charge, ftpc east ", 20,-2.,2.);
  m_glb_chargeFW = QAH::H1F("QaGtrkChrgFW",    "globtrk: charge, ftpc west ", 20,-2.,2.);
  m_glb_xfFE     = QAH::H1F("QaGtrkXfFE",      "globtrk: x of first hit on trk, ftpc east", 70,-35.,35.);
  m_glb_xfFW     = QAH::H1F("QaGtrkXfFW",      "globtrk: x of first hit on trk, ftpc west", 70,-35.,35.);
  m_glb_yfFE     = QAH::H1F("QaGtrkYfFE",      "globtrk: y of first hit on trk, ftpc east", 70,-35.,35.);
  m_glb_yfFW     = QAH::H1F("QaGtrkYfFW",      "globtrk: y of first hit on trk, ftpc west", 70,-35.,35.);
  m_glb_zfFE     = QAH::H1F("QaGtrkZfFE",      "globtrk: z of first hit on trk, ftpc east", 75,-225.,-150.);
  m_glb_zfFW     = QAH::H1F("QaGtrkZfFW",      "globtrk: z of first hit on trk, ftpc west", 75,150.,225.);
  m_glb_radfFE   = QAH::H1F("QaGtrkRFE",       "globtrk: radial position of first hit, ftpc east",35,0.,35.);
  m_glb_radfFW   = QAH::H1F("QaGtrkRFW",       "globtrk: radial position of first hit, ftpc west",35,0.,35.);
  m_lengthFE     = QAH::H1F("QaGtrkLengthFE",  "globtrk: track length, ftpc east", 60,0.,120.);
  m_lengthFW     = QAH::H1F("QaGtrkLengthFW",  "globtrk: track length, ftpc west", 60,0.,120.);
  m_psiFE        = QAH::H1F("QaGtrkPsiFE",     "globtrk: psi, ftpc east", 90, 0.,360.);
  m_psiFW        = QAH::H1F("QaGtrkPsiFW",     "globtrk: psi, ftpc west", 90, 0.,360.);
  m_etaFE        = QAH::H1F("QaGtrkEtaFE",     "globtrk: eta, ftpc east",80,-4.5,-2.);
  m_etaFW        = QAH::H1F("QaGtrkEtaFW",     "globtrk: eta, ftpc west",80,2.,4.5);
  m_pTFE         = QAH::H1F("QaGtrkPtFE",      "globtrk: pT, ftpc east",50,0.,10.);
  m_pTFW         = QAH::H1F("QaGtrkPtFW",      "globtrk: pT, ftpc west",50,0.,10.);
  m_momFE        = QAH::H1F("QaGtrkPFE",       "globtrk: momentum, ftpc east ",50,0.,5.);
  m_momFW        = QAH::H1F("QaGtrkPFW",       "globtrk: momentum, ftpc west ",50,0.,5.);
  m_chisq0FE     = QAH::H1F("QaGtrkChisq0FE",  "globtrk: chi2/ndf x,y fit, ftpc east", 50, 0.,5.);
  m_chisq0FW     = QAH::H1F("QaGtrkChisq0FW",  "globtrk: chi2/ndf x,y fit, ftpc west", 50, 0.,5.);
  m_chisq1FE     = QAH::H1F("QaGtrkChisq1FE",  "globtrk: chi2/ndf r,z fit, ftpc east", 50, 0.,5.);
  m_chisq1FW     = QAH::H1F("QaGtrkChisq1FW",  "globtrk: chi2/ndf r,z fit, ftpc west", 50, 0.,5.);

// 2D - ftpc

   m_pT_eta_recFE = QAH::H2F("QaGtrkPtVsEtaFE","globtrk: log10 pT vs eta, ftpcE",20,-4.5,-2.,40,1.,4.);
    m_pT_eta_recFE->SetXTitle("eta");
    m_pT_eta_recFE->SetYTitle("log10 pT");
   m_pT_eta_recFW = QAH::H2F("QaGtrkPtVsEtaFW","globtrk: log10 pT vs eta, ftpcW",20,2.,4.5,40,1.,4.);
    m_pT_eta_recFW->SetXTitle("eta");
    m_pT_eta_recFW->SetYTitle("log10 pT");
  m_globtrk_xf_yfFE = QAH::H2F("QaGtrkXfYfFE","globtrk: Y vs X of first hit on trk, ftpcE", 70,-35.,35.,70,-35.,35.);
    m_globtrk_xf_yfFE->SetXTitle("x first");
    m_globtrk_xf_yfFE->SetYTitle("y first");
  m_globtrk_xf_yfFW = QAH::H2F("QaGtrkXfYfFW","globtrk: Y vs X of first hit on trk, ftpcW", 70,-35.,35.,70,-35.,35.);
    m_globtrk_xf_yfFW->SetXTitle("x first");
    m_globtrk_xf_yfFW->SetYTitle("y first");
  m_globtrk_padtimeFE = QAH::H2F("QaGtrkPadTimeFtpcE","globtrk: #pads vs #timebins, ftpcE",12,0.5,12.5,10,0.5,10.5);
//  m_globtrk_padtimeFE = QAH::H2F("QaGtrkPadTimeFtpcE","globtrk: #pads vs #timebins of hits, ftpcE",12,0.5,12.5,10,0.5,10.5);
    m_globtrk_padtimeFE->SetXTitle("#timebins");
    m_globtrk_padtimeFE->SetYTitle("#pads");
  m_globtrk_padtimeFW = QAH::H2F("QaGtrkPadTimeFtpcW","globtrk: #pads vs #timebins, ftpcW",12,0.5,12.5,10,0.5,10.5);
//  m_globtrk_padtimeFW = QAH::H2F("QaGtrkPadTimeFtpcW","globtrk: #pads vs #timebins of hits, ftpcW",12,0.5,12.5,10,0.5,10.5);
    m_globtrk_padtimeFW->SetXTitle("#timebins");
    m_globtrk_padtimeFW->SetYTitle("#pads");
  m_eta_trklengthFE = QAH::H2F("QaGtrkLengthVEtaFE","globtrk: trk length vs eta, ftpcE",25,-4.5,-2.,30,0.,120.);
    m_eta_trklengthFE->SetXTitle("eta");
    m_eta_trklengthFE->SetYTitle("length");
  m_eta_trklengthFW = QAH::H2F("QaGtrkLengthVEtaFW","globtrk: trk length vs eta, ftpcW",25,2.,4.5,30,0.,120.);
    m_eta_trklengthFW->SetXTitle("eta");
    m_eta_trklengthFW->SetYTitle("length");
  m_npoint_lengthFE = QAH::H2F("QaGtrkNPntLengthFE","globtrk: N pnts vs length, ftpcE",30,0.,120.,15,0.,15.);
    m_npoint_lengthFE->SetXTitle("trk length");
    m_npoint_lengthFE->SetYTitle("Npoints on trk");
  m_npoint_lengthFW = QAH::H2F("QaGtrkNPntLengthFW","globtrk: N pnts vs length, ftpcW",30,0.,120.,15,0.,15.);
    m_npoint_lengthFW->SetXTitle("trk length");
    m_npoint_lengthFW->SetYTitle("Npoints on trk");

  } // ftpHists

  m_global_pxl_hit = QAH::H1F("QaGtrkPxlHit","PIXEL: Hits per global track",5,-0.5,4.5);
  m_global_pxl_hit->SetXTitle("hits in PIXEL");
  m_global_ist_hit = QAH::H1F("QaGtrkIstHit","IST: Hits per global track",5,-0.5,4.5);
  m_global_ist_hit->SetXTitle("hits in IST");
  m_global_hft_hit = QAH::H1F("QaGtrkHftHit","HFT: Hits per global track",5,-0.5,4.5);
  m_global_hft_hit->SetXTitle("hits in HFT subsystem");

  m_global_hft_hit->GetXaxis()->SetBinLabel(1,"");
  m_global_hft_hit->GetXaxis()->SetBinLabel(2,"PIXEL (inner)");
  m_global_hft_hit->GetXaxis()->SetBinLabel(3,"PIXEL (outer)");
  m_global_hft_hit->GetXaxis()->SetBinLabel(4,"IST");
  m_global_hft_hit->GetXaxis()->SetBinLabel(5,"SSD");

}
//____________________________________________________
void StQABookHist::BookHistPrim(){

// for method MakeHistPrim - from table primtrk

// 1D
  m_primtrk_tot     = QAH::H1F("QaPtrkTot",   "primtrk: tot num tracks",50,0.,5000.);
  m_primtrk_tot_sm  = QAH::H1F("QaPtrkTotsm", "primtrk: tot num tracks",50,0.,200.);
  m_primtrk_iflag   = QAH::H1F("QaPtrkFlag",  "primtrk: iflag - all",160,-799.,900.);
  m_primtrk_good    = QAH::H1F("QaPtrkGood",  "primtrk: tot num tracks iflag>0",50,0.,5000.);
  m_primtrk_good_sm = QAH::H1F("QaPtrkGoodsm","primtrk: tot num tracks iflag>0",50,0.,200.);
  if (silHists)
    m_primtrk_goodTTS = QAH::H1F("QaPtrkGoodTTS","primtrk: tot num tracks iflag>0, tpc,svt",150,0.,4500.);
  m_primtrk_goodF   = QAH::H2F("QaPtrkGoodF",  "primtrk: tot num tracks iflag>0, ftpc",150,0.,1500.,150,0.,1500.);
  m_primtrk_goodF->SetXTitle("East");
  m_primtrk_goodF->SetYTitle("West");
  m_primglob_good   = QAH::H1F("QaPtrkGlob","primtrk: ratio primary/global tracks w/ iflag>0",50,0,1);
  m_primglob_fit    = QAH::H1F("QaPtrkGlobFit","primtrk: ratio primary/global nfit points",50,0,2);
  m_pdet_id         = QAH::H1F("QaPtrkDetId",  "primtrk: Detector ID good tracks - all",48,-0.5,47.5);
  if (silHists) {
    m_primtrk_meanptTTS = QAH::MH1F("QaPtrkMeanPtTTS","primtrk: <pT>, tpc, tpc+svt",50,0.,2.,2);
    m_primtrk_meanptTTS->Rebin(0,"TPC+SVT");
    m_primtrk_meanptTTS->Rebin(1,"TPC");
    m_primtrk_meanptTTS->SetStats(kFALSE);
  }
  m_primtrk_meanptF = QAH::MH1F("QaPtrkMeanPtF","primtrk: <pT>, ftpc",50,0.,2.,2);
  m_primtrk_meanptF->Rebin(0,"East");
  m_primtrk_meanptF->Rebin(1,"West");
  m_primtrk_meanptF->SetStats(kFALSE);
  if (silHists) {
    m_primtrk_meanetaTTS = QAH::MH1F("QaPtrkMeanEtaTTS","primtrk: <eta>, tpc, tpc+svt",40,-2.,2.,2);
    m_primtrk_meanetaTTS->Rebin(0,"TPC+SVT");
    m_primtrk_meanetaTTS->Rebin(1,"TPC");
    m_primtrk_meanetaTTS->SetStats(kFALSE);
  }
  m_primtrk_meanetaF = QAH::MH1F("QaPtrkMeanEtaF","primtrk: |<eta>|, ftpc",40,2,5,2);
  m_primtrk_meanetaF->Rebin(0,"East");
  m_primtrk_meanetaF->Rebin(1,"West");
  m_primtrk_meanetaF->SetStats(kFALSE);
  if (silHists) {
    m_ppsiTTS         = QAH::MH1F("QaPtrkPsiTTS","primtrk: psi (deg), tpc, svt", 36, 0.,360.,2);
    m_ppsiTTS->Rebin(0,"TPC+SVT");
    m_ppsiTTS->Rebin(1,"TPC");
    m_ppsiTTS->SetStats(kFALSE);
    m_petaTTS         = QAH::MH1F("QaPtrkEtaTTS","primtrk: eta, tpc,svt",40,-2.,2.,2);
    m_petaTTS->Rebin(0,"TPC+SVT");
    m_petaTTS->Rebin(1,"TPC");
    m_petaTTS->SetStats(kFALSE);
    m_ppTTTS          = QAH::MH1F("QaPtrkPtTTS", "primtrk: pT, tpc,svt",50,0.,5.,2);
    m_ppTTTS->Rebin(0,"TPC+SVT");
    m_ppTTTS->Rebin(1,"TPC");
    m_ppTTTS->SetStats(kFALSE);
    m_pchisq0TTS     = QAH::MH1F("QaPtrkChisq0TTS", "primtrk: chisq0, tpc,svt", 50, 0.,5.,2);
    m_pchisq0TTS->Rebin(0,"TPC+SVT");
    m_pchisq0TTS->Rebin(1,"TPC");
    m_pchisq0TTS->SetStats(kFALSE);
    m_pfpoint_lengthTTS = QAH::H2F("QaPtrkFitPntLTTS","primtrk: N fit pnts vs length, tpc,tpc+svt",25,70.,350.,25,0.,50.);
    m_pfpoint_lengthTTS->SetXTitle("trk length");
    m_pfpoint_lengthTTS->SetYTitle("Npoints on trk");
  }

// 1D tpc
  m_ppointT      = QAH::H1F("QaPtrkNPntT",   "primtrk: N points on trk,tpc", 81, -0.5,80.5);
  m_pmax_pointT  = QAH::H1F("QaPtrkNPntMaxT","primtrk: N max pnts on trk, tpc", 76, -1,151);
  m_pfit_pointT  = QAH::H1F("QaPtrkNPntFitT","primtrk: N fit pnts on trk, tpc", 81, -0.5,80.5);
  m_prim_ratiomT = QAH::H1F("QaPtrkRnmT",    "primtrk: ratio Nfit/max pnt, tpc", 55, 0., 1.1);
  m_prim_chargeT = QAH::H1F("QaPtrkChrgT",   "primtrk: charge, tpc ", 20,-2.,2.);
  m_prim_r0T     = QAH::H1F("QaPtrkR0T",     "primtrk: radius at start (cm), tpc ", 50,0.,.1);
  m_prim_phi0T   = QAH::H1F("QaPtrkPhi0T",   "primtrk: azimuth (phi) at start (deg,force 0,360), tpc ", 64,0.,360.);
  m_prim_z0T     = QAH::H1F("QaPtrkZ0T",     "primtrk: z-coord at start (cm), tpc ", 50, -300.,300.);
  m_prim_curvT   = QAH::H1F("QaPtrkCurvT",   "primtrk: log10 curvature (1/cm), tpc ", 80,-3.5,0.5);
  m_prim_f0      = QAH::MH1F("QaPtrkf0",     "primtrk: first point: hit-helix, tpc",60,-3.,3.,3);
  m_prim_f0->Rebin(0,"X");
  m_prim_f0->Rebin(1,"Y");
  m_prim_f0->Rebin(2,"Z");
  m_prim_xfT     = QAH::H1F("QaPtrkXfT",     "primtrk: x of first hit on trk, tpc", 50,-200.,200.);
  m_prim_xf0     = QAH::H1F("QaPtrkXf0",     "primtrk: first point: x_hit - x_helix, tpc",60,-3.,3.);
  m_prim_yfT     = QAH::H1F("QaPtrkYfT",     "primtrk: y of first hit on trk, tpc", 50,-200.,200.);
  m_prim_yf0     = QAH::H1F("QaPtrkYf0",     "primtrk: first point: y_hit - y_helix, tpc",60,-3.,3.);
  m_prim_zfT     = QAH::H1F("QaPtrkZfT",     "primtrk: z of first hit on trk, tpc", 50,-200.,200.);
  m_prim_zf0     = QAH::H1F("QaPtrkZf0",     "primtrk: first point: z_hit - z_helix, tpc",60,-3.,3.);
  m_prim_radfT   = QAH::H1F("QaPtrkRT",      "primtrk: radial position of first hit, tpc", 50,0.,200.);
  m_prim_rzf0    = QAH::MH1F("QaPtrkRZf0",   "primtrk: first point: hit - helix (r,z), tpc",60,-3.,3.,2);
  m_prim_rzf0->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_prim_rzf0->Rebin(1,"z_{dif}");
  m_prim_rzf0->SetStats(kFALSE);
  m_prim_rzl0    = QAH::MH1F("QaPtrkRZl0",   "primtrk: last point: hit - helix (r,z), tpc",60,-3.,3.,2);
  m_prim_rzl0->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_prim_rzl0->Rebin(1,"z_{dif}");
  m_prim_rzl0->SetStats(kFALSE);
  m_plengthT     = QAH::H1F("QaPtrkLengthT", "primtrk: track length, tpc", 50,0.,300.);
  m_ppsiT        = QAH::H1F("QaPtrkPsiT",    "primtrk: psi, tpc (deg)", 36, 0.,360.);
  m_ptanlT       = QAH::H1F("QaPtrkTanlT",   "primtrk: tanl, tpc",32,-4.,4.);
  m_prim_thetaT  = QAH::H1F("QaPtrkThetaT",  "primtrk: theta (degrees), tpc",36,0.,180.);
  m_petaT        = QAH::H1F("QaPtrkEtaT",    "primtrk: eta, tpc",40,-2.,2.);
  m_ppTT         = QAH::H1F("QaPtrkPtT",     "primtrk: pT, tpc",50,0.,5.);
  m_pmomT        = QAH::H1F("QaPtrkPT",      "primtrk: momentum, tpc",50,0.,5.);
  m_pchisq0T     = QAH::H1F("QaPtrkChisq0T", "primtrk: chisq0, tpc", 50, 0.,5.);

// 2D - tpc
  m_ppT_eta_recT = QAH::H2F("QaPtrkPtVsEtaT","primtrk: log10 pT vs eta, tpc", 20,-2.,2.,40,1.,4.);
    m_ppT_eta_recT->SetXTitle("eta");
    m_ppT_eta_recT->SetYTitle(" log10 pT (MeV)");
  m_primtrk_xf_yfTE = QAH::H2F("QaPtrkXfYfTE",  "primtrk: Y vs X of first hit on trk, tpc east", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yfTE->SetXTitle("x first");
    m_primtrk_xf_yfTE->SetYTitle("y first");
  m_primtrk_xf_yfTW = QAH::H2F("QaPtrkXfYfTW",  "primtrk: Y vs X of first hit on trk, tpc west", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yfTW->SetXTitle("x first");
    m_primtrk_xf_yfTW->SetYTitle("y first");
  m_ptanl_zfT = QAH::H2F("QaPtrkTanlzf","primtrk: tanl(dip) versus zfirst-zvtx, tpc,tpc+svt",60,-3.,3.,60,-3.,3.);
    m_ptanl_zfT->SetXTitle("zfirst-zvtx");
    m_ptanl_zfT->SetYTitle("tanl");
  m_pmom_trklengthT = QAH::H2F("QaPtrkPVsTrkLength","primtrk: log10 mom vs trk length, tpc",50,70.,350.,40,1.,4.);
    m_pmom_trklengthT->SetXTitle("trk length");  
    m_pmom_trklengthT->SetYTitle("log10 P (MeV)");
  m_peta_trklengthT = QAH::H2F("QaPtrkLengthVEtaT","primtrk: trk length vs eta, tpc",20,-2.,2.,50,70.,350.);
    m_peta_trklengthT->SetXTitle("eta");
    m_peta_trklengthT->SetYTitle("length");
  m_pnpoint_lengthT = QAH::H2F("QaPtrkNPntLengthT","primtrk: N pnts vs length, tpc",25,70.,350.,40,1.,81.);
    m_pnpoint_lengthT->SetXTitle("trk length");
    m_pnpoint_lengthT->SetYTitle("Npoints on trk");
  m_pfpoint_lengthT = QAH::H2F("QaPtrkFitPntLengthT","primtrk: N fit pnts vs length, tpc",25,70.,350.,40,1.,81.);
    m_pfpoint_lengthT->SetXTitle("trk length");
    m_pfpoint_lengthT->SetYTitle("Npoints on trk");
  m_pchisq0_momT = QAH::H2F("QaPtrkChi0MomT","primtrk: Chisq0 vs log10 mom, tpc",40,1.,4.,50,0.,5.);
    m_pchisq0_momT->SetXTitle("log10 P (MeV)");
    m_pchisq0_momT->SetYTitle("chisq0") ;
  m_pchisq0_etaT = QAH::H2F("QaPtrkChi0EtaT","primtrk: Chisq0 vs eta, tpc",20,-2.,2.,20,0.,5.);
    m_pchisq0_etaT->SetXTitle("eta");
    m_pchisq0_etaT->SetYTitle("chisq0");
  m_pchisq0_dipT = QAH::H2F("QaPtrkChi0TanlT","primtrk: Chisq0 vs tanl(dip), tpc",20,-3.2,3.2,20,0.,5.);
    m_pchisq0_dipT->SetXTitle("dip angle");
    m_pchisq0_dipT->SetYTitle("chisq0");
  m_pchisq0_zfT = QAH::H2F("QaPtrkChi0zfT","primtrk: Chisq0 vs zfirst, tpc",20,-200.,200.,20,0.,5.);
    m_pchisq0_zfT->SetXTitle("zfirst");
    m_pchisq0_zfT->SetYTitle("chisq0");
  m_ppsi_phiT = QAH::H2F("QaPtrkPsiPhiT","primtrk: psi vs phi, tpc",36, 0.,360.,36,0.,360.);
     m_ppsi_phiT->SetXTitle("phi");
     m_ppsi_phiT->SetYTitle("psi");

  if (silHists) {
// 1D tpc + silicon (svt+ssd)
  m_ppointTS      = QAH::H1F("QaPtrkNPntTS",   "primtrk: N points on trk,tpc+svt", 60, 0.,60.);
  m_pmax_pointTS  = QAH::H1F("QaPtrkNPntMaxTS","primtrk: N max pnts on trk, tpc+svt", 50, 0.,100.);
  m_pfit_pointTS  = QAH::H1F("QaPtrkNPntFitTS","primtrk: N fit pnts on trk, tpc+svt", 60, 0.,60.);
  m_prim_ratiomTS = QAH::H1F("QaPtrkRnmTS",    "primtrk: ratio Nfit/max pnt, tpc+svt", 55, 0., 1.2005);
  m_prim_chargeTS = QAH::H1F("QaPtrkChrgTS",   "primtrk: charge, tpc+svt ", 20,-2.,2.);
  m_prim_r0TS     = QAH::H1F("QaPtrkR0TS",     "primtrk: radius at start (cm), tpc+svt", 50,0.,0.1);
  m_prim_phi0TS   = QAH::H1F("QaPtrkPhi0TS",   "primtrk: azimuth (phi) at start (deg,force 0-360),tpc+svt", 64, 0.,360.);
  m_prim_z0TS     = QAH::H1F("QaPtrkZ0TS",     "primtrk: z-coord at start (cm), tpc+svt", 50, -50.,50.);
  m_prim_curvTS   = QAH::H1F("QaPtrkCurvTS",   "primtrk: log10 curvature (1/cm), tpc+svt", 80,-3.5,0.5);
  m_prim_f0TS     = QAH::MH1F("QaPtrkf0TS",    "primtrk: first point: hit-helix, tpc+svt",50,-5.,5.,3);
  m_prim_f0TS->Rebin(0,"X");
  m_prim_f0TS->Rebin(1,"Y");
  m_prim_f0TS->Rebin(2,"Z");
  m_prim_f0TS->SetStats(kFALSE);
  m_prim_xfTS     = QAH::H1F("QaPtrkXfTS",     "primtrk: x of first hit on trk, tpc+svt", 50,-200.,200.);
  m_prim_xf0TS    = QAH::H1F("QaPtrkXf0TS",    "primtrk: first point: x_hit - x_helix, tpc+svt",50,-5.,5.);
  m_prim_yfTS     = QAH::H1F("QaPtrkYfTS",     "primtrk: y of first hit on trk, tpc+svt", 50,-200.,200.);
  m_prim_yf0TS    = QAH::H1F("QaPtrkYf0TS",    "primtrk: first point: y_hit - y_helix, tpc+svt",50,-5.,5.);
  m_prim_zfTS     = QAH::H1F("QaPtrkZfTS",     "primtrk: z of first hit on trk, tpc+svt", 50,-200.,200.);
  m_prim_zf0TS    = QAH::H1F("QaPtrkZf0TS",    "primtrk: first point: z_hit - z_helix, tpc+svt",50,-5.,5.);
  m_prim_radfTS   = QAH::H1F("QaPtrkRTS",      "primtrk: radial position of first hit, tpc+svt", 50,0.,200.);
  m_prim_rzf0TS   = QAH::MH1F("QaPtrkRZf0TS",  "primtrk: first point: hit - helix (r,z), tpc+svt",60,-3.,3.,2);
  m_prim_rzf0TS->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_prim_rzf0TS->Rebin(1,"z_{dif}");
  m_prim_rzf0TS->SetStats(kFALSE);
  m_prim_rzl0TS   = QAH::MH1F("QaPtrkRZl0TS",  "primtrk: last point: hit - helix (r,z), tpc+svt",60,-3.,3.,2);
  m_prim_rzl0TS->Rebin(0,"#pm #sqrt{ x_{dif}^{2}+y_{dif}^{2}}");
  m_prim_rzl0TS->Rebin(1,"z_{dif}");
  m_prim_rzl0TS->SetStats(kFALSE);
  m_plengthTS     = QAH::H1F("QaPtrkLengthTS", "primtrk: track length, tpc+svt", 50,0.,300.);
  m_ppsiTS        = QAH::H1F("QaPtrkPsiTS",    "primtrk: psi, tpc+svt (deg) ", 36, 0.,360.);
  m_ptanlTS       = QAH::H1F("QaPtrkTanlTS",   "primtrk: tanl, tpc+svt",32,-4.,4.);
  m_prim_thetaTS  = QAH::H1F("QaPtrkThetaTS",  "primtrk: theta (degrees), tpc+svt",36,0.,180.);
  m_petaTS        = QAH::H1F("QaPtrkEtaTS",    "primtrk: eta, tpc+svt",40,-2.,2.);
  m_ppTTS         = QAH::H1F("QaPtrkPtTS",     "primtrk: pT, tpc+svt",50,0.,5.);
  m_pmomTS        = QAH::H1F("QaPtrkPTS",      "primtrk: momentum, tpc+svt",50,0.,5.);
  m_pchisq0TS     = QAH::H1F("QaPtrkChisq0TS", "primtrk: chisq0, tpc+svt", 50, 0.,5.);
  m_prim_ssd_phi  = QAH::H1F("QaPtrkPhifSSD",  "primtrk: phi of ssd point (per event)",64,0,360);

// 2D - tpc + silicon (svt + ssd)
  m_ppT_eta_recTS = QAH::H2F("QaPtrkPtVsEtaTS","primtrk: log10 pT vs eta, tpc+svt", 20,-2.,2.,40,1.,4.);
    m_ppT_eta_recTS->SetXTitle("eta");
    m_ppT_eta_recTS->SetYTitle(" log10 pT (MeV)");
  m_primtrk_xf_yfTS = QAH::H2F("QaPtrkXfYfTS",  "primtrk: Y vs X of first hit on trk, tpc+svt", 40,-200.,200.,40,-200.,200.);
    m_primtrk_xf_yfTS->SetXTitle("x first");
    m_primtrk_xf_yfTS->SetYTitle("y first");
  m_ptanl_zfTS = QAH::H2F("QaPtrkTanlzfTS","primtrk: tanl(dip) versus zfirst-zvtx, svt",60,-3.,3.,60,-3.,3.);
    m_ptanl_zfTS->SetXTitle("zfirst-zvtx");
    m_ptanl_zfTS->SetYTitle("tanl");
  m_pmom_trklengthTS = QAH::H2F("QaPtrkPVsTrkLTS","primtrk: log10 mom vs trk length, tpc+svt",50,70.,350.,40,1.,4.);
    m_pmom_trklengthTS->SetXTitle("trk length");  
    m_pmom_trklengthTS->SetYTitle("log10 P (MeV)");
  m_peta_trklengthTS = QAH::H2F("QaPtrkLVEtaTS","primtrk: trk length vs eta, tpc+svt",20,-2.,2.,50,70.,350.);
    m_peta_trklengthTS->SetXTitle("eta");
    m_peta_trklengthTS->SetYTitle("length");
  m_pnpoint_lengthTS = QAH::H2F("QaPtrkNPntLTS","primtrk: N pnts vs length, tpc+svt",25,70.,350.,25,0.,50.);
    m_pnpoint_lengthTS->SetXTitle("trk length");
    m_pnpoint_lengthTS->SetYTitle("Npoints on trk");
  m_pfpoint_lengthTS = QAH::H2F("QaPtrkFitPntLTS","primtrk: N fit pnts vs length, tpc+svt",25,70.,350.,25,0.,50.);
    m_pfpoint_lengthTS->SetXTitle("trk length");
    m_pfpoint_lengthTS->SetYTitle("Npoints on trk");
  m_pchisq0_momTS = QAH::H2F("QaPtrkChi0MomTS","primtrk: Chisq0 vs log10 mom, tpc+svt",40,1.,4.,50,0.,5.);
    m_pchisq0_momTS->SetXTitle("log10 P (MeV)");
    m_pchisq0_momTS->SetYTitle("chisq0") ;
  m_pchisq0_etaTS = QAH::H2F("QaPtrkChi0EtaTS","primtrk: Chisq0 vs eta, tpc+svt",20,-2.,2.,20,0.,5.);
    m_pchisq0_etaTS->SetXTitle("eta");
    m_pchisq0_etaTS->SetYTitle("chisq0");
  m_pchisq0_dipTS = QAH::H2F("QaPtrkChi0TanlTS","primtrk: Chisq0 vs tanl(dip), tpc+svt",20,-3.2,3.2,20,0.,5.);
    m_pchisq0_dipTS->SetXTitle("dip angle");
    m_pchisq0_dipTS->SetYTitle("chisq0");
  m_pchisq0_zfTS = QAH::H2F("QaPtrkChi0zfTS","primtrk: Chisq0 vs zfirst, tpc+svt",20,-200.,200.,20,0.,5.);
    m_pchisq0_zfTS->SetXTitle("zfirst");
    m_pchisq0_zfTS->SetYTitle("chisq0");
  m_ppsi_phiTS = QAH::H2F("QaPtrkPsiPhiTS","primtrk: psi vs phi, tpc+svt",36, 0.,360.,36,0.,360.);
    m_ppsi_phiTS->SetXTitle("phi");
    m_ppsi_phiTS->SetYTitle("psi");
  } // silHists

  if (ftpHists) { // FTPC hists

// 1D ftpc
  // east (solid) and west(dashed) on same plot
  m_ppointF      = QAH::MH1F("QaPtrkNPntF",    "primtrk: N points on trk,ftpc",15,0.,15.,2);
  m_ppointF->Rebin(0,"East");
  m_ppointF->Rebin(1,"West");
  m_ppointF->SetStats(kFALSE);
  m_pmax_pointF  = QAH::MH1F("QaPtrkNPntMaxF", "primtrk: N max pnts on trk, ftpc",15,0.,15.,2);
  m_pmax_pointF->Rebin(0,"East");
  m_pmax_pointF->Rebin(1,"West");
  m_pmax_pointF->SetStats(kFALSE);
  m_prim_ratiomF = QAH::MH1F("QaPtrkRnmF",     "primtrk: ratio Nfit/max pnt, ftpc",55,0.,1.1,2);
  m_prim_ratiomF->Rebin(0,"East");
  m_prim_ratiomF->Rebin(1,"West");
  m_prim_ratiomF->SetStats(kFALSE);
  m_prim_chargeF = QAH::MH1F("QaPtrkChrgF",    "primtrk: charge, ftpc",20,-2.,2.,2);
  m_prim_chargeF->Rebin(0,"East");
  m_prim_chargeF->Rebin(1,"West");
  m_prim_chargeF->SetStats(kFALSE);
  m_prim_xfF     = QAH::MH1F("QaPtrkXfF",      "primtrk: x of first hit on trk, ftpc",70,-35.,35.,2);
  m_prim_xfF->Rebin(0,"East");
  m_prim_xfF->Rebin(1,"West");
  m_prim_xfF->SetStats(kFALSE);
  m_prim_yfF     = QAH::MH1F("QaPtrkYfF",      "primtrk: y of first hit on trk, ftpc",70,-35.,35.,2);
  m_prim_yfF->Rebin(0,"East");
  m_prim_yfF->Rebin(1,"West");
  m_prim_yfF->SetStats(kFALSE);
  m_prim_zfF     = QAH::MH1F("QaPtrkZfF",      "primtrk: z of first hit on trk, ftpc",75,-225.,225.,2);
  m_prim_zfF->Rebin(0,"East");
  m_prim_zfF->Rebin(1,"West");
  m_prim_zfF->SetStats(kFALSE);
  m_prim_radfF   = QAH::MH1F("QaPtrkRF",       "primtrk: radial position of first hit, ftpc",35,0.,35.,2);
  m_prim_radfF->Rebin(0,"East");
  m_prim_radfF->Rebin(1,"West");
  m_prim_radfF->SetStats(kFALSE);
  m_plengthF     = QAH::MH1F("QaPtrkLengthF",  "primtrk: track length, ftpc",50,150.,300.,2);
  m_plengthF->Rebin(0,"East");
  m_plengthF->Rebin(1,"West");
  m_plengthF->SetStats(kFALSE);
  m_ppsiF        = QAH::MH1F("QaPtrkPsiF",     "primtrk: psi, ftpc", 90, 0.,360.,2);
  m_ppsiF->Rebin(0,"East");
  m_ppsiF->Rebin(1,"West");
  m_ppsiF->SetStats(kFALSE);
  m_petaF        = QAH::MH1F("QaPtrkEtaF",     "primtrk: |eta|, ftpc",80,2,5,2);
  m_petaF->Rebin(0,"East");
  m_petaF->Rebin(1,"West");
  m_petaF->SetStats(kFALSE);
  m_ppTF         = QAH::MH1F("QaPtrkPtF",      "primtrk: pT, ftpc",50,0.,5.,2);
  m_ppTF->Rebin(0,"East");
  m_ppTF->Rebin(1,"West");
  m_ppTF->SetStats(kFALSE);
  m_pmomF        = QAH::MH1F("QaPtrkPF",       "primtrk: momentum, ftpc",50,0.,5.,2);
  m_pmomF->Rebin(0,"East");
  m_pmomF->Rebin(1,"West");
  m_pmomF->SetStats(kFALSE);
  m_pchisq0F     = QAH::MH1F("QaPtrkChisq0F",  "primtrk: chi2/ndf x,y fit, ftpc",50,0.,5.,2);
  m_pchisq0F->Rebin(0,"East");
  m_pchisq0F->Rebin(1,"West");
  m_pchisq0F->SetStats(kFALSE);
  m_pchisq1F     = QAH::MH1F("QaPtrkChisq1F",  "primtrk: chi2/ndf r,z fit, ftpc",50,0.,5.,2);
  m_pchisq1F->Rebin(0,"East");
  m_pchisq1F->Rebin(1,"West");
  m_pchisq1F->SetStats(kFALSE);
  // separate east and west histograms
  m_ppointFE      = QAH::H1F("QaPtrkNPntFE",    "primtrk: N points on trk,ftpc east", 15, 0.,15.);
  m_ppointFW      = QAH::H1F("QaPtrkNPntFW",    "primtrk: N points on trk,ftpc west", 15, 0.,15.);
  m_pmax_pointFE  = QAH::H1F("QaPtrkNPntMaxFE", "primtrk: N max pnts on trk, ftpc east", 15, 0.,15.);
  m_pmax_pointFW  = QAH::H1F("QaPtrkNPntMaxFW", "primtrk: N max pnts on trk, ftpc west", 15, 0.,15.);
  m_prim_ratiomFE = QAH::H1F("QaPtrkRnmFE",     "primtrk: ratio Nfit/max pnt, ftpc east", 55, 0., 1.1);
  m_prim_ratiomFW = QAH::H1F("QaPtrkRnmFW",     "primtrk: ratio Nfit/max pnt, ftpc west", 55, 0., 1.1);
  m_prim_chargeFE = QAH::H1F("QaPtrkChrgFE",    "primtrk: charge, ftpc east ", 20,-2.,2.);
  m_prim_chargeFW = QAH::H1F("QaPtrkChrgFW",    "primtrk: charge, ftpc west ", 20,-2.,2.);
  m_prim_xfFE     = QAH::H1F("QaPtrkXfFE",      "primtrk: x of first hit on trk, ftpc east",70,-35.,35.);
  m_prim_xfFW     = QAH::H1F("QaPtrkXfFW",      "primtrk: x of first hit on trk, ftpc west",70,-35.,35.);
  m_prim_yfFE     = QAH::H1F("QaPtrkYfFE",      "primtrk: y of first hit on trk, ftpc east",70,-35.,35.);
  m_prim_yfFW     = QAH::H1F("QaPtrkYfFW",      "primtrk: y of first hit on trk, ftpc west",70,-35.,35.);
  m_prim_zfFE     = QAH::H1F("QaPtrkZfFE",      "primtrk: z of first hit on trk, ftpc east",75,-225.,-150.);
  m_prim_zfFW     = QAH::H1F("QaPtrkZfFW",      "primtrk: z of first hit on trk, ftpc west",75,150.,225.);
  m_prim_radfFE   = QAH::H1F("QaPtrkRFE",       "primtrk: radial position of first hit, ftpc east",35,0.,35.);
  m_prim_radfFW   = QAH::H1F("QaPtrkRFW",       "primtrk: radial position of first hit, ftpc west",35,0.,35.);
  m_plengthFE     = QAH::H1F("QaPtrkLengthFE",  "primtrk: track length, ftpc east",50,150.,300.);
  m_plengthFW     = QAH::H1F("QaPtrkLengthFW",  "primtrk: track length, ftpc west",50,150.,300.);
  m_ppsiFE        = QAH::H1F("QaPtrkPsiFE",     "primtrk: psi, ftpc east", 90, 0.,360.);
  m_ppsiFW        = QAH::H1F("QaPtrkPsiFW",     "primtrk: psi, ftpc west", 90, 0.,360.);
  m_petaFE        = QAH::H1F("QaPtrkEtaFE",     "primtrk: eta, ftpc east",80,-4.5,-2.);
  m_petaFW        = QAH::H1F("QaPtrkEtaFW",     "primtrk: eta, ftpc west",80,2.,4.5);
  m_ppTFE         = QAH::H1F("QaPtrkPtFE",      "primtrk: pT, ftpc east",50,0.,5.);
  m_ppTFW         = QAH::H1F("QaPtrkPtFW",      "primtrk: pT, ftpc west",50,0.,5.);
  m_pmomFE        = QAH::H1F("QaPtrkPFE",       "primtrk: momentum, ftpc east ",50,0.,5.);
  m_pmomFW        = QAH::H1F("QaPtrkPFW",       "primtrk: momentum, ftpc west ",50,0.,5.);
  m_pchisq0FE     = QAH::H1F("QaPtrkChisq0FE",  "primtrk: chi2/ndf x,y fit, ftpc east", 50, 0.,5.);
  m_pchisq0FW     = QAH::H1F("QaPtrkChisq0FW",  "primtrk: chi2/ndf x,y fit, ftpc west", 50, 0.,5.);
  m_pchisq1FE     = QAH::H1F("QaPtrkChisq1FE",  "primtrk: chi2/ndf r,z fit, ftpc east", 50, 0.,5.);
  m_pchisq1FW     = QAH::H1F("QaPtrkChisq1FW",  "primtrk: chi2/ndf r,z ft, ftpc west", 50, 0.,5.);

// 2D - ftpc
  m_ppT_eta_recFE = QAH::H2F("QaPtrkPtVsEtaFE","primtrk: log10 pT vs eta, ftpcE",20,-4.5,-2.,40,1.,4.);
  m_ppT_eta_recFW = QAH::H2F("QaPtrkPtVsEtaFW","primtrk: log10 pT vs eta, ftpcW",20,2.,4.5,40,1.,4.);
  m_primtrk_xf_yfFE = QAH::H2F("QaPtrkXfYfFE","primtrk: Y vs X of first hit on trk, ftpcE",70,-35.,35.,75,-35.,35.);
    m_primtrk_xf_yfFE->SetXTitle("x first");
    m_primtrk_xf_yfFE->SetYTitle("y first");
  m_primtrk_xf_yfFW = QAH::H2F("QaPtrkXfYfFW","primtrk: Y vs X of first hit on trk, ftpcW",70,-35.,35.,70,-35.,35.);
    m_primtrk_xf_yfFW->SetXTitle("x first");
    m_primtrk_xf_yfFW->SetYTitle("y first");
  m_peta_trklengthFE = QAH::H2F("QaPtrkLengthVEtaFE","primtrk: trk length vs eta, ftpcE",25,-4.5,-2.,50,0.,300.);
    m_peta_trklengthFE->SetXTitle("eta");
    m_peta_trklengthFE->SetYTitle("length");
  m_peta_trklengthFW = QAH::H2F("QaPtrkLengthVEtaFW","primtrk: trk length vs eta, ftpcW",25,2.,4.5,50,0.,300.);
    m_peta_trklengthFW->SetXTitle("eta");
    m_peta_trklengthFW->SetYTitle("length");
  m_pnpoint_lengthFE = QAH::H2F("QaPtrkNPntLengthFE","primtrk: N pnts vs length, ftpcE",50,0.,300.,15,0.,15.);
    m_pnpoint_lengthFE->SetXTitle("trk length");
    m_pnpoint_lengthFE->SetYTitle("Npoints on trk");
  m_pnpoint_lengthFW = QAH::H2F("QaPtrkNPntLengthFW","primtrk: N pnts vs length, ftpcW",50,0.,300.,15,0.,15.);
    m_pnpoint_lengthFW->SetXTitle("trk length");
    m_pnpoint_lengthFW->SetYTitle("Npoints on trk");

  } // ftpHists

// 2D - SVT drift length
  if (silHists)
    m_svt_loc = QAH::H2F("QaPtrkSvtLoc","primtrk: SVT hit time bins",256,0,128,432,-0.5,431.5);

  m_primary_pxl_hit = QAH::H1F("QaPtrkPxlHit","PIXEL: Hits per primary track",5,-0.5,4.5);
  m_primary_pxl_hit->SetXTitle("hits in PIXEL");
  m_primary_ist_hit = QAH::H1F("QaPtrkIstHit","IST: Hits per primary track",5,-0.5,4.5);
  m_primary_ist_hit->SetXTitle("hits in IST");
  m_primary_hft_hit = QAH::H1F("QaPtrkHftHit","HFT: Hits per primary track",5,-0.5,4.5);
  m_primary_hft_hit->SetXTitle("hits in HFT subsystem");

  m_primary_hft_hit->GetXaxis()->SetBinLabel(1,"");
  m_primary_hft_hit->GetXaxis()->SetBinLabel(2,"PIXEL (inner)");
  m_primary_hft_hit->GetXaxis()->SetBinLabel(3,"PIXEL (outer)");
  m_primary_hft_hit->GetXaxis()->SetBinLabel(4,"IST");
  m_primary_hft_hit->GetXaxis()->SetBinLabel(5,"SSD");

}
//_____________________________________________________________________________
void StQABookHist::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedxr  = QAH::H1F("QaDedxNum",     "dedx: number of tracks", 64,0., 16000.); 

  m_ndedxT   = QAH::H1F("QaDedxNdedxT", "dedx: number of point to define dE/dx, tpc",50,0., 50.);  
  m_dedx0T   = QAH::H1F("QaDedxDedx0T", "dedx: dE/dx mean (GeV/cm), tpc", ndedx, 0., 0.0005);
  m_dedx1T   = QAH::H1F("QaDedxDedx1T", "dedx: dE/dx error on mean, tpc", ndedx, 0., 1);
  //m_dedxTTS  = QAH::H1F("QaDedxBBTTS",  "dedx: ratio <dE/dx> mean to Bethe-Bloch <dE/dx>, tpc,tpc+svt",
  //			50,0.,10.);
  m_dedxTTS  = QAH::H1F("QaDedxBTTS",  "dedx: ratio <dE/dx> mean to Bichsel <dE/dx>, tpc,tpc+svt",
			50,0.,10.);
  // east and west on same plot
  m_ndedxF   = QAH::MH1F("QaDedxNdedxF", "dedx: number of point to define dE/dx, ftpc",50,0.,10.,2);
  m_ndedxF->Rebin(0,"East");
  m_ndedxF->Rebin(1,"West");
  m_ndedxF->SetStats(kFALSE);
  m_dedx0F   = QAH::MH1F("QaDedxDedx0F", "dedx: dE/dx mean, ftpc",ndedx,0.,750.,2);
  m_dedx0F->Rebin(0,"East");
  m_dedx0F->Rebin(1,"West");
  m_dedx0F->SetStats(kFALSE);
  // east and west on separate plots
  m_ndedxFE   = QAH::H1F("QaDedxNdedxFE", "dedx: number of point to define dE/dx, ftpcE",50,0.,10.);
  m_dedx0FE   = QAH::H1F("QaDedxDedx0FE", "dedx: dE/dx mean, ftpcE", ndedx,  0., 750.);
  
  m_ndedxFW   = QAH::H1F("QaDedxNdedxFW", "dedx: number of point to define dE/dx, ftpcW",50,0.,10.);  
  m_dedx0FW   = QAH::H1F("QaDedxDedx0FW", "dedx: dE/dx mean, ftpcW", ndedx,  0., 750.);
}
//_____________________________________________________________________________
void StQABookHist::BookHistPID(){
  
  // for MakeHistPID - from tables globtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  
  m_p_dedx_rec = QAH::H2F("QaPidGlobtrkDstdedxPVsDedx","PID: globtrk-dst_dedx,  p vs dedx (reconstructed)",
			  cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
  m_p_dedx_rec->SetXTitle("p (GeV)");
  m_p_dedx_rec->SetYTitle("dedx (keV/cm)");
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistVertex(){
  // for MakeHistVertex - from table dst_vertex
  
  
  m_v_num   = QAH::H1F("QaVtxNum",  " vertex: num vertices ",50,0.,10000.);
  m_v_num_sm= QAH::H1F("QaVtxNumsm",  " vertex: num vertices ",50,0.,500.);

  m_v_vtxid = QAH::H1F("QaVtxVtxId"," vertex,2ndary: Vertex ID ",10,0.,10.);
  m_v_x     = QAH::H1F("QaVtxX",    " vertex,2ndary: x ",50,-250.,250.);
  m_v_y     = QAH::H1F("QaVtxY",    " vertex,2ndary: y ",50,-250.,250.);
  m_v_z     = QAH::H1F("QaVtxZ",    " vertex,2ndary: z ",50,-250.,250.);
  m_v_pchi2 = QAH::H1F("QaVtxChisq"," vertex,2ndary: chisq/dof ",50,0.,50.);
  m_v_r     = QAH::H1F("QaVtxR",    " vertex,2ndary: r ",100,0,50);
  
  m_pv_vtxid = QAH::H1F("QaVtxPrVtxId"," vertex,prim: Vertex ID ",10,0.,10.);
  m_pv_x     = QAH::H1F("QaVtxPrX",    " vertex,prim: x ",50,-5.,5.);
  m_pv_y     = QAH::H1F("QaVtxPrY",    " vertex,prim: y ",50,-5.,5.);
  m_pv_z     = QAH::H1F("QaVtxPrZ",    " vertex,prim: z ",50,-50.,50.);
  m_pv_xy    = QAH::H2F("QaVtxPrXY",   " vertex,prim: x versus y",50,-5.,5.,50,-5.,5.);
  m_pv_pchi2 = QAH::H1F("QaVtxPrChisq"," vertex,prim: chisq/dof ",40,0.,20.);
  m_pv_r     = QAH::H1F("QaVtxPrR",    " vertex,prim: r ",100,0,0.1);

//  m_vtx_phi_dist  = QAH::H2F("QaV0VtxPhiDist",
//            "V0 azimuthal distribution",36,0.,360.,25,1.,101.);
//  m_vtx_phi_dist->SetXTitle("Mean of phi(V0)");
//  m_vtx_phi_dist->SetYTitle("RMS of phi(V0)");
  m_vtx_phi_dist  = QAH::H1F("QaV0VtxPhiDist",
            "V0 azimuthal distribution relative to primvtx",36,0.,360.);
  m_vtx_r_dist  = QAH::H1F("QaV0VtxRDist",
            "V0 radial distribution relative to primvtx",50,0.,200.);
  m_vtx_z_dist  = QAH::H1F("QaV0VtxZDist",
            "V0 Z distribution relative to primvtx",60,-30.,30.);

  m_v0             = QAH::H1F("QaV0Vtx","log10 total V0s (0 => -0.5)",45,-0.55,3.95);
  m_ev0_lama_hist  = QAH::H1F("QaV0LambdaMass","V0: Lambda mass",25,1.05,1.15);
  m_ev0_k0ma_hist  = QAH::H1F("QaV0K0Mass","V0: k0 mass",25,.4,.6);

  m_xi_tot     = QAH::H1F("QaXiVtxTot", "log10 total Xis (0 => -0.5)",45,-0.55,3.95);
  m_xi_ma_hist = QAH::H1F("QaXiaMass",  "Xi: Xi mass",25,1.2,1.4);

  m_kink_tot   = QAH::H1F("QaKinkTot",  "log10 total kinks (0 => -0.5)",35,-0.55,2.95);

  m_vtx_FtpcEastTpc_xy = QAH::H2F("QaVtxFtpcETpcXY",
				  " vertex,prim: x(ftpcE)-x(tpc) vs y(ftpcE)-y(tpc)",
				  800, -20., 20., 800, -20., 20.);
  m_vtx_FtpcEastTpc_z  = QAH::H1F("QaVtxFtpcETpcZ",
				  " vertex,prim: z(ftpcE)-z(tpc)",
				  100, -10., 10.);
  m_vtx_FtpcWestTpc_xy = QAH::H2F("QaVtxFtpcWTpcXY",
				  " vertex,prim: x(ftpcW)-x(tpc) vs y(ftpcW)-y(tpc)",
				  800, -20., 20., 800, -20., 20.);
  m_vtx_FtpcWestTpc_z  = QAH::H1F("QaVtxFtpcWTpcZ",
				  " vertex,prim: z(ftpcW)-z(tpc)",
				  100, -10., 10.);
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistPoint(){

  m_pnt_tot     = QAH::H1F("QaPointTot", "point: # hits total ",100, 0.,400000.);
  m_pnt_tot_med = QAH::H1F("QaPointTotmed","point: # hits total ",100, 0.,25000.);
  m_pnt_tot_sm  = QAH::H1F("QaPointTotsm", "point: # hits total ",100, 0.,2500.);
  m_pnt_id      = QAH::H1F("QaPointId","point: detector ID of hit",30,0.,30.);

  m_pnt_tpc     = QAH::H1F("QaPointTpc",  "point: # hits tpc ",100, 0.,300000.);
  // east and west on same plot
  m_pnt_ftpc   = QAH::MH1F("QaPointFtpc", "point: # hits ftpc",100,0.,25000.,2);
  m_pnt_ftpc->Rebin(0,"East");
  m_pnt_ftpc->Rebin(1,"West");
  m_pnt_ftpc->SetStats(kFALSE);
  // east and west on separate plots
  m_pnt_ftpcE   = QAH::H1F("QaPointFtpcE","point: # hits ftpcE ",100, 0.,25000.);
  m_pnt_ftpcW   = QAH::H1F("QaPointFtpcW","point: # hits ftpcW ",100, 0.,25000.);
  if (silHists) {
    m_pnt_svt     = QAH::H1F("QaPointSvt",  "point: # hits svt ",600, 0.,15000.);
    m_pnt_svtLaser= QAH::H2F("QaPointSvtLaser","point: laser spots, svt ",150,0,600,65,0.,130.);
    m_pnt_svtLaser->SetXTitle("event in file");
    m_pnt_svtLaserDiff= QAH::MH2F("QaPointSvtLaserDiff","point: diff of laser spots, svt ",150,0,600,101,9.8,50.2,2);
    m_pnt_svtLaserDiff->SetXTitle("event in file");
    m_pnt_svtLaserDiff->SetYTitle("time buckets");
    m_pnt_svtLaserDiff->Rebin(0,"Laser 1");
    m_pnt_svtLaserDiff->Rebin(1,"Laser 2");
  }
  if (silHists || (histsSet>=StQA_run14 && histsSet<StQA_run17)) {
    m_pnt_xyS     = QAH::H2F("QaPointXYSvt","point: x-y distribution of hits, svt,ssd",125,-25,25,125,-25,25);
    m_pnt_xyS->SetStats(kFALSE);
    m_pnt_xyS->SetXTitle("x [cm]");
    m_pnt_xyS->SetYTitle("y [cm]");

    // for run >= 14, change the title of the histogram with vertex detector hit distribution in x-y
    // FIXME: move this part to BookHistPoint and pass histsSet there?
    if (histsSet>=StQA_run14 && histsSet<StQA_run17)
      m_pnt_xyS->SetTitle("PIXEL, IST, SSD: Distribution of hits in XY");
  }
  // Now using polar coords (lego didn't work well because of inner radius and colored zero)
  // Drawing a lego plot requires r-phi,r. Drawing a plain polar plot reguirs r,r-phi
  m_pnt_rpTW    = QAH::H2F("QaPointRPTpcW","point: r-phi distribution of hits, tpcW",20,58.75,196.75,72,0,TMath::TwoPi());
  m_pnt_rpTE    = QAH::H2F("QaPointRPTpcE","point: r-phi distribution of hits, tpcE",23,58.75,196.75,72,0,TMath::TwoPi());
  m_z_hits      = QAH::H1F("QaPointZhits","point: z distribution of hits, tpc",100,-210,210);
  m_pnt_timeT   = QAH::MH1F("QaPointTimeT","point: time bucket distribution of hits, tpc",45,0,450,2);
  m_pnt_timeT->Rebin(0,"East");
  m_pnt_timeT->Rebin(1,"West");
  m_pnt_timeT->SetStats(kFALSE);
  m_pnt_phiT    = QAH::MH1F("QaPointPhiT","point: #phi distribution of hits, tpc",36,0,360,2);
  m_pnt_phiT->Rebin(0,"East");
  m_pnt_phiT->Rebin(1,"West");
  m_pnt_phiT->SetStats(kFALSE);
  m_pnt_padrowT = QAH::MH1F("QaPointPadrowT","point: padrow distribution of hits, tpc",72,0.5,72.5,2);
  m_pnt_padrowT->Rebin(0,"East");
  m_pnt_padrowT->Rebin(1,"West");
  m_pnt_padrowT->SetStats(kFALSE);
  m_pnt_padrowT->SetXTitle("padrow number");

  if (silHists) {
    m_pnt_zS      = QAH::H1F("QaPointZhitsS","point: z distribution of hits, svt",100,-35,35);
    m_pnt_phiS    = QAH::H1F("QaPointPhiS","point: #phi distribution of hits, svt",36,0,360);
    m_pnt_barrelS = QAH::H1F("QaPointBarrelS","point: barrel distribution of hits, svt",3,0.5,3.5);
    m_pnt_barrelS->SetXTitle("barrel number");
  }
  if (silHists) {
    m_pnt_ssd     = QAH::H1F("QaPointSsd",  "point: # hits ssd ",200, 0.,5000.);
    m_pnt_phiSSD  = QAH::H1F("QaPointPhiSSD","SSD: #phi of hits (per event)",36,0,360);
    m_pnt_lwSSD   = QAH::H2F("QaPointLWSSD","SSD: wafer id vs ladder id (per event)",20,0.5,20.5,16,0.5,16.5);
    m_pnt_lwSSD->SetXTitle("Ladder #");
    m_pnt_lwSSD->SetYTitle("Wafer (sensor) #");
    m_pnt_lwSSD->SetStats(kFALSE);
    m_pnt_sizeSSD = QAH::MH1F("QaPointSizeSSD","SSD: size of clusters",10,0.5,10.5,2);
    m_pnt_sizeSSD->Rebin(0,"P-side");
    m_pnt_sizeSSD->Rebin(1,"N-side");
    m_pnt_sizeSSD->SetStats(kFALSE);
    m_pnt_eSSD = QAH::H1F("QaPointESSD","SSD: log10(energy) of hits",90,-5,-2);
  }
  if (histsSet>=StQA_run14 && histsSet<StQA_run17) {
    m_pnt_sst     = QAH::H1F("QaPointSst",  "point: # hits sst ",200, 0.,5000.);
    m_pnt_phiSST  = QAH::H1F("QaPointPhiSST","SST: #phi of hits (per event)",36,0,360);
    m_pnt_lwSST   = QAH::H2F("QaPointLWSST","SST: wafer id vs ladder id (per event)",20,0.5,20.5,16,0.5,16.5);
    m_pnt_lwSST->SetXTitle("Ladder #");
    m_pnt_lwSST->SetYTitle("Wafer (sensor) #");
    m_pnt_lwSST->SetStats(kFALSE);
    m_pnt_sizeSST = QAH::MH1F("QaPointSizeSST","SST: size of clusters",10,0.5,10.5,2);
    m_pnt_sizeSST->Rebin(0,"P-side");
    m_pnt_sizeSST->Rebin(1,"N-side");
    m_pnt_sizeSST->SetStats(kFALSE);
    m_pnt_eSST = QAH::H1F("QaPointESST","SST: log10(energy) of hits",90,-5,-2);
  }

  m_pnt_xyFE    = QAH::H2F("QaPointXYFtpcE","point: x-y distribution of hits, ftpcE",70,-35,35,70,-35,35);
  m_pnt_xyFW    = QAH::H2F("QaPointXYFtpcW","point: x-y distribution of hits, ftpcW",70,-35,35,70,-35,35);
  m_pnt_planeF  = QAH::MH1F("QaPointPlaneF","point: plane distribution of hits, ftpc",20,0.5,20.5,2);
  m_pnt_planeF->Rebin(0,"East");
  m_pnt_planeF->Rebin(1,"West");
  m_pnt_planeF->SetStats(kFALSE);
  m_pnt_planeF->SetXTitle("plane number");
  m_pnt_padtimeFE    = QAH::H2F("QaPointPadTimeFtpcE","point: #pads vs #timebins of hits, ftpcE",12,0.5,12.5,10,0.5,10.5);
  m_pnt_padtimeFE->SetXTitle("#timebins");
  m_pnt_padtimeFE->SetYTitle("#pads");
  m_pnt_padtimeFW    = QAH::H2F("QaPointPadTimeFtpcW","point: #pads vs #timebins of hits, ftpcW",12,0.5,12.5,10,0.5,10.5);
  m_pnt_padtimeFW->SetXTitle("#timebins");
  m_pnt_padtimeFW->SetYTitle("#pads");


}
//_____________________________________________________________________________
void StQABookHist::BookHistEMC(){

  if (!((gROOT->GetClass("StEmcMath")) && (gROOT->GetClass("StEmcGeom"))))
    return;
// Book the hists for SimulatorMaker
  m_emc_nhit=QAH::H2F("EmcNHitsVsDet","emc: Number of hit(log) .vs. Detector #",100,0.0,4.5,4,0.5,4.5);
  m_emc_etot=QAH::H2F("EmcEtotVsDet","emc: Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,4.5);

  const Char_t* tit={"Barrel"};
  const Int_t   nx[4] = {40,40,300,20};
  const Int_t   ny[4]  = {120, 120, 60, 900};
  Float_t rpi = M_PI + 0.00001;
  TString name, title;
  TArrayD *xb = StEmcMath::binForSmde();

  for(Int_t i=0; i<4; i++){
    name  = detname[i] + "Hits";
    title = tit  + detname[i] + " hits dist.";
    if(i==2) m_emc_hits[i]=QAH::H2F(name,title, xb->GetSize()-1,xb->GetArray(),ny[i],-rpi,rpi);
    else m_emc_hits[i] = QAH::H2F(name,title, nx[i],-1.,+1., ny[i],-rpi, rpi);

    name   = detname[i] + "Energy2D";
    title  = tit + detname[i] + " energy dist. in eta&phi";
    if(i==2) m_emc_energy2D[i] = QAH::H2F(name,title, xb->GetSize()-1,xb->GetArray(), ny[i],-rpi,rpi);
    else m_emc_energy2D[i] = QAH::H2F(name,title, nx[i],-1.,+1., ny[i],-rpi, rpi);

    name  = detname[i] + "Adc";
    title = tit + detname[i] + " ADC dist.";
    m_emc_adc[i]  = QAH::H1F(name,title, 5001, -0.5, 5000.5);

    name   = detname[i] + "Energy";
    title  = tit + detname[i] + " energy dist.";
    m_emc_energy[i] = QAH::H1F(name,title, 600, 0.0, 60.0);
  }
  delete xb;

// Book the hists for cluster finder
  Int_t greta[4]={40,40,300,20};   // eta bins
  Int_t grphi[4]={120,120,60,900}; // phi bins  => 16-apr by PAI
  Float_t myPI = M_PI + 0.0001;
  
  m_emc_ncl=QAH::H2F("EmcNcluster","emc: Number of cluster(log) .vs. Detector #",40,0.0,4.0, 4,0.5,4.5);
  m_emc_etotCl=QAH::H2F("EmcEcluster" ,"emc: Total PreCluster Energy(log) .vs. Detector #", 60,-2.0,4.0, 4,0.5,4.5);

  Float_t rmsMax=0.026;
  Int_t   rmsN=52;
  m_emc_sig_e= QAH::H2F("EmcRMSeta" ,"emc: Sigma(eta) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  m_emc_sig_p= QAH::H2F("EmcRMSphi" ,"emc: Sigma(phi) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  for (Int_t i=0; i<4; i++)
  {
    TString name_h = detname[i] + "_cluster";
    TString name_e = detname[i] + "_cluster_energy";
    TString tit_h  = detname[i] + " cluster";
    TString tit_e  = detname[i] + " energy of cluster";
    if(i==2) {
      m_emc_cl[i]     = QAH::H2F(name_h,tit_h,greta[i],-1.0,1.0,grphi[i],-M_PI*1.015, M_PI*0.985);
      m_emc_energyCl[i] = QAH::H2F(name_e,tit_e,greta[i],-1.0,1.0,grphi[i],-M_PI*1.015, M_PI*0.985);
    } else {
      m_emc_cl[i]     = QAH::H2F(name_h,tit_h,greta[i],-1.0,1.0,grphi[i],-myPI, myPI);
      m_emc_energyCl[i] = QAH::H2F(name_e,tit_e,greta[i],-1.0,1.0,grphi[i],-myPI, myPI);
    }
    

    name  = detname[i] + "ClNum";
    title   = "Number hits in cluster for " + detname[i];
    m_emc_HitsInCl[i]   = QAH::H1F(name, title, 21, -0.5, 20.5);

    name  = detname[i] + "ClEnergy";
    title   = "Energy of cluster for " + detname[i];
    m_emc_EnergyCl[i]    = QAH::H1F(name, title, 2000, 0.0, 20.0);

    TString name_eta  = detname[i] + "Eta";
    TString tit_eta   = "Eta of clusters for " + detname[i];
    TString name_phi  = detname[i] + "Phi";
    TString tit_phi   = "Phi of clusters for " + detname[i];
    if(i==2) {
      TArrayD *xb  = StEmcMath::binForSmde();
      if(xb) {
        m_emc_EtaInCl[i] = QAH::H1F(name_eta, tit_eta, xb->GetSize()-1, xb->GetArray());
        delete xb;
      }
      m_emc_PhiInCl[i]   = QAH::H1F(name_phi, tit_phi, grphi[i], -M_PI*1.015, M_PI*0.985);
    } else { 
      m_emc_EtaInCl[i]   = QAH::H1F(name_eta, tit_eta, greta[i], -1., 1.);
      m_emc_PhiInCl[i]   = QAH::H1F(name_phi, tit_phi, grphi[i], -myPI, myPI);
    }
  }
  // Book the hists for Emc Points
  const TString catname[] = {"EmcCat1", "EmcCat2", "EmcCat3", "EmcCat4"};

  for (Int_t i=0; i<4; i++) {
    name = catname[i] + "_Point_Energy";
    title = catname[i] + " Point Energy";
    m_emc_point_energy[i]= QAH::H1F(name,title,100,0.,10.);

    name = catname[i] + "_Point_Eta";
    title = catname[i] + " Point Eta";
    m_emc_point_eta[i]= QAH::H1F(name,title,100,-1.,1.);

    name = catname[i] + "_Point_Phi";
    title = catname[i] + " Point Phi";
    m_emc_point_phi[i]= QAH::H1F(name,title,100,-3.14,3.14);
   
    name = catname[i] + "_Sigma_Eta";
    title = catname[i] + " Sigma Eta";
    m_emc_point_sigeta[i]= QAH::H1F(name,title,100,0.,.05);
  
    name = catname[i] + "_Sigma_Phi";
    title = catname[i] + " Sigma Phi";
    m_emc_point_sigphi[i]= QAH::H1F(name,title,100,0.,.05);

    name = catname[i] + "_Delta_Eta";
    title = catname[i] + " Delta Eta";
    m_emc_point_deleta[i]= QAH::H1F(name,title,100,-.2,.2);

    name = catname[i] + "_Delta_Phi";
    title = catname[i] + " Delta Phi";
    m_emc_point_delphi[i]= QAH::H1F(name,title,100,-.2,.2);

    name = catname[i] + "_Points_Multiplicity";
    title = catname[i] + " Points Multiplicity";
    m_emc_points[i]= QAH::H1F(name,title,200,0.,1000.);

    name = catname[i] + "_Track_Momenta";
    title = catname[i] + " Track Momenta ";
    m_emc_point_trmom[i]= QAH::H1F(name,title,100,0.,10.);
  }

  m_emc_point_flag= QAH::H1F(" Point Flag","Point Flag",5,0.5,5.5);

}
//_____________________________________________________________________________
void StQABookHist::BookHistEval(){

// these only get filled if the geant dataset is available!

   m_geant_reco_pvtx_x  = QAH::H1F("QaGRpvtxDx"," diff geant - reco prim vtx X",
                              100, -0.25,0.25);
   m_geant_reco_pvtx_y  = QAH::H1F("QaGRpvtxDy"," diff geant - reco prim vtx Y",
                              100, -0.25,0.25);
   m_geant_reco_pvtx_z  = QAH::H1F("QaGRpvtxDz"," diff geant - reco prim vtx Z",
                              100, -0.25,0.25);
   m_geant_reco_vtx_z_z = QAH::H2F("QaGRpvtxDzZ",
        " reco pvtx Z vs diff geant - reco Z", 100, -0.5,0.5,100,-50.,50.);
     m_geant_reco_vtx_z_z->SetXTitle("z vtx resolution (cm)");
     m_geant_reco_vtx_z_z->SetYTitle("z position of vtx (cm)");

}
//_____________________________________________________________________________
void StQABookHist::BookHistBBC(){

  Char_t ID[4];
  Int_t i,j;

  m_bbc_adc[0] = QAH::MH1F("QaBbcAdcES","BBC East Small ADC",100,0.5,400.5,8);
  m_bbc_adc[1] = QAH::MH1F("QaBbcAdcEL","BBC East Large ADC",100,0.5,400.5,8);
  m_bbc_adc[2] = QAH::MH1F("QaBbcAdcWS","BBC West Small ADC",100,0.5,400.5,8);
  m_bbc_adc[3] = QAH::MH1F("QaBbcAdcWL","BBC West Large ADC",100,0.5,400.5,8);
  m_bbc_tdc[0] = QAH::MH1F("QaBbcTdcES","BBC East Small TDC",100,0.5,750.5,8);
  m_bbc_tdc[1] = QAH::MH1F("QaBbcTdcEL","BBC East Large TDC",100,0.5,2000.5,8);
  m_bbc_tdc[2] = QAH::MH1F("QaBbcTdcWS","BBC West Small TDC",100,0.5,750.5,8);
  m_bbc_tdc[3] = QAH::MH1F("QaBbcTdcWL","BBC West Large TDC",100,0.5,2000.5,8);
  for (i=0; i<8; i++) {
    sprintf(ID,"%d",i+1);
    for (j=0; j<4; j++) {
      m_bbc_adc[j]->Rebin(i,ID);
      m_bbc_tdc[j]->Rebin(i,ID);
    } 
  } 

}
//_____________________________________________________________________________
void StQABookHist::BookHistFPD(){

  QAH::MMH1F(m_fpd_top,2,"QaFpdTop%d","FPD Top ADC %d-%d",100,0.5,1500.5,8,1);
  QAH::MMH1F(m_fpd_bottom,2,"QaFpdBottom%d","FPD Bottom ADC %d-%d",100,0.5,1500.5,8,1);
  QAH::MMH1F(m_fpd_south,2,"QaFpdSouth%d","FPD South ADC %d-%d",100,0.5,1500.5,8,1);
  QAH::MMH1F(m_fpd_north,2,"QaFpdNorth%d","FPD North ADC %d-%d",100,0.5,1500.5,6,1);
  
// Book ADC histograms: FPD SUM signals
  m_fpd_sums[0] = QAH::H1F("QaFpdSums0","FPD SUM Top",100,0.5,2050.5);
  m_fpd_sums[1] = QAH::H1F("QaFpdSums1","FPD SUM Bottom",100,0.5,2050.5);
  m_fpd_sums[2] = QAH::H1F("QaFpdSums2","FPD SUM South",100,0.5,2050.5);
  m_fpd_sums[3] = QAH::H1F("QaFpdSums3","FPD SUM North",100,0.5,2050.5);
  m_fpd_sums[4] = QAH::H1F("QaFpdSums4","FPD SUM SmdX",100,0.5,6000.5);
  m_fpd_sums[5] = QAH::H1F("QaFpdSums5","FPD SUM SmdY",100,0.5,6000.5);
  m_fpd_sums[6] = QAH::H1F("QaFpdSums6","FPD SUM Pres1",100,0.5,1500.5);
  m_fpd_sums[7] = QAH::H1F("QaFpdSums7","FPD SUM Pres2",100,0.5,1500.5);

}
//_____________________________________________________________________________
void StQABookHist::BookHistPMD(){

  QAH::MMH1F(m_pmd_sm_hit,12,"QaPmdSmHit%d",
	     "PMD SM-wise Hit Multiplicity %02d-%02d",1000,0.,500.,2);
  QAH::MMH1F(m_pmd_sm_adc,12,"QaPmdSmAdcHit%d",
	     "PMD SM-wise ADC/Hit Multiplicity %02d-%02d",100,0.,600.,2);
  QAH::MMH1F(m_pmd_chain_hit,24,"QaPmdChHitChain%d",
	     "PMD Channel-wise Hit, Chain %02d-%02d",1728,-0.5,1727.5,2);
  QAH::MMH1F(m_pmd_chain_adc,24,"QaPmdChAdcChain%d",
	     "PMD Channel-wise ADC, Chain %02d-%02d",1728,-0.5,1727.5,2);

  m_pmd_total_hit = QAH::H2F("QaPmdTotalHit","PMD Total Hits",100,0.,2e5,100,0.,4.);
  m_pmd_total_hit->SetXTitle("event id");
  m_pmd_total_hit->SetYTitle("log10");
  m_pmd_total_adc = QAH::H2F("QaPmdTotalAdc","PMD Total ADC",100,0.,2e5,100,0.,6.);
  m_pmd_total_adc->SetXTitle("event id");
  m_pmd_total_adc->SetYTitle("log10");
  m_cpv_total_hit = QAH::H2F("QaCpvTotalHit","CPV Total Hits",100,0.,2e5,100,0.,4.);
  m_cpv_total_hit->SetXTitle("event id");
  m_cpv_total_hit->SetYTitle("log10");
  m_cpv_total_adc = QAH::H2F("QaCpvTotalAdc","CPV Total ADC",100,0.,2e5,100,0.,6.);
  m_cpv_total_adc->SetXTitle("event id");
  m_cpv_total_adc->SetYTitle("log10");
  
}
//_____________________________________________________________________________
void StQABookHist::BookHistTOF(){

  m_tof_hit_tray = QAH::H2F("QaTofHitvsTray","TOF Hits vs tray",120,0.5,120.5,100,0.,100.);
  m_tof_hit_module = QAH::H2F("QaTofHitvsModule","TOF Hits vs Module",65,-32.5,32.5,100,0.,100.);
  m_tof_match_tray = QAH::H2F("QaTofMatchvsTray","TOF Matched Hits vs tray",120,0.5,120.5,100,0.,100.);
  m_tof_match_module = QAH::H2F("QaTofMatchvsModule","TOF Matched Hits vs Module",65,-32.5,32.5,100,0.,100.);
  m_tof_vpd_hit =  QAH::H2F("QaTofHitvsVpdHit","TOF Hits vs Vpd Hits",50,0.,50.,100,0.,5000.);
  m_tof_vtx_z =  QAH::H2F("QaTofVpdZvsTpcZ","VPD vtxz vs TPC vtxz",100,-100.,100.,100,-100.,100.);
  m_tof_PID =  QAH::H2F("QaTofPID","TOF InvBeta vs p",100,0.,5.,100,0.,4.);

}
//_____________________________________________________________________________
void StQABookHist::BookHistHFT(){

  m_nhit_Pxl_Ist = QAH::H2F("QaPxlvsIstHit","PIXEL hits vs IST hits",500,0.,20000.,100,0.,5000.);
  m_nhit_Pxl_Ist->SetXTitle("PIXEL hits");
  m_nhit_Pxl_Ist->SetYTitle("IST hits");

  m_nhit_Pxl_Sst = QAH::H2F("QaPxlvsSstHit","PIXEL hits vs SST hits",500,0.,20000.,100,0.,5000.);
  m_nhit_Pxl_Sst->SetXTitle("PIXEL hits");
  m_nhit_Pxl_Sst->SetYTitle("SST hits");

  m_nhit_Ist_Sst = QAH::H2F("QaIstvsSsdHit","IST hits vs SST hits",100,0.,5000.,100,0.,5000.);
  m_nhit_Ist_Sst->SetXTitle("IST hits");
  m_nhit_Ist_Sst->SetYTitle("SST hits");

}
//_____________________________________________________________________________
void StQABookHist::BookHistPXL(){

  m_pxl_hit_phi_z_Pxl1= QAH::H2F("QaPxlHitPhivsZPxl1","PIXEL: hits vs phi vs z in inner layer (per event)",20,-10.,10.,100,-TMath::Pi(),TMath::Pi());
  m_pxl_hit_phi_z_Pxl1->SetXTitle("z [cm]");
  m_pxl_hit_phi_z_Pxl1->SetYTitle("#phi");
  m_pxl_hit_phi_z_Pxl2= QAH::H2F("QaPxlHitPhivsZPxl2","PIXEL: hits vs phi vs z in outer layer (per event)",20,-10.,10.,100,-TMath::Pi(),TMath::Pi());
  m_pxl_hit_phi_z_Pxl2->SetXTitle("z [cm]");
  m_pxl_hit_phi_z_Pxl2->SetYTitle("#phi");

  m_pxl_hit_ladder = QAH::H1F("QaPxlHitvsLadder","PIXEL: hits per ladder (per event)",5,0.,5.);
  m_pxl_hit_ladder->SetXTitle("Ladder ID");

  m_pxl_hit_sector_sensor_Pxl1 = QAH::H2F("QaPxlHitvsSectorvsSensorPxl1","PIXEL: hits vs sector vs sensor in inner layer (per event)",10,0.5,10.5,10,0.5,10.5);
  m_pxl_hit_sector_sensor_Pxl1->SetXTitle("Sector ID");
  m_pxl_hit_sector_sensor_Pxl1->SetYTitle("Sensor ID");
  m_pxl_hit_sector_sensor_Pxl1->SetStats(kFALSE);

  m_pxl_hit_sector_sensor_Pxl2 = QAH::H2F("QaPxlHitvsSectorvsSensorPxl2","PIXEL: hits vs ladder vs sensor in outer layer (per event)",40,0.5,40.5,10,0.5,10.5);
  m_pxl_hit_sector_sensor_Pxl2->SetXTitle("Ladder ID");
  m_pxl_hit_sector_sensor_Pxl2->SetYTitle("Sensor ID");
  m_pxl_hit_sector_sensor_Pxl2->SetStats(kFALSE);

  m_pxl_nhit_Pxl1_tpc_mult = QAH::H2F("QaPxl1HitvsTpcMult","PIXEL: Hits in inner layer vs TPC multiplicity",100,0.,12000.,500,0.,10000.);
  m_pxl_nhit_Pxl1_tpc_mult->SetXTitle("PIXEL hits (inner layer)");
  m_pxl_nhit_Pxl1_tpc_mult->SetYTitle("TPC multiplicity");

  m_pxl_nhit_Pxl2_tpc_mult = QAH::H2F("QaPxl2HitvsTpcMult","PIXEL: Hits in outer layer vs TPC multiplicity",100,0.,8000.,500,0.,10000.);
  m_pxl_nhit_Pxl2_tpc_mult->SetXTitle("PIXEL hits (outer layer)");
  m_pxl_nhit_Pxl2_tpc_mult->SetYTitle("TPC multiplicity");

  m_pxl_nhit_Pxl1_tof_mult = QAH::H2F("QaPxl1HitvsTofMult","PIXEL: Hits in inner layer vs TOF multiplicity",100,0.,12000.,100,0.,5000.);
  m_pxl_nhit_Pxl1_tof_mult->SetXTitle("PIXEL hits (inner layer)");
  m_pxl_nhit_Pxl1_tof_mult->SetYTitle("ToF hits");

  m_pxl_nhit_Pxl2_tof_mult = QAH::H2F("QaPxl2HitvsTofMult","PIXEL: Hits in outer layer vs TOF multiplicity",100,0.,8000.,100,0.,5000.);
  m_pxl_nhit_Pxl2_tof_mult->SetXTitle("PIXEL hits (outer layer)");
  m_pxl_nhit_Pxl2_tof_mult->SetYTitle("ToF hits");

  m_pxl_nhit_Pxl1_Pxl2 = QAH::H2F("QaPxlHitLayer1vsLayer2","PIXEL: Hits in inner vs outer layer (per event)",100,0.,12000.,100,0.,8000.);
  m_pxl_nhit_Pxl1_Pxl2->SetXTitle("hits inner layer");
  m_pxl_nhit_Pxl1_Pxl2->SetYTitle("hits outer layer");

}
//_____________________________________________________________________________
void StQABookHist::BookHistIST(){


  m_ist_hit_phi_z = QAH::H2F("QaIstHitPhivsZ","IST: Hits vs phi vs z (per event)",60,-30.,30.,100,-TMath::Pi(),TMath::Pi());
  m_ist_hit_phi_z->SetXTitle("z [cm]");
  m_ist_hit_phi_z->SetYTitle("#phi");

  m_ist_hit_ladder = QAH::H1F("QaIstHitvsLadder","IST: Hits per ladder (per event)",25,0.,25.);
  m_ist_hit_ladder->SetXTitle("Ladder ID");

  m_ist_hit_ladder_sensor = QAH::H2F("QaIstHitvsLaddervsSensor","IST: Hits vs ladder vs sensor (per event)", 24,0.5,24.5, 6, 0.5, 6.5);
  m_ist_hit_ladder_sensor->SetXTitle("Ladder ID");
  m_ist_hit_ladder_sensor->SetYTitle("Sensor ID");
  m_ist_hit_ladder_sensor->SetStats(kFALSE);


  m_ist_nhit_tpc_mult = QAH::H2F("QaIstHitvsTpcMult","IST hits vs TPC multiplicity",100,0.,5000.,500,0.,10000.);
  m_ist_nhit_tpc_mult->SetXTitle("IST hits");
  m_ist_nhit_tpc_mult->SetYTitle("TPC multiplicity");

  m_ist_nhit_tof_mult = QAH::H2F("QaIstHitvsTofMult","IST hits vs TOF multiplicity",100,0.,5000.,100,0.,5000.);
  m_ist_nhit_tof_mult->SetXTitle("IST hits");
  m_ist_nhit_tof_mult->SetYTitle("ToF hits");

}
//_____________________________________________________________________________
void StQABookHist::BookHistMTD(){

  m_MtdNHits       = QAH::H1F("QaMtdNHits","Number of MTD hits per event;N",50,0,50);
  m_MtdHitMap      = QAH::H2F("QaMtdHitMap","MTD: channel vs backleg of hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  m_MtdNMatchHits  = QAH::H1F("QaMtdNMatchHits","Number of matched MTD hits per event;N",20,0,20);
  m_MtdMatchHitMap = QAH::H2F("QaMtdMatchHitMap","MTD: channel vs backleg of matched hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);

}
//_____________________________________________________________________________
void StQABookHist::BookHistEPD(){

  TString name, title;
  const TString side[2] = {"East", "West"};
  
  const int nPP = 12;
  for (int i=0; i<2; i++){
    for (int j=0; j<nPP; j++){
      (((name = "QaEpdAdc") += side[i]) += "PP") += (j+1);
      ((((title = "EPD ") += side[i]) += " PP") += (j+1)) += " ADC";
      m_epd_adc[(i*nPP)+j] = QAH::H2F(name,title,31,1,31,4096,0,4095);
      m_epd_adc[(i*nPP)+j]->SetXTitle("Tile");
      m_epd_adc[(i*nPP)+j]->SetYTitle("ADC");
      
      (((name = "QaEpdTac") += side[i]) += "PP") += (j+1);
      ((((title = "EPD ") += side[i]) += " PP") += (j+1)) += " TAC";
      m_epd_tac[(i*nPP)+j] = QAH::H2F(name,title,9,1,9,4096,0,4095);
      m_epd_tac[(i*nPP)+j]->SetXTitle("Tile");
      m_epd_tac[(i*nPP)+j]->SetYTitle("TAC");
    }
  }
    
}
//_____________________________________________________________________________
void StQABookHist::BookHistiTPC(){
  char nameiTPC[60], titleiTPC[60];
  for(int i = 0; i < 24; i++)
    {
      sprintf(nameiTPC,"TPC_charge_time_inner_s%d",   i+1);
      sprintf(titleiTPC,"TPC adc vs time-bin inner sector %d",   i+1);
      m_TPC_ch_time_inner[i] = QAH::H2F(nameiTPC, titleiTPC, 50, 0, 511, 250 , 0, 500);
      m_TPC_ch_time_inner[i] ->SetXTitle("time bin");
      m_TPC_ch_time_inner[i] ->SetYTitle("adc");
    }
  
  for(int i = 0; i < 24; i++)
    {
      sprintf(nameiTPC,"TPC_charge_time_outer_s%d",   i+1);
      sprintf(titleiTPC,"TPC adc vs time-bin outer sector %d",   i+1);
      m_TPC_ch_time_outer[i] = QAH::H2F(nameiTPC, titleiTPC, 50, 0, 511, 250 , 0, 500);
      m_TPC_ch_time_outer[i] ->SetXTitle("time bin");
      m_TPC_ch_time_outer[i] ->SetYTitle("adc");
    }
  
  for(int i = 0; i < 24; i++)
    {
      sprintf(nameiTPC,"TPC_charge_nrow_s%d",   i+1);
      sprintf(titleiTPC,"TPC adc vs pad-row sector %d",   i+1);
      m_TPC_ch_nrow[i] = QAH::H2F(nameiTPC, titleiTPC, 72 ,0.5, 72.5, 250 , 0, 500);
      m_TPC_ch_nrow[i] ->SetXTitle("pad row");
      m_TPC_ch_nrow[i] ->SetYTitle("adc");
    }

  sprintf(nameiTPC,"TPC_adc_sec_inner");
  sprintf(titleiTPC,"TPC adc vs sector inner");
  m_TPC_adc_sec_inner = QAH::H2F(nameiTPC, titleiTPC, 24 ,0.5, 24.5, 250 , 0, 500);
  m_TPC_adc_sec_inner ->SetXTitle("sector");
  m_TPC_adc_sec_inner ->SetYTitle("adc");
  
  sprintf(nameiTPC,"TPC_adc_sec_outer");
  sprintf(titleiTPC,"TPC adc vs sector outer");
  m_TPC_adc_sec_outer = QAH::H2F(nameiTPC, titleiTPC, 24 ,0.5, 24.5, 250 , 0, 500);
  m_TPC_adc_sec_outer ->SetXTitle("sector");
  m_TPC_adc_sec_outer ->SetYTitle("adc");
}
//_____________________________________________________________________________

