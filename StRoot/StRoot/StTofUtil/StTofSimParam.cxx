/*******************************************************************
 *
 * $Id: StTofSimParam.cxx,v 1.7 2008/09/02 18:39:43 dongx Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Simulation Parameters for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofSimParam.cxx,v $
 * Revision 1.7  2008/09/02 18:39:43  dongx
 * update on MRPC slow parameters
 *
 * Revision 1.6  2008/08/28 18:43:19  dongx
 * Added MRPC-TOF simulation parameters
 * Added TOF/VPD fast simulation parameters
 *
 * Revision 1.5  2007/04/17 23:01:52  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.4  2003/09/02 17:59:10  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/04/22 00:03:35  geurts
 * doubled the m_nphe_to_adc parameter (used in StTofSimMaker).
 *
 * Revision 1.2  2002/01/22 07:18:08  geurts
 * doxygenized
 *
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
//! Time-of-Flight Simulation Utilities
/*! \class StTofSimParam
    \author Frank Geurts

    <p>A package of simulation routines for the STAR Time-of-Flight
    detector. Currently the only method prints the default initialized
    values.<p>

    To do:
    <ul>
    <li> create STAR dBase entries and add members to access those<li>
    <li> move general simulation methods from the StTofSimMaker into
         StTofSimParam</li>
    </ul>
*/
#include "StTofSimParam.h"
#include <Stiostream.h>
#include "StMessMgr.h"
#include "math.h"
//#include "St_XDFFile.h"
//#include "St_DataSetIter.h"


/// default constructor, initializes default simulation parameters
StTofSimParam::StTofSimParam(){ 
/// Default Switches
  mSimuTofp    = false;
  mSimuVpd     = true;
  mSimuTofFast = true;
  mSimuTofSlow = false;

/// Tofp simulation parameters
  m_adc_overflow       = 1024.0     ;
  m_attlen             =  110.0     ;
  m_cath_eff           =    0.2     ;
  m_cath_surf          =    0.61    ;
  m_delay              =   80.2e-12 ;
  m_elec_noise         =    0.      ;
  m_gate_t0            =    3.e-9   ;
  m_gate_width         =  200.e-9   ;
  m_geo_from_geant     =    1.      ;
  m_GeV_2_n_photons    =    1.e+7   ;
  m_nphe_to_adc        =    0.2     ;
  m_phys_noise         =    0.      ;
  m_position_tolerance =    0.3     ;
  m_slat_para          =    0.      ; // no slat response table yet.
  m_start_res          =   50.e-12  ;
  m_surf_loss          =    0.42    ;
  m_time_res           =   16.e-12  ;
  m_tdc_overflow       = 2048.0     ;

///MRPC TOF simulation parameters
  m_n_gap              = 6          ;
  m_nmax_clus          = 20         ;
  m_d_in               = 0.54       ; //! mm
  m_d_out              = 1.10       ; //! mm
  m_d_gap              = 0.22       ; //! mm
  m_alpha_dg           = 31.944     ; //! alpha*dg
  m_er                 = 5.0        ;
  m_clus_par[0]        = 10.07      ;
  m_clus_par[1]        = -6.781     ;
  m_clus_par[2]        = 6.396      ;
  m_clus_par[3]        = -0.6407    ;
  m_vd_mean            = 2.28e-4    ; //! mm/ps
  m_nmean_e            = 2.42       ;
  m_nmax_e             = 1.6e+7     ;
  m_dt                 = 25.        ; //! in ps
  m_ndt                = 600        ; //! maximum ndt in slow simulation
  m_toffset            = 6000.      ; //! time to start slow simulation (ps)
  m_adc_thre           = 12.8       ; //! femto-Coulomb
  m_xtalk_dy           = 1.0        ;
  m_res_fee            = 45.e-12    ;

/// fast simulation parameters
  m_res_tof            = 85.e-12    ;
  m_thre_tof           = 1.e-7      ;
  m_res_vpd            = 140.e-12   ;
  m_thre_vpd           = 1.e-7      ;

/// efficiency
  for(int i=0;i<120;i++) {
    for(int j=0;j<192;j++) {
      m_eff_tof[i][j] = 0.95;
    }
  }
  ///
  /// put the list of dead channels here
  ///
  for(int i=0;i<50;i++) {
    if(i<38) m_eff_vpd[i] = 0.95;
    else m_eff_vpd[i] = 0.0;
  }

}


/// default empty destructor
StTofSimParam::~StTofSimParam(){ /* nope */}


/// initializes calibration from XDF or dBase (not functioning yet)
void StTofSimParam::init(){
  // Char_t *InputXdfFile= "cts_pars.xdf";
  // St_XDFFile xdf(InputXdfFile);
  // St_DataSet *ctfs = xdf.NextEventGet();
  // 
  // assert (ctfs);
  // St_DataSetIter gime(ctfs);
  // St_cts_mpara *stafSimParam(0);
  // stafSimParam = (St_cts_mpara *) gime("cts_tof");
  // cts_mpara_st *sim = stafSimParam->GetTable();
  // 
  // // convert STAF database of simulation to StRoot database      
  // mSimParam = *sim;
}  

float StTofSimParam::nclus(float beta) const 
{
  if (beta>0.999999) beta = 0.999999;
  if (beta<0.) beta = 0.;

  float ga = 1./sqrt(1.-beta*beta);
  float para = 1./sqrt(ga);
  float clus = m_clus_par[0] + m_clus_par[1]*para + m_clus_par[2]*para*para + m_clus_par[3]*para*para*para;

  return clus;
}

void StTofSimParam::print(){
  LOG_INFO << "------StTofSimParam::print()------" << endm;
  if(mSimuTofp) {
    LOG_INFO << " ==>> TOFp parameters <<== " << endm;
    LOG_INFO << " adc_overflow       = " << m_adc_overflow       << endm; 
    LOG_INFO << " attlen             = " << m_attlen             << endm; 
    LOG_INFO << " cath_eff           = " << m_cath_eff           << endm; 
    LOG_INFO << " cath_surf          = " << m_cath_surf          << endm; 
    LOG_INFO << " delay              = " << m_delay              << endm; 
    LOG_INFO << " elec_noise         = " << m_elec_noise         << endm; 
    LOG_INFO << " gate_t0            = " << m_gate_t0            << endm; 
    LOG_INFO << " gate_width         = " << m_gate_width         << endm; 
    LOG_INFO << " geo_from_geant     = " << m_geo_from_geant     << endm; 
    LOG_INFO << " GeV_2_n_photons    = " << m_GeV_2_n_photons    << endm; 
    LOG_INFO << " nphe_to_adc        = " << m_nphe_to_adc        << endm; 
    LOG_INFO << " phys_noise         = " << m_phys_noise         << endm;
    LOG_INFO << " position_tolerance = " << m_position_tolerance << endm; 
    LOG_INFO << " slat_para          = " << m_slat_para          << endm; 
    LOG_INFO << " start_res          = " << m_start_res          << endm; 
    LOG_INFO << " surf_loss          = " << m_surf_loss          << endm; 
    LOG_INFO << " tdc_overflow       = " << m_tdc_overflow       << endm; 
    LOG_INFO << " time_res           = " << m_time_res           << endm;
  }
  if(mSimuVpd) {
    LOG_INFO << " ==>> VPD parameters <<== " << endm;
    LOG_INFO << " res_vpd            = " << m_res_vpd            << endm;
    LOG_INFO << " thre_vpd           = " << m_thre_vpd           << endm;
  }
  if(mSimuTofFast) {
    LOG_INFO << " ==>> MRPC TOF fast simu parameters <<== " << endm;
    LOG_INFO << " res_tof            = " << m_res_tof            << endm;
    LOG_INFO << " thre_tof           = " << m_thre_tof           << endm;
  }
  if(mSimuTofSlow) {
    LOG_INFO << " ==>> MRPC TOF slow simu parameters <<== " << endm;
    LOG_INFO << " n_gap              = " << m_n_gap              << endm;
    LOG_INFO << " nmax_clus          = " << m_nmax_clus          << endm;
    LOG_INFO << " d_in               = " << m_d_in               << endm;
    LOG_INFO << " d_out              = " << m_d_out              << endm;
    LOG_INFO << " d_gap              = " << m_d_gap              << endm;
    LOG_INFO << " alpha              = " << alpha()              << endm;
    LOG_INFO << " er                 = " << m_er                 << endm;
    LOG_INFO << " vd_mean            = " << m_vd_mean            << endm;
    LOG_INFO << " nmean_e            = " << m_nmean_e            << endm;
    LOG_INFO << " nmax_e             = " << m_nmax_e             << endm;
    LOG_INFO << " dt                 = " << m_dt                 << endm;
    LOG_INFO << " ndt                = " << m_ndt                << endm;
    LOG_INFO << " toffset            = " << m_toffset            << endm;
    LOG_INFO << " adc_thre           = " << m_adc_thre           << endm;
    LOG_INFO << " xtalk_dy           = " << m_xtalk_dy           << endm;
    LOG_INFO << " res_fee            = " << m_res_fee            << endm;
  }
  LOG_INFO << " ==>> VPD dead channels <<== " << endm;
  for(int i=0;i<38;i++) {
    if(m_eff_vpd[i]<1.e-4) {
      const char *westeast = (i/19) ? "West" : "East";
      int itube = i%19 + 1;
      LOG_INFO << "   " << westeast << " tube " << itube << endm;
    }
  }
  LOG_INFO << " ==>> TOF dead channels <<== " << endm;
  for(int i=0;i<120;i++) {
    for(int j=0;j<192;j++) {
      int itray = i+1;
      int imodule = j/6 + 1;
      int icell = j%6 + 1;
      if(m_eff_tof[i][j]<1.e-4) 
        LOG_INFO << "   tray " << itray << " module " << imodule << " cell " << icell << endm;
    }
  }
  LOG_INFO << "----------------------------------" << endm;
}
