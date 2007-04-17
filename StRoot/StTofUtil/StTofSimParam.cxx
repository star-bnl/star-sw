/*******************************************************************
 *
 * $Id: StTofSimParam.cxx,v 1.5 2007/04/17 23:01:52 dongx Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Simulation Parameters for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofSimParam.cxx,v $
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
//#include "St_XDFFile.h"
//#include "St_DataSetIter.h"


/// default constructor, initializes default simulation parameters
StTofSimParam::StTofSimParam(){ 
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

void StTofSimParam::print(){
  LOG_INFO << "------StTofSimParam::print()------" << endm;
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
  LOG_INFO << "----------------------------------" << endm;
}
