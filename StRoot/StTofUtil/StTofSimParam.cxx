/*******************************************************************
 *
 * $Id: StTofSimParam.cxx,v 1.3 2003/04/22 00:03:35 geurts Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Simulation Parameters for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofSimParam.cxx,v $
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
#include <iostream.h>
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
  cout << "------StTofSimParam::print()------" << endl;
  cout << " adc_overflow       = " << m_adc_overflow       << endl; 
  cout << " attlen             = " << m_attlen             << endl; 
  cout << " cath_eff           = " << m_cath_eff           << endl; 
  cout << " cath_surf          = " << m_cath_surf          << endl; 
  cout << " delay              = " << m_delay              << endl; 
  cout << " elec_noise         = " << m_elec_noise         << endl; 
  cout << " gate_t0            = " << m_gate_t0            << endl; 
  cout << " gate_width         = " << m_gate_width         << endl; 
  cout << " geo_from_geant     = " << m_geo_from_geant     << endl; 
  cout << " GeV_2_n_photons    = " << m_GeV_2_n_photons    << endl; 
  cout << " nphe_to_adc        = " << m_nphe_to_adc        << endl; 
  cout << " phys_noise         = " << m_phys_noise         << endl;
  cout << " position_tolerance = " << m_position_tolerance << endl; 
  cout << " slat_para          = " << m_slat_para          << endl; 
  cout << " start_res          = " << m_start_res          << endl; 
  cout << " surf_loss          = " << m_surf_loss          << endl; 
  cout << " tdc_overflow       = " << m_tdc_overflow       << endl; 
  cout << " time_res           = " << m_time_res           << endl; 
  cout << "----------------------------------" << endl;
}
