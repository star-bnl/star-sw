/*:>--------------------------------------------------------------------
**: FILE:       toy_simulator.c
**: HISTORY:
**:             19jan98-v000a-hpl- Created by OGAWA, AKio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "toy_simulator.h"
#include "emc_def.h"

extern float ran0(long*);
extern float gasdev(long*);

long type_of_call toy_simulator_(
  TABLE_HEAD_ST     *ems_control_h,   EMS_CONTROL_ST          *ems_control ,
  TABLE_HEAD_ST *ems_cal_control_h,   EMS_CAL_CONTROL_ST  *ems_cal_control ,
  TABLE_HEAD_ST         *emc_adc_h,   EMC_ADC_ST                  *emc_adc )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:   toy_simulator
**: DESCRIPTION: create a artifical EMC hits which looks like pedestal or led
**:              or whatever using random number generator.
**: AUTHOR:     OGAWA, Akio
**: ARGUMENTS:
**:       IN:
**:    INOUT: 
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, j, l, nout, n=0, det, nok_c, nok_cc;
    long eta, phi, bits;
    float noise, noise_wid, noise_lev, gaus_peak, gaus_wid, ocp;
    long iseed=999; 

    nok_c = ems_control_h->nok - 1;
    nok_cc = ems_cal_control_h->nok - 1;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    det = ems_cal_control[nok_cc].det;
    switch(det) {
    case BEMC: 
      phi = ems_control[nok_c].bemc_phi_nbin;
      eta = ems_control[nok_c].bemc_eta_nbin; 
      bits= (short)ems_control[nok_c].bemc_adcbits;
      break;
    case BPRS:
      phi = ems_control[nok_c].bemc_phi_nbin;
      eta = ems_control[nok_c].bemc_eta_nbin;
      bits= (short)ems_control[nok_c].bprs_adcbits;
      break;
    case EEMCR: case EEMCL: 
      phi = ems_control[nok_c].eemc_phi_nbin;
      eta = ems_control[nok_c].eemc_eta_nbin;   
      bits= (short)ems_control[nok_c].eemc_adcbits;
      break;
    case EPRSR: case EPRSL:
      phi = ems_control[nok_c].eemc_phi_nbin;
      eta = ems_control[nok_c].eemc_eta_nbin;
      bits= (short)ems_control[nok_c].eprs_adcbits;
      break;
    default: 
      puts("emc_cal_control.det is invalid. Specify the detector\n");
      return STAFCV_BAD;
    }
    nout = eta * phi;
    if(emc_adc_h->maxlen < nout){
      puts("Need more space for emc_adc\n");
      return STAFCV_BAD;
    }
    if(eta < 1 || phi < 1){
      puts("neta or ephi is less than 1, need inputs in ems_control\n");
      return STAFCV_BAD;
    }

    switch (ems_cal_control[nok_cc].toy_sim_mode){
    case 0:   /* Pedestal data for ADC-A, all zero energy */
      noise_lev = ems_cal_control[nok_cc].noise_level;
      noise_wid = ems_cal_control[nok_cc].noise_width;
      for( i = 0; i < phi ; i++ ){
	for( j = 0; j < eta ; j++ ){
	  emc_adc[n].det = det;
	  emc_adc[n].eta_bin = j;
	  emc_adc[n].phi_bin = i;
	  noise=ran0(&iseed);
	  if(noise > noise_lev){
	    emc_adc[n].energy = 0.0;
	  }else{  /* Add some flat noise, if you want */
	    emc_adc[n].energy = noise_wid*ran0(&iseed); 
	    if(emc_adc[n].energy < 0.0) emc_adc[n].energy = 0.0;
	  }
	  n++;
	}
      }
      break;
    case 1: /* Gaussian energy deposit */
      gaus_peak = ems_cal_control[nok_cc].gaus_peak;
      gaus_wid  = ems_cal_control[nok_cc].gaus_width;
      ocp       = ems_cal_control[nok_cc].occupancy;
      for( i = 0; i < phi ; i++ ){
        for( j = 0; j < eta ; j++ ){
	  emc_adc[n].det = det;
	  emc_adc[n].eta_bin = j;
	  emc_adc[n].phi_bin = i;
	  noise=ran0(&iseed);
	  if(noise < ocp){
	    emc_adc[n].energy = gaus_peak + gaus_wid * gasdev(&iseed);
	    if(emc_adc[n].energy < 0.0) emc_adc[n].energy = 0.0;
	    n++;
	  }	  
	}
      }
      break;     
    default :
      puts("ems_cal_control.toy_sim_mode is out of range");
      return STAFCV_BAD;
    }
    ems_cal_control[nok_cc].iseed=iseed;
    emc_adc_h->nok = n;
    return STAFCV_OK;
}


