/*:>--------------------------------------------------------------------
**: FILE:       fake_adcslope.c
**: HISTORY:
**:             19jan98-v000a-hpl- Created by OGAWA, AKio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "fake_adcslope.h"
#include "emc_def.h"

extern float ran0(long*);

long type_of_call fake_adcslope_(
  TABLE_HEAD_ST     *ems_control_h,  EMS_CONTROL_ST          *ems_control ,
  TABLE_HEAD_ST *ems_cal_control_h,  EMS_CAL_CONTROL_ST  *ems_cal_control ,
  TABLE_HEAD_ST   *fake_adcslope_h,  EMC_ADCSLOPE_ST       *fake_adcslope )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    fake_adcslope_
**: DESCRIPTION: Create a fake slope data using random number generator.
**: AUTHOR:     OGAWA, Akio
**: ARGUMENTS:
**:       IN:
**:    INOUT: 
**:           fake_adcslope    - fake adc slope data
**:          fake_adcslppe_h   - header Structure for fake_adcslpoe
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
    long i, j, n, func, nok_c, nok_cc, nok_fp, det;
    long iseed=999; 
    float bits;

    if(fake_adcslope_h->maxlen <= 0 ) {
      puts("maxlen is less equal zero\n");
      return STAFCV_BAD;
    }
    if(fake_adcslope_h->nok > fake_adcslope_h->maxlen){
      puts("nok is more than maxlen\n");
      return STAFCV_BAD;
    }
  
    n=fake_adcslope_h->nok;
    nok_c=ems_control_h->nok-1;
    nok_cc=ems_cal_control_h->nok-1;
    det = ems_cal_control[nok_cc].det;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    switch (det){
    case BEMC:
      fake_adcslope[n].nphi = ems_control[nok_c].bemc_phi_nbin;
      fake_adcslope[n].neta = ems_control[nok_c].bemc_eta_nbin;
      bits = (float)ems_control[nok_c].bemc_adcbits;
      break;
    case EEMCR: case EEMCL: 
      fake_adcslope[n].nphi = ems_control[nok_c].eemc_phi_nbin;
      fake_adcslope[n].neta = ems_control[nok_c].eemc_eta_nbin;
      bits = (float)ems_control[nok_c].eemc_adcbits;
      break;
    case BPRS:
      fake_adcslope[n].nphi = ems_control[nok_c].bemc_phi_nbin;
      fake_adcslope[n].neta = ems_control[nok_c].bemc_eta_nbin;
      bits = (float)ems_control[nok_c].bprs_adcbits;
      break;
    case EPRSR: case EPRSL:
      fake_adcslope[n].nphi = ems_control[nok_c].eemc_phi_nbin;
      fake_adcslope[n].neta = ems_control[nok_c].eemc_eta_nbin;
      bits = (float)ems_control[nok_c].eprs_adcbits;
      break;
    default:
      puts("ems_cal_control.det is invalid. Specify the detector\n");
      return STAFCV_BAD;
    }
    if(fake_adcslope[n].neta < 1 || fake_adcslope[n].nphi < 1){
      puts("neta or ephi is less than 1, need inputs in ems_control\n");
      return STAFCV_BAD;
    }
    fake_adcslope[n].flag = -1; /* flag=-1: fake data */
    fake_adcslope[n].det = det; /* flag=-1: fake data */
    func=ems_cal_control[nok_cc].slope_fun;
    switch(func){
    case 1:
      for( i = 0; i < fake_adcslope[n].nphi ; i++ ){
	for( j = 0; j < fake_adcslope[n].neta ; j++ ){
	  fake_adcslope[n].p0[i][j]= ems_cal_control[nok_cc].slope_ave *
	    (1.0 + ems_cal_control[nok_cc].slope_var*(ran0(&iseed)-0.5));
	}
      }
      break;
    default:
      puts("Function is not implimented yet!");
      return STAFCV_BAD;
    }    
    fake_adcslope[n].fun=func;
    fake_adcslope_h->nok++;
    ems_cal_control[nok_cc].iseed=iseed;
    return STAFCV_OK;
}
