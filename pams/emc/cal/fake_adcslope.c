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
  TABLE_HEAD_ST *fake_slp_header_h,  EMC_CALIB_HEADER_ST *fake_slp_header ,
  TABLE_HEAD_ST        *fake_slp_h,  EMC_ADCSLOPE_ST            *fake_slp )
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
    long i, j, k, l, nok_c, nok_cc, nok_fp;
    short func, det, nmod, neta, nsub, tot;
    long iseed=9999; 
    float bits;

    nok_c=ems_control_h->nok-1;
    nok_cc=ems_cal_control_h->nok-1;
    det = ems_cal_control[nok_cc].det;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    switch (det){
    case BEMC: case BPRS: case BSMDE: case BSMDP:
    case EEMC: case EPRS: case ESMDE: case ESMDP:
      nmod = ems_control[nok_c].nmodule[det-1];
      neta = ems_control[nok_c].neta[det-1];
      nsub = ems_control[nok_c].nsub[det-1];
      bits = (float)ems_control[nok_c].adcbits[det-1];
      break;
    default:
      puts("***fake_adcslope: ems_cal_control.det is invalid. Specify the detector");
      return STAFCV_BAD;
    }
    printf("fake_adcslope: det,nmod,neta,nsub=%d,%d,%d,%d\n",det,nmod,neta,nsub);
    if(nmod < 1 || neta < 1 || nsub < 1 ){
      puts("***fake_adcslope: nmod, neta or nsub is less than 1, need inputs in ems_control");
      return STAFCV_BAD;
    }
    tot=nmod*neta*nsub;
    if(fake_slp_h->maxlen < tot ) {
      puts("***fake_adcslope: Need more space on fake_slp.");
      return STAFCV_BAD;
    }
    func=ems_cal_control[nok_cc].slope_fun;
    fake_slp_header[0].det    = det;
    fake_slp_header[0].nmodule= nmod;
    fake_slp_header[0].neta   = neta;
    fake_slp_header[0].nsub   = nsub;
    fake_slp_header[0].flag   = -1;       /* flag=-1: fake data */
    fake_slp_header[0].func   = func;     /* func from ems_cal_control */
    switch(func){
    case 1:
      l=0;
      for( i = 0; i < nmod ; i++ ){
	for( j = 0; j < neta ; j++ ){
	  for( k = 0; k < nsub ; k++ ){
	    fake_slp[l].p0 = ems_cal_control[nok_cc].slope_ave *
	      (1.0 + ems_cal_control[nok_cc].slope_var*(ran0(&iseed)-0.5));
	    l++;
	  }
	}
      }
      break;
    default:
      puts("***fake_adcslope: Function has not been implimented yet!");
      return STAFCV_BAD;
    }    
    ems_cal_control[nok_cc].iseed=iseed;
    fake_slp_header_h->nok=1;
    fake_slp_h->nok=l;
    return STAFCV_OK;
}
