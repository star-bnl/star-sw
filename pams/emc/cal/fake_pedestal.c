/*:>--------------------------------------------------------------------
**: FILE:       fake_pedestal.c
**: HISTORY:
**:             19jan98-v000a-hpl- Created by OGAWA, AKio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "fake_pedestal.h"
#include "emc_def.h"

extern float ran0(long*);

long type_of_call fake_pedestal_(
  TABLE_HEAD_ST     *ems_control_h,   EMS_CONTROL_ST          *ems_control ,
  TABLE_HEAD_ST *ems_cal_control_h,   EMS_CAL_CONTROL_ST  *ems_cal_control ,
  TABLE_HEAD_ST        *fake_ped_h,   EMC_PEDESTAL_ST            *fake_ped )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    fake_pedestal_
**: DESCRIPTION: Create a fake pedestal parameters.
**: AUTHOR:     OGAWA, Akio
**: ARGUMENTS:
**:       IN:
**:    INOUT: 
**:           fake_ped    - fake pedestal data
**:          fake_ped_h   - header Structure for fake_ped
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, j, n, nok_c, nok_cc;
    long iseed=999; 

    if(fake_ped_h->maxlen <= 0 ) {
      puts("maxlen is less equal zero");
      return STAFCV_BAD;
    }
    if(fake_ped_h->nok+1 > fake_ped_h->maxlen){
      puts("nok is more than maxlen");
      return STAFCV_BAD;
    }
    n = fake_ped_h->nok;
    nok_c = ems_control_h->nok - 1;
    nok_cc = ems_cal_control_h->nok - 1;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    switch(ems_cal_control[nok_cc].det){
    case EEMCR: case EEMCL: case EPRSR: case EPRSL:
      fake_ped[n].nphi = ems_control[nok_c].eemc_phi_nbin;
      fake_ped[n].neta = ems_control[nok_c].eemc_eta_nbin;   
      break;
    case BEMC: case BPRS:
      fake_ped[n].nphi = ems_control[nok_c].bemc_phi_nbin;
      fake_ped[n].neta = ems_control[nok_c].bemc_eta_nbin;
      break;
    default: 
      puts("emc_cal_control.det is invalid. Specify the detector.");
      return STAFCV_BAD;
    }
    if(fake_ped[n].neta < 1 || fake_ped[n].nphi < 1){
      puts("neta or ephi is less than 1, need inputs in ems_control.");
      return STAFCV_BAD;
    }
    fake_ped[n].flag = -1;              /* flag=-1 fake data */
    fake_ped[n].det  = ems_cal_control[nok_cc].det; 
    for( i = 0; i < fake_ped[n].nphi ; i++ ){
      for( j = 0; j < fake_ped[n].neta ; j++ ){
	fake_ped[n].ped[i][j]=(short)ems_cal_control[nok_cc].ped_channel
                             +(short)( ems_cal_control[nok_cc].ped_var
                             *(ran0(&iseed) - 0.5) );
      }
    } 
    ems_cal_control[nok_cc].iseed=iseed;
    fake_ped_h->nok++;
    return STAFCV_OK;
}

