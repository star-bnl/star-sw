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
  TABLE_HEAD_ST *fake_ped_header_h,   EMC_CALIB_HEADER_ST *fake_ped_header ,
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

    long i, j, k, l, nok_c, nok_cc, det, nmod, neta, nsub, temp, tot;
    long iseed=999; 

    nok_c = ems_control_h->nok - 1;
    nok_cc = ems_cal_control_h->nok - 1;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    det=ems_cal_control[nok_cc].det;
    switch(det){
    case BEMC: case BPRS: case BSMDE: case BSMDP:
    case EEMC: case EPRS: case ESMDE: case ESMDP:
      nmod = ems_control[nok_c].nmodule[det-1];
      neta = ems_control[nok_c].neta[det-1];   
      nsub = ems_control[nok_c].nsub[det-1];   
      break;
    default: 
      puts("***fake_pedestal: emc_cal_control.det is invalid. Specify the detector.");
      return STAFCV_BAD;
    }
    printf("fake_pedestal: det,nmod,neta,nsub=%d,%d,%d,%d\n",det,nmod,neta,nsub);
    if(nmod < 1 || neta < 1 || nsub < 1){
      puts("***fake_pedestal: nmod,neta or nsub is less than 1, need inputs in ems_control.");
      return STAFCV_BAD;
    }
    tot=nmod*neta*nsub;
    if(fake_ped_h->maxlen < tot ) {
      puts("***fake_pedestal: Need more space on fake_ped.");
      return STAFCV_BAD;
    }
    fake_ped_header[0].det    = det;
    fake_ped_header[0].nmodule= nmod;
    fake_ped_header[0].neta   = neta;
    fake_ped_header[0].nsub   = nsub;
    fake_ped_header[0].flag   = -1;              /* flag=-1 fake data */
    fake_ped_header[0].func   = 0;               /* func=0 for pedestal */
    l=0;
    for( i = 0; i < nmod ; i++ ){
      for( j = 0; j < neta ; j++ ){
	for( k = 0; k < nsub ; k++ ){ 
	  fake_ped[l].ped=(short)ems_cal_control[nok_cc].ped_channel
	    +(short)( ems_cal_control[nok_cc].ped_var
		      *(ran0(&iseed) - 0.5) );
	  l++;
	}
      } 
    }
    ems_cal_control[nok_cc].iseed=iseed;
    fake_ped_header_h->nok=1;
    fake_ped_h->nok=l;
    return STAFCV_OK;
}

