/*:>--------------------------------------------------------------------
**: FILE:       emc_adc_sim
**: HISTORY:
**:             00jan1998     Created by OGAWA, Akio
**:             10sep1998     Import Pavlinov Aleksei's codes 
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "emc_adc_sim.h"
#include "emc_def.h"

extern float ran0(long*);

long type_of_call emc_adc_sim_(
  TABLE_HEAD_ST     *ems_control_h,     EMS_CONTROL_ST      *ems_control ,
  TABLE_HEAD_ST *ems_cal_control_h, EMS_CAL_CONTROL_ST  *ems_cal_control ,
  TABLE_HEAD_ST      *ped_header_h,EMC_CALIB_HEADER_ST       *ped_header ,
  TABLE_HEAD_ST    *emc_pedestal_h,    EMC_PEDESTAL_ST     *emc_pedestal ,
  TABLE_HEAD_ST      *slp_header_h,EMC_CALIB_HEADER_ST       *slp_header ,
  TABLE_HEAD_ST    *emc_adcslope_h,    EMC_ADCSLOPE_ST     *emc_adcslope ,
  TABLE_HEAD_ST         *emc_hits_h,       EMC_HITS_ST         *emc_hits )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    emc_adc_sim_
**: DESCRIPTION: Give fake pedestals and slope valiations to ADC
**: AUTHOR:     OGAWA, AKio (akio@bnl.gov)
**: ARGUMENTS:
**:       IN:
**:       ems_control     - control parameters
**:      ems_control_h    - header Structure for ems_control
**:       emc_pedestal    - pedestal data
**:      emc_pedestal_h   - header Structure for emc_pedestal
**:       emc_adcslope    - slope data
**:      emc_adcslope_h   - header Structure for emc_adcslope
**:    INOUT:
**:            emc_hits    - energy is input, adc channel is output 
**:           emc_hits_h   - header Structure for emc_hits
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, j, det, eta, module, sub, bits;
    long nok_c, nok_cc, ped_width, ped;
    long iseed = 99999;
    short nmod, neta, nsub, nse;

    if(emc_hits_h->nok==0) retyrn STAFCV_OK;
    nok_c=ems_control_h->nok-1;
    nok_cc=ems_cal_control_h->nok-1;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    det = (long)emc_hits[0].det;
    switch(det){
    case BEMC: case BPRS: case BSMDE: case BSMDP:
    case EEMC: case EPRS: case ESMDE: case ESMDP:
      bits = (float)ems_control[nok_c].adcbits[det-1];
      nmod = ems_control[nok_c].nmodule[det-1];
      neta = ems_control[nok_c].neta[det-1];
      nsub = ems_control[nok_c].nsub[det-1];
      break;
    default:
      puts("***emc_adc_sim: emc_hits has wrong det");
      return STAFCV_BAD;
    }
    if(ped_header[0].det != det){
      puts("***emc_adc_sim: Pedestal data has wrong det");
      return STAFCV_BAD;
    }
    if(nmod != ped_header[0].nmodule ||
       neta != ped_header[0].neta    ||
       nsub != ped_header[0].nsub    ){
      puts("***emc_adc_sim: Pedestal data has wrong module/eta/sub number");
      return STAFCV_BAD;
    }
    if(ped_header[0].func != 0){
      puts("***emc_adc_sim: Pedestal data is not pedestal");
      return STAFCV_BAD;
    }
    if(slp_header[0].det != det){
      puts("***emc_adc_sim: Adcslope data has wrong det");
      return STAFCV_BAD;
    }
    if(nmod != slp_header[0].nmodule ||
       neta != slp_header[0].neta    ||
       nsub != slp_header[0].nsub    ){
      puts("***emc_adc_sim: Adcslope data has wrong module/eta/sub number");
      return STAFCV_BAD;
    }
    ped_width = ems_cal_control[nok_cc].ped_width;
    nse=nsub*neta;
    switch(slp_header[0].func){
    case 1:
      for(i = 0; i < emc_hits_h->nok; i++){
        module = emc_hits[i].module;
	eta = emc_hits[i].eta;
	sub = emc_hits[i].sub;
	j = sub-1+(eta-1)*nsub+(module-1)*nse;
	ped = ped_width*(ran0(&iseed)-0.5);       /* put some width on pedestal */
	emc_hits[i].adc = ped + emc_pedestal[j].ped
	  + (float)emc_hits[i].adc/emc_adcslope[j].p0;	
	if (emc_hits[i].adc < 0){ 
	  emc_hits[i].adc = 0;              
	}
	if (emc_hits[i].adc > bits){ 
	  emc_hits[i].adc = bits;              /*OVERFLOW*/
	}
      }   
      break;
    default:
      puts("***emc_adc_sim: Function has not been implimented yet!");
      return STAFCV_BAD;
    }    

    ems_cal_control[nok_cc].iseed=iseed;
    return STAFCV_OK;
}

