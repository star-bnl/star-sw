/*:>--------------------------------------------------------------------
**: FILE:       energy_to_adc.c
**: HISTORY:
**:             00jan1998     Created by OGAWA, Akio
**:             10sep1998     Import Pavlinov Aleksei's codes 
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "energy_to_adc.h"
#include "emc_def.h"

extern float ran0(long*);

long type_of_call energy_to_adc_(
  TABLE_HEAD_ST     *ems_control_h,    EMS_CONTROL_ST      *ems_control ,
  TABLE_HEAD_ST *ems_cal_control_h,EMS_CAL_CONTROL_ST  *ems_cal_control ,
  TABLE_HEAD_ST    *emc_pedestal_h,   EMC_PEDESTAL_ST     *emc_pedestal ,
  TABLE_HEAD_ST    *emc_adcslope_h,   EMC_ADCSLOPE_ST     *emc_adcslope ,
  TABLE_HEAD_ST         *emc_adc_h,        EMC_ADC_ST          *emc_adc )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    energy_to_adc_
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
**:            emc_adc    - energy is input, adc channel is output 
**:           emc_adc_h   - header Structure for emc_adc
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, det, eta, phi, bits;
    long nok_c, nok_p, nok_s, nok_cc, ped_width, ped;
    long iseed = 99999;

    if(emc_adc_h->maxlen <= 0 ) {
      puts("emc_adc_h->maxlen is less equal zero");
      return STAFCV_BAD;
    }
    if(emc_adc_h->nok > emc_adc_h->maxlen){
      puts("emc_adc_h->nok is more than maxlen");
      return STAFCV_BAD;
    }
    nok_c=ems_control_h->nok-1;
    nok_cc=ems_cal_control_h->nok-1;
    nok_p=emc_pedestal_h->nok-1;
    nok_s=emc_adcslope_h->nok-1;
    if(ems_cal_control[nok_cc].iseed != 0) iseed=ems_cal_control[nok_cc].iseed;
    det = (long)emc_adc[0].det;
    switch(det){
    case BEMC:
      bits = (float)ems_control[nok_c].bemc_adcbits;
      break;
    case EEMCR: case EEMCL:
      bits = (float)ems_control[nok_c].eemc_adcbits;
      break;
    case BPRS:
      bits = (float)ems_control[nok_c].bprs_adcbits;
      break;
    case EPRSR: case EPRSL:
      bits = (float)ems_control[nok_c].eprs_adcbits;
      break;
    default:
      puts("ems_cal_control.det is invalid. Specify the detector\n");
      return STAFCV_BAD;
    }
    if(emc_pedestal[nok_p].det != det){
      puts("Pedestal data has wrong det");
      return STAFCV_BAD;
    }
    if(emc_adcslope[nok_s].det != det){
      puts("Adcslope data has wrong det");
      return STAFCV_BAD;
    }

    ped_width = ems_cal_control[nok_cc].ped_width;
    for(i = 0; i < emc_adc_h->nok; i++){
	eta = emc_adc[i].eta_bin;
	phi = emc_adc[i].phi_bin;
	ped = ped_width*(ran0(&iseed)-0.5);       /* put some width on pedestal */
	emc_adc[i].adc = ped + emc_pedestal[nok_p].ped[phi][eta]
	               + (float)emc_adc[i].adc/emc_adcslope[nok_p].p0[phi][eta];
	if (emc_adc[i].adc < 0){ 
	  emc_adc[i].adc = 0;              
	}
	if (emc_adc[i].adc > bits-1){ 
	  emc_adc[i].adc = bits - 1;              /*OVERFLOW*/
	}
    }    
    ems_cal_control[nok_cc].iseed=iseed;
    return STAFCV_OK;
}

