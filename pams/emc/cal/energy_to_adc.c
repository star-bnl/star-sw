/*:>--------------------------------------------------------------------
**: FILE:       energy_to_adc.c
**: HISTORY:
**:             00jan93-v000a-hpl- Created by OGAWA, Akio
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
**: DESCRIPTION: Convert simulation energy deposit to ADC
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

    long i, det, eta, phi, bits, kink=0;
    long nok_c, nok_p, nok_s, nok_cc, ped_width, ped;
    long iseed = 99999;

    if(emc_adc_h->maxlen <= 0 ) {
      puts("emc_adc_h->maxlen is less equal zero\n");
      return STAFCV_BAD;
    }
    if(emc_adc_h->nok > emc_adc_h->maxlen){
      puts("emc_adc_h->nok is more than maxlen\n");
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
      kink=ems_cal_control[nok_cc].bemc_adckink;
      break;
    case EEMCR: case EEMCL:
      bits = (float)ems_control[nok_c].eemc_adcbits;
      kink=ems_cal_control[nok_cc].eemc_adckink;
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
	ped = ped_width*(ran0(&iseed)-0.5); /* put some width on pedestal */
	emc_adc[i].adc = ped + emc_pedestal[nok_p].pedA[phi][eta]
	               + emc_adc[i].energy/emc_adcslope[nok_p].pA0[phi][eta];
	if(ems_cal_control[nok_cc].force_B != 0 || 
           (kink != 0 && emc_adc[i].adc > kink)) {
	    emc_adc[i].adc = ped + emc_pedestal[nok_p].pedB[phi][eta]
	               + emc_adc[i].energy/emc_adcslope[nok_p].pB0[phi][eta]; 
	    if (emc_adc[i].adc < bits){ 
	      emc_adc[i].adc += bits;
	    } else {           /*overflow*/
	      emc_adc[i].adc = 2*bits - 1;
	    }
	}
    }    
    ems_cal_control[nok_cc].iseed=iseed;
    return STAFCV_OK;
}
