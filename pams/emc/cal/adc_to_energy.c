/*:>--------------------------------------------------------------------
**: FILE:       energy_to_adc.c
**: HISTORY:
**:             00jan93-v000a-hpl- Created by OGAWA, Akio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "adc_to_energy.h"
#include "emc_def.h"

long type_of_call adc_to_energy_(
  TABLE_HEAD_ST    *emc_pedestal_h,   EMC_PEDESTAL_ST     *emc_pedestal ,
  TABLE_HEAD_ST    *emc_adcslope_h,   EMC_ADCSLOPE_ST     *emc_adcslope ,
  TABLE_HEAD_ST         *emc_adc_h,        EMC_ADC_ST          *emc_adc )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    adc_to_energy_
**: DESCRIPTION: Convert ADC to energy
**: AUTHOR:     OGAWA, AKio (akio@bnl.gov)
**: ARGUMENTS:
**:       IN:
**:       emc_pedestal    - pedestal data
**:      emc_pedestal_h   - header Structure for emc_pedestal
**:       emc_adcslope    - slope data
**:      emc_adcslope_h   - header Structure for emc_adcslope
**:    INOUT:
**:            emc_adc    - adc channel is output, energy is output 
**:           emc_adc_h   - header Structure for emc_adc
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, det, eta, phi, nok_p, nok_s;

    if(emc_adc_h->nok <= 0 ) {
      puts("adc_to_energy : emc_adc_h->nok is less equal zero. No input.\n");
      return STAFCV_OK;
    }
    nok_p=emc_pedestal_h->nok-1;
    nok_s=emc_adcslope_h->nok-1;
    det = (long)emc_adc[0].det;
    switch(det){
    case BEMC: case EEMCR: case EEMCL:
    case BPRS: case EPRSR: case EPRSL:
      break;
    default:
      puts("***adc_to_energy: emc_adc[0].det is invalid. Unknown detector.\n");
      return STAFCV_BAD;
    }
    if(emc_pedestal[nok_p].det != det){
      puts("***adc_to_energy: Pedestal data is not for this datector.\n");
      return STAFCV_BAD;
    }
    if(emc_adcslope[nok_s].det != det){
      puts("***adc_to_energy: Adcslope data is not for this detector.\n");
      return STAFCV_BAD;
    }
    for(i = 0; i < emc_adc_h->nok; i++){
      eta = emc_adc[i].eta_bin;
      phi = emc_adc[i].phi_bin;
      emc_adc[i].energy = (emc_adc[i].adc - emc_pedestal[nok_p].ped[phi][eta])
	                / emc_adcslope[nok_p].p0[phi][eta];
      if(emc_adc[i].energy < 0.0){
	emc_adc[i].energy = 0.0;
      }
    }    
    return STAFCV_OK;
}

