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
  TABLE_HEAD_ST      *ped_header_h,EMC_CALIB_HEADER_ST       *ped_header ,
  TABLE_HEAD_ST    *emc_pedestal_h,    EMC_PEDESTAL_ST     *emc_pedestal ,
  TABLE_HEAD_ST      *slp_header_h,EMC_CALIB_HEADER_ST       *slp_header ,
  TABLE_HEAD_ST    *emc_adcslope_h,    EMC_ADCSLOPE_ST     *emc_adcslope ,
  TABLE_HEAD_ST        *emc_hits_h,        EMC_HITS_ST         *emc_hits )
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
**:            emc_hits    - adc channel is output, energy is output 
**:           emc_hits_h   - header Structure for emc_hits
**:      OUT:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long i, j, det, module, eta, sub, nmod, neta, nsub, nse;
    float slp;

    det = (long)emc_hits[0].det;
    switch(det){
    case BEMC: case BPRS: case BSMDE: case BSMDP:
    case EEMC: case EPRS: case ESMDE: case ESMDP:
      break;
    default:
      puts("***adc_to_energy: emc_hits[0].det is invalid. Unknown detector");
      return STAFCV_BAD;
    }
    if(ped_header[0].det != det){
      puts("***adc_to_energy: Pedestal data is not for this datector");
      return STAFCV_BAD;
    }
    if(slp_header[0].det != det){
      puts("***adc_to_energy: Adcslope data is not for this detector");
      return STAFCV_BAD;
    }
    nmod = ped_header[0].nmodule;
    neta = ped_header[0].neta;
    nsub = ped_header[0].nsub;
    nse=nsub*neta;
    if(nmod != slp_header[0].nmodule ||
       neta != slp_header[0].neta    ||
       nsub != slp_header[0].nsub    ){
      puts("***adc_to_energy: Pedestal & Adcslope data have different # of module/eta/sub");
      return STAFCV_BAD;
    }
    switch(slp_header[0].func){
    case 1:
      for(i = 0; i < emc_hits_h->nok; i++){
	if(emc_hits[i].adc>-1){   /* if adc=-1, leave energy as it is */
	  module = emc_hits[i].module;
	  eta    = emc_hits[i].eta;
	  sub    = emc_hits[i].sub;
	  j = sub-1+(eta-1)*nsub+(module-1)*nse;
	  slp    = emc_adcslope[j].p0;      
	  if(slp > 0.0){
	    emc_hits[i].energy = (emc_hits[i].adc-emc_pedestal[j].ped)/slp;
	    if(emc_hits[i].energy < 0.0){
	      emc_hits[i].energy = 0.0;
	    }
	  } else {
	    puts("***adc_to_energy: Adcslope data is less equal 0.0");
	    printf("***adc_to_energy: module,eta,sub= %d %d %f\n",module,eta,sub);
	    emc_hits[i].energy = -1.0;
	  }
	}    
      }
      break;
    default:
      puts("***emc_adc_sim: Function has not been implimented yet!");
      return STAFCV_BAD;
    }    
    return STAFCV_OK;
}

