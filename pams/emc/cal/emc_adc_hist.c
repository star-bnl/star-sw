/*:>--------------------------------------------------------------------
**: FILE:       emc_adc_hist.c
**: HISTORY:
**:             00jan93-v000a-hpl- Created by OGAWA, Akio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "emc_adc_hist.h"
#include "emc_def.h"

long emc_adc_hist_(
  TABLE_HEAD_ST          *ems_control_h, EMS_CONTROL_ST           *ems_control ,
  TABLE_HEAD_ST *emc_adc_hist_control_h, EMC_ADC_HIST_CONTROL_ST  *emc_adc_hist_control ,
  TABLE_HEAD_ST              *emc_adc_h, EMC_ADC_ST               *emc_adc ,
  TABLE_HEAD_ST          *emc_adc_cal_h, EMC_ADC_CAL_ST           *emc_adc_cal )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    emc_adc_hist.c
**: DESCRIPTION: EMC ADC histgraming interface
**: AUTHOR:     OGAWA, AKio (akio@bnl.gov)
**: ARGUMENTS:
**:       IN:
**:       ems_control     - control parameters
**:      ems_control_h    - header Structure for ems_control
**:       emc_adc_hist_control  - control parameters
**:      emc_adc_hist_control_h - header Structure for ems_control
**:            emc_adc    - input adc channels
**:           emc_adc_h   - header Structure for emc_adc
**:    INOUT: files...
**:      OUT: to the histgrams...
**:           emc_adc_cal - fitting results foor mode 5
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
    char title[80], cdet[5];
    long i, j, det, neta, nphi, eta, phi, bits, base, id, nbin;
    long nok_c, nok_h, nok_a;
    float min, max, vmx=0.0, adc, one=1.0; 

    nok_h=emc_adc_hist_control_h->nok-1;
    det=emc_adc_hist_control[nok_h].det;
    base = emc_adc_hist_control[nok_h].id_base;
    switch (emc_adc_hist_control[nok_h].init){

    case 0: /* book histgrams */
      nok_c=ems_control_h->nok-1;
      min = (float)emc_adc_hist_control[nok_h].min;
      max = (float)emc_adc_hist_control[nok_h].max;
      nbin = emc_adc_hist_control[nok_h].nbin;
      switch (det) {
      case BEMC:
	nphi = ems_control[nok_c].bemc_phi_nbin;
	neta = ems_control[nok_c].bemc_eta_nbin;
	bits = (float)ems_control[nok_c].bemc_adcbits;
	sprintf(cdet, "BEMC ");
	break;
      case EEMCR: 
	nphi = ems_control[nok_c].eemc_phi_nbin;
	neta = ems_control[nok_c].eemc_eta_nbin;
	bits = (float)ems_control[nok_c].eemc_adcbits;
	sprintf(cdet, "EEMCR");
	break;
      case EEMCL: 
	nphi = ems_control[nok_c].eemc_phi_nbin;
	neta = ems_control[nok_c].eemc_eta_nbin;
	bits = (float)ems_control[nok_c].eemc_adcbits;
	sprintf(cdet, "EEMCL");
	break;
      case BPRS:
	nphi = ems_control[nok_c].bemc_phi_nbin;
	neta = ems_control[nok_c].bemc_eta_nbin;
	bits = (float)ems_control[nok_c].bprs_adcbits;
	sprintf(cdet, "BPRS ");
	break;
      case EPRSR: 
	nphi = ems_control[nok_c].eemc_phi_nbin;
	neta = ems_control[nok_c].eemc_eta_nbin;
	bits = (float)ems_control[nok_c].eprs_adcbits;
	sprintf(cdet, "EPRSR");
	break;
      case EPRSL:
	nphi = ems_control[nok_c].eemc_phi_nbin;
	neta = ems_control[nok_c].eemc_eta_nbin;
	bits = (float)ems_control[nok_c].eprs_adcbits;
	sprintf(cdet, "EPRSL");
	break;
      default:
	puts("emc_adc_hist_control.det is invalid. Specify the detector\n");
	return STAFCV_BAD;
      }      
      for(i=0; i<nphi; i++){
	for(j=0; j<neta; j++){
	  id=1000*i+j+base;
	  sprintf(title,"%5s:phi=%d,eta=%d$\n", cdet, i, j);
	  printf("%40s\n",title);
	  printf("%d, %d, %f, %f, %f\n", id, nbin, min, max, vmx);
	  hbook1_(&id, title, &nbin, &min, &max, &vmx); 
	}
      }  
      emc_adc_hist_control[nok_h].init = 1;
      break;

    case 1:  /* filling */
      nok_a=emc_adc_h->nok-1;
      for(i = 0; i < emc_adc_h->nok; i++){
	phi = (long)emc_adc[i].phi_bin;
        eta = (long)emc_adc[i].eta_bin;
	id = 1000*phi + eta + base;
	adc = (float)emc_adc[i].adc;
	hf1_(&id, &adc, &one);
      }
      break;

    default:
      puts("emc_adc_hist_control.init is invalid.\n");
      return STAFCV_BAD;
    }
    return STAFCV_OK;
}





