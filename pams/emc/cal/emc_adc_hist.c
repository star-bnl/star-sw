/**:>--------------------------------------------------------------------
**: FILE:       emc_adc_hist.c
**: HISTORY:
**:             00jan93-v000a-hpl- Created by OGAWA, Akio
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include <stdio.h>
#include "emc_adc_hist.h"
#include "emc_def.h" 
#include "cfortran.h"
#include "hbook.h"
#define BOOK     0
#define FILL     1
#define RESET    2
#define DEL      3
#define MAX_CH   10
#define MEAN_CH  11
#define GAUSS    12

long type_of_call emc_adc_hist_(
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
**:           emc_adc_cal - fitting results for mode>10
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
    char title[80], cdet[5], copt[5]="HIST";
    long i, j, k, det, neta, nphi, eta, phi, bits, base;
    long nok_c, nok_h, nok_a, nok_f, mode;
    int id, nbin, num=1, ic=1;
    int count, max_count, chan;
    float min, max, vmx=0.0, adc, one=1.0; 
    float c, av, sd, chi2, par[3];

    nok_h=emc_adc_hist_control_h->nok-1;
    det=emc_adc_hist_control[nok_h].det;
    base = emc_adc_hist_control[nok_h].id_base;
    mode = emc_adc_hist_control[nok_h].init;

    if(mode != FILL){
      nok_c=ems_control_h->nok-1;	
      min = (float)emc_adc_hist_control[nok_h].min;
      max = (float)emc_adc_hist_control[nok_h].max;
      nbin = (int)emc_adc_hist_control[nok_h].nbin;
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
	puts("***emc_adc_hist: emc_adc_hist_control.det is invalid\n");
	return STAFCV_BAD;
      }      
      if(mode >= MAX_CH){
	if(emc_adc_cal_h->nok < emc_adc_cal_h->maxlen){
	  nok_f = emc_adc_cal_h->nok++;
	}else{
	  puts("***emc_adc_hist: not enough space in emc_adc_cal\n");
	  return STAFCV_BAD;
	}
      }
      for(i=0; i<nphi; i++){
	for(j=0; j<neta; j++){
	  id=1000*(i+1)+(j+1)+base;
	  switch (mode){
	  case BOOK: 
	    if(HEXIST(id) == 1) HDELET(id);
	    sprintf(title,"%5s:phi=%d,eta=%d$", cdet, i+1, j+1);
	    HBOOK1(id, title, nbin, min, max, vmx); 
	    break;	
	  case RESET:
	    HRESET(id, " "); 
	    break;
	  case DEL:  
	    HDELET(id); 
	    break;
	  case MAX_CH:
	    max_count=0;
	    chan=0;
	    for(k=0; k<nbin; k++){
	      count = HI(id,k);
	      if(max_count < count){
		max_count=count;
		chan=k;
	      }
	    }
	    emc_adc_cal[nok_f].ch[i][j]=(short)chan;
	    emc_adc_cal[nok_f].dev[i][j]=0.0;
	    break;
	  case MEAN_CH:
	    emc_adc_cal[nok_f].ch[i][j]=(short)HSTATI(id, 1, copt, num);
	    emc_adc_cal[nok_f].dev[i][j]=(short)HSTATI(id, 2, copt, num);
	    break;
	  case GAUSS:
	    HFITGA(id, c, av, sd, chi2, ic, par); 
	    emc_adc_cal[nok_f].ch[i][j]=(short)av;
	    emc_adc_cal[nok_f].dev[i][j]=(short)sd;
	    break;
	  default:
	    puts("***emc_adc_hist: emc_adc_hist_control.init is invalid.\n");
	    return STAFCV_BAD;
	  }
	}
      }
    }else{    /* filling */
      nok_a=emc_adc_h->nok-1;
      for(i = 0; i < emc_adc_h->nok; i++){
	phi = (long)emc_adc[i].phi_bin;
        eta = (long)emc_adc[i].eta_bin;
      	id = 1000*(phi+1) + (eta+1) + base;
	adc = (float)emc_adc[i].adc;
	HF1(id, adc, one);
      }
    }
    return STAFCV_OK;
}



