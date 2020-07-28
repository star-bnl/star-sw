#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_201901.h"

//Thresholds are in ET with 0.0316GeV/count, maxed 256 @ 8.0GeV
u_int EMTHR1_201901=16;                //0.5GeV
u_int EMTHR2_201901=32;                //1.0GeV
u_int HADTHR1_201901=16;               //0.5GeV
u_int HADTHR2_201901=32;               //1.0GeV
u_int JETTHR1_201901=32;               //1.0GeV
u_int JETTHR2_201901=64;               //2.0GeV
float EM_HERATIO_THR_201901=1.0/4.0;   //H4x4 < E4x4 * Threshold
float HAD_HERATIO_THR_201901=1.0/4.0;  //H4x4 > E4x4 * Threshold
float FPSTHR_201901=50;                //FPS > 1/2 MIP

//u_int TRG_SELECT_201901=0x3ff;    //All 10 Triggers ORed and sent to TCU
u_int TRG_SELECT_201901=0x358;      //GAM2(0x8),ELE1(0x10),HAD1(0x40),JP1(0x100),JP2(0x200)

u_short fcs_trg_run_201901(){
    u_short dsm_out ;	
    link_t link_out_2[kNS*kLink2];  // Stage 2 outputs: 2 each North/South links
    geom_t geo ;
    
    // helpers
    static u_short adc_in[8] ;	// zapped to 0
    static int init_done ;
    static ped_gain_t ped_gain[kNS][kDet][kDep][kCh];
    
    printf("fcs_trg_run_201901 Starting with NS=%d det=%d ecal=%d hcal=%d prs=%d link2=%d\n",
	   kNS,kDet,kDep,kEcalDep,kHcalDep,kPresDep,kLink2);
    if(init_done==0) {	// first time call; bring in the various databases
	for(int ns=0;ns<kNS;ns++) {
	    if(ns==0) continue;
	    for(int det=0;det<kDet;det++) {
		for(int dep=0;dep<kDep;dep++) {
		    if(det==0 && dep>=kEcalDep) break;
		    if(det==1 && dep>=kHcalDep) break;
		    if(det==2 && dep>=kPresDep) break;
		    for(int ch=0;ch<kCh;ch++) {
			ped_gain[ns][det][dep][ch].ped = fcs_trg_pedestal[ns][det][dep][ch] ;
			
			int i_gain = (u_short)(fcs_trg_pt_correction[ns][det][dep][ch] * fcs_trg_gain_correction[ns][det][dep][ch]*64.0+0.5) ;
			
			ped_gain[ns][det][dep][ch].gain = i_gain ;
			if(fcs_trgDebug>=4) 
			    printf("fcs_trg_run_201901 init %1d %1d %2d %2d %4d %6.3f %6.3f %5d\n",
				   ns,det,dep,ch,
				   ped_gain[ns][det][dep][ch].ped,
				   fcs_trg_pt_correction[ns][det][dep][ch],
				   fcs_trg_gain_correction[ns][det][dep][ch],
				   ped_gain[ns][det][dep][ch].gain);			
			
		    }
		}
	    }
	}	
	init_done = 1 ;
	printf("fcs_trg_run_201901 init done\n");
    }
    
    for(int ns=0;ns<kNS;ns++) {
	if(ns==0) continue;
	link_t link_ecal[kEcalDep];
	link_t link_hcal[kHcalDep];	
	link_t link_pres[kPresDep];	
	geo.ns = ns ;
	
	for(int det=0; det<kDet; det++) {	    
	    geo.det = det ;
	    
	    for(int dep=0;dep<kDep; dep++) {
		if(det==0 && dep>=kEcalDep) break;
		if(det==1 && dep>=kHcalDep) break;
		if(det==2 && dep>=kPresDep) break;
		u_int adc_prime[32] ;	// summed, pedestal subtracted, gain-corrected ADC		
		geo.dep = dep ;
		
		for(int ch=0; ch<kCh; ch++) {
		    ped_gain_t pg ;
		    
		    pg.ped = ped_gain[ns][det][dep][ch].ped ;
		    pg.gain = ped_gain[ns][det][dep][ch].gain ;
		    
		    geo.ch = ch ;
		    
		    //since this is used in the simulation where I get only 1 ADC value for the RHIC tick 
		    // I will mock up the 8 samples thusly:
		    adc_in[3] = fcs_trg_sim_adc[ns][det][dep][ch] ;
		    
		    for(int tick=0;tick<8;tick++) {
			stage_0(adc_in[tick],tick,geo,&pg,&adc_prime[ch]) ;
		    }
		}
		
		// now run the per-board stage 1
		switch(det){
		case 0: stage_1_201901(adc_prime,geo,&link_ecal[dep]); break;	
		case 1: stage_1_201901(adc_prime,geo,&link_hcal[dep]);	break;
		case 2: stage_1_201901(adc_prime,geo,&link_pres[dep]);	break;
		}
	    }	    
	 }	
	 // Stage 2 executes here. For run19, this send bits to TCU
	stage_2_201901(link_ecal, link_hcal, link_pres, geo, &link_out_2[ns*kLink2]);	       
    }    
    // Final stage, but no stage3 for run19
    stage_3_201901(link_out_2, &dsm_out) ;
    
    return dsm_out ;
}


