#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_0.h"

//Thresholds are in ET with 0.0316GeV/count, maxed 255 @ 8.0GeV
u_int EMTHR1=32;                //1GeV
u_int EMTHR2=64;                //2GeV
u_int EMTHR3=95;                //3GeV
u_int HADTHR1=32;               //1GeV
u_int HADTHR2=64;               //2GeV
u_int HADTHR3=96;               //3GeV
u_int JETTHR1=64;               //2GeV
u_int JETTHR2=128;              //4GeV
u_int JETTHR3=255;              //8GeV
float EM_HERATIO_THR=1.0/4.0;   //H4x4 < E4x4 * Threshold
float HAD_HERATIO_THR=1.0/4.0;  //H4x4 > E4x4 * Threshold
float FPSTHR=50;                //FPS > 1/2 MIP

u_short fcs_trg_run_0(){
	u_short dsm_out ;	
	link_t link_out_2[4] ;	// Stage 2 outputs: 2 each North/South links
	geom_t geo ;

	// helpers
	static u_short adc_in[8] ;	// zapped to 0
	static int init_done ;
	static ped_gain_t ped_gain[2][3][20][32] ;

	printf("fcs_trg_run_0 Starting with NS=%d det=%d ecal=%d hcal=%d prs=%d link2=%d\n",
	       kNS,kDet,kDep,kEcalDep,kHcalDep,kPresDep,kLink2);
	if(init_done==0) {	// first time call; bring in the various databases
	    for(int ns=0;ns<kNS;ns++) {
		for(int det=0;det<kDet;det++) {
		    for(int dep=0;dep<kDep; dep++) {
			if(det==0 && dep>=kEcalDep) break;
			if(det==1 && dep>=kHcalDep) break;
			if(det==2 && dep>=kPresDep) break;
			for(int ch=0;ch<kCh;ch++) {
			    ped_gain[ns][det][dep][ch].ped = fcs_trg_pedestal[ns][det][dep][ch] ;
			    
			    int i_gain = (u_short)(fcs_trg_pt_correction[ns][det][dep][ch] * fcs_trg_gain_correction[ns][det][dep][ch]*64.0+0.5) ;
			    
			    ped_gain[ns][det][dep][ch].gain = i_gain ;
			    if(fcs_trgDebug>=4) 
				printf("fcs_trg_run init %1d %1d %2d %2d %4d %6.3f %6.3f %5d\n",
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
	    printf("fcs_trg_run init done\n");
	}

	for(int ns=0;ns<2;ns++) {
	    link_t link_ecal[kEcalDep] ;
	    link_t link_hcal[kHcalDep] ;
	    link_t link_pres[kPresDep] ;
	    geo.ns = ns ;
	    
	    for(int det=0;det<kDet;det++) {
		geo.det = det ;
		
		for(int dep=0;dep<kDep;dep++) {		    
		    if(det==0 && dep>=kEcalDep) break;
		    if(det==1 && dep>=kHcalDep) break;
		    if(det==2 && dep>=kPresDep) break;
		    u_int adc_prime[32] ;	// summed, pedestal subtracted, gain-corrected ADC
		    geo.dep = dep ;
		    
		    for(int ch=0;ch<kCh;ch++) {
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
		    if(det==0) {
			stage_1_0(adc_prime,geo,&link_ecal[dep]) ;	
		    }
		    else if(det==1) {
			stage_1_0(adc_prime,geo,&link_hcal[dep]) ;
		    }
		    else if(det==2) {
			stage_1_0(adc_prime,geo,&link_pres[dep]) ;
		    }
		}
		
	    }
	    
	    // Stage 2 executes here
	    stage_2_0(link_ecal, link_hcal, link_pres, geo, &link_out_2[ns*2]) ;				    
	}

	// Final stage
	stage_3_0(link_out_2, &dsm_out) ;

	return dsm_out ;
}


