#include <stdio.h>
#include <sys/types.h>
#include <string.h>

// Serial link abstraction type. I want it this way because this is how
// it actually works in the FPGA: 8 strobes of 8 bits
struct link_t {
	u_char d[8] ;	// thus 8x8=64 bits
};

// GLobal geometry type which can be used by the procedures below if
// they need to "know" who they are. 
struct geom_t {
	u_char ns ;	// 0=north,1=south
	u_char det ;	// 0=ecal,1=hcal,2=pre
	u_char dep ;	// 0..22 e.g. ECAL has 23 boards per side, others much less
	u_char ch ;	// 0..31 used only for debugging really
} ;


// Per-channel pedestal/gain-correction tables. Used only in Stage 0
// machinations.
struct ped_gain_t {
	u_short ped ;	// 15 bits max
	u_short gain ;	// 10 bits : 4.6 fixed point
} ;

// Pedestal & gain tables for Stage 0
static ped_gain_t ped_gain[2][3][23][32] ;	// NS,DET,DEP,CH



// Stage 0 is where the raw ADCs get summed, pedestal subtracted, gain corrected.
// This I expect to do within the framework.
// The example below should be pretty finished.
 
int stage_0(short radc, int timebin, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	static int sum ;
	static int peak ;
	static int last ;
	
	switch(timebin) {
	case 0 :
		sum = radc ;	// reset 
		peak = 0 ;	// reset
		last = 0 ;	// reset 
		break ;	
	case 1 :
		sum += radc ;

		break ;
	case 2 :
		sum += radc ;

		last = radc ;	// cache tb==2
		
		break ;
	case 3 :		// assume this is where the peak should fall
		sum += radc ;

		if(radc >= last) peak = 1 ;	// rising part

		last = radc ;	// cache also tb==3

		break ;
	case 4 :
		sum += radc ;

		if(radc < last) peak |= 2 ;	// falling part

		break ;
	case 5 :
		sum += radc ;

		break ;
	case 6 :
		sum += radc ;

		break ;
	case 7 :
		sum += radc ;

		sum -= pg->ped ;	// subtract pedestal
		if(sum < 0) sum = 0 ;	// no underflow allowed

		sum *= pg->gain ;	// gain & pt correction -- note fixed point multiplication with 4.6f

		sum >>= 6 ;		// back to 19 bits
 
		break ;
	}


	if(timebin==7 && peak==3) *dta_out = sum ;	// valid datum!!!
	else *dta_out = 0 ;

//	printf("---> %d:%d:%d:%d = tick %d: peak %d, sum %d\n",
//	       geo.ns,geo.det,geo.dep,geo.ch,
//	       timebin,peak,*dta_out) ;

	return 0 ;
}



// First stage Trigger algorithm: local to any DEP board.
// Inputs are 32 bit ADC values, output is what gets sent on the outgoing link.
// Processing is detector dependent thus one expects to use the geometry.

int stage_1(u_int adc[32], geom_t geo, link_t *output)
{
	//simple high tower example

	u_int max_i = 0 ;
	u_int max = 0 ;


	for(int i=0;i<32;i++) {
		if(adc[i] > max) {
			max = adc[i] ;
			max_i = i ;
		}
	}

	printf("stage_1: %d:%d:%d = max_i %d = %d max\n",geo.ns,geo.det,geo.dep,max_i,max) ;

	output->d[0] = max & 0xFF ;
	output->d[1] = (max >> 8) & 0xFF ;
	output->d[2] = (max >> 16) & 0xFF ;
	output->d[3] = (max >> 24) & 0xFF ;
	output->d[4] = max_i ;	
	output->d[5] = geo.dep ;
	output->d[6] = geo.ns ;
	output->d[7] = 0xAB ;

	return 0 ;
}


// Stage 2v is reserved if we ever want to pass data from the 4 slaves to 1 master.
// Currently unused but is here for completeness.

int stage_2v(link_t slave[5], geom_t geo, link_t *output) {

	memset(output,0,sizeof(link_t)) ;

	return 0 ;
}


// Processing on the North or South DEP/IO flavoured board. 
// Inputs are up to 32 links but I already organized them according to strawman.
// output is 1 link over the external OUT connector.
// Right now we assume we have 20 inputs from ECAL, 6 from HCAL and 4 from PRE.
// We also assume there is no need to know if this is North or South as
// the processing is exactly the same. Right??

int stage_2(link_t ecal[20], link_t hcal[6], link_t pre[4], geom_t geo, link_t *output)
{
	// Simple High Tower algorithm for test

	u_int max_ecal_adc ;
	u_int max_ecal_id ;

	max_ecal_adc = 0 ;
	max_ecal_id = 0 ;

	for(int i=0;i<20;i++) {
		u_int adc = ecal[i].d[0] | (ecal[i].d[1]<<8) | (ecal[i].d[2]<<16) | (ecal[i].d[3]<<24) ;
		if(adc > max_ecal_adc) {
			max_ecal_adc = adc ;
			max_ecal_id = ecal[i].d[5]*32+ecal[i].d[4] ;	// linearized channel
		}
	}

	//to 16 bits
	printf("stage_2: NS %d: ecal id %d, val %d\n",geo.ns,max_ecal_id,max_ecal_adc) ;

	memset(output,0,sizeof(link_t)) ;

	output->d[0] = max_ecal_adc & 0xFF ;
	output->d[1] = (max_ecal_adc >> 8) & 0xFF ;
	output->d[2] = (max_ecal_adc >> 16) & 0xFF ;

	output->d[7] = 0x12 ;	// debugging

	return 0 ;
}

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

int stage_3(link_t link[2], u_short *dsm_out)
{
	// High Tower trigger example

	u_int ecal_adc[2] ;
	u_int max_ecal_adc ;

	ecal_adc[0] = (link[0].d[2]<<16) | (link[0].d[1]<<8) | link[0].d[0] ;
	ecal_adc[1] = (link[1].d[2]<<16) | (link[1].d[1]<<8) | link[1].d[0] ;


	if(ecal_adc[0]>ecal_adc[1]) {
		max_ecal_adc = ecal_adc[0] ;
	}
	else {
		max_ecal_adc = ecal_adc[1] ;
	}

	printf("stage_3: max ECAL ADC %d\n",max_ecal_adc) ;

	*dsm_out = 0 ;


	// for an example, hardcoded High Tower Threshods: 10,100,1000
	if(max_ecal_adc > 10) *dsm_out |= 1 ;
	if(max_ecal_adc > 100) *dsm_out |= 2 ;
	if(max_ecal_adc > 1000) *dsm_out |= 4 ;

	return 0 ;
}



// Main driver code with an example read from a file.
// The core loops should not be modified since they already
// code most of what is known about FCS.

int main()
{
	FILE *f = fopen("dep_adc.txt","r") ;

	u_short adc[2][3][23][32][8] ;	// ns,det,dep,ch,tick
	int event = 0 ;

	for(int ns=0;ns<2;ns++) {
		for(int det=0;det<3;det++) {
			for(int dep=0;dep<23;dep++) {
				for(int ch=0;ch<32;ch++) {
					// mock-up on how this is going to be calculated
					double pt_correction = 4.8 ;	// can be from 0 to 16
					double gain_correction = 1.2 ;	// typically around 1.0

					u_int i_corr = (u_short) (pt_correction*gain_correction*64+0.5) ;
					i_corr &= 0x3FF ;	// just 10 bits in the 4.6 fixed point, or resolution is 1/64

					ped_gain[ns][det][dep][ch].gain = i_corr ;
				}
			}
		}
	}

	memset(adc,0,sizeof(adc)) ;

	while(!feof(f)) {

		int tick, ns, det, dep, ch, radc ;
		int ret = fscanf(f,"%d %d %d %d %d %d",&tick,&ns,&det,&dep,&ch,&radc) ;

		//printf("file: ret %d: tick %d, radc %d\n",ret,tick,radc) ;

		if(ret != 6) continue ;	// garbage on input?

		if(tick<0 || tick>7) {	// end-of-RHIC-strobe --> start event
			u_short dsm_out ;
			link_t link_out_2[2] ;	// Stage 2 outputs: 2 North/South links
			geom_t geo ;

			event++ ;	// events count from 1


			for(ns=0;ns<2;ns++) {
				link_t link_ecal[20] ;
				link_t link_hcal[6] ;
				link_t link_pre[4] ;

				geo.ns = ns ;

				for(det=0;det<3;det++) {

					geo.det = det ;

					for(dep=0;dep<20;dep++) {
						u_int adc_prime[32] ;	// summed, pedestal subtracted, gain-corrected ADC

						geo.dep = dep ;

						for(ch=0;ch<32;ch++) {
							ped_gain_t pg ;

							pg.ped = ped_gain[ns][det][dep][ch].ped ;
							pg.gain = ped_gain[ns][det][dep][ch].gain ;

							geo.ch = ch ;

							for(tick=0;tick<8;tick++) {
								stage_0(adc[ns][det][dep][ch][tick],tick,geo,&pg,&adc_prime[ch]) ;
							}
						}

						// now run the per-board stage 1
						if(det==0 && dep<20) {
							stage_1(adc_prime,geo,&link_ecal[dep]) ;	
						}
						else if(det==1 && dep<6) {
							stage_1(adc_prime,geo,&link_hcal[dep]) ;
						}
						else if(det==2 && dep<4) {
							stage_1(adc_prime,geo,&link_pre[dep]) ;
						}
					}

				}

				// Stage 2 executes here
				stage_2(link_ecal, link_hcal, link_pre, geo, &link_out_2[ns]) ;			

			}

			// Final stage
			stage_3(link_out_2, &dsm_out) ;

			// output to Trigger
			printf("Event %d: decision 0x%03X\n",event,dsm_out) ;

			memset(adc,0,sizeof(adc)) ;
			
			continue ;

		}

		adc[ns][det][dep][ch][tick] = radc ;	// store locally

	}

	printf("********* done with %d events\n",event) ;

	return 0 ;
}


