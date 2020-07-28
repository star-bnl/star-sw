#include <stdio.h>
#include <string.h>	// for memset


#include "fcs_trg.h"


main()
{
	
	// At the start we first need to grab gain corrections and pedestals from some file or database
	// but in this example I will generate dummies

	for(int ns=0;ns<2;ns++) {
	for(int det=0;det<3;det++) {
	for(int dep=0;dep<20;dep++) {
	for(int ch=0;ch<32;ch++) {

		fcs_trg_pt_correction[ns][det][dep][ch] = 3.0 ;		// don't know, some number, get it from Akio
		fcs_trg_gain_correction[ns][det][dep][ch] = 1.0 ;	// knowning nothing better...
		fcs_trg_pedestal[ns][det][dep][ch] = 0 ;		// to simplify simulation matters...

	}}}}


	// Loop over events

	for(int evt=1;evt<=100;evt++) {

		// and now I need to get the ADC data into the global...
		// just zap all for this example
		memset(fcs_trg_sim_adc,0,sizeof(fcs_trg_sim_adc)) ;	

		// ... but put something in just 1 channel for this simple example
		fcs_trg_sim_adc[0][0][0][0] = evt*10 ;	// make it change a bit...


		// and now run the event simulation
		u_short dsm_out = fcs_trg_run() ;
		
		// print the final DSM bits
		printf("event %d: DSM 0x%03X\n",evt,dsm_out) ;
	}

	return 0 ;
}
