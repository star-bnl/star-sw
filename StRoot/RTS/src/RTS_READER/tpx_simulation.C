#include <sys/types.h>
#include <stdio.h>

#include <rtsLog.h>


#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>
#include <RTS_READER/daq_det.h>


int main(int argc, char *argv[])
{
	daq_dta *dta ;		// will use it all over the place...

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;

	rts_reader r("R") ;	// gimme tha class; call it "R"

	r.enable("tpx") ;	// I just want TPX


	/******** gains can be loaded from a database ************/
	dta = r.det("tpx")->put("gain") ;	// need the class

	for(int sec=1;sec<=24;sec++) {	
	for(int row=1;row<=45;row++) {
		daq_det_gain *gain = (daq_det_gain *) dta->request(183) ;	// request space for 183 pads...

		// kill pad 0 ;
		gain[0].gain = 0.0 ;

		for(int pad=1;pad<=182;pad++) {
			gain[pad].gain = 1.0 ;	// or whatever you get from some database
			gain[pad].t0 = 0.0 ;	// or whatever you get from some database
		}

		dta->finalize(183,sec,row) ;
	}
	}

	// MUST call InitRun(xxx) _AFTER_ gains are loaded...
	r.InitRun(123) ;
	
	for(int ev=0;ev<5;ev++) {	// simulate 5 events...
		// get the ADC data somehow and feed it into TPX

		dta = r.det("tpx")->put("adc_sim") ;	// start event...

		// feed the data, pad by pad
		
		int sec = ev+1 ;	// invent sector
		int row = ev + 2 ;	// invent row

		for(int pad=5;pad<10;pad++) {
			daq_sim_adc_tb *d = (daq_sim_adc_tb *) dta->request(512) ;	// max 512 timebins
		
			int pix_count = 12 ;	// invented
			for(int k=0;k<pix_count;k++) {	
				d[k].adc = 10+k ;	// invented...
				d[k].tb = 100+k ;	// invented...
				d[k].track_id = sec+row ;	// invented
			}

			dta->finalize(pix_count,sec,row,pad) ;
		}

		// you might wish to verify that what you inserted is correct
		// but use "adc_sim" instead of "adc" in the "get"


		// run the cluster finder and get the clusters out: same as clusters from the data
		// but bank is "cld_sim"

		dta = r.det("tpx")->get("cld_sim") ;

		while(dta->iterate()) {
			printf("sec %2d, row %3d: %d clusters\n",dta->sec,dta->row,dta->ncontent) ;

			for(u_int i=0;i<dta->ncontent;i++) {
				printf("pad %f [%d:%d], tb %f [%d:%d], charge %d, flags 0x%X: track %d, Q %d\n",
				       dta->sim_cld[i].cld.pad,
				       dta->sim_cld[i].cld.p1,
				       dta->sim_cld[i].cld.p2,
				       dta->sim_cld[i].cld.tb,
				       dta->sim_cld[i].cld.t1,
				       dta->sim_cld[i].cld.t2,
				       dta->sim_cld[i].cld.charge,
				       dta->sim_cld[i].cld.flags,
				       dta->sim_cld[i].track_id,
				       dta->sim_cld[i].quality) ;
				       

			}
		}
	}

	return 0 ;
}
