#include <sys/types.h>
#include <stdio.h>
#include <assert.h>

#include <rtsLog.h>


#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>

#include <DAQ_TPX/daq_tpx.h>

int main(int argc, char *argv[])
{
	daq_dta *dta ;	// used for any kind of data; transparent pointer

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;



	rts_reader r("r_sim") ;
	r.enable("*") ;


#if 0
	// do gains example; one loads them from database but I don't know how...
	dta = r.det("tpx")->put("gain") ;

	for(int sec=1;sec<=24;sec++) {
	for(int row=0;row<=45;row++) {
		daq_det_gain *gain = (daq_det_gain *) dta->request(183) ;	// max pad+1		
		
		assert(gain) ;

		LOG(DBG,"sec %d, row %d",sec,row) ;
		gain[0].gain = 0.0 ;	// kill pad0 just in case..

		for(int pad=1;pad<=182;pad++) {
			gain[pad].gain = (row==0)? 0.0 : 1.0 ;			// kill row 0; these are unconnected pads
			gain[pad].t0 = 0.0 ;
		}
		LOG(DBG,"finilize sec %d, row %d",sec,row) ;

		dta->finalize(183,sec,row) ;
	}
	}
#endif

	/*
	InitRun will setup the internal representations of gain 
	and other necessary structures but if the gains have not
	been previously loaded as shown in the example above they
	will be set to 1.0!
	*/

	r.InitRun(123) ;


	for(int ev=0;ev<10;ev++) {
	// loop over sectors
	for(int sec=1;sec<=24;sec++) {
	for(int row=1;row<=45;row++) {

		// create (or reuse) the adc_sim bank...
		dta = r.det("tpx")->put("adc_sim") ;


		// add a bunch of adc data for a specific sector:row
		for(int cou=0;cou<1;cou++) {
		for(int pad=86;pad<=88;pad++) {	// invent these pads...
			int tb_cou ;

			// allocate space for at least 512 pixels (timebins)
			daq_sim_adc_tb *d = (daq_sim_adc_tb *) dta->request(512) ;

			tb_cou = 0 ;

			// add adc data for this specific sector:row:pad
			for(u_int k=0;k<5;k++) {
				d[k].adc = 10+k ;
				d[k].tb = 100+k ;
				d[k].track_id = cou+100 ;
				tb_cou++ ;
			}

			// we used 12 pixels...
			dta->finalize(tb_cou,sec,row,pad) ;
		}
		}
		
#define SIM_VERIFY	
#ifdef SIM_VERIFY
		// verify data!
		dta = r.det("tpx")->get("adc_sim") ;

		while(dta->iterate()) {
			printf("*** sec %2d, row %2d, pad %3d: %3d pixels\n",dta->sec,dta->row,dta->pad,dta->ncontent) ;

			for(u_int i=0;i<dta->ncontent;i++) {
				printf("    %2d: adc %4d, tb %3d: track %4d\n",i,
				       dta->sim_adc[i].adc,
				       dta->sim_adc[i].tb,
				       dta->sim_adc[i].track_id
				       );
			}
		}
#endif

		// rerun the cluster finder on the simulated data...
		dta = r.det("tpx")->get("cld_sim") ;

		while(dta->iterate()) {
			if((dta->row != row) || (dta->sec != sec)) {
				fprintf(stderr,"Big error!\n") ;
			}
			printf("CLD sec %2d: row %2d: %d clusters\n",dta->sec, dta->row, dta->ncontent) ;

			for(u_int i=0;i<dta->ncontent;i++) {
			       printf("    pad %f[%d:%d], tb %f[%d:%d], cha %d, fla 0x%X: track %d, Q %d\n",
				      dta->sim_cld[i].cld.pad,
				      dta->sim_cld[i].cld.p1,
				      dta->sim_cld[i].cld.p2,
				      dta->sim_cld[i].cld.tb,
				      dta->sim_cld[i].cld.t1,
				      dta->sim_cld[i].cld.t2,
				      dta->sim_cld[i].cld.charge,
				      dta->sim_cld[i].cld.flags,
				      dta->sim_cld[i].track_id,
				      dta->sim_cld[i].quality
				      );
			}

		}

		
	}
	}
	}

	return 0 ;
}

	
