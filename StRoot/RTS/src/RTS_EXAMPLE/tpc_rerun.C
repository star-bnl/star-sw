/*
	The example shows how to rerun the cluster finder
	on the TPX (or TPC) cluster data...
*/


#include <stdio.h>
#include <sys/types.h>

#include <rtsLog.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_TPX/daq_tpx.h>



int main(int argc, char *argv[])
{
	rtsLogOutput(RTS_LOG_STDERR) ;
//	rtsLogLevel(NOTE) ;

	daqReader *dr = new daqReader(argv[1]) ;	// open file


	// need this specific construct so that the cluster finder can initialize!
	daq_tpx *tpx = new daq_tpx(dr) ;	// insert the TPX detector explicitly in the DAQ_READER
	tpx->fcf_run_compatibility = 10 ;

	tpx->InitRun(123) ;			// initialize the run with some dummy run number...


	while(dr->get(0,0)) {			// zip through the input files...


	int got_adc_data = 0 ;
	daq_dta *dd, *sim_dta ;

#define DUMP_CLD_IN_FILE
#ifdef DUMP_CLD_IN_FILE
	// if you care, you can dump the in-file clusters here
	dd = dr->det("tpx")->get("cld") ;
	while(dd && dd->iterate()) {
		for(u_int i=0;i<dd->ncontent;i++) {
			switch(dd->cld[i].flags) {
			case 0 :	// normal
			case 1 :	// normal
			case 2 :	// normal
			case 3 :	// should not really use
				continue ;
			default :
				break ;
			}

			printf("in file: row %2d: pad %f [%d:%d], tb %f [%d:%d], charge %d, flags 0x%X\n",dd->row,
			       dd->cld[i].pad,
			       dd->cld[i].p1,
			       dd->cld[i].p2,
			       dd->cld[i].tb,
			       dd->cld[i].t1,
			       dd->cld[i].t2,
			       dd->cld[i].charge,
			       dd->cld[i].flags) ;

		}



	}
		
#endif

//#define DO_CLD
#ifdef DO_CLD
	dd = dr->det("tpx")->get("adc") ;	// get the ADC data
	if(dd == 0) {
		LOG(WARN,"No adc data in this event...") ;
		continue ;			// not there, skip...
	}


	sim_dta = dr->det("tpx")->put("adc_sim") ;	// create the ADC data for simulation


	// read input data and fill in the adc_sim
	while(dd->iterate()) {
		got_adc_data = 1 ;
		daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(512) ;	// ask for space
		
		// copy over
		for(u_int i=0;i<dd->ncontent;i++) {
			sim_d[i].adc = dd->adc[i].adc ;
			sim_d[i].tb = dd->adc[i].tb ;
			sim_d[i].track_id = 0xFFFF ;	// should be 0xFFFF if you don;t have tracking...
		}

		// wrap up this pad with the correct length & coordinates
		sim_dta->finalize(dd->ncontent,dd->sec,dd->row,dd->pad) ;

	}

	if(!got_adc_data) {
		LOG(WARN,"No ADC data in this event...") ;
		continue ;
	}
	LOG(NOTE,"Doing adc data...") ;

	// OK, now we have the ADC data ready for re-clusterfinding so let's do it


	dd = dr->det("tpx")->get("cld_sim") ;	// this will rerun the cluster finder on the "adc_sim" data
	if(dd == 0) continue ;	// error

	// dump the newly found data out...
	while(dd->iterate()) {
		//printf("sec %2d, row %3d: %d clusters\n",dd->sec,dd->row,dd->ncontent) ;

		for(u_int i=0;i<dd->ncontent;i++) {
#if 0
			if(dd->sim_cld[i].cld.flags != 0) continue ;
			if((dd->sim_cld[i].cld.p2 - dd->sim_cld[i].cld.p1) != 2) continue ;

			u_short tid = dd->sim_cld[i].track_id ;

			daq_dta *dta = dr->det("tpx")->get("adc_sim") ;
			while(dta && dta->iterate()) {
				daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) dta->Void ;

				for(int i=0;i<dta->ncontent;i++) {
					if(sim_d[i].track_id == tid) {
						printf("%5d %d %d %d %d\n",tid,dta->row,dta->pad,sim_d[i].tb,sim_d[i].adc) ;
					}
				}
			}
#endif

#if 1
			switch(dd->sim_cld[i].cld.flags) {
			case 0 :
			case 2 :
				continue ;
			default:
				break ;
			}

//			if(dd->sim_cld[i].cld.tb < 15.0) 
			printf("rerun: row %2d: pad %f [%d:%d], tb %f [%d:%d], charge %d, flags 0x%X: track %d, Q %d\n",dd->row,
			       dd->sim_cld[i].cld.pad,
			       dd->sim_cld[i].cld.p1,
			       dd->sim_cld[i].cld.p2,
			       dd->sim_cld[i].cld.tb,
			       dd->sim_cld[i].cld.t1,
			       dd->sim_cld[i].cld.t2,
			       dd->sim_cld[i].cld.charge,
			       dd->sim_cld[i].cld.flags,
			       dd->sim_cld[i].track_id,
			       dd->sim_cld[i].quality) ;
#endif
		}
	}

#endif

	}	// end of ebent

	return 0 ;
	}
