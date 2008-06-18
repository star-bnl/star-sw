#include <sys/types.h>
#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <rtsLog.h>


#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>
#include <RTS_READER/daq_det.h>




int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;
	int c ;


	char do_print ;
	char do_check ;
	char do_usage ;
	char do_summary ;
	char do_help ;
	int do_sector = -1 ;

	u_int num_evts = 0xFFFFFFFF ;	// a lot...
	char *c_do_dets = "*" ;	// defaults to all I know about!

	rtsLogOutput(RTS_LOG_STDERR) ;
	rtsLogLevel(WARN) ;

	do_help = do_print = do_check = do_usage = do_summary = 0 ;


	daq_dta *dta ;	// daq_dta class grabs the data out of a DET bank...
	rts_reader r("R") ;	// call myself "R"; used for debugging prints...

	while((c = getopt(argc, argv,"S:d:pcsC:n:h")) != EOF) {
		switch(c) {
		case 'h' :
			do_help = 1 ;
			break ;
		case 'd' :
			rtsLogLevel(optarg) ;
			break ;
		case 'p' :
			do_print = 1 ;
			break ;
		case 'c' :
			do_check = 1 ;
			break ;
		case 's' :
			do_summary = 1 ;
			break ;
		case 'C' :
			c_do_dets = optarg ;
			break ;
		case 'n' :
			num_evts = atoi(optarg) ;
			break ;
		case 'S' :
			do_sector = atoi(optarg) ;
			break ;
		default :
			do_usage = 1 ;
			break ;
		}
	}


	if(do_usage) {
		fprintf(stderr,"Usage: %s [-d loglevel] [-p (to print on stdout)] [-c (to do checking)] files...\n",argv[0]) ;
		return -1 ;
	}


	r.enable(c_do_dets) ;			// enable the selected det or dets

	// this will print something useful regarding usage 
	if(do_help) {
		daq_dta d ;



		printf("rts_reader: %s\n",r.GetCVS()) ;
		printf("daq_det: %s\n",r.det("*")->GetCVS()) ;
		printf("daq_dta: %s\n",d.GetCVS()) ;
		r.det(c_do_dets)->help() ;

		return 0 ;
	}


	// we need a filename here:
	if(optind >= argc) {
		fprintf(stderr,"Usage: %s [-d loglevel] [-p (to print on stdout)] [-c (to do checking)] files...\n",argv[0]) ;
		return -1 ;
	}

	// pick up the filenames
	while(optind < argc) {
		r.add_input(argv[optind]) ;	// add all files to the list...
		optind++ ;
	}

	r.InitRun(123) ;	// some dummy for now


	/*
		rts_reader Make() will:
		
		- move to the next event in the file and...
		- call the det.Make() for all the enabled dets above
	*/
	while(r.Make() != EOF) {

		// check to see if the event has some error
		if(r.l_errno) {
			LOG(ERR,"At file \"%s\", event %d: error [%s]",r.select_files[r.cur_file_ix],r.cur_event_in_file,strerror(r.l_errno)) ;
			break ;
		}
		else {
			LOG(DBG,"Got event %d",r.cur_event) ;
		}

		
		/********************************************************/
		/*************** TPX ************************************/
		/*******************************************************/

		/******** ADC in file ****************************/
		dta  = r.det("tpx")->get("adc") ;	// grab the "adc" banks


		while(dta && dta->iterate()) {
			if(do_print) {
				// this is how one knows the coordinates
				printf("sec %2d, row %2d, pad %3d: %d items\n",dta->sec,dta->row,dta->pad,dta->ncontent) ;
			}
				// and now get the content
			for(u_int i=0;i<dta->ncontent;i++) {
				if(do_print) {
					printf("\t%4d:\t tb %3d = %3d adc\n",i,
					       dta->adc[i].tb,
					       dta->adc[i].adc) ;

				}
			}


		}


		/********* CLD in file *********/
		dta  = r.det("tpx")->get("cld") ;	// grab the cluster banks

		while(dta && dta->iterate()) {
			// and now get the content
			for(u_int i=0;i<dta->ncontent;i++) {
				if(1) {
					printf("%d %d %f %d %d %f %d %d %d 0x%X\n",r.cur_event,dta->row,
					       dta->cld[i].pad,dta->cld[i].p1,dta->cld[i].p2,
					       dta->cld[i].tb,dta->cld[i].t1,dta->cld[i].t2,
					       dta->cld[i].charge,dta->cld[i].flags) ;

				}
			}


		}


		/***********************************************************/
		/************************* TOF *****************************/

		/******* raw bank ***********/
		dta = r.det("tof")->get("raw") ;

		while(dta && dta->iterate()) {
			if(do_print) {
				printf("sector %d: rdo %d: %d bytes\n",dta->sec,dta->rdo,dta->ncontent) ;
			}

			for(u_int i=0;i<dta->ncontent/4;i++) {
				if(do_print) printf("%d: 0x%08X [%u dec]\n",i,dta->Int32[i],dta->Int32[i]) ;
			}
		}


		/***********************************************************/
		/************************ PP2PP ****************************/

		/****** raw ********************/
		dta = r.det("pp2pp")->get("raw") ;

		while(dta && dta->iterate()) {
			if(do_print) {
				printf("sector %d: rdo %d: %d bytes\n",dta->sec,dta->rdo,dta->ncontent) ;
			}

			for(u_int i=0;i<dta->ncontent/4;i++) {
				if(do_print) printf("%d: 0x%08X [%u dec]\n",i,dta->Int32[i],dta->Int32[i]) ;
			}
		}

		/************************************************************/
		/*********************** ESMD *******************************/

		/****** raw example ************/
		dta = r.det("esmd")->get("raw") ;
		while(dta && dta->iterate()) {
			for(u_int i=0;i<dta->ncontent;i++) {
				if(do_print) printf("esmd raw %4d: 0x%04X\n",i,dta->Short[i]) ;
			}
		}
		
		/****** ADC example ************/
		dta = r.det("esmd")->get("adc") ;
		if(dta && dta->iterate()) {
			// ESMD has a fixed size array so there is no need to loop over ncontent
			for(int i=0;i<48;i++) {
				for(int j=0;j<192;j++) {
					if(do_print) printf("esmd adc: %2d:%3d %4d\n",i,j,dta->esmd[i][j]) ;
				}
			}

		}

		/****** ESMD headers/preambles *******/
		dta = r.det("esmd")->get("preamble") ;
		if(dta && dta->iterate()) {
			for(int i=0;i<48;i++) {
				for(int j=0;j<4;j++) {
					if(do_print) printf("esmd preamble: %2d:%3d %4d\n",i,j,dta->esmd_pre[i][j]) ;
				}
			}

		}


		/************************************************************/
		/*********************** TPC *******************************/

		/****** adc example ************/
		dta = r.det("tpc")->get("adc",do_sector) ;
		while(dta && dta->iterate()) {
			if(do_print) printf("tpc adc: sector %d, row %d, pad %d: %d\n",dta->sec,dta->row,dta->pad,dta->ncontent) ;
		}

		
		/******* CLD example *****************/
		dta = r.det("tpc")->get("cld",do_sector) ;
		while(dta && dta->iterate()) {
			if(do_print) printf("tpc cld: sector %d, row %d: %d\n",dta->sec,dta->row,dta->ncontent) ;
		}


		/*************************************************************/
		/************************* End of Event **********************/

		// quit after a count of events?
		if(r.cur_event >= num_evts) break ;
	}

	r.FinishRun(123) ;

	// example how to get some per-run quantity out after
	// the run has finished i.e. the pedestals or gains...

	//dta = r.det("tpx")->get("gains_c") ;
	//dta = r.det("tpx")->get("peds_c") ;

	return 0 ;
}
