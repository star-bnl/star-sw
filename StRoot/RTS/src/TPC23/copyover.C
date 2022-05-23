#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/mman.h>
#include <getopt.h>
#include <sys/prctl.h>
#include <errno.h>
#include <math.h>

#include <rtsLog.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>

static struct stat_t {
	double mean ;
	double rms ;
	int cou ;
} stat[7][256] ;

int main(int argc, char *argv[])
{
	int events_max = 64 ;
	int sector = 1 ;
	const char *to_dir = 0 ;
	const char *fname ;
	const char *det = "tpx" ;
	int want_stat = 0 ;
	int max_tb = 0 ;

	int c ;

	rtsLogOutput(RTS_LOG_STDERR) ;

	while((c=getopt(argc,argv,"e:d:D:wS:"))!=EOF) {
	switch(c) {
	case 'e' :
		events_max = atoi(optarg) ;
		break ;
	case 'd' :
		to_dir = optarg ;
		break ;
	case 'D' :
		det = optarg;
		break ;
	case 'w' :
		want_stat = 1 ;
		break ;
	case 'S' :
		sector = atoi(optarg) ;
		break ;
	}
	}
	

	fname = argv[optind] ;


	// fill the structures first...
	daqReader *rdr = new daqReader((char *)fname) ;
	daq_dta *dd ;

	LOG(INFO,"Opened %s, using sector %s:%02d",fname,det,sector) ;



	int cou = 1 ;	

	for(;;) {

	if(rdr->get(0,EVP_TYPE_ANY)) ;
	else {
		break ;
	}

	if(cou==1) printf("%s:%02d, run %08u\n",det,sector,rdr->run) ;

	dd = rdr->det(det)->get("raw",sector) ;

	int got_it = 0 ;	

	while(dd && dd->iterate()) {
		char fname[128] ;
		
		got_it = 1;

		if(to_dir==0) {
			//LOG(WARN,"%d: event %d: sector %2d, RDO %d: %u bytes -- no -d DIR!",det,cou,sector,dd->rdo,dd->ncontent) ;
			continue ;
		}

		sprintf(fname,"%s/%s%02d_%d_%d",to_dir,det,sector,dd->rdo,cou) ;

		FILE *f = fopen(fname,"w") ;

		if(f==0) {
			LOG(ERR,"%s: %s",fname,strerror(errno)) ;
			continue ;
		}
		else {
			LOG(TERR,"%s: copying %d bytes",fname,dd->ncontent) ;
		}

		int ret = fwrite(dd->Void,dd->ncontent,1,f) ;
		if(ret != 1) {
			LOG(ERR,"... ret %d",ret) ;
		}

		fclose(f) ;		


		
	}


	if(want_stat) {
	if(strstr(det,"itpc")) {

		dd = rdr->det(det)->get("sampa",sector) ;

		while(dd && dd->iterate()) {
			int rdo = (dd->row >> 4)+1;
                        int port = (dd->row & 0xF) ;
//                        int ch = (dd->pad) & 0xFF ;
//                        int fee_id = (dd->pad >> 8) ;



			stat[rdo][port].mean += dd->ncontent ;
			stat[rdo][port].rms += dd->ncontent * dd->ncontent ;
			stat[rdo][port].cou++ ;

			for(u_int i=0;i<dd->ncontent;i++) {
				if(dd->adc[i].tb>max_tb) max_tb = dd->adc[i].tb ;
			}
		}

	}
	else {
		for(int rdo=3;rdo<=6;rdo++) {

		dd = rdr->det(det)->get("altro",sector,rdo) ;

		while(dd && dd->iterate()) {
			int altro = dd->row ;

			altro /= 2 ;	// I want FEE really

			//printf("... rdo %d, altro %d: %d\n",rdo,altro,dd->ncontent) ;

			stat[rdo][altro].mean += dd->ncontent ;
			stat[rdo][altro].rms += dd->ncontent * dd->ncontent ;
			stat[rdo][altro].cou++ ;
			
			max_tb = 420 ;

		}
		}

	}
	}


	if(got_it) {
		if(cou==events_max) break ;
		cou++ ;
	}

	}


	LOG(INFO,"After %d events, max_tb %d",cou,max_tb) ;


	for(int r=1;r<=6;r++) {
	for(int p=0;p<256;p++) {
		int cou = stat[r][p].cou ;
		if(cou==0) continue ;

		double mean = stat[r][p].mean/cou ;
		double rms = stat[r][p].rms/cou ;

		rms = sqrt(rms-mean*mean) ;

		printf("rdo %d, port %2d: %f +- %f, occupancy %.1f\n",r,p,mean,rms,100*mean/max_tb) ;
	}}

	return 0 ;
}
