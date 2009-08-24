#include <stdio.h>

#include <DAQ_TPX/tpxGain.h>


int main(int argc, char *argv[])
{
	char *fname = "/RTS/conf/tpx/tpx_gains.txt" ;	// default
	int ret ;

	if(argc==2) {	// override default if provided
		fname = argv[1] ;
	}

	tpxGain tpx_gain ;	// constructor

	tpx_gain.init(0) ;	// create and zap storage for the whole TPX

	ret = tpx_gain.from_file(fname) ;	// read from file...
	if(ret < 0) {
		fprintf(stderr,"********* some failure on file %s!\n",fname) ;
		return -1 ;
	}

	printf("File opened: %s\n",fname) ;
	printf("Run used [if available]: %08u\n",tpx_gain.c_run) ;
	printf("Date changed [if available]: date %08u, time %06u\n",tpx_gain.c_date,tpx_gain.c_time) ;

	// show how the gains & t0 is obtained and used...

	for(int s=1;s<=24;s++) {			// NOTE: sector counts from 1
		for(int r=1;r<=45;r++) {		// NOTE: row counts from 1
			for(int p=1;p<=182;p++) {	// NOTE: pads count from 1
				float g, t0 ;		// gains and t0 are actually stored as "float"...

				g = tpx_gain.get_gains(s,r,p)->g ;
				t0 = tpx_gain.get_gains(s,r,p)->t0 ;

				printf("%d %d %d %.3f %.3f\n",s,r,p,g,t0) ;
			}
		}
	}

	return 0 ;
}
