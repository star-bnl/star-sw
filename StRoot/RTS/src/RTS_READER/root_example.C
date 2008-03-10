
#ifndef __CINT__

#include <rtsLog.h>
#include <stdio.h>

#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>
#include <RTS_READER/daq_det.h>


int root_example() ;

int main()
{
	return root_example() ;
} ;

#endif

int root_example()
{
#ifdef __CINT__
	gSystem->Load("St_base.so") ;
	gSystem->Load("StChain.so") ;

	gSystem->Load("SFS.so") ;
	gSystem->Load("RTS_READER.so") ;
	gSystem->Load("DAQ_TPX.so") ;
#else
	rtsLogOutput(RTS_LOG_STDERR) ;
#endif

	rts_reader c ;




	c.enable("tpx") ;	// tpx is currently the only one enabled...
	
	c.add_input("/net/daqman/data/tonko/TPX16/tpx_laser8338018_dbg.sfs") ;
	// you can repeat the add_input to add up to 1024 files...



	while(c.Make() >= 0) {	// loop through all events of all added file...
		daq_dta *d ;

		printf("******* event %d ***********\n",c.cur_event) ;
		
		d = c.det("tpx")->get("adc",16) ;

		while(d->iterate()) {
			printf("sector %2d, row %2d, pad %3d: %d objects\n",d->sec,d->row,d->pad,d->ncontent) ;

			for(unsigned int i=0;i<d->ncontent;i++) {
				//printf("\ttb %3d = %3d\n",d->adc[i].tb,d->adc[i].adc) ;
			}
		}

		d = c.det("tpx")->get("cld",16) ;

		while(d->iterate()) {
			for(unsigned int i=0;i<d->ncontent;i++) {
				printf("%d %d %f %d %d %f %d %d %d %d\n",c.cur_event,d->row,
				       d->cld[i].pad,d->cld[i].p1,d->cld[i].p2,
				       d->cld[i].tb,d->cld[i].t1,d->cld[i].t2,
				       d->cld[i].charge,d->cld[i].flags) ;


			}
		}

	}

	printf("****** Done with %d events...\n",c.cur_event) ;

	return c.cur_event ;
}

