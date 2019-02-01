#include <stdio.h>
#include <sys/types.h>


#include <DAQ_READER/daq_dta.h>
#include <DAQ_ITPC/itpcFCF.h>



int main(int argc, char *argv[])
{
	
	int dummy_sector = 1 ;
	int dummy_row = 1 ;
	char buff[1024*1024] ;	// working buffer for FCF

	// input simulated data arrays
	u_short sim_adc[256][512];	
	u_short track_id[256][512] ;	// same as above...

	itpc_fcf_c *fcf = new itpc_fcf_c ;	// make one
	
	fcf->max_x = 256 ;			// aka "pad", from 1..256
	fcf->max_y = 512 ;			// aka "timebin", from 1..512
	fcf->det_type = 2 ;			// 2==OTHER 

	fcf->init(dummy_sector,0) ;			// 1=dummy sector number


	//prepare events
	for(int e=1;e<=100;e++) {		// e.g. 100 events

		printf("Starting event %d\n",e) ;


		// one needs to zap input arrays
		memset(sim_adc,0,sizeof(sim_adc)) ;
		memset(track_id,0,sizeof(track_id)) ;

		// create some dummy data for this example
		for(int x=20;x<25;x++) {
			for(int y=100;y<120;y++) {
				sim_adc[x][y] = 12+e ;		// some ADC value
				track_id[x][y] = e*100 ;		// some track id
			}
		}

		// start an event or plane in this case
		fcf->event_start() ;

		// run the data preparation stage over all "x"; note that _numbering_ starts from 1
		for(int x=0;x<256;x++) {
			fcf->do_ch_sim(dummy_row,x+1,sim_adc[x],track_id[x]) ;
		}

		
		// run clusterfinder and output to "buff"
		int words = fcf->do_fcf(buff,sizeof(buff)) ;


		// extract results from buff

		u_int *end_buff = (u_int *)(buff+words*4) ;
		u_int *p_buff = (u_int *) buff ;

		while(p_buff < end_buff) {
			u_int row = *p_buff++ ;
			u_int version = *p_buff++ ;
			u_int int_cou = *p_buff++ ;

			int ints_per_cluster = row >> 16 ;
			row &= 0xFFF ;

			int clusters = int_cou/ints_per_cluster ;

			for(int i=0;i<clusters;i++) {
				daq_sim_cld_x dc ;

				fcf->fcf_decode(p_buff,&dc,version) ;
				p_buff += ints_per_cluster ;

				// print a sample of available data
				// "x", "y", charge, flags, track_id, quality

				printf("... %d: %f %f %d 0x%X %d %d\n",i+1,
				       dc.cld.pad,dc.cld.tb,dc.cld.charge,dc.cld.flags,
				       dc.track_id,dc.quality) ;

			}



		}

		printf("Ending event %d\n",e) ;
	}


	delete fcf ;

	return 0 ;
}
