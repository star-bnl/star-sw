#include <stdio.h>
#include <sys/types.h>

#include <rtsLog.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

//#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcFCF.h>

u_short slice_dta[512][121][41] ;

itpc_fcf_c fcf_helper ;

int main(int argc, char *argv[])
{
	rtsLogOutput(RTS_LOG_STDERR) ;
	daqReader *rdr ;
	int evts = 0 ;

	rdr = new daqReader(argv[1]) ;

	((daq_itpc *)(rdr->det("itpc")))->setup_fcf(1,0) ;	// ITPC reverse


	fcf_helper.det_type = 1 ;
	fcf_helper.y_is_timebin = 0 ;

	int itpc_evts = 0 ;
	for(;;) {
		int s ;
		int got_data = 0 ;
		daq_dta *cld ;

		rdr->get(0,EVP_TYPE_ANY) ;

		if(rdr->status != 0) break ;

		s = 20 ;	// just fix the sector to 20

		daq_dta *dd = rdr->det("itpc")->get("adc",20) ;

		daq_dta *sim_dta = rdr->det("itpc")->put("adc_sim") ;


		evts++ ;
		//printf("***** Event %d\n",evts) ;
		//fflush(stdout) ;
		
//		if(evts < 15) continue ;

		memset(slice_dta,0,sizeof(slice_dta)) ;
		while(dd && dd->iterate()) {
			int row = dd->row ;
			int pad = dd->pad ;
			
			if(row==0) continue ;

			got_data = 1 ;
			int y = row ;
			int x = pad ;

			int real_x = fcf_helper.pad_to_x(x,y) ;
		
			//printf("RP %d:%d -> %d\n",y,x,real_x) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				int tb = dd->adc[i].tb ;
				int adc = dd->adc[i].adc ;

				int slice = tb ;

				slice_dta[slice][real_x][y] = adc ;

				//printf("row %d, pad %d, tb %d, ADC %d\n",row,pad,tb,adc) ;
			}
		}

		if(got_data==0) continue ;

		// now smooth it
		for(int slice=0;slice<500;slice++) {

			for(int x=0;x<=120;x++) {
			for(int y=0;y<=40;y++) {
			for(int i=0;i<5;i++) {
				slice_dta[slice][x][y] += slice_dta[slice+i][x][y] ;
			}
			}
			}

			for(int x=0;x<=120;x++) {
			for(int y=0;y<=40;y++) {
				slice_dta[slice][x][y] = (u_short)((double)slice_dta[slice][x][y]/5.0+0.5) ;

				u_short adc = slice_dta[slice][x][y] ;

				if(adc) {
					//printf("tb %3d = pad %d, row %d = %d\n",slice,x,y,adc) ;
				}

			}
			}
		}

		u_int pix_count = 0 ;
		for (int slice=0;slice<500;slice++) {

			for(int x=1;x<=120;x++) {
				
				daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(41) ;	// this is the Y request
				
				int cou = 0 ;
				for(int y=1;y<=40;y++) {
					u_short adc = slice_dta[slice][x][y] ;

					if(adc) {
						sim_d[cou].adc = adc ;
						sim_d[cou].tb = y ;
						sim_d[cou].track_id = 0xFFFF ;
						cou++ ;
					}
				}
				
				sim_dta->finalize(cou,s,slice,x) ;
				pix_count += cou ;

			}
		}



		//printf("*** Running simulation: %d pixels\n",pix_count) ;
		//fflush(stdout) ;

		cld = rdr->det("itpc")->get("cld_sim",s) ;

		while(cld && cld->iterate()) {

			daq_sim_cld_x *dc = (daq_sim_cld_x *)cld->Void ;

			for(u_int i=0;i<cld->ncontent;i++) {
				printf("%d %d %f %f %d %d\n",itpc_evts,cld->row,dc->cld.pad,dc->cld.tb,dc->cld.charge,dc->cld.flags) ;
				//printf("RERUN: %d: sec %2d, row %2d: %f %f %d 0x%X - track %d, Q %d, pixels %d, max adc %d\n",i,cld->sec,cld->row,
				//       dc->cld.pad,dc->cld.tb,
				//       dc->cld.charge,dc->cld.flags,
				//       dc->track_id,dc->quality,
				//       dc->pixels,dc->max_adc) ;
				dc++ ;
			}
			fflush(stdout) ;
		}

		itpc_evts++ ;

		//if(itpc_evts==100) break ;


	}


	// only to print out statistics; not necessary
	((daq_itpc *)(rdr->det("itpc")))->run_stop() ;



	return 0 ;
}
