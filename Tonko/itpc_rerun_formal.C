#include <stdio.h>
#include <sys/types.h>

#include <rtsLog.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include <DAQ_ITPC/daq_itpc.h>

#define FROM_FILE 1

int main(int argc, char *argv[])
{
	rtsLogOutput(RTS_LOG_STDERR) ;
	daqReader *rdr ;
	int evts = 0 ;

	rdr = new daqReader(argv[1]) ;


	// example of gains; will use file for that
	FILE *f = fopen("/RTS/conf/itpc/itpc_gains.txt","r") ;
	if(f==0) {
		LOG(ERR,"Can't open gain file") ;
		return -1 ;
	}


	static struct g_s_t {
		float g ;
		float t ;
	} g_s[25][41][121] ;

	int bad_ch = 0 ;
	int all_ch = 0 ;
	while(!feof(f)) {
		char buff[128] ;
		int sec,rdo,port,ch,row,pad ;
		float g, t ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(strlen(buff)<1) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %f %f",&sec,&rdo,&port,&ch,&row,&pad,&g,&t) ;
		if(ret != 8) continue ;

		if(g<0.01) bad_ch++ ;
		all_ch++ ;

		g_s[sec][row][pad].g = g ;
		g_s[sec][row][pad].t = t ;
	}

	LOG(TERR,"From gain file: %d/%d bad channels",bad_ch,all_ch) ;
	fclose(f) ;

#if 1
	// and now load them up
	daq_dta *gain_dta = rdr->det("itpc")->put("gain") ;

	for(int s=1;s<=24;s++) {	// sectors loop
		for(int r=1;r<=40;r++) {	// rows loop
			daq_det_gain *gain = (daq_det_gain *) gain_dta->request(121) ;
			for(int p=0;p<=120;p++) {	// pad loop
				gain[p].gain = g_s[s][r][p].g ;
				gain[p].t0 = g_s[s][r][p].t ;
			}
			gain_dta->finalize(121,s,r) ;
		}
	}
#endif


#ifdef FROM_FILE
	int itpc_evts = 0 ;
	for(;;) {
	daq_dta *cld ;

	rdr->get(0,EVP_TYPE_ANY) ;

	if(rdr->status != 0) break ;

	evts++ ;

	//if(evts != 26) continue ;

	for(int s=1;s<=24;s++) {
		int got_data = 0 ;

		daq_dta *dd = rdr->det("itpc")->get("adc",s) ;

		daq_dta *sim_dta = rdr->det("itpc")->put("adc_sim") ;


		//int row_sum[41] ;
		//memset(row_sum,0,sizeof(row_sum)) ;

		while(dd && dd->iterate()) {
			int row = dd->row ;
			int pad = dd->pad ;

			got_data = 1 ;

			daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(512) ;

			for(u_int i=0;i<dd->ncontent;i++) {
				sim_d[i].adc = dd->adc[i].adc ;
				sim_d[i].tb = dd->adc[i].tb ;
				sim_d[i].track_id = 0xFFFF ;

				//row_sum[row] += (int)(sim_d[i].adc * g_s[s][row][pad].g) ;
				//if(evts == 26) printf("%d %d %d %d\n",row,pad,sim_d[i].tb,sim_d[i].adc) ;

				//LOG(TERR,"%d %d %d : %d %d",s,row,pad,sim_d[i].adc,sim_d[i].tb) ;
			}

			sim_dta->finalize(dd->ncontent,s,row,pad) ;
		}

		//for(int r=1;r<=40;r++) {
		//	if(row_sum[r]) printf("row %d, sum %d\n",r,row_sum[r]) ;
		//}


		int c_cou = 0 ;
		if(got_data) {

			printf("*** Running simulation: evt %d, sector %d\n",evts,s) ;

			cld = rdr->det("itpc")->get("cld_sim",s) ;

			while(cld && cld->iterate()) {

				daq_sim_cld_x *dc = (daq_sim_cld_x *)cld->Void ;

				for(u_int i=0;i<cld->ncontent;i++) {
					printf("RERUN: %d: sec %2d, row %2d: %f %f %d 0x%X - track %d, Q %d, pixels %d, max adc %d\n",c_cou,cld->sec,cld->row,
					       dc->cld.pad,dc->cld.tb,
					       dc->cld.charge,dc->cld.flags,
					       dc->track_id,dc->quality,
					       dc->pixels,dc->max_adc) ;
					dc++ ;
					c_cou++ ;
				}
			}
			fflush(stdout) ;
		}

		printf("*** Data from file: evt %d, sector %d\n",evts,s) ;

		cld = rdr->det("itpc")->get("cld",s) ;

		c_cou = 0 ;
		while(cld && cld->iterate()) {

			daq_cld *dc = (daq_cld *)cld->Void ;

			for(u_int i=0;i<cld->ncontent;i++) {
				printf("FILE: %d: sec %2d, row %2d: %f %f %d 0x%X\n",c_cou,cld->sec,cld->row,
				       dc->pad,dc->tb,
				       dc->charge,dc->flags) ;
				dc++ ;
				c_cou++ ;
			}

		}

		fflush(stdout) ;

	}

	itpc_evts++ ;
		

	}


	// only to print out statistics; not necessary
	((daq_itpc *)(rdr->det("itpc")))->run_stop() ;

	delete rdr ;	// be explicit so valgrind can capture this

#else
	daq_itpc *itpc = (daq_itpc *) new daq_itpc ;

	// Event loop
	for(evts=0;evts<10;evts++) {

		for(int s=1;s<=24;s++) {

			// fill the data
			//daq_dta *sim_dta = rdr->det("itpc")->put("adc_sim") ;
			daq_dta *sim_dta = itpc->put("adc_sim") ;

			for(int r=1;r<=40;r++) {
				
				for(int p=10;p<=12;p++) {

					daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(512) ;

					int cou = 0 ;
					for(int i=0;i<10;i++) {
						sim_d[i].adc = 123 ;
						sim_d[i].tb = 100+i ;
						sim_d[i].track_id = r ;

						cou++ ;
					}

					sim_dta->finalize(cou,s,r,p) ;
				}
			}

			
			//and now run it
			//daq_dta *cld = rdr->det("itpc")->get("cld_sim",s) ;
			daq_dta *cld = itpc->get("cld_sim",s) ;

			while(cld && cld->iterate()) {

				daq_sim_cld_x *dc = (daq_sim_cld_x *)cld->Void ;

				for(u_int i=0;i<cld->ncontent;i++) {

					printf("RERUN: %d: sec %2d, row %2d: %f %f %d 0x%X - track %d, Q %d, pixels %d, max adc %d\n",i,cld->sec,cld->row,
					       dc->cld.pad,dc->cld.tb,
					       dc->cld.charge,dc->cld.flags,
					       dc->track_id,dc->quality,
					       dc->pixels,dc->max_adc) ;


					dc++ ;
				}
			}
		}
	}
#endif

	return 0 ;
}
