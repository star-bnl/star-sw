
#include <string.h>
#include <stdio.h>

#include <rtsLog.h>
#include <daqModes.h>

#include <DAQ_TPX/tpxCore.h>
#include <DAQ_TPX/tpxStat.h>

tpxStat::tpx_stat_struct tpxStat::r[6] ;


void tpxStat::run_start(u_int rb_mask)
{

	memset(r,0,sizeof(r)) ;

	for(int i=0;i<6;i++) {
		if(rb_mask & (1<<i)) {
			int a, c ;


			r[i].should = 1 ;

			// set the max ADC
			// figure out which altros need to be present
			for(a=0;a<256;a++) {
			for(c=0;c<16;c++) {
				int row, pad ;

				tpx_from_altro(i,a,c,row,pad) ;

				if(row <= 45) {	// expect him!
					//LOG(TERR,"RDO %d: expect altro %d:%d, row %d",i+1,a,c,row) ;
					r[i].a[a].should = 1 ;
				}
				r[i].a[a].c[c].min_adc = 0xFFFF ;
			}
			}
		}
	}

	return ;
}

int tpxStat::run_stop(FILE *ofile, u_int rb_mask, int run_type)
{

	int should ;
	int err = 0 ;

	if(ofile==0) return 0 ;

for(int i=0;i<6;i++) {
	int a, c ;

	if(rb_mask & (1<<i)) should = 1 ;
	else should = 0 ;


	if(r[i].should != should) {
		err++ ;
		fprintf(ofile,"%sERROR: [RDO %d] in_RC %d, in_data %d -- MISMATCH\n%s",ANSI_RED,i+1,should,r[i].should,ANSI_RESET) ;
		LOG(ERR,"[RDO %d] in_RC %d, in_data %d -- MISMATCH",i+1,should,r[i].should) ;
	}


	if(r[i].count==0) {
		if(r[i].should) {
			err++ ;
			fprintf(ofile,"%sERROR: [RDO %d] expect data, found none!\n%s",ANSI_RED,i+1,ANSI_RESET) ;
			LOG(ERR,"[RDO %d] expect data, found none!",i+1) ;
		}
		continue ;
	}
	else {
		if(!r[i].should) {
			err++ ;
			fprintf(ofile,"%sERROR: [RDO %d] expect none, found events %u\n%s",ANSI_RED,i+1,r[i].count,ANSI_RESET) ;
			LOG(ERR,"[RDO %d] expect none, found events %u",i+1,r[i].count) ;
		}
		else {
//			fprintf(ofile,"----- RDO %d: found events %u\n",i+1,r[i].count) ;
		}
	}


	for(a=0;a<256;a++) {
		int a_err = 0 ;
		for(c=0;c<16;c++) {
			u_int have, expect ;
			
			int warn = 0 ;

			have = r[i].a[a].c[c].count ;
			if(r[i].a[a].should) {
				expect = r[i].count ;
			}
			else {
				expect = 0 ;
			}

			//if(have || expect) {
			//	LOG(TERR,"%d %d %d %d",i,a,c,have) ;
			//}
			
			if(have != expect) {
				warn = 1 ;
				a_err = 1 ;
				fprintf(ofile,"\t%sERROR: ",ANSI_RED) ;
				LOG(ERR,"AID %3d:%2d: expect %d counts, have %d",a,c,expect,have) ;
			}
			else {	
				if(have) {
					if(run_type == RUN_TYPE_PED) {
						if(r[i].a[a].c[c].min_adc < 10) warn = 1 ;
						if(r[i].a[a].c[c].max_adc > 200) warn = 1 ;
					}
					if(warn) fprintf(ofile,"\t%sWARN: ",ANSI_CYAN) ;
				}
			}

			

			if(warn) {
				err++ ;
				fprintf(ofile,"AID %3d:%2d: expect %d counts, have %d; min %4u, max %4u\n%s",a,c,expect,have,
					r[i].a[a].c[c].min_adc,
					r[i].a[a].c[c].max_adc,ANSI_RESET) ;
			}

		}
		if(r[i].a[a].should && (a_err==0)) {
#ifdef TEST_RDO
			fprintf(ofile,"\tAID %3d OK...\n",a) ;
#endif
		}
	}
	
}

	fflush(ofile) ;

	return err ;
}

void tpxStat::accum(char *rdobuff, int bytes)
{
	int t ;
	u_int *data_end ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;


	t = tpx_get_start(rdobuff, bytes/4, &rdo, 0) ;

	if(t <= 0) return ;	// non data event...



	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.t = t ;

	r[a.rdo].count++ ;

	data_end = rdo.data_end ;

	do {
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;


		r[a.rdo].a[a.id].c[a.ch].count++ ;


		for(int i=0;i<a.count;i++) {
			if(a.adc[i] > r[a.rdo].a[a.id].c[a.ch].max_adc) {
				r[a.rdo].a[a.id].c[a.ch].max_adc = a.adc[i] ;
			}
			if(a.adc[i] < r[a.rdo].a[a.id].c[a.ch].min_adc) {
				r[a.rdo].a[a.id].c[a.ch].min_adc = a.adc[i] ;
			}

		}

		if(data_end == rdo.data_start) {
			LOG(WARN,"Data end == data start...") ;
		}

	} while(data_end && (data_end > rdo.data_start)) ;


	return ;




}
