#include <math.h>
#include <string.h>
#include <stdio.h>

#include <rtsLog.h>
#include <daqModes.h>

#include <TPX/tpx_altro_to_pad.h>

#include "tpxCore.h"
#include "tpxStat.h"


//#include <thread_dbg.h>
#define TLOG()


#define QA_TB_START	147
#define QA_TB_STOP	153

#define QA_CHARGE	640.0

static const int fee_check[] = {
//	100,
//	101,
//	102,
//	103,
//	104,
	107,
	108,
	109,
	110,
	111,
	114,
	115,
	116,
	117,
	118,
//	121,
//	122,
//	123,
//	124
} ;

static const int fee_check_count = sizeof(fee_check)/sizeof(fee_check[0]) ;

static FILE *stripes_f ;

void tpxStat::run_start(u_int rb_mask, int r_type)
{

	run_type = r_type ;

	memset(r,0,sizeof(r)) ;
	stripes = 0 ;	

	stripes_f = 0 ;
	if(run_type != RUN_TYPE_PED) {
//		stripes_f = fopen("/RTScache/stripes.txt","w") ;
	}

	if((run_type == RUN_TYPE_DAQCHECK) && (rb_mask==4)) {	// RDO #3
		fee_check_on = 1 ;
		memset(fee_check_altros,0,sizeof(fee_check_altros)) ;
		memset(fee_check_data,0,sizeof(fee_check_data)) ;
	}
	else fee_check_on = 0 ;

	for(int i=0;i<6;i++) {
		if(rb_mask & (1<<i)) {
			int a, c ;


			r[i].should = 1 ;

			if(fee_check_on) {
			// figure out which altros need to be present
				for(int j=0;j<fee_check_count;j++) {

					a = (fee_check[j]<<1) & 0xFF ;

					fee_check_altros[a] = 1 ;
					fee_check_altros[a|1] = 1 ;

					r[i].a[a].should  = 1;
					r[i].a[a|1].should = 1 ;

					for(c=0;c<16;c++) {
						r[i].a[a].c[c].min_adc = 0xFFFF ;
						r[i].a[a|1].c[c].min_adc = 0xFFFF ;

						fee_check_data[j*2][c].aid = a ;
						fee_check_data[j*2+1][c].aid = a|1 ;
					}

				}
			}
			else {
				int s_real, r_real ;
				tpx36_to_real(sector,i+1,s_real,r_real) ;
				r_real-- ;

				for(a=0;a<256;a++) {
				for(c=0;c<16;c++) {
					int row, pad ;

					tpx_from_altro(r_real,a,c,row,pad) ;
	
					if(row <= 45) {	// expect him!
						r[i].a[a].should = 1 ;


					}
					r[i].a[a].c[c].min_adc = 0xFFFF ;
				}
				}

			}
		}
	}

	return ;
}

int tpxStat::run_stop(FILE *ofile, u_int rb_mask, int run_type, char *fname)
{

	int should ;
	int err = 0 ;
	int str_cou = 0 ;

	if(ofile==0) return 0 ;

	if(stripes_f) fclose(stripes_f) ;
	stripes_f = 0 ;

	if(stripes && (run_type != RUN_TYPE_PED)) {
		LOG(WARN,"saw %d occurences of more than 400 timebins",stripes) ;
		for(int i=0;i<6;i++) {
		int s_real, r_real ;

		if(rb_mask & (1<<i)) ;
		else continue ;

		tpx36_to_real(sector,i+1,s_real,r_real) ;
		r_real-- ;

		for(int a=0;a<256;a++) {
		for(int c=0;c<16;c++) {
			if(r[i].a[a].c[c].stripes) {
				int row, pad ;

				str_cou++ ;

				if(str_cou > 4) continue ;

				tpx_from_altro(r_real,a,c,row,pad) ;
				LOG(WARN,"Stripes %d/%d: RDO %d: AID %3d:%02d, r:p %2d:%03d",
				    r[i].a[a].c[c].stripes,r[i].a[a].c[c].count,i+1,a,c,row,pad) ;

				if(str_cou == 4) {
					LOG(WARN,"Stripes logging stopped, stripes %d",stripes) ;
				}
			}
		}
		}
		}
	}

	for(int i=0;i<6;i++) {



	int a, c ;
	int s_real, r_real ;


	if(rb_mask & (1<<i)) should = 1 ;
	else should = 0 ;


	if(r[i].errs) {
		LOG(WARN,"[RDO %d] had %d ALTRO errors",i+1,r[i].errs) ;
	}

	if(r[i].should != should) {
		err++ ;
		fprintf(ofile,"%sERROR: [RDO %d] in_RC %d, in_data %d -- MISMATCH\n%s",ANSI_RED,i+1,should,r[i].should,ANSI_RESET) ;
		LOG(ERR,"[RDO %d] in_RC %d, in_data %d -- MISMATCH",i+1,should,r[i].should) ;
	}


	if(r[i].count==0) {
		if(r[i].should) {
			err++ ;
			fprintf(ofile,"%sERROR: [RDO %d] expect data, found none!\n%s",ANSI_RED,i+1,ANSI_RESET) ;
			LOG(WARN,"[RDO %d] expect data, found none!",i+1) ;
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

	tpx36_to_real(sector,i+1,s_real,r_real) ;
	r_real-- ;	// start from 0 ;


	for(a=0;a<256;a++) {
		int a_err = 0 ;
		for(c=0;c<16;c++) {
			u_int have, expect ;
			
			int warn = 0 ;
			int p_err = 0 ;

			have = r[i].a[a].c[c].count ;
			if(r[i].a[a].should) {
				expect = r[i].count ;
			}
			else {
				expect = 0 ;
			}

			int row ;
			int pad ;

			tpx_from_altro(r_real,a,c,row,pad) ;

			for(int j=0;j<tpx_odd_fee_count;j++) {

				if((tpx_odd_fee[j].sector != s_real) || (tpx_odd_fee[j].rdo != (r_real+1))) continue ;

				for(int k=0;k<2;k++) {
					int aid = tpx_odd_fee[j].altro_id_padplane ;
					
					if(a == (aid+k)) {
						expect = 0 ;
						if(c==0) {
							LOG(WARN,"[RDO %d] altro %3d was marked odd in the gain file...",i+1,a) ;
						}
					}
				}
			}

			if(have != expect) {
				warn = 1 ;
				a_err = 1 ;
				p_err = 1 ;

				LOG(WARN,"[RDO %d] AID %3d:%2d (r:p %d:%d): expect %d counts, have %d",i+1,a,c,
				    row,pad,
				    expect,have) ;
			}


			if(have && expect) {
				int ped_warn = 0 ;
				if(run_type == RUN_TYPE_PED) {
					if(r[i].a[a].c[c].min_adc < 10) ped_warn = 1 ;
					if(r[i].a[a].c[c].max_adc > 200) ped_warn = 1 ;
				}

				if(ped_warn) {
					LOG(WARN,"[RDO %d] AID %3d:%2d (r:p %d:%d) pedestal: min adc %3d, max adx %4d",
					    i+1,a,c,row,pad,
					    r[i].a[a].c[c].min_adc,
					    r[i].a[a].c[c].max_adc) ;
					warn = 1 ;
				}


			}


			if(warn) {
				if(p_err) fprintf(ofile,"\t%sERROR: ",ANSI_RED) ;
				else fprintf(ofile,"\t%sWARN: ",ANSI_CYAN) ;			

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

	if(fee_check_on) {
		FILE *f = fopen(fname,"w") ;

		for(int i=0;i<fee_check_count;i++) {
		int fee = fee_check[i] ;
		
		for(int j=0;j<2;j++) {
		int ix = 2*i + j ;

	
		for(int ch=0;ch<16;ch++) {
			int err ;
			char errstr[1024] ;
			errstr[0] = 0 ;
			err = 0 ;

			double cou = fee_check_data[ix][ch].count ;
			int aid = fee_check_data[ix][ch].aid ;
			int row, pad ;

			u_char j1 ;

			tpx_from_altro(2,aid,ch,row,pad) ;

			j1 = tpx_altro_to_j1[aid&1][ch] ;

			if(cou == 0.0) {
				LOG(DBG,"   ALTRO %3d:%02d: %4d counts",aid,ch,(int)fee_check_data[ix][ch].count) ;
				fprintf(f,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",24,3,fee,j1,aid,ch,row,pad,
				       0.0,0.0,3,"No data") ;

				continue ;
			}

			fee_check_data[ix][ch].ped /= cou ;
			fee_check_data[ix][ch].rms /= cou ;
			fee_check_data[ix][ch].charge /= cou ;
			fee_check_data[ix][ch].t0 /= cou ;

			fee_check_data[ix][ch].ped_sigma /= cou ;
			fee_check_data[ix][ch].rms_sigma /= cou ;
			fee_check_data[ix][ch].charge_sigma /= cou ;
			fee_check_data[ix][ch].t0_sigma /= cou ;

			fee_check_data[ix][ch].ped_sigma = sqrt(fee_check_data[ix][ch].ped_sigma - fee_check_data[ix][ch].ped*fee_check_data[ix][ch].ped) ;
			fee_check_data[ix][ch].rms_sigma = sqrt(fee_check_data[ix][ch].rms_sigma - fee_check_data[ix][ch].rms*fee_check_data[ix][ch].rms) ;
			fee_check_data[ix][ch].charge_sigma = sqrt(fee_check_data[ix][ch].charge_sigma - fee_check_data[ix][ch].charge*fee_check_data[ix][ch].charge) ;
			fee_check_data[ix][ch].t0_sigma = sqrt(fee_check_data[ix][ch].t0_sigma - fee_check_data[ix][ch].t0*fee_check_data[ix][ch].t0) ;


			LOG(TERR,"   A %3d:%02d: %4d cou: ped %.2f +- %.2f, rms %.2f +- %.2f, cha %.2f +- %.2f, t0 %.2f +- %.2f",aid,ch,(int)cou,
				fee_check_data[ix][ch].ped, fee_check_data[ix][ch].ped_sigma, 
				fee_check_data[ix][ch].rms, fee_check_data[ix][ch].rms_sigma, 
				fee_check_data[ix][ch].charge, fee_check_data[ix][ch].charge_sigma, 
				fee_check_data[ix][ch].t0, fee_check_data[ix][ch].t0_sigma) ;

			double gain = 	fee_check_data[ix][ch].charge / QA_CHARGE ;
			if((gain<0.9) || (gain>1.1)) {
				strcat(errstr,"Bad gain,") ;
				err = 1 ;
			}
			
			double tmp = fee_check_data[ix][ch].ped ;
			if((tmp > 100.0) || (tmp<5.0)) {
				strcat(errstr,"Bad pedestal,") ;
				err = 2 ;
			}
			
			tmp = fee_check_data[ix][ch].rms ;
			if((tmp < 0.4) || (tmp>1.2)) {
				strcat(errstr,"Noisy,") ;
				err = 2 ;
			}

			if(r[2].count != cou) {
				strcat(errstr,"Flaky readout,") ;
				err = 3 ;
			}

			fprintf(f,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",24,3,fee,j1,aid,ch,row,pad,
				gain,
				fee_check_data[ix][ch].t0,
				err,errstr) ;


		}
		}
		}

		fclose(f) ;
	}

	return err ;
}

void tpxStat::accum(char *rdobuff, int bytes)
{
	int t ;
	u_int *data_end ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;
	int errors = 0 ;
	const u_int MAX_ERRORS = 10 ;
	int r0_logical ;

	TLOG() ;
	t = tpx_get_start(rdobuff, bytes/4, &rdo, 0) ;

	if(t <= 0) return ;	// non data event or some error

	r0_logical = tpx36_from_real(sector, rdo.sector, rdo.rdo) ;
	r0_logical-- ;	// so it starts from 0
	
//	if(sector && (rdo.sector != sector)) {
//		LOG(ERR,"sector mismatch: expect %d, in data %d",sector,rdo.sector) ;
//		return ;
//	}


	if(rdo.rdo > 6) {
		LOG(ERR,"rdo error: %d",rdo.rdo) ;
		return ;
	}

	a.what = TPX_ALTRO_DO_ADC ;	// I need this for the stripes count
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.t = t ;
	a.sector = rdo.sector ;
	a.ch = 0 ; // can't rely


	if(r[r0_logical].errs >= MAX_ERRORS) {
		a.log_err = 0 ;
	}
	else {
		a.log_err = 1 ;
	}

	r[r0_logical].count++ ;

	data_end = rdo.data_end ;

	TLOG() ;

	do {
		TLOG() ;
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;
		TLOG() ;
		if(a.err || (data_end==0)) {
			//LOG(ERR,"Got err %d:%d, log error = %d: data_end %p",a.id,a.ch,a.log_err,data_end) ;
			errors = 1 ;
		}
		else {
			TLOG() ;
			if((a.rdo >= 6) || (a.ch >= 16)) {
				LOG(ERR,"What !? %d %d",a.rdo,a.ch) ;
			}
			TLOG() ;
			if(a.count >= 400) {
				TLOG() ;
				stripes++ ;
				TLOG() ;
				r[r0_logical].a[a.id].c[a.ch].stripes++ ;				
				if((stripes % 100)==0) {
					LOG(NOTE,"A lot of stripes: %d",stripes) ;
				}

				TLOG() ;
				if(stripes_f) {
					fprintf(stripes_f,"==> RDO %d, cou %d: %d %d\n",a.rdo,r[r0_logical].count,a.row,a.pad) ;
					for(int i=0;i<a.count;i++) {
						fprintf(stripes_f,"%d %d\n",a.tb[i],a.adc[i]) ;
					}
				}

			}
			TLOG() ;
		}
		
		//LOG(TERR,"%d: AID %d:%d",data_end-rdo.data_start,a.id,a.ch) ;

		if((data_end==0) || (a.ch>15) || (a.rdo >= 6)) {
			LOG(ERR,"Bad boy: rdo %d, AID %d:%d -- %p",a.rdo,a.id,a.ch,data_end) ;
			continue ;
		}

		r[r0_logical].a[a.id].c[a.ch].count++ ;

		TLOG() ;
		if(run_type == RUN_TYPE_PED) {
			for(int i=0;i<a.count;i++) {
				if(a.adc[i] > r[r0_logical].a[a.id].c[a.ch].max_adc) {
					r[r0_logical].a[a.id].c[a.ch].max_adc = a.adc[i] ;
				}
				if(a.adc[i] < r[r0_logical].a[a.id].c[a.ch].min_adc) {
					r[r0_logical].a[a.id].c[a.ch].min_adc = a.adc[i] ;
				}

			}
		}

		if(fee_check_on) {
		
			int a_ok = -1 ;
			for(int i=0;i<fee_check_count*2;i++) {
				int a0 ;
				a0 = fee_check_data[i][0].aid ;
				if(a.id==a0) {
					a_ok = i ;
					break ;
				}
			}

			if(a_ok <0) {
				LOG(WARN,"RDO %d: spurious ALTRO %3d",rdo.rdo,a.id) ;
				continue ;
			}
	
			fee_check_data[a_ok][a.ch].count++ ;

			double ped, rms, cou ;
			ped = rms = cou = 0.0 ;
			for(int i=0;i<a.count;i++) {
				if(a.tb[i] < 100) {
					ped += a.adc[i] ;
					rms += a.adc[i] * a.adc[i];
					cou++ ;
				}
			}
		
			if(cou == 0.0) cou = 1.0 ;	// oh well...

			ped /= cou ;
			rms /= cou ;
			rms = sqrt(rms - ped*ped) ;

			fee_check_data[a_ok][a.ch].ped += ped ;
			fee_check_data[a_ok][a.ch].ped_sigma += ped*ped ;

			fee_check_data[a_ok][a.ch].rms += rms ;
			fee_check_data[a_ok][a.ch].rms_sigma += rms*rms ;
			
			double d_sum, d_t0 ;
			d_sum = d_t0 = 0.0 ;
			for(int i=0;i<a.count;i++) {
				if((a.tb[i] >= QA_TB_START) && (a.tb[i] <= QA_TB_STOP)) {
					double d = a.adc[i] - ped ;

					d_sum += d ;
					d_t0 += d*a.tb[i] ;
				}
			}

			fee_check_data[a_ok][a.ch].charge += d_sum ;
			fee_check_data[a_ok][a.ch].charge_sigma += d_sum*d_sum ;
		
			if(d_sum == 0.0) d_t0 = 100.0 ;
			else d_t0 /= d_sum ;

			fee_check_data[a_ok][a.ch].t0 += d_t0 ;
			fee_check_data[a_ok][a.ch].t0_sigma += d_t0*d_t0 ;

			//LOG(TERR,"Got A %3d:%02d, count %d: ped %f, rms %f, charge %f, t0 %f",a.id,a.ch,a.count,ped,rms,d_sum,d_t0) ;


		}	// end of fee_check_on

		if(data_end == rdo.data_start) {
			LOG(WARN,"Data end == data start...") ;
		}

	} while(data_end && (data_end > rdo.data_start)) ;

	TLOG() ;

	if(errors) {
		r[r0_logical].errs++ ;
		if(r[r0_logical].errs == MAX_ERRORS) {	
			LOG(WARN,"RDO %d has %d errors -- stopping logging",r0_logical+1,r[r0_logical].errs) ;
		}
	} ;

	return ;

}
