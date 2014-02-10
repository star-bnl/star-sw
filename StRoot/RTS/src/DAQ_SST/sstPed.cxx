#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>

#include <rtsLog.h>
#include <daqModes.h>
#include <rtsSystems.h>

#include <DAQ_READER/daq_dta.h>

#include "sstPed.h"



sstPed::sstPed()
{
	valid = 0 ;
	rb_mask = 0x07 ;	// assume max...
	sector = 1 ;		// assume 1

	sizeof_ped = sizeof(struct peds) * SST_RDO_COU ;	// for SST_RDO_COU RDOs
	
	ped_store = 0 ;	// unassigned!

	memset(sst_rdr,0,sizeof(sst_rdr)) ;


	return ;
}


sstPed::~sstPed()
{
	if(ped_store) {
		free(ped_store) ;
		ped_store = 0 ;
	}

	for(int i=0;i<SST_RDO_COU;i++) {
		if(sst_rdr[i]) {
			delete sst_rdr[i] ;
			sst_rdr[i] = 0 ;	
		}
	}

	valid = 0 ;

	return ;
}

#if 0
int sstPed::run_stop()
{
	for(int r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		int evts = sst_stat[r].evts ;

		if(sst_stat[r].err) {
			LOG(ERR,"RDO %d: %d errors in %d events",r+1,sst_stat[r].err,evts) ;
		}

		for(int arm=0;arm<SST_ARM_COU;arm++) {
			if(sst_stat[r].arm_mask & (1<<arm)) ;
			else continue ;

			for(int apv=0;apv<SST_APV_COU;apv++) {
				int errs = sst_stat[r].err_apv[arm][apv] ;
				int cou = sst_stat[r].cou_apv[arm][apv] ;

				if(errs || (cou && (cou!=evts))) {
					LOG(ERR,"RDO %d: ARM %d, APV %2d: %d errors, in %d/%d events",
					    r+1,arm,apv,errs,cou,evts) ;
				}
			}
		}
	}

	return 0 ;
}

#endif

void sstPed::init(int active_rbs)
{
	valid = 0 ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (struct peds *) malloc(sizeof_ped) ;
	}

	for(int i=0;i<SST_RDO_COU;i++) {
		if((rb_mask & (1<<i)) && (sst_rdr[i]==0)) {
			sst_rdr[i] = new daq_sst(0) ;
		}
	}

	memset(ped_store,0,sizeof_ped) ;

	LOG(TERR,"Pedestals zapped: rb_mask 0x%02X",rb_mask) ;
}




/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition.
	rdo counts from 1.
*/
void sstPed::accum(char *evbuff, int bytes, int rdo1)
{
	LOG(NOTE,"entering %d",rdo1) ;

	int rdo = rdo1 - 1 ;	// since rdo1 is from 1

	LOG(NOTE,"accum %d",rdo) ;

	struct peds *p = ped_store + rdo ;

	LOG(NOTE,"accum %d",rdo1) ;

	daq_dta *dd  ;

	LOG(NOTE,"accum %d",rdo1) ;

	dd = sst_rdr[rdo1-1]->handle_adc(sector,rdo1, evbuff,bytes/4) ;

//	LOG(TERR,"Got %p",dd) ;

	while(dd && dd->iterate()) {
		if(dd->sec != sector) continue ;
		if(dd->rdo != rdo1) continue ;

		int fiber = dd->pad ;	// that's the fiber...

		daq_sst_data_t *f = (daq_sst_data_t *) dd->Void ;


		LOG(NOTE,"sec %d, rdo %d, fiber %d: %d",dd->sec,dd->rdo,fiber,dd->ncontent) ;

		for(u_int i=0;i<dd->ncontent;i++) {
			int adc ;
			int hy ;
			int strip ;

			hy = f[i].hybrid ;
			adc = f[i].adc ;
			strip = f[i].strip ;

			int adc_prime = (adc+300)%1024 ;	//tha SST oddness; Jim Thomas email


//			LOG(TERR,"    %d %d %d %d",hy,strip,adc,adc_prime) ;

//			if((adc_prime<300)||(adc_prime>700)) continue ;	// outlier cuts; Jim Thomas email
			
			if(p->cou[fiber][hy][strip] > 0xFFF0) continue ;	// protect u_short

			p->ped[fiber][hy][strip] += (float) adc_prime ;
			p->rms[fiber][hy][strip] += (float) (adc_prime * adc_prime) ;
			p->cou[fiber][hy][strip]++ ;
		}
	}


	return ;

}


void sstPed::calc()
{

	const int MIN_EVENTS = 99 ;


	LOG(NOTE,"Calculating pedestals") ;

	u_int bad[SST_RDO_COU][SST_FIBER_COU] ;

	memset(bad,0,sizeof(bad)) ;

	for(int r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		for(int fib=0;fib<SST_FIBER_COU;fib++) {
		for(int hy=0;hy<SST_HYBRID_COU;hy++) {
		for(int strip=0;strip<SST_STRIP_COU;strip++) {


			if(ped->cou[fib][hy][strip] == 0) {	// never seen in the data!
				ped->ped[fib][hy][strip] = 0 ;
				ped->rms[fib][hy][strip] = -1.0 ;
			}
			else {
				double pp, rr ;

				
				pp = ped->ped[fib][hy][strip] / (double) ped->cou[fib][hy][strip] ;

				rr = ped->rms[fib][hy][strip] / (double) ped->cou[fib][hy][strip] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[fib][hy][strip] = pp ;
				ped->rms[fib][hy][strip] = rr ;


					

			}


			if(ped->cou[fib][hy][strip] < MIN_EVENTS) {
				//LOG(WARN,"%d %d %d %d: %d",r,fib,hy,strip,ped->cou[fib][hy][strip]) ;
				bad[r][fib]++ ;
			}
		}
		}
		}

	}

	valid = 1 ;
	for(int r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		for(int fib=0;fib<SST_FIBER_COU;fib++) {
			if(bad[r][fib]) {
				LOG(ERR,"RDO %d, FIBER %d: has %d strips with insufficent counts!",r+1,fib,bad[r][fib]) ;
				valid = 0 ;
			}
		}
	}
	
	LOG(TERR,"Pedestals calculated.") ;
	
	return ;
}

#if 0
int sstPed::to_evb(char *buff)
{
	int r, arm, apv, c, t ;
	u_short *dta = (u_short *) buff ;	


	if(!valid) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d",valid) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	*dta++ = 0xBEEF ;		// signature
	*dta++ = 0x0001 ;		// version
	*dta++ = SST_RDO_COU ;		// ARM
	*dta++ = SST_FIBER_COU ;
	*dta++ = SST_HYBRID_COU ;		// channel count
	*dta++ = SST_STRIP_COU ;		// timebin count


	for(r=0;r<SST_RDO_COU;r++) {
		if(rb_mask && (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		*dta++ = r+1 ;			// ARC, from 1
		u_short *apv_cou = dta++ ;
		*apv_cou = 0 ;

		// need to dump the apv_meta_zs_t bank!!!

		for(arm=0;arm<SST_ARM_COU;arm++) {
		for(apv=0;apv<SST_APV_COU;apv++) {

		if(ped->expect_cou[arm][apv] == 0) continue ;	// no hits at all...

		*dta++ = arm ;
		*dta++ = apv ;
		(*apv_cou)++ ;

		for(c=0;c<SST_CH_COU;c++) {
		for(t=0;t<tb_cou_ped;t++) {

				u_short pp ;

				pp = (u_short)(ped->ped[arm][apv][c][t] + 0.5)  ;
				*dta++ = pp;

				pp = (u_short)(ped->rms[arm][apv][c][t]*16.0  + 0.5) ;

				*dta++ = pp ;
		}
		}
		}
		}

	}

	LOG(TERR,"Pedestals prepared for later EVB, %d bytes",(char *)dta-buff) ;

	return ((char *)dta-buff) ;
}
#endif

int sstPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	char f_fname[128] ;

	if(!valid) {
		LOG(CAUTION,"Pedestals are not valid -- not caching!") ;
		return -1 ;
	}

	time_t tim = time(0) ;

	for(int r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		
		if(fname) {
			sprintf(f_fname,"%s/sst_pedestals_s%d_r%d.txt",fname,sector,r+1) ;
		}
		else {
			sprintf(f_fname,"/RTScache/sst_pedestals_s%d_r%d.txt",sector,r+1) ;
		}


		f = fopen(f_fname,"w") ;
		if(f==0) {
			LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",f_fname,strerror(errno)) ;
			continue ;
		}



		fprintf(f,"# Run %08u\n",run) ;
		fprintf(f,"# Date %s",ctime(&tim)) ;
		fprintf(f,"# Sector %d\n",sector) ;
		fprintf(f,"# RDO %d\n",r+1) ;

		fprintf(f,"\n") ;

		struct peds *peds = ped_store + r ;

		for(int fib=0;fib<SST_FIBER_COU;fib++) {
		for(int hy=0;hy<SST_HYBRID_COU;hy++) {
		for(int strip=0;strip<SST_STRIP_COU;strip++) {

			fprintf(f,"%d %2d %3d %.3f %.3f\n",fib,hy,strip,
				peds->ped[fib][hy][strip],
				peds->rms[fib][hy][strip]) ;
		}
		}
		}

		fclose(f) ;
		LOG(TERR,"Pedestals written to cache \"%s\"",f_fname) ;
	}




	return 1 ;
}

