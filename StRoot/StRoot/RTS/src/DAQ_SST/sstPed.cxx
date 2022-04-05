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

#include "daq_sst.h"
#include "sstPed.h"



sstPed::sstPed()
{
	valid = 0 ;
	rb_mask = 0x07 ;	// assume max...
	sector = 1 ;		// assume 1

	sizeof_ped = sizeof(daq_sst_ped_t) * SST_RDO_COU ;	// for SST_RDO_COU RDOs
	
	ped_store = 0 ;	// unassigned!

	sst_rdr = 0 ;

	return ;
}


sstPed::~sstPed()
{
	if(ped_store) {
		free(ped_store) ;
		ped_store = 0 ;
	}

	if(sst_rdr) {
		delete sst_rdr ;
		sst_rdr = 0 ;	
	}

	valid = 0 ;

	return ;
}


void sstPed::init(int active_rbs)
{
	valid = 0 ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (daq_sst_ped_t *) malloc(sizeof_ped) ;
	}

	if(sst_rdr == 0) sst_rdr = new daq_sst(0) ;

	memset(ped_store,0,sizeof_ped) ;

	memset(sst_rdr->events,0,sizeof(sst_rdr->events)) ;
	memset(sst_rdr->fiber_events,0,sizeof(sst_rdr->fiber_events)) ;

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

	LOG(NOTE,"accum %d",rdo1) ;

	daq_sst_ped_t *p = ped_store + rdo ;

	sst_rdr->raw_to_adc_utility(sector,rdo1,evbuff,bytes/4,p,2) ;

	LOG(NOTE,"accum done %d",rdo1) ;

	return ;

}


void sstPed::calc()
{

	const u_int MIN_EVENTS = 90 ;


	LOG(NOTE,"Calculating pedestals") ;

	u_int bad[SST_RDO_COU][SST_FIBER_COU] ;

	memset(bad,0,sizeof(bad)) ;

	for(int r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		daq_sst_ped_t *ped = ped_store + r ;

		for(int fib=0;fib<SST_FIBER_COU;fib++) {
		for(int hy=0;hy<SST_HYBRID_COU;hy++) {
		for(int strip=0;strip<SST_STRIP_COU;strip++) {


			if(ped->cou[fib][hy][strip] == 0) {	// never seen in the data!
				ped->ped[fib][hy][strip] = 0 ;
				ped->rms[fib][hy][strip] = -1.0 ;
			}
			else {
				double pp, rr ;

				
				pp = (double) ped->ped[fib][hy][strip] / (double) ped->cou[fib][hy][strip] ;

				rr = (double) ped->rms[fib][hy][strip] / (double) ped->cou[fib][hy][strip] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[fib][hy][strip] = pp ;
				ped->rms[fib][hy][strip] = rr ;


					

			}

			if(ped->cou[fib][hy][strip] < MIN_EVENTS) {
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
				LOG(WARN,"RDO %d, FIBER %d: has %d strips with insufficent counts!",r+1,fib,bad[r][fib]) ;
			}
			if(sst_rdr->fiber_events[r][fib] < MIN_EVENTS) {
				LOG(ERR,"RDO %d, FIBER %d has only %d events (< %d)",r+1,fib,sst_rdr->fiber_events[r][fib],MIN_EVENTS) ;
				valid = 0 ;
			}
		}
	}
	
	LOG(TERR,"Pedestals calculated.") ;
	
	return ;
}


int sstPed::to_evb(char *buff)
{
	int r,f,h,s ;
	u_short *dta = (u_short *) buff ;	


	if(!valid) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d",valid) ;
	}



	int rdo_cou = 0 ;
	for(r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		rdo_cou++ ;
	}


	LOG(TERR,"Preparing pedestals for later EVB: mask 0x%X, rdos %d",rb_mask,rdo_cou) ;

	*dta++ = 0xBEEF ;		// signature
	*dta++ = 0x0001 ;		// version
	*dta++ = rdo_cou ;		
	*dta++ = SST_FIBER_COU ;
	*dta++ = SST_HYBRID_COU ;	
	*dta++ = SST_STRIP_COU ;	


	for(r=0;r<SST_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		daq_sst_ped_t *ped = ped_store + r ;

		*dta++ = r+1 ;			// RDO, from 1


		for(f=0;f<SST_FIBER_COU;f++) {
		for(h=0;h<SST_HYBRID_COU;h++) {

			for(s=0;s<SST_STRIP_COU;s++) {
				
				short pp ;

				pp = (short)(ped->ped[f][h][s] + 0.5)  ;
				*dta++ = pp;

				pp = (u_short)(ped->rms[f][h][s]*16.0  + 0.5) ;
				*dta++ = pp ;
			}
		}
		}
	}

	while(((char *)dta-buff)%4) dta++ ;

	LOG(TERR,"Pedestals prepared for later EVB, %d bytes",(char *)dta-buff) ;

	return ((char *)dta-buff) ;
}


int sstPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	char f_fname[128] ;
	int ret ;

	if(!valid) {
		LOG(CAUTION,"Pedestals are not valid -- not caching!") ;
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


		f = fopen("/tmp/ssd_peds","w") ;
		if(f==0) {
			LOG(U_TONKO,"ped::to_cache can't open output file \"%s\" [%s]","/tmp/ssd_peds",strerror(errno)) ;
			continue ;
		}



		fprintf(f,"# Run %08u\n",run) ;
		fprintf(f,"# Date %s",ctime(&tim)) ;
		fprintf(f,"# Sector %d\n",sector) ;
		fprintf(f,"# RDO %d\n",r+1) ;

		fprintf(f,"\n") ;

		daq_sst_ped_t *peds = ped_store + r ;

		for(int fib=0;fib<SST_FIBER_COU;fib++) {
		for(int hy=0;hy<SST_HYBRID_COU;hy++) {
		for(int strip=0;strip<SST_STRIP_COU;strip++) {

			fprintf(f,"%d %2d %3d %.1f %.1f\n",fib,hy,strip,
				peds->ped[fib][hy][strip],
				peds->rms[fib][hy][strip]) ;
		}
		}
		}

		fclose(f) ;


		char sys[256] ;

		if(valid) {
			sprintf(sys,"/bin/cp  /tmp/ssd_peds %s",f_fname) ;
			ret = system(sys) ;
			if(ret==0) {
				LOG(TERR,"Pedestals written to cache \"%s\" [ret %d]",f_fname,ret) ;
			}
			else {
				LOG(U_TONKO,"Pedestals not written to cache \"%s\" [ret %d]",f_fname,ret) ;
			}
		}



		sprintf(sys,"/bin/cp  /tmp/ssd_peds /net/ssd-upgrade/data/PEDESTALS/sst_pedestals_%08u_s%d_r%d.txt >/dev/null 2>/dev/null",run,sector,r+1) ;
		ret = system(sys) ;
		if(ret==0) {
			LOG(TERR,"Executed [%s], ret %d",sys,ret) ;
		}
		else {
			LOG(U_TONKO,"Not executed [%s], ret %d",sys,ret) ;
		}
	}




	return 1 ;
}

