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

#include <DAQ_READER/daq_dta.h>

#include "fgtPed.h"



fgtPed::fgtPed()
{
	valid = 0 ;
	rb_mask = 0x03 ;	// assume all..

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	
	sizeof_ped = sizeof(struct peds) * FGT_RDO_COU ;	// for FGT_RDO_COU RDOs

	
	ped_store = 0 ;	// unassigned!

	fgt_rdr = 0;

	return ;
}


fgtPed::~fgtPed()
{
	if(ped_store) {
		free(ped_store) ;
	}

	if(fgt_rdr) delete fgt_rdr ;

	return ;
}


void fgtPed::init(int active_rbs)
{
	valid = 0 ;

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (struct peds *) malloc(sizeof_ped) ;

		fgt_rdr = new daq_fgt(0) ;
	}

	memset(ped_store,0,sizeof_ped) ;

	LOG(TERR,"Pedestals zapped: rb_mask 0x%02X",rb_mask) ;
}


int fgtPed::do_zs(char *src, int in_bytes, char *dst, int rdo1)
{
	LOG(WARN,"NOT YET DONE!") ;

	u_short *d_out = (u_short *) dst ;
	int fiber ;
	u_short cap ;
	u_short *d ;
	u_int *d32 = (u_int *) src ;

	LOG(NOTE,"FGT ZS: rdo %d: in bytes %d",rdo1,in_bytes) ;

	d = (u_short *)(src + 10*4) ;	// data start at 10th word

	cap = (u_short) (d32[8] & 0xFF) ;	// cap is now full 8 bits

	fiber = (rdo1 - 1) ;

	*d_out++ = 0x0000 ;	// version 0; until done

	u_short *count = d_out++ ;	// save counter spot
	*d_out++ = d32[8] & 0xFFFF ;	// save 16 bits of the cap!
	*d_out++ = fiber ;

	u_short *tmp = d_out ;

#if 0
	double *ped = (ped_store + rdo1 - 1)->ped[cap] ;
	u_short *thr = (ped_store + rdo1 - 1)->thr[cap] ;
	
	for(int ii=0;ii<4800;ii++) {
		u_short dta = *d ;
		if(dta > *thr) {
			*d_out++ = ii ;
			*d_out++ = dta - (int)(*ped + 0.5) ;
		}
		d++ ;
		thr++ ;
		ped++ ;
	}
#endif

	*count = (d_out - tmp)/2 ;

	int out_bytes = (char *)d_out - dst ;

	if(out_bytes > in_bytes) {
		valid_evts[rdo1-1]++ ;	// not really!
		LOG(NOTE,"FGT ZS: rdo %d: in bytes %d, out bytes %d",rdo1,in_bytes,out_bytes) ;
	}
	else {
		LOG(NOTE,"FGT ZS: rdo %d: in bytes %d, out bytes %d",rdo1,in_bytes,out_bytes) ;
	}

	evts[rdo1-1]++ ;
	return out_bytes ;
}


/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition.
	rdo counts from 1.
*/
void fgtPed::accum(char *evbuff, int bytes, int rdo1)
{
	LOG(WARN,"NOT DONE!") ;

	int rdo = rdo1 - 1 ;	// since rdo1 is from 1



	// skip first few events!
	if(evts[rdo] <= 3) {
		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo,evts[rdo]) ;
		return ;
	}

	valid_evts[rdo]++ ;

        LOG(NOTE,"RDO %d: event %d",rdo,evts[rdo]) ;

	struct peds *p = ped_store + rdo ;

	daq_dta *dd = fgt_rdr->handle_adc(1,rdo1, evbuff) ;

	while(dd->iterate()) {
		if(dd->rdo != rdo1) continue ;

		int arm = dd->sec ;
		int apv = dd->pad ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

		for(u_int i=0;i<dd->ncontent;i++) {
			int adc ;
			int ch ;

			if(f[i].tb != 1) continue ;

			ch = f[i].ch ;
			adc = f[i].adc ;

			p->ped[arm][apv][ch] += (double) adc ;
			p->rms[arm][apv][ch] += (double) (adc * adc) ;
			p->cou[arm][apv][ch]++ ;
		}
	}


	return ;

}

void fgtPed::do_thresh(double n_sigma)
{

	if(!ped_store || !valid) {
		LOG(ERR,"fgt:do_thresh invalid") ;
		return ;
	}


	// use the 0th timebin!
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *p = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			p->thr[arm][apv][c] = (u_short) (p->ped[arm][apv][c] + p->rms[arm][apv][c] * n_sigma + 0.5) ;

		}
		}
		}
	}

	return ;

}

void fgtPed::calc()
{

	const u_int MIN_EVENTS = 20 ;


	LOG(NOTE,"Calculating pedestals") ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int ch=0;ch<FGT_CH_COU;ch++) {

			if(ped->cou[arm][apv][ch] == 0) {
				ped->ped[arm][apv][ch] = 0xFFFF ;
				ped->rms[arm][apv][ch] = 9.999 ;
			}
			else {
				double pp, rr ;

				pp = ped->ped[arm][apv][ch] / (double) ped->cou[arm][apv][ch] ;
				rr = ped->rms[arm][apv][ch] / (double) ped->cou[arm][apv][ch] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[arm][apv][ch] = pp ;
				ped->rms[arm][apv][ch] = rr ;
			}
		}
		}
		}

	}


	int bad = 0 ;
	int real_bad = 0 ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int ch=0;ch<FGT_CH_COU;ch++) {
			if(ped->cou[arm][apv][ch] < MIN_EVENTS) {
				bad++ ;

				if(bad<50) {
					LOG(WARN,"RDO %d, ARM %d, APV %2d: CH %3d: only %d events!",r+1,arm,apv,ch,ped->cou[arm][apv][ch]) ;
				}
				else if(bad==50) {
					LOG(WARN,"Stopping detailed bad cap logging...") ;
				}

				if(ped->cou[arm][apv][ch] == 0) real_bad++ ;
			}
		
		}
		}
		}
	}

	LOG(TERR,"Pedestals calculated. RDO counts: %u %u",valid_evts[0],valid_evts[1]) ;

	valid = ! bad ;	// if there's any problem I invalidate validity!

	if(valid) {
		//LOG(TERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;
	}
	else {
		LOG(ERR,"FGT pedestals not good (%d channels not good, %d missing)",bad,real_bad) ;
		if(!real_bad) {
			LOG(WARN,"But since no real bad I will allow it!") ;
			valid = 1 ;
		}
	}

	return ;
}


int fgtPed::to_evb(char *buff)
{
	int r, arm, apv, c ;


	u_short *dta = (u_short *) buff ;	


	if(!valid) {
		// log error but continue...
		LOG(WARN,"ped::to_evb peds are bad: valid %d",valid) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	for(r=0;r<FGT_RDO_COU;r++) {
		struct peds *ped = ped_store + r ;

		*dta++ = 0x0000 ;		// version
		*dta++ = FGT_ARM_COU ;
		*dta++ = FGT_APV_COU ;
		*dta++ = FGT_CH_COU ;			
		*dta++ = r ;			// fiber...	

		for(arm=0;arm<FGT_ARM_COU;arm++) {
		for(apv=0;apv<FGT_APV_COU;apv++) {
		for(c=0;c<FGT_CH_COU;c++) {

				u_int rr, pp ;

				rr = (u_int)(ped->rms[arm][apv][c] * 8.0 + 0.5) ;
				if(rr > 0x3F) rr = 0x3F ;	// maximum I can have!

				
				pp = (u_int)(ped->ped[arm][apv][c] + 0.5)  ;
				if(pp > 0x3FF) pp = 0x3FF ;	// maximum I can have!

				*dta++ = (rr<<10)|pp ;

		}
		}
		}

	}

	LOG(TERR,"Pedestals prepared for later EVB, %d bytes",(char *)dta-buff) ;

	return ((char *)dta-buff) ;
}

int fgtPed::from_cache(char *fname) 
{
	FILE *f ;
	char *fn ;
	
	init(0x3) ;	// to clear ped storage for all FGT_RDO_COU RDOs
	
	
	// trivial load from disk...
	if(fname) {
		fn = fname ;
		f = fopen(fname,"r") ;
	}
	else {
		fn = "/RTScache/pedestals.txt" ;
		f = fopen(fn,"r") ;
	}

	if(f==0) {
		LOG(ERR,"ped::from_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(NOTE,"Loading pedestals from cache \"%s\"...",fn) ;

	while(!feof(f)) {
		int r, arm, apv, ch ;
		float pp, rr ;

		int ret = fscanf(f,"%d %d %d %d %f %f",&r,&arm,&apv,&ch,&pp,&rr) ;
		if(ret != 6) continue ;

		struct peds *peds = ped_store + (r-1) ;

		peds->ped[arm][apv][ch] = pp ;
		peds->rms[arm][apv][ch] = rr ;
	}

	fclose(f) ;
	LOG(TERR,"Pedestals loaded from cache \"%s\"",fn) ;


	valid = 1 ;

	return valid ;
}

int fgtPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	char *fn ;


	if(!valid) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d -- not caching",valid) ;
		return -1 ;
	}

	if(fname) {
		fn = fname ;
	}
	else {
		fn = "/RTScache/pedestals.txt" ;
	}


	f = fopen(fn,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(NOTE,"Writing pedestals to cache \"%s\"...",fn) ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *peds = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			fprintf(f,"%d %d %2d %3d %7.3f %.3f\n",r+1,arm,apv,c,
				peds->ped[arm][apv][c],
				peds->rms[arm][apv][c]) ;
		}
		}
		}
	}

	fclose(f) ;	

	LOG(TERR,"Pedestals written to cache \"%s\"",fn) ;

	return 1 ;
}

int fgtPed::special_setup(int run_type, int sub_type)
{

	return 1 ;
}



