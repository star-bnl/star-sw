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

#include "bsmdPed.h"

	
bsmdPed::bsmdPed()
{
	valid = 0 ;
	rb_mask = 0x3F ;	// assume all..

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	
	sizeof_ped = sizeof(struct peds) * 6 ;	// for 6 RDOs

	ped_store = 0 ;	// unassigned!

	sector = -1 ;	// uniti...


	return ;
}


bsmdPed::~bsmdPed()
{
	if(ped_store) free(ped_store) ;

	return ;
}


void bsmdPed::init(int active_rbs)
{
	valid = 0 ;

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (struct peds *) malloc(sizeof_ped) ;
	}

	memset(ped_store,0,sizeof_ped) ;

	LOG(TERR,"Pedestals zapped: sector %2d, rb_mask 0x%02X.", sector, rb_mask) ;
}


int bsmdPed::do_zs(char *src, int in_bytes, char *dst, int rdo1, u_int *adc_sum)
{
	u_short *d_out = (u_short *) dst ;
	int fiber ;
	u_short cap ;
	u_short *d ;
	u_int *d32 = (u_int *) src ;
	double sum_adc = 0.0 ;	// for phase scanning

	LOG(NOTE,"BSMD ZS: rdo %d: in bytes %d",rdo1,in_bytes) ;

	d = (u_short *)(src + 10*4) ;	// data start at 10th word

	cap = (u_short) (d32[8] & 0x7F) ;	// cap is now 16 bits!

	fiber = (sector-1)*2 + (rdo1 - 1) ;

	*d_out++ = 0x0002 ;	// version 2!
	u_short *count = d_out++ ;	// save counter spot
	*d_out++ = d32[8] & 0xFFFF ;	// save 16 bits of the cap!
	*d_out++ = fiber ;

	u_short *tmp = d_out ;

	double *ped = (ped_store + rdo1 - 1)->ped[cap] ;
	u_short *thr = (ped_store + rdo1 - 1)->thr[cap] ;
	
	for(int ii=0;ii<4800;ii++) {
		u_short dta = *d ;
		if(dta > *thr) {

			sum_adc += (double)dta - (*ped) ;

			*d_out++ = ii ;
			*d_out++ = dta - (int)(*ped + 0.5) ;
		}
		d++ ;
		thr++ ;
		ped++ ;
	}
	*count = (d_out - tmp)/2 ;

	int out_bytes = (char *)d_out - dst ;

	if(out_bytes > in_bytes) {
		valid_evts[rdo1-1]++ ;	// not really!
		LOG(NOTE,"BSMD ZS: rdo %d: in bytes %d, out bytes %d",rdo1,in_bytes,out_bytes) ;
	}
	else {
		LOG(NOTE,"BSMD ZS: rdo %d: in bytes %d, out bytes %d",rdo1,in_bytes,out_bytes) ;
	}

	evts[rdo1-1]++ ;

	if(adc_sum) {
		*adc_sum = (u_int) (sum_adc + 0.5) ;
	}

	return out_bytes ;
}


/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition.
	rdo counts from 1.
*/
void bsmdPed::accum(char *evbuff, int bytes, int rdo1)
{
	int cap ;
	u_short *d_in ;
	u_int *d32 ;
	int rdo = rdo1 - 1 ;	// since rdo1 is from 1

	d_in = (u_short *) evbuff ;
	d32 = (u_int *) evbuff ;

	// this is where the cap should be...
	cap = d32[8] & 0x7F ;	// mask off, in case of problems...
	

	evts[rdo]++ ;


	// skip first few events!
	if(evts[rdo] <= 3) {
		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo,evts[rdo]) ;
		return ;
	}



	valid_evts[rdo]++ ;

        LOG(NOTE,"RDO %d: event %d",rdo,evts[rdo]) ;

	struct peds *p = ped_store + rdo ;

	if(p->cou[cap] > 0xFFF0) return ;
	p->cou[cap]++ ;
	
	// move to start of data
	d_in = (u_short *)(evbuff + 10*4) ;	// start at the 10th

	for(int j=0;j<4800;j++) {

		int adc = d_in[j] ;	// hack!

		p->ped[cap][j] += (double) adc ;
		p->rms[cap][j] += (double) (adc*adc) ;

	}



	return ;

}

void bsmdPed::do_thresh(double thr_sm, double thr_pre)
{
	double n_sigma ;

	if(!ped_store || !valid) {
		LOG(ERR,"bsmd:do_thresh invalid") ;
		return ;
	}


	for(int r=0;r<6;r++) {
		if((sector==2) && (r>2)) n_sigma = thr_pre ;	// preshower...
		else n_sigma = thr_sm ;

		struct peds *p = ped_store + r ;
		for(int c=0;c<128;c++) {
			for(int t=0;t<4800;t++) {
				p->thr[c][t] = (u_short) (p->ped[c][t] + p->rms[c][t] * n_sigma + 0.5) ;
			}
		}
	}

	return ;

}

void bsmdPed::calc()
{
	int r, cap, ch ;
	int bad ;
	const u_int MIN_EVENTS = 20 ;


	LOG(NOTE,"Calculating pedestals for sector %2d",sector) ;


	for(r=0;r<6;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		for(cap=0;cap<128;cap++) {
		for(ch=0;ch<4800;ch++) {

			if(ped->cou[cap] == 0) {
				ped->ped[cap][ch] = 0xFFFF ;
				ped->rms[cap][ch] = 9.999 ;
			}
			else {
				double pp, rr ;

				pp = ped->ped[cap][ch] / (double) ped->cou[cap] ;
				rr = ped->rms[cap][ch] / (double) ped->cou[cap] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[cap][ch] = pp ;
				ped->rms[cap][ch] = rr ;
			}
		}
		}
	}


	bad = 0 ;
	int real_bad = 0 ;

	for(r=0;r<6;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		for(cap=0;cap<128;cap++) {
			if(ped->cou[cap] < MIN_EVENTS) {
				bad++ ;

				if(bad<50) {
					LOG(WARN,"RDO %d: cap %3d: only %d events!",r+1,cap,ped->cou[cap]) ;
				}
				else if(bad==50) {
					LOG(WARN,"Stopping detailed bad cap logging...") ;
				}

				if(ped->cou[cap] == 0) real_bad++ ;
			}
		
		}
	}

	LOG(TERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;

	valid = ! bad ;	// if there's any problem I invalidate validity!

	if(valid) {
		//LOG(TERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;
	}
	else {
		LOG(ERR,"BSMD pedestals not good (%d caps not good, %d missing)",bad,real_bad) ;
		if(!real_bad) {
			LOG(WARN,"But since no real bad I will allow it!") ;
			valid = 1 ;
		}
	}

	return ;
}


int bsmdPed::to_evb(char *buff)
{
	int r, p, t ;
	int fiber ;

	u_short *dta = (u_short *) buff ;	



	if(!valid) {
		// log error but continue...
		LOG(WARN,"ped::to_evb peds are bad: valid %d",valid) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	for(r=0;r<6;r++) {
		struct peds *ped = ped_store + r ;

		fiber = (sector-1)*6 + r  ;	// fiber counts from 0

		*dta++ = 0x0002 ;	// version
		*dta++ = 4800 ;		// count per cap
		*dta++ = 128 ;		// caps
		*dta++ = fiber ;	// fiber...	// match current sector+rdo to old fiber [0..11]

		for(p=0;p<128;p++) {
			for(t=0;t<4800;t++) {

				u_int rr, pp ;

				rr = (u_int)(ped->rms[p][t] * 8.0 + 0.5) ;
				if(rr > 0x3F) rr = 0x3F ;	// maximum I can have!

				
				pp = (u_int)(ped->ped[p][t] + 0.5)  ;
				if(pp > 0x3FF) pp = 0x3FF ;	// maximum I can have!

				*dta++ = (rr<<10)|pp ;

			}

		}

	}

	LOG(TERR,"Pedestals prepared for later EVB, sector %2d: %d bytes",sector,(char *)dta-buff) ;
	return ((char *)dta-buff) ;
}

int bsmdPed::from_cache(char *fname) 
{
	FILE *f ;
	char *fn ;

	init(0x3F) ;	// to clear ped storage for all 6 RDOs
	
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
		int r, p , t ;
		float pp, rr ;
		char buff[128] ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#' || buff[0]=='/') continue ;

		int ret = sscanf(buff,"%d %d %d %f %f",&r,&p,&t,&pp,&rr) ;
		if(ret != 5) continue ;

		struct peds *peds = ped_store + (r-1) ;

		peds->ped[p][t] = pp ;
		peds->rms[p][t] = rr ;
	}

	fclose(f) ;
	LOG(TERR,"Pedestals loaded from cache \"%s\": sector %2d.",fn,sector) ;


	valid = 1 ;

	return valid ;
}

int bsmdPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	int r, p, t ;
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
	time_t tim = time(0) ;
	fprintf(f,"# Detector %s\n","BSMD") ;
	fprintf(f,"# Sector %2d\n",sector) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"\n") ;

	for(r=0;r<6;r++) {
		struct peds *peds = ped_store + r ;

		for(p=0;p<128;p++) {
			for(t=0;t<4800;t++) {
				fprintf(f,"%d %d %d %8.3f %.3f\n",r+1,p,t,peds->ped[p][t],peds->rms[p][t]) ;
			}
		}
	}

	fclose(f) ;	

	LOG(TERR,"Pedestals written to cache \"%s\", for sector %2d...",fn,sector) ;

	return 1 ;
}

int bsmdPed::special_setup(int run_type, int sub_type)
{
	int r, p, t ;
	int m ;

	switch(run_type) {
	case RUN_TYPE_PULSER_A :
	case RUN_TYPE_PED_A :
	case RUN_TYPE_PED_B :
		LOG(WARN,"Special Pedestal setup: %d, %d",run_type, sub_type) ;
		break ;
	default :
		return 1 ;
	}

	for(r=0;r<6;r++) {
		struct peds *ped = ped_store + r ;
		for(p=0;p<128;p++) {
		
			switch(run_type) {
			case RUN_TYPE_PULSER_A :
				for(t=100;t<110;t++) ped->ped[p][t] = 0.0 ;
				for(t=400;t<415;t++) ped->ped[p][t] = 0.0 ;
				break ;
			case RUN_TYPE_PED_A :	// starts with ped=0
				m = 0 ;			
				for(t=0;t<512;) {
					for(int i=0;i<16;i++) {
						ped->ped[p][t+i] = m * 1023.0 ;
					}
					if(m==0) m = 1 ;
					else m = 0 ;
					t += 16 ;
				}
				break ;
			case RUN_TYPE_PED_B :	// starts with ped=1
				m = 1 ;			
				for(t=0;t<512;) {
					for(int i=0;i<16;i++) {
						ped->ped[p][t+i] = m * 1023.0 ;
					}
					if(m==0) m = 1 ;
					else m = 0 ;
					t += 16 ;
				}
				break ;
			default :	// some pattern
				for(t=0;t<512;t++) ped->ped[p][t] = 1023.0 ;	// kill all
				for(t=p;t<(p+10);t++) ped->ped[p][t] = 0 ;		// some pattern depending on row
				break ;	
			}
		}

	}


	valid = 1 ;

	return 1 ;
}



