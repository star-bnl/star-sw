#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>

#include <rtsLog.h>
#include <TPC/rowlen.h>
#include <daqModes.h>
#include <TPX/tpx_altro_to_pad.h>
#include <SFS/sfs_index.h>

//#include "tpxCore.h"
#include "stgcPed.h"


static	const int MIN_EVENTS = 100 ;

#define STGC_PED_FILENAME	"/RTScache/pedestals"

stgcPed::stgcPed()
{

	valid = 0 ;
	rb_mask = 0x3F ;	// assume all..

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	
	max_events = 1000 ;
	

	sector = -1 ;	// uniti...


	return ;
}


stgcPed::~stgcPed()
{
	stgcPed::clear() ;

	return ;
}


void stgcPed::clear()
{
	memset(peds,0,sizeof(peds)) ;
}


// called at start of run...
void stgcPed::init(int sec, int active_rbs)
{


	valid = 0 ;

	rb_mask = active_rbs ;
	sector = sec ;

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	memset(altro_found,0,sizeof(altro_found)) ;

	stgcPed::clear() ;	// zap storage ;

}

void stgcPed::smooth()
{
	LOG(TERR,"Running smooth...") ;

	LOG(TERR,"I have %f",peds[2][0].ped[0]) ;

	for(int a=0;a<256;a++) {
	for(int c=0;c<16;c++) {
		double sum = 0.0 ;
		double cou = 0.0 ;
		double rms = 0.0 ;

		
#if 0		
		for(int t=0;t<512;t++) {
			double ped = peds[a][c].ped[t] ;
			double p_rms = peds[a][c].rms[t] ;

			if(a==2 && c==0) LOG(TERR,"tb %d = %f %f",t,ped,p_rms) ;

			if(ped==0.0 && p_rms==0.0) break ;
			if(ped>1022 && p_rms>9) break ;

			sum += ped ;
			rms += ped*ped ;
			cou++ ;
		}
#endif
		// use only the first 16ish timebins; before the ripple starts

		for(int t=0;t<16;t++) {
			double ped = peds[a][c].ped[t] ;
			double p_rms = peds[a][c].rms[t] ;

			//if(a==2 && c==0) LOG(TERR,"tb %d = %f %f",t,ped,p_rms) ;

			if(ped==0.0 && p_rms==0.0) break ;
			if(ped>1022 && p_rms>9) break ;

			sum += ped ;
			rms += ped*ped ;
			cou++ ;
		}


		if(cou==0) continue ;

		sum /= cou ;
		rms /= cou ;

		rms = sqrt(rms-sum*sum) ;

		for(int t=0;t<512;t++) {
			peds[a][c].ped[t] = sum ;
			peds[a][c].rms[t] = rms ;
		}

		LOG(TERR,"A%d:%d - %d %d",a,c,(int)cou,(int)peds[a][c].ped[0]) ;
	}
	}

}

/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition
*/
void stgcPed::accum(char *evbuff, int bytes)
{
	int t ;
	u_int *data_end ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;

	t = tpx_get_start(evbuff, bytes/4, &rdo, 0) ;

	if(t <= 0) return ;	// non data event...

	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.t = t ;
	a.sector = rdo.sector ;
	a.log_err = 0 ;

	
	evts[a.rdo]++ ;

	// skip first few events!
	if(evts[a.rdo] <= 3) {
		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo.rdo,evts[a.rdo]) ;
		return ;
	}

	valid_evts[a.rdo]++ ;


	data_end = rdo.data_end ;

	do {
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;
		accum(&a) ;
	} while(data_end && (data_end > rdo.data_start)) ;


	return ;

}

void stgcPed::accum(tpx_altro_struct *aa)
{
	int a = aa->id ;
	int ch = aa->ch ;

	altro_found[a]++ ;

	for(int i=0;i<aa->count;i++) {
		int tb, adc ;

		adc = aa->adc[i] ;
		tb = aa->tb[i] ;

		peds[a][ch].ped[tb] += (double) adc ;
		peds[a][ch].rms[tb] += (double) (adc*adc) ;
		peds[a][ch].cou[tb]++ ;
	}


	return ;
}

void stgcPed::calc()
{
	int bad ;


	LOG(NOTE,"Calculating pedestals for sector %2d",sector) ;

	bad = 0 ;

	for(int a=0;a<256;a++) {
	if(!altro_found[a]) continue ;

	for(int c=0;c<16;c++) {
		for(int t=0;t<512;t++) {
			if(peds[a][c].cou[t] == 0) {
				peds[a][c].ped[t] = 1023.0 ;
				peds[a][c].rms[t] = 9.999 ;
			}
			else {
				double pp, rr ;

				pp = peds[a][c].ped[t] / (double) peds[a][c].cou[t] ;
				rr = peds[a][c].rms[t] / (double) peds[a][c].cou[t] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				peds[a][c].ped[t] = pp ;
				peds[a][c].rms[t] = rr ;
			}
		}
		LOG(TERR,"AID %d:%d = %f +- %f",a,c,peds[a][c].ped[15],peds[a][c].rms[15]) ;
	}
	}


	for(int r=0;r<6;r++) {
		if(rb_mask & (1<<r)) {
			if(valid_evts[r] < MIN_EVENTS) {
				bad = 1 ;
				LOG(ERR,"RDO %d: not enough valid events (%d < %d) [%d]",r+1,valid_evts[r],MIN_EVENTS,evts[r]) ;
			}
		}
	}

//	LOG(TERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;

	valid = ! bad ;	// if there's any problem I invalidate validity!

	if(valid) {
		LOG(TERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;
	}
	else {
		LOG(ERR,"Pedestals calculated. RDO counts: %u %u %u %u %u %u",valid_evts[0],valid_evts[1],valid_evts[2],valid_evts[3],valid_evts[4],valid_evts[5]) ;
	}

	return ;
}

// returns bytes!
int stgcPed::to_altro(char *buff, int rb, int timebins)
{
	int row, pad, t ;
	int a, ch ;

	char *rbuff = buff ;

	if(!valid) {
		LOG(ERR,"ped::to_altro peds are bad: RDO %d: valid %d",rb+1,valid) ;
	}



//	LOG(TERR,"Preparing pedestals for Slo%02d:%d (Shw%02d:%d)...",sector,rb+1,s_real,r_real) ;

	for(a=0;a<256;a++) {



	if(!altro_found[a]) continue ;

	LOG(TERR,"ALTRO %d: found %d",a,altro_found[a]) ;

	for(ch=0;ch<16;ch++) {


		u_int *addr = (u_int *) rbuff ;	// remember where to store the address
	
		rbuff += 4 ;	// skip 4 bytes

		u_short *ptr = (u_short *) rbuff ;	// start

		int tcou = 0 ;	// zero counter...

		// get the corresponding row & pad

		struct stgcPed::peds *ped = &(peds[a][ch]) ;


		// pedestal memory for the altro is really odd

		// first should be the pedestals from the start
		// of trigger...
		for(t=15;t<timebins+15;t++) {
			*ptr++ = (u_short) ped->ped[t] ;

			//if(ch==0 && t<40) LOG(TERR,"A%d:%d: %d: tb %d = %d",a,ch,tcou,t,(u_short)ped->ped[t]) ;

			if((row==42)&&(pad==140)) {
				//LOG(TERR,"%d,%d = %d",t,tcou,(u_short)ped->ped[t]) ;
			}
			tcou++ ;
		}



		// follow with a "wall" of 1023
		for(;t<(TPX_MAX_TB+15);t++) {
			u_short val = (u_short) 1023 ;

			*ptr++ = val ;
			if((row==42)&&(pad==140)) {
				//LOG(TERR,"%d,%d = %d",t,tcou,val) ;
			}

			tcou++ ;
		}



		// and this is the pedestal of the pre-trigger
		// actually, I'm totally confused... this count of 15 must
		// exist but the value seems irrelevant...
		for(t=0;t<15;t++) {
			u_short val = (u_short) ped->ped[t] ;
			*ptr++ = val ;
			if((row==42)&&(pad==140)) {
				//LOG(TERR,"%d,%d = %d",t,tcou,val) ;
			}

			tcou++ ;
		}

		// this, last value is the one that gets used for the pre- pedestals
		u_short val = (u_short) ped->ped[0] ;
		*ptr++ = val ;
		if((row==42)&&(pad==140)) {
			//LOG(TERR,"%d,%d = %d",t,tcou,val) ;
		}

		tcou++ ;



		
		// need to be even
		if(tcou & 1) {
			LOG(WARN,"tcou %d is odd, adding ped of tb %d?",tcou,t) ;
			*ptr++ = (u_short) ped->ped[0]; // was ped[t]; then ped[0]
			tcou++ ;
		}

		int aid = a ;

		*addr = (aid << 24) | (ch << 16) | tcou ;

		rbuff += 2 * tcou ;	// skip stored...
	}
	}



	LOG(NOTE,"Pedestals prepared for RDO %d, bytes %d",rb+1,rbuff-buff) ;
	return rbuff - buff ;	// bytes!
}

int stgcPed::to_evb(char *buff)
{
	return 0 ;
}

int stgcPed::from_cache(char *fname, u_int rb_msk) 
{
	FILE *f ;
	char fn[64]  ;
	const char *pn ;

	// trivial load from disk...
	if(fname) {
		pn = fname ;
	}
	else {
		pn = "/RTScache/pedestals" ;
	}


	int err = 0 ;

	memset(peds,0,sizeof(peds)) ;
	memset(altro_found,0,sizeof(altro_found)) ;

	for(int rdo=1;rdo<=6;rdo++) {
		if(rb_mask & (1<<(rdo-1))) ;
		else continue ;
		
		sprintf(fn,"%s_s%02d_r%d.txt",pn,sector,rdo) ;
		f = fopen(fn,"r") ;

		if(f==0) {
			LOG(ERR,"ped::from_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
			err++ ;
			continue ;
		}


		LOG(NOTE,"Loading pedestals from cache \"%s\"...",fn) ;


		while(!feof(f)) {
			u_int r, p , t ;
			float pp, rr ;
			char buff[64] ;
			int bad = 0 ;

			if(fgets(buff,sizeof(buff),f)==0) continue ;
			
			switch(buff[0]) {
			case '#' :
			case '/' :
				continue ;
			}

			int ret = sscanf(buff,"%d %d %d %f %f",&r,&p,&t,&pp,&rr) ;
			if(ret != 5) continue ;
			
			if(r>=256) bad = 1 ;
			if(p>=16) bad = 1 ;
			if(t>=512) bad = 1 ;

			if(bad) {
				LOG(ERR,"WHA %d %d",r,p) ;
				continue ;
			}

			altro_found[r]++ ;

			

			peds[r][p].ped[t] = pp ;
			peds[r][p].rms[t] = rr ;

			if(p==0 && t==20) LOG(TERR,"S%d:%d: %f %f",r,p,pp,rr) ;
		}

		fclose(f) ;
	}


	if(!err) {
		LOG(TERR,"Pedestals loaded from cache (last was \"%s\"): sector %2d [0x%02X].",fn,sector,rb_mask) ;
		valid = 1 ;
	}
	else {
		LOG(ERR,"Pedestals failed from cache (last was \"%s\"): sector %2d [0x%02X].",fn,sector,rb_mask) ;
		valid = 0 ;
	}




	return valid ;
}

int stgcPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	char fn[64] ;
	const char *pn ;
	char *asc_date ;


	if(!valid) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d -- not caching",valid) ;
		return -1 ;
	}


	time_t tm = time(0) ;
	asc_date = ctime(&tm) ;

	if(fname) {
		pn = fname ;
	}
	else {
		pn = "/RTScache/pedestals" ;
	}

	// changed to per-RDO on Jan 13, 2010.

	for(int rdo=1;rdo<=6;rdo++) {


		if(rb_mask & (1<<(rdo-1))) ;
		else continue ;

		
		// check if the RDO was present!
		if(valid_evts[rdo-1] < MIN_EVENTS) {
			LOG(ERR,"Sector %2d, RDO %d has %d events -- not caching!",sector,rdo,valid_evts[rdo-1]) ;
			continue ;
		}

		sprintf(fn,"%s_s%02d_r%d.txt",pn,sector,rdo) ;

		f = fopen(fn,"w") ;
		if(f==0) {
			LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
			continue ;
		}


		LOG(NOTE,"Writing pedestals to cache \"%s\"...",fn) ;


		fprintf(f,"# Detector %s\n","STGC") ;
		fprintf(f,"# Run %08u\n",run) ;
		fprintf(f,"# Date %s",asc_date) ;
		fprintf(f,"# Logical sector %d, logical RDO %d\n",sector,rdo) ;
		fprintf(f,"\n") ;

		for(int a=0;a<256;a++) {
		if(!altro_found[a]) continue ;

		for(int c=0;c<16;c++) {

			for(int t=0;t<512;t++) {	
				fprintf(f,"%d %d %d %.3f %.3f\n",a,c,t,peds[a][c].ped[t],peds[a][c].rms[t]) ;
			}
		}
		}

		fclose(f) ;	
	}

	LOG(TERR,"Pedestals written to cache \"%s\", for sector %2d...",fn,sector) ;

	return 1 ;
}



int stgcPed::kill_bad(int rdo0, int a, int ch)
{
	for(int t=0;t<512;t++) {
		peds[a][ch].ped[t] = 1023.0 ;
		peds[a][ch].rms[t] = 9.999 ;
	}

	return 1 ;
}


