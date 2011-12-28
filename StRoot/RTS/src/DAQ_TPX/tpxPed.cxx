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

#include "tpxCore.h"
#include "tpxPed.h"
#include "tpxGain.h"

static	const u_int MIN_EVENTS = 500 ;
#define TPX_PED_FILENAME	"/RTScache/pedestals"

tpxPed::tpxPed()
{
	smoothed = 0 ;
	valid = 0 ;
	rb_mask = 0x3F ;	// assume all..

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	
	sizeof_ped = sizeof(struct peds) * 46 * 183 ;

	ped_store = 0 ;	// unassigned!

	max_events = 1000 ;
	
	clock_source = 9 ; // non-existant
	sector = -1 ;	// uniti...
	return ;
}


tpxPed::~tpxPed()
{
	if(ped_store) free(ped_store) ;

	return ;
}

#if 0
tpxPed::peds *tpxPed::get(int row, int pad)
{
	return (ped_store + row*183 + pad) ;
}
#endif

void tpxPed::init(int active_rbs)
{
	smoothed = 0 ;
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

/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition
*/
void tpxPed::accum(char *evbuff, int bytes)
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
	if(tpx_rdo_dbg[a.rdo].delta < 200000) {
		LOG(WARN,"RDO %d: skipping event %d: delta %u too small",rdo.rdo,evts[a.rdo],tpx_rdo_dbg[a.rdo].delta) ;
		usleep(10000) ;
		return ;
	}

	valid_evts[a.rdo]++ ;

        LOG(NOTE,"RDO %d: event %d: delta %u OK",rdo.rdo,evts[a.rdo],tpx_rdo_dbg[a.rdo].delta) ;

	data_end = rdo.data_end ;

	do {
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;
		accum(&a) ;
	} while(data_end && (data_end > rdo.data_start)) ;


	return ;

}

void tpxPed::accum(tpx_altro_struct *a)
{
	int i ;
	int row, pad ;
	struct peds *p ;



	row = a->row ;
	pad = a->pad ;



	p = get(row, pad) ;
	if(p==0) {
		LOG(ERR,"ped::accum for row %d, pad %d, A %d:%d bad?",row,pad,a->id,a->ch) ;
		return ;
	}

	// do not allow more than 1000 events; use tb[20]'s counter...
	if(p->cou[20] > max_events) return ;
	
	
	LOG(DBG,"count %d",a->count) ;
	for(i=0;i<a->count;i++) {
		int tb, adc ;

		adc = a->adc[i] ;
		tb = a->tb[i] ;

		p->ped[tb] += (double) adc ;
		p->rms[tb] += (double) (adc*adc) ;
		p->cou[tb]++ ;
	}


	return ;
}

void tpxPed::calc()
{
	int r,p,t ;
	int bad ;


	LOG(NOTE,"Calculating pedestals for sector %2d",sector) ;

	bad = 0 ;

	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(r,p) ;

		for(t=0;t<512;t++) {
			if(ped->cou[t] == 0) {
				ped->ped[t] = 1023.0 ;
				ped->rms[t] = 9.999 ;
			}
			else {
				double pp, rr ;

				pp = ped->ped[t] / (double) ped->cou[t] ;
				rr = ped->rms[t] / (double) ped->cou[t] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[t] = pp ;
				ped->rms[t] = rr ;
			}
		}
	}
	}

	for(r=0;r<6;r++) {
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
int tpxPed::to_altro(char *buff, int rb, int timebins)
{
	int row, pad, t ;
	int a, ch ;

	FILE *fff = 0 ;
	char fname[128] ;

	sprintf(fname,"/RTScache/altro_ped_%d_%03d.txt",rb+1,timebins) ;

//	fff = fopen(fname,"w") ;
//	if(fff==0) {
//		LOG(WARN,"Can't open \"%s\"",fname) ;
//	}

	char *rbuff = buff ;

	if(!valid || !smoothed) {
		LOG(ERR,"ped::to_altro peds are bad: RDO %d: valid %d, smoothed %d",rb+1,valid,smoothed) ;
	}

	LOG(NOTE,"Preparing pedestals for RDO %d...",rb) ;

	for(a=0;a<256;a++) {
	for(ch=0;ch<16;ch++) {

		tpx_from_altro(rb,a,ch,row,pad) ;

		if(row > 45) continue ;	// not here...



		u_int *addr = (u_int *) rbuff ;	// remember where to store the address
	
		rbuff += 4 ;	// skip 4 bytes

		u_short *ptr = (u_short *) rbuff ;	// start

		int tcou = 0 ;	// zero counter...

		// get the corresponding row & pad

		struct peds *ped = get(row,pad) ;


		for(t=0;t<timebins+15;t++) {
			if(fff) fprintf(fff,"%3d %2d %3d %3d\n",a,ch,t,(u_short)ped->ped[t]) ;
		}
#if 0
		// copy as shorts BUT:
		//	needs to go from 15 _AND_ needs to be even!
		for(t=15;t<509;t++) {
			*ptr++ = (u_short) ped->ped[t] ;
			tcou++ ;
		}

		if(tcou & 1) {
			*ptr++ = (u_short) ped->ped[t] ;
			tcou++ ;
		}
#endif

		// pedestal memory for the altro is really odd

		// first should be the pedestals from the start
		// of trigger...
		for(t=15;t<timebins+15;t++) {
			*ptr++ = (u_short) ped->ped[t] ;
			tcou++ ;
		}

		// follow with pre-trigger pedestals
		for(t=0;t<15;t++) {
			*ptr++ = (u_short) ped->ped[t] ;
			tcou++ ;
		}

		// need to be even
		if(tcou & 1) {
			//LOG(WARN,"tcou %d is odd, adding ped of tb %d?",tcou,t) ;
			*ptr++ = (u_short) ped->ped[0]; // was ped[t]; then ped[0]
			tcou++ ;
		}

		int aid = a ;
		for(int i=0;i<tpx_fee_override_cou;i++) {
			if(sector == tpx_fee_override[i].sector) {
			if(rb == (tpx_fee_override[i].rdo-1)) {
			int fee = a & 0xFE ;
			if(fee == tpx_fee_override[i].orig_altro) {
				
				if(a & 1) {
					aid = tpx_fee_override[i].curr_altro | 1 ;

				}
				else {
					aid = tpx_fee_override[i].curr_altro ;
				}

				LOG(NOTE,"Sector %2d, RDO %d: overriding ALTRO from %3d to %3d",sector,rb+1,a,aid) ;
			}
			}
			}
		}

		*addr = (aid << 24) | (ch << 16) | tcou ;

		rbuff += 2 * tcou ;	// skip stored...
	}
	}

	if(fff) fclose(fff) ;

	LOG(NOTE,"Pedestals prepared for RDO %d, bytes %d",rb+1,rbuff-buff) ;
	return rbuff - buff ;	// bytes!
}

int tpxPed::to_evb(char *buff)
{
	int r, p, t ;

	char *rbuff = buff ;	// remember

	if(!valid || !smoothed) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d, smoothed %d",valid,smoothed) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	for(r=0;r<=45;r++) {
		for(p=1;p<=tpc_rowlen[r];p++) {
			struct peds *ped = get(r, p) ;

			u_short *addr = (u_short *) rbuff ;	// remember address
			*(addr+1) = (r<<8)|p ;	// row/pad
			
			u_short *ptr = addr + 2;	// read to store

			for(t=0;t<512;t++) {
				double rms = (ped->rms[t] * 16.0) ;
				u_short val ;

				if((u_short)rms > 0x3F) val = 0x3F ;
				else val = (u_short) rms ;
				
				// sanity check!
				if(ped->ped[t] == 0) {
					LOG(WARN,"WTF? ped 0 in rp %d:%d, tb %d",r,p,t) ;
				}

				*ptr++ = (val << 10) | (u_short)ped->ped[t] ;
			}

			*addr = t ;	// unsert count at the first short
			rbuff = (char *) ptr ;
		}
	}
	// short cou
	// short row|pad
	// short: 6bit RMS, 10bit ped

	LOG(TERR,"Pedestals prepared for later EVB, sector %2d: %d bytes",sector,rbuff-buff) ;
	return (rbuff-buff) ;
}

int tpxPed::from_cache(char *fname, u_int rb_msk) 
{
	FILE *f ;
	char fn[64]  ;
	char *pn ;

	init(rb_msk) ;	// to clear
	
	// trivial load from disk...
	if(fname) {
		pn = fname ;
	}
	else {
		pn = "/RTScache/pedestals" ;
	}


	int err = 0 ;

	for(int rdo=1;rdo<=6;rdo++) {
		if(rb_mask & (1<<(rdo-1))) ;
		else continue ;
		
		sprintf(fn,"%s_r%d.txt",pn,rdo) ;
		f = fopen(fn,"r") ;

		if(f==0) {
			LOG(ERR,"ped::from_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
			err++ ;
			continue ;
		}


		LOG(NOTE,"Loading pedestals from cache \"%s\"...",fn) ;

		while(!feof(f)) {
			int r, p , t ;
			float pp, rr ;

			int ret = fscanf(f,"%d %d %d %f %f",&r,&p,&t,&pp,&rr) ;
			if(ret != 5) continue ;

			struct peds *peds = get(r,p) ;

			//if((r==12) && (p==158) && (t==0)) LOG(TERR,"peds row %d, pad %d: %f %f",r,p,pp,rr) ;

			peds->ped[t] = pp ;
			peds->rms[t] = rr ;
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

	smoothed = 0 ;


	return valid ;
}

int tpxPed::to_cache(char *fname, u_int run)
{
	FILE *f, *f_sum ;
	int r, p, t ;
	char fn[64] ;
	char f_sum_name[128] ;
	char *pn ;

	if(!valid || smoothed) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d, smoothed %d -- not caching",valid,smoothed) ;
		return -1 ;
	}

	if(fname) {
		pn = fname ;
	}
	else {
		pn = "/RTScache/pedestals" ;
	}

	// changed to per-RDO on Jan 13, 2010.

	for(int rdo=1;rdo<=6;rdo++) {

		// check if the RDO was present!
		if(valid_evts[rdo-1] < MIN_EVENTS) {
			LOG(WARN,"Sector %2d, RDO %d has %d events -- not caching!",sector,rdo,valid_evts[rdo-1]) ;
			continue ;
		}

		sprintf(fn,"%s_r%d.txt",pn,rdo) ;

		

		f = fopen(fn,"w") ;
		if(f==0) {
			LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
			continue ;
		}

		if(run==0) {
			sprintf(f_sum_name,"/RTScache/ped_sum_s%02d_r%d_%u_%d.txt",sector,rdo,(u_int)time(NULL),clock_source) ;
		}
		else {
			sprintf(f_sum_name,"/RTScache/ped_sum_s%02d_r%d_%08u_%d.txt",sector,rdo,run,clock_source) ;
		}

		f_sum = fopen(f_sum_name,"w") ;
		if(f_sum==0) {
			LOG(ERR,"ped::to_cache can't open trace file \"%s\" [%s]",f_sum_name,strerror(errno)) ;
		}


		LOG(NOTE,"Writing pedestals to cache \"%s\"...",fn) ;

		for(r=0;r<=45;r++) {

		// ONLY from 1 to rowlen!
		for(p=1;p<=tpc_rowlen[r];p++) {
			int t_rdo, t_a, t_ch ;

			tpx_to_altro(r,p,t_rdo,t_a,t_ch) ;
			if(t_rdo != rdo) continue ;

			struct peds *peds = get(r, p) ;

			double sum = 0.0 ;
			int cou = 0 ;

			for(t=0;t<22;t++) {
				sum += peds->ped[t] ;
				cou++ ;
			}

			if(f_sum) fprintf(f_sum,"%d %d %.5f\n",r,p,sum/(double)cou) ;

			for(t=0;t<512;t++) {	
			
				//if((r==12) && (p==158) && (t==0)) LOG(TERR,"peds row %d, pad %d: %f %f",r,p,peds->ped[t],peds->rms[t]) ;
				fprintf(f,"%d %d %d %.3f %.3f\n",r,p,t,peds->ped[t],peds->rms[t]) ;
			}
		}
		}

		fclose(f) ;	
		if(f_sum) fclose(f_sum) ;
	}

	LOG(TERR,"Pedestals written to cache \"%s\", for sector %2d...",fn,sector) ;

	return 1 ;
}

int tpxPed::special_setup(int run_type, int sub_type)
{
	int r, p, t ;
	int m ;

	switch(run_type) {
	case RUN_TYPE_PULSER_A :
	case RUN_TYPE_PULSER :
	case RUN_TYPE_PED_A :
	case RUN_TYPE_PED_B :
		LOG(WARN,"Special Pedestal setup: %d, %d",run_type, sub_type) ;
		break ;
	default :
		return 1 ;
	}

	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(r,p) ;

		
		switch(run_type) {
		case RUN_TYPE_PULSER_A :
			// make it about 5% occupancy
			for(t=100;t<110;t++) ped->ped[t] = 0.0 ;
			//for(t=200;t<220;t++) ped->ped[t] = 0.0 ;
			for(t=400;t<415;t++) ped->ped[t] = 0.0 ;
			break ;
		case RUN_TYPE_PULSER :
			for(t=TPX_PULSER_PED_START;t<=TPX_PULSER_PED_STOP;t++) ped->ped[t] = 0.0 ;
			break ;
		case RUN_TYPE_PED_A :	// starts with ped=0
			m = 0 ;			
			for(t=0;t<512;) {
				for(int i=0;i<16;i++) {
					ped->ped[t+i] = m * 1023.0 ;
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
					ped->ped[t+i] = m * 1023.0 ;
				}
				if(m==0) m = 1 ;
				else m = 0 ;
				t += 16 ;
			}
			break ;
		default :	// some pattern
			for(t=0;t<512;t++) ped->ped[t] = 1023.0 ;	// kill all
			for(t=p;t<(p+10);t++) ped->ped[t] = 0 ;		// some pattern depending on row
			break ;	
		}

	}
	}	

	valid = 1 ;
	smoothed = 1 ;

	return 1 ;
}


void tpxPed::smooth()
{
	int r, p, t ;

	double mean ;
	int cou ;

	if(smoothed || !valid) {
		LOG(ERR,"ped::smooth sector %2d invalid: smoothed %d, valid %d",sector,smoothed,valid) ;
		return ;
	}

#define TPX_GG_START	22
#define TPX_START_OF_RIPPLE	32
#define TPX_GG_STOP	51	// was 32 before Feb 20, 2008!
#define TPX_CLEAN_SLATE 56

	LOG(NOTE,"Smoothing pedestals...") ;

	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(r,p) ;
		double smoother[513] ;
		double ripple[513] ;

#ifdef TPX_PED_CONSTANT
		mean = 0.0 ;
		cou = 0 ;

		for(t=0;t<30;t++) {
			mean += ped->ped[t] ;
			cou++ ;
		}

		mean /= (double)cou ;

		// we need to round off correctly
		for(t=0;t<512;t++) ped->ped[t] = (float) ((u_short) (mean+0.5)) ;	

#else		// for GG

		// this gets funky...

		/******  time before GG _and_ ripple -- flat! */

		mean = 0.0 ;
		cou = 0 ;

		for(t=0;t<22;t++) {	// before GG & ripple
			mean += ped->ped[t] ;
			cou++ ;
		}
		mean /= (double)cou ;


		// subtract this mean from all timebins to get the ripple...
		for(t=0;t<512;t++) {
			ripple[t] = ped->ped[t] - mean ;	// difference from the mean...
		}	

		// before the GG and ripple we use the simple mean
		for(t=0;t<22;t++) {
			smoother[t] = mean ;
		}

		// GG on, but no ripple yet: we use what was calculated;
		for(t=22;t<32;t++) {
			smoother[t] = ped->ped[t] ;	// keep the same ....
		}

		// for the first 2 timebins we subtract the ripple from some close
		// timebins but which are after the GG. Scaled down a bit.
		for(t=32;t<34;t++) {
			smoother[t] = ped->ped[t] - 0.6 *ripple[t+24] ;
		}

		// scale is now 1
		for(t=34;t<56;t++) {
			smoother[t] = ped->ped[t] - ripple[t+24] ;
		}

		/****** after this we do 8 pixel smoothing */
		for(t=56;t<512;t++) {
			mean = 0.0 ;
			cou = 0 ;
			for(int j=0;j<8;j++) {
				if(((t+j)<512) && (ped->ped[t+j]<1000.0)) {
					mean += ped->ped[t+j] ;
					cou++ ;
				}
			}

			if(cou) {
				mean /= (double) cou ;
			}
			else {
				mean = ped->ped[t] ;
			}

			smoother[t] = mean ;
		}

		
		// we need to round off correctly!
		for(t=0;t<512;t++) {
			ped->ped[t] = (double) ((u_short) (smoother[t]+0.5)) ;	
		}


#endif

	}
	}

	LOG(TERR,"Pedestals smoothed: sector %2d",sector) ;
	smoothed = 1 ;

	return ;
}


void tpxPed::kill_bad(int row, int pad)
{
	struct peds *p ;
	int t ;

	p = get(row, pad) ;

	for(t=0;t<512;t++) {
		p->ped[t] = 1023.0 ;
		p->rms[t] = 9.999 ;
	}

	return ;
}

// what's this???
int tpxPed::from_evb(char *buff, int bytes) 
{
	init(0x3F) ;	// zap struct
	valid = 0 ;
	smoothed = 0 ;



	return 0 ;
}

// unused....
int tpxPed::summarize(FILE *log)
{
	int notes = 0 ;


	return notes ;
}

