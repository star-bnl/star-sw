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
	
	max_events = 1000 ;
	
	clock_source = 9 ; // non-existant
	sector = -1 ;	// uniti...

	memset(ped_rdo_store,0,sizeof(ped_rdo_store)) ;



	return ;
}


tpxPed::~tpxPed()
{
	tpxPed::clear() ;

	return ;
}


void tpxPed::clear()
{
	for(int r=0;r<6;r++) {
		if(ped_rdo_store[r].peds) {
			free(ped_rdo_store[r].peds) ;
			ped_rdo_store[r].peds = 0 ;
		}
	}

	return ;
}


// called at start of run...
void tpxPed::init(int sec, int active_rbs)
{

	smoothed = 0 ;
	valid = 0 ;

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;

	rb_mask = active_rbs ;
	sector = sec ;

	tpxPed::clear() ;	// zap storage ;

	for(int r=0;r<6;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		int s_real, r_real ;


		tpx36_to_real(sector,r+1,s_real,r_real) ;

		ped_rdo_store[r].r_real = r_real ;
		ped_rdo_store[r].s_real = s_real ;

		ped_rdo_store[r].peds = (struct peds *) malloc(sizeof(struct peds)*1152) ;
		memset(ped_rdo_store[r].peds,0,sizeof(struct peds)*1152) ;

		memset(ped_rdo_store[r].ix,0xFF,sizeof(ped_rdo_store[r].ix)) ;

		int cou = 0 ;
		for(int a=0;a<256;a++) {
			for(int c=0;c<16;c++) {
				int row, pad ;

				tpx_from_altro(r_real-1,a,c,row,pad) ;
				if(row==255) continue ;

				//LOG(TERR,"Real RDO %d: A%d:%d = RP %d:%d",r_real,a,c,row,pad) ;
				ped_rdo_store[r].peds[cou].row = row ;				
				ped_rdo_store[r].peds[cou].pad = pad ;
				ped_rdo_store[r].ix[row][pad] = cou ;
				cou++ ;
			}
		}

		LOG(TERR,"peds_inited: sector %d, RDO real %d: %d",s_real,r_real,cou) ;

	}

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
	int r0_logical ;

	t = tpx_get_start(evbuff, bytes/4, &rdo, 0) ;

	if(t <= 0) return ;	// non data event...



	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.t = t ;
	a.sector = rdo.sector ;
	a.log_err = 0 ;

	r0_logical = tpx36_from_real(sector,rdo.sector,rdo.rdo) - 1 ;
	
	evts[r0_logical]++ ;


	// skip first few events!
	if(evts[r0_logical] <= 3) {
		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo.rdo,evts[r0_logical]) ;
		return ;
	}

	// what to do with this!!!???
	if(tpx_rdo_dbg[a.sector-1][a.rdo].delta < 200000) {
		LOG(WARN,"RDO %d: skipping event %d: delta %u too small",rdo.rdo,evts[r0_logical],tpx_rdo_dbg[a.sector-1][a.rdo].delta) ;
		usleep(10000) ;
		return ;
	}

	valid_evts[r0_logical]++ ;

        LOG(NOTE,"RDO %d: event %d: delta %u OK",rdo.rdo,evts[r0_logical],tpx_rdo_dbg[a.sector-1][a.rdo].delta) ;

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
	int r0_logical ;

	r0_logical = tpx36_from_real(sector, a->sector, a->rdo+1) ;
	r0_logical-- ;	// need from 0 later on...

	row = a->row ;
	pad = a->pad ;

	if(tpx_is_stgc) {
		LOG(TERR,"RDO0 %d, RP %d:%d, AID %d:%d",r0_logical,row,pad,a->id,a->ch) ;
	}

	p = get(r0_logical,row, pad) ;

	if(p==0) {
		LOG(ERR,"ped::accum for row %d, pad %d, A %d:%d bad (real RDO %d)?",row,pad,a->id,a->ch,a->rdo+1) ;
		LOG(ERR,"Slog %d:%d, Shw %d:%d",sector,r0_logical+1,a->sector,a->rdo+1) ;
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
	int rl0 ;
	int bad ;


	LOG(NOTE,"Calculating pedestals for sector %2d",sector) ;

	bad = 0 ;

	for(rl0=0;rl0<6;rl0++) {
	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(rl0,r,p) ;
		if(ped==0) continue ;

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
	int s_real, r_real ;

	FILE *fff = 0 ;


#if 0
	char fname[128] ;
	sprintf(fname,"/RTScache/altro_ped_%d_%03d.txt",rb+1,timebins) ;

	fff = fopen(fname,"w") ;
	if(fff==0) {
		LOG(WARN,"Can't open \"%s\"",fname) ;
	}
#endif

	char *rbuff = buff ;

	if(!valid || !smoothed) {
		LOG(ERR,"ped::to_altro peds are bad: RDO %d: valid %d, smoothed %d",rb+1,valid,smoothed) ;
	}



	tpx36_to_real(sector,rb+1,s_real,r_real) ;

//	LOG(TERR,"Preparing pedestals for Slo%02d:%d (Shw%02d:%d)...",sector,rb+1,s_real,r_real) ;

	for(a=0;a<256;a++) {
	for(ch=0;ch<16;ch++) {

		tpx_from_altro(r_real-1,a,ch,row,pad) ;

		if(row > 45) continue ;	// not here...



		u_int *addr = (u_int *) rbuff ;	// remember where to store the address
	
		rbuff += 4 ;	// skip 4 bytes

		u_short *ptr = (u_short *) rbuff ;	// start

		int tcou = 0 ;	// zero counter...

		// get the corresponding row & pad

		struct peds *ped = get(rb,row,pad) ;
		if(ped==0) {
			LOG(ERR,"RDO %d (real %d): row %d, pad %d",rb+1,r_real,row,pad) ;
			continue ;
		}

		for(t=0;t<timebins+15;t++) {
			if(fff) fprintf(fff,"%d %d %d %d %d %d\n",row,pad,a,ch,t,(u_short)ped->ped[t]) ;
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



#if 0
		// THIS is how it was done pre-FY11
		// follow with pre-trigger pedestals
		for(t=0;t<15;t++) {
			*ptr++ = (u_short) ped->ped[t] ;
			tcou++ ;
		}
#endif

#if 0
		// testing....
		// follow with 510
//		for(t=0;t<20;t++) {
//			*ptr++ = t ;
//			tcou++ ;
//		}

		*ptr++ = 5 ;
		tcou++ ;
		*ptr++ = 10 ;
		tcou++ ;
		*ptr++ = 15 ;
		tcou++ ;
		*ptr++ = 20 ;
		tcou++ ;
#endif
		
		// need to be even
		if(tcou & 1) {
			LOG(WARN,"tcou %d is odd, adding ped of tb %d?",tcou,t) ;
			*ptr++ = (u_short) ped->ped[0]; // was ped[t]; then ped[0]
			tcou++ ;
		}

		int aid = a ;
		for(int i=0;i<tpx_fee_override_cou;i++) {
			if(s_real == tpx_fee_override[i].sector) {
			if(r_real == (tpx_fee_override[i].rdo)) {
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
	sfs_index sfs ;

	char *rbuff = buff ;	// remember

	if(!valid || !smoothed) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d, smoothed %d",valid,smoothed) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	for(int rl0=0;rl0<4;rl0++) {

	char sname[32] ;
	int s_real, r_real ;
	tpx36_to_real(sector,rl0+1,s_real,r_real) ;

	sprintf(sname,"sec%02d/rb%02d/pedrms",s_real,r_real) ;
	char *save_rbuff = rbuff ;
	int h_bytes = sfs.putfileheader(save_rbuff,sname,0) ;

	rbuff += h_bytes ;

	u_short *addr = (u_short *) rbuff ;

	for(r=0;r<=45;r++) {
		for(p=1;p<=tpc_rowlen[r];p++) {
			struct peds *ped = get(rl0,r, p) ;
			if(ped==0) continue ;

			addr = (u_short *) rbuff ;	// remember address
			*(addr+1) = (r<<8) | p ;	// row/pad
			
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
	sfs.putfileheader(save_rbuff,sname,rbuff-save_rbuff-h_bytes) ;

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
		
		int s_real, r_real ;

		tpx36_to_real(sector,rdo,s_real,r_real) ;

		sprintf(fn,"%s_s%02d_r%d.txt",pn,s_real,r_real) ;
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
			char buff[64] ;

			if(fgets(buff,sizeof(buff),f)==0) continue ;
			
			switch(buff[0]) {
			case '#' :
			case '/' :
				continue ;
			}

			int ret = sscanf(buff,"%d %d %d %f %f",&r,&p,&t,&pp,&rr) ;
			if(ret != 5) continue ;

			struct peds *peds = get(rdo-1,r,p) ;

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
//	char f_sum_name[128] ;
	char *pn ;
	char *asc_date ;

	static float old_sum[46][183] ;


	if(!valid || smoothed) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d, smoothed %d -- not caching",valid,smoothed) ;
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
		int s_real, r_real ;

		if(rb_mask & (1<<(rdo-1))) ;
		else continue ;

		// need real sector and real RDO here!!!
		tpx36_to_real(sector,rdo,s_real,r_real) ;
		
		// check if the RDO was present!
		if(valid_evts[rdo-1] < MIN_EVENTS) {
			LOG(ERR,"Sector %2d, RDO %d has %d events -- not caching!",sector,rdo,valid_evts[rdo-1]) ;
			continue ;
		}

		sprintf(fn,"%s_s%02d_r%d.txt",pn,s_real,r_real) ;

		// first read old peds...

		memset(old_sum,0,sizeof(old_sum)) ;

		f = fopen(fn,"r") ;
		if(f==0) {
			LOG(ERR,"ped::to_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
		}
		else {

			while(!feof(f)) {
				float fped, frms ;
				char buff[64] ;

				if(fgets(buff,sizeof(buff),f)==0) continue ;

				switch(buff[0]) {
				case '#' :
				case '/' :
					continue ;
				}

				int ret = sscanf(buff,"%d %d %d %f %f",&r,&p,&t,&fped,&frms) ;
				if(ret != 5) continue ;

				if(t < 22) {
					old_sum[r][p] += fped ;
				}
			}

			for(r=0;r<=45;r++) {
			for(p=1;p<=tpc_rowlen[r];p++) {
				old_sum[r][p] /= 22.0 ;
			}
			}

			fclose(f) ;
		}


		f = fopen(fn,"w") ;
		if(f==0) {
			LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
			continue ;
		}

		f_sum = 0 ;
#if 0
		if(run==0) {
			sprintf(f_sum_name,"/RTScache/ped_sum_s%02d_r%d_%u_%d.txt",s_real,r_real,(u_int)time(NULL),clock_source) ;
		}
		else {
			sprintf(f_sum_name,"/RTScache/ped_sum_s%02d_r%d_%08u_%d.txt",s_real,r_real,run,clock_source) ;
		}

		f_sum = fopen(f_sum_name,"w") ;
		if(f_sum==0) {
			LOG(ERR,"ped::to_cache can't open trace file \"%s\" [%s]",f_sum_name,strerror(errno)) ;
		}

#endif

		LOG(NOTE,"Writing pedestals to cache \"%s\"...",fn) ;


		fprintf(f,"# Detector %s\n","TPX") ;
		fprintf(f,"# Run %08u\n",run) ;
		fprintf(f,"# Date %s",asc_date) ;
		fprintf(f,"# Logical sector %d, logical RDO %d\n",sector,rdo) ;
		fprintf(f,"# Hardware sector %d, hardware RDO %d\n",s_real, r_real) ;
		fprintf(f,"\n") ;


		for(r=0;r<=45;r++) {

		// ONLY from 1 to rowlen!
		for(p=1;p<=tpc_rowlen[r];p++) {
			int t_rdo, t_a, t_ch ;

			tpx_to_altro(r,p,t_rdo,t_a,t_ch) ;
			if(t_rdo != r_real) {
				//LOG(WARN,"RDO %d, real %d: RP %d:%d, AID %d:%d",t_rdo,r_real,r,p,t_a,t_ch) ;
				continue ;
			}

			struct peds *peds = get(rdo-1,r, p) ;
			if(peds == 0) {
				LOG(WARN,"Shouldnt %d %d %d %d",r_real,t_rdo,r,p) ;
				continue ;
			}

			double sum = 0.0 ;
			int cou = 0 ;

			for(t=0;t<22;t++) {
				sum += peds->ped[t] ;
				cou++ ;
			}

			sum /= (double)cou ;

			double p_diff = sum - old_sum[r][p] ;
			if(fabs(p_diff)>1.0) {
				LOG(WARN,"RDO %d (S%02d:%d): ped_compare r:p %d:%d = %.1f",rdo,s_real,r_real,r,p,p_diff) ;
			}


			if(f_sum) fprintf(f_sum,"%d %d %.5f\n",r,p,sum) ;

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

int tpxPed::hlt_debug_setup(int param)
{
	int delta_tb = param % 100 ;
	int delta_pad = param / 100 ;
	int hits ;

	if(delta_tb < 6) delta_tb = 6 ;
	if(delta_pad < 3) delta_pad = 3 ;

	hits = 0 ;

	for(int rl0=0;rl0<4;rl0++) {

	for(int r=1;r<=45;r++) {

	for(int p=3;p<=(tpc_rowlen[r]-delta_pad-2);p+=delta_pad) {

		for(int pd=0;pd<2;pd++) {
			struct peds *ped = get(rl0,r,p+pd) ;
			if(ped==0) continue ;

			for(int t=15;t<400;t+=delta_tb) {


				hits++ ;

				for(int td=0;td<5;td++) {
					double val = ped->ped[t+td] ;
					// lower the pedestal 5 ADC counts
					if(val < 10.0) val = 0.0 ;
					else val -= 10.0 ;

					ped->ped[t+td] = val ;
				}
			}
		}
	}
	}
	}

	LOG(TERR,"param %u: delta pad %d,time %d: %d hits",param,delta_pad,delta_tb,hits/2) ;

	valid = 1 ;
	smoothed = 1 ;

	return 1 ;
}

int tpxPed::special_setup(int run_type, int sub_type)
{
	int r, p, t ;
//	int m ;

	switch(run_type) {
	case RUN_TYPE_PULSER_A :
	case RUN_TYPE_PULSER :
	case RUN_TYPE_PED_A :
	case RUN_TYPE_PED_B :

		break ;
	case RUN_TYPE_HLT_DEBUG :
		LOG(WARN,"Special Pedestal setup: %d, %d",run_type, sub_type) ;
		hlt_debug_setup(sub_type) ;
		return 1 ; 
	case RUN_TYPE_LASER :
		if(sub_type) {
			break ;
		}
		return 1 ;
	default :
		if(sub_type) {
			break ;

		}
		return 1 ;
	}


	LOG(ERR,"Special Pedestal setup: %d, %d (just a warning)",run_type, sub_type) ;

	for(int rl0=0;rl0<4;rl0++) {
	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(rl0,r,p) ;
		if(ped==0) continue ;
		
		switch(run_type) {
		case RUN_TYPE_PHYS :	//occupancy increase; used e.g. in noise-calibration runs hot_channels
			for(t=100;t<(100+sub_type);t++) {
				ped->ped[t] -= 10 ;
			}
			break ;
		case RUN_TYPE_PULSER_A :
			// make it about 5% occupancy
			for(t=100;t<110;t++) ped->ped[t] = 0.0 ;
			//for(t=200;t<220;t++) ped->ped[t] = 0.0 ;
			for(t=400;t<415;t++) ped->ped[t] = 0.0 ;
			break ;
		case RUN_TYPE_PULSER :
			for(t=TPX_PULSER_PED_START;t<=TPX_PULSER_PED_STOP;t++) ped->ped[t] = 0.0 ;
			break ;
		case RUN_TYPE_HLT_DEBUG :
			
			break ;
		case RUN_TYPE_PED_A :	// starts with ped=0
/*
			m = 0 ;			
			for(t=0;t<512;) {
				for(int i=0;i<16;i++) {
					ped->ped[t+i] = m * 1023.0 ;
				}
				if(m==0) m = 1 ;
				else m = 0 ;
				t += 16 ;
			}
*/

			
			for(t=0;t<512;t++) {
				ped->ped[t] = 0 ;
			}

			//for(t=0;t<20;t++) ped->ped[t] = 10 ;
			for(t=390;t<512;t++) ped->ped[t] = 20 ;

			break ;
		case RUN_TYPE_PED_B :	// starts with ped=1


/* don't know what this was...
			m = 1 ;			
			for(t=0;t<512;) {
				for(int i=0;i<16;i++) {
					ped->ped[t+i] = m * 1023.0 ;
				}
				if(m==0) m = 1 ;
				else m = 0 ;
				t += 16 ;
			}
*/

			// use the first timebin for all...
			for(t=1;t<512;t++) ped->ped[t] = ped->ped[0] ;

			break ;
		case RUN_TYPE_LASER :
			for(t=368;t<=383;t++) ped->ped[t] = 0.0 ;
			break ;

		default :	// some pattern
			for(t=0;t<512;t++) ped->ped[t] = 1023.0 ;	// kill all
			for(t=p;t<(p+10);t++) ped->ped[t] = 0 ;		// some pattern depending on row
			break ;	
		}

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

	#define TPX_GG_START		20	// depends on TCD!
	#define TPX_START_OF_RIPPLE	32	// doesn't depend on TCD
	#define TPX_START_CORRECTION	34
	#define TPX_USE_DATA		298
	#define TPX_GG_DOWN		420


	LOG(NOTE,"Smoothing pedestals...") ;

	for(int rl0=0;rl0<4;rl0++) {
	for(r=0;r<=45;r++) {
	for(p=0;p<=182;p++) {
		struct peds *ped = get(rl0,r,p) ;
		if(ped==0) continue ;

		double smoother[513] ;
		double ripple[24] ;


		// this gets funky...
		for(t=0;t<512;t++) {
			smoother[t] = ped->ped[t] ;
		}
		
		/******  time before GG _and_ ripple -- flat! */
		mean = 0.0 ;
		cou = 0 ;

		for(t=0;t<TPX_GG_START;t++) {	// before GG & ripple
			mean += smoother[t] ;
			cou++ ;
		}
		mean /= (double)cou ;

		for(t=0;t<TPX_GG_START;t++) {
			smoother[t] = mean ;
		}


		// now get the ripple at some nice point
		mean = 0.0 ;
		cou = 0 ;

		for(t=TPX_USE_DATA;t<(TPX_USE_DATA+8);t++) {
			mean += smoother[t] ;
			cou++ ;
		}

		mean /= (double) cou ;

		// calculate the ripple
		for(t=TPX_USE_DATA;t<(TPX_USE_DATA+8);t++) {
			ripple[t-TPX_USE_DATA] = smoother[t] - mean ;
		}
		
		// special tweak!
		smoother[TPX_START_OF_RIPPLE] -= ripple[2] * 0.6 ;

		//apply ripple correction
		for(t=TPX_START_CORRECTION;t<512;t+=8) {
			for(int i=0;i<8;i++) {
				if((t+i)>434) continue ;	// skip
				smoother[t+i] -= ripple[i] ;
			}
		}

		
		// finally, we need to round off correctly!
		for(t=0;t<512;t++) {
			ped->ped[t] = (double) ((u_short) (smoother[t]+0.5)) ;	
		}




	}
	}
	}

	LOG(TERR,"Pedestals smoothed: sector %2d",sector) ;
	smoothed = 1 ;

	return ;
}


int tpxPed::kill_bad(int r0_logical,int row, int pad)
{
	struct peds *p ;

	p = get(r0_logical,row, pad) ;
	if(p==0) return 0 ;

	for(int t=0;t<512;t++) {
		p->ped[t] = 1023.0 ;
		p->rms[t] = 9.999 ;
	}

	return 1 ;
}


