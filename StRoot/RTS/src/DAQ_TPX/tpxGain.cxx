#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

#include <rtsLog.h>
#include <TPC/rowlen.h>

#include <TPX/tpx_altro_to_pad.h>

#include "tpxCore.h"
#include "tpxGain.h"

#define TPX_NEG_TB	2
#define TPX_POS_TB	5




struct tpx_odd_fee_t tpx_odd_fee[256] ;
int tpx_odd_fee_count = 0 ;

tpxGain::tpxGain()
{
	events = 0 ;
	load_time = 0 ;
	sector = 0 ;	// assume all...

	// mark memory as cleared
	aux = 0 ;
	means = 0 ;
	fee_found = 0 ;
	memset(gains,0,sizeof(gains)) ;
	
	// bad_fee's are sticky and can only be entered via a file!
	memset(bad_fee,0,sizeof(bad_fee)) ;
	tpx_odd_fee_count = 0 ;

	memset(bad_rdo_mask,0,sizeof(bad_rdo_mask)) ;
	return ;
}

tpxGain::~tpxGain()
{
	free_store() ;

	return ;
}

void tpxGain::free_store()
{
	if(aux) {
		free(aux) ;
		aux = 0 ;
	}

	if(means) {
		free(means) ;
		means = 0 ;
	}

	if(fee_found) {
		free(fee_found) ;
		fee_found = 0 ;
	}

	for(int i=0;i<24;i++) {
		if(gains[i]) {
			free(gains[i]) ;
			gains[i] = 0 ;
		}
	}

	return ;
}

/*
	Initialize calculation at beginning of a run...
*/
void tpxGain::init(int sec)
{
	int bytes ;

	events = 0 ;
	load_time = 0 ;	// force reload next time if I reinit gains!
	sector = sec ;

	

	bytes = sizeof(struct means) * 24 * 46 ;
	if(means==0) {
		means = (struct means *) malloc(bytes) ;
	}
	memset(means,0,bytes) ;

	bytes = sizeof(struct aux) * 24 * 46 * 182 ;
	if(aux==0) {
		aux = (struct aux *) malloc(bytes) ;
	}
	memset(aux,0,bytes) ;

	bytes = sizeof(struct fee_found_t) * 25 * 7 ;
	if(fee_found == 0) {
		fee_found = (struct fee_found_t *) malloc(bytes) ;
		
	}
	memset(fee_found,0,bytes) ;

	for(int i=0;i<24;i++) {
		bytes = sizeof(struct gains) * 46 * 182 ;
		if(gains[i]==0) {
			gains[i] = (struct gains *) malloc(bytes) ;
		}
		memset(gains[i],0,bytes) ;
	}


	memset(tpx_pulser_peak,0,sizeof(tpx_pulser_peak))  ;

//	tb_start = 181 ;
//	tb_stop = 190 ;

	tb_start = 179 ;
	tb_stop = 188 ;

	memset(bad_rdo_mask,0,sizeof(bad_rdo_mask)) ;

	return ;
}


/*
	Called at end of event
*/
void tpxGain::ev_done() 
{
	
#ifdef BLOODY_BAD
	if(events==0) {	// this is called after the first, special, event...
		int tpx_pulser_tb[25][46] ;

		int s, r, p ;

		int s_start, s_stop ;
		if(sector) {
			s_start = s_stop = sector ;
		}
		else {
			s_start = 1 ;
			s_stop = 24 ;
		}

		// for the whole TPX!
		int histo_tb[512] ;
		memset(histo_tb,0,sizeof(histo_tb)) ;

		memset(tpx_pulser_tb,0,sizeof(tpx_pulser_tb))  ;

		char fname[64] ;
		sprintf(fname,"/RTS/log/tpx/tb_distrib_%02d.txt",sector) ;
		FILE *tf = fopen(fname,"w") ;


		for(s=s_start;s<=s_stop;s++) {
		for(r=1;r<=45;r++) {			// skip non-connected pads

		// for this particular row!
		int histo_charge[1024] ;
		int histo_tb_a[512] ;

		memset(histo_charge,0,sizeof(histo_charge)) ;
		memset(histo_tb_a,0,sizeof(histo_tb_a)) ;

		for(p=4;p<=(tpc_rowlen[r]-3);p++) {	// skip first 3, last 3 pads in a row...
			int tb = (int) get_gains(s,r,p)->t0 ;	// tb of the maximum peak
			int g = (int) get_gains(s,r,p)->g ;	// charge of the maximum peak

			if(g) {
				histo_tb_a[tb]++ ;
			
				if(tf) fprintf(tf,"S %2d, ROW %2d, PAD %3d: tb %4d, peak %4d\n",s,r,p,tb,g) ;

				histo_tb[tb]++ ;	// histogram the timebins...
				histo_charge[g]++ ;	// histogram the max peak...
			}
		}

		// get the most probable peak charge for this row
		int prob_peak = 0 ;
		int prob_max = 0 ;

		for(int i=0;i<1024;i++) {
			if(histo_charge[i] > prob_max) {
				prob_max = histo_charge[i] ;
				prob_peak = i ;
			}
		}

		tpx_pulser_peak[s][r] = prob_peak ;

		prob_peak = 0 ;
		prob_max = 0 ;

		for(int i=0;i<512;i++) {
			if(histo_tb_a[i] > prob_max) {
				prob_max = histo_tb_a[i] ;
				prob_peak = i ;
			}
		}

		tpx_pulser_tb[s][r] = prob_peak ;




		}	// row
		}	// sector...


		// find the maximum of the timebin peak
		int mean_tb = 0 ;
		int max_val = 0 ;


		for(int i=0;i<512;i++) {
			if(tf) fprintf(tf,"TB %4d %d\n",i,histo_tb[i]) ;

			if(histo_tb[i] > max_val) {
				max_val = histo_tb[i] ;
				mean_tb = i ;
			}
		}


		tb_start = (int) (mean_tb - TPX_NEG_TB) ;
		tb_stop = (int) (mean_tb + TPX_POS_TB) ;

		// get the ranges of the peaks...
		int min_charge = 0x7FFFFFFF ;
		int max_charge = 0 ;

		for(s=s_start;s<=s_stop;s++) {
			for(r=1;r<=45;r++) {
				if(tf) fprintf(tf,"CHA %2d %2d %d: tb %d\n",s,r,tpx_pulser_peak[s][r],tpx_pulser_tb[s][r]) ;

				if(tpx_pulser_peak[s][r] > max_charge) {
					max_charge = tpx_pulser_peak[s][r] ;
				}
				if(tpx_pulser_peak[s][r] < min_charge) {
					min_charge = tpx_pulser_peak[s][r] ;
				}		
			}
		}

		if(tf) fclose(tf) ;

		if((mean_tb==184) || (mean_tb==185)) {		// pulser on local clock from the TCD
			tb_start = 181 ;
			tb_stop = 190 ;
		}

		LOG(TERR,"Peak found at timebin %d [%d..%d], peak charge range [%d..%d]",mean_tb,tb_start,tb_stop,
			min_charge, max_charge) ;

		
		// re-clear!
		for(int i=0;i<24;i++) {
			memset(gains[i],0,sizeof(struct gains)*46*182) ;
		}

	}
#endif
	if(events==0) {
		LOG(WARN,"Using hardcoded tb range [%3d:%3d]!",tb_start,tb_stop) ;
	}

	events++ ;
	LOG(NOTE,"After event %d",events) ;

	return ;
}

/*
	Called once per event per RDO.
	I.e. "evbuff" points to the RDO contrib
*/
void tpxGain::accum(char *evbuff, int bytes)
{
	int t ;
	int i ;
	u_int *data_end ;
	int sec ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;
	struct gains *gs ;
	struct aux *as ;

	/*
		1st event is used to determine the timebin window
		since the RHIC clocks might differ from run to run
	*/

	t = tpx_get_start(evbuff, bytes/4, &rdo, 0) ;

	LOG(NOTE,"RDO %d: %d bytes,token %d",rdo.rdo,bytes,t) ;

	if(t <= 0) return ;	// non data event...


	sec = rdo.sector ;
	data_end = rdo.data_end ;
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.what = TPX_ALTRO_DO_ADC ;
	a.t = rdo.token ;
	a.sector = rdo.sector ;
	a.log_err = 0 ;

	// got an rdo
	struct fee_found_t *fee_f = get_fee_found(sec,rdo.rdo) ;
	fee_f->got_one++ ;



	LOG(DBG,"gain: evt %d (got_one %d), sector %d, rdo %d",events, fee_f->got_one,rdo.sector, rdo.rdo) ;

	do {
		double charge, mtb ;

		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;

		//fee_found[sec][rdo.rdo].ch_count[a.id][a.ch]++ ;

		fee_f->ch_count[a.id][a.ch]++ ;

		//LOG(TERR,"event %d: rp %d:%d, got one %d: count %d",events,a.row,a.pad,fee_f->got_one,a.count) ;

		if((a.row>45) || (a.pad>182)) {	// the FEE is not supposed to be in this RDO!
			LOG(TERR,"Should not be here! row %d, pad %d, aid %3d:%02d",a.row,a.pad, a.id, a.ch) ;
			continue ;
		}

		gs = get_gains(sec,a.row,a.pad) ;
		as = get_aux(sec,a.row,a.pad) ;


#ifdef BLOODY_CRAP
		if((events==0)) {	// special processing for 1st event for connected pads...
			// find the timebin of the peak charge...
			for(i=0;i<a.count;i++) {
				if(a.adc[i] > gs->g) {
					gs->t0 = a.tb[i] ;	// the timebin of the maximum charge
					gs->g = a.adc[i] ;	// the maximum charge
				}
			}
		}
		else {
#else
		{
#endif
			double cou, noise ;
			
			charge = 0.0 ;
			cou = 0.0 ;
			mtb = 0.0 ;
			noise = 0.0 ;

			int peak = 0 ;

			for(i=0;i<a.count;i++) {
				if(a.adc[i] == 0) continue ;	// this is possible due to altro packing style...

				// sum up the main pulser peak...
				if((a.tb[i]>=tb_start) && (a.tb[i]<=tb_stop)) {



					if(a.adc[i] > peak) {
						peak = a.adc[i] ;
					}

					charge += a.adc[i] ;
					mtb += a.adc[i] * a.tb[i] ;

				}
				else if(a.tb[i]<(tb_start-5)) {	// sum up stuff before the peak
					noise += a.adc[i] ;
					cou++ ;

				}
			}

			if(cou) noise /= cou ;


			if(charge) {
				mtb /= charge ;


				as->cou++ ;
				gs->g += charge ;
				gs->t0 += mtb ;

			}
			else {
				//LOG(WARN,"rp %d:%d -- no charge",a.row,a.pad) ;
				as->low_pulse++ ;
			}

			if((noise > 40.0) || (cou>20)) {
				LOG(NOTE,"%d: row %d, pad %d, charge %f, noise %f, cou %f",events,a.row,a.pad,charge,noise,cou) ;
				as->noise++ ;
			}
			
#if 0		
			// NEEDS MORE WORK!
			// since charge_peak is a TPX constant we must allow for
			// row-to-row gain differences so the window should stay open...

			if(peak < (0.5*charge_peak)) {
				LOG(DBG,"EV %d:row %d:%d, peak %d, charge low %f",fee_found[sec][rdo.rdo].got_one,a.row,a.pad,peak,charge) ;
				//if(a.row && (a.pad==10)) LOG(WARN,"EV %d:row %d:%d, peak %d, charge low %f",fee_found[sec][rdo.rdo].got_one,a.row,a.pad,peak,charge) ;
				
				//as->low_pulse++ ;
			}
			else if(peak > (1.5*charge_peak)) {
				LOG(DBG,"row %d:%d, peak %d, charge high %f",a.row,a.pad,peak,charge) ;
				//as->high_pulse++ ;
			}
			else {	// just right!
			}
#endif			

			
		}
	} while(data_end && (data_end > rdo.data_start)) ;


	return ;

}

/*
	Called at end of run
*/
void tpxGain::calc()
{
	int s, r, p ;
	int c ;
	double g_rms, t0_rms, c_rms ;

	g_rms = t0_rms = c_rms = 0 ;


	// HACK! due to my f**** way of doing this I will need to add one to the events here...
//?	events++ ;

	LOG(DBG,"gain_calc: doing calculation with %d events",events) ;




	
	int s_start, s_stop ;

	if(sector==0) {	// whole TPX!
		s_start = 1 ;
		s_stop = 24 ;
	}
	else {
		s_start = sector ;
		s_stop = sector ;
	}


	char fname[128];
	sprintf(fname,"/RTS/log/tpx/tpx_raw_gains_%02d.txt",sector) ;
	FILE *ofile = fopen(fname,"w") ;

	for(s=s_start;s<=s_stop;s++) {


	for(r=1;r<=45;r++) {	// I changed this so it skips row 0, i.e. diconnected pads...

	if(get_means(s,r)->g || get_means(s,r)->t0) {
		LOG(ERR,"%f %f",get_means(s,r)->g,get_means(s,r)->t0) ;
	}

	// calc the mean, where appropriate...
	for(p=1;p<=tpc_rowlen[r];p++) {
		c = get_aux(s,r,p)->cou ;	// get count of events for this pad...

		if(c != events) {
			if((r==1)&&(p==10)) LOG(WARN,"srp %d:%d:%d: events %d, cou %d",s,r,p,events,c) ;
		}

		if(!c) {	// nothing ever fell in the timing acceptance window
			get_gains(s,r,p)->g = 0.0 ;
			get_gains(s,r,p)->t0 = 9.999 ;	

			int aa,cc,rdo ;
			tpx_to_altro(r,p,rdo,aa,cc) ;
			rdo-- ;	// we want from 0
			if(bad_rdo_mask[s] & (1<<rdo)) {
				// kill all the rows & pads in this RDO
				//LOG(WARN,"Masked Sector %d, RDO %d, row %d, pad %d",s,rdo+1,r,p) ;
				get_gains(s,r,p)->t0 = -5.0 ;

			}

			// back to indexing from 1!
			rdo++ ;
			int c = bad_fee[s][rdo][0] ;
			
			for(int i=0;i<c;i++) {
				int al=bad_fee[s][rdo][i+1];
				for(int j=0;j<2;j++) {
					//LOG(WARN,"Checking ALTRO %3d against %3d, RDO %d, row %d, pad %d",aa,al,rdo,r,p) ;
					if(aa == (int)al) {
						//LOG(WARN,"Masked ALTRO %3d, RDO %d, row %d, pad %d",aa,rdo,r,p) ;
						get_gains(s,r,p)->t0 = -5.0 ;
					}
					al++ ;
				}
			}
			
		}
		else {
			//if(c != (events-1)) LOG(WARN,"Events %d, cou %d",events,c) ;
			
			
			get_gains(s,r,p)->g /= c ;
			get_gains(s,r,p)->t0 /= c ;

			//printf("...%d %d: %.3f %.3f\n",r,p,get_gains(s,r,p)->g, get_gains(s,r,p)->t0) ;
		}
	}
	
	/*
		Now we need to calculate the mean CHARGE and T0 of a ROW
	
		We will skip first and last 3 pads because the gain 
		(calculated with the pulser) is usually wrong.


		We will also skip any pads which had _any_ to low
		or to high pulse...
	*/
								     
	c = 0 ;
	int accepted_pads_cou = 0 ;

	// calc noise etc.
	for(p=1;p<=(tpc_rowlen[r]);p++) {	// skip first and last 3 pads!
		struct aux *aux = get_aux(s,r,p) ;
		
		if(events) {
			aux->low_pulse = (100 * aux->low_pulse) / events ;
			aux->high_pulse = (100 * aux->high_pulse) / events ;
			aux->noise = (100 * aux->noise) / events ;
		}
	}

	double row_means = 0.0 ;
	for(p=4;p<=(tpc_rowlen[r]-3);p++) {	// skip first and last 3 pads!
		struct aux *aux = get_aux(s,r,p) ;
		int fired = aux->cou ;
		

		if(fired) ;
		else aux->low_pulse = 100 ;	// obviously

		if(get_gains(s,r,p)->g == 0.0) aux->low_pulse = 100 ;

		if((aux->low_pulse > 10) || (aux->noise > 10) || (aux->high_pulse > 10)) {
			if(r != 0) LOG(NOTE,"ROW %d, pad %d: lo %d, noise %d, hi %d -- skipping",r,p,
					aux->low_pulse, aux->noise, aux->high_pulse) ;
			continue ;
		}
		row_means += get_gains(s,r,p)->g ;
		c++ ;

	}

	accepted_pads_cou = c ;

	// calculate the rough mean charge of the row...
	if(c==0) {
		row_means = 0.0 ;
//		LOG(WARN,"ROW %d: no good pads???",r) ;
	}
	else row_means /= c ;


	/* 
		now, we skip all gains where the gain is not within
		10% of the mean
	*/

	c = 0 ;
	int tot_pads_cou ;
	int good_pads_cou ;

	tot_pads_cou = good_pads_cou = 0 ;

	for(p=4;p<=(tpc_rowlen[r]-3);p++) {
		double g = get_gains(s,r,p)->g ;

		// skip pads which are outside of the narrow window...
		if( (g>(row_means*0.9)) && (g<(row_means*1.1))) {

			get_means(s,r)->g += g ;
			get_means(s,r)->g_rms += g * g ;

			get_means(s,r)->t0 += get_gains(s,r,p)->t0 ;
			get_means(s,r)->t0_rms += get_gains(s,r,p)->t0 * get_gains(s,r,p)->t0 ;
			c++ ;
		}

		tot_pads_cou++ ;
	}

	good_pads_cou = c ;	// remember	

	// now calculate the nicer means...
	if(c==0) {
		// no row in the data...
		get_means(s,r)->t0 = tb_stop ;
	}
	else {
		get_means(s,r)->g /= c ;
		get_means(s,r)->g_rms /= c ;

		get_means(s,r)->g_rms = sqrt(get_means(s,r)->g_rms - get_means(s,r)->g * get_means(s,r)->g) ;
		
		get_means(s,r)->t0 /= c ;
		get_means(s,r)->t0_rms /= c ;

		get_means(s,r)->t0_rms = sqrt(get_means(s,r)->t0_rms - get_means(s,r)->t0 * get_means(s,r)->t0) ;
	}


	/* 
		Now we apply the per-row normalization of the gain and T0 constants using
		the previously calculated average gains and T0
	*/

	for(p=1;p<=tpc_rowlen[r];p++) {

		if(get_gains(s,r,p)->g) {
			if(ofile) fprintf(ofile,"%d %d %d %.3f %.3f [%.3f]\n",s,r,p,get_gains(s,r,p)->g,get_gains(s,r,p)->t0,get_means(s,r)->t0) ;

			get_gains(s,r,p)->g = get_means(s,r)->g / get_gains(s,r,p)->g ;
			get_gains(s,r,p)->t0 = get_means(s,r)->t0 - get_gains(s,r,p)->t0;
			
			if(get_gains(s,r,p)->g > 9.9) {
				get_gains(s,r,p)->g = 0.0 ;	// kill!
				get_gains(s,r,p)->t0 = -9.9 ;
			}

			//printf("...%d %d: %.3f %.3f\n",r,p,get_gains(s,r,p)->g, get_gains(s,r,p)->t0) ;
		}
	}
	

	if(get_means(s,r)->g != 0.0) {
		LOG(NOTE,"Sector %2d, row %2d: charge %.3f +- %.3f; t0 %.3f +- %.3f; rough mean %.3f; good/acc/all pads %d/%d/%d",
		    s,r,
		    get_means(s,r)->g, get_means(s,r)->g_rms,
		    get_means(s,r)->t0, get_means(s,r)->t0_rms,
		    row_means,good_pads_cou, accepted_pads_cou, tot_pads_cou) ;
	}
	else {
		LOG(WARN,"Sector %2d, row %2d: charge %f +- %f; t0 %f +- %f; rough_mean %.1f; good/acc/all pads %d/%d/%d",
		    s,r,
		    get_means(s,r)->g, get_means(s,r)->g_rms,
		    get_means(s,r)->t0, get_means(s,r)->t0_rms,
		    row_means,good_pads_cou, accepted_pads_cou, tot_pads_cou) ;
	}


	// for logging reasons we calc the RMS of the whole TPC but we skip unconnected pads
	if(get_means(s,r)->g) {
		
		//printf("Sector %d, row %d: %f +- %f charge, %f += %f t0 [%d cou]\n",s,r,
		//       get_means(s,r)->g, get_means(s,r)->g_rms,
		//       get_means(s,r)->t0, get_means(s,r)->t0_rms, c) ;
		
		g_rms += get_means(s,r)->g_rms ;
		t0_rms += get_means(s,r)->t0_rms ;
		c_rms++ ;
	}
	


	}	// end of row calc...
	}	// end of sector calc



	g_rms /= c_rms ;
	t0_rms /= c_rms ;

	if(ofile) fclose(ofile) ;

	LOG(TERR,"gain_calc: %d events used: Mean RMS: %.3f gain, %.3f T0",events,g_rms,t0_rms) ;


	return ;
}
	
void tpxGain::do_default(int sector)
{
	int s, r, p ;


	// zap and create defaults in case the file is missing!
	memset(bad_fee,0,sizeof(bad_fee)) ;
	tpx_odd_fee_count = 0 ;

	if(sector) {
		if(gains[sector-1] == 0) {
			gains[sector-1] = (struct gains *) malloc(sizeof(struct gains) * 46 * 182) ;
		}
	}
	else {
		for(int i=0;i<24;i++) {
			if(gains[i] == 0) {
				gains[i] = (struct gains *) malloc(sizeof(struct gains) * 46 * 182) ;
			}
		}
	}

	// create defaults!
	for(s=1;s<=24;s++) {
	for(r=0;r<=45;r++) {
	for(p=1;p<=182;p++) {
		if(gains[s-1]) {
			set_gains(s,r,p,1.0,0.0) ;
		}
	}
	}
	}

	return ;
}

int tpxGain::from_file(char *fname, int sec)
{
	FILE *f ;
	int s, r, p ;
	float g, t0 ;
	struct stat buff ;

	// try file...
	sector = sec ;

	f = fopen(fname,"r") ;

	if(f==0) {
		LOG(WARN,"from_file: error in fopen \"%s\" [%s]",fname,strerror(errno)) ;
		do_default(sector) ;
		return 1 ;	// assume hange...
	}

	

	stat(fname, &buff) ;

	LOG(DBG,"After stat %s %d %d",fname,buff.st_mtime,load_time) ;

	if(load_time < buff.st_mtime) {
		LOG(DBG,"Will reload...") ;
		LOG(INFO,"Reloading \"%s\"",fname) ;
	}
	else {
		LOG(DBG,"Wont relaod") ;
		LOG(INFO,"Keeping cached copy of gains...") ;
		return 0 ;	// no change
	}

	//LOG(DBG,"What?") ;

	load_time = time(NULL) ;
	do_default(sector) ;	// zap to all 1...
	
	LOG(TERR,"reading gains from \"%s\" for sector %d...",fname, sector) ;

	while(!feof(f)) {
		char str[1024] ;

		if(fgets(str,sizeof(str)-1,f)==0) continue ;

		if(strlen(str)==0) continue ;	// empty
		if((str[0]=='#') || (str[0]=='/')) continue ;	// comment


		int ret = sscanf(str,"%d %d %d %f %f\n",&s,&r,&p,&g,&t0) ;

		if(ret != 5) continue ;

	
		if(s < 0) {	// special case! the whole TPC-FEE in "pad" is bad
			s *= -1 ;

			if(sector && (s != sector)) continue ;

			int altro ;

			// counter is in location 0!
			int c = bad_fee[s][r][0] ;

			// the TPC_FEE is in the file as "pad"
			
			// move to ALTRO_ID
			altro = (p<<1) & 0xFF ;
			bad_fee[s][r][c+1] = altro ;

			bad_fee[s][r][0]++ ;	// increment count of bad FEEs...

			// do the same for the global
			c = tpx_odd_fee_count ;
			tpx_odd_fee[c].sector = s ;
			tpx_odd_fee[c].rdo = r ;
			tpx_odd_fee[c].status = 2 ;	// mark bad
			tpx_odd_fee[c].tpc_fee_padplane = p ;
			tpx_odd_fee[c].altro_id_padplane = altro ;

			tpx_odd_fee_count++ ;

			LOG(INFO,"Bad FEE %d: sector %2d, RB %d, ALTRO %3d (TPC-FEE %3d)",bad_fee[s][r][0],s,r,altro,p) ;

			continue ;
		}


		if(sector && (s != sector)) continue ;

		set_gains(s,r,p,g,t0) ;

	}
	fclose(f) ;

	// now kill the pads where we had the bad fees!
	for(s=1;s<=24;s++) {

		if(sector && (s != sector)) continue ;

		for(r=1;r<=6;r++) {
			
			for(u_int c=0;c<bad_fee[s][r][0];c++) {
				int fee ;

				fee = bad_fee[s][r][c+1] ;

				for(int i=0;i<2;i++) {		// kill both ALTROs...
				for(int ch=0;ch<16;ch++) {	// ...all channels..
					int row, pad ;
					
					tpx_from_altro(r-1,fee+i,ch, row, pad) ;

					if(row>45) {
						LOG(ERR,"What????") ;
						continue ;
					}

					set_gains(s,row,pad,0.0,-5.0) ;	// t0 -5 signifies the whole FEE was killed...


					LOG(DBG,"Killing rp %d:%d for bad FEE %3d in sector %2d",row,pad,fee,s) ;
				}
				}

			}
		}
	}

	LOG(TERR,"Gains read.") ;
	return 1 ;	// changed!
}


int tpxGain::to_file(char *fname) 
{

	FILE *f ;
	int s, r, p ;

	if(strcmp(fname,"stdout")==0) f = stdout ;
	else {
		f = fopen(fname,"w") ;
	}

	if(f==0) {
		LOG(ERR,"gains: fopen \"%s\" [%s]",fname,strerror(errno)) ;
		return -1 ;
	}
	


	int s_start, s_stop ;
	if(sector>0) {
		s_start = sector ;
		s_stop = sector ;
	}
	else {
		s_start = 1 ;
		s_stop = 24 ;
	}

	LOG(TERR,"gains: writing to file \"%s\" for sectors %d..%d...",fname,s_start,s_stop) ;

	for(s=s_start;s<=s_stop;s++) {
	for(r=1;r<=45;r++) {
	for(p=1;p<=tpc_rowlen[r];p++) {
		fprintf(f,"%d %d %d %.3f %6.3f\n",s,r,p,
			get_gains(s,r,p)->g,
			get_gains(s,r,p)->t0) ;
	}
	}
	}

	if(f != stdout)	fclose(f) ;

	LOG(TERR,"gains: written.") ;

	return 0 ;
}

void tpxGain::compare(char *fname, int mysec) 
{
	FILE *f ;
	int s, r, p ;
	float g, t0 ;

	int both, old_only, new_only ;

	old_only = new_only = both = 0 ;
	
	f = fopen(fname,"r") ;

	if(f==0) {
		LOG(ERR,"from_file: error in fopen \"%s\" [%s]",fname,strerror(errno)) ;
		return  ;
	}


	while(!feof(f)) {
		char str[1024] ;

		if(fgets(str,sizeof(str)-1,f)==0) continue ;

		if(strlen(str)==0) continue ;	// empty
		if((str[0]=='#') || (str[0]=='/')) continue ;	// comment


		int ret = sscanf(str,"%d %d %d %f %f\n",&s,&r,&p,&g,&t0) ;

		if(ret != 5) continue ;
		if(s < 0) continue ;

		if(mysec && (s != mysec)) continue ;	// only look at my sectors

		if(r==0) continue ;	// skip unphysical channels

		if(g==0.0) {	// bad in old file
			if(get_gains(s,r,p)->g == 0.0) both++ ;
			else old_only++ ;
		}
		else {		// good in old file

			// here I need to differentiate between bad FEEs and masked RDOs!
			if(get_gains(s,r,p)->g == 0.0) {
				if(get_gains(s,r,p)->t0 == -5.0) {
					LOG(DBG,"FEE was marked as bad (%d,%d,%d) -- skipping",s,r,p) ;
				}
				else {
					new_only++ ;
				}
			}
		}
	}
	
	fclose(f) ;

	if(new_only>10) {
		LOG(ERR, "gain_compare, sector %d: seems to have new bad pads: both %3d, new_only %3d, old_only %d",mysec,both,new_only,old_only) ;
	}
	else {
		LOG(INFO,"gain_compare, sector %d: both %3d, new_only %3d, old_only %d",mysec,both,new_only,old_only) ;
	}
	return ;
}

int tpxGain::summarize(char *fname, FILE *log)
{
	int s, r, a ;
	char reason[1024] ;
	u_int good, bad ;
	FILE *ofile ;
	int notes = 0 ;

	good = bad = 0 ;
	reason[0] = 0 ;	// empty string...

	if(fname==0) ofile = 0 ;
	else if(strcmp(fname,"stdout")==0) ofile = stdout ;
	else {
		ofile = fopen(fname,"w") ;
		if(ofile==0) {
			LOG(ERR,"summarize: error in fopen \"%s\" [%s]",fname,strerror(errno)) ;
			return notes ;
		}
	}
		


	int s_start, s_stop ;
	if(sector) {
		s_start = s_stop = sector ;
	}
	else {
		s_start = 1 ;
		s_stop = 24 ;
	}

	// check fee sanity here!

	for(s=s_start;s<=s_stop;s++) {
	for(r=1;r<=6;r++) {	// RB
		struct fee_found_t *fee_f = get_fee_found(s,r) ;

		if(fee_f->got_one == 0) continue ;
	
		if(events != (int)fee_f->got_one) {
			LOG(WARN,"Run had sector %d, RB %d: expect %u events, got %u",s,r,events,fee_f->got_one) ;
		}
		else {
			LOG(NOTE,"Run had sector %d, RB %d: expect %u events, got %u",s,r,events,fee_f->got_one) ;
		}

		for(int i=0;i<36;i++) {
			int fee = tpx_rdo_fees(r,i) ;
			if(fee == 255) continue ;

			for(int fch=0;fch<32;fch++) {	// FEE channel! [0..15] are on the upper row, [16..31] on the lower

				int ch = tpx_old_to_new_ch[fch] ;

				if(ch > 15) {
					ch -= 16 ;
					a = ((fee << 1) | 1) & 0xFF ;
				}
				else {
					a = (fee << 1) & 0xFF ;
				}

				int  err = 0 ;

				int seen = fee_f->ch_count[a][ch] ;

				int row = tpx_altro_to_pad[r-1][a][ch].row ;
				int pad = tpx_altro_to_pad[r-1][a][ch].pad ;


				double g = get_gains(s,row,pad)->g ;
				double t0 = get_gains(s,row,pad)->t0 ;

				struct aux *aux = get_aux(s,row,pad) ;

				reason[0] = 0 ;

				if(seen != events) {

					if(seen == 0) {
						err = 3 ;
						sprintf(reason+strlen(reason),"[Bad - Missing]") ;
					}
					else {
						if(seen>events) {
							err = 3 ;
							sprintf(reason+strlen(reason),"[Bad - Flaky readout: seen %d times in %d events]",seen,events) ;
						}
					}

				}


				int bad_t0, bad_gain, bad_noise, bad_low ;
				bad_t0 = bad_gain = bad_noise = bad_low = 0 ;

				if(row != 0) {	// only for physically connected rows!
						

					if((g<0.9) || (g>1.1)) {				
						bad_gain = 1 ;
			

						// first & last pads are usually bad
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}
					if((t0<-0.15) || (t0>0.15)) {
						bad_t0 = 1 ;


						// first and last are usually bad...
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}


					if(aux->noise > 10) {
						bad_noise = aux->noise ;

						// first & last pads are usually bad
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}
		
					if(aux->low_pulse > 1) {
						bad_low = aux->low_pulse ;

						// first & last pads are usually bad
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}
		
				}
				

				// new as of Jun 11, 08:
				// override fch to be J1 pin number as given to Tonko by Bob!
				u_char j1 = tpx_altro_to_j1[a&1][ch] ;

				if(err > 1) {
					if(row && bad_gain) sprintf(reason+strlen(reason),"[Bad gain %.1f]",g) ;					
					if(row && bad_t0) sprintf(reason+strlen(reason),"[Bad t0 %.1f]",t0) ;
					if(row && bad_noise) sprintf(reason+strlen(reason),"[Bad noise %d%%]",bad_noise) ;
					if(row && bad_low) sprintf(reason+strlen(reason),"[Bad low %d%%]",bad_low) ;



					notes++ ;
					if(log) {
						fprintf(log,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,row,pad,
							g,t0,err,reason) ;
					}
				}


				if((err>1) || bad_gain || bad_t0 || bad_noise || bad_low) {	// this now includes edge pads!
					g = 0.0 ;	// mark real bad
					get_gains(s,row,pad)->g = 0.0 ;	// need it later for comparison...
				}

				if(ofile) fprintf(ofile,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,row,pad,
				       g,t0,err,reason) ;

				fee_f->ch_count[a][ch] *= -1 ;	// mark as seen and done!
			}
		}

		for(a=0;a<256;a++) {
			for(int ch=0;ch<16;ch++) {
				u_char j1 = tpx_altro_to_j1[a&1][ch] ;
				if(fee_f->ch_count[a][ch] > 0) {
					int fee = tpx_altro_to_fee(r,a) ;
					notes++ ;
					if(log) {
						fprintf(log,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,-1,-1,
							0.0,0.0,3,"[Bad - Spurious channel]") ;

					}
					if(ofile) fprintf(ofile,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,-1,-1,
					       0.0,0.0,3,"[Bad - Spurious channel]") ;
				}
			}
		}
					


	}
	}

	if(ofile && (ofile != stdout)) fclose(ofile) ;

	return notes ;
}

