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

#define TPX_PULSER_PED		95
#define TPX_PULSER_START	100
#define TPX_PULSER_STOP		103
#define TPX_PULSER_TIME		100.84	// or perhaps 100.90?

struct tpx_odd_fee_t tpx_odd_fee[256] ;
int tpx_odd_fee_count = 0 ;


static u_int histo_data[2000] ;
static void histo_init()
{
	memset(histo_data,0,sizeof(histo_data)) ;
}

static void histo_fill(double gain)
{
	int i_gain = (int) gain ;

	if((i_gain>=0) && (i_gain<2000)) histo_data[i_gain]++ ;

	return ;
}

static double histo_peak()
{
	int max_sum, sum ;
	int start_i = 0;
	double peak ;

	max_sum = 0 ;
	for(int i=0;i<(2000-10);i++) {
		sum = 0 ;
		for(int j=0;j<10;j++) {
			sum += histo_data[i+j] ;
		}
		if(sum > max_sum) {
			max_sum = sum ;
			start_i = i ;
		}
	}

	sum = 0 ;
	peak = 0.0 ;

	for(int i=start_i;i<(start_i+10);i++) {
		sum += histo_data[i] ;
		peak += i*histo_data[i] ;
	}


	if(sum) {
		peak /= sum ;
		peak += 0.5 ;	// to unbias it...
	}

	return peak ;
}

	

tpxGain::tpxGain()
{
	events = 0 ;
	load_time = 0 ;
	sector = 0 ;	// assume all...
	c_run = c_date = c_time = 0 ;

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

#if 0
	// used in run FY09
	tb_start = 179 ;
	tb_stop = 188 ;
#endif

	memset(bad_rdo_mask,0,sizeof(bad_rdo_mask)) ;

	return ;
}


/*
	Called at end of event
*/
void tpxGain::ev_done() 
{
	if(events==0) {
		LOG(WARN,"Using hardcoded tb range [%3d:%3d]!",TPX_PULSER_START,TPX_PULSER_STOP) ;
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

		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;

		fee_f->ch_count[a.id][a.ch]++ ;

		if((a.row>45) || (a.pad>182)) {	// the FEE is not supposed to be in this RDO!
			LOG(TERR,"Should not be here! row %d, pad %d, aid %3d:%02d",a.row,a.pad, a.id, a.ch) ;
			continue ;
		}

		gs = get_gains(sec,a.row,a.pad) ;
		as = get_aux(sec,a.row,a.pad) ;


		double cou, noise ;
			
		cou = 0.0 ;
		noise = 0.0 ;


		if(a.count) as->cou++ ;

		for(i=0;i<a.count;i++) {
			if(a.adc[i] == 0) continue ;	// this is possible due to altro packing style...

			int adc_i = a.adc[i] ;
			int tb_i = a.tb[i] ;

			// sum up the main pulser peak...
			if((tb_i>=TPX_PULSER_PED) && (tb_i<=TPX_PULSER_STOP)) {
				as->adc_store[tb_i - TPX_PULSER_PED] += adc_i ;
			}
			else if(tb_i < TPX_PULSER_PED) {	// sum up stuff before the peak
				noise += adc_i ;
				cou++ ;
			}
		}

		if(cou) noise /= cou ;


		if((noise > 40.0) || (cou>20)) {
			as->noise++ ;
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
	//FILE *ofile = stdout ;

	for(s=s_start;s<=s_stop;s++) {


	for(r=1;r<=45;r++) {	// I changed this so it skips row 0, i.e. diconnected pads...

	if(get_means(s,r)->g || get_means(s,r)->t0) {
		LOG(ERR,"%f %f",get_means(s,r)->g,get_means(s,r)->t0) ;
	}

	// calc the mean, where appropriate...
	for(p=1;p<=tpc_rowlen[r];p++) {
		c = get_aux(s,r,p)->cou ;	// get count of events for this pad...

		if(!c) {	// nothing ever fell on this pad _at_all_
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
			int ped_cou = 0 ;
			double ped = 0.0 ;

			struct aux *as = get_aux(s,r,p) ;

			for(int i=TPX_PULSER_PED;i<TPX_PULSER_START;i++) {
				ped += (double) as->adc_store[i-TPX_PULSER_PED] / (double) c ;
				ped_cou++ ;
			}

			ped /= ped_cou ;
			
			double charge, t0 ;
			charge = t0 = 0.0 ;

			for(int i=TPX_PULSER_START;i<TPX_PULSER_STOP;i++) {
				double val = (double) as->adc_store[i-TPX_PULSER_PED] / (double) c - ped ; 
				charge += val ;
				t0 += i * val ;
			}

			// and fill in the final absolute charge and t0...

			get_gains(s,r,p)->g = charge ;		// this might be small or even negative in case the pad is not connected

			if(charge) {
				get_gains(s,r,p)->t0 = t0/charge ;
			}
			else {
				get_gains(s,r,p)->t0 = 0.0 ;	// *shrug* what else...
			}

		}
	}
	
	/*
		Now we need to calculate the mean CHARGE and T0 of a ROW
	
		We will skip first and last 3 pads because the gain 
		(calculated with the pulser) is usually wrong.

		We will also skip any pads which had _any_ to low
		or to high pulse...
	*/

	// normalize the odd pulser to 100%
	for(p=1;p<=tpc_rowlen[r];p++) {	// skip first and last 3 pads!
		struct aux *aux = get_aux(s,r,p) ;
		
		if(events) {	// normalize to 100%
			aux->low_pulse = (100 * aux->low_pulse) / events ;
			aux->high_pulse = (100 * aux->high_pulse) / events ;
			aux->noise = (100 * aux->noise) / events ;
		}
	}

	histo_init() ;

	c = 0 ;

	for(p=1;p<=tpc_rowlen[r];p++) {
		struct aux *aux = get_aux(s,r,p) ;
		int fired = aux->cou ;
		

		if(fired) ;			// do nothing, all is well...
		else aux->low_pulse = 100 ;	// obviously

		// the gain now has the mean charge, absolute, so let's cut on some
		// sanitiy numbers here i.e. 100 ...
		if(get_gains(s,r,p)->g < 100.0) aux->low_pulse = 100 ;
		if(get_gains(s,r,p)->g > 2000.0) aux->high_pulse = 100 ;

		// skip obviously bad ones...
		if((aux->low_pulse > 10) || (aux->noise > 10) || (aux->high_pulse > 10)) {
			if(r != 0) LOG(NOTE,"ROW %d, pad %d: lo %d, noise %d, hi %d -- skipping",r,p,
					aux->low_pulse, aux->noise, aux->high_pulse) ;
			continue ;
		}
		
		// use only _known_ good ones
		if((p>=4) && (p<=(tpc_rowlen[r]-3))) {
			histo_fill(get_gains(s,r,p)->g) ;
			c++ ;
		}
	}

	int accepted_pads_cou = c ;
	
	double row_means = histo_peak() ;	// rough mean of the charge on this row...

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


#if 0
	/* Dump absolute values here for debugging! */
	printf("%2d %2d %f %f %f %f\n",s,r,
	       get_means(s,r)->g, get_means(s,r)->g_rms,
	       get_means(s,r)->t0, get_means(s,r)->t0_rms) ;
#endif

	/* 
		Now we apply the per-row normalization of the gain and T0 constants using
		the previously calculated average gains and T0
	*/

	for(p=1;p<=tpc_rowlen[r];p++) {

		if(ofile) fprintf(ofile,"%d %d %d %.3f %.3f ",s,r,p,get_gains(s,r,p)->g,get_gains(s,r,p)->t0) ;

		if(get_gains(s,r,p)->g) {			
			// this is the actual correction...
			get_gains(s,r,p)->g = get_means(s,r)->g / get_gains(s,r,p)->g ;	// relative to row
			get_gains(s,r,p)->t0 = TPX_PULSER_TIME - get_gains(s,r,p)->t0;	// absolute to TPX!
		}

		if(ofile) fprintf(ofile,"%.3f %.3f\n",get_gains(s,r,p)->g, get_gains(s,r,p)->t0) ;
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
		if(gains[s-1]) {
			memset(gains[s-1],0,sizeof(struct gains) * 46 * 182) ;
		}
		else {
			continue ;
		}

		for(r=1;r<=45;r++) {
			for(p=1;p<=tpc_rowlen[r];p++) {
				set_gains(s,r,p,1.0,0.0) ;	// we'll set the gains to 1.0 only for valid pads...
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
		return -1 ;	// assume hange...
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

	// we don't know yet what those values were...
	c_run = 0 ;
	c_date = 0 ;
	c_time = 0 ;

	u_int f_date, f_time ;
	
	struct tm tm ;
	localtime_r(&buff.st_mtime, &tm) ;
	
	f_date = (tm.tm_year+1900) * 10000 + (tm.tm_mon+1) * 100 + tm.tm_mday ;
	f_time = tm.tm_hour * 10000 + tm.tm_min * 100 + tm.tm_sec ;

	//LOG(DBG,"What?") ;

	load_time = time(NULL) ;
	do_default(sector) ;	// zap to all 1...
	
	LOG(TERR,"reading gains from \"%s\" for sector %d...",fname, sector) ;



	while(!feof(f)) {
		char str[1024] ;

		if(fgets(str,sizeof(str)-1,f)==0) continue ;

		if(strlen(str)==0) continue ;	// empty

		if((str[0]=='#') || (str[0]=='/')) {	// comment
			char *cix ;
			if((cix = strstr(str,"Run "))) {
				sscanf(cix+4,"%u",&c_run) ;
			}
			else if((cix = strstr(str,"Date "))) {
				sscanf(cix+5,"%u",&c_date) ;
			}
			else if((cix = strstr(str,"Time "))) {
				sscanf(cix+5,"%u",&c_time) ;
			}
			else {
				continue ;	// comment
			}
		}

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

	LOG(TERR,"Gains read: run %08u, date %08u [%08u], time %06u [%06u]",c_run,c_date,f_date,c_time,f_time) ;
	return 1 ;	// changed!
}


int tpxGain::to_file(char *fname) 
{

	FILE *f ;
	int s, r, p ;

	time_t tim = time(NULL) ;

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


	struct tm tm ;
	localtime_r(&tim, &tm) ;

	c_date = (tm.tm_year+1900) * 10000 + (tm.tm_mon+1) * 100 + tm.tm_mday ;
	c_time = tm.tm_hour * 10000 + tm.tm_min * 100 + tm.tm_sec ;

	LOG(TERR,"gains: writing to file \"%s\" for sectors %d..%d: run %u, date %u, time %u",fname,
	    s_start,s_stop,
	    c_run, c_date, c_time) ;

	fprintf(f,"# $Id: tpxGain.cxx,v 1.20 2009/08/31 19:33:30 tonko Exp $\n") ;	// CVS id!
	fprintf(f,"# Run %u\n",c_run) ;

	for(s=s_start;s<=s_stop;s++) {
	for(r=1;r<=45;r++) {
	for(p=1;p<=tpc_rowlen[r];p++) {
		fprintf(f,"%d %d %d %.3f %6.3f\n",s,r,p,
			get_gains(s,r,p)->g,
			get_gains(s,r,p)->t0) ;
	}
	}
	}

	// dump at the end!
	fprintf(f,"# Date %u\n",c_date) ;
	fprintf(f,"# Time %u\n",c_time) ;

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
			getcwd(reason,100) ;
			LOG(ERR,"summarize: error in fopen %s\"%s\" [%s] ",reason,fname,strerror(errno)) ;
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

					if((t0<-0.30) || (t0>0.30)) {
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

