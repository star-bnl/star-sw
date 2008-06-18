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

#include <DAQ_TPX/tpxCore.h>
#include <DAQ_TPX/tpxGain.h>

#define TPX_NEG_TB	1
#define TPX_POS_TB	3



// counts sector & rdo from 1!
static struct fee_found {
	u_int got_one ;
	int ch_count[256][16] ;	// ALtro ID & CH is the index...
} fee_found[25][7] ;



tpxGain::tpxGain()
{
	events = 0 ;
	load_time = 0 ;
	sector = 0 ;	// assume all...

	// mark memory as cleared
	aux = 0 ;
	means = 0 ;
	memset(gains,0,sizeof(gains)) ;

	// bad_fee's are sticky and can only be entered via a file!
	memset(bad_fee,0,sizeof(bad_fee)) ;

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


	for(int i=0;i<24;i++) {
		bytes = sizeof(struct gains) * 46 * 182 ;
		if(gains[i]==0) {
			gains[i] = (struct gains *) malloc(bytes) ;
		}
		memset(gains[i],0,bytes) ;
	}

	memset(fee_found,0,sizeof(fee_found)) ;

	return ;
}


/*
	Called at end of event
*/
void tpxGain::ev_done() 
{
	if(events==0) {	// this is called after the first, special, event...

		int s, r, p ;

		double mean_tb ;
		double mean_peak ;
		int mean_cou ;
		
		mean_tb = 0.0 ;
		mean_peak = 0.0 ;
		mean_cou = 0 ;

		int s_start, s_stop ;
		if(sector) {
			s_start = s_stop = sector ;
		}
		else {
			s_start = 1 ;
			s_stop = 24 ;
		}

		for(s=s_start;s<=s_stop;s++) {
		for(r=1;r<=45;r++) {			// skip non-connected pads
		for(p=3;p<=(tpc_rowlen[r]-2);p++) {	// skip first 2, last 2 pads in a row...
			float tb = get_gains(s,r,p)->t0 ;
			float g = get_gains(s,r,p)->g ;

			// calc only for sane values... typical peak is 300 so we go 3 times less/more...
			if(tb && (g>100.0) && (g<900.0)) {
				mean_tb += tb ;
				mean_peak += g ;
				mean_cou++ ;
			}
		}
		}
		}

		if(mean_cou) {
			mean_tb /= mean_cou ;
			mean_peak /= mean_cou ;
		}
		else {
			mean_tb = 200.0 ;
			mean_peak = 300.0 ;
		}

		tb_start = (int) (mean_tb - TPX_NEG_TB) ;
		tb_stop = (int) (mean_tb + TPX_POS_TB) ;
		charge_peak = (int) (mean_peak + 0.5);

		if(mean_cou) {
			LOG(INFO,"Peak found at timebin %f window is [%d..%d], mean peak charge %f [%d]",mean_tb,tb_start,tb_stop,mean_peak,charge_peak) ;
		}
		else {
			LOG(ERR,"Can;t fine mean peak!") ;
		}
		
		// re-clear!
		for(int i=0;i<24;i++) {
			memset(gains[i],0,sizeof(struct gains)*46*182) ;
		}

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

	LOG(DBG,"RDO %d: %d bytes,token %d",rdo.rdo,bytes,t) ;

	if(t <= 0) return ;	// non data event...


	sec = rdo.sector ;
	data_end = rdo.data_end ;
	a.rdo = rdo.rdo - 1 ;	// a.rdo counts from 0
	a.what = TPX_ALTRO_DO_ADC ;
	a.t = rdo.token ;


	// got an rdo
	fee_found[sec][rdo.rdo].got_one++ ;



	LOG(DBG,"gain: evt %d (got_one %d), sector %d, rdo %d",events, fee_found[sec][rdo.rdo].got_one,rdo.sector, rdo.rdo) ;

	do {
		double charge, cou, mtb ;

		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;

		fee_found[sec][rdo.rdo].ch_count[a.id][a.ch]++ ;
	
		if((a.row>45) || (a.pad>182)) {	// the FEE is not supposed to be in this RDO!
			LOG(NOTE,"Should not be here! row %d, pad %d, aid %3d:%02d",a.row,a.pad, a.id, a.ch) ;
			continue ;
		}

		gs = get_gains(sec,a.row,a.pad) ;
		as = get_aux(sec,a.row,a.pad) ;


		//if(a.row==0) {
		//	LOG(DBG,"Not connected to pad: AID %d:%d, rp %d:%d",a.id,a.ch,a.row,a.pad) ;
		//}

		if((events==0)) {	// special processing for 1st event for connected pads...
						// find the timebin of the peak charge...
			for(i=0;i<a.count;i++) {
				// use some sanity...
				if((a.tb[i]>100) && (a.tb[i]<300)) {
					if(a.adc[i] > gs->g) {
						gs->t0 = a.tb[i] ;
						gs->g = a.adc[i] ;
					}
				}
			}
		}
		else {

			charge = 0.0 ;
			cou = 0.0 ;
			mtb = 0.0 ;
			int peak = 0 ;


			for(i=0;i<a.count;i++) {
				if((a.tb[i]>=tb_start) && (a.tb[i]<=tb_stop)) {

					//printf("... %d %d\n",a.tb[i],a.adc[i]) ;
					if(a.adc[i] > peak) {
						peak = a.adc[i] ;
					}

					charge += a.adc[i] ;
					mtb += a.adc[i] * a.tb[i] ;
					cou++ ;
				}
			}

			if(charge) {
				mtb /= charge ;

				//printf("... %d:%d -- %f %f, cou %f\n",a.row,a.pad,charge,mtb,cou) ;
			}
			else {
				mtb = tb_stop ;	// MUST put some sane value!
			}

			as->cou++ ;
			gs->g += charge ;
			gs->t0 += mtb ;
		
			// since charge_peak is a TPX constant we must allow for
			// row-to-row gain differences so the window should stay open...

			if(peak < (0.5*charge_peak)) {
				LOG(DBG,"EV %d:row %d:%d, peak %d, charge low %f",fee_found[sec][rdo.rdo].got_one,a.row,a.pad,peak,charge) ;
				if(a.row && (a.pad==10)) LOG(WARN,"EV %d:row %d:%d, peak %d, charge low %f",fee_found[sec][rdo.rdo].got_one,a.row,a.pad,peak,charge) ;
				
				as->low_pulse++ ;
			}
			else if(peak > (1.5*charge_peak)) {
				LOG(DBG,"row %d:%d, peak %d, charge high %f",a.row,a.pad,peak,charge) ;
				as->high_pulse++ ;
			}
			else {	// just right!
			}
			

			
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

	LOG(INFO,"gain_calc: doing calculation with %d events",events) ;




	
	int s_start, s_stop ;

	if(sector==0) {	// whole TPX!
		s_start = 1 ;
		s_stop = 24 ;
	}
	else {
		s_start = sector ;
		s_stop = sector ;
	}

	for(s=s_start;s<=s_stop;s++) {
	for(r=0;r<=45;r++) {


	// calc the mean, where appropriate...
	for(p=1;p<=tpc_rowlen[r];p++) {
		c = get_aux(s,r,p)->cou ;



		if(!c) {
			get_gains(s,r,p)->g = 0.0 ;
			get_gains(s,r,p)->t0 = 0.0 ;	// never seen in the data...
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
	
		We will skip first and last 2 pads because the gain 
		(calculated with the pulser) is usually wrong.


		We will also skip any pads which had _any_ to low
		or to high pulse...
	*/
								     
	c = 0 ;
	double row_means = 0.0 ;
	for(p=3;p<=(tpc_rowlen[r]-2);p++) {
		if(get_gains(s,r,p)->g) {
			// for the row mean, skip low & high pulse guys!
			if(get_aux(s,r,p)->low_pulse || get_aux(s,r,p)->high_pulse) {
				LOG(WARN,"ROW %d, pad %d: lo %d, hi %d -- skipping",r,p,get_aux(s,r,p)->low_pulse,get_aux(s,r,p)->high_pulse) ;
				continue ;
			}

			row_means += get_gains(s,r,p)->g ;

			c++ ;
		}
	}

	if(c==0) row_means = 0.0 ;
	else row_means /= c ;


	/* 
		now, we skip all gains where the gain is not within
		10% of the mean
	*/

	c = 0 ;
	for(p=3;p<=(tpc_rowlen[r]-2);p++) {
		double g = get_gains(s,r,p)->g ;

		if( (g>(row_means*0.9)) && (g<(row_means*1.1))) {

			get_means(s,r)->g += g ;
			get_means(s,r)->g_rms += g * g ;

			get_means(s,r)->t0 += get_gains(s,r,p)->t0 ;
			get_means(s,r)->t0_rms += get_gains(s,r,p)->t0 * get_gains(s,r,p)->t0 ;
			c++ ;
		}
	}

	

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
			get_gains(s,r,p)->g = get_means(s,r)->g / get_gains(s,r,p)->g ;
			get_gains(s,r,p)->t0 = get_means(s,r)->t0 - get_gains(s,r,p)->t0;
			
			if(get_gains(s,r,p)->g > 9.9) get_gains(s,r,p)->g = 9.9 ;

			//printf("...%d %d: %.3f %.3f\n",r,p,get_gains(s,r,p)->g, get_gains(s,r,p)->t0) ;
		}
		else {
			get_gains(s,r,p)->t0 = 9.9 ;
		}

	}
	

	if(get_means(s,r)->g != 0.0) {
		LOG(INFO,"Sector %2d, row %2d: charge %f +- %f; t0 %f +- %f",
		    s,r,
		    get_means(s,r)->g, get_means(s,r)->g_rms,
		    get_means(s,r)->t0, get_means(s,r)->t0_rms) ;
	}



	// for logging reasons we calc the RMS of the whole TPC but we skip unconnected pads
	if(r && get_means(s,r)->g) {
		
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

	LOG(TERR,"gain_calc: done. Mean RMS: %.3f gain, %.3f T0",g_rms,t0_rms) ;


	return ;
}
	
void tpxGain::do_default(int sector)
{
	int s, r, p ;


	// zap and create defaults in case the file is missing!
	memset(bad_fee,0,sizeof(bad_fee)) ;

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
		LOG(ERR,"from_file: error in fopen \"%s\" [%s]",fname,strerror(errno)) ;
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

			int altro ;

			// counter is in location 0!
			int c = bad_fee[s][r][0] ;


			altro = (p<<1) & 0xFF ;
			bad_fee[s][r][c+1] = altro ;

			bad_fee[s][r][0]++ ;

			LOG(INFO,"Bad FEE %d: sector %2d, RB %d, TPX-FEE %3d, TPC-FEE %3d",bad_fee[s][r][0],s,r,altro,p) ;

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
	
	f = fopen(fname,"w") ;

	if(f==0) {
		LOG(ERR,"gains: fopen \"%s\" [%s]",fname,strerror(errno)) ;
		return -1 ;
	}
	


	int s_start, s_stop ;
	if(sector) {
		s_start = sector ;
		s_stop = sector ;
	}
	else {
		s_start = 1 ;
		s_stop = 24 ;
	}

	LOG(TERR,"gains: writing to file \"%s\" for sectors %d..%d...",fname,s_start,s_stop) ;

	for(s=s_start;s<=s_stop;s++) {
	for(r=0;r<=45;r++) {
	for(p=1;p<=tpc_rowlen[r];p++) {
		struct aux *as ;
		// HACK!
//		if(get_gains(s,r,p)->t0) 

		as = get_aux(s,r,p) ;

		if(as->cou || as->low_pulse || as->high_pulse)
		fprintf(f,"%d %d %d %.5f %.5f\n",s,r,p,
			get_gains(s,r,p)->g,
			get_gains(s,r,p)->t0) ;
	}
	}
	}

	fclose(f) ;
	LOG(TERR,"gains: written.") ;

	return 0 ;
}

void tpxGain::compare(char *fname) 
{
	FILE *f ;
//	int s, r, p ;
	
	f = fopen(fname,"r") ;

	if(f==0) {
		LOG(ERR,"from_file: error in fopen \"%s\" [%s]",fname,strerror(errno)) ;
		return  ;
	}

	
	fclose(f) ;

	LOG(WARN,"compare(%s) not enabled yet!",fname) ;

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

	if(fname==0) ofile = stdout ;
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
		if(fee_found[s][r].got_one == 0) continue ;
	
		LOG(TERR,"Run had sector %d, RB %d: expect %u events, got %u",s,r,events,fee_found[s][r].got_one) ;


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

				int seen = fee_found[s][r].ch_count[a][ch] ;

				int row = tpx_altro_to_pad[r-1][a][ch].row ;
				int pad = tpx_altro_to_pad[r-1][a][ch].pad ;

				double g = get_gains(s,row,pad)->g ;
				double t0 = get_gains(s,row,pad)->t0 ;

				reason[0] = 0 ;

				if(seen != events) {

					if(seen == 0) {
						err = 3 ;
						sprintf(reason+strlen(reason),"[Missing]") ;
					}
					else {
						if(seen>events) {
							err = 3 ;
							sprintf(reason+strlen(reason),"[Flaky readout: seen %d times in %d events]",seen,events) ;
						}
					}

				}

				if(row != 0) {	// only for physically connected rows!
						

					if((g<0.9) || (g>1.1)) {							
						sprintf(reason+strlen(reason),"[Bad gain %.1f]",g) ;

						// first & last pads are usually bad
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}
					if((t0<-0.1) || (t0>0.1)) {
						sprintf(reason+strlen(reason),"[Bad t0 %.1f]",t0) ;

						// first and last are usually bad...
						if((pad<=2) || (pad>=(tpc_rowlen[row]-1))) {
							if(err<1) err = 1 ;
						}
						else {
							if(err<2) err = 2 ;
						}
					}

					if((g==0.0) && (t0>=9.0)) {	// regardless of position, there is NOTHING there
						if(err<2) err = 2 ;
					}

					
				}

				// new as of Jun 11, 08:
				// override fch to be J1 pin number as given to Tonko by Bob!
				u_char j1 = tpx_altro_to_j1[a&1][ch] ;

				if(err > 1) {
					notes++ ;
					if(log) {
						fprintf(log,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,row,pad,
							g,t0,err,reason) ;
					}
				}


				fprintf(ofile,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,row,pad,
				       g,t0,err,reason) ;

				fee_found[s][r].ch_count[a][ch] *= -1 ;	// mark as seen and done!
			}
		}

		for(a=0;a<256;a++) {
			for(int ch=0;ch<16;ch++) {
				u_char j1 = tpx_altro_to_j1[a&1][ch] ;
				if(fee_found[s][r].ch_count[a][ch] > 0) {
					int fee = tpx_altro_to_fee(r,a) ;
					notes++ ;
					if(log) {
						fprintf(log,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,-1,-1,
							0.0,0.0,3,"[Spurious channel]") ;

					}
					fprintf(ofile,"%2d %d %3d %2d %3d %2d %2d %3d %.3f %6.3f %d %s\n",s,r,fee,j1,a,ch,-1,-1,
					       0.0,0.0,3,"[Spurious channel]") ;
				}
			}
		}
					


	}
	}

	if(ofile && (ofile != stdout)) fclose(ofile) ;

	return notes ;
}

