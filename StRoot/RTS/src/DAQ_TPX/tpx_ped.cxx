#include <math.h>
#include <errno.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <rtsLog.h>


#include <DAQ_TPX/tpxCore.h>
#include <RTS_READER/daq_algo.h>
#include <RTS_READER/daq_dta.h>

#define TPX_PED_FNAME	"/RTScache/tpx_pedestals_s%02d_r%02d.txt"

class tpx_ped : public daq_algo
{
public:
	tpx_ped(const char *c="tpx_ped") {
		algo_name = c ;
		memset(peds,0,sizeof(peds)) ;
		LOG(INFO,"%s: constructor",algo_name) ;
	} ;

	~tpx_ped() ;

	int Make(daq_dta *inp) ;	// from Offline
	int Make(void *r, char *o=0, int max_bytes=0) ;	// from Online

	int InitRun(int run) ;
	int FinishRun(int run) ;

	int from_file(const char *fname=0) ;
	int to_file(const char *fname=0) ;


//	void set_defaults(int sec, int rdo) ;
//	void check_defaults() ;
//	int min_sec, max_sec ;
//	int min_rdo, max_rdo ;

	static const int MAX_SECTOR = 24 ;
	static const int MAX_RDO = 6 ;

	// local to TPX
	void *provide(int s, int r, int clear=0) ;

	int smooth() ;
	int to_altro(char *buff, int rb) ;
	int kill_bad(int row, int pad) ;



	void *peds[MAX_SECTOR+1][MAX_RDO+1] ;
} ;


struct tpx_peds_c {
	u_char aid ;
	u_char ch ;
	u_char row ;
	u_char pad ;
	u_short cou ;
	float ped[512] ;
	float rms[512] ;
} ; 


tpx_ped::~tpx_ped()
{
	for(int s=0;s<=MAX_SECTOR;s++) {
	for(int r=0;r<=MAX_RDO;r++) {
		if(peds[s][r]) free(peds[s][r]) ;
	}
	}
}

#if 0
void tpx_ped::set_defaults(int sec, int rdo)
{
	def_sector = sec ;
	def_rdo = rdo ;

	check_defaults() ;
}

void tpx_ped::check_defaults()
{
	if(def_sector <= 0) {
		min_sec = 1 ;
		max_sec = MAX_SECTOR ;
	}
	else {
		min_sec = max_sec = def_sector ;
	}

	if(def_rdo <= 0) {
		min_rdo = 1 ;
		max_rdo = MAX_RDO ;
	}
	else {
		min_rdo = max_rdo = def_rdo ;
	}
}
#endif

void *tpx_ped::provide(int s, int r, int clear)
{
	int bytes = 72*16*sizeof(tpx_peds_c) ;

	if(peds[s][r] == 0) {

		peds[s][r]  = malloc(bytes) ;
		RTS_ASSERT(peds[s][r]) ;

		clear = 1 ;


	}

	if(clear) {
		memset(peds[s][r],0,bytes) ;
	}

	return peds[s][r] ;
}

/*
	InitRun is called when we actually calculate pedestals!

	from_file() is called in all other cases!

	
*/
int tpx_ped::InitRun(int run)
{
	evt_num = 0 ;

	for(int s=0;s<=MAX_SECTOR;s++) {
	for(int r=0;r<=MAX_RDO;r++) {
		if(peds[s][r]) {
			free(peds[s][r]) ;
			peds[s][r] = 0 ;

		}
	}
	}

	return 0 ;
}


int tpx_ped::Make(void *rdesc, char *out_buffer, int max_bytes)
{
	LOG(INFO,"%s: hook_Make()",algo_name) ;

	evt_num++ ;

	tpx_rdo_event *rdo ;
	tpx_altro_struct a ;
	int s, r ;

	rdo = (tpx_rdo_event *) rdesc ;

	// skip non-data type!

	s = rdo->sector ;
	r = rdo->rdo ;
		
	struct tpx_peds_c (*p)[16] = (struct tpx_peds_c (*)[16]) provide(s,r) ;


	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo->rdo - 1 ;
	a.t = rdo->token ;

	u_int *data_end = rdo->data_end ;

	do {
		data_end = tpx_scan_to_next(data_end, rdo->data_start, &a) ;
	


		struct tpx_peds_c *use = 0 ;

		int empty = -1 ;
		for(int i=0;i<72;i++) {
			if((p[i][0].pad != 0) && (p[i][0].aid == a.id)) {	// AID taken
				use = &(p[i][a.ch]);
				break ;
			}
			else if(p[i][0].pad == 0) {
				empty = i ;
			}
		}

		if(use==0) {	// not found
			if(empty < 0) {
				LOG(ERR,"Too many altros") ;
				continue ;
			}

			// mark as taken
			p[empty][0].aid = a.id ;
			p[empty][0].pad = 1 ;	// mark as taken


			use = &(p[empty][a.ch])  ;
			use->aid = a.id ;
			use->ch = a.ch ;
			use->row = a.row ;
			use->pad = a.pad ;
		}


		if(use->cou >= 1000) continue ;	// I stop at 1000 counts
		if(a.count == 0) continue ;


		use->cou++ ;

		for(int i=0;i<a.count;i++) {
			double da ;

			da = (double) a.adc[i] ;

			use->ped[a.tb[i]] += da ;
			use->rms[a.tb[i]] += da*da ;
		
		}


	} while(data_end && (data_end > rdo->data_start)) ;

	
	return 0 ;
} ;



	
int tpx_ped::from_file(const char *fpattern)
{
	int reloaded = 0 ;

	check_defaults() ;

	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<max_rdo;r++) {
		char fname[128] ;
		FILE *f ;

		if(fpattern==0) fpattern = TPX_PED_FNAME ;

		sprintf(fname,fpattern,s,r) ;

		if(check_file(fname)==0) continue ;	// no reload necessary!



		f = fopen(fname,"r") ;
		if(f==0) {
			LOG(WARN,"%s: fopen(%s) [%s]",algo_name,fname,strerror(errno)) ;
			continue ;
		}

		reloaded = 1 ;

		struct tpx_peds_c (*p)[16] = (struct tpx_peds_c (*)[16]) provide(s,r,1) ;
		
		while(!feof(f)) {
			int row, pad, t ;
			float fped, frms ;

			int ret = fscanf(f,"%d %d %d %f %f",&row,&pad,&t,&fped,&frms) ;
			if(ret != 5) continue ;

			int rdo, aid, ach ;
			tpx_to_altro(row,pad,rdo,aid,ach) ;
 
			if(rdo != r) {
				LOG(ERR,"Expect rdo %d, got %d",r,rdo) ;
			}

			struct tpx_peds_c *use = 0 ;

			int empty = -1 ;
			for(int i=0;i<72;i++) {
				if((p[i][0].pad != 0) && (p[i][0].aid == aid)) {	// AID taken
					use = &(p[i][ach]);
					break ;
				}
				else if(p[i][0].pad == 0) {
					empty = i ;
				}
			}

			if(use==0) {	// not found
				if(empty < 0) {
					LOG(ERR,"Too many altros") ;
					continue ;
				}

				// mark as taken
				p[empty][0].aid = aid ;
				p[empty][0].pad = 1 ;	// mark as taken


				use = &(p[empty][ach])  ;

				use->aid = aid ;
				use->ch = ach ;
				use->row = row ;
				use->pad = pad ;
			}


			use->cou++ ;

			use->ped[t] = fped ;
			use->rms[t] = frms ;

		}

		fclose(f) ;

	}
	}


	return reloaded ;
};

int tpx_ped::to_file(const char *pattern)
{
	if(pattern == 0) pattern = TPX_PED_FNAME ;

	for(int s=1;s<=MAX_SECTOR;s++) {
	for(int r=1;r<=MAX_RDO;r++) {
		if(peds[s][r] == 0) continue ;


		char fname[32] ;
		FILE *f ;

		sprintf(fname,pattern,s,r) ;

		f = fopen(fname,"w") ;
		if(f == 0) {
			LOG(ERR,"%s: fopen(%s) [%s]",algo_name,fname,strerror(errno)) ;
			continue ;
		}


		struct tpx_peds_c (*p)[16] = (struct tpx_peds_c (*)[16]) peds[s][r] ;

		for(int i=0;i<72;i++) {
			if(p[i][0].pad == 0) continue ;

			for(int c=0;c<16;c++) {
				for(int t=0;t<512;t++) {
					fprintf(f,"%d %d %d %.3f %.3f\n",
						p[i][c].row, p[i][c].pad, t, 
						p[i][c].ped[t], p[i][c].rms[t]) ;
				}
			}
		}

		fclose(f) ;
	}	
	}


	return 0 ;
}

int tpx_ped::FinishRun(int orun)
{
	LOG(INFO,"%s: FinishRun(%d)",algo_name,orun) ;

	for(int s=1;s<=MAX_SECTOR;s++) {
	for(int r=1;r<=MAX_RDO;r++) {

		struct tpx_peds_c (*p)[16] = (struct tpx_peds_c (*)[16]) peds[s][r] ;

		if(p==0) continue ;

		for(int a=0;a<72;a++) {

			if(p[a][0].pad == 0) continue ;	// never seen in data...

			for(int c=0;c<16;c++) {
				struct tpx_peds_c *use = &(p[a][c]) ;

				if(use->cou) {
					for(int t=0;t<512;t++) {
						use->ped[t] /= use->cou ;

						use->rms[t] /= use->cou ;
						use->rms[t] = sqrt(use->ped[t] * use->ped[t] - use->rms[t]) ;
					}

				}

			}
		}

	}
	}


	return 0 ;
}

int tpx_ped::Make(daq_dta *dd)
{
	int s, r ;

	evt_num++ ;

	// I expect the "raw" bank -- I hope it.s there!


	while(dd->iterate()) {	// over sectors RDOs...
		struct tpx_rdo_event rdo ;
				
		s = dd->sec ;
		r = dd->rdo ;

		int words = dd->ncontent / 4 ;
		char *rdo_ptr = (char *)dd->Byte ;

		RTS_ASSERT(rdo_ptr) ;
		RTS_ASSERT(words) ;

		int ret = tpx_get_start(rdo_ptr, words, &rdo, 0) ;
		if(ret < 0) {
			LOG(ERR,"%s: sector %d, rdo %d",algo_name,s,r) ;
		}

		Make(&rdo,0,0) ;
	}
	
	return 0 ;
}

#if 0			
int tpx_ped::to_altro()
{
	smooth() ;

	// aply bad pads, perhaps?

}
	
#endif	
