#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_sst.h"


const char *daq_sst::help_string = "SST\n\
raw	returns raw data\n" ;

class daq_det_sst_factory : public daq_det_factory
{
public:
	daq_det_sst_factory() {
		daq_det_factory::det_factories[SST_ID] = this ;
	}

	daq_det *create() {
		return new daq_sst ;
	}
} ;

static daq_det_sst_factory sst_factory ;


daq_sst::daq_sst(daqReader *rts_caller) 
{
	rts_id = SST_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "sst" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	ped = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_sst::~daq_sst() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete ped ;

	return ;
}



daq_dta *daq_sst::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;

	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,rdo) ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(sec,rdo) ;
	}
	else if(strcasecmp(bank,"pedrms")==0) {
		return handle_ped(sec) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}




/*
	mode = 0     just check
	mode = 1     insert int adc strucutres
	mode = 2     pedestal calculation
*/
int daq_sst::raw_to_adc_utility(int s, int r, char *rdobuff, int words, daq_sst_ped_t *peds, int mode)
{
	u_int *dta = (u_int *)rdobuff ;

	u_int *d32 = dta ;
	u_int *d32_end = dta + words ;
	u_int *d32_start = dta ;

	int adc_count = 0 ;

	events[r-1]++ ;

	int e = events[r-1] ;

	//sanity!
	if(d32_end[-1] != 0xBBBBBBBB) {
		LOG(ERR,"S%d-%d: %u: last word is 0x%08X, expect 0xBBBBBBBB -- data corrupt,skipping!",s,r,e,d32_end[-1]) ;
		return -1 ;
	}

	
	//we'll assume SST has bugs in the header so we don't do any checks but search for the data
	//immediatelly
	int found = 0 ;
	while(d32<d32_end) {
		u_int d = *d32++ ;

		if(d == 0xDDDDDDDD) {
			d32-- ;	// move back ;
			found = 1 ;
			break ;
		}
	}

	if(!found) {
		LOG(ERR,"S%d-%d: %u: can't find 0xDDDDDDDD -- data corrupt, skipping!",s,r,e) ;
		return -1 ;
	}


	int fib = 0 ;
	while(d32<d32_end) {
		u_int d = *d32++ ;

		if(d != 0xDDDDDDDD) {
			u_int *d_here = d32 - 1 ;	// go back one
			LOG(ERR,"S%d-%d: %u: fiber %d: can't find 0xDDDDDDDD at offset %d [0x%08X] -- data corrupt, skipping!",s,r,e,fib,
			    d_here-d32_start,*d_here) ;

			d_here -= 2 ;
			for(int i=0;i<5;i++) {
				LOG(ERR,"     %d: 0x%08X",d_here-d32_start,*d_here) ;
				d_here++ ;
			}
			goto err_ret ;
		}

		u_int fiber_id = d32[0] ;
		// fiber id: 0xfffwwwwm
		// fff-> flags 000 OK, 001 empty, 002 overflow
		// wwww -> word length
		// m -> mode: 0 raw, 1 ped-subtracted

		int words = fiber_id & 0x000FFFF0 ;
		words >>= 4 ;

		LOG(NOTE,"S%d-%d: fiber %d: ID 0x%08X, words %d",
		    s,r,fib,fiber_id,words) ;
		
		int known_data_format = 1 ;	// assume I know what I'm doing...

		if(fiber_id & 0xFFF00000) {
			switch(fiber_id) {
			case 0x001000A0 :	// empty fiber in raw mode
			case 0x001000A1 :	// empty fiber in ZS mode
				LOG(NOTE,"S%d-%d: %u: fiber %d: empty [0x%08X]",s,r,e,fib,fiber_id) ;					
				break ;
			case 0x002100A1 : //overflow in pedestal subtracted mode *shrug*
				LOG(NOTE,"S%d-%d: %u: fiber %d: overflow [0x%08X]",s,r,e,fib,fiber_id) ;
				break ;
			default:
				LOG(WARN,"S%d-%d: %u: fiber %d: odd fiber_id [0x%08X]",s,r,e,fib,fiber_id) ;
				known_data_format = 0 ;
				break ;
			}
		}

		int pipe_mode = fiber_id & 0xF ;

		switch(pipe_mode) {
		case 0 :	//RAW
			break ;
		case 1 :	//COMPRESSED (aka ZS) ;
			known_data_format = 2 ;
			break ;
		case 3 :	//Common Mode
			known_data_format = 2 ;
			break ;
		case 2 :	//ZS but with RAW 
		case 4 :	//Commong Mode with RAW
		default :
			LOG(ERR,"S%d-%d: %u: fiber %d: unknown pipe mode 0x%X (0x%08X)",s,r,e,fib,pipe_mode,fiber_id) ;
			goto err_ret ;			
		}

/*
			
		if((fiber_id & 0xF) != 0) {
			if((fiber_id & 0xF)==1) {	// ZS data
				LOG(NOTE,"S%d-%d: %u: fiber %d: ZS data 0x%08X...",s,r,e,fib,fiber_id) ;
				known_data_format = 2 ;
				//goto err_ret ;			
			}
			else {
				LOG(ERR,"S%d-%d: %u: fiber %d: unknown data mode 0x%08X",s,r,e,fib,fiber_id) ;
				goto err_ret ;			
			}


		}
*/



		words -= 1 ;	// for some reason...

		//first 9 words are some header
		for(int i=0;i<12;i++) {
			LOG(NOTE,"   %d: 0x%08X",i,d32[i]) ;
		}
			
		d32 += 9 ;	// skip this header
		words -= 9 ;	// adjust the remaining words...

		// If I don't know what the data format is, I'll skip decoding...
		if(!known_data_format) {
			LOG(WARN,"Unknown data format") ;
			d32 += words ;
			fib++ ;
			if(fib==8) break ;
			continue ;
		}

		// want only non-ZS data in pedestal runs!
		if((mode==2) && (known_data_format != 1)) {			
			LOG(ERR,"S%d-%d: %u: fiber %d: ZS data 0x%08X in pedestal runs is not allowed",s,r,e,fib,fiber_id) ;
			d32 += words ;
			fib++ ;
			if(fib==8) break ;
			continue ;
		}


		int strip = 0 ;
		int hybrid = 0 ;


		daq_sst_data_t *sst = 0 ;
		daq_sst_data_t *sst_start  ;

		switch(mode) {
		case 1 :	// used in the DAQ Reader
			if(words) {
				sst = (daq_sst_data_t *)adc->request(3*words) ;
			}
			break ;
		case 2 :	// used during real-time pedestal calc

			break ;
		default :
			mode = 0 ;	// just check the data...
			break ;
		}

		sst_start = sst ;

		int no_error = 1 ;
		switch(known_data_format) {
		case 2 :	// ZS data

			for(int i=0;i<words;i++) {
				d = *d32++ ;

				int strip = (d & 0xFFC000)>>14 ;

				if(mode==1) {
					sst->adc = d & 0x3FF ;
					sst->hybrid = (d & 0x3C00)>>10 ;
					sst->strip = strip ;
				}
				if(strip >= 768) {
					LOG(ERR,"S%d-%d: fiber %d, bad strip %d",s,r,fib,strip) ;
					no_error = 0 ;
				}
				sst++ ;

			}
			//assume all OK with fiber
			if(no_error) fiber_events[r-1][fib]++ ;

			break ;
		case 1 :
			
		

		//here is the ADC
		for(int i=0;i<words;i++) {
			d = *d32++ ;
			int adc_prime ;
			int aadc ;

			if(strip >= 768) {
				LOG(ERR,"S%d-%d: fiber %d, bad strip %d",s,r,fib,strip) ;
				goto err_ret ;
			}
					
			aadc = d & 0x3FF ;
			switch(mode) {
			case 1 :
				sst->strip = strip ;	
				sst->hybrid = hybrid ;
				sst->adc = aadc ;
				break ;
			case 2 :
				adc_prime = (aadc+SST_PED_ADC_OFFSET)%1024 ;
				if(adc_prime >= (1024-SST_OUTLIERBAND)) ;
				else if(adc_prime <= (SST_OUTLIERBAND)) ;
				else {
					if(peds->cou[fib][hybrid][strip] < 0xFFF0) {	// protect the 16bit counter
						peds->ped[fib][hybrid][strip] += adc_prime ;
						peds->rms[fib][hybrid][strip] += adc_prime * adc_prime ;
						peds->cou[fib][hybrid][strip]++ ;
					}
				}
				break ;
			}
			hybrid++ ;
			sst++ ;

			if(hybrid==16) {
				hybrid = 0 ;
				strip++ ;
			}

			////////////////////////////

			if(strip >= 768) {
				LOG(ERR,"S%d-%d: fiber %d, bad strip %d",s,r,fib,strip) ;
				goto err_ret ;
			}

			aadc = (d & 0xFFC00)  >> 10 ;
			switch(mode) {
			case 1 :
				sst->strip = strip ;
				sst->hybrid = hybrid ;
				sst->adc = aadc ;
				break ;
			case 2 :
				adc_prime = (aadc+SST_PED_ADC_OFFSET)%1024 ;
				if(adc_prime >= (1024-SST_OUTLIERBAND)) ;
				else if(adc_prime <= (SST_OUTLIERBAND)) ;
				else {
					if(peds->cou[fib][hybrid][strip] < 0xFFF0) {	// protect the 16bit counter
						peds->ped[fib][hybrid][strip] += adc_prime ;
						peds->rms[fib][hybrid][strip] += adc_prime * adc_prime ;
						peds->cou[fib][hybrid][strip]++ ;
					}
				}


				break ;

			}
			hybrid++ ;
			sst++ ;
				
			if(hybrid==16) {
				hybrid = 0 ;
				strip++ ;
			}


			///////////////////////////////

			if(strip >= 768) {
				LOG(ERR,"S%d-%d: fiber %d, bad strip %d",s,r,fib,strip) ;
				goto err_ret ;
			}

			aadc = (d & 0x3FF00000) >> 20 ;
			switch(mode) {
			case 1 :
				sst->strip = strip ;
				sst->hybrid = hybrid ;
				sst->adc = aadc ; 
				break ;
			case 2 :
				adc_prime = (aadc+SST_PED_ADC_OFFSET)%1024 ;

				if(adc_prime >= (1024-SST_OUTLIERBAND)) ;
				else if(adc_prime <= (SST_OUTLIERBAND)) ;
				else {
					if(peds->cou[fib][hybrid][strip] < 0xFFF0) {	// protect the 16bit counter
						peds->ped[fib][hybrid][strip] += adc_prime ;
						peds->rms[fib][hybrid][strip] += adc_prime * adc_prime ;
						peds->cou[fib][hybrid][strip]++ ;
					}
				}
				break ;
			}
			hybrid++ ;
			sst++ ;

			if(hybrid==16) {
				hybrid = 0 ;
				strip++ ;
			}

		}
		//all OK with fiber; came to the end and counted all the necessary strips
		if(words==(SST_STRIP_COU*SST_HYBRID_COU)/3) fiber_events[r-1][fib]++ ;

		break ;
		}
			
		//end of adc
		if((mode==1) && (sst-sst_start)) {
			LOG(NOTE,"Got %d structs, requested %d",sst-sst_start,3*words) ;
			adc->finalize(sst-sst_start,s,r,fib) ;
		}

		adc_count += sst-sst_start ;

		
		LOG(NOTE,"RDO %d, fiber %d: words %d",r,fib,words) ;

		fib++ ;
		if(fib==8) break ;

			
	}

	if(*d32 != 0xCCCCCCCC) {
		LOG(ERR,"S%d-%d: %u: can't find 0xCCCCCCCC at offset %d [0x%08X] -- data corrupt, skipping!",s,r,e,d32-d32_start,*d32) ;
		d32 -= 2 ;
		while(d32 < d32_end) {
			LOG(ERR,"    %d: 0x%08X",d32-d32_start,*d32) ;
			d32++ ;
		}
		goto err_ret ;
	}
		

	LOG(NOTE,"%d %d -- returing %d",s,r,adc_count) ;

	return adc_count  ;	// objects

	err_ret:;

	return -1 ;
}


daq_dta *daq_sst::handle_adc(int sec, int rdo, char *rdobuff, int words)
{
	int r_start, r_stop ;
	int s_start, s_stop ;



	if(sec <= 0) {
		s_start = 1 ;
		s_stop = 2 ;
	}
	else {
		s_start = s_stop = sec ;
	}

	if(rdo<=0) {
		r_start = 1 ;
		r_stop = 3 ;		// 1 sector has 3, 2nd has 2
	}
	else {
		r_start = r_stop = rdo ;
	}


	adc->create(128,"sst_adc",rts_id,DAQ_DTA_STRUCT(daq_sst_data_t)) ;

	LOG(NOTE,"handle_adc: %d %d, %d %d",s_start,s_stop,r_start,r_stop) ;

	for(int s=s_start;s<=s_stop;s++) {

	for(int r=r_start;r<=r_stop;r++) {
		u_int *dta ;

		if(rdobuff == 0) {
			daq_dta *raw_d = handle_raw(s,r) ;
			if(raw_d == 0) continue ;
			if(raw_d->iterate() == 0) continue ;

			dta = (u_int *) raw_d->Void ;
			words = raw_d->ncontent/4 ;

			//for ERRORs printounts
			daq_trg_word trg[120] ;
			get_l2((char *)dta,words,trg,r) ;

		}
		else {
			dta = (u_int *) rdobuff ;

//			LOG(WARN,"Running from buffer: words %d, RDO %d",words,r) ;
		} ;


		raw_to_adc_utility(s,r,(char *)dta,words,0,1) ;


	}	// end of loop over RDOs [1..3]

	}	// end of loop over Sectors [1..2]

	adc->rewind() ;

	return adc ;
	
}


daq_dta *daq_sst::handle_raw(int sec, int rdo)
{
	char *st ;
	int r_start, r_stop ;
	int s_start, s_stop ;
	int bytes ;
	char str[256] ;
	char *full_name ;


	assert(caller) ;	// sanity...

	if(!present) {
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}



	if(sec <= 0) {
		s_start = 1 ;
		s_stop = 2 ;
	}
	else {
		s_start = s_stop = sec ;
	}

	if(rdo<=0) {
		r_start = 1 ;
		r_stop = 3 ;		// 1 sector has 3, 2nd has 2
	}
	else {
		r_start = r_stop = rdo ;
	}


	raw->create(8*1024,"sst_raw",rts_id,DAQ_DTA_STRUCT(char)) ;


	for(int s=s_start;s<=s_stop;s++) {

	for(int r=r_start;r<=r_stop;r++) {
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, s, r) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) continue ;
		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes


		st = (char *) raw->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

	
		raw->finalize(bytes,s,r,0) ;	;

	}	// end of loop over RDOs [1..3]

	}	// end of loop over Sectors [1..2]

	raw->rewind() ;

	return raw ;
	
}

	
int daq_sst::get_l2(char *buff, int words, struct daq_trg_word *trg, int rdo)
{
	// will look the same as PXL!
	int t_cou = 0 ;
	u_int *d32 = (u_int *)buff ;
	u_int err = 0 ;
	int last_ix = words - 1 ;
	int token, daq_cmd, trg_cmd ;
	u_int token_word ;

	// quick sanity checks...
	// errors in the lower 16 bits are critical in the sense that
	// I'm unsure about the token and events coherency.
	// Critical errors issue ERROR
	// Others issue WARN

	if(d32[0] != 0xAAAAAAAA) err |= 0x10000 ;			// header error
	if(d32[last_ix] != 0xBBBBBBBB) err |= 0x1	;	// trailer error

	token_word = d32[1] ;	// default

	//let's enumerate observed errors depending on location of 0xDDDDDDDD
	int got_dd = -1 ;
	for(int i=0;i<12;i++) {	// search, oh, 12 words only...
		if(d32[i] == 0xDDDDDDDD) {
			got_dd = i ;
			break ;
		}
	}

	switch(got_dd) {
	case 7 :		//no 0xAAAAAAAA at all
		token_word = d32[0] ;
		err |= 0x20000 ;
		break ;
	case 8 :		// this is normal
		break ;
	case 9 :		// double 0xAAAAAAAA
		token_word = d32[2] ;
		err |= 0x40000 ;	
		break ;
	case -1 :	// none found
		if(token_word == 0x22200000) ;	// trigger only event?
		else {
			err |= 0x2 ;	// unknown case so far...
		}
		break ;
	default :
		err |= 0x2 ;	// unknown case so far...
		break ;
	}


	// special TCD-only event check; is this implemented by SST?? Should be!
	if(token_word == 0x22200000) {
		LOG(WARN,"RDO %d: trigger-only event...",rdo) ;
		token = 4097 ;
		daq_cmd = 0 ;
		trg_cmd = 4 ;
	}
	else {	
		//check for USB trigger
		if(token_word & 0xFFF00000) {	// USB trigger?? Can't be
			err |= 4 ;
		}

		token = token_word & 0xFFF ;
		daq_cmd = (token_word & 0xF000) >> 12 ;
		trg_cmd = (token_word & 0xF0000) >> 16 ;
	}

	// more sanity
	if(token == 0) {
		token = 4097 ;	// override with dummy token!
		err |= 4 ;
	}


	if(trg_cmd != 4) err |= 4 ;

	trg[t_cou].t = token ;
	trg[t_cou].daq = daq_cmd ;
	trg[t_cou].trg = trg_cmd ;
	trg[t_cou].rhic = d32[7] ;
	trg[t_cou].rhic_delta = 0 ;
	t_cou++ ;

#if 0	
	// get other trigger commands...
	int last_p = last_ix - 1 ;	// at CRC

	//u_int crc = d32[last_p] ;


	last_p-- ;	// at end of TCD info
	int first_trg = -1 ;		

	for(int i=last_p;i>=0;i--) {
		if(d32[i] == 0xCCCCCCCC) {	// trigger commands header...
			first_trg = i + 1 ;
			break ;
		}
	}

	if(first_trg > 0) {	// found other trigger commands...
		for(int i=first_trg;i<=last_p;i++) {
			trg[t_cou].t = d32[i] & 0xFFF ;
			trg[t_cou].daq = (d32[i] & 0xF000) >> 12 ;
			trg[t_cou].trg = (d32[i] & 0xF0000) >> 16 ;
			trg[t_cou].rhic = trg[0].rhic + 1 ;	// mock it up...
			trg[t_cou].rhic_delta = 0 ;

			switch(trg[t_cou].trg) {
			case 0xF :
			case 0xE :
			case 0xD :
				break ;
			default :
				continue ;
			}

			t_cou++ ;

			if(t_cou >= 120) {	// put a sanity limiter...
				err |= 8 ;
				break ;
			}
		}
	}
#endif

	//err = t_cou ;
	if(err & 0xFFFF) {
		LOG(ERR,"RDO %d: error 0x%X, t_cou %d",rdo,err,t_cou) ;

		for(int i=0;i<16;i++) {
			LOG(ERR,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}

		int s = last_ix - 10 ;
		if(s < 0) s = 0 ;

		for(int i=s;i<=last_ix;i++) {
			LOG(ERR,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}

		//HACK
		trg[0].t = 4097 ;	// kill this guy...
		if(t_cou) t_cou = 1 ;	// disregard anything else as well...
	}
	else if(err & 0xFFFF0000) {	//non critical, warnings

		LOG(WARN,"RDO %d: error 0x%X, t_cou %d",rdo,err,t_cou) ;

		for(int i=0;i<16;i++) {
			LOG(WARN,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}

		int s = last_ix - 10 ;
		if(s < 0) s = 0 ;

		for(int i=s;i<=last_ix;i++) {
			LOG(WARN,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}



	}

	return t_cou ;
}

daq_dta *daq_sst::handle_ped(int sec)
{




	char str[128] ;
	char *full_name ;
	int bytes ;
	u_short *d, *d_in ;
	int s_start, s_stop ;


	LOG(NOTE,"handle_ped(%d)",sec) ;

	if(sec<=0) {
		s_start = 1 ;
		s_stop = 2 ;
	}
	else {
		s_start = s_stop = sec ;
	}

	ped->create(8,"sst_pedrms",rts_id,DAQ_DTA_STRUCT(daq_sst_pedrms_t)) ;

	for(sec=s_start;sec<=s_stop;sec++) {

	sprintf(str,"%s/sec%02d/pedrms",sfs_name, sec) ;

	LOG(NOTE,"Trying %s",str) ;

	full_name = caller->get_sfs_name(str) ;
		
	if(full_name) {
		LOG(NOTE,"full_name %s",full_name) ;
	}

	if(!full_name) continue  ;
	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(NOTE,"bytes %d",bytes) ;

	d = (u_short *) malloc(bytes) ;
	d_in = d ;
			
	int ret = caller->sfs->read(str, (char *)d, bytes) ;
	if(ret != bytes) {
		LOG(ERR,"ret is %d") ;
	}

	if(d[0] != 0xBEEF) {
		LOG(ERR,"Bad pedestal version") ;
	}

	if(d[1] != 1 ) {
		LOG(ERR,"Bad pedestal version") ;
	}

	int rdo_cou = d[2] ;
	int fib_cou = d[3] ;
	int hy_cou = d[4] ;
	int strip_cou = d[5] ;


	d += 6 ;	// skip header

//	int max_ix = (bytes/2) ;

	daq_sst_pedrms_t *f_ped = 0 ;

	for(int r=0;r<rdo_cou;r++) {
		int rdo1 = *d++ ;

		for(int f=0;f<fib_cou;f++) {

			f_ped = (daq_sst_pedrms_t *) ped->request(1) ;

			for(int h=0;h<hy_cou;h++) {
			for(int s=0;s<strip_cou;s++) {
				short ped = (short)*d++ ;
				short rms = (short)*d++ ;

				f_ped->ped[h][s] = ped ;
				f_ped->rms[h][s] = rms ;

			}
			}

			ped->finalize(1,sec,rdo1,f) ;
		}

	}

	free(d_in) ;

	}

	ped->rewind() ;


	return ped ;
}


