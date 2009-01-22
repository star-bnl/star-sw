#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daqReader.h>

#include "daq_pp2pp.h"

static int pp2pp_decode(struct pp2pp_t *d, char *raw, int bytes) ;

class daq_det_pp2pp_factory : public daq_det_factory
{
public:
        daq_det_pp2pp_factory() {
                daq_det_factory::det_factories[PP_ID] = this ;
        }

        daq_det *create() {
                return new daq_pp2pp ;
        }
} ;

static daq_det_pp2pp_factory pp2pp_factory ;



const char *daq_pp2pp::help_string = "\
\n\
PP2PP Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=pp[1..2]; \n\
	adc	returns=ptr of ADC data; c1=pp[1..2]; \n\
\n\
\n\
" ;



daq_pp2pp::daq_pp2pp(daqReader *rts_caller) 
{
	rts_id = PP_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
#ifndef PP_MVME
	if(caller) caller->insert(this, rts_id) ;
#endif
	
	raw = new daq_dta ;
	adc = new daq_dta ;
	

	LOG(DBG,"%s: constructor: caller %p, endianess %d",name,rts_caller,endianess) ;
	return ;
}

daq_pp2pp::~daq_pp2pp() 
{
	LOG(DBG,"%s: Destructor",name) ;
	if(raw) delete raw ;
	if(adc) delete adc ;

	return ;
}



daq_dta *daq_pp2pp::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;
	if(!present) return 0 ;	// this det is not in this event...


	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(sec, row) ;
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_pp2pp::handle_adc(int sec, int rdo)
{
	int min_sec, max_sec ;
	int found_some = 0 ;

	// sanity
	if(sec==-1) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<0) || (sec>MAX_SEC)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	adc->create(1,"pp2pp_t",rts_id,DAQ_DTA_STRUCT(pp2pp_t)) ;

	for(int i=min_sec;i<=max_sec;i++) {
		daq_dta *sec_dta ;

		sec_dta = handle_raw(sec, -1) ;	// rdo is ignored...
		if(sec_dta == 0) continue ;

		int ret = sec_dta->iterate() ;
		if(ret == 0) continue ;


		found_some = 1 ;
		LOG(TERR,"pp2pp adc: sector %d, words %d",i,sec_dta->ncontent) ;

		// extract modules
		struct pp2pp_t *d = (pp2pp_t *) adc->request(1) ;


		memset(d,0,sizeof(pp2pp_t)) ;
		
		ret = pp2pp_decode(d,(char *)sec_dta->Void, sec_dta->ncontent) ;
		if(ret < 0) {
			LOG(ERR,"pp2pp_decode failed for sector %d",i) ;
			continue ;
		}
		adc->finalize(1,i,d->seq_id,d->chain_id) ;
		

	}

	adc->rewind() ;

	if(found_some) return adc ;
	else return 0 ;
}


daq_dta *daq_pp2pp::handle_raw(int sec, int rdo)
{
	char str[128] ;
	char *full_name ;
	int found_some = 0 ;

	int tot_bytes ;
	int min_rdo, max_rdo ;
	int min_sec, max_sec ;
	struct {
		int sec ;
		int rb ;
		u_int bytes ;
	} obj[MAX_SEC*(MAX_RDO+1)] ;	// pp2pp has a special rdo#0 case!

	// sanity
	if(sec==-1) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<0) || (sec>MAX_SEC)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	if(rdo==-1) {
		min_rdo = 0 ;
		max_rdo = 0 ;
	}
	else if((rdo<0) || (rdo>MAX_RDO)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

#ifdef PP_MVME
	return 0 ;
#else
	// bring in the bacon from the SFS file....
	assert(caller) ;

	// calc total bytes
	tot_bytes = 0 ;
	int o_cou = 0 ;
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
	
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, s, r) ;
		full_name = caller->get_sfs_name(str) ;
		if(!full_name) continue ;

		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		int size = caller->sfs->fileSize(str) ;	// this is bytes

		LOG(DBG,"Got %d",size) ;

		if(size <= 0) {
			LOG(NOTE,"%s: %s: not found in this event",name,str) ;
			continue ;
		}
		else {
			obj[o_cou].sec = s ;
			obj[o_cou].rb = r ;
			obj[o_cou].bytes = size ;

			o_cou++ ;
	
			tot_bytes += size ;
			found_some = 1 ;
			LOG(INFO,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	raw->create(tot_bytes,"pp2pp_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	for(int i=0;i<o_cou;i++) {

		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, obj[i].sec, obj[i].rb) ;
		full_name = caller->get_sfs_name(str) ;
		if(!full_name) continue ;

		char *mem = (char *)raw->request(obj[i].bytes) ;

		int ret = caller->sfs->read(full_name, mem, obj[i].bytes) ;

                if(ret != (int)obj[i].bytes) {
                        LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
                                obj[i].bytes,ret,strerror(errno)) ;
                }
                else {
                        LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;

                }
		
		raw->finalize(obj[i].bytes, obj[i].sec, obj[i].rb, 0) ;

	}


	raw->rewind() ;

	if(found_some) return raw ;
	else return 0 ;
#endif
}

int daq_pp2pp::get_token(char *addr, int words)
{
	int cou ;
	struct daq_trg_word trg[8] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}


int daq_pp2pp::get_l2(char *addr, int words, struct daq_trg_word *trgs, int prompt)
{
	u_int *d = (u_int *)addr ;
	int t_cou = 0 ;
	u_int datum ;
	int trg_cou ;
	u_int *trg_dta ;

#ifdef OLDOLDOLD
	// for now:
	datum = d[1] ;
	datum = swap32(datum) ;	// endianess swap
	

	// shift left 1
	datum >>= 1 ;

	trgs[t_cou].t = (datum&0xF00) | ((datum & 0xF000)>>8) | ((datum & 0xF0000)>>16);
	trgs[t_cou].daq = (datum>>4) & 0xF ;
	trgs[t_cou].trg = datum & 0xF ;
	trgs[t_cou].rhic = 0 ;
	trgs[t_cou].rhic_delta = 0 ;
	t_cou++ ;

	if(prompt) {
		LOG(TERR,"    dta 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",d[0],d[1],d[2],d[3],d[4]) ;
		LOG(TERR,"    addr 0x%08X, word 0x%08X: daq %d, trg %d, T %4d, bytes %d",addr,d[1],trgs[0].daq,trgs[0].trg,trgs[0].t,words*4) ;
	}

	// I add a L2
        trgs[t_cou].t = trgs[0].t ;
        trgs[t_cou].daq = 0 ;
        trgs[t_cou].trg = 15 ;
        trgs[t_cou].rhic = 1 ;
        trgs[t_cou].rhic_delta = 1 ;
        t_cou++ ;
#endif

	if(prompt) {

		LOG(TERR,"words %d: dta 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",words,d[0],d[1],d[2],d[3],d[4]) ;

	}
	// get count
	trg_cou = d[words-1] ;
	// move to start
	trg_dta = &(d[words-1-trg_cou]) ;

	for(int i=0;i<trg_cou;i++) {
		datum = trg_dta[i] ;
		
		if((datum & 0xFF000000) == 0xEE000000) {	// prompt

			trgs[t_cou].t = (datum&0xF00) | ((datum & 0xF000)>>8) | ((datum & 0xF0000)>>16);
			trgs[t_cou].daq = (datum>>4) & 0xF ;
			trgs[t_cou].trg = datum & 0xF ;
			trgs[t_cou].rhic = 0 ;
			trgs[t_cou].rhic_delta = 0 ;

			if(prompt) {
				LOG(TERR,"T %4d (prompt): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
			}

			t_cou++ ;
			break ;
		}

	}

	if(t_cou == 0) {	// no prompt contrib! invent token 4097

		trgs[t_cou].t = 4097 ;
		trgs[t_cou].daq = 0 ;
		trgs[t_cou].trg = 5 ;
		trgs[t_cou].rhic = 0 ;
		trgs[t_cou].rhic_delta = 0 ;

		if(prompt) {
			LOG(TERR,"T %4d (no data): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
		}

		t_cou++ ;


	}

	for(int i=0;i<trg_cou;i++) {
		datum = trg_dta[i] ;
		
		if((datum & 0xFF000000) != 0xEE000000) {	// FIFO!

			trgs[t_cou].t = (datum&0xF00) | ((datum & 0xF000)>>8) | ((datum & 0xF0000)>>16);
			trgs[t_cou].daq = (datum>>4) & 0xF ;
			trgs[t_cou].trg = datum & 0xF ;
			trgs[t_cou].rhic = 1 ;
			trgs[t_cou].rhic_delta = 1 ;

			if(prompt) {
				LOG(TERR,"T %4d (FIFO): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
			}


			// keep only L2 commands!
			switch(trgs[t_cou].trg) {
			case 13 :
			case 15 :
				break ;
			default :
				continue ;
			}

			if(trgs[t_cou].t == 0) {
				LOG(ERR,"T %4d (FIFO): daq %d, trg %d -- token 0, skipping",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;				
			}
			else {
				t_cou++ ;
			}
		}

	}



	return t_cou ;	
}




static int pp2pp_decode(struct pp2pp_t *d, char *raw, int bytes) 
{
	u_int *d32 ;

	d32 = (u_int *) raw ;	// data is BIG ENDIAN!

	// we are still debugging!
	
	for(int i=0;i<10;i++) {
		LOG(TERR,"pp2pp data: %2d: 0x%08X",i,b2h32(d32[i])) ;
	}

	return 0 ;	// call it OK for now...
}
