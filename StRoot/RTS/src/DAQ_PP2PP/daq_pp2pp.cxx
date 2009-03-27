#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daqReader.h>

#include "daq_pp2pp.h"



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
	name = rts2name(rts_id) ;
	sfs_name = "pp2pp" ;
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

		sec_dta = handle_raw(i, -1) ;	// rdo is ignored...
		if(sec_dta == 0) continue ;

		int ret = sec_dta->iterate() ;
		if(ret == 0) continue ;


		found_some = 1 ;
		LOG(NOTE,"pp2pp adc: sector %d, words %d",i,sec_dta->ncontent) ;

		// extract modules
		ret = decode(i,(char *)sec_dta->Void, sec_dta->ncontent) ;
		if(ret < 0) {
			LOG(ERR,"pp2pp_decode failed for sector %d",i) ;
			continue ;
		}		
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
			LOG(NOTE,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	if(o_cou == 0) return 0 ;

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


	if(prompt) {
		LOG(DBG,"words %d: dta 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",words,d[0],d[1],d[2],d[3],d[4]) ;

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
				LOG(NOTE,"T %4d (prompt): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
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
			LOG(NOTE,"T %4d (no data): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
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
				LOG(NOTE,"T %4d (FIFO): daq %d, trg %d",trgs[t_cou].t,trgs[t_cou].daq,trgs[t_cou].trg) ;
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




int daq_pp2pp::decode(int sec_id, char *raw, int bytes) 
{
	u_int *d32 ;
	u_short *d16 ;
	u_short seq[2], trg[2] ;
	u_char *d8 ;
	int ret = 0  ;

	int seq_id, chain_id, svx_id ;

	int words = bytes/4 ;

	// we are still debugging!
	d32 = (u_int *) raw ;	// data is BIG ENDIAN!
	d16 = (u_short *) raw ;

	for(int i=0;i<bytes/4;i++) {
		LOG(DBG,"pp2pp data: %2d: 0x%08X",i,b2h32(d32[i])) ;
	}

	int trg_cou = b2h32(d32[words-1]) ;
	int trg_ix = words - 1 - trg_cou ;

	for(int i=0;i<trg_cou;i++) {
		LOG(NOTE,"pp2pp: trg %d/%d: 0x%08X",i+1,trg_cou,b2h32(d32[trg_ix+i])) ;
	}


	int w16 = 2 * (words - trg_cou - 1) ;	// 16 bit words left

	LOG(DBG,"16bit words left: %d",w16) ;

	int cur_ix = 0 ;
	int next_good_ix = 0 ;

	int bunch_xing = -1 ;
	u_int trigger = 0xFFFFFFFF ;

	while(cur_ix < w16) {
		struct pp2pp_t *d = 0 ;	// to provoke a core dump
		int requested = 0 ;

		int i ;

		// grab sequencer descriptor
		seq[0] = b2h16(d16[cur_ix]) ;
		cur_ix++ ;
		seq[1] = b2h16(d16[cur_ix]) ;
		cur_ix++ ;

		// grab trigger data, although I will ignore it...
		trg[0] = b2h16(d16[cur_ix]) ;
		cur_ix++ ;
		trg[1] = b2h16(d16[cur_ix]) ;
		cur_ix++ ;

		seq_id = (seq[0] >> 8) >> 2 ;
		chain_id = (seq[0] >> 8) & 3 ;


		if(trigger == 0xFFFFFFFF) {
			trigger = (trg[1] << 16) | trg[0] ;
		}
		else {
			u_int tmp = (trg[1] << 16) | trg[0] ;

			if(tmp != trigger) {
				ret |= 1 ;
				LOG(ERR,"pp2pp: seq %d:%d: expect trigger 0x%08X, read 0x%08X",seq_id,chain_id,trigger,tmp) ;
				return ret ;
			}
			else {
				LOG(DBG,"pp2pp: seq %d:%d: expect trigger 0x%08X, read 0x%08X",seq_id,chain_id,trigger,tmp) ;
			}
		}

		if(bunch_xing < 0) {
			bunch_xing = seq[0] & 0xFF ;
		}
		else {
			int tmp = seq[0] & 0xFF ;

			if(tmp != bunch_xing) {
				ret |= 2 ;
				LOG(ERR,"pp2pp: seq %d:%d: expect xing 0x%02X, read 0x%02X",seq_id,chain_id,bunch_xing,tmp) ;
				return ret ;
			}
			else {
				LOG(DBG,"pp2pp: seq %d:%d: expect xing 0x%02X, read 0x%02X",seq_id,chain_id,bunch_xing,tmp) ;
			}
		}





		int fifo_w16 = (seq[1] >>8) | ((seq[1] & 0xF)<<8) ;
		
		LOG(DBG,"seq 0x%04X, 0x%04X; trg 0x%04X, 0x%04X; len %d",seq[0],seq[1],trg[0],trg[1],fifo_w16) ;
		LOG(NOTE,"pp2pp: seq_id %d:%d; words %d",seq_id,chain_id,fifo_w16) ;

		// subtract 2 because of the trigger word
		fifo_w16 -= 2 ;

		d8 = (u_char *) &(d16[cur_ix]) ;
		
		next_good_ix = cur_ix + fifo_w16 ;
		if(fifo_w16 & 1) {	// padding
			next_good_ix++ ;
		}

		for(i=0;i<fifo_w16;i++) {
			int ch, c_adc ;

			ch = *d8++ ;
			c_adc = *d8++ ;

			LOG(DBG,"Word %d/%d: ch %d, c_adc %d",i,fifo_w16,ch,c_adc) ;

			if(ch & 0x80) {
				if(c_adc != 0) {
					ret |= 4 ;
					LOG(ERR,"Bad channel in seq %d:%d: %d %d",seq_id,chain_id,ch,c_adc) ;
					break ;
				}
				else {
					LOG(NOTE,"SVX break: seq %d:%d: SVX 0x%02X",seq_id, chain_id,ch) ;
			 	}

				if(requested) {
					adc->finalize(1, sec_id, d->seq_id, d->chain_id) ;
					requested = 0 ;
				}

				svx_id = ch & 0x7F ;	
				d = (struct pp2pp_t *) adc->request(1) ;

				requested = 1 ;

				d->seq_id = seq_id  ;
				d->chain_id = chain_id ;
				d->svx_id = svx_id  ;

				d->error = ret ;
				memset(d->adc,0,sizeof(d->adc)) ;		

			}
			else {				
				if(!requested) {
					LOG(ERR,"Bad data -- SVX ID was not found in sequencer %d, chain %d  (0x%X 0x%X)",seq_id, chain_id, ch,c_adc) ;
					ret |=4 ;
					break ;
				}
				else {
					LOG(DBG,"datum %d/%d: %3d = 0x%02X",i,fifo_w16,ch,c_adc) ;
					d->adc[ch] = c_adc ;
				}
			}

			cur_ix++ ;
		}


			
		if(fifo_w16 & 1) {
			int ch, c_adc ;

			ch = *d8++ ;
			c_adc = *d8++ ;

			LOG(DBG,"Padding %d/%d: %3d = 0x%02X",i,fifo_w16,ch,c_adc) ;

			cur_ix++ ;
		}

		if(requested) {
			adc->finalize(1, sec_id, d->seq_id, d->chain_id) ;
			requested = 0 ;
		}
		
		cur_ix = next_good_ix ;

	}

	return ret ;
}
