#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <RTS_READER/daq_dta.h>
#include <RTS_READER/rts_reader.h>

#include "daq_pp2pp.h"






const char *daq_pp2pp::help_string = "\
\n\
PP2PP Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..2]; c2=rdo[0..0]; \n\
\n\
\n\
" ;



daq_pp2pp::daq_pp2pp(const char *dname, rts_reader *rts_caller) 
{
	rts_id = PP_ID ;
	name = rts2name(rts_id) ;

	raw = new daq_dta ;

	caller = rts_caller ;
	

	LOG(DBG,"%s: constructor: caller %p, endianess %d",name,rts_caller,endianess) ;
	return ;
}

daq_pp2pp::~daq_pp2pp() 
{
	LOG(DBG,"%s: DEstructor",name) ;
	delete raw ;

	return ;
}



daq_dta *daq_pp2pp::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	if(!presence()) return 0 ;	// this det is not in this event...


	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}



daq_dta *daq_pp2pp::handle_raw(int sec, int rdo)
{
	char str[128] ;
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

	// bring in the bacon from the SFS file....
	assert(caller) ;

	// calc total bytes
	tot_bytes = 0 ;
	int o_cou = 0 ;
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
	
		sprintf(str,"%s/%s/sec%02d/rb%02d/raw",caller->fs_cur_evt, "pp2pp", s, r) ;
	
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
			LOG(INFO,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	raw->create(tot_bytes,(char *)name,rts_id,DAQ_DTA_STRUCT(u_char)) ;

	for(int i=0;i<o_cou;i++) {

		sprintf(str,"%s/%s/sec%02d/rb%02d/raw",caller->fs_cur_evt, "pp2pp", obj[i].sec, obj[i].rb) ;

		daq_store *st = raw->get() ;

		st->sec = obj[i].sec ;
		st->row = obj[i].rb ;
		st->nitems = obj[i].bytes ;

		char *mem = (char *)(st + 1) ;

		int ret = caller->sfs->read(str, mem, st->nitems) ;

                if(ret != (int)st->nitems) {
                        LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
                                st->nitems,ret,strerror(errno)) ;
                }
                else {
                        LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
                        raw->commit() ;
                }


	}

	raw->rewind() ;
	return raw ;
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

	
	return t_cou ;	
}
