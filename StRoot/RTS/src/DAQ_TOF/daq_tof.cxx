#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>

#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>

#include "daq_tof.h"






const char *daq_tof::help_string = "\
\n\
TOF Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;



daq_tof::daq_tof(const char *dname, rts_reader *rts_caller) 
{
	rts_id = TOF_ID ;
	name = rts2name(rts_id) ;

	raw = new daq_dta ;

	caller = rts_caller ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_tof::~daq_tof() 
{
	LOG(DBG,"%s: DEstructor",name) ;
	delete raw ;

	return ;
}



daq_dta *daq_tof::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
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



daq_dta *daq_tof::handle_raw(int sec, int rdo)
{
	char str[128] ;
	int tot_bytes ;
	int min_rdo, max_rdo ;
	int min_sec, max_sec ;
	struct {
		int sec ;
		int rb ;
		u_int bytes ;
	} obj[MAX_SEC*MAX_RDO] ;

	// sanity
	sec = 1 ;	// nothing else for now...
	min_sec = max_sec = sec ;

	if(rdo==-1) {
		min_rdo = 1 ;
		max_rdo = MAX_RDO ;
	}
	else if((rdo<1) || (rdo>MAX_RDO)) return 0 ;
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
	
		sprintf(str,"%s/%s/sec%02d/rb%02d/raw",caller->fs_cur_evt, "tof", s, r) ;
	
		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		int size = caller->sfs->fileSize(str) ;	// this is bytes

		LOG(DBG,"Got %d",size) ;

		if(size <= 0) {
			LOG(DBG,"%s: %s: not found in this event",name,str) ;
			continue ;
		}
		else {
			obj[o_cou].sec = s ;
			obj[o_cou].rb = r ;
			obj[o_cou].bytes = size ;

			o_cou++ ;
	
			tot_bytes += size ;
			LOG(NOTE,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	raw->create(tot_bytes,(char *)name,rts_id,DAQ_DTA_STRUCT(u_char)) ;

	for(int i=0;i<o_cou;i++) {

		sprintf(str,"%s/%s/sec%02d/rb%02d/raw",caller->fs_cur_evt, "tof", obj[i].sec, obj[i].rb) ;

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


// knows how to get the token out of an event...
int daq_tof::get_token(char *addr, int words)
{
	int cou ;
	struct daq_trg_word trg[128] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;	// special marker...
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}

// knows how to get a/the L2 command out of the event...
int daq_tof::get_l2(char *addr, int words, struct daq_trg_word *trg, int prompt)
{
	u_int *w ;
	int cou = 0 ;
	int t_cou = 0 ;


	w = (u_int *)addr ;
	words-- ;	// point to last datum now...

	LOG(NOTE,"First words 0x%08X 0x%08X 0x%08X, last words 0x%08X 0x%08X 0x%08X [+0x%08X], %u",
	    w[0],w[1],w[2],w[words-2],w[words-1],w[words],w[words+1],words+1) ;


	// prompt token is in word 0!
	trg[t_cou].t = w[0] & 0xFFF ;
	trg[t_cou].daq = (w[0]>>12) & 0xF ;
	trg[t_cou].trg = (w[0]>>16) & 0xF ;
	trg[t_cou].rhic = 0 ;
	trg[t_cou].rhic_delta = 0 ;
	t_cou++ ;


	if(prompt) LOG(NOTE,"[%d] prompt: T %4d, trg %d, daq %d",prompt,trg[0].t,trg[0].trg,trg[0].daq) ;
	if(trg[0].t == 0) {
		LOG(ERR,"[%d] prompt: T %4d, trg %d, daq %d",prompt,trg[0].t,trg[0].trg,trg[0].daq) ;
		trg[0].t = 4097 ;
	}

	if((w[0] >> 28) != 0xA) {
		LOG(ERR,"First word in data has an incorrect signature 0x%08X",w[0]) ;
		trg[0].t = 4097 ;
	}

	// move backwards to the start of the trigger block
	while(words) {
		if((w[words] >> 28)==0xA) {	// trigger stuff
			// fish trigger command
			int daq = (w[words]>>12) & 0xF ; 
			int cmd = (w[words]>>16) & 0xF ;
			int t = (w[words]) & 0xFFF ;
			
			if(prompt) LOG(NOTE,"   [%d] FIFO %d: T %4d, trg %d, daq %d",prompt,cou,t,cmd,daq);

			words-- ;
			cou++ ;
		}
		else {
			break ;
		}
	}



	words++ ;	// move forward to start of trigger

	// words not points to the first trigger of the FIFO block

	for(int i=0;i<cou;i++) {
		
		trg[t_cou].t = w[words+i] & 0xFFF ;
		trg[t_cou].daq = (w[words+i]>>12) & 0xF ;
		trg[t_cou].trg = (w[words+i]>>16) & 0xF ;
		trg[t_cou].rhic = i+1 ;
		trg[t_cou].rhic_delta = i+1 ;

		if(trg[t_cou].t == 0) {
			LOG(ERR,"   [%d] FIFO %d: T %4d, trg %d, daq %d",prompt,i,trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq);
			continue ;
		}

		// we will take all the non-L2 components here...
		switch(trg[t_cou].trg) {
		case 13 :
		case 15 :
			break ;
		default :		// take out ALL L0 commands!
			continue ;
		}

		t_cou++ ;
	}


	return t_cou ;
}
