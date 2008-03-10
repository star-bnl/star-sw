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
