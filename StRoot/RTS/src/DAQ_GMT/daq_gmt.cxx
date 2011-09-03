#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_gmt.h"


const char *daq_gmt::help_string = "GMT\n\
adc	returns gmt_t;\n\
raw	returns raw data\n" ;

class daq_det_gmt_factory : public daq_det_factory
{
public:
	daq_det_gmt_factory() {
		daq_det_factory::det_factories[GMT_ID] = this ;
	}

	daq_det *create() {
		return new daq_gmt ;
	}
} ;

static daq_det_gmt_factory gmt_factory ;


daq_gmt::daq_gmt(daqReader *rts_caller) 
{
	rts_id = GMT_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "gmt" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_gmt::~daq_gmt() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;


	return ;
}



daq_dta *daq_gmt::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;

	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(rdo) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_gmt::handle_raw(int rdo)
{
	char *st ;
	int r_start, r_stop ;
	int bytes ;

	assert(caller) ;	// sanity...

	if(!present) {
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}

	char str[256] ;
	char *full_name ;


	if(rdo<=0) {
		r_start = 1 ;
		r_stop = 1 ;	
	}
	else {
		r_start = r_stop = rdo ;
	}


	raw->create(8*1024,"gmt_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

	for(int r=r_start;r<=r_stop;r++) {
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, 1, r) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) continue ;
		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes


		st = (char *) raw->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

	
		raw->finalize(bytes,0,r,0) ;	// sector 0;
	}

	raw->rewind() ;

	return raw ;
	
}

	
