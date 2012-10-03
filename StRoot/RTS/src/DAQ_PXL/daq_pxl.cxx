#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_pxl.h"


const char *daq_pxl::help_string = "PXL\n\
raw	returns raw data\n" ;

// for PXL proper
class daq_det_pxl_factory : public daq_det_factory
{
public:
	daq_det_pxl_factory() {
		daq_det_factory::det_factories[PXL_ID] = this ;
	}

	daq_det *create() {
		return new daq_pxl ;
	}
} ;

static daq_det_pxl_factory pxl_factory ;


daq_pxl::daq_pxl(daqReader *rts_caller) 
{
	rts_id = PXL_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "pxl" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_pxl::~daq_pxl() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	return ;
}



daq_dta *daq_pxl::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;

	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,rdo) ;
	}

	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_pxl::handle_raw(int sec, int rdo)
{
	char *st ;
	int r_start, r_stop ;
	int s_start, s_stop ;
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


	if(sec<=0) {
		s_start = 1 ;
		s_stop = 2 ;
	}
	else {
		s_start = s_stop = sec ;
	}

	if(rdo<=0) {
		r_start = 1 ;
		r_stop = 5 ;	
	}
	else {
		r_start = r_stop = rdo ;
	}


	raw->create(8*1024,"pxl_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

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

	
			raw->finalize(bytes,s,r,0) ;
		}
	}

	raw->rewind() ;

	return raw ;
	
}

	




// used to grab trigger info from the event header
int daq_pxl::get_l2(char *buff, int words, struct daq_trg_word *trg, int rdo)
{
	int t_cou = 0 ;
	u_int *d32 = (u_int *)buff ;


	int token = d32[1] & 0xFFF ;
	int daq_cmd = (d32[1] & 0xF000) >> 12 ;
	int trg_cmd = (d32[1] & 0xF0000) >> 16 ;

	trg[t_cou].t = token ;
	trg[t_cou].daq = daq_cmd ;
	trg[t_cou].trg = trg_cmd ;
	trg[t_cou].rhic = d32[7] ;
	t_cou++ ;

	return t_cou ;
	
	
}

