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
	u_int err = 0 ;
	int last_ix = words - 1 ;
	int token, daq_cmd, trg_cmd ;

	// quick sanity checks...
	if(d32[0] != 0xAAAAAAAA) err |= 1 ;	// header error
	if(d32[last_ix] != 0xBBBBBBBB) err |= 2	;	// trailer error
//	if((d32[1] & 0xFFF00000) != 0xCCC00000) err |= 8 ;	// junk in trigger/daq/token
	if((d32[1] & 0xFFF00000) != 0xC0000000) err |= 8 ;	// junk in trigger/daq/token
	
	// special TCD-only event check
//	if(d32[1] == 0xCCC0FFFF) {
	if(d32[1] == 0xC000FFFF) {
		LOG(NOTE,"RDO %d: trigger-only event...",rdo) ;
		token = 4097 ;
		daq_cmd = 0 ;
		trg_cmd = 4 ;
	}
	else {
	
		token = d32[1] & 0xFFF ;
		daq_cmd = (d32[1] & 0xF000) >> 12 ;
		trg_cmd = (d32[1] & 0xF0000) >> 16 ;
	}

	// more sanity
	if(err || (token == 0)) {
		token = 4097 ;	// override with dummy token!
		err |= 8 ;
	}

	if(trg_cmd != 4) err |= 8 ;

	trg[t_cou].t = token ;
	trg[t_cou].daq = daq_cmd ;
	trg[t_cou].trg = trg_cmd ;
	trg[t_cou].rhic = d32[7] ;
	trg[t_cou].rhic_delta = 0 ;
	t_cou++ ;

	
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
				err |= 4 ;
				break ;
			}
		}
	}

	//err = t_cou ;
	if(err) {
		LOG(ERR,"RDO %d: error 0x%X, t_cou %d",rdo,err,t_cou) ;

		for(int i=0;i<16;i++) {
			LOG(ERR,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}

		int s = last_ix - 10 ;
		if(s < 0) s = 0 ;

		for(int i=s;i<=last_ix;i++) {
			LOG(ERR,"  RDO %d: %2d/%2d: 0x%08X",rdo,i,words,d32[i]) ;
		}


	}


	return t_cou ;
	
	
}

