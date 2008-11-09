#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_bsmd.h"

const char *daq_bsmd::help_string = "BSMD\n\
adc	returns bsmd_t;\n\
raw	returns raw data\n" ;


daq_bsmd::daq_bsmd(daqReader *rts_caller) 
{
	rts_id = BSMD_ID ;
	name = sfs_name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_bsmd::~daq_bsmd() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;

	return ;
}



daq_dta *daq_bsmd::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw() ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc() ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_bsmd::handle_raw()
{
	char str[128] ;
	char *full_name ;

	int bytes ;
	char *fiber_p[12] ;
	int fiber_bytes[12] ;
	int in_legacy ;
	int ret ;


	assert(caller) ;	// sanity...

	LOG(NOTE,"%s: present 0x%X",name,present) ;

	if(!present) return 0 ;

	bytes = 0 ;

	if(present & 1) {	// in datap!
		emc_reader(caller->mem, 0, 0, rts_id, fiber_p, fiber_bytes) ;
		bytes = fiber_bytes[0] ;

		LOG(DBG,"%s: found fiber 1, %d bytes",name,bytes) ;	
		in_legacy = 1 ;
	}
	else {	// SFS
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, 1, 1) ;
		full_name = caller->get_sfs_name(str) ;
		
		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
		if(!full_name) return 0 ;

		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"Got %d",bytes) ;

		if(bytes <= 0) {
			LOG(ERR,"%s: %s: not found in this event",name,str) ;
			return 0  ;
		}

		in_legacy = 0 ;
	}

	
	raw->create(bytes,"bsmd_raw",rts_id,DAQ_DTA_STRUCT(char)) ;


	char *st = (char *) raw->request(bytes) ;

	if(in_legacy) {
		memcpy(st, fiber_p[0], bytes) ;
		ret = bytes ;
	}
	else {
		ret = caller->sfs->read(str, st, bytes) ;
	}

	if(ret != bytes) {
		LOG(ERR,"%s: error in read",name) ;
		return 0 ;
	}

	raw->finalize(ret,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	raw->rewind() ;

	return raw ;
	
}

	

daq_dta *daq_bsmd::handle_adc()
{
	u_short *raw_dta ;

	daq_dta *dd = handle_raw() ;

	if(dd && dd->iterate()) {
		raw_dta = (u_short *) dd->Byte ;
	}
	else {
		return 0 ;
	}

	adc->create(1,"bsmd_adc", rts_id, DAQ_DTA_STRUCT(bsmd_t)) ;

	bsmd_t *bsmd_p = (bsmd_t *) adc->request(1) ;	// 1 object

	u_short *data = (u_short *)((u_int)raw_dta + 4 + 128) ;	// move to data start

	// FY04 data has only 30 instead of 48 so we need to zap all...
	memset(bsmd_p,0,sizeof(bsmd_t)) ;	
	
	int max_fee ;
	if(raw->ncontent < (48*192)) {	// FY04 data
		max_fee = 30 ;
	}
	else {
		max_fee = 48 ;
	}

	


	adc->finalize(1,1,1,0) ;

	adc->rewind() ;

	return adc ;
}


int daq_bsmd::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt)
{
	u_short *us = (u_short *)buff ;

	// L0 part
	trg[0].t = l2h16(us[2])*256 + l2h16(us[3]) ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// BSMD does not give the correct L0, only L2 so we invent 4
	trg[0].rhic = l2h16(us[4]) ;
	
	// L2 part
	trg[1].t = trg[0].t ;	// copy over token
	trg[1].trg = us[0] ;	// this is where the trg cmd ought to be
	trg[1].daq = us[1] ;
	trg[1].rhic = trg[0].rhic + 1 ;

	
	return 2 ;
}
