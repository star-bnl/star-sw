#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>




#include <SFS/sfs_index.h>
#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>


#include "daq_esmd.h"

extern int emc_reader(char *m, int rts_id, char *retval[12], int retbytes[12]) ;

daq_esmd::daq_esmd(const char *dname, rts_reader *rts_caller) 
{
	rts_id = ESMD_ID ;
	name = rts2name(rts_id) ;

	caller = rts_caller ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	preamble = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_esmd::~daq_esmd() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete preamble ;

	return ;
}



daq_dta *daq_esmd::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{


	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcasecmp(bank,"raw")==0) {
		return handle_raw() ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc() ;
	}
	else if(strcasecmp(bank,"preamble")==0) {
		return handle_preamble() ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_esmd::handle_raw()
{
	char str[128] ;
	int bytes ;
	char *fiber_p[12] ;
	int fiber_bytes[12] ;
	int in_legacy ;
	int ret ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	if(!presence()) {	// not in SFS
		
		// try legacy DATAP
		if(emc_reader(caller->legacy_p, rts_id, fiber_p, fiber_bytes)==0) {	// not in legacy
			return 0 ;
		}

		bytes = fiber_bytes[0] ;
		in_legacy = 1 ;		
		
	}
	else {
		sprintf(str,"%s/%s/sec%02d/rb%02d/raw",caller->fs_cur_evt, name, 1, 1) ;

		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		bytes = caller->sfs->fileSize(str) ;	// this is bytes

		LOG(DBG,"Got %d",bytes) ;

		if(bytes <= 0) {
			LOG(NOTE,"%s: %s: not found in this event",name,str) ;
			return 0  ;
		}

		in_legacy = 0 ;
	}

	
	raw->create(bytes,(char *)name,rts_id,DAQ_DTA_STRUCT(u_short)) ;


	char *st = (char *) raw->request(bytes/2) ;

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

	raw->finalize(ret/2,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...


	raw->rewind() ;
	return raw ;
}

daq_dta *daq_esmd::handle_adc()
{
	u_short (*data)[192] ;
	int max_fee ;
	int ix ;

	if(handle_raw()==0) return 0 ;


	adc->create(raw->ncontent*2,(char *)name, rts_id, DAQ_DTA_STRUCT(u_short)) ;


	data = (u_short (*)[192]) adc->request(48*192) ;	// objecst

	// FY04 data has only 30 instead of 48 so we need to zap this
	memset(data,0,sizeof(short)*48*192) ;	
	
	if(raw->ncontent < (48*192)) {	// FY04 data
		max_fee = 30 ;
	}
	else {
		max_fee = 48 ;
	}

	// skip preamble and header (64)
	ix = 64 + max_fee * 4 ;
	
	for(int j=0;j<192;j++) {
		for(int i=0;i<max_fee;i++) {
			data[i][j] = raw->Short[ix++] ;
		}
	}

	adc->finalize(48*192,1,1,0) ;

	adc->rewind() ;

	return adc ;
}

daq_dta *daq_esmd::handle_preamble()
{
	u_short (*data)[4] ;
	int max_fee ;
	int ix ;

	if(handle_raw()==0) return 0 ;


	preamble->create(raw->ncontent*2,(char *)name, rts_id, DAQ_DTA_STRUCT(u_short)) ;


	data = (u_short (*)[4]) preamble->request(48*4) ;	// objecst

	// FY04 data has only 30 instead of 48 so we need to zap this
	memset(data,0,sizeof(short)*48*4) ;	
	
	if(raw->ncontent < (48*192)) {	// FY04 data
		max_fee = 30 ;
	}
	else {
		max_fee = 48 ;
	}

	// skip header (64)
	ix = 64  ;
	
	for(int j=0;j<4;j++) {
		for(int i=0;i<max_fee;i++) {
			data[i][j] = raw->Short[ix++] ;
		}
	}

	preamble->finalize(48*4,1,1,0) ;

	preamble->rewind() ;

	return preamble ;

}

int daq_esmd::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt)
{
	u_short *us = (u_short *)buff ;

	trg[0].t = l2h16(us[2])*256 + l2h16(us[3]) ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// ESMD does not give the correct L0, only L2
	trg[0].rhic = l2h16(us[4]) ;
	
	trg[1].t = trg[0].t ;
	trg[1].trg = us[0] ;
	trg[1].daq = us[1] ;
	trg[1].rhic = trg[0].rhic + 1 ;

	
	return 2 ;
}
