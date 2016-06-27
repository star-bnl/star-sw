#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_esmd.h"

int esmd_crate_map[] = {
  0x40, 0x41, 0x42, 0x43,
  0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b,
  0x4c, 0x4d, 0x4e, 0x4f,
  0x50, 0x51, 0x52, 0x53,
  0x54, 0x55, 0x56, 0x57,
  0x58, 0x59, 0x5a, 0x5b,
  0x5c, 0x5d, 0x5e, 0x5f,
  0x60, 0x61, 0x62, 0x63,
  0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b,
  0x6c, 0x6d, 0x6e, 0x6f
};

const char *daq_esmd::help_string = "ESMD\n\
adc	returns esmd_t;\n\
raw	returns raw data\n" ;

class daq_det_esmd_factory : public daq_det_factory
{
public:
	daq_det_esmd_factory() {
		daq_det_factory::det_factories[ESMD_ID] = this ;
	}

	daq_det *create() {
		return new daq_esmd ;
	}
} ;

static daq_det_esmd_factory esmd_factory ;


daq_esmd::daq_esmd(daqReader *rts_caller) 
{
	rts_id = ESMD_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "esmd" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_esmd::~daq_esmd() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;

	return ;
}



daq_dta *daq_esmd::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{	
	Make() ;
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



daq_dta *daq_esmd::handle_raw()
{
	char *from , *st ;
	int bytes ;

	assert(caller) ;	// sanity...



	if(!present) {
		LOG(ERR,"%s: not present?",name) ;
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *mem = (char *)legacyDetp(rts_id, caller->mem) ;
		from = emc_single_reader(mem, &bytes, rts_id) ;
		if(from == 0) return 0 ;


		raw->create(bytes,"esmd_raw",rts_id,DAQ_DTA_STRUCT(char)) ;
		st = (char *) raw->request(bytes) ;



		memcpy(st, from, bytes) ;

	}
	else {	// SFS
		char str[256] ;
		char *full_name ;

		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, 1, 1) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) return 0 ;

		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

		raw->create(bytes,"esmd_raw",rts_id,DAQ_DTA_STRUCT(char)) ;
		st = (char *) raw->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}
	}

	
	raw->finalize(bytes,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	raw->rewind() ;

	return raw ;
	
}

	

daq_dta *daq_esmd::handle_adc()
{
	u_short *raw_dta ;

	daq_dta *dd = handle_raw() ;

	LOG(DBG,"%s: got raw %p",name,dd) ;

	if(dd && dd->iterate()) {
		raw_dta = (u_short *) dd->Byte ;
	}
	else {
		return 0 ;
	}

	adc->create(1,"esmd_adc", rts_id, DAQ_DTA_STRUCT(esmd_t)) ;

	esmd_t *esmd_p = (esmd_t *) adc->request(1) ;	// 1 object


		
	u_short *data = (u_short *)((char *)raw_dta + 128) ;	

	if(present & DET_PRESENT_DATAP) {
		data += 2 ;	// for old, VME based RBs we need to also skip 4 bytes of junk
	}

	// FY04 data has only 30 instead of 48 so we need to zap all...
	memset(esmd_p,0,sizeof(esmd_t)) ;	


	
	int max_fee ;
	if(raw->ncontent < (48*192)) {	// FY04 data
		max_fee = 30 ;
	}
	else {
		max_fee = 48 ;
	}

	

	
	for(int j=0;j<ESMD_PRESIZE;j++) {
		for(int i=0;i<max_fee;i++) {
			esmd_p->preamble[i][j] = l2h16(*data++) ;
		}
	}

	for(int j=0;j<ESMD_DATSIZE;j++) {
		for(int i=0;i<max_fee;i++) {
			esmd_p->adc[i][j] = l2h16(*data++) ;
		}
	}



	adc->finalize(1,1,1,0) ;

	adc->rewind() ;

	return adc ;
}


int daq_esmd::get_l2(char *buff, int buff_words, struct daq_trg_word *trg, int rdo1)
{
	const int ESMD_DDL_BYTES = 18948 ;
	int buff_bytes = buff_words * 4 ;

	u_short *us = (u_short *)buff ;

	u_short t_hi = l2h16(us[2]) ;
	u_short t_lo = l2h16(us[3]) ;

	int err = 0 ;

	if(buff_bytes != ESMD_DDL_BYTES) {
		err |= 1 ;
		LOG(ERR,"Received %d bytes, expect %d!?",buff_bytes,ESMD_DDL_BYTES) ;
	}

	if((t_lo & 0xFF00) || (t_hi & 0xFFF0)) {	//error
		err |= 1 ;
		LOG(ERR,"Corrupt token: t_hi 0x%04X, t_lo 0x%04X",t_hi,t_lo) ;

		// sanitize
		t_lo &= 0xFF ;
		t_hi &= 0xF ;

	}


	if(us[0] != 4) {	// Gerard fixed the trigger command
		//LOG(WARN,"trg_cmd %d?",us[0]) ;
		us[0] = 4 ;
	}

	// L0 part
	trg[0].t = t_hi*256 + t_lo ;
	trg[0].daq = us[1] ;	
	trg[0].trg = us[0] ;	
	trg[0].rhic = l2h16(us[4]) ;
	

//	if(us[0] != 0xF) {
//		err |= 1 ;
//		LOG(ERR,"trg cmd not 15 == 0x%04X",us[0]) ;
//	}

	if(trg[0].t == 0) {
		err |= 1 ;
		LOG(ERR,"token 0!") ;
	}

	if(err) {
		LOG(WARN,"RDO %d error: 0x%04X 0x%04X 0x%04X 0x%04X 0x%04X",rdo1, us[0],us[1],us[2],us[3],us[4]) ;
		
	}

	if(err & 1) {	// critical
		return -1 ;
	}

	return 1 ;
}
