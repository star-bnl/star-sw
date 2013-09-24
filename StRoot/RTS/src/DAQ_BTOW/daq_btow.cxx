#include <sys/types.h>
#include <string.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>


#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>



#include <DAQ_EMC/daq_emc.h>	// need this for the old style stuff...

#include "daq_btow.h"


// maps received from Gerard on Mar 17,2006
const int btow_crate_map[] = {
  0x12, 0x11, 0x10, 0x1e,
  0x1d, 0x1c, 0x1b, 0x1a,
  0x19, 0x18, 0x17, 0x16,
  0x15, 0x14, 0x13, 0x01,
  0x0f, 0x0e, 0x0d, 0x0c,
  0x0b, 0x0a, 0x09, 0x08,
  0x07, 0x06, 0x05, 0x04,
  0x03, 0x02
} ;

class daq_det_btow_factory : public daq_det_factory
{
public:
	daq_det_btow_factory() {
		daq_det_factory::det_factories[BTOW_ID] = this ;
	}

	daq_det *create() {
		return new daq_btow ;
	}
} ;

static daq_det_btow_factory btow_factory ;


const char *daq_btow::help_string = "BTOW tst\n" ;



daq_btow::daq_btow(daqReader *rts_caller)
{

	LOG(DBG,"BTOW: rts_id %d, name %s",rts_id,name) ;

	rts_id  = BTOW_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "btow" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_btow::~daq_btow()
{
	LOG(DBG,"%s: destructor",name) ;

	delete adc ;
	delete raw ;

	return ;
}

/* figure out the presence... */
int daq_btow::Make()
{
	int dummy ;

	present = 0 ;	// assume not...


	assert(caller) ;
	
	evt_num++ ;

	LOG(DBG,"%s: Make()",name) ;

	if(presence()) {	// in SFS
		present |= DET_PRESENT_SFS;
		LOG(NOTE,"%s: %d: has SFS(%s)",name,evt_num,sfs_name) ;
	}
	else if(legacyDetp(rts_id, caller->mem)) {	// directly in DATAP
		present |= DET_PRESENT_DATAP ;
		LOG(NOTE,"%s: %d: has DATAP",name,evt_num) ;
	}
	else if(getEmcTrgData(caller->mem,1,&dummy)) {	// perhaps in the old TRG bank (FY08); BTOW has index 1!
		present |= DET_PRESENT_TRG ;
		LOG(NOTE,"%s: %d: has DATAP within Trigger",name,present) ;
	}
	else {
		LOG(DBG,"%s: not present",name) ;
	}



	return present ;

}
	
daq_dta *daq_btow::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	Make() ;
	if(!present) return 0 ;


	if(strcasecmp(bank,"raw") == 0) {
		return handle_raw() ;
	}
	else if(strcasecmp(bank,"adc") == 0) {
		return handle_adc() ;
	}
	else {
		LOG(ERR,"%s: unknown bank \"%s\"",name,bank) ;
		return 0 ;
	}

}


daq_dta *daq_btow::handle_raw()
{
	char *from, *st ;
	int bytes ;
	char str[256] ;	
	char *full_name = "?" ;	// just a dummy...

	from = 0 ;
	full_name = "?" ;

	assert(caller) ;


	if(present & DET_PRESENT_DATAP) {	// datap...		
		char *mem = (char *) legacyDetp(rts_id, caller->mem) ;
		from = emc_single_reader(mem, &bytes, rts_id) ;	
		if(from == 0) return 0 ;

	}
	else if(present & DET_PRESENT_TRG) {
		from = getEmcTrgData(caller->mem,1,&bytes) ;
		if(from == 0) return 0 ;

	}
	else {
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name,1,1) ;
		full_name = caller->get_sfs_name(str) ;
		if(!full_name) return 0 ;

		bytes = caller->sfs->fileSize(full_name) ;

	}

	raw->create(bytes,"btow_raw",rts_id,DAQ_DTA_STRUCT(char)) ;
	st = (char *) raw->request(bytes) ;

	if(present & DET_PRESENT_SFS) {	// from SFS...
		int ret = caller->sfs->read(full_name, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}
	}
	else {
		assert(from) ;
		memcpy(st, from, bytes) ;
	}

	raw->finalize(bytes,1,1,0) ;
	raw->rewind() ;

	LOG(DBG,"Returning raw bank...") ;
	return raw ;
}


daq_dta *daq_btow::handle_adc()
{
	u_short *raw_dta ;

	LOG(DBG,"Entering adc") ;

	daq_dta *dd = handle_raw() ;

	LOG(DBG,"raw bank %p",dd) ;

	if(dd && dd->iterate()) {
		raw_dta = (u_short *) dd->Byte ;
	}
	else {
		return 0 ;
	}


	LOG(DBG,"Got raw bank, on to adc...") ;

	adc->create(1,"adc", rts_id, DAQ_DTA_STRUCT(btow_t)) ;

	btow_t *btow_p = (btow_t *) adc->request(1) ;	// need 1 struct...


	// unpack 

	// This is really a bad hack where DDL DAQ and DDL Trigger moved the
	// whole event by 4 bytes to stay compatible with the VME BTOW Receiver

	u_short *data = (u_short *)((char *)raw_dta + 4 + 128) ;	// 4 byte dummy, 128 byte header



#if 0
	u_short *ppp = (u_short *) raw_dta ;
	for(int i=0;i<10;i++) {
		LOG(TERR,"%d: 0x%04X",i,ppp[i]) ;
	}
#endif
	
	for(int j=0;j<BTOW_PRESIZE;j++) {
		for(int i=0;i<BTOW_MAXFEE;i++) {
			btow_p->preamble[i][j] = l2h16(*data++) ;
		}
	}

	for(int j=0;j<BTOW_DATSIZE;j++) {
		for(int i=0;i<BTOW_MAXFEE;i++) {
			btow_p->adc[i][j] = l2h16(*data++) ;
		}
	}


	adc->finalize(1,1,1,0) ;
	adc->rewind() ;

	return adc ;
}


int daq_btow::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	const int BTOW_DDL_BYTES = 9972 ;
	int buff_bytes = words * 4 ;
	int rdo1 = rdo  ;
	int tcou = 0 ;

	u_short *us = (u_short *)addr ;

	u_short t_hi = l2h16(us[2]) ;
	u_short t_lo = l2h16(us[3]) ;

	int err = 0 ;

	if(buff_bytes != BTOW_DDL_BYTES) {
		err |= 1 ;
		LOG(ERR,"Received %d bytes, expect %d!?",buff_bytes,BTOW_DDL_BYTES) ;
	}

	if((t_lo & 0xFF00) || (t_hi & 0xFFF0)) {	//error
		err |= 1 ;
		LOG(ERR,"Corrupt token: t_hi 0x%04X, t_lo 0x%04X",t_hi,t_lo) ;

		// sanitize
		t_lo &= 0xFF ;
		t_hi &= 0xF ;

	}


	if(us[0] != 4) {	// was fixed only in FY13...
		//LOG(WARN,"trg cmd not 4 == 0x%04X",us[0]) ;
		us[0] = 4 ;
	}

	// L0 part
	trg[tcou].t = t_hi*256 + t_lo ;
	trg[tcou].daq = us[1] ;
	trg[tcou].trg = us[0] ;	
	trg[tcou].rhic = l2h16(us[4]) ;
	tcou++ ;



	if(trg[0].t == 0) {
		err |= 1 ;
		LOG(ERR,"token 0!") ;
	}

	if(err) {
		LOG(WARN,"RDO %d: 0x%04X 0x%04X 0x%04X 0x%04X 0x%04X",rdo1, us[0],us[1],us[2],us[3],us[4]) ;
		
	}

	if(err & 1) {	// critical
		return -1 ;
	}

	return tcou ;



}

