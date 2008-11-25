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
		present |= 2;
	}
	else if(legacyDetp(rts_id, caller->mem)) {	// directly in DATAP
		present |= 1 ;
	}
	else if(getEmcTrgData(caller->mem,1,&dummy)) {	// perhaps in the old TRG bank (FY08); BTOW has index 1!
		present |= 4 ;
	}

	switch(present) {
	case 1 :
		LOG(NOTE,"%s: %d: has DATAP",name,evt_num) ;
		break ;
	case 2 :
		LOG(NOTE,"%s: %d: has SFS(%s)",name,evt_num,sfs_name) ;
		break ;
	case 4 :
		LOG(NOTE,"%s: %d: has DATAP within Trigger",name,present) ;
		break ;
	default:
		LOG(NOTE,"%s: not present",name) ;
		break ;
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


	if(present & 1) {	// datap...		
		char *mem = (char *) legacyDetp(rts_id, caller->mem) ;
		from = emc_single_reader(mem, &bytes, rts_id) ;	
		if(from == 0) return 0 ;

	}
	else if(present & 4) {
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

	if(present & 2) {	// from SFS...
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
	u_short *data = (u_short *)((u_int)raw_dta + 4 + 128) ;	// 4 byte dummy, 128 byte header
	
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
