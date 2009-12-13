#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_sc.h"

extern int sc_reader(char *m, struct sc_t *sc, u_int driver) ;


class daq_det_sc_factory : public daq_det_factory
{
public:
	daq_det_sc_factory() {
		//fprintf(stderr,"SC: inserting into factory\n") ;
		daq_det_factory::det_factories[SC_ID] = this ;
	}

	daq_det *create() {
		//fprintf(stderr,"SC: creating\n") ;
		return new daq_sc ;
	}
} ;

static daq_det_sc_factory sc_factory ;



const char *daq_sc::help_string = "SC tst\n" ;



daq_sc::daq_sc(daqReader *rts_caller) : daq_det(rts_caller)
{
	LOG(DBG,"SC: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = SC_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "scd" ;	
	
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;
	raw = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_sc::~daq_sc()
{
	LOG(DBG,"%s: destructor",name) ;

	delete legacy ;
	delete raw ;

	return ;
}

	
daq_dta *daq_sc::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	Make() ;
	if(!present) return 0 ;


	if(!bank || strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") == 0) {
		return handle_legacy() ;
	}
	else if(strcasecmp(bank,"raw") == 0) {
		return handle_raw() ;
	}

	LOG(ERR,"%s: unknown bank %s",name,bank) ;
	return 0 ;


}


daq_dta *daq_sc::handle_raw()
{
	int err = 0 ;
	char *local_store = 0 ;
	int ret ;
	char str[256] ;
	char *full_name ;
	int bytes ;

	if(!(present & DET_PRESENT_SFS)) return 0 ;	// no SFS -> no raw



	sprintf(str,"%s",sfs_name) ;
	full_name = caller->get_sfs_name(str) ;

	if(!full_name) {
		err = 1 ;
		goto ret_error ;
	}

	bytes = caller->sfs->fileSize(full_name) ;
	if(bytes <= 0) {
		err = 2 ;
		goto ret_error ;
	}

	raw->create(bytes,"sc_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

	local_store = (char *) raw->request(bytes) ;

	ret = caller->sfs->read(str, local_store, bytes) ;
	if(ret != bytes) {
		err = 3 ;
		goto ret_error ;
	}


	raw->finalize(bytes,0,0,0) ;
	raw->rewind() ;

	return raw ;

	ret_error:;

	LOG(ERR,"%s: handle_raw: error %d",name,err) ;
	return 0 ;
	
}

daq_dta *daq_sc::handle_legacy()
{

	int err = 0 ;


	// I need one object of sc_t type but let the create decide on the necessary size
	legacy->create(1,"sc_t",rts_id,DAQ_DTA_STRUCT(sc_t)) ;
	

	sc_t *sc_p = (sc_t *) legacy->request(1) ;	// need ONE sc_t object


	if(present & DET_PRESENT_DATAP) { 	
		sc_reader(caller->mem, sc_p, m_Debug) ;
	}
	else {	// SFS
		daq_dta *dd = handle_raw() ;

		if(dd==0) {
			err = 1 ;
			goto ret_error ;
		}

		dd->iterate() ;	// do it once...


		struct SCD *scd = (struct SCD *) dd->Void ;
		
		int swapit = 0 ;
		if(scd->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {	// swap
			swapit = 1 ;
		}

		LOG(DBG,"Need to swap SCD(sfs) %d?",swapit) ;

		sc_p->time = qswap32(swapit,scd->time) ;

		u_int tmp_f = qswap32(swapit,scd->mag_field) ;
		memcpy(&(sc_p->mag_field),&tmp_f,4) ;
		

		sc_p->timelag = 0 ;	// I don't have DATAP! I can't tell the time difference!
		sc_p->valid = 1 ;	// what else can I say????

		for(u_int i=0;i<(sizeof(sc_p->rich_scalers)/4);i++) {
			sc_p->rich_scalers[i] = qswap32(swapit, scd->rich_scalers[i]) ;
		}

	}

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;

	ret_error :;

	LOG(ERR,"%s: handle_legacy: error %d",name,err) ;

	return 0 ;
}
