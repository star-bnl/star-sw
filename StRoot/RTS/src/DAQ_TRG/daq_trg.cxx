#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_trg.h"


const char *daq_trg::help_string = "TRG tst\n" ;

class daq_det_trg_factory : public daq_det_factory
{
public:
        daq_det_trg_factory() {
                daq_det_factory::det_factories[TRG_ID] = this ;
        }

        daq_det *create() {
                return new daq_trg ;
        }
} ;

static daq_det_trg_factory trg_factory ;


extern int trg_reader(char *m, struct trg_t *trg, u_int driver, u_int daqbits) ;
extern char *trg_find_raw(char *m, int *bytes); 

daq_trg::daq_trg(daqReader *rts_caller) : daq_det(rts_caller)
{
	LOG(DBG,"TRG: rts_id %d, name %s, caller %p",rts_id,name,caller) ;

	// dname is ignored 
	rts_id  = TRG_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "trg" ;
	
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;
	raw = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_trg::~daq_trg()
{
	LOG(DBG,"%s: destructor",name) ;

	delete legacy ;
	delete raw ;

	return ;
}

	
daq_dta *daq_trg::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	Make() ;
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") == 0) {
		return handle_legacy() ;
	}
	else if(strcasecmp(bank,"raw") == 0) {
		return handle_raw() ;
	}
	else {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
	}

	return 0 ;

}


daq_dta *daq_trg::handle_legacy()
{

	if(!(present & DET_PRESENT_DATAP)) {
		LOG(NOTE,"%s: can't have legacy without DATAP (post FY08), yet...",name) ;
		return 0 ;
	}

	// I need one object of trg_t type but let the create decide on the necessary size
	legacy->create(1,"trg_t",rts_id,DAQ_DTA_STRUCT(trg_t)) ;
	

	trg_t *trg_p = (trg_t *) legacy->request(1) ;	// need ONE trg_t object

	// dabits hack...
	trg_reader(caller->mem, trg_p, m_Debug,caller->daqbits) ;


	trg_p->daqbits = caller->daqbits ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}

daq_dta *daq_trg::handle_raw()
{
	int err = 0 ;
	int bytes = 0 ;
	char *ptr = 0 ;
	char str[256] ;
	char *full_name ;
	int ret ;

	if(present & DET_PRESENT_DATAP) {	// old DATAP based
		ptr = trg_find_raw(caller->mem, &bytes) ;

		LOG(DBG,"%s: raw from DATAP: %d bytes",name,bytes) ;

		if((ptr == 0) || (bytes == 0)) {
			err = 1 ;
			goto ret_error ;
		}


		raw->create(bytes,"trg_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

		char *where = (char *) raw->request(bytes) ;
		memcpy(where, ptr, bytes) ;
		


	}
	else {	// new SFS based
		sprintf(str,"%s",sfs_name) ;
		full_name = caller->get_sfs_name(str) ;

		if(!full_name) {
			err = 2 ;
			goto ret_error ;
		}

		bytes = caller->sfs->fileSize(full_name) ;
		if(bytes <= 0) {
			err = 3 ;
			goto ret_error ;
		}

		LOG(DBG,"%s(%s): full_name %s, bytes %d",name,sfs_name,full_name,bytes) ;

		raw->create(bytes,"trg_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

		ptr = (char *) raw->request(bytes) ;

		ret = caller->sfs->read(full_name, ptr, bytes) ;
		if(ret != bytes) {
			err = 3 ;
			goto ret_error ;
		}

	}


	raw->finalize(bytes,0,0,0) ;
	raw->rewind() ;
	return raw ;

	ret_error:;

	LOG(ERR,"%s: handle_raw: error %d",name,err) ;
	return 0 ;
}
