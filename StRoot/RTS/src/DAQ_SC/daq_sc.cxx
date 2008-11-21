#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

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
	sfs_name = "sc" ;	
	
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_sc::~daq_sc()
{
	LOG(DBG,"%s: destructor",name) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_sc::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") != 0) {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
		return 0 ;
	}

	return handle_legacy() ;

}


daq_dta *daq_sc::handle_legacy()
{

	// I need one object of sc_t type but let the create decide on the necessary size
	legacy->create(1,"sc_t",rts_id,DAQ_DTA_STRUCT(sc_t)) ;
	

	sc_t *sc_p = (sc_t *) legacy->request(1) ;	// need ONE sc_t object
 
	sc_reader(caller->mem, sc_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
