#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_ric.h"

extern int ric_reader(char *m, struct ric_t *ric, u_int driver) ;

const char *daq_ric::help_string = "RIC tst\n" ;


class daq_det_ric_factory : public daq_det_factory
{
public:
        daq_det_ric_factory() {
                daq_det_factory::det_factories[RIC_ID] = this ;
        }

        daq_det *create() {
                return new daq_ric ;
        }
} ;

static daq_det_ric_factory ric_factory ;



daq_ric::daq_ric(daqReader *rts_caller)
{
	LOG(DBG,"RIC: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = RIC_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_ric::~daq_ric()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_ric::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	Make() ;
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") != 0) {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
		return 0 ;
	}

	return handle_legacy() ;

}


daq_dta *daq_ric::handle_legacy()
{

	// I need one object of ric_t type but let the create decide on the necessary size
	legacy->create(1,"ric_t",rts_id,DAQ_DTA_STRUCT(ric_t)) ;
	

	ric_t *ric_p = (ric_t *) legacy->request(1) ;	// need ONE ric_t object
 
	ric_reader(caller->mem, ric_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
