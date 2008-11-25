#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_ssd.h"


const char *daq_ssd::help_string = "SSD tst\n" ;

class daq_det_ssd_factory : public daq_det_factory
{
public:
        daq_det_ssd_factory() {
                daq_det_factory::det_factories[SSD_ID] = this ;
        }

        daq_det *create() {
                return new daq_ssd ;
        }
} ;

static daq_det_ssd_factory ssd_factory ;



daq_ssd::daq_ssd(daqReader *rts_caller)
{
	LOG(DBG,"SSD: rts_id %d, name %s",rts_id,name) ;

	rts_id  = SSD_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_ssd::~daq_ssd()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_ssd::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
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


daq_dta *daq_ssd::handle_legacy()
{

	// I need one object of ssd_t type but let the create decide on the necessary size
	legacy->create(1,"ssd_t",rts_id,DAQ_DTA_STRUCT(ssd_t)) ;
	

	ssd_t *ssd_p = (ssd_t *) legacy->request(1) ;	// need ONE ssd_t object
 
	ssd_reader(caller->mem, ssd_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
