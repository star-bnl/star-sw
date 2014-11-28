#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_svt.h"

extern int svt_reader(char *m, struct svt_t *svt, u_int driver) ;

const char *daq_svt::help_string = "SVT tst\n" ;


class daq_det_svt_factory : public daq_det_factory
{
public:
        daq_det_svt_factory() {
                daq_det_factory::det_factories[SVT_ID] = this ;
        }

        daq_det *create() {
                return new daq_svt ;
        }
} ;

static daq_det_svt_factory svt_factory ;



daq_svt::daq_svt(daqReader *rts_caller)
{
	LOG(DBG,"SVT: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = SVT_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "svt" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;


	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_svt::~daq_svt()
{
	LOG(DBG,"%s: destructor",name) ;

	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_svt::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
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


daq_dta *daq_svt::handle_legacy()
{

	// I need one object of svt_t type but let the create decide on the necessary size
	legacy->create(1,"svt_t",rts_id,DAQ_DTA_STRUCT(svt_t)) ;
	

	svt_t *svt_p = (svt_t *) legacy->request(1) ;	// need ONE svt_t object
 
	svt_reader(caller->mem, svt_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
