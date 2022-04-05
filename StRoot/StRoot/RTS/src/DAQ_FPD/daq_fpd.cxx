#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_fpd.h"


extern int fpd_reader(char *m, struct fpd_t *fpd, u_int driver) ;

const char *daq_fpd::help_string = "FPD tst\n" ;

class daq_det_fpd_factory : public daq_det_factory
{
public:
        daq_det_fpd_factory() {
                daq_det_factory::det_factories[FPD_ID] = this ;
        }

        daq_det *create() {
                return new daq_fpd ;
        }
} ;

static daq_det_fpd_factory fpd_factory ;



daq_fpd::daq_fpd(daqReader *rts_caller)
{
	LOG(DBG,"FPD: rts_id %d, name %s",rts_id,name) ;


	rts_id  = FPD_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_fpd::~daq_fpd()
{
	LOG(DBG,"%s: destructor",name) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_fpd::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") != 0) {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
		return 0 ;
	}

	return handle_legacy() ;

}


daq_dta *daq_fpd::handle_legacy()
{

	// I need one object of fpd_t type but let the create decide on the necessary size
	legacy->create(1,"fpd_t",rts_id,DAQ_DTA_STRUCT(fpd_t)) ;
	

	fpd_t *fpd_p = (fpd_t *) legacy->request(1) ;	// need ONE fpd_t object
 
	fpd_reader(caller->mem, fpd_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
