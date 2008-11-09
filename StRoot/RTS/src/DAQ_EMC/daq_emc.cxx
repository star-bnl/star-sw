#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_emc.h"



const char *daq_emc::help_string = "EMC tst\n" ;



daq_emc::daq_emc(daqReader *rts_caller)
{
	LOG(DBG,"EMC: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = BTOW_ID ;	// watch it! this is ust here for show...
	sfs_name = "EMC" ;
	caller = rts_caller ;
	// NO INSERT since this is a dummy detector!!!

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_emc::~daq_emc()
{
	LOG(DBG,"%s: destructor",name) ;

	delete legacy ;

	return ;
}

int daq_emc::Make()
{
	return 0 ;
}

daq_dta *daq_emc::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") != 0) {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
		return 0 ;
	}

	return handle_legacy() ;

}


daq_dta *daq_emc::handle_legacy()
{

	// I need one object of emc_t type but let the create decide on the necessary size
	legacy->create(1,"emc_t",rts_id,DAQ_DTA_STRUCT(emc_t)) ;
	

	emc_t *emc_p = (emc_t *) legacy->request(1) ;	// need ONE emc_t object
 
	int ret = emc_reader(caller->mem, emc_p, m_Debug, 0, 0, 0) ;

	if(ret == 0) return 0 ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
