#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_pmd.h"

extern int pmd_reader(char *m, struct pmd_t *pmd, u_int driver) ;

const char *daq_pmd::help_string = "PMD tst\n" ;



daq_pmd::daq_pmd(daqReader *rts_caller)
{
	LOG(DBG,"PMD: rts_id %d, name %s",rts_id,name) ;


	rts_id  = PMD_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_pmd::~daq_pmd()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_pmd::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") != 0) {
		LOG(ERR,"%s: unknown bank %s",name,bank) ;
		return 0 ;
	}

	return handle_legacy() ;

}


daq_dta *daq_pmd::handle_legacy()
{

	// I need one object of pmd_t type but let the create decide on the necessary size
	legacy->create(1,"pmd_t",rts_id,DAQ_DTA_STRUCT(pmd_t)) ;
	

	pmd_t *pmd_p = (pmd_t *) legacy->request(1) ;	// need ONE pmd_t object
 
	pmd_reader(caller->mem, pmd_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
