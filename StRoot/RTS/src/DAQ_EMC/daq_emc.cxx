#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_emc.h"

#include <DAQ_BTOW/daq_btow.h>
#include <DAQ_ETOW/daq_etow.h>
#include <DAQ_ESMD/daq_esmd.h>
#include <DAQ_BSMD/daq_bsmd.h>

const char *daq_emc::help_string = "EMC tst\n" ;

class daq_det_emc_factory : public daq_det_factory
{
public:
        daq_det_emc_factory() {
                daq_det_factory::pseudo_factories[BTOW_ID] = this ;
        }

        daq_det *create() {
                return new daq_emc ;
        }
} ;

static daq_det_emc_factory emc_factory ;



daq_emc::daq_emc(daqReader *rts_caller)
{
	LOG(DBG,"EMC: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = -BTOW_ID ;	// watch it! this is ust here for show...
	name = sfs_name = "emc_pseudo" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

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
	daq_dta *dd ;
	int found_some = 0 ;

	// I need one object of emc_t type but let the create decide on the necessary size
	legacy->create(1,"emc_t",rts_id,DAQ_DTA_STRUCT(emc_t)) ;

	emc_t *emc_p = (emc_t *) legacy->request(1) ;	// need ONE emc_t object
 
	memset(emc_p,0,sizeof(struct emc_t)) ;	// just in case...

	dd = caller->det("btow")->get("adc") ;
	if(dd && dd->iterate()) {
		found_some = 1 ;

		btow_t *btow_p = (btow_t *)dd->Void ;


	}

	dd = caller->det("etow")->get("adc") ;
	while(dd && dd->iterate()) {
		found_some = 1 ;

		etow_t *etow_p = (etow_t *) dd->Void ;

		

	}

	dd = caller->det("esmd")->get("adc") ;
	while(dd && dd->iterate()) {
		found_some = 1 ;

		esmd_t *esmd_p = (esmd_t *) dd->Void ;

	}

	// BSMD is slightly more complex
	if((dd=caller->det("bsmd")->get("adc"))) {
		while(dd->iterate()) {
			found_some = 1 ;

			bsmd_t *bsmd_p = (bsmd_t *) dd->Void ;


		}
	}
	else if((dd=caller->det("bsmd")->get("adc_non_zs"))) {
		while(dd->iterate()) {
			found_some = 1 ;
	
			bsmd_t *bsmd_p = (bsmd_t *) dd->Void ;
		}
	}


	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	if(found_some) return legacy ;
	else return 0 ;
}
