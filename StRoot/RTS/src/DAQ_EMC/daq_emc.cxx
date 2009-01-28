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
	LOG(WARN,"Using EMC_LEGACY is deprecated! Rather use the specific readers: BTOW, BSMD, ETOW, ESMD") ;

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

	// I need one object of emc_t type.
	// It's ugly that I need to allocate memory before I know
	// if the detectors are present but it would just get too
	// ugly otherwise

	legacy->create(1,"emc_t",rts_id,DAQ_DTA_STRUCT(emc_t)) ;

	emc_t *emc_p = (emc_t *) legacy->request(1) ;	// need ONE emc_t object
 
	memset(emc_p,0,sizeof(struct emc_t)) ;	// must clear all

	emc_p->etow_max_ch = ETOW_MAXFEE * ETOW_DATSIZE ;
	emc_p->btow_max_ch = BTOW_MAXFEE * BTOW_DATSIZE ;
	emc_p->esmd_max_ch = ESMD_MAXFEE * ESMD_DATSIZE ;
	emc_p->bsmd_max_ch = BSMD_FIBERS * BSMD_DATSIZE ;

	dd = caller->det("btow")->get("adc") ;
	while(dd && dd->iterate()) {
		found_some = 1 ;


		emc_p->btow_in = 1;

		btow_t *d = (btow_t *) dd->Void ;

		for(int i=0;i<BTOW_MAXFEE;i++) {
			for(int j=0;j<BTOW_PRESIZE;j++) {
				emc_p->btow_pre[i][j] = d->preamble[i][j] ;
			}
			for(int j=0;j<BTOW_DATSIZE;j++) {
				if(d->adc[i][j] > 0) emc_p->btow_ch++ ;
				emc_p->btow_new[i][j] = d->adc[i][j] ;
			}

		}
				
	}

	dd = caller->det("etow")->get("adc") ;
	while(dd && dd->iterate()) {
		found_some = 1 ;

		emc_p->etow_in = 1;

		etow_t *d = (etow_t *) dd->Void ;

		for(int i=0;i<ETOW_MAXFEE;i++) {
			for(int j=0;j<ETOW_PRESIZE;j++) {
				emc_p->etow_pre[i][j] = d->preamble[i][j] ;
			}
			for(int j=0;j<ETOW_DATSIZE;j++) {
				if(d->adc[i][j] > 0) emc_p->etow_ch++ ;
				emc_p->etow[i][j] = d->adc[i][j] ;
			}

		}
		

	}

	dd = caller->det("esmd")->get("adc") ;
	while(dd && dd->iterate()) {
		found_some = 1 ;

		emc_p->esmd_in = 1;
		emc_p->esmd_max_fee = ESMD_MAXFEE ;	// NOTE that this is the max value, always!

		esmd_t *d = (esmd_t *) dd->Void ;

		for(int i=0;i<ESMD_MAXFEE;i++) {
			for(int j=0;j<ESMD_PRESIZE;j++) {
				emc_p->esmd_pre[i][j] = d->preamble[i][j] ;
			}
			for(int j=0;j<ESMD_DATSIZE;j++) {
				if(d->adc[i][j] > 0) emc_p->esmd_ch++ ;
				emc_p->esmd[i][j] = d->adc[i][j] ;
			}

		}		
	}



	// BSMD is slightly more complex
	if((dd=caller->det("bsmd")->get("adc_non_zs"))) {	// try the non-ZS banks first..
		while(dd->iterate()) {
			found_some = 1 ;

			emc_p->bsmd_in = 1 ;
			emc_p->bsmd_raw_in = 1 ;

			bsmd_t *d = (bsmd_t *) dd->Void ;

			int rdo_ix = dd->rdo - 1 ;	// rdo from 1; but ix from 0
			for(int i=0;i<BSMD_DATSIZE;i++) {
				if(d->adc[i] > 0) emc_p->bsmd_ch++ ;	// count >0

				emc_p->bsmd[rdo_ix][i] = d->adc[i] ;
			}

			emc_p->bsmd_cap[rdo_ix] = d->cap ;
		}
	}
	else if((dd=caller->det("bsmd")->get("adc"))) {
		while(dd->iterate()) {
			found_some = 1 ;
	
			emc_p->bsmd_in = 1 ;
			emc_p->bsmd_raw_in = 0 ;	// make sure!

			bsmd_t *d = (bsmd_t *) dd->Void ;

			int rdo_ix = dd->rdo - 1 ;	// rdo from 1; but ix from 0

			for(int i=0;i<BSMD_DATSIZE;i++) {
				if(d->adc[i] > 0) emc_p->bsmd_ch++ ;	// count >0

				emc_p->bsmd[rdo_ix][i] = d->adc[i] ;
			}

			emc_p->bsmd_cap[rdo_ix] = d->cap ;

		}
	}


	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	if(found_some) return legacy ;
	else return 0 ;
}
