#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_pmd.h"

extern int pmd_reader(char *m, struct pmd_t *pmd, u_int driver) ;

const char *daq_pmd::help_string = "PMD tst\n" ;

class daq_det_pmd_factory : public daq_det_factory
{
public:
        daq_det_pmd_factory() {
                daq_det_factory::det_factories[PMD_ID] = this ;
        }

        daq_det *create() {
                return new daq_pmd ;
        }
} ;

static daq_det_pmd_factory pmd_factory ;



daq_pmd::daq_pmd(daqReader *rts_caller)
{
	LOG(DBG,"PMD: rts_id %d, name %s",rts_id,name) ;


	rts_id  = PMD_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "pmd" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;
	raw = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_pmd::~daq_pmd()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;
	delete raw ;

	return ;
}

	
daq_dta *daq_pmd::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
{
	Make() ;
	if(!present) return 0 ;

	if(strcmp(bank,"*")==0) bank = "legacy" ;	// set default, if called with *

	if(strcasecmp(bank,"legacy") == 0) {
		return handle_legacy() ;
	}
	
	if(strcasecmp(bank,"raw") == 0) {
		return handle_raw(c1) ;
	}
	
	LOG(ERR,"%s: unknown bank %s",name,bank) ;
	return 0 ;


}


daq_dta *daq_pmd::handle_legacy()
{

	// I need one object of pmd_t type but let the create decide on the necessary size
	legacy->create(1,"pmd_t",rts_id,DAQ_DTA_STRUCT(pmd_t)) ;
	

	pmd_t *pmd_p = (pmd_t *) legacy->request(1) ;	// need ONE pmd_t object

	LOG(DBG,"PMD: present is %d",present) ;
	
	memset(pmd_p,0,sizeof(pmd_t)) ;	// zap it!

	if(present & DET_PRESENT_DATAP) {
		pmd_reader(caller->mem, pmd_p, 0) ;
	}
	else {
		for(int s=1;s<=2;s++) {	// need to get to both banks

			daq_dta *store = handle_raw(s) ;
			if(store && store->iterate()) {
				LOG(DBG,"PMD: calling reader with %p for %d",store->Void,s) ;
				pmd_reader((char *)store->Void, pmd_p, s) ;
			}
		}
	}

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}


daq_dta *daq_pmd::handle_raw(int sec)	// counts from 1
{
	char str[256] ;
	char *full_name ;
	char *st ;
	int bytes ;



	sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name,sec,0) ;
	full_name = caller->get_sfs_name(str) ;

	if(!full_name) return 0 ;
	

	bytes = caller->sfs->fileSize(full_name) ;

	raw->create(bytes,"pmd_raw",rts_id,DAQ_DTA_STRUCT(char)) ;
	st = (char *) raw->request(bytes) ;

	int ret = caller->sfs->read(str,st,bytes) ;
	if(ret != bytes) {
		LOG(ERR,"PMD: ret is %d",ret) ;
	}

	raw->finalize(bytes,sec,0,0) ;
	raw->rewind() ;

	return raw ;
}
