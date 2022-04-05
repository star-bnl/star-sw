#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_l3.h"

extern int l3_reader(char *m, struct l3_t *l3, u_int driver) ;

const char *daq_l3::help_string = "L3 tst\n" ;

class daq_det_l3_factory : public daq_det_factory
{
public:
        daq_det_l3_factory() {
                daq_det_factory::det_factories[L3_ID] = this ;
        }

        daq_det *create() {
                return new daq_l3 ;
        }
} ;

static daq_det_l3_factory l3_factory ;



daq_l3::daq_l3(daqReader *rts_caller)
{
	LOG(DBG,"L3: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = L3_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "l3" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_l3::~daq_l3()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_l3::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
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


daq_dta *daq_l3::handle_legacy()
{

	// I need one object of l3_t type but let the create decide on the necessary size
	legacy->create(1,"l3_t",rts_id,DAQ_DTA_STRUCT(l3_t)) ;
	

	l3_t *l3_p = (l3_t *) legacy->request(1) ;	// need ONE l3_t object

	if(present & DET_PRESENT_DATAP) {
		l3_reader(caller->mem, l3_p, 0) ;
	}
	else {
		char str[256] ;
		char *full_name ;

		sprintf(str,"gl3/l3_gtd") ;
		full_name = caller->get_sfs_name(str) ;

		LOG(DBG,"full_name %s",full_name) ;

		if(!full_name) {
			sprintf(str,"l3/l3_gtd") ;
			full_name = caller->get_sfs_name(str) ;

			LOG(DBG,"full_name %s",full_name) ;


			if(!full_name) 	return 0 ;
		}

		int bytes = caller->sfs->fileSize(full_name) ;

		LOG(DBG,"bytes %d",bytes) ;

		char *mem = (char *)valloc(bytes) ;


		int ret = caller->sfs->read(str, mem, bytes) ;
		
		LOG(DBG,"ret %d",ret) ;

		l3_reader(mem, l3_p, 1) ;

		free(mem) ;
	}

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
