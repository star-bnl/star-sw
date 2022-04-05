#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_ftp.h"

extern int ftp_reader(char *m, struct ftp_t *ftp, u_int driver) ;

const char *daq_ftp::help_string = "FTP tst\n" ;

class daq_det_ftp_factory : public daq_det_factory
{
public:
        daq_det_ftp_factory() {
                daq_det_factory::det_factories[FTP_ID] = this ;
        }

        daq_det *create() {
                return new daq_ftp ;
        }
} ;

static daq_det_ftp_factory ftp_factory ;



daq_ftp::daq_ftp(daqReader *rts_caller)
{
	LOG(DBG,"FTP: rts_id %d, name %s",rts_id,name) ;

	// dname is ignored 
	rts_id  = FTP_ID ;
	sfs_name = name = rts2name(rts_id) ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,caller) ;
}

daq_ftp::~daq_ftp()
{
	LOG(DBG,"%s: destructor",name) ;
	if(caller) caller->de_insert(rts_id) ;

	delete legacy ;

	return ;
}

	
daq_dta *daq_ftp::get(const char *bank, int c1, int c2, int c3, void *p1, void *p2)
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


daq_dta *daq_ftp::handle_legacy()
{

	// I need one object of ftp_t type but let the create decide on the necessary size
	legacy->create(1,"ftp_t",rts_id,DAQ_DTA_STRUCT(ftp_t)) ;
	

	ftp_t *ftp_p = (ftp_t *) legacy->request(1) ;	// need ONE ftp_t object
 
	ftp_reader(caller->mem, ftp_p, m_Debug) ;

	legacy->finalize(1,0,0,0) ;	// 1 entry; sector 0, row 0, pad 0
	legacy->rewind() ;

	return legacy ;
}
