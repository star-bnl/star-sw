#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include "daq_rhicf.h"

extern int rhicf_reader(char *mem, struct rhicf_t *rhicf, u_int driver) ;




const char *daq_rhicf::help_string = "\
\n\
RHICF Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;

class daq_det_rhicf_factory : public daq_det_factory
{
public:
        daq_det_rhicf_factory() {
                daq_det_factory::det_factories[RHICF_ID] = this ;
        }

        daq_det *create() {
                return new daq_rhicf ;
        }
} ;

static daq_det_rhicf_factory rhicf_factory ;



daq_rhicf::daq_rhicf(daqReader *rts_caller) 
{
	rts_id = RHICF_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "rhicf" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;


	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_rhicf::~daq_rhicf() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;


	return ;
}



daq_dta *daq_rhicf::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		


	if(strcasecmp(bank,"raw")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_raw(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_rhicf::handle_raw(int sec, int rdo)
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	raw->create(size,"rhicf_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	char *st = (char *) raw->request(size) ;

	caller->sfs->read(full_name, st, size) ;

        raw->finalize(size,1,1,0) ;

	raw->rewind() ;

	return raw ;

}

// knows how to get the token out of an event...
int daq_rhicf::get_token(char *addr, int words)
{
	int cou ;
	struct daq_trg_word trg[128] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;	// special marker...
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}

// knows how to get a/the L2 command out of the event...
int daq_rhicf::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	int t_cou = 0 ;
	int in_words = words ;
	int err = 0 ;


	if(err) {
		LOG(ERR,"[%d] Bad Event: T %4d: words %d",
		    rdo,trg[0].t,in_words) ;

	}

	if(err & 1) {	// critical -- blow the whole event
		return -1 ;
	}

	return t_cou ;
}

