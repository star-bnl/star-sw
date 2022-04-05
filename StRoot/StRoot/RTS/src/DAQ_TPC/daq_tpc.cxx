#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>




#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_tpc.h"

extern int tpc_reader(char *m, tpc_t *tpc, int sec, int flag) ;


class daq_det_tpc_factory : public daq_det_factory
{
public:
	daq_det_tpc_factory() {
		daq_det_factory::det_factories[TPC_ID] = this ;
	}

	daq_det *create() {
		return new daq_tpc ;
	}
} ;

static daq_det_tpc_factory tpc_factory ;




daq_tpc::daq_tpc(daqReader *rts_caller) 
{
	rts_id = TPC_ID ;
	name = sfs_name = rts2name(rts_id) ;

	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	legacy = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_tpc::~daq_tpc() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete legacy ;

	return ;
}



daq_dta *daq_tpc::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;
	if(!present) return 0 ;


	if(!bank || (strcasecmp(bank,"legacy")==0)) bank = "legacy" ;


	if(strcasecmp(bank,"legacy")==0) {
		return handle_legacy(sec,row) ;
	}



	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}

daq_dta *daq_tpc::handle_legacy(int sec, int rdo)
{
	int min_s, max_s ;
	int found_something = 0 ;

	if(sec <= 0) {
		min_s = 1 ;
		max_s = 24 ;
	}
	else {
		min_s = max_s = sec ;
	}

	legacy->create((max_s-min_s+1)*sizeof(tpc_t),"tpc_t",rts_id,DAQ_DTA_STRUCT(tpc_t)) ;

	for(int s=min_s;s<=max_s;s++) {
		int have ;

		tpc_t *tpc_p = (tpc_t *) legacy->request(1) ;
		
		// old tpc_reader wanted sectors counting from 0!!!
		have = tpc_reader(caller->mem, tpc_p, s-1, m_Debug) ;

		if(have) {
			found_something = 1 ;
			legacy->finalize(1,s,0,0) ;	// accept
		}
		else {
			LOG(NOTE,"%s: sector %d: not found",name,s) ;
		}

	}

	legacy->rewind() ;
	
	LOG(NOTE,"%s: done",name) ;

	if(found_something) return legacy ;
	else return 0 ;
}


int daq_tpc::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt)
{
	trg[0].t = 1234 ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// TPC does not give the correct L0, only L2
	trg[0].rhic = 0 ;
	
	return 1 ;
}
