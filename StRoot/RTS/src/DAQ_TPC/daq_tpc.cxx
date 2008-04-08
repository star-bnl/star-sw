#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>




#include <SFS/sfs_index.h>
#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_dta.h>


#include "daq_tpc.h"

extern int tpc_reader(char *m, int type, int sec, daq_dta **ddta) ;

daq_tpc::daq_tpc(const char *dname, rts_reader *rts_caller) 
{
	rts_id = TPC_ID ;
	name = rts2name(rts_id) ;

	caller = rts_caller ;

	adc = new daq_dta ;
	cld = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_tpc::~daq_tpc() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete cld ;
	delete adc ;

	return ;
}



daq_dta *daq_tpc::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{


	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcasecmp(bank,"cld")==0) {
		return handle_cld(sec,row) ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(sec,row) ;
	}



	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_tpc::handle_cld(int sec, int rdo)
{
	int min_sec, max_sec ;

	if(sec<=0) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = sec ;
	}


	cld->create(100,(char *)"cld",TPC_ID,DAQ_DTA_STRUCT(daq_cld)) ;

	for(int i=min_sec;i<=max_sec;i++) {
		tpc_reader(caller->legacy_p, 1, i, &cld) ;
	}

	cld->rewind() ;
	return cld ;
}

daq_dta *daq_tpc::handle_adc(int sec, int rdo)
{
	int min_sec, max_sec ;

	if(sec<=0) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = sec ;
	}


	adc->create(100,(char *)"adc_tb",TPC_ID,DAQ_DTA_STRUCT(daq_adc_tb)) ;

	for(int i=min_sec;i<=max_sec;i++) {
		tpc_reader(caller->legacy_p, 0, i, &adc) ;
	}

	adc->rewind() ;
	return adc ;
}


int daq_tpc::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt)
{
	trg[0].t = 0 ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// TPC does not give the correct L0, only L2
	trg[0].rhic = 0 ;
	
	return 1 ;
}
