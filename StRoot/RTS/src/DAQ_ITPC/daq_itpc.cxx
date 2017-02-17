#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_itpc.h"
#include "itpcCore.h"

const char *daq_itpc::help_string = "\
\n\
ITPC Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;

class daq_det_itpc_factory : public daq_det_factory
{
public:
        daq_det_itpc_factory() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                daq_det_factory::pseudo_factories[SVT_ID] = this ;
        }

        daq_det *create() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                return new daq_itpc ;
        }
} ;

static daq_det_itpc_factory itpc_factory ;



daq_itpc::daq_itpc(daqReader *rts_caller) 
{
	LOG(DBG,"%s",__PRETTY_FUNCTION__) ;

	rts_id = -SVT_ID ;
	name = "itpc_pseudo" ;
	sfs_name = "itpc" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	ifee_raw = new daq_dta ;
	ifee_sampa = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_itpc::~daq_itpc() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete ifee_raw ;


	return ;
}



daq_dta *daq_itpc::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		


	if(strcasecmp(bank,"ifee_raw")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_ifee_raw() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"ifee_sampa")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_ifee_sampa() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_itpc::handle_ifee_raw()
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/fee_raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	ifee_raw->create(size,"itpc_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;
	char *st = (char *) ifee_raw->request(size) ;

	caller->sfs->read(full_name, st, size) ;

	LOG(DBG,"sfs read succeeded") ;

        ifee_raw->finalize(size,1,1,0) ;

	ifee_raw->rewind() ;

	return ifee_raw ;

}

daq_dta *daq_itpc::handle_ifee_sampa()
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/fee_raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	char *ptr = (char *) malloc(size) ;
	LOG(DBG,"Malloc at %p",ptr) ;

	caller->sfs->read(full_name, ptr, size) ;

	LOG(DBG,"sfs read succeeded") ;

	ifee_sampa->create(1000,"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;

	LOG(NOTE,"Starting fee_scan") ;

	itpc_data_c dta_c ;
	dta_c.rdo_start(1) ;

	//raw data at "ptr"
	while(dta_c.fee_scan((u_short *)ptr,size/2)) {
		daq_adc_tb *at = (daq_adc_tb *) ifee_sampa->request(dta_c.tb_cou) ;
		for(int i=0;i<dta_c.tb_cou;i++) {
			at[i].adc = dta_c.at[i].adc ;	
			at[i].tb = dta_c.at[i].tb ;
		}
		ifee_sampa->finalize(dta_c.tb_cou,dta_c.sector,dta_c.fee_id,dta_c.fee_ch) ;
	}

	dta_c.rdo_zap(dta_c.rdo_p) ;
	free(ptr) ;

	ifee_sampa->rewind() ;

	return ifee_sampa ;

}

// knows how to get the token out of an event...
int daq_itpc::get_token(char *addr, int words)
{
	LOG(ERR,"get_token") ;

	int cou ;
	struct daq_trg_word trg[128] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;	// special marker...
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}



// knows how to get a/the L2 command out of the event...
int daq_itpc::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	int t_cou = 0 ;
	static int token ;

	token++ ;
	if(token>4095) token = 1 ;

	trg[t_cou].t = token ;
	trg[t_cou].trg = 4 ;
	trg[t_cou].daq = 2 ;
	t_cou++ ;

	return t_cou ;
}

