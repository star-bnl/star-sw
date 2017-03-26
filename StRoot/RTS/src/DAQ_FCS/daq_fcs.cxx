#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_fcs.h"

const char *daq_fcs::help_string = "\
\n\
FCS Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;

class daq_det_fcs_factory : public daq_det_factory
{
public:
        daq_det_fcs_factory() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                daq_det_factory::det_factories[FCS_ID] = this ;
        }

        daq_det *create() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                return new daq_fcs ;
        }
} ;

static daq_det_fcs_factory fcs_factory ;



daq_fcs::daq_fcs(daqReader *rts_caller) 
{
	LOG(DBG,"%s",__PRETTY_FUNCTION__) ;

	rts_id = FCS_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "fcs" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fcs::~daq_fcs() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;

	return ;
}



daq_dta *daq_fcs::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		


	if(strcasecmp(bank,"raw")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_raw() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"adc")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_adc() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_fcs::handle_raw()
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	raw->create(size,"fcs_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;
	char *st = (char *) raw->request(size) ;

	caller->sfs->read(full_name, st, size) ;

	LOG(DBG,"sfs read succeeded") ;

        raw->finalize(size,1,1,0) ;

	raw->rewind() ;

	return raw ;

}

daq_dta *daq_fcs::handle_adc()
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
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

	adc->create(1000,"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;

	LOG(NOTE,"Starting fee_scan") ;
#if 0
	fcs_data_c dta_c ;
	dta_c.rdo_start(1) ;

	//raw data at "ptr"
	while(dta_c.fee_scan((u_short *)ptr,size/2)) {
		daq_adc_tb *at = (daq_adc_tb *) adc->request(dta_c.tb_cou) ;
		for(int i=0;i<dta_c.tb_cou;i++) {
			at[i].adc = dta_c.at[i].adc ;	
			at[i].tb = dta_c.at[i].tb ;
		}
		adc->finalize(dta_c.tb_cou,dta_c.sector,dta_c.fee_id,dta_c.fee_ch) ;
	}

	dta_c.rdo_zap(dta_c.rdo_p) ;
#endif
	free(ptr) ;

	adc->rewind() ;

	return adc ;

}

// knows how to get the token out of an event...
int daq_fcs::get_token(char *addr, int words)
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
int daq_fcs::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
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

