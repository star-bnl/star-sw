#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_fgt.h"


const char *daq_fgt::help_string = "FGT\n\
adc	returns fgt_t;\n\
raw	returns raw data\n" ;

class daq_det_fgt_factory : public daq_det_factory
{
public:
	daq_det_fgt_factory() {
		daq_det_factory::det_factories[FGT_ID] = this ;
	}

	daq_det *create() {
		return new daq_fgt ;
	}
} ;

static daq_det_fgt_factory fgt_factory ;


daq_fgt::daq_fgt(daqReader *rts_caller) 
{
	rts_id = FGT_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "fgt" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fgt::~daq_fgt() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;

	return ;
}



daq_dta *daq_fgt::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;
	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(rdo) ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(rdo) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_fgt::handle_raw(int rdo)
{
	char *from , *st ;
	int r_start, r_stop ;
	int bytes ;

	assert(caller) ;	// sanity...

	if(!present) {
		LOG(ERR,"%s: not present?",name) ;
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}

	char str[256] ;
	char *full_name ;

	sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, 1, 1) ;
	full_name = caller->get_sfs_name(str) ;
		
	if(!full_name) return 0 ;
	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	raw->create(bytes,"fgt_raw",rts_id,DAQ_DTA_STRUCT(char)) ;
	st = (char *) raw->request(bytes) ;
		
	int ret = caller->sfs->read(str, st, bytes) ;
	if(ret != bytes) {
		LOG(ERR,"ret is %d") ;
	}

	
	raw->finalize(bytes,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	raw->rewind() ;

	return raw ;
	
}

	

daq_dta *daq_fgt::handle_adc(int rdo)
{
	u_short *raw_dta ;

	daq_dta *dd = handle_raw(rdo) ;

	LOG(DBG,"%s: got raw %p",name,dd) ;

	if(dd && dd->iterate()) {
		raw_dta = (u_short *) dd->Byte ;
	}
	else {
		return 0 ;
	}

	LOG(WARN,"FGT ADC not yet supported....") ;
	return 0 ;
}


int daq_fgt::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int do_log)
{
	u_short *us = (u_short *)buff ;

	u_short t_hi = l2h16(us[2]) ;
	u_short t_lo = l2h16(us[3]) ;

	int err = 0 ;

	if((t_lo & 0xFF00) || (t_hi & 0xFFF0)) {	//error
		err = 1 ;
		if(do_log) {
			LOG(ERR,"Corrupt token: t_hi 0x%04X, t_lo 0x%04X",t_hi,t_lo) ;
		}
	}

	if(err) {
		trg[0].t = 4097 ;
		trg[0].trg = 0 ;
		trg[0].daq = 0 ;
		
		return 1 ;
	}

	// L0 part
	trg[0].t = t_hi*256 + t_lo ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// FGT does not give the correct L0, only L2 so we invent 4
	trg[0].rhic = l2h16(us[4]) ;
	
	// L2 part
	trg[1].t = trg[0].t ;	// copy over token
	trg[1].trg = 15 ;	// for now! us[0] ;	// this is where the trg cmd ought to be
	trg[1].daq = us[1] ;
	trg[1].rhic = trg[0].rhic + 1 ;

	if(us[0] != 0xF) {
		LOG(ERR,"trg cmd not 15? 0x%04X 0x%04X 0x%04X 0x%04X 0x04X",us[0],us[1],us[2],us[3],us[4]) ;
	}

	return 2 ;
}
