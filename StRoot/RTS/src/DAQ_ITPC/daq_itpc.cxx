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
#include "itpcInterpreter.h"
#include "itpcPed.h"

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
                daq_det_factory::det_factories[ITPC_ID] = this ;
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

	rts_id = ITPC_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "itpc" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	ifee_fy17_raw = new daq_dta ;
	ifee_fy17_sampa = new daq_dta ;

	raw = new daq_dta ;
	sampa = new daq_dta ;
	ped = new daq_dta ;

	adc_sim = new daq_dta;;

	it = new itpcInterpreter ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_itpc::~daq_itpc() 
{
	LOG(DBG,"%s",__PRETTY_FUNCTION__) ;

	delete ifee_fy17_raw ;
	delete ifee_fy17_sampa ;

	delete raw ;
	delete sampa ;
	delete ped ;

	delete adc_sim ;

	return ;
}


daq_dta *daq_itpc::put(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
{

	assert(in_bank) ;

	if(strcasecmp(in_bank,"adc_sim")==0) {
		adc_sim->create(32*1024,(char *)"adc_sim",rts_id,DAQ_DTA_STRUCT(daq_sim_adc_tb)) ;
		return adc_sim ;
	}

	LOG(ERR,"%s: unknown bank type \"%s\"",name,in_bank) ;
	return 0 ;

}

daq_dta *daq_itpc::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		

	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_sampa(sec,row,1) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"sampa")==0) {
		return handle_sampa(sec,row,0) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"pedrms")==0) {
		return handle_ped(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	// these are FY17 banks!
	else if(strcasecmp(bank,"ifee_fy17_raw")==0) {
		return handle_ifee_fy17_raw() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"ifee_fy17_sampa")==0) {
		return handle_ifee_fy17_sampa() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}


daq_dta *daq_itpc::handle_raw(int sec, int rdo)
{
	int min_sec, max_sec, min_rdo, max_rdo ;

	// bring in the bacon from the SFS file....
	assert(caller) ;



	if((sec <= 0)||(sec>24)) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = sec ;
	}

	if((rdo <= 0)||(rdo>4)) {
		min_rdo = 1 ;
		max_rdo = 4 ;
	}
	else {
		min_rdo = max_rdo = rdo ;
	}


	raw->create(16*1024,(char *)"raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;



	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		char str[128] ;	

	
		sprintf(str,"%s/sec%02d/rdo%d/ifee",sfs_name,s,r) ;

		char *full_name = caller->get_sfs_name(str) ;

		LOG(DBG,"name [%s] -> full_name [%s]",str,full_name) ;

		if(full_name == 0) continue ;	

		int size = caller->sfs->fileSize(full_name) ;

		LOG(NOTE,"full_name [%s] --> size %d",full_name,size) ;

		if(size <= 0) continue ;	// this is really an error!

		char *mem = (char *) raw->request(size) ;

		if(mem==0) {
			LOG(ERR,"ITPC: error in %d %d",s,r) ;
			continue ;
		}

		caller->sfs->read(full_name,mem,size) ;

		raw->finalize(size,s,r,0) ;

	}}

	raw->rewind() ;

	return raw ;

}

class sampa_c : public itpcData
{
public:
	sampa_c() {;} ;
	~sampa_c() {;} ;

	daq_dta *dta ;
	daq_adc_tb *at ;
	int in_adc ;

	
	void ch_start(int c) {
		ch = c ;
		tb_cou = 0 ;

		//LOG(TERR,"CH %d start",ch) ;
		at = (daq_adc_tb *) dta->request(512) ;
	}

	void accum(int sec0, int rdo0, int port0, int fee_id, int ch, int tb, int adc) {
		at[tb_cou].adc = adc ;
		at[tb_cou].tb = tb ;
		tb_cou++ ;
	}
	
	void ch_done(int err) {

		if(in_adc) {
			int row, pad ;
			itpc_ifee_to_rowpad(fee_id, ch, row, pad) ;
			dta->finalize(tb_cou, sector, row, pad) ;
		}
		else {
			dta->finalize(tb_cou, sector, fee_id, ch) ;
		}

		if(err) {
			LOG(ERR,"%d %d %d done, %d",sector,fee_id,ch,tb_cou) ;
		}

	}

} ;


daq_dta *daq_itpc::handle_sampa(int sec, int rdo, int in_adc)
{
	class sampa_c sampa_c ;

	int min_sec, max_sec, min_rdo, max_rdo ;


	// bring in the bacon from the SFS file....
	assert(caller) ;



	if((sec <= 0)||(sec>24)) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = sec ;
	}

	if((rdo <= 0)||(rdo>4)) {
		min_rdo = 1 ;
		max_rdo = 4 ;
	}
	else {
		min_rdo = max_rdo = rdo ;
	}

	sampa_c.dta = sampa ;
	sampa_c.in_adc = in_adc ;

	sampa->create(1024,(char *)"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;

	it->ped_c = &sampa_c ;
	it->run_start(0) ;	// just in case
	it->start_event(0) ;


	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		daq_dta *rdo_dta ;
		u_int *dta ;
		u_int words ;
		int ret ;

		rdo_dta = handle_raw(s,r) ;

		if(rdo_dta==0) continue ;

		ret = rdo_dta->iterate() ;
		if(ret==0) continue ;

		dta = (u_int *)rdo_dta->Byte ;
		words = rdo_dta->ncontent/4 ;

		if(words==0) continue ;

		// first 4 words are the GTP header so let's skip
		dta += 4 ;
		words -= 4 ;

		it->sector_id = s ;
		it->rdo_id = r ;

		ret = it->rdo_scan(dta,words) ;

		LOG(NOTE,"rdo_scan %d:%d, words %d, ret %d",s,r,words,ret) ;


	}}

	sampa->rewind() ;

	return sampa ;
}


daq_dta *daq_itpc::handle_ped(int sec, int rdo)
{

	return 0 ;
}


// FY17 bank
daq_dta *daq_itpc::handle_ifee_fy17_raw()
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

	ifee_fy17_raw->create(size,"itpc_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;
	char *st = (char *) ifee_fy17_raw->request(size) ;

	caller->sfs->read(full_name, st, size) ;

	LOG(DBG,"sfs read succeeded") ;

        ifee_fy17_raw->finalize(size,1,1,0) ;

	ifee_fy17_raw->rewind() ;

	return ifee_fy17_raw ;

}

// FY17 bank
daq_dta *daq_itpc::handle_ifee_fy17_sampa()
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

	ifee_fy17_sampa->create(1000,"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;

	LOG(NOTE,"Starting fee_scan") ;

	// USES old FY17 class
	itpc_data_c dta_c ;
	dta_c.rdo_start(0,0,0) ;

	//raw data at "ptr"
	while(dta_c.fee_scan((u_short *)ptr,size/2)) {
		daq_adc_tb *at = (daq_adc_tb *) ifee_fy17_sampa->request(dta_c.tb_cou) ;
		for(int i=0;i<dta_c.tb_cou;i++) {
			at[i].adc = dta_c.at[i].adc ;	
			at[i].tb = dta_c.at[i].tb ;
		}
		ifee_fy17_sampa->finalize(dta_c.tb_cou,dta_c.sector,dta_c.fee_id,dta_c.fee_ch) ;
	}

	dta_c.rdo_zap(dta_c.rdo_p) ;
	free(ptr) ;

	ifee_fy17_sampa->rewind() ;

	return ifee_fy17_sampa ;

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

static inline u_int sw16(u_int d)
{
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;

        return d ;
}


// knows how to get a/the L2 command out of the event...
int daq_itpc::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
#ifdef ITPC_FY17
	int t_cou = 0 ;
	static int token ;

	token++ ;
	if(token>4095) token = 1 ;

	trg[t_cou].t = token ;
	trg[t_cou].trg = 4 ;
	trg[t_cou].daq = 2 ;
	t_cou++ ;

	return t_cou ;
#else
	u_int err = 0 ;
	u_int trg_fired ;
//	u_int v_fired ;
	int trg_cou ;
	int t_cou = 0 ;
	u_int evt_status ;
	int trl_ix ;

	u_int *d = (u_int *)addr + 4 ;	// skip header

	// NOTE that since Dec 2017 the 16 bit words are swapped!!!

	// since there are buggy TEF events without the start comma lets' search for it
	for(int i=0;i<16;i++) {
		if(d[i] == 0xCCCC001C) {
			d = d + i ;
			break ;
		}
	}

	if(sw16(d[0]) != 0x001CCCCC) {	// expect start-comma
		LOG(ERR,"First word 0x%08X of %d words - bad",d[0],words) ;
		err |= 1 ;
		goto err_end ;
	}

	//LOG(TERR,"   0x%08X 0x%08X 0x%08X", sw16(d[1]),sw16(d[2]),sw16(d[words-1])) ;
	
	if(sw16(d[1]) != 0x98000004) { // not a triggered event
		trg[0].t = 4096 ;	// a "log" event
		trg[0].daq = 0 ;
		trg[0].trg = 0 ;
	
		LOG(WARN,"%d: not a triggered event 0x%08X",rdo,sw16(d[1])) ;

		return 1 ;	
	}

	if(sw16(d[2]) != 0x12340000) {	// wrong version
		LOG(ERR,"Wrong version 0x%X",d[2]) ;
		err |= 2 ;
		goto err_end ;
	}

	trg_fired = sw16(d[3]) ;
//	v_fired = sw16(d[4]) ;	// if 0, no prompt trigger	



	// this gets messy so we won't check
	/*
	if(sw16(d[words-1]) != 0xFFFF005C) {	// expect stop-comma
		err |= 0x10 ;
		goto err_end ;
	}
	*/

	//find trailer start-header
	trl_ix = -1 ;
	for(int i=(words-1);i>=0;i--) {
		if(sw16(d[i]) == 0x98001000) {
			trl_ix = i ;
			break ;
		}
	}


	if(trl_ix < 0) {
		LOG(ERR,"No trailer found") ;
		err |= 0x20 ;
		goto err_end ;
	}

	trl_ix++ ;

	if(sw16(d[trl_ix++]) != 0xABCD0000) {
		LOG(ERR,"Wrong trailer word") ;
		err |= 0x40 ;
		goto err_end ;
	}

	evt_status = sw16(d[trl_ix++]) ;
	trg_cou = sw16(d[trl_ix++]) ;

	//LOG(TERR,"trg_cou %d, fired %d",trg_cou,trg_fired) ;

	trg[0].reserved[0] = trg_fired ;

	for(int i=0;i<trg_cou;i++) {
		trg[i+1].reserved[0] = sw16(d[trl_ix++]) ;
	}
	

	if(evt_status) {
		LOG(ERR,"... RDO %d: %d/%d -- evt status 0x%08X, trg_cou %d",rdo,sw16(d[6]),sw16(d[5]),evt_status,trg_cou) ;
	}

	
	if(trg_cou==0 && trg[0].reserved[0]==0) {	// Monitoring Event
		trg[0].t = 4096 ;
		trg[0].daq = 0 ;
		trg[0].trg = 0 ;

		return 1 ;
	}

	// get prompt trigger/token
	t_cou = 0 ;
	if(trg[0].reserved[0]==0) {
		trg[t_cou].t = 4097 ;
		trg[t_cou].daq = 0 ;
		trg[t_cou].trg = 0 ;
		t_cou++ ;
	}
	else {
		u_int v = trg[0].reserved[0] ;
		u_int t ;

		t = ((v>>8)&0xF)<<8 ;
		t |= ((v>>12)&0xF)<<4 ;
		t |= ((v>>16)&0xF) ;

		trg[t_cou].trg = v & 0xF ;
		trg[t_cou].daq = (v>>4) & 0xF ;
		trg[t_cou].t = t ; 

		if((v&0xFFF00000) != 0x04300000) {
			LOG(ERR,"RDO %d: 0x%08X: %d %d %d",rdo,trg[0].reserved[0],trg[0].t,trg[0].trg,trg[0].daq) ;
		}
			
		t_cou++ ;

	}

	for(int i=1;i<(trg_cou+1);i++) {
		u_int v = trg[i].reserved[0] ;
		u_int t ;

		t = ((v>>8)&0xF)<<8 ;
		t |= ((v>>12)&0xF)<<4 ;
		t |= ((v>>16)&0xF) ;

		trg[t_cou].trg = v & 0xF ;
		trg[t_cou].daq = (v>>4) & 0xF ;
		trg[t_cou].t = t ; 

		if(trg[t_cou].trg>=4 && trg[t_cou].trg<13) {	// FIFO trg
			//LOG(WARN,"RDO %d: %d/%d: 0x%08X: %d %d %d",rdo,i,(trg_cou+1),
			//    v,
			//    trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			

			if((v&0xFFF00000) != 0x04300000) {
				LOG(WARN,"RDO %d: %d/%d: 0x%08X: %d %d %d",rdo,i,(trg_cou+1),
				    v,
				    trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			}
			else {
				continue ;
			}
		}

		//if(trg[t_cou].trg==2 && trg[t_cou].t==10 && trg[t_cou].daq==3) continue ;
		if(trg[t_cou].trg<=2) {
			LOG(WARN,"Odd trg_cmd: T %d, trg %d, daq %d",trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			continue ;
		}
		t_cou++ ;
	}


		

	return t_cou ;

	err_end:;

	LOG(ERR,"RDO %d: Error in get_l2 %d",rdo,err) ;

	return 0 ;


#endif



}

