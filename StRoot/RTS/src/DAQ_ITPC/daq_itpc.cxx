#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

//#include <DAQ_TPX/tpxFCF.h>

#include "daq_itpc.h"
#include "itpcCore.h"
#include "itpcInterpreter.h"
#include "itpcPed.h"
#include "itpcFCF.h"

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
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

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
	cld = new daq_dta ;

	adc_sim = new daq_dta;
	cld_sim = new daq_dta ;
	gain = new daq_dta ;

	it = new itpcInterpreter ;

	memset(fcf,0,sizeof(fcf)) ;
	fcf_det_type = 1 ;	// ITPC
	fcf_det_orient = 1 ;	// normal

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_itpc::~daq_itpc() 
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

	delete ifee_fy17_raw ;
	delete ifee_fy17_sampa ;

	delete raw ;
	delete sampa ;
	delete ped ;
	delete cld ;

	delete adc_sim ;
	delete gain ;
	delete cld_sim ;

	delete it ;

	for(int i=0;i<25;i++) {
		if(fcf[i]) delete fcf[i] ;
	}

	return ;
}

void daq_itpc::setup_fcf(int det, int orient)
{
	fcf_det_type = det ;
	fcf_det_orient = orient ;
}

void daq_itpc::run_stop() 
{
	for(int s=1;s<=24;s++) {
		if(fcf[s]) fcf[s]->run_stop() ;
	}

}

daq_dta *daq_itpc::put(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
{

	assert(in_bank) ;

	if(strcasecmp(in_bank,"adc_sim")==0) {
		adc_sim->create(32*1024,(char *)"adc_sim",rts_id,DAQ_DTA_STRUCT(daq_sim_adc_tb)) ;
		return adc_sim ;
	}
	else if(strcasecmp(in_bank,"gain")==0) {
		gain->create(32*1024,(char *)"gain",rts_id,DAQ_DTA_STRUCT(daq_det_gain)) ;
		return gain ;
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
	else if(strcasecmp(bank,"cld")==0) {
		return handle_cld(sec) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"cld_sim")==0) {
		return handle_cld_sim(sec) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"adc_sim")==0) {
		if(adc_sim->is_empty()) return 0 ;
		adc_sim->rewind() ;
		return adc_sim ;
	}
	else if(strcasecmp(bank,"gain")==0) {
		if(gain->is_empty()) return 0 ;
		gain->rewind() ;
		return gain ;
	}
	
	// ********************************************
	// ************ these are FY17 banks!
	// ********************************************
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

daq_dta *daq_itpc::handle_cld_sim(int sec)
{
	int min_sec, max_sec ;
	u_char evt_started[25] ;

	memset(evt_started,0,sizeof(evt_started)) ;

	if(sec <= 0) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = sec ;
		max_sec = sec ;
	}


	daq_dta *sim = get("adc_sim") ;

	//LOG(TERR,"%s: %d",__PRETTY_FUNCTION__,sec) ;

	while(sim && sim->iterate()) {
		int s = sim->sec ;	//shorthand
		u_short sim_array[512] ;
		u_short track_array[512] ;

		//LOG(TERR,"Here %d",s) ;

		if((min_sec<=s) && (s<=max_sec)) ;
		else continue ;



		if(fcf[s]==0) {
			fcf[s] = new itpc_fcf_c ;	// implicit run start within constructor

			fcf[s]->my_id = s ;
			fcf[s]->sector_id = s ;
			fcf[s]->offline = 1 ;
			fcf[s]->det_type = fcf_det_type ;
			fcf[s]->y_is_timebin = fcf_det_orient ;

			fcf[s]->init(s,0) ;	// in all cases


			// I think I will load gains here...
			daq_dta *g = get("gain") ;
			if(g) fcf[s]->init(g) ;


		}

		if(!evt_started[s]) {
			fcf[s]->event_start() ;
			evt_started[s] = 1 ;
		}


		daq_sim_adc_tb *sim_dta = (daq_sim_adc_tb *) sim->Void ;

		memset(sim_array,0,sizeof(sim_array)) ;
		memset(track_array,0,sizeof(track_array)) ;

		for(u_int i=0;i<sim->ncontent;i++) {
			sim_array[sim_dta[i].tb] = sim_dta[i].adc ;
			track_array[sim_dta[i].tb] = sim_dta[i].track_id ;

		}

		fcf[s]->do_ch_sim(sim->row,sim->pad,sim_array,track_array) ;
	}

	//LOG(TERR,"After loading of data") ;

	cld_sim->create(1024,(char *)"cld_sim",rts_id,DAQ_DTA_STRUCT(daq_sim_cld_x)) ;

	char *buff = (char *) malloc(1024*1024) ;
	for(int s=min_sec;s<=max_sec;s++) {
		if(fcf[s]) ;
		else {
			//LOG(ERR,"What? No sector %d?",s) ;
			continue ;
		}

		int bytes = fcf[s]->do_fcf(buff,1024*1024) ;	// returns words really
		//LOG(TERR,"Sector %d: %d words",s,bytes) ;

		bytes *= 4 ;	// and now it's bytes


		u_int *end_buff = (u_int *)(buff + bytes) ;
		u_int *p_buff = (u_int *)buff ;

		while(p_buff < end_buff) {
			u_int row = *p_buff++ ;
			u_int version = *p_buff++ ;
			u_int int_cou = *p_buff++ ;

			int ints_per_cluster = (row>>16) ;
			row &= 0xFFF ;

			int clusters = int_cou/ints_per_cluster ;

			//LOG(TERR,"clusters %d, sector %d, row %d",clusters,s,row) ;

			daq_sim_cld_x *dc = (daq_sim_cld_x *) cld_sim->request(clusters) ;
			
			for(int i=0;i<clusters;i++) {
				fcf[s]->fcf_decode(p_buff,dc,version) ;
				
				p_buff += ints_per_cluster ;
				dc++ ;
			}

			cld_sim->finalize(clusters,s,row) ;
		}
	}

	cld_sim->rewind() ;

	free(buff) ;

	return cld_sim ;
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
			dta->finalize(tb_cou, sector+1, row, pad) ;
		}
		else {
			dta->finalize(tb_cou, sector+1, rdo*16+port, fee_id*256+ch) ;

		}

		if(err) {
			LOG(ERR,"%d %d %d done, %d",sector+1,fee_id,ch,tb_cou) ;
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
		//dta += 4 ;
		//words -= 4 ;

		it->sector_id = s ;
		it->rdo_id = r ;
		sampa_c.rdo = r ;

//		ret = it->rdo_scan(dta,words) ;
		ret = it->rdo_scan_top(dta,words) ;

		if(ret < 0) LOG(ERR,"rdo_scan S%d:%d, words %d, ret %d",s,r,words,ret) ;
		else LOG(NOTE,"rdo_scan S%d:%d, words %d, ret %d",s,r,words,ret) ;


	}}

	sampa->rewind() ;

	return sampa ;
}


daq_dta *daq_itpc::handle_cld(int sec)
{

	int min_sec, max_sec ;
	itpc_fcf_c *fcf ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	if((sec <= 0)||(sec>24)) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = sec ;
	}


	cld->create(1024,(char *)"cld",rts_id,DAQ_DTA_STRUCT(daq_cld)) ;


	for(int s=min_sec;s<=max_sec;s++) {
		char str[128] ;	

	
		sprintf(str,"%s/sec%02d/cld",sfs_name,s) ;

		char *full_name = caller->get_sfs_name(str) ;

		LOG(DBG,"name [%s] -> full_name [%s]",str,full_name) ;

		if(full_name == 0) continue ;	

		int size = caller->sfs->fileSize(full_name) ;

		LOG(NOTE,"full_name [%s] --> size %d",full_name,size) ;

		if(size <= 0) continue ;	// this is really an error!

		char *mem = (char *) malloc(size) ;

		if(mem==0) {
			LOG(CRIT,"ITPC: error in %d %d",s) ;
			break ;
		}

		caller->sfs->read(full_name,mem,size) ;

		u_int *end_buff = (u_int *)(mem+size) ;
		u_int *p_buff = (u_int *)mem ;

		while(p_buff < end_buff) {
			// ints-per-cluster | row
			// version
			// count of clusters

			u_int row = *p_buff++ ;
			u_int version = *p_buff++ ;
			u_int int_cou = *p_buff++ ;

			int ints_per_cluster = (row>>16) ;
			row &= 0xFFFF ;

			//LOG(TERR,"ROW %d: cou %d[0x%X], version 0x%04X, ints_per_cluster %d",row,int_cou,int_cou,version,ints_per_cluster) ;

			int clusters = int_cou/ints_per_cluster ;	

			daq_cld *dc = (daq_cld *)cld->request(clusters) ;

			for(int i=0;i<clusters;i++) {
				fcf->fcf_decode(p_buff,dc,version) ;

				p_buff += ints_per_cluster ;	// for now, but depends on version!
				dc++ ;
			}

			cld->finalize(clusters,s,row,0) ;
		}

		free(mem) ;

	}

	cld->rewind() ;

	return cld ;
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
	d = ((d>>16)&0xFFFF)|(d<<16) ;

/*
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;
*/

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
	u_int trg_cou ;
	int t_cou = 0 ;
	u_int evt_status ;
	int trl_ix ;
	u_int ds ;
	int rdo_version ;
	char buff[128] ;
	int buff_cou ;

	u_int *d = (u_int *)addr + 4 ;	// skip header
	words -= 4 ;

	// NOTE that since Dec 2017 the 16 bit words are swapped!!!

	// since there are buggy TEF events without the start comma lets' search for it
	for(int i=0;i<16;i++) {
		if(d[i] == 0xCCCC001C) {
			d = d + i ;
			words-- ;
			break ;
		}
	}

	if(sw16(d[0]) != 0x001CCCCC) {	// expect start-comma
		LOG(ERR,"First word 0x%08X of %d words - bad",d[0],words) ;
		err |= 1 ;
		goto err_end ;
	}

	//LOG(TERR,"   0x%08X 0x%08X 0x%08X", sw16(d[1]),sw16(d[2]),sw16(d[words-1])) ;
	ds = sw16(d[1]) ;	// first payload word from RDO

	if(ds==0x980000004) {	// Pre-Mar 2018 triggered event
		rdo_version = 0 ;
	}
	else if((ds&0xFF00000F)==0x98000004) {	// Apr+ 2018 triggered event
		rdo_version = (ds >> 4) & 0xFF ;
	}
	else {
		// Hm, not a triggered event!?
		trg[0].t = 4096 ;	// a "log" event
		trg[0].daq = 0 ;
		trg[0].trg = 0 ;
	
		LOG(WARN,"%d: not a triggered event 0x%08X",rdo,ds) ;

		return 1 ;	
	}


	switch(rdo_version) {
	case 0 :
		if(sw16(d[2]) != 0x12340000) {	// wrong version
			LOG(ERR,"%d: wrong version 0x%X",rdo,d[2]) ;
			err |= 2 ;
			goto err_end ;
		}
	
		trg_fired = sw16(d[3]) ;
		break ;
	case 1 :
		trg_fired = sw16(d[2]) ;
		break ;
	default :
		LOG(ERR,"%d: wrong version 0x%X",rdo,rdo_version) ;
		err |= 2 ;
		goto err_end ;
	}

	//find trailer start-header, scanning from the end
	if(words < 1) {
		LOG(ERR,"%d: bad words %d",rdo,words) ;
		err |= 0x100 ;
		goto err_end ;
	}


	if(trg_fired==0) {
		if(rdo_version==1) {
			if(sw16(d[3])==0x980000FC) {
				//LOG(TERR,"RDO_mon") ;
				u_int *dd = d+4 ;
				u_int *dd_end = d+words ;
				buff_cou = 0 ;
				while(dd < dd_end) {
					u_int sd = sw16(*dd) ;
					u_int dd_r = sd & 0xFFFFFF00 ;

					if(sd==0x580000FD) {
						//LOG(TERR,"RDO_mon end") ;
						if(buff_cou) {
							buff[buff_cou++] = 0 ;
							LOG(INFO,"RDO_asc(get_l2) %d: \"%s\"",rdo,buff) ;
							buff_cou = 0 ;
						}
						break ;
					}
					else if(dd_r==0x9800F500) {
						//LOG(TERR,"... 0x%08X",sd) ;

						int c = sd & 0xFF ;
						if(c=='\n' || buff_cou==120) {
							buff[buff_cou++] = 0 ;
							LOG(INFO,"RDO_asc(get_l2) %d: \"%s\"",rdo,buff) ;
							buff_cou = 0 ;
						}
						else {
							buff[buff_cou++] = c ;
						}
					}
					else LOG(TERR,"... 0x%08X",sd) ;
					dd++ ;
				}
			}
		}
	}

	trl_ix = -1 ;

	// hunt backwards for the signature but skup a bunch of last words which are just
	// filler (sentinels) == 0
	for(int i=(words-6);i>=0;i--) {
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

	trl_ix++ ;	// skip 0x98001000

	if(rdo_version==0) {
		if(sw16(d[trl_ix++]) != 0xABCD0000) {
			LOG(ERR,"Wrong trailer word ABCD") ;
			err |= 0x40 ;
			goto err_end ;
		}
	}

	evt_status = sw16(d[trl_ix++]) ;
	trg_cou = sw16(d[trl_ix++]) ;

	if(evt_status) {
		LOG(ERR,"RDO %d: get_l2: evt status 0x%08X, trg_fired 0x%08X, trg_cou %d",rdo,evt_status,trg_fired,trg_cou) ;
		trg[0].t = -ETIMEDOUT ;
		trg[0].daq = trg[0].trg = 0 ;
		return 1 ;
	}

	//LOG(TERR,"trg_cou %d, fired %d",trg_cou,trg_fired) ;

	trg[0].reserved[0] = trg_fired ;

	if(trg_cou > 128) {
		LOG(ERR,"Bad trg cou 0x%08X",trg_cou) ;
		err |= 0x80 ;
		goto err_end ;
	}
	else if (trg_cou > 60) {
		LOG(WARN,"%d: Lots of triggers %d",rdo,trg_cou) ;
	}

	for(u_int i=0;i<trg_cou;i++) {
		if(trl_ix >= words) {
			LOG(WARN,"%d: %d/%d = 0x%08X",rdo,trl_ix,words,sw16(d[trl_ix])) ;
			trg[i+1].reserved[0] = 0 ;
			trl_ix++ ;
		}
		else {
			trg[i+1].reserved[0] = sw16(d[trl_ix++]) ;
		}
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

		if(((v&0xFFF00000) != 0x04300000)&&((v&0xFFF00000)!=0x08300000)) {	// 0x043 external trigger, 0x083 local trigger
			LOG(ERR,"RDO %d: 0x%08X: %d %d %d",rdo,trg[0].reserved[0],trg[0].t,trg[0].trg,trg[0].daq) ;
		}
			
		t_cou++ ;

	}

	for(u_int i=1;i<(trg_cou+1);i++) {
		u_int v = trg[i].reserved[0] ;
		u_int t ;

		if(v==0) continue ;

		t = ((v>>8)&0xF)<<8 ;
		t |= ((v>>12)&0xF)<<4 ;
		t |= ((v>>16)&0xF) ;

		trg[t_cou].trg = v & 0xF ;
		trg[t_cou].daq = (v>>4) & 0xF ;
		trg[t_cou].t = t ; 

		//LOG(TERR,"trg %d: 0x%08X: trg %d",i,v,v&0xF) ;

		if(trg[t_cou].trg>=4 && trg[t_cou].trg<13) {	// FIFO trg
			//LOG(WARN,"RDO %d: %d/%d: 0x%08X: %d %d %d",rdo,i,(trg_cou+1),
			//    v,
			//    trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			

			if((v&0xFFF00000) != 0x04300000) {
				LOG(NOTE,"RDO %d: %d/%d: 0x%08X: %d %d %d",rdo,i,(trg_cou+1),
				    v,
				    trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			}
			else {
				continue ;
			}
		}
		if(trg[t_cou].trg==14) {	// HACK!!!
			continue ;
		}

		//if(trg[t_cou].trg==2 && trg[t_cou].t==10 && trg[t_cou].daq==3) continue ;
		if(trg[t_cou].trg<=2) {
			LOG(WARN,"%d: odd trg_cmd: T %d, trg %d, daq %d",rdo,trg[t_cou].t,trg[t_cou].trg,trg[t_cou].daq) ;
			continue ;
		}
		t_cou++ ;
	}


//	for(int i=0;i<t_cou;i++) {
//		const char *s = "" ;
//		if((trg[i].reserved[0] & 0xFFF00000)!=0x04300000) s = "OVERRUN" ;
//
//		LOG(TERR,"%d: %d/%d: T %d, trg %d [0x%08X %s]",rdo,i+1,t_cou,trg[i].t,trg[i].trg, trg[i].reserved[0],s) ;
//	}

	return t_cou ;

	err_end:;

	LOG(ERR,"RDO %d: Error in get_l2 0x%X [words %d]",rdo,err,words) ;

	return 0 ;


#endif



}

