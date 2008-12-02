#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_tpx.h"

#include <DAQ_TPC/daq_tpc.h>	// solely for the "legacy" use!


#include "tpxCore.h"
#include "tpxGain.h"
#include "tpxPed.h"
#include "tpxFCF.h"
#include "tpxStat.h"

#include <DAQ1000/ddl_struct.h>


class daq_det_tpx_factory : public daq_det_factory
{
public:
        daq_det_tpx_factory() {
                daq_det_factory::det_factories[TPX_ID] = this ;
        }

        daq_det *create() {
                return new daq_tpx ;
        }
} ;

static daq_det_tpx_factory tpx_factory ;



const char *daq_tpx::help_string = "\
\n\
****** TPX Help ******* \n\
\n\
Sector is [1..24]\n\
Rdo    is [1..6]\n\
\n\
Supported Banks: \n\
	raw	(sector,rdo); returns (char *) of start of DDL data\n\
	adc	(sector,rdo); returns tb_adc data\n\
	cld	(sector,rdo); returns float cld from data\n\
	cld_raw	(sector,rdo); returns cld_raw CLD data\n\
\n\
m_Mode:\n\
	(1<<0)	calc pedestals from data when get(adc) called\n\
	(1<<1)	calc gain from data when get(adc) called\n\
\n\
Not yet done: \n\
	cld_c	(sector,rdo); returns float cld calculated from data\n\
	gain	(sector,rdo,0,char *fname); loads gains from database: \n\
			fname == 0	from Offline databases\n\
			fname != 0	from file fname\n\
	gain_c	(sector,rdo); returns calculated gains (see m_Mode) \n\
	ped	(sector,rdo); returns ped/rms from file\n\
	ped_c	(sector,rdo); returns calculated ped/rms (see m_Mode) \n\
	log\n\
" ;


void daq_tpx::help() const 
{ 
	printf("%s\n%s\n",GetCVS(),help_string) ; 
} ;

daq_tpx::daq_tpx(daqReader *rts_caller) 
{
	// override mother...

	rts_id = TPX_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "tpx" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	// create now!
	legacy = new daq_dta ;

	raw = new daq_dta ;	// in file, compressed
	cld_raw = new daq_dta ;	// in file, compressed
	ped_raw = new daq_dta ;

	adc = new daq_dta ;	// from "raw", decompressed

	cld = new daq_dta ;	// from "cld_raw", decompressed

	adc_sim = new daq_dta ;	// external input
	gain = new daq_dta ;
	ped = new daq_dta ;
	
	cld_sim = new daq_dta ;	// from "adc_sim", decompressed
	
	ped_c = new daq_dta ;
	gain_c = new daq_dta ;


	gain_algo = new tpxGain() ;	// always needed for bad pads...
	stat_algo = 0 ;
	ped_algo = 0 ;
	for(int i=0;i<=24;i++) {
		fcf_algo[i] = 0 ;
	}
	fcf_tmp_storage = 0 ;
	
	LOG(DBG,"%s: constructor: caller %p",name, caller) ;
	return ;
}

daq_tpx::~daq_tpx() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	// daq data 
	delete raw ;
	delete cld_raw ;
	delete ped_raw ;

	delete adc ;

	delete cld ;

	delete adc_sim ;
	delete gain ;
	delete ped ;

	delete cld_sim ;

	delete ped_c ;
	delete gain_c ;

	delete legacy ;

	// algorithms and associated storage...
	if(gain_algo) delete gain_algo ;
	if(ped_algo) delete ped_algo ;
	if(stat_algo) delete stat_algo ;

	for(int i=0;i<=24;i++) {
		if(fcf_algo[i]) delete fcf_algo[i] ;
	}
	if(fcf_tmp_storage) free(fcf_tmp_storage) ;

	return ;
}


#if 0
/*
	For online: in_buffer, in_bytes, out_buffer, out_bytes MUST be set!
*/
int daq_tpx::Make()
{
	int pres ;
	int mode ;


	present = 0 ;

	mode = GetMode() ;

	pres = presence() ;
	if(pres) {
		evt_num++ ;
		present |= 2 ;
		LOG(NOTE,"%s: Make(): presence %d, evt %d: m_Mode 0x%08X, event_mode 0x%08X",name,pres,evt_num,mode,event_mode) ;
	}
	else {
		LOG(DBG,"%s: Make(): not found in this event...",name) ;
		return 0 ;
	}


	if(mode & m_Mode_DAQ_RT) {
		// Online! DO NOT TOUCH THIS!
		// it works _ONLY_ for one RDO of one sector!

		tpx_rdo_event rdo ;

		char *obuff = out_buffer ;
		int max_bytes = out_bytes ;
		out_bytes = 0 ;	// clear

		if((in_buffer == 0) || (in_bytes<=0) || (out_buffer==0) || (max_bytes==0)) {
			LOG(ERR,"Bad buffers -- skipping") ;
			return -1 ;
		}
		
		// get the tpx_rdo structure which is all we need for later...
		ret = tpx_get_start(in_buffer, in_bytes/4, &rdo, 0) ;
		
		if(ret < 0) {
			LOG(ERR,"Horrible error -- skipping") ;
			return -1 ;
		}

		int do_raw = 0 ;
		switch(rdo.type) {
		case DDL_TYPE_DTA:
			if(event_mode & m_Mode_DAQ_RAW) {
				do_raw = 1 ;
			}
			break ;
		case DDL_TYPE_LOG :
			// tpxCore deal with log
			do_raw = 1 ;
			break ;
		default :
			// tpxCore deal with misc
			do_raw = 1 ;
			break ;
		}

		if(do_raw) {
			ret = Raw->Make(&rdo,obuff,max_bytes) ;
			obuff += ret ;
			max_bytes -= ret ;
		}


		if(rdo.type != DDL_TYPE_DTA) return 0 ;	// that's it

		// always!
		Stat->Make(&rdo) ;


		if(event_mode & m_Mode_DAQ_GAIN) {
			Gain->Make(&rdo) ;
		}
		else if(event_mode & m_Mode_DAQ_PED) {
			Ped->Make(&rdo) ;
		}
		else if(event_mode & m_Mode_DAQ_FCF_RAW) {
			ret = Fcf->Make(&rdo, obuff, max_bytes) ;
			obuff += ret ;
			max_bytes -= ret ;
		}


		out_bytes = obuff - out_buffer ;
		return 0 ;	// all OK!
	}

	// Offline is here...
	int min_sec, max_sec ;
	int min_rdo, max_rdo ;

	if(def_sector <= 0) {
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else {
		min_sec = max_sec = def_sector ;
	}

	if(def_rdo <= 0) {
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else {
		min_rdo = max_rdo = def_rdo ;
	}


	// At this event, SFS file can have:
	//	rb01/raw
	//	rb01/cld
	//	rb01/ped	(token 0 only!)
	//	rb01/gain	(token 0 only!)

	// FCF needs cld
	// 


	return 0 ;
}

#endif

int daq_tpx::InitRun(int run)
{
	u_int mode = GetMode() ;

	LOG(NOTE,"%s: InitRun(%d), m_Mode 0x%08X",name, run, mode) ;

	
	run_num = run ;
	evt_num = 0 ;

	// for all runs
		


	// when working in Online
	if(mode & m_Mode_DAQ_RT) {
		LOG(ERR,"Not yet ready for Online!") ;
		return -1 ;
	}


	// offline setup: try our canonical location first
	gain_algo->from_file("/RTS/conf/tpx/tpx_gains.txt",0) ;	// all sectors!

	// if we have externally applied gains we will use them,
	// otherwise we'll try to use them from the canonical location...
	// look into "gains and set the found channels there
	daq_dta *g = get("gain") ;

	LOG(DBG,"get(gain) returns %p",g) ;
	if(g) {
		LOG(TERR,"Using externally generated gains") ;
		while(g->iterate()) {
			LOG(DBG,"\tsec %d, row %d: %d",g->sec,g->row,g->ncontent) ;
			for(u_int pad=1;pad<g->ncontent;pad++) {
				LOG(DBG,"Gains: %d %d %d %f %f",g->sec,g->row,pad,g->gain[pad].gain,g->gain[pad].t0) ;
				gain_algo->set_gains(g->sec,g->row,pad,g->gain[pad].gain,g->gain[pad].t0) ;
			}
		}
	}

	
	if(mode & m_Mode_DAQ_PED) {
		if(ped_algo==0) {
			ped_algo = new tpxPed ;
		}

//		ped_algo->init() ;	// ALL sectors

	}
	
	if(mode & m_Mode_DAQ_GAIN) {
		gain_algo->init(def_sector) ;	// ALL sectors!
	}


	// what about FCF?
	


	return 0 ;
}

int daq_tpx::FinishRun(int old)
{
	u_int mode = GetMode() ;

	LOG(NOTE,"%s: Run %d finished with %d events, m_Mode 0x%08X",name,run_num,evt_num,mode) ;

	if(mode & m_Mode_DAQ_PED) {
		LOG(NOTE,"Calculating peds") ;
			
		assert(ped_algo) ;

//		ped_algo->calc() ;

		// dump them to "ped_c"!
		
		delete(ped_algo) ;
		ped_algo = 0 ;

	}
	
	if(mode & m_Mode_DAQ_GAIN) {
		
		LOG(NOTE,"Calculating gains") ;

//		gain_algo->calc() ;
	
		// dump them to "gain_c"!
	}


	// we free FCF storage, if any!
	for(int i=0;i<=24;i++) {
		if(fcf_algo[i]) {
			delete fcf_algo[i] ;
			fcf_algo[i] = 0 ;
		}
	}

	if(fcf_tmp_storage) {
		free(fcf_tmp_storage) ;
		fcf_tmp_storage = 0 ;
	}


	return 0 ;
}



daq_dta *daq_tpx::get(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
{
	const char *bank ;



	if(in_bank==0) {	// just wants to know if I'm here so return some read-only non-NULL memory
		bank = "cld" ;	// default		
	}
	else {
		bank = in_bank ;
	}

	LOG(DBG,"%s: looking for bank %s",name,bank) ;


	// list created banks first...
	if(strcasecmp(bank,"adc_sim")==0) {
		if(adc_sim->is_empty()) return 0 ;
		adc_sim->rewind() ;
		return adc_sim ;
	}
	else if(strcasecmp(bank,"gain")==0) {
		if(gain->is_empty()) return 0 ;	// not created!
		gain->rewind() ;	// we want to use the bank...
		return gain ;
	}
	else if(strcasecmp(bank,"cld_sim")==0) {
		return handle_cld_sim(sec,row) ;
	}


	// after this all the banks need to be in the file...
	Make() ;
	if(!present) return 0 ;	// this det is not in this event...


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,row) ;		// actually sec, rdo; r
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(sec,row) ;	// actually sec, rdo:
	}
	else if(strcasecmp(bank,"cld_raw")==0) {
		return handle_cld_raw(sec,row) ;	// actually sec, rdo:
	}
	else if(strcasecmp(bank,"cld")==0) {
		return handle_cld(sec,row) ;	// actually sec, rdo:
	}
	else if(strcasecmp(bank,"pedrms")==0) {
		return handle_ped(sec,row) ;	// actually sec, rdo:
	}
	else if(strcasecmp(bank,"legacy")==0) {
		return handle_legacy(sec,row) ;	// actually sec, rdo:
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}


	return 0 ;
}

daq_dta *daq_tpx::put(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
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

daq_dta *daq_tpx::handle_legacy(int sec, int rdo)
{
	int max_s, min_s ;
	daq_dta *dd ;
	int found_something = 0 ;

	if(sec <= 0) {
		min_s = 1 ;
		max_s = 24 ;
	}
	else {
		min_s = max_s = sec ;
	}

	legacy->create(1,"tpx_legacy",rts_id,DAQ_DTA_STRUCT(tpc_t)) ;

	for(int s=min_s;s<=max_s;s++) {
		struct tpc_t *tpc_p = 0 ;

		// check for pedestal data first!
		dd = handle_ped(s,-1) ;
		if(dd) {
			tpc_p = (struct tpc_t *) legacy->request(1) ;
			memset(tpc_p,0,sizeof(tpc_t)) ;

			

			tpc_p->mode = 1 ;	// pedestal mode!
			
			while(dd->iterate()) {

				found_something = 1 ;
			}

			legacy->finalize(1,s,0,0) ;


			continue ;	// do NOT allow other ADC checks!
		}

		// grab the ADC data first...
		dd = handle_adc(s,-1) ;
		if(dd) {
			tpc_p = (struct tpc_t *) legacy->request(1) ;
			memset(tpc_p,0,sizeof(tpc_t)) ;			
			
			while(dd->iterate()) {
				int r = dd->row - 1 ;
				int p = dd->pad - 1 ;

				for(u_int i=0;i<dd->ncontent;i++) {
					//tpc_p->adc[r][p][pr_cou[r][p]] = dd->adc[i].adc ;
					//tpc_p->adc[r][p][pr_cou[r][p]] = dd->adc[i].tb ;

					tpc_p->channels_sector++ ;

				}
				found_something = 1 ;
			}
		}

		// grab CLD data next
		dd = handle_cld(s,-1) ;
		if(dd) {
			if(tpc_p == 0) {
				tpc_p = (struct tpc_t *) legacy->request(1) ;
				memset(tpc_p,0,sizeof(tpc_t)) ;
			}

			while(dd->iterate()) {


				found_something = 1 ;
			}

			tpc_p->has_clusters = 1 ;

		}

		if(tpc_p) {
			tpc_p->mode = 0 ;
			//...

			legacy->finalize(1,s,0,0) ;
		}

	}

	if(found_something) return legacy ;
	else return 0 ;

}

daq_dta *daq_tpx::handle_ped(int sec, int rdo)
{
	char str[128] ;
	int tot_bytes ;
	int min_sec, max_sec, min_rdo, max_rdo ;

	// sanity
	if(sec <= 0) {		// ALL sectors
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	if(rdo <= 0) {		// ALL RDOs in this sector
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<1) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	assert(caller) ;


	// calc total bytes
	tot_bytes = 0 ;

	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {

		sprintf(str,"%s/sec%02d/rb%02d/pedrms",sfs_name, s, r) ;
	
		LOG(NOTE,"%s: trying sfs on \"%s\"",name,str) ;

		char *full_name = caller->get_sfs_name(str) ;
		if(full_name == 0) continue ;	// not in this event...

		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"%s: sector %d, rdo %d : ped size %d",name,s,r,size) ;

		if(size <= 0) {
			assert(!"can't be 0") ;
		}

		// allocate temporary storage
		char *tmp_cache = (char *) valloc(size) ;
		assert(tmp_cache) ;

		// read in the data into temp storage...
		int ret = caller->sfs->read(full_name, tmp_cache, size) ;

		if(ret != (int)size) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				size,ret,strerror(errno)) ;
			free(tmp_cache) ;
			continue ;
		}


		if(tot_bytes == 0) {	// nothing done so far so we will create the first guess...
			ped->create(size,"ped",rts_id,DAQ_DTA_STRUCT(daq_det_pedrms)) ;
		}
		tot_bytes += size ;


		// do the actual decoding...
		u_short *d16 = (u_short *) tmp_cache ;

		while(size > 0) {
			u_int r_id = l2h32(*(u_int *)d16) ;

			size -= 4 ;	// 1 int == 4 bytes
			d16 += 2 ;	// 1 int == 2 shorts

			int row = (r_id & 0xFF000000) >> 24 ;
			int pad = (r_id & 0x00FF0000) >> 16 ;
			int cou = (r_id & 0x0000FFFF) ;

			size -= cou * 2 ;	// data is shorts

			daq_det_pedrms *d = (daq_det_pedrms *) ped->request(cou*10) ;	// force more allocation
			
			for(int i=0;i<cou;i++) {
				u_short tmp = l2h16(*d16++) ;

				int i_rms = (tmp & 0xFC00) >> 10 ;
				int i_ped = tmp & 0x3FF ;

				d[i].rms = (float)i_rms / 16.0 ;
				d[i].ped = i_ped ;

			}

			ped->finalize(cou,s,row,pad) ;
		}

		free(tmp_cache) ;	// release temporary storage...

		
	}
	}

	
	LOG(DBG,"Returning from ped_handler") ;

	if(tot_bytes == 0) return 0 ;	// nothing found...

	ped->rewind() ;
	return ped ;

}


daq_dta *daq_tpx::handle_adc(int sec, int rdo)
{

	int min_sec, max_sec ;
	int min_rdo, max_rdo ;


	// sanity
	if(sec <= 0) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = sec ;
		max_sec = sec ;
	}

	if(rdo <= 0) {
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<0) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	// get a size estimate
	int rdos = 0 ;
	
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		rdos++ ;
	}
	}

	// guess the byte size...
	int guess_bytes = rdos * 1152 * (sizeof(daq_store) + 10*sizeof(daq_adc_tb)) ;

	adc->create(guess_bytes,(char *)"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;


	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		daq_dta *rdo_dta ;


		char *rdo_ptr ;
		struct tpx_rdo_event rdo ;
		struct tpx_altro_struct a ;
		int rdo_words ;

		LOG(NOTE,"Calling handle_raw for %d:%d",s,r) ;		
		rdo_dta = handle_raw(s, r) ;	// 	bring the raw data in, RDO-by_RDO!


		if(rdo_dta == 0) {
			LOG(WARN,"rdo_dta NULL?") ;
			continue ;	// sorry, not found...
		}

		int ret = rdo_dta->iterate() ;	// move from the header...
		if(ret==0) {	// no content
			continue ;
		}

		LOG(DBG,"Called handle_raw for %d:%d, iterate %d, returned %d objs",s,r,ret,rdo_dta->ncontent) ;				
		if(rdo_dta->ncontent == 0) continue ;	// nothing found...

		rdo_ptr = (char *)rdo_dta->Byte ;
		rdo_words = rdo_dta->ncontent / 4 ;

		int token = tpx_get_start(rdo_ptr, rdo_words, &rdo, 0) ;

		if(token <= 0) {
			LOG(ERR,"horrible error, token is %d?",token) ;
			continue ;
		}


		u_int *data_end = rdo.data_end ;
		a.rdo = rdo.rdo -1 ;
		a.t = token ;
		a.what = TPX_ALTRO_DO_ADC ;
		a.log_err = 0 ;

		do {
			data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;		

			if(a.count == 0) continue ;	// no data for this guy...
	
			daq_adc_tb *at = (daq_adc_tb *) adc->request(a.count) ;
	
			//LOG(DBG,"%d: %d:%d %d",adc->obj_cou,a.row,a.pad,a.count) ;

			for(u_int i=0 ; i < a.count ; i++) {
				at[i].adc = a.adc[i] ;
				at[i].tb = a.tb[i] ;

			}

			adc->finalize(a.count, s, a.row, a.pad) ;

		} while(data_end && (data_end > rdo.data_start)) ;	


	}
	}


	adc->rewind() ;	// wind data pointers to the beginning so that they can be used


	

	return adc ;

}


	

daq_dta *daq_tpx::handle_raw(int sec, int rdo)
{
	char str[128] ;
	int tot_bytes ;
	int min_sec, max_sec, min_rdo, max_rdo ;
	struct {
		int sec ;
		int rb ;
		u_int bytes ;
	} obj[24*6] ;

	// sanity
	if(sec <= 0) {		// ALL sectors
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	if(rdo <= 0) {		// ALL RDOs in this sector
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<1) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	assert(caller) ;


	// calc total bytes
	tot_bytes = 0 ;
	int o_cou = 0 ;

	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {


		//sprintf(str,"%s/%s/sec%02d/rb%02d/adc",caller->fs_cur_evt, "tpx", s, r) ;
		sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name, s, r) ;
	
		LOG(NOTE,"%s: trying sfs on \"%s\"",name,str) ;

		char *full_name = caller->get_sfs_name(str) ;
		if(full_name == 0) continue ;

		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"%s: sector %d, rdo %d : raw size %d",name,s,r,size) ;

		if(size <= 0) {
			if(size < 0) {
				LOG(DBG,"%s: %s: not found in this event",name,str) ;
			}
			continue ;
		}
		else {
			obj[o_cou].rb = r ;
			obj[o_cou].sec = s ;
			obj[o_cou].bytes = size ;

			o_cou++ ;

			tot_bytes += size ;

			LOG(DBG,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	raw->create(tot_bytes,(char *)"raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	// bring in the bacon from the SFS file....
	for(int i=0;i<o_cou;i++) {
		
		sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name,obj[i].sec, obj[i].rb) ;
		char *full_name = caller->get_sfs_name(str) ;
		if(!full_name) continue ;

		LOG(NOTE,"%s: request %d bytes",name,obj[i].bytes) ;
		
		char *mem = (char *) raw->request(obj[i].bytes) ;

		int ret = caller->sfs->read(full_name, mem, obj[i].bytes) ;

		if(ret != (int)obj[i].bytes) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				obj[i].bytes,ret,strerror(errno)) ;
		}
		else {
			LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
		}
		
		raw->finalize(obj[i].bytes, obj[i].sec, obj[i].rb, 0) ;
	}

	
	LOG(DBG,"Returning from raw_handler") ;
	raw->rewind() ;
	return raw ;

}

daq_dta *daq_tpx::handle_cld_raw(int sec, int rdo)
{
	char str[128] ;
	int tot_bytes ;
	int min_sec, max_sec, min_rdo, max_rdo ;
	struct {
		int sec ;
		int rb ;
		u_int bytes ;
	} obj[24*6] ;

	// sanity
	if(sec <= 0) {		// ALL sectors
		min_sec = 1 ;
		max_sec = 24 ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	if(rdo <= 0) {		// ALL RDOs in this sector
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<1) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	assert(caller) ;


	// calc total bytes
	tot_bytes = 0 ;
	int o_cou = 0 ;
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {


		sprintf(str,"%s/sec%02d/cld%02d",sfs_name, s, r) ;
	
		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		char *full_name = caller->get_sfs_name(str) ;
		if(full_name == 0) continue ;

		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"%s: sector %d, rdo %d : cld size %d",name,s,r,size) ;


		if(size <= 0) {
			if(size < 0) {
				LOG(DBG,"%s: %s: not found in this event",name,str) ;
			}
			continue ;
		}
		else {
			obj[o_cou].rb = r ;
			obj[o_cou].sec = s ;
			obj[o_cou].bytes = size ;

			o_cou++ ;

			tot_bytes += size ;

			LOG(DBG,"%s: %s: reading in \"%s\": bytes %d",name,str,"cld_raw", size) ;
		}
	}
	}

	cld_raw->create(tot_bytes,(char *)"cld_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	// bring in the bacon from the SFS file....
	for(int i=0;i<o_cou;i++) {
		
		sprintf(str,"%s/sec%02d/cld%02d",sfs_name,obj[i].sec, obj[i].rb) ;
		char *full_name = caller->get_sfs_name(str) ;
		if(full_name==0) continue ;

		char *mem = (char *) cld_raw->request(obj[i].bytes) ;
		
		int ret = caller->sfs->read(full_name, mem, obj[i].bytes) ;

		if(ret != (int)obj[i].bytes) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				obj[i].bytes,ret,strerror(errno)) ;
		}
		else {
			LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
		}

		cld_raw->finalize(obj[i].bytes, obj[i].sec, obj[i].rb, 0) ;
	}

	
	LOG(DBG,"Returning from cld_raw_handler") ;
	cld_raw->rewind() ;

	return cld_raw ;

}


daq_dta *daq_tpx::handle_cld(int sec, int rdo)
{

	int min_sec, max_sec ;
	int min_rdo, max_rdo ;


	// sanity
	if(sec <= 0) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = sec ;
		max_sec = sec ;
	}

	if(rdo <= 0) {
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<0) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	// get a size estimate
	int rdos = 0 ;
	
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		rdos++ ;
	}
	}

	// guess the byte size...
	int guess_bytes = rdos * (1152/10) * (sizeof(daq_store) + 10*sizeof(daq_cld)) ;

	cld->create(guess_bytes,(char *)"cld",rts_id,DAQ_DTA_STRUCT(daq_cld)) ;


	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		daq_dta *dd ;


		LOG(DBG,"Calling handle_cld_raw for %d:%d",s,r) ;		
		dd = handle_cld_raw(s, r) ;	// 	bring the raw data in, RDO-by_RDO!


		if(dd == 0) {
			continue ;	// sorry, not found...
		}

		int ret = dd->iterate() ;	// move from the header...
		if(ret==0) {	// no content
			continue ;
		}

		LOG(DBG,"Called handle_cld_raw for %d:%d, iterate %d, returned %d objs",s,r,ret,dd->ncontent) ;				

		int bytes = dd->ncontent ;
		if(bytes <= 0) continue ;

		u_int *buff = dd->Int32 ;
		
		u_int *end_buff = buff + bytes/4 ;
		u_int *p_buff = buff ;

		while(p_buff < end_buff) {
			u_int row = *p_buff++ ;
			u_int cou = *p_buff++ ;
			

			u_int version = (row >> 16) ;
			row &= 0xFFFF ;

			daq_cld *dc = (daq_cld *) cld->request(cou) ;
	

			for(u_int i=0;i<cou;i++) {
				p_buff += fcf_algo[0]->fcf_decode(p_buff, dc, version) ;
				dc++ ;
			}

			cld->finalize(cou,s,row,0) ;

		
#ifdef OLD_WAY
			while(cou) {
				daq_store *st = cld->get() ;
				
				st->sec = s ;
				st->row = row ;
				st->nitems = 1 ;	// just one

				//daq_cld *dc = (daq_cld *)(st+1) ;

				double p, t ;
				int p1,p2,t1,t2,cha,fla ;
				int ptmp ;

				fla = 0 ;

				// pad
				u_int tmp = *p_buff & 0xFFFF ;
				if(tmp & 0x8000) fla |= FCF_MERGED ;
				if(tmp & 0x4000) fla |= FCF_DEAD_EDGE ;

				p = (double)(tmp & 0x3FFF) / 32.0 ;
				
				// time
				tmp = *p_buff >> 16 ;
				if(tmp & 0x8000) fla |= FCF_ONEPAD ;
				t = (double)(tmp & 0x7FFF) / 32.0 ;
				
				p_buff++ ;

				cha = *p_buff >> 16 ;

				if(cha >= 0x8000) fla |= FCF_BIG_CHARGE ;

				ptmp = *p_buff & 0xFFFF ;

				if(ptmp & 0x8000) fla |= FCF_ROW_EDGE ;
				if(ptmp & 0x4000) fla |= FCF_BROKEN_EDGE ;

				t1 = ptmp & 0xF ;
				t2 = (ptmp >> 4) & 0xF ;


				p1 = (ptmp >> 8) & 0x7 ;
				p2 = (ptmp >> 11) & 0x7 ;

				t1 = (u_int)t - t1 ;
				t2 = (u_int)t + t2 ;
				p1 = (u_int)p - p1 ;
				p2 = (u_int)p + p2 ;

				dc->t1 = t1 ;
				dc->t2 = t2 ;
				dc->p1 = p1 ;
				dc->p2 = p2 ;
				dc->charge = cha ;
				dc->flags = fla ;
				dc->pad = p ;
				dc->tb = t ;

				cld->commit() ;

				p_buff++ ;
				//LOG(INFO,"\trow %d\t%d: %f [%d:%d], %f [%d:%d] %d 0x%04X",row,cou,p,p1,p2,t,t1,t2,cha,fla) ;
				cou-- ;
			}
#endif

		}



	}
	}


	cld->rewind() ;	// wind data pointers to the beginning so that they can be used
	

	return cld ;

}


	

static int cmpr_sim_adc(const void *first, const void *second)
{
	daq_sim_adc_tb *f = (daq_sim_adc_tb *)first ;
	daq_sim_adc_tb *s = (daq_sim_adc_tb *)second ;

	if(f->tb == s->tb) return 0 ;
	if(f->tb > s->tb) return -1 ;	// reverse sort!

	return 1 ;
}


/*
	cld_sim works on sector row!
*/
daq_dta *daq_tpx::handle_cld_sim(int sec, int row)
{

	int min_sec, max_sec ;
	int min_row, max_row ;


	// sanity
	if(sec <= 0) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<1) || (sec>24)) return 0 ;
	else {
		min_sec = sec ;
		max_sec = sec ;
	}

	if(row <= 0) {
		min_row = 1 ;
		max_row = 45 ;
	}
	else if((row<0) || (row>45)) return 0 ;
	else {
		min_row = max_row = row ;
	}

	// get a size estimate
	int rows = 0 ;
	
	for(int s=min_sec;s<=max_sec;s++) {
		if(fcf_algo[s]) {
			LOG(DBG,"start_evt(): sec %d\n",s) ;
			fcf_algo[s]->start_evt() ;	// make sure we start a new event!
		}

		for(int r=min_row;r<=max_row;r++) {
			rows++ ;
		}
	}

	// guess the byte size: assume 30 hits per row
	int guess_bytes = rows * (sizeof(daq_store) + 30*sizeof(daq_sim_cld)) ;

	cld_sim->create(guess_bytes,(char *)"cld_sim",rts_id,DAQ_DTA_STRUCT(daq_sim_cld)) ;

	// adc_sim data is flattened out so we will run if we find the sec/row
	// requested
	daq_dta *sim = get("adc_sim") ;

	if(sim==0) {
		LOG(ERR,"%s: you need to add simulated data first!",name) ;
		return cld_sim ;
	}


	while(sim->iterate()) {
		if((min_sec<=sim->sec) && (sim->sec<= max_sec) && (min_row<=sim->row) && (sim->row <= max_row)) ;
		else continue ;

		// this is how I allocate the algorithm
		if(fcf_algo[sim->sec]==0) {
			LOG(NOTE,"No algo assigned for sector %d -- creating one!",sim->sec) ;
			fcf_algo[sim->sec] = new tpxFCF ;
			fcf_algo[sim->sec]->config(0x3F,1) ;	// assume all 6 RDOs; extra data + annotations

			fcf_algo[sim->sec]->apply_gains(sim->sec,gain_algo) ;
			fcf_algo[sim->sec]->start_evt() ;

			if(fcf_tmp_storage==0) {
				fcf_tmp_storage = (u_int *)valloc(FCF_TMP_BYTES) ;
			}
		}

		u_short track_id[512] ;
		tpx_altro_struct a ;

		a.row = sim->row ;
		a.pad = sim->pad ;
		a.count = 0 ;

		// NEED to sort in falling timebin!
		qsort(sim->sim_adc, sim->ncontent, sizeof(sim->sim_adc[0]),cmpr_sim_adc) ;

		
	
		for(u_int i=0;i<sim->ncontent;i++) {
			a.adc[i] = sim->sim_adc[i].adc ;
			a.tb[i] = sim->sim_adc[i].tb ;
			track_id[i] = sim->sim_adc[i].track_id ;

			a.count++ ;
		}


		fcf_algo[sim->sec]->do_pad(&a, sim->sim_adc) ;
		LOG(DBG,"do_pad(): sec %d, row %d, pad %d: %d",sim->sec,a.row,a.pad,a.count) ;
	}



	for(int s=min_sec;s<=max_sec;s++) {
		if(fcf_algo[s]) {
			int words = fcf_algo[s]->stage2(fcf_tmp_storage,FCF_TMP_BYTES) ;
			if(words<=0) continue ;

			LOG(DBG,"Sector %d: %d words",s,words) ;			
			u_int *p_buff = fcf_tmp_storage ;
			u_int *end_buff = p_buff + words ;

			while(p_buff < end_buff) {
				u_int row = *p_buff++ ;
				u_int cou = *p_buff++ ;
				int g_cou = 0 ;
			

				u_int version = (row >> 16) ;
				row &= 0xFFFF ;

				daq_sim_cld *cld = (daq_sim_cld *) cld_sim->request(cou) ;

				while(cou) {
					int skip = fcf_algo[s]->fcf_decode(p_buff, cld + g_cou, version) ;
					
					g_cou++ ;

					p_buff += skip ;
					cou-- ;

				}
			
				
				cld_sim->finalize(g_cou,s,row) ;
			}			
		}
	}

	
	cld_sim->rewind() ;

	return cld_sim ;
}


// knows how to get the token out of an event while trying also find a l0 command
int daq_tpx::get_token(char *addr, int words)
{
	daq_trg_word trgs[128] ;

	get_l2(addr, words, trgs, 1) ;

	

	if(trgs[0].t==0) {
		LOG(ERR,"Token 0 not allowed but I will try to use the other triggers...") ;
		trgs[0].t = 4097 ;
	}


	return trgs[0].t ;

}

// dumps the known accept/abort trigger decisions from
// the FIFO part of the event.
// returns the count
int daq_tpx::get_l2(char *addr, int words, struct daq_trg_word *trgs, int prompt)
{
	struct tpx_rdo_event rdo ;
	int cou = 0 ;
	u_int collision = 0 ;


	tpx_get_start(addr, words, &rdo, prompt) ;

	LOG(DBG,"rdo %d, rdo token %d, trg cou %d",rdo.rdo,rdo.token,rdo.trg_cou) ;

	for(u_int i=0;i<rdo.trg_cou;i++) {
		u_int dta = rdo.trg[i].data ;
		u_int marker = rdo.trg[i].csr >> 24 ;
		u_int rhic = rdo.trg[i].rhic_counter ;

		if((marker==0) || (marker==0xEE)) {	// marks the prompt configuration

			trgs[cou].t = dta & 0xFFF ;
			trgs[cou].daq = (dta >> 12) & 0xF ;
			trgs[cou].trg = (dta >> 16) & 0xF ;
			//trgs[cou].trg = 4 ;	// need to force this for old code...
			trgs[cou].rhic_delta = 0 ;
			trgs[cou].rhic = rhic ;

			if(trgs[cou].t==0) {
				LOG(ERR,"RDO %d: token 0 -- ignoring",rdo.rdo) ;
				continue ;
			}
			// check for overrun
			if((dta & 0x3000000) != 0x2000000) {
				LOG(ERR,"RDO %d: T %d: BUSY overrun: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
			}

			cou++ ;

			collision = rhic ;
		}

	}
	
	if(cou==0) {	// no prompt trigger contrib; use the one from the RDO header.
		// either for a logging event (4096) or the one from the trigger-only (4097)
		
		if(rdo.token != 4096) {
			rdo.token = 4097 ;
			LOG(NOTE,"No triggers in event, making it %d",rdo.token) ;
		}

		trgs[cou].t = rdo.token ;
		trgs[cou].daq = 0 ;
		trgs[cou].trg = 5 ;	// dummy
		trgs[cou].rhic_delta = 0 ;
		trgs[cou].rhic = 0 ;
		

		cou++ ;

	}
	else if (cou > 1) {
		LOG(ERR,"RDO %d: token %d? -- too many prompt contributions!",rdo.rdo,trgs[0].t) ;
	}

	// at this point at least one contribution exists (i.e. cou>=1):
	// real L0 t
	// 4096 for log events
	// 4097 for events without an L0

	if(trgs[0].t == 0) {
		LOG(ERR,"Token 0 in RDO %d: making it 4097",rdo.rdo) ;
		trgs[0].t = 4097 ;
	}

	for(u_int i=0;i<rdo.trg_cou;i++) {
		u_int dta = rdo.trg[i].data ;
		u_int marker = rdo.trg[i].csr >> 24 ;
		u_int rhic = rdo.trg[i].rhic_counter ;

		if(marker==0xFF) {	// FIFO
			trgs[cou].t = dta & 0xFFF ;
			trgs[cou].daq = (dta >> 12) & 0xF ;
			trgs[cou].trg = (dta >> 16) & 0xF ;
			trgs[cou].rhic = rhic ;



			switch(trgs[cou].trg) {
			case 13 :
			case 15 :
				break ;
			default :
				// check for overrun
				if((dta & 0x3000000) != 0x2000000) {
					LOG(ERR,"RDO %d: T %d: BUSY overrun: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
				}
				continue ;
			}

			// we took out all the FIFO l0 contribs here!

			if(rhic >= collision) {
				trgs[cou].rhic_delta = rhic - collision ;
			}
			else {
				trgs[cou].rhic_delta = -(collision - rhic) ;
			}

			if(trgs[cou].t == 0) {
				LOG(ERR,"RDO %d: token 0 in L2 contribution 0x%08X -- skipping",rdo.rdo,dta) ;
				continue ;
			}

			cou++ ;
		}

	}

#if 0
// older NIOS code...
	if((trgs[0].t<4096) && (rdo.trg_cou == 1) && (tcd_in_use == 0)) {	// no TCD, badly emulated
		trgs[cou].t = trgs[0].t ;
		trgs[cou].daq = 0 ;
		trgs[cou].trg = 15 ;	// accept!
		trgs[cou].rhic = (trgs[0].rhic) + 1 ;
		trgs[cou].rhic_delta = 1 ;


		cou++ ;
	}
#endif


	return cou ;
}
