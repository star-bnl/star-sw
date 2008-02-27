#include <sys/types.h>
#include <errno.h>



#include "daq_tpx.h"
#include <RTS_READER/daq_dta.h>

#include <rts.h>
#include <rtsLog.h>		// DAQ logging
#include <rtsSystems.h>

#include <SFS/sfs_index.h>

#include <DAQ_TPX/tpxCore.h>
#include <DAQ_TPX/tpxGain.h>
#include <DAQ_TPX/tpxPed.h>
#include <DAQ_TPX/tpxFCF.h>
#include <DAQ_TPX/tpxStat.h>

#include <DAQ1000/ddl_struct.h>

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



daq_tpx::daq_tpx(const char *dname, rts_reader *rts_caller) 
{
	rts_id = TPX_ID ;
	name = rts2name(rts_id) ;


	raw = new daq_dta ;
	adc = new daq_dta ;
	cld_raw = new daq_dta ;
	cld = new daq_dta ;
	
	caller = rts_caller ;
	
	LOG(DBG,"%s: constructor: caller %p, detHandler %p",name, rts_caller) ;
	return ;
}

daq_tpx::~daq_tpx() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete cld_raw ;
	delete cld ;



	return ;
}


/*
	For online: in_buffer, in_bytes, out_buffer, out_bytes MUST be set!
*/
int daq_tpx::Make()
{
	int pres ;
	int mode ;


	mode = GetMode() ;

	pres = presence() ;
	if(pres) {
		evt_num++ ;
		LOG(NOTE,"%s: Make(): presence %d, evt %d: m_Mode 0x%08X, event_mode 0x%08X",name,pres,evt_num,mode,event_mode) ;
	}
	else {
		LOG(DBG,"%s: Make(): not found in this event...",name) ;
		return 0 ;
	}

#if 0
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

#endif
	return 0 ;
}

int daq_tpx::InitRun(int run)
{
	u_int mode = GetMode() ;

	LOG(NOTE,"%s: InitRun(%d), m_Mode 0x%08X",name, run, mode) ;


	run_num = run ;
	evt_num = 0 ;
#if 0
	// when working in Online
	if(mode & m_Mode_DAQ_RT) {
		// let's not screw around let's just allocate what we need now
		if(Ped == 0) {
			Ped = new algo_tpx_ped ;
		}

		if(Gain == 0) {
			Gain = new algo_tpx_gain ;
		}
		// I always need gains because of bad FEEs
		Gain->from_file(def_sector, def_rdo) ;
		Gain->to_altro(bad_fee) ;	// fill in that structure!

		if(Physics == 0) {
			Physics = new algo_tpx_physics ;
		}

		if(Fcf == 0) {
			Fcf = new algo_tpx_fcf ;
		}
	}
	else {	// offline
		if(mode == 0) {	// Offline did not change the mode, by default they expect FCF!
			mode |= m_Mode_DAQ_FCF ;
			SetMode(mode) ;
		}
	}

	// always!
	if(Stat == 0) {
		Stat = new algo_tpx_stat ;
	}
	Stat->InitRun(run) ;

	
	if(mode & m_Mode_DAQ_PED) {
		if(Ped==0) {
			Ped = new algo_tpx_ped ;
		}

		Ped->InitRun(run) ;
	}
	else if(mode & m_Mode_DAQ_GAIN) {
		if(Gain==0) {
			Gain = new algo_tpx_gain ;
		}
	
		Gain->InitRun(run) ;
	}
	else {
		if(mode & m_Mode_DAQ_FCF_RAW) {
			if(Fcf==0) {
				Fcf = new algo_tpx_fcf ;
			}
			
			if(mode & m_Mode_DAQ_RT) {	// I have gains already
				;
			}
			else {
				if(Gain == 0) Gain = new algo_tpx_gain ;

				Gain->from_db(def_sector, def_rdo) ;	
			}

			Fcf->InitRun(run) ;
			Fcf->gain(Gain) ;
		}

	}
#endif
	return 0 ;
}

int daq_tpx::FinishRun(int old)
{
	u_int mode = GetMode() ;

	LOG(NOTE,"%s: Run %d finished with %d events, m_Mode 0x%08X",name,run_num,evt_num,mode) ;
#if 0
	if(mode & m_Mode_DAQ_PED) {
		LOG(NOTE,"Calculating peds") ;

		Ped->FinishRun(old) ;

		if(mode & m_Mode_DAQ_RT) {
			Ped->to_file() ;
			Ped->to_sfs() ;	// for evb!

			Ped->_to_altro() ;
		}
		else {	
			Ped->to_store(ped) ;

			delete Ped ;
			Ped = 0 ;
		}

	}
	else if(mode & m_Mode_DAQ_GAIN) {
		
		LOG(NOTE,"Calculating gains") ;

		Gain->FinishRun(old) ;

		if(mode & m_Mode_DAQ_RT) {
			Gain->to_file() ;
			Gain->to_sfs() ;

			Gain->_to_altro() ;
		}
		else {
			Gain->to_store(gain) ;
			delete Gain ;
			Gain = 0 ;
		}
	}
	else if(mode & m_Mode_FCF_RAW) {

		Fcf->FinishRun(old) ;

	}

	Statistics->FinishRun(old) ;

	if(mode & m_Mode_DAQ_RT) {
		Statistics->to_file() ;
		Statistics->to_sfs() ;
	}
	else {
		Statistics->to_store(statistics) ;
	}

#endif 
	return 0 ;
}


int daq_tpx::get_token(char *buff, int buff_bytes)
{
        struct tpx_rdo_event rdo ;

        tpx_get_start(buff, buff_bytes/4, &rdo, 1) ;

        if(rdo.token==0) return -ENOSYS ;       // error for TPX


        return rdo.token ;
}

int daq_tpx::get_l2(char *buff, int buff_bytes, daq_trg_word *trgs, int prompt)
{
	struct tpx_rdo_event rdo ;
	int cou = 0 ;
	int words = buff_bytes / 4 ;


	tpx_get_start(buff, words, &rdo, 0) ;

	LOG(DBG,"trg cou %d",rdo.trg_cou) ;

	for(u_int i=0;i<rdo.trg_cou;i++) {
		u_int dta = rdo.trg[i].data ;
		u_int marker = rdo.trg[i].csr >> 16 ;
		trgs->clock = rdo.trg[i].rhic_counter ;
		trgs->misc = rdo.trg[i].csr ;

		LOG(DBG,"%d: 0x%08X 0x%08X",i,dta,rdo.trg[i].csr) ;

		if(prompt) {
			if((marker==0)||(marker==0xEEEE)) {	// prompt: 0 is real, 0xEEEE is self triggered
				trgs->t = dta & 0xFFF ;
				trgs->daq = (dta >> 12) & 0xF ;
				trgs->trg = (dta >> 16) & 0xF ;
				return 1 ;	// only 1, by definition
			}

		}
		else {
			if(marker==0xFFFF) {	// FIFO
				trgs->t = dta & 0xFFF ;
				trgs->daq = (dta >> 12) & 0xF ;
				trgs->trg = (dta >> 16) & 0xF ;
				trgs++ ;
				cou++ ;
			}
		}
	}

	
	return cou ;

}


daq_dta *daq_tpx::get(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
{
	const char *bank ;

	if(!presence()) return 0 ;	// this det is not in this event...

	if(in_bank==0) {	// just wants to know if I'm here so return some read-only non-NULL memory
		bank = "cld" ;	// default		
	}
	else {
		bank = in_bank ;
	}

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

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
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}


	return 0 ;
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

		LOG(DBG,"Calling handle_raw for %d:%d",s,r) ;		
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
	

		do {
			data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;		

			if(a.count == 0) continue ;	// no data for this guy...


			daq_store *st = adc->get() ;
		
			//LOG(DBG,"%d: %d:%d %d",adc->obj_cou,a.row,a.pad,a.count) ;

			st->sec = s ;
			st->row = a.row ;
			st->pad = a.pad ;
			st->nitems = a.count ;

			struct daq_adc_tb *at = (struct daq_adc_tb *)(st + 1) ;	// move to storage
			for(u_int i=0 ; i < st->nitems; i++) {
				at[i].adc = a.adc[i] ;
				at[i].tb = a.tb[i] ;

			}

			adc->commit() ;


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


		sprintf(str,"%s/%s/sec%02d/rb%02d/adc",caller->fs_cur_evt, "tpx", s, r) ;
	
		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		int size = caller->sfs->fileSize(str) ;	// this is bytes

		LOG(DBG,"Got %d",size) ;

		if(size <= 0) {
			if(size < 0) {
				LOG(WARN,"%s: %s: not found in this event",name,str) ;
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
		
		sprintf(str,"%s/%s/sec%02d/rb%02d/adc",caller->fs_cur_evt, "tpx", 
			obj[i].sec, obj[i].rb) ;

		struct daq_store *st = raw->get() ;

		st->sec = obj[i].sec ;
		st->row = obj[i].rb ;
		st->nitems = obj[i].bytes ;

		char *mem = (char *)(st + 1) ;
	
		int ret = caller->sfs->read(str, mem, st->nitems) ;

		if(ret != (int)st->nitems) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				st->nitems,ret,strerror(errno)) ;
		}
		else {
			LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
			raw->commit() ;
		}
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


		sprintf(str,"%s/%s/sec%02d/cld%02d",caller->fs_cur_evt, "tpx", s, r) ;
	
		LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;

		int size = caller->sfs->fileSize(str) ;	// this is bytes

		LOG(DBG,"Got %d",size) ;

		if(size <= 0) {
			if(size < 0) {
				LOG(WARN,"%s: %s: not found in this event",name,str) ;
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
		
		sprintf(str,"%s/%s/sec%02d/cld%02d",caller->fs_cur_evt, "tpx", 
			obj[i].sec, obj[i].rb) ;

		struct daq_store *st = cld_raw->get() ;

		st->sec = obj[i].sec ;
		st->row = obj[i].rb ;
		st->nitems = obj[i].bytes ;

		char *mem = (char *)(st + 1) ;
	
		int ret = caller->sfs->read(str, mem, st->nitems) ;

		if(ret != (int)st->nitems) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				st->nitems,ret,strerror(errno)) ;
		}
		else {
			LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
			cld_raw->commit() ;
		}
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
			int row = *p_buff++ ;
			int cou = *p_buff++ ;

			while(cou) {
				daq_store *st = cld->get() ;
				
				st->sec = s ;
				st->row = row ;
				st->nitems = 1 ;	// just one

				daq_cld *dc = (daq_cld *)(st+1) ;

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

		}



	}
	}


	cld->rewind() ;	// wind data pointers to the beginning so that they can be used
	

	return cld ;

}


	


#if 0
/* create clusters from raw data... */
daq_dta *daq_tpx::handle_cld_c(char *bank, int sec, int rdo, char *force_store, int max_bytes, char *rdo_buff, int rdo_bytes)
{
	return 0 ;

	int o_cou ;
	struct {
		u_char sec ;
		u_char rdo ;
		char *ptr ;
		u_int bytes ;
	} obj[24*6] ;

	o_cou = 0 ;

	if(rdo_buff == 0) {	// from SFS!
		int min_sec, max_sec ;
		int min_rdo, max_rdo ;

		max_bytes = 0 ;

		for(int s=min_sec;s<max_sec;s++) {
		for(int r=min_rdo;r<max_rdo;r++) {

			rdo_dta = handle_raw(s,r) ;

			if(rdo_dta && rdo_dta->iterate()) {
				obj[o_cou].sec = s ;
				obj[o_cou].rdo = r ;
				obj[o_cou].ptr = rdo_dta->Byte ;
				obj[o_cou].bytes = rdo_dta->ncontent ;

				o_cou++ ;

				max_bytes += rdo_dta->ncontent ;

			}

		}
		}

	}
	else {
		obj[0].ptr = rdo_buff ;
		obj[0].bytes = rdo_bytes ;
		obj[0].sec = sec ;
		obj[0].rdo = rdo ;

		obj_cou = 1 ;
	}

	if(strncasecmp(bank,"adc")) {
		dd = adc ;
		dd->algo = do_adc ;
	}
	else if(strncasecmp(bank,"cld_raw")) {
		dd = cld_raw ;
		dd->algo = do_fcf ;
	}

	// run raw clusterfinder and outout to force_store with max size max_bytes...
	cld_raw->create(max_bytes,(char *)name,rts_id,DAQ_DTA_STRUCT(cld_raw), force_store) ;

	for(int i=0; i< obj_cou;i++) {
		do_fcf(dd, obj[i].sec, obj[i].rdo, obj[i].in_buff, obj[i].in_bytes) ;	
	}

	
	cld_raw->rewind() ;

	return cld_raw ;

}
#endif
