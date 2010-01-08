#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_hlt.h"


const char *daq_hlt::help_string = "HLT\n\
tpx	returns TPX SL3;\n\
trg	returns TRG SL3;\n" ;

class daq_det_hlt_factory : public daq_det_factory
{
public:
	daq_det_hlt_factory() {
		daq_det_factory::pseudo_factories[L3_ID] = this ;
	}

	daq_det *create() {
		return new daq_hlt ;
	}
} ;

static daq_det_hlt_factory hlt_factory ;


daq_hlt::daq_hlt(daqReader *rts_caller) 
{
	rts_id = -L3_ID ;	// note negative number!!!

	name = "hlt" ;
	sfs_name = "hlt" ;
	caller = rts_caller ;
	
	if(caller) {
		caller->insert(this, rts_id) ;
	}

	tpx = new daq_dta ;
	tof = new daq_dta ;
	trg = new daq_dta ;
	gl3 = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_hlt::~daq_hlt() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete tpx ;
	delete tof ;
	delete trg ;
	delete gl3 ;

	return ;
}



daq_dta *daq_hlt::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;
	if(present == 0) return 0 ;


	if(strcasecmp(bank,"tpx")==0) {
		return handle_tpx(sec) ;
	}
	else if(strcasecmp(bank,"trg")==0) {
		return handle_trg() ;
	}
	else if(strcasecmp(bank,"tof")==0) {
		return handle_tof() ;
	}
	else if(strncasecmp(bank,"gl3",3)==0) {
		return handle_gl3(sec, bank) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_hlt::handle_tpx(int sec)
{
	char *st ;
	int s_start, s_stop ;
	int bytes ;

	assert(caller) ;	// sanity...

	if(!present) {
		LOG(ERR,"%s: not present?",name) ;
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}

	if(sec<=0) {
		s_start = 1 ;
		s_stop = 24 ;
	}
	else {
		s_start = s_stop = sec ;
	}


	for(int s=s_start;s<=s_stop;s++) {
		char str[256] ;
		char *full_name ;

		sprintf(str,"%s/tpx/sec%02d/sl3",sfs_name, sec) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) return 0 ;
		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

		tpx->create(bytes,"hlt_tpx",rts_id,DAQ_DTA_STRUCT(char)) ;
		st = (char *) tpx->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

		tpx->finalize(bytes,s,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	}
	

	tpx->rewind() ;

	return tpx ;
	
}

daq_dta *daq_hlt::handle_trg()
{
	char *st ;
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

	sprintf(str,"%s/trg/sec%02d/sl3",sfs_name, 1) ;
	full_name = caller->get_sfs_name(str) ;
		
	if(!full_name) return 0 ;
	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	trg->create(bytes,"hlt_trg",rts_id,DAQ_DTA_STRUCT(char)) ;
	st = (char *) trg->request(bytes) ;
		
	int ret = caller->sfs->read(str, st, bytes) ;
	if(ret != bytes) {
		LOG(ERR,"ret is %d") ;
	}

	trg->finalize(bytes,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	
	trg->rewind() ;

	return trg ;
	
}

daq_dta *daq_hlt::handle_tof()
{
	char *st ;
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

	sprintf(str,"%s/tof/sec%02d/sl3",sfs_name, 1) ;
	full_name = caller->get_sfs_name(str) ;
		
	if(!full_name) return 0 ;
	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	tof->create(bytes,"hlt_tof",rts_id,DAQ_DTA_STRUCT(char)) ;
	st = (char *) tof->request(bytes) ;
		
	int ret = caller->sfs->read(str, st, bytes) ;
	if(ret != bytes) {
		LOG(ERR,"ret is %d") ;
	}

	tof->finalize(bytes,1,1,0) ;	// sector 1; rdo 1; pad irrelevant...
	
	tof->rewind() ;

	return tof ;
	
}

daq_dta *daq_hlt::handle_gl3(int sec, const char *bank)
{
	char *st ;
	int bytes ;
	int cou = 0 ;

	assert(caller) ;	// sanity...

	if(!present) {
		LOG(ERR,"%s: not present?",name) ;
		return 0 ;
	}
	else {
		LOG(DBG,"%s: sec %d, bank %s: present %d",name,sec,bank,present) ;
	}

	char str[256] ;
	char *full_name ;

	sprintf(str,"%s/gl3",sfs_name) ;
	full_name = caller->get_sfs_name(str) ;

	LOG(DBG,"Trying 1: %s: got %s",str,full_name) ;

	if(!full_name) {	// alternate, old form
		sprintf(str,"%s/gl3/sec%02d",sfs_name, 1) ;
		full_name = caller->get_sfs_name(str) ;

		LOG(DBG,"Trying 2: %s: got %s",str,full_name) ;
		if(!full_name) return 0 ;
	}


	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	gl3->create(1024,"hlt_gl3",rts_id,DAQ_DTA_STRUCT(char)) ;

	fs_dir *dir = caller->sfs->opendir(full_name) ;
	if(dir==0) return 0 ;

	fs_dirent *entry ;
	while((entry = caller->sfs->readdir(dir))) {

		bytes = caller->sfs->fileSize(entry->full_name) ;	// this is bytes
		if(bytes == 0) {
			LOG(WARN,"%s: 0 bytes?",entry->full_name) ;
			continue ;
		}

		cou++ ;

		LOG(DBG,"%d: %s %s %d",cou,entry->full_name,entry->d_name,bytes) ;

		st = (char *) gl3->request(bytes+sizeof(hlt_gl3_t)) ;
	
		((hlt_gl3_t *)st)->buff = st + sizeof(hlt_gl3_t) ;
		((hlt_gl3_t *)st)->bytes = bytes ;
		char *yada = ((hlt_gl3_t *)st)->name ;
		strncpy(yada,entry->d_name,32) ;


		int ret = caller->sfs->read(entry->full_name, st+sizeof(hlt_gl3_t), bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

		gl3->finalize(bytes+sizeof(hlt_gl3_t),cou,1,0) ;	// sector 1; rdo 1; pad irrelevant...

	}

	gl3->rewind() ;

	return gl3 ;
	
}


	
