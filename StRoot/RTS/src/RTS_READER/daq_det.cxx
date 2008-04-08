#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>	// for htonl

#include <rtsLog.h>

#include <SFS/sfs_index.h>
#include <RTS_READER/rts_reader.h>
#include <RTS_READER/daq_det.h>
#include <RTS_READER/daq_dta.h>


int daq_det::endianess = 1 ;



daq_det::daq_det(const char *dname, rts_reader *rts_caller) {

	name = dname ;
	rts_id = -1 ;			// this should be correctly overriden in the member

	memset(mydet,0,sizeof(mydet)) ;

	SetMode(0) ;

	file_ix = 0 ;
	evt_ix = 0 ;


	caller = rts_caller ;

	multi_mask = 0 ;

	present = 0 ;
	legacy = 0 ;	// assume not legacy

	run_num = -1 ;	// unknown at startup...
	evt_num = 0 ;
		
	def_sector = def_rdo = -1 ;	// assume ALL

	in_buffer = 0 ;
	out_buffer = 0 ;
	in_bytes = 0 ;
	out_bytes = 0 ;

	if(htonl(0x12345678) != 0x12345678) {
		endianess = 0 ;	// little ;
	}
	else {
		endianess = 1 ;	// big
	}


	LOG(DBG,"daq_det: %s [%d], caller %p: endianess %c",name,rts_id,caller,endianess?'B':'L') ;

	return ;
}



daq_det::~daq_det() 
{
	LOG(DBG,"~daq_det: %s [%d]",name,rts_id) ;

	if(rts_id == -123) {	// I'm the special dispatcher!
		for(int i=0;i<32;i++) {
			if(mydet[i]) {
				LOG(DBG,"dispatched: deleting %d: %s",i,mydet[i]->name) ;
				delete mydet[i] ;
			}
			mydet[i] = 0 ;
		}
	}

	return ;
}

int daq_det::Make() 
{
	LOG(DBG,"Make: %s [%d]: evt_num %d",name,rts_id,evt_num) ;

	evt_num++ ;

	if(rts_id == -123) {
		for(int i=0;i<32;i++) {
			if(multi_mask & (1<<i)) {
				assert(mydet[i]) ;
				mydet[i]->Make() ;
			}
		}
	}

	return 0 ;
}

int daq_det::Init() 
{
	LOG(DBG,"Init: %s [%d]",name,rts_id) ;

	if(rts_id == -123) {
		for(int i=0;i<32;i++) {
			if(multi_mask & (1<<i)) {
				assert(mydet[i]) ;
				mydet[i]->Init() ;
			}
		}
		multi_mask = 0 ;
	}
	return 0 ;
}

int daq_det::InitRun(int run_number) 
{
	run_num = run_number ;
	evt_num = 0 ;

	LOG(DBG,"InitRun: %s [%d]: run %09u",name,rts_id,run_number) ;

	if(rts_id == -123) {
		for(int i=0;i<32;i++) {
			if(multi_mask & (1<<i)) {
				assert(mydet[i]) ;
				mydet[i]->InitRun(run_number) ;
			}
		}
		multi_mask = 0 ;
	}
	return 0 ;
}

int daq_det::FinishRun(int old_run_number) 
{

	LOG(DBG,"FinishRun: %s [%d]: run %09u",name,rts_id,old_run_number) ;

	if(rts_id == -123) {
		for(int i=0;i<32;i++) {
			if(multi_mask & (1<<i)) {
				assert(mydet[i]) ;
				mydet[i]->FinishRun(old_run_number) ;
			}
		}

		multi_mask = 0 ;
	}
	return 0 ;
}

void daq_det::help() const 
{	
	if(rts_id == -123) {
		for(int i=0;i<32;i++) {
			if(multi_mask & (1<<i)) {
				assert(mydet[i]) ;
				mydet[i]->help() ;
			}
		}
	}
	else {
		printf("%s\nNo help written for %s\n",GetCVS(),name) ;
	}
}


int daq_det::presence() 
{ 

	if(caller==0) {
		present = 1 ;
		return present ;	// no RTS; we are in ESB!
	}

	// efficiency: check if we were already called for this file_ix and event_ix
	if((file_ix != caller->cur_file_ix) || (evt_ix != caller->cur_evt_ix)) {
		fs_dir *dir ;
		fs_dirent *e ;

		present = 0 ;	// assume not there...

		// first check if we are present in this event at all...
		char *n = caller->fs_cur_evt ;

		if(caller->sfs == 0) return present ;	// not initialized yet...


		dir = caller->sfs->opendir(n) ;	// this is the event directory

		
		if(dir == 0) return present ;	// still not initialized yet


		// traverse it finding "this_det" 
		while((e = caller->sfs->readdir(dir))) {
			//LOG(TERR,"%s:%s:full %s: dir %s...",name,dir->full_name,e->full_name,e->d_name) ;
			if(strncasecmp(e->d_name,name,strlen(name))==0) {
				present = 1 ;
				break ;
			}

		}

		// must remember to closedir!
		caller->sfs->closedir(dir) ;

		file_ix = caller->cur_file_ix ;
		evt_ix = caller->cur_evt_ix ;
	
		LOG(DBG,"%s: %s: new check, found me %d?",name,n,present) ;
	}

	LOG(DBG,"%s: already checked: found %d, m_mode %d",name,present,m_Mode) ;
	
	return present ;
} ;



int daq_det::get_token(char *addr, int words)
{
	daq_trg_word trgs[128] ;

	int ret = get_l2(addr, words, trgs, 1) ;
	if(ret == 0) {
		LOG(ERR,"No triggers?") ;
		return -1000 ;
	}
	

	if(trgs[0].t==0) {
		LOG(ERR,"Token 0 not allowed but I will try to use the other triggers...") ;
		trgs[0].t = 4097 ;
	}


	return trgs[0].t ;

}


int daq_det::get_l2(char *buff, int buff_bytes, daq_trg_word *trg, int prompt) 
{

	LOG(ERR,"%s: get_l2() not written!",name) ;
	return 0 ;
}


daq_dta  *daq_det::get(const char *bank,int c1, int c2, int c3, void *p1, void *p2) 
{
	LOG(NOTE,"%s: get() not written!",name) ;

	return 0 ;
}
	

daq_dta  *daq_det::put(const char *bank,int c1, int c2, int c3, void *p1, void *p2) 
{
	LOG(ERR,"%s: put() not written!",name) ;

	return 0 ;
}
	

