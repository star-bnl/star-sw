
#include <RTS_READER/daq_det.h>

#include <SFS/sfs_index.h>

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
		if(legacy==0) {
			while((e = caller->sfs->readdir(dir))) {
				//LOG(TERR,"%s:%s:full %s: dir %s...",name,dir->full_name,e->full_name,e->d_name) ;
				if(strncasecmp(e->d_name,name,strlen(name))==0) {
					present = 1 ;
					break ;
				}
			}
		}
		else {
			LOG(ERR,"%s: is legacy and I have not coded this yet!",name) ;
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


int daq_det::get_token(char *buff, int buff_bytes)
{
	LOG(ERR,"%s: get_token() not written!",name) ;
	return -1 ;
}


int daq_det::get_l2(char *buff, int buff_bytes, daq_trg_word *trg, int prompt) 
{

	LOG(ERR,"%s: get_l2() not written!",name) ;
	return 0 ;
}


daq_dta  *daq_det::get(const char *bank,int c1, int c2, int c3, void *p1, void *p2) 
{
	LOG(ERR,"%s: get() not written!",name) ;

	return 0 ;
}
	
