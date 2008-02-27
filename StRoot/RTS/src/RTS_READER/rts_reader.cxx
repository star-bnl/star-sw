#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include "rts_reader.h"


// all the dets we know about
#include <SFS/sfs_index.h>

#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TOF/daq_tof.h>



#define RTS_MASK_ALL	0xFFFFFFFF
#define RTS_MASK(id)  (1<<(id))

static u_int parse_det_string(const char *list) ;
static u_int is_physics(const char *name) ;
static int rts_non_physics(const char *name) ;

// constructor
rts_reader::rts_reader(const char *name)
{
	iname = name ;	// instance name...
	LOG(NOTE,"%s: constructor",iname) ;

	l_errno = 0 ;

	cur_run_number = 0 ;

	cur_file_ix = 0 ;
	cur_desc = -1 ;
	memset(select_files,0,sizeof(select_files)) ;
	num_files = 0 ;

	cur_event_in_file = 0 ;
	cur_event = 0 ;
	
	sfs = 0 ;		// no sfs yet...
	sfs_owner = 0 ;		// did I do new?

	dets_enabled[0] = 0 ;	// make empty string
	rts_dets_enabled = 0 ;

	fs_root_dir = NULL;   

	dispatcher = new daq_det("dispatcher",this) ;
	assert(dispatcher) ;
	dispatcher->rts_id = -123 ;	// SPECIAL ID for dispatcher! normally either -1 if unassigned or positive if a valid det...


	return ;
}

rts_reader::~rts_reader() 
{

	LOG(NOTE,"%s: destructor",iname) ;


	if(dispatcher) {
		delete dispatcher ;
	}

	if(sfs) {
		if(fs_root_dir) sfs->closedir(fs_root_dir) ;
		if(sfs_owner) delete sfs ;
	}

//	if(datap) delete datap ;

	dispatcher = 0 ;
	sfs = 0 ;
	fs_root_dir = 0 ;

	return ;
}

int rts_reader::Init()
{
	assert(dispatcher) ;
	dispatcher->multi_mask = rts_dets_enabled ;
	return dispatcher->Init() ;
} ;

int rts_reader::InitRun(int run_num)
{	
	LOG(NOTE,"%s: InitRun(%d)",iname,run_num) ;

	assert(dispatcher) ;
	dispatcher->multi_mask = rts_dets_enabled ;
	return dispatcher->InitRun(run_num) ;
}

int rts_reader::FinishRun(int old)
{
	LOG(NOTE,"%s: FinishRun(%d)",iname,old) ;

	assert(dispatcher) ;
	dispatcher->multi_mask = rts_dets_enabled ;
	return dispatcher->FinishRun(old) ;
}


int rts_reader::handle_open()
{
	// normally, we have an open descriptor and we just return ASAP
	if(sfs && fs_root_dir) return 1 ;	// nothing to do...

	if(sfs==0) {	// I have no object, need one
		LOG(DBG,"No sfs object -- creating one...") ;
		sfs_owner = 1 ;
		sfs = new sfs_index ;
		assert(sfs) ;

		fs_root_dir = 0 ;	// no other option!
	}

	// sfs exists from here on...

	if(fs_root_dir) {	// exists from previous call, zap it
		sfs->closedir(fs_root_dir) ;
		fs_root_dir = 0 ;
	}

	fs_cur_evt[0] = 0 ;
	cur_evt_ix = 0 ;
	cur_desc = -1 ;
	cur_event_in_file = 0 ;
	
	// open the SFS file if owner
	if(sfs_owner) {
		if(cur_file_ix < num_files) {	// more files to open
			errno = 0 ;	// this is the system errno...
			cur_desc = sfs->mount((char *)select_files[cur_file_ix], O_RDONLY, 0666) ;
			if(cur_desc < 0) {
				if(errno==0) errno = EIO ;
				LOG(ERR,"%s: file \"%s\": sfs mount error [%s]",iname,select_files[cur_file_ix],strerror(errno)) ;
				return -1 ;
			}
			LOG(NOTE,"%s: opened file \"%s\" OK",iname,select_files[cur_file_ix]) ;
		}
		else {	// end of filename list
			return 0 ;
		}
	}
	else {
		LOG(NOTE,"Running with an external SFS object...") ;
	}

	// find the root directory
	errno = 0 ;
	fs_root_dir = sfs->opendir("/") ;
	if(fs_root_dir == 0) {
		if(errno == 0) errno = EIO ;

		LOG(ERR,"%s: opendir \"/\" failed [%s]",iname,strerror(errno))  ;

		if(cur_desc >= 0) sfs->umount() ;
		cur_desc = -1 ;

		return -1 ;
	}
	else {
		LOG(DBG,"opendir OK...") ;
	}

	return 1 ;	// all OK...
}

void rts_reader::handle_close()
{
	assert(sfs) ;

	if(fs_root_dir) {
		LOG(DBG,"Closing fs_root_dir") ;
		sfs->closedir(fs_root_dir) ;	
	}

	fs_root_dir = 0 ;

	if(sfs_owner) {
		assert(sfs) ;

		LOG(NOTE,"%s: closing input %s",iname,select_files[cur_file_ix]) ;


		if(cur_desc >= 0) sfs->umount() ;
		cur_desc = -1 ;

		delete sfs ;	
		sfs = 0 ;
	}



	fs_root_dir = 0 ;
	cur_evt_ix = 0 ;
	fs_cur_evt[0] = 0 ;
	cur_event_in_file = 0 ;

	return ;
}

int rts_reader::get_event()
{
	fs_dirent *entry ;
	
	// position at next entry in the root
	next_entry: ;

	// Argh: readdir always returns an internal pointer to a static member
	// so I need to copy it!!!!

	assert(sfs) ;

	assert(fs_root_dir) ;

	entry = sfs->readdir(fs_root_dir) ;
	if(entry == 0) {
		LOG(DBG,"No more entries in SFS...") ;
		return EOF ;	// done!
	}

	// copy over .readdirs static member!
	strcpy(fs_cur_evt, entry->full_name) ;

	// shorthand
	char *n = entry->full_name ;
	int bytes = entry->sz ;

	
	/*
		simple sanity checks:
		1) must have size 0 i.e. be a directory
		2) name must start with "/"
		3) dir name must be "/"
	*/
	if(bytes != 0) {
		LOG(ERR,"%s:%s:%s: not a directory, bytes == %d",iname,select_files[cur_file_ix],n,bytes) ;
	}
	if(*n != '/') {
		LOG(ERR,"%s:%s:%s: no slash as first char?",iname,select_files[cur_file_ix],n) ;
	}

	u_int ix = is_physics(n) ;
	if(ix==0) {
		int id = rts_non_physics(n) ;
		LOG(NOTE,"%s:%s:%s: not a physics event, belongs to %d[%s]...",iname,select_files[cur_file_ix],n,id,rts2name(id)) ;
		goto next_entry ;
	}

	cur_evt_ix = ix ;
	cur_event_in_file++ ;
	cur_event++ ;

	LOG(NOTE,"%s:%s:%s: physics event, cou %d in file, %d so far...",iname,select_files[cur_file_ix],n,cur_event_in_file,cur_event) ;

	return cur_event_in_file ;
}


/*
	Make() _ONLY_ handles calls from files!
*/
int rts_reader::Make()
{
	int ret ;

	LOG(DBG,"%s: Make() called", iname) ;


	reopen_new: ;


	ret = handle_open() ;

	if(ret < 0) {	// some error 
		LOG(ERR,"%s: file \"%s\": open error [%s]",iname,select_files[cur_file_ix],strerror(errno)) ;
		cur_file_ix++ ;
		goto reopen_new ;
	}
	else if(ret==0) {
		LOG(NOTE,"%s: Make: no more files and events -- done!",iname) ;
		return EOF ;
	}
	
	// at this point a valid sfs pointer exists and
	// the fs_root_dir is also valid!

	ret = get_event() ;

	if(ret < 0) {		// end of file....
		LOG(DBG,"get event returns %d",ret) ;
		handle_close() ;
		// that's it if we are running via a sfs object instead of a file...
		if(!sfs_owner) {
			LOG(NOTE,"No more events in the sfs object -- done") ;
			return EOF ;
		}

		cur_file_ix++ ;
		goto reopen_new ;
	}
	// release all of the previous data before we return with good news
	//det("*")->release() ;

	assert(dispatcher) ;
	dispatcher->multi_mask = rts_dets_enabled ;
	dispatcher->Make() ;	// call the dets Make()!

	LOG(DBG,"%s: Make: cur_file \"%s\" [%d], file evt %d, tot event %d",iname,
		select_files[cur_file_ix],cur_file_ix,cur_event_in_file,cur_event) ;

	return 1 ;
}



/*
	We call all the constuctors here!
*/
int rts_reader::enable(u_int rts_mask)
{

	if(rts_dets_enabled) {
		LOG(ERR,"%s: sorry -- can't enable twice!",iname) ;
		return -1 ;
	}



	// we go through all 32 bits and make SURE
	// we asigned something to the mydet
	
	for(int rts_id=0;rts_id<32;rts_id++) {
		char *name = rts2name(rts_id) ;
		if(name == 0) {
			name = "UNKNOWN" ;
		}

		if(rts_mask & (1<<rts_id)) {
			// need to construct a det here
			switch(rts_id) {
			case TPX_ID :
				rts_dets_enabled |= (1<<rts_id) ;
				dispatcher->mydet[rts_id] = new daq_tpx(name,this) ;
				break ;
			case TOF_ID :
				rts_dets_enabled |= (1<<rts_id) ;
				dispatcher->mydet[rts_id] = new daq_tof(name,this) ;
				break ;

			default :
				if(rts_mask != 0xFFFFFFFF) LOG(ERR,"%s: %s[%d] has not been coded at this time!",iname,name,rts_id) ;
				dispatcher->mydet[rts_id] = new daq_det(name,this) ;
				break ;
			}
		}
		else {
			dispatcher->mydet[rts_id] = new daq_det(name, this) ;
		}

		assert(dispatcher->mydet[rts_id]) ;
	}
	
	// create the string representation as well...
	for(int i=0;i<32;i++) {
		if(rts_dets_enabled & (1<<i)) {
			strcat(dets_enabled,dispatcher->mydet[i]->name) ;
			strcat(dets_enabled," ") ;
		}
	}


	LOG(NOTE,"%s: enabled %s",iname,dets_enabled) ;

	
	return 1 ;
} ;

int rts_reader::enable(const char *which)
{
	assert(which) ;

	u_int mask = parse_det_string(which) ;

	return enable(mask) ;
}

int rts_reader::add_input(class sfs_index *use_sfs)
{
	if(sfs) {	// I already own sfs?
		LOG(ERR,"SFS already made???") ;
		if(sfs_owner) delete sfs ;
	}

	sfs = use_sfs ;
	sfs_owner = 0 ;	// I'm not the owner so I can't delete!

	return 0 ;
}


int rts_reader::add_input(const char *fname)
{
	u_int max_files ;

	assert(fname) ;

	max_files = sizeof(select_files)/sizeof(char *) ;

	if(num_files >= max_files) {
		LOG(ERR,"%s: at fname \"%s\": too many files %d --> max is %d",iname,fname,num_files,max_files) ;
		return -1 ;
	}

	select_files[num_files] = fname ;
	num_files++ ;

	return 0 ;

}

/*
	If called with a string argument, first turn it into
	the canonical bitfield representation
*/
daq_det *rts_reader::det(const char *which)
{
	assert(which) ;

	u_int mask = parse_det_string(which) ;

	return det(mask) ;
}

daq_det *rts_reader::det(u_int rts_mask)
{
	assert(dispatcher) ;

	rts_mask &= rts_dets_enabled ;	// make sure we don't use any other det...

	// count bits in the mask...
	int cou = 0 ;
	int last_id = 0 ;
	for(int i=0;i<32;i++) {
		if(rts_mask & (1<<i)) {
			cou++ ;
			last_id = i ;
		}
	}

	if(cou==1) {	// special case of only 1 DET -- dispatch directly to save on time!
		LOG(DBG,"%s: calling single det %s",iname,rts2name(last_id)) ;
		return dispatcher->mydet[last_id] ;

	}
	
	// in case we request more than one det we'll return the pointer
	// to dispatcher
	dispatcher->multi_mask = rts_mask ;
	return dispatcher ;
}

/*
	parse the string of the form i.e. "tpc ssd tpx" and
	return a bitlist of RTS detectors
*/
static u_int parse_det_string(const char *list)
{
	u_int mask = 0 ;

	//LOG(DBG,"Parsing \"%s\"",list) ;

	if(strcmp(list,"*")==0) return RTS_MASK_ALL ;

	reparse:;

	for(int i=0;i<32;i++) {
		char *name = rts2name(i) ;

		if(name==0) continue ;

		//LOG(DBG,"Checking id %d: %s",i,name) ;

		if(strncasecmp(list,name,strlen(name))==0) {
			//LOG(DBG,"********* Got %d: %s",i,name) ;
			mask |= (1<<i) ;
			break ;
		}
	}
	
	// move to either end or to the next space
	int got_space = 0 ;
	while(*list != 0) {
		if(*list == ' ') got_space = 1 ;
		list++ ;
	
		if(got_space) goto reparse ;

	} ;

//	LOG(DBG,"Returning 0x%08X",mask) ;
	return mask ;
}

/*
	returns the index (the number after the leading /#) if
	it finds it or 0 if not. 0 is not allowed.
*/
static u_int is_physics(const char *name)
{
	if(strncmp(name,"/#",2)==0) {
		u_int ix ;
		if(sscanf(name,"/#%d/",&ix)!=1) return 0 ;
		return ix ;
	}
	
	return 0 ;

}

/*
	returns the RTS_ID of the detector or -1 if not a special
	event
*/
static int rts_non_physics(const char *name)
{
	if(strncmp(name,"/#",2)==0) return -1 ;

	name++ ;	// move from "/"

	return name2rts(name) ;	// name2rts will only look at starting characters i.e. "/tpx/asdasdasd" & "/tpxadssssss" will both match!
}

