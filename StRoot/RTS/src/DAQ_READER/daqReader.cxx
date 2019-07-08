#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <assert.h>
#include <iccp2k.h>

#if defined(__linux__) || defined(__APPLE__)
#include <sched.h>
// some older Linuxes (notably 6.2) crash and burn
// because of not having madvise so...
// Redhat EL3 crashes the kernel! with madvise... christ...
#define madvise(x,y,z)

#else
#include <procfs.h>
#endif

// MUST BE INCLUDED BEFORE ANY OTHER RTS INCLUDE!
//#include <daqReader.hh>

#include <rtsLog.h>
#include <daqFormats.h>
#include <iccp.h>
#include <SFS/sfs_index.h>
#include <rts.h>

#include "daqReader.h"
#include "msgNQLib.h"
#include "cfgutil.h"
#include "daq_det.h"

#ifndef MADV_DONTNEED
#define madvise(x,y,z)
#endif


u_int evp_daqbits ;

//Tonko:
static const char cvs_id_string[] = "$Id: daqReader.cxx,v 1.71 2019/07/08 15:21:30 jml Exp $" ;

static int evtwait(int task, ic_msg *m) ;
static int ask(int desc, ic_msg *m) ;

DATAP *getlegacydatap(char *mem, int bytes);

//static int thisNode = EVP_NODE ;
//static int evpDesc ;
static const char *getCommand(void) ;




// CONSTRUCTOR!
daqReader::daqReader(char *mem, int size)
{


    init();
    input_type = pointer;
  
    data_memory = mem;
    event_memory = mem;
    evt_offset_in_file = 0;

    data_size = size;  
  
    file_size = size;
    // LOG("JEFF", "filesize=%d mem=%p",file_size,mem);

    crit_cou = 0;


}

daqReader::daqReader(char *name) 
{
  struct stat64 stat_buf ;

  // Default map flags
  map_prot = PROT_READ;
  map_flags = MAP_SHARED | MAP_NORESERVE;

  init();

  if(name == NULL) {	// EVP
    input_type = live;
    isevp = 1;
    if(reconnect() < 0) status = EVP_STAT_CRIT;	// will loop until success or Cntrl-C I guess...
    return ;	// that's it....
  }

  // Move the filename in...
  strcpy(fname, name);


  // This code is reached only if the argument was non-NULL
  // file or directory?
  if(stat64(fname, &stat_buf) < 0) {	// error
    LOG(CRIT,"Can't stat \"%s\" [%s]",fname,strerror(errno),0,0,0);
    status = EVP_STAT_CRIT;
    sleep(1);
    return;
  }

  LOG(NOTE,"Using file \"%s\"...",fname,0,0,0,0) ;

  // directory?
  if(stat_buf.st_mode & S_IFDIR) {
    LOG(DBG,"Running through a directory %s...",fname,0,0,0,0) ;
    input_type = dir;

    // change to that directory...
 //    if(chdir(fname) < 0) {
//       LOG(ERR,"Can't chdir %s [%s]",fname,strerror(errno),0,0,0) ;
//       sleep(1) ;
//       return ;
//     }

    status = EVP_STAT_OK ;
    return;
  }	
  
  // If not, it must be a file...
  file_size = stat_buf.st_size ;
  evt_offset_in_file = 0;
  input_type = file;

  strcpy(file_name,fname) ;

  // descriptor for ".daq" view of file...
  LOG(DBG,"Running through a file %s of %d bytes",fname,file_size,0,0,0) ;
	
  desc = open64(fname,O_RDONLY,0444) ;
  if(desc < 0) {	// error
    LOG(ERR,"Can't open %s [%s]",fname,strerror(errno),0,0,0) ;
    sleep(1) ;
    return ;
  }
	 	  
  status = EVP_STAT_OK ;

  return ;
}

int daqReader::getDetectorSize(const char *det)
{
  if (!sfs) return 0;
  
  SfsDirsize sz;

  sfs->getDirSize((char *)det, &sz);
  // printf("sizeof(%s) : %lld  (%3.1f%% overhead)  %lld %lld\n",det, sz.dataSize, ((double)(sz.size - sz.dataSize)) / ((double)(sz.dataSize)), sz.dataSize, sz.size);

  return (int)sz.size;
}

void daqReader::init()
{
  // Get evphostname
  EVP_HOSTNAME = (char *)_EVP_HOSTNAME;
  static char evp_hostname[100];
  char *str = getenv((char *)"EVP_HOSTNAME");

  if(str) {
    strcpy(evp_hostname, str);
    EVP_HOSTNAME = evp_hostname;
  }


#ifndef RTS_ONLINE
  // this is only for Offline!

  // rtsLogLevel(WARN) ;
  // rtsLogAddDest("130.199.60.86",RTS_LOG_PORT_READER) ;	// reader.log to daqman

  // allow this one message to daqman's reader log...
  //rtsLogOutput(RTS_LOG_NET) ;
  //LOG(INFO,"%s",cvs_id_string) ;
  // back to STDERR
  rtsLogOutput(RTS_LOG_STDERR) ;	// STDERR only!
  LOG(INFO,"%s",cvs_id_string) ;
#endif


  // if(strcmp(EVP_HOSTNAME, _EVP_HOSTNAME) != 0) {
  LOG(DBG,"EVP_HOSTNAME set to %s", EVP_HOSTNAME);
  //  }

  isevp = 0;   // assume not... 
  fname[0] = '\0';
  memmap = new MemMap();
  sfs = new sfs_index();
  sfs_lastevt = 0;
	
  runconfig = (rccnf *) valloc(sizeof(rccnf)) ;
  memset(runconfig,0,sizeof(rccnf));

  memset(dets,0,sizeof(dets)) ;
  memset(pseudo_dets,0,sizeof(pseudo_dets));

  // setup...
  do_open = 1 ;
  do_mmap = 1 ;
  strcpy(evp_disk,"") ;

  desc = -1 ;
  //mem_mapped = NULL ;
  //bytes_mapped = 0 ;

  issued = 0 ;
  last_issued = time(NULL) ;
  status = EVP_STAT_CRIT ;
  //  tot_bytes = 0 ;

  input_type = none;

  event_number = 0 ;	// last known event...
  total_events = 0 ;
  readall_reset();
  bytes = 0 ;

  file_size = 0 ;		// only if it's a real big file...

  status = EVP_STAT_OK ;
	  
  return ;
}

int daqReader::setOpen(int flg)
{	
	int ret ;

	ret = do_open ;
	do_open = flg ;

	return ret ;
}

int daqReader::setMmap(int flg)
{	
	int ret ;

	ret = do_mmap ;
	do_mmap = flg ;

	return ret ;
}

int daqReader::setLog(int flg)
{	

	if(flg) {
		rtsLogOutput(RTS_LOG_STDERR|RTS_LOG_NET) ;
	}
	else {
		rtsLogOutput(RTS_LOG_NET) ;
	}

	return 0 ;
}


char *daqReader::setEvpDisk(char *name)
{
	char *saved = _static_str_return_;

	strcpy(saved, evp_disk) ;
	strncpy(evp_disk,name,sizeof(evp_disk)-1) ;

	return saved ;
}

daqReader::~daqReader(void)
{
	LOG(DBG,"Destructor %s",fname,0,0,0,0) ;

	// clean-up input file...
	if(desc >= 0) close(desc) ;


	if(input_type == live) {
		msgNQDelete(evpDesc) ;
	}

	if(memmap) delete(memmap);
	if(sfs) delete(sfs) ;
        if(runconfig) free(runconfig) ;

	// call detector destructors!
	for(int i=0;i<32;i++) {
		if(dets[i]) {
			LOG(DBG,"Will destruct det %d",i) ;
			delete(dets[i]) ;
		}
		if(pseudo_dets[i]) {
			LOG(DBG,"Will destruct pseudo det %d",i) ;
			delete(pseudo_dets[i]) ;
		}
	}


	return ;
}

/*
	Args
	=====
	If	num == 0	Next event
		num > 0		That particula event (if it makes sense...)


	Output
	======
	Returns a valid pointer on success or NULL on error...
*/


char *daqReader::get(int num, int type)
{
    //  static int crit_cou = 30 ;
    //static struct LOGREC lrhd ;
    int ret ;
    //int leftover, tmp ;
    // char _evp_basedir_[40];
    //static char _last_evp_dir_[40];

    memset(trgIds, 0xffffffff, sizeof(trgIds));
    trgIdsSet = 0;
    trgIdsNotPresent = 0;

    event_number++;
    LOG(DBG, "processing nth event.  n=%d from %s %lld",event_number, getInputType(),evt_offset_in_file);
  
    // nix some variables which may have no meaning for all streams...
    evb_type = 0 ;
    evb_type_cou = 0 ;
    evb_cou = 0 ;
    run = 0 ;

    memset(streaming_node, 0, sizeof(streaming_node));
    streaming_evb = -1;
    streaming_seq = -1;

    // check status of constructor or previous get()
	
    int delay = getStatusBasedEventDelay();
    if(delay) {
	if(delay == -1) {
	    LOG(CRIT, "Exiting because of critical errors");
	    exit(0);
	}

	LOG(DBG, "Delay of %d usec because of previous event status %d",delay, status);
	usleep(delay);
    }
	  
    // unmap previous event if any...
    if(memmap->mem) {
	memmap->unmap();
    }

    if((input_type == dir) ||
       (input_type == live)) {
    
	if(getNextEventFilename(num, type) < 0) {
	    event_number--;
	    return NULL;
	}

	if(openEventFile() < 0) {
	    if(input_type == dir) {
		LOG(NOTE,"No File, but didn't see token 0 - stopping...",event_number) ;
		status = EVP_STAT_EOR ;
		event_number--;
		//event_number = 1;
	    }
	    LOG("JEFF", "x");
	    return NULL;
	}
    }
  
    // at this point:
    //     1.  there is no file and we are memory mapped already
    //     2.  (or) the file is opened...
    //
    // Why? either: 
    //
    //  1. Is a single event file in which case it was opened above
    //     the current position is set to the begining of the file
    // 
    // (or)
    // 
    //  2. it is a multi-event file and the file is opened in the constructer
    //     or remains open from the previous event.   The current position is set to 
    //     the begining of the next event   (points at:  LRHD or DATAP or sfs FILE record)
    //    
    //if((input_type == file) || (input_type == pointer)) event_number++ ;
	
    if(input_type == pointer) {   // start at beginning...
	if(event_memory == NULL) 
	    event_memory = data_memory;
    }

    // Sets:
    //     event_size, lrhd_offset, datap_offset & status if failure...

 repeat:

    int error = getEventSize();  

    if(status == EVP_STAT_LOG) { 
	// we know this is a stand alone SFS file
	// and we know it is a log message, or at least has no event
	// try the next directory

	LOG(DBG, "Skipping a non-event SFS file...");

	lseek64(desc, event_size, SEEK_CUR);
	evt_offset_in_file += event_size;
	//LOG("JEFF", "evt_offset_in_file = %d",evt_offset_in_file);
	event_size = 0;
	goto repeat;
    }
  
    // We handle the possible padding bug by
    // searching and tacking the extra padding to the end of the event
    // where it can do us no harm...
    //
    int padchecks = 0;
    for(;;) {
	padchecks++;
	int inc = addToEventSize(event_size);
  
	if(inc == 0) {
	    LOG(DBG, "No extra increment... event size=%d",event_size);
	    break;
	}

	LOG(WARN, "Found a padding bug.  Adding %d to event size",inc);
	event_size += inc;

	if(padchecks > 5) {
	    LOG(ERR, "Error finding next event...");
	    status = EVP_STAT_EOR;
	    return NULL;
	}
    }

    LOG(DBG, "Event size is %d (%d) %d",event_size, error, status); 

    if(status == EVP_STAT_EOR) {
	LOG(DBG, "Status = EOR");
//	    LOG("JEFF", "x");
	return NULL;
    }

    if(error == 0) {
	status = EVP_STAT_EOR;
	    LOG(NOTE, "x");
	return NULL;
    }

    if(error < 0) {
	status = EVP_STAT_EVT;
	    LOG("JEFF", "x");
	return NULL;
    }

    if((event_size + evt_offset_in_file) > file_size) {
	LOG(WARN,"This event is truncated... Good events %d [%d+%d > %d]...", total_events,evt_offset_in_file,event_size,file_size,0) ;
	if((input_type == pointer) ||
	   (input_type == dir)) {
	    status = EVP_STAT_EVT ;
	}
	else {
	    status = EVP_STAT_EOR ;
	}
	    LOG("JEFF", "x");
	return NULL;
    }
    

    if(input_type != pointer) {
	LOG(DBG, "Mapping event file %s, offset %d, size %d",
	    file_name, evt_offset_in_file, event_size);
    
	char *mapmem = memmap->map(desc, evt_offset_in_file, event_size, map_prot, map_flags);
	if(!mapmem) {
	    LOG(CRIT, "Error mapping memory for event");
	    exit(0);
	}
    }
    else {
	memmap->map_real_mem(event_memory, event_size);
    }

    LOG(DBG, "Event is now in memory:  start=0x%x, length=%d",memmap->mem,event_size);

    // Neccessary?
    if(input_type == pointer) {
	if(run == 0) {
	    LOG(DBG, "Does this ever get called?");
	    run = readall_run;
	}
    }

    LOG(DBG, "about to mount sfs file: %s %d 0x%x",file_name, evt_offset_in_file, sfs);

    // Now, mount the sfs file...
    // The mount unmounts and closes previous mount... 
    LOG(DBG, "mounting single dir(mem): off=%d sz=%d",evt_offset_in_file, event_size);


     
    LOG(DBG, "[%c%c%c%c]",memmap->mem[0],memmap->mem[1],memmap->mem[2],memmap->mem[3]);

    //printf("%p, %d %lld\n",memmap->mem,event_size, evt_offset_in_file);
    ret = sfs->mountSingleDirMem(memmap->mem, event_size, evt_offset_in_file);


    if(ret < 0) {
	LOG(ERR, "Error mounting sfs?");
	status = EVP_STAT_EVT;
	    LOG("JEFF", "x");
	return NULL;
    }

    // CD to the current event...
    fs_dir *fsdir = sfs->opendir("/");
    for(;;) {
	fs_dirent *ent = sfs->readdir(fsdir);
	if(!ent) {
	    sfs->closedir(fsdir);
	    LOG(ERR, "Error finding event directory in sfs?");

	    // Skip directory... go to next
	    status = EVP_STAT_EVT;
	    LOG("JEFF", "x");
	    return NULL;
	}

	LOG(DBG, "does dir (%s) satisfy '/#' or '/nnnn'",ent->full_name);

	if(memcmp(ent->full_name, "/#", 2) == 0) {
	    LOG(DBG, "change sfs dir to %s",ent->full_name);

	    seq = atoi(&ent->full_name[2]);

	    sfs->cd(ent->full_name);
	    sfs->closedir(fsdir);
	    break;
	}

	if(memcmp(ent->full_name, "/%", 2) == 0) {
	    char _node[10];
	    char _evb[10];
	    char _idx[10];
	    memset(_node, 0, sizeof(_node));
	    memset(_evb, 0, sizeof(_evb));
	    memset(_idx, 0, sizeof(_idx));

	    char *s = &ent->full_name[2];
	    char *d = _node;
	    while(*s != '-') {
		*d++ = *s++;
	    }
	    *s++;
	    d = &_evb[0];
	    while(*s != '-') {
		*d++ = *s++;
	    }
	    *s++;
	    d = &_idx[0];
	    while(*s != '\0') {
		*d++ = *s++;
	    }

	    strcpy(streaming_node, _node);
	    streaming_evb = atoi(_evb);
	    streaming_seq = atoi(_idx);

	    LOG(DBG, "change sfs dir to %s (%s) (%s) (%s)",
		ent->full_name, _node, _evb, _idx);

	    sfs->cd(ent->full_name);
	    sfs->closedir(fsdir);
	    break;
	}
	    
	if(allnumeric(&ent->full_name[1])) {
	    seq = atoi(&ent->full_name[1]);
	    sfs->cd(ent->full_name);
	    sfs->closedir(fsdir);
	    break;
	}
     

	LOG(DBG, "SFS event directory not yet found: %s",ent->full_name);
    }
    
    fs_dirent *datap = sfs->opendirent("legacy");
    if(datap) {
	mem = memmap->mem + datap->offset;
	LOG(DBG, "Event has a datap bank at 0x%x",mem);
    }
    else {
	mem = NULL;
	LOG(DBG, "Event has no DATAP bank");
    }


    bytes = 0;
    if(mem) {
	bytes = event_size - (mem - memmap->mem);
	LOG(DBG, "size = %d %d",event_size, bytes);
    }

    // Fill in run number
    //
    run = 0;
    fs_dirent *lrhd_ent = sfs->opendirent("lrhd");
    if(lrhd_ent) {
	char *lrhd_buff = memmap->mem + lrhd_ent->offset;
	LOGREC *lrhd_rec = (LOGREC *)lrhd_buff;
	run = lrhd_rec->lh.run;

	if(lrhd_rec->lh.byte_order != 0x04030201) {
	    run = swap32(run);
	}
    }


    // Now we need to fill in the summary information from datap
    //
    SummaryInfo info;
    fs_dirent *summary = sfs->opendirent("EventSummary");
    if(summary) {
	char *buff = memmap->mem + summary->offset;
	fillSummaryInfo(&info,(gbPayload *)buff);
	copySummaryInfoIn(&info);
    }
    else { // take it from datap
	LOG(DBG, "No EventSummary, search for legacy datap");
	summary = sfs->opendirent("legacy");
	if(!summary) {
	    LOG(DBG, "No EventSummary and no DATAP... hacking summary info");
	    hackSummaryInfo();
	}
	else {
	    char *buff = memmap->mem + summary->offset;
	    fillSummaryInfo(&info,(DATAP *)buff);
	    copySummaryInfoIn(&info);
	}
    }

    // all done - all OK
    status = EVP_STAT_OK ;
    //tot_bytes += bytes ;
    total_events++ ;


    // move to next event although it may make no sense...
    long long int endpos = lseek64(desc, 0, SEEK_CUR);

    long long int nexteventpos = lseek64(desc, event_size, SEEK_CUR) ;
	
    LOG(DBG,"End of event:  start_offset=%d  end_offset=%lld, file size %lld",endpos,nexteventpos,file_size) ;

    evt_offset_in_file = nexteventpos;
    //LOG("JEFF", "evt_offset_in_file = %d",evt_offset_in_file);

    // Now we want to get detsinrun/evpgroupsinrun
    // First read from the EvbSummary

    fs_dirent *esum = sfs->opendirent("EvbSummary");
    if(esum) {
	LOG(DBG, "We've got an EvbSummary");
	char *buff = (char *)memmap->mem + esum->offset;
	EvbSummary *evbsum = (EvbSummary *)buff;
	detsinrun = evbsum->detectorsInRun;
	detsinrun64 = evbsum->detectorsInRun;
	evpgroupsinrun = 0xffffffff;
    }
    else {
	LOG(DBG, "No EvbSummary Record");
	if(run != (unsigned int)runconfig->run) {
	    char rccnf_file[256];

	    runconfig->run = 0;   // set invalid to start...
      
      
	    if(input_type == live) {
		sprintf(rccnf_file,"%s%s/%d/0",evp_disk,_evp_basedir_,run) ;
	    }
	    else if(input_type == dir) {
		sprintf(rccnf_file,"%s/0",fname);
	    }
	    else if (input_type == file) {
		sprintf(rccnf_file,"rccnf_%d.txt",run);
	    }
      
	    if(input_type != pointer) {
		if(getRccnf(rccnf_file, runconfig) < 0) {
		    LOG(DBG, "No runconfig file %s",rccnf_file,0,0,0,0);
		}
	    }
      
	    if(runconfig->run == 0) {
		detsinrun = 0xffffffff;
		detsinrun64 = 0xffffffffffffffff;
		evpgroupsinrun = 0xffffffff;
	    }
	    else {
		detsinrun = runconfig->detMask;
		detsinrun64 = runconfig->detMask;
		evpgroupsinrun = runconfig->grpMask;
	    }
	}
    }

    /* Tonko; May 11, 2012
       Catch the TPC FY12 UU future-protection bug
    */
    detector_bugs = 0;	// clear them all first
    detector_bugs64 = 0;

    if(detectors & (1 << TPX_ID)) {
	if((run >= 13114025) && (run < 13130030)) {
	    if(trgcmd == 4) {

		for(int s=1;s<=24;s++) {
		    for(int r=1;r<=6;r++) {
			// skip known dead RDOs during this period
			if((s==5) && (r==1)) continue ;
			if((s==6) && (r==1)) continue ;
			if((s==7) && (r==1)) continue ;
			if((s==14) && (r==3)) continue ;
			if((s==21) && (r==1)) continue ;
			if((s==22) && (r==2)) continue ;

	
			char name[32] ;

			sprintf(name,"tpx/sec%02d/cld%02d",s,r) ;
			if(!get_sfs_name(name)) {
			    detectors &= ~(1<<TPX_ID) ;
			    LOG(WARN,"run %d, seq %d -- removing TPX due to FY12 UU future-protection bug",run,seq) ;

			    detector_bugs |= (1<<TPX_ID) ;	// set tje bug status
			    detector_bugs64 |= (1<<TPX_ID);
			    goto bug_check_done ;
			}
		    }
		}
	    }
	}
    bug_check_done:;
    }

    // Tonko: before we return, call Make which prepares the DETs for operation...
    //Make() ;

    // *****
    // jml 2/13/07
    // now we return pointer to this
    // get datap by evp->mem
    //
    // why? each reader needs to know whether it is acting on 
    // a sfs file or a .daq buffer
    //
    // the current design is messed up in that every detector bank
    // is global and static while the evpReader is a class
    // that can have multiple instances.   the detReader functions
    // used to take datap pointers so nothing at all could be passed
    // to them.   At least this hack doesn't require modifications to
    // existing code...
    //

    // ****
    // old false comment...
    // return the pointer at the beginning of DATAP!
    // at this point we return the pointer to READ_ONLY DATAP
    // the event size is "bytes"
    // ****
    return (char *)this;
}


// Skip n events then, call get...
char *daqReader::skip_then_get(int numToSkip, int num, int type)
{
    //LOG("JEFF", "into skip");
  if(input_type != file) {
    LOG(CRIT, "Can't call skip_then_get unless running from file...");
    status = EVP_STAT_EVT;
    return NULL;
  }

  event_number++;

  int error;

  for(int i=0;i<numToSkip;i++) {
    event_number++;
    
  repeat2:
    error = getEventSize();
    
    if(status == EVP_STAT_LOG) {  
      lseek64(desc, event_size, SEEK_CUR);
      evt_offset_in_file += event_size;
      //LOG("JEFF", "evt_offset_in_file = %d",evt_offset_in_file);
      event_size = 0;
      goto repeat2;
    }


    // handle potential padding bug...
    int padchecks = 0;
    for(;;) {
      padchecks++;
      int inc = addToEventSize(event_size);
  
      if(inc == 0) {
	LOG(DBG, "No extra increment... event size=%d",event_size);
	break;
      }

      LOG(WARN, "Found a padding bug.  Adding %d to event size",inc);
      event_size += inc;

      if(padchecks > 5) {
	LOG(ERR, "Error finding next event...");
	status = EVP_STAT_EOR;
	return NULL;
      }
    }

    if(status == EVP_STAT_EOR) {
      return NULL;
    }
    
    if(error == 0) {
      status = EVP_STAT_EOR;
      return NULL;
    }
    
    if(error < 0) {
      status = EVP_STAT_EVT;
      return NULL;
    }

    if((event_size + evt_offset_in_file) > file_size) {
      LOG(WARN, "This event is truncated");
      status = EVP_STAT_EOR;
      return NULL;
    }

    long long int nexteventpos = lseek64(desc, event_size, SEEK_CUR);
    LOG(DBG, "skip evt pos = %lld", nexteventpos);

    evt_offset_in_file += event_size;
    //LOG("JEFF", "evt_offset_in_file = %d",evt_offset_in_file);
  }

  LOG(DBG, "out of skip");
  return get(num, type);
}

  // Get event size...
  int daqReader::addToEventSize(int sz)
  {
    if(input_type == pointer) return 0;

    long long int orig_offset = lseek64(desc, 0, SEEK_CUR);

    LOG(DBG, "orig_offset = %lld sz=%d",orig_offset,sz);

    lseek64(desc, sz, SEEK_CUR);

    char buff[10];
    int ret = read(desc, buff, 8);
    if(ret == 0) {
      lseek64(desc, orig_offset, SEEK_SET);
      return 0;
    }
  
    if(memcmp(buff, "LRHD",4) == 0) {
      lseek64(desc, orig_offset, SEEK_SET); 
      return 0;
    }

    if(memcmp(buff, "DATAP",4) == 0) {
      lseek64(desc, orig_offset, SEEK_SET);
      return 0;
    }

    if(memcmp(buff, "SFS", 3) == 0) {
      lseek64(desc, orig_offset, SEEK_SET);
      return 0;
    }

    if(memcmp(buff, "FILE", 4) == 0) {
      lseek64(desc, orig_offset, SEEK_SET);
      return 0;
    }

    LOG(ERR, "buff = %c%c%c  off=%lld",buff[0],buff[1],buff[2], orig_offset);

    lseek64(desc, orig_offset, SEEK_SET);

  
    return 8192;
  }

  int daqReader::getEventSize()
  {
    MemMap headermap;
    char *m;
    int swap;

    int ret = -1;    // lets be a pessimist and assume failure...
    long long int offset = 0;
    long long int space_left;

    //LOG("JEFF", "Get event size");

    status = EVP_STAT_OK;
    event_size = 0;

    if(input_type == pointer) {
      m = event_memory;
      space_left = data_size - (m - data_memory);


      //LOG("JEFF", "space_left=%d ds=%d m-dm=%d",space_left,data_size,m-data_memory);
    }
    else {
      offset = lseek64(desc, 0, SEEK_CUR);
      space_left = file_size - offset;

      if(space_left > 1024) space_left = 1024;
  
      LOG(DBG, "Space left = %d",space_left);

      if(space_left == 0) return 0;
      if(space_left < (long long int)sizeof(LOGREC)) return -1;

      m = headermap.map(desc, offset, space_left, map_prot, map_flags);
      if(!m) {
	LOG(ERR, "Error mapping header information");
	return -1;
      }
    }

  
    //LOG("JEFF", "space_left=%d",space_left);

    if(space_left == 0) return 0;
    if(space_left < (long long int)sizeof(LOGREC)) {
      LOG(NOTE, "File truncated: only %lld bytes left",space_left);
      status = EVP_STAT_EOR;
      goto done;
    }
  
    LOG(DBG, "OFFSET = %lld", offset);

    // Wait, we might not have a LRHD or DATAP...
    while((memcmp(m, "LRHD", 4) != 0) &&
	  (memcmp(m, "DATAP", 5) != 0)) {
    
	//LOG("JEFF", "Event starts with %c%c%c%c%c not LRHD or DATAP.  Check if sfs file...",m[0],m[1],m[2],m[3],m[4]);
    
      sfs_index *tmp_sfs = new sfs_index();
      int sz = tmp_sfs->getSingleDirSize(file_name, evt_offset_in_file);

      //LOG("JEFF", "single dir size = %d",sz);

      // Check to see if its a valid directory...
      if(sz > 0) {
	tmp_sfs->mountSingleDir(file_name, evt_offset_in_file);
      
	int satisfy=0;

	// CD to the current event...
	fs_dir *fsdir = tmp_sfs->opendir("/");
	fs_dirent *ent = tmp_sfs->readdir(fsdir);
	if(!ent) {
	  tmp_sfs->closedir(fsdir);
	  LOG(ERR, "Error finding event directory in sfs?");
	
	  // Skip directory... go to next
	  status = EVP_STAT_EVT;
	  sz = -1;
	  goto done;
	}
	
	LOG(DBG, "does dir (%s) satisfy '/#' or '/nnnn'",ent->full_name);
      
	if(memcmp(ent->full_name, "/#", 2) == 0) {
	  satisfy = 1;
	}

	// streaming...
	if(memcmp(ent->full_name, "/%", 2) == 0) {
	    satisfy = 1;
	}
	
	if(allnumeric(&ent->full_name[1])) {
	  satisfy = 1;
	}
	
	tmp_sfs->closedir(fsdir);

	if(satisfy == 0) {
	  status = EVP_STAT_LOG;
	  //LOG("JEFF", "STATLOG");
	} 

	tmp_sfs->umount();
      }

      delete tmp_sfs;

      if(sz < 0) {
	LOG(ERR, "Event starts with %c%c%c%c%c not LRHD or DATAP and not a SFS file... bad event",m[0],m[1],m[2],m[3],m[4]);

	status = EVP_STAT_EVT;
	goto done;
      }

      event_size = sz;
      ret = 0;
      goto done;
    }

    // LOG("JEFF", "here?");

    //  Now at the start of the real event!
    //
    // if datap, simple...
    if(memcmp(m, "DATAP", 5) == 0) {
      DATAP *datap = (DATAP *)m;
      int swap = (datap->bh.byte_order == 0x04030201) ? 0 : 1;
    
      event_size = qswap32(swap, datap->len) * 4;
      ret = 0;
      goto done;
    }

    // Ok... typical event with LRHD starting point
    // first skip non-data banks
    LOGREC *lrhd;

    for(;;) {   
      if(memcmp(m, "LRHD", 4) == 0) {
	lrhd = (LOGREC *)m;

	LOG(DBG, "record_type = %c%c%c%c", 
	    lrhd->record_type[0],
	    lrhd->record_type[1],
	    lrhd->record_type[2],
	    lrhd->record_type[3]);
	  
	if(memcmp(lrhd->record_type, "DATA", 4) == 0) break;

	if(memcmp(lrhd->record_type, "ENDR", 4) == 0) {

	  LOG(DBG, "Got ENDR record");

	  status = EVP_STAT_EOR;
	  goto done;
	}
	
	LOG(DBG, "Skipping LRHD record type %c%c%c%c",
	    lrhd->record_type[0],
	    lrhd->record_type[1],
	    lrhd->record_type[2],
	    lrhd->record_type[3]);

	space_left -= sizeof(LOGREC);
	offset += sizeof(LOGREC);
	m += sizeof(LOGREC);  
	event_size += sizeof(LOGREC);

	if(space_left < (long long int)sizeof(LOGREC)) {
	  LOG(NOTE, "File truncated: only %lld bytes left",space_left);
	  status = EVP_STAT_EOR;
	  goto done;
	}

      }
      else {
	LOG(DBG, "Corrupted headers: %c%c%c%c%c",
	    m[0],m[1],m[2],m[3],m[4]);
      
	goto done;
      }
    }

    //LOG("JEFF", "here");

    // This is a valid event!
    ret = 0;

    // Have the DATA LRHD in *m	
    //LOG("JEFF", "event_size=%d",event_size);

    swap = (lrhd->lh.byte_order == 0x04030201) ? 0 : 1;
    event_size += qswap32(swap,lrhd->length) * 4;

    //LOG("JEFF", "here %d",event_size);
  done:

    //LOG("JEFF", "event_size=%d",event_size);
    if(ret == 0) {
      ret = event_size;
    }

    headermap.unmap();
    return ret;
  }


  char *daqReader::getInputType()
    {
      switch(input_type) {
      case none: 
	return "none";
      case pointer:
	return "pointer";
      case file:
	return "file";
      case live:
	return "live";
      case dir:
	return "dir";
      }

      return "null";
    }

  int daqReader::openEventFile()
  {
    struct stat64 stat_buf ;

    // First close file if any...
    if(desc > 0) {
      close(desc);
      desc = -1;
    }

    errno = 0 ;

    desc = open64(file_name,O_RDONLY,0666) ;
    if(desc < 0) {	
      LOG(NOTE,"Error opening file %s [%s] - skipping...",file_name,strerror(errno),0,0,0) ;
      status = EVP_STAT_EVT ;
      return -1; 
    }

    // get the file_size ;
    int ret = stat64(file_name,&stat_buf) ;
    if(ret < 0) {	
      LOG(ERR,"Can't stat %s",file_name,0,0,0,0) ;
      status = EVP_STAT_EVT ;
      close(desc) ;
      desc = -1 ;
      return -1;
    }
   
    file_size = stat_buf.st_size ;
    evt_offset_in_file = 0;
    bytes = 0 ;
    return 0;
  }

  int daqReader::getNextEventFilename(int num, int type)
  {
    if((event_number != 1) && (token == 0)) {	// we read at least one event and it was token==0 thus this is it...
      LOG(DBG,"Previous event (%d) was Token 0 in directory - stopping...",event_number,0,0,0,0) ;
      status = EVP_STAT_EOR ;
      if(input_type == live) {
	event_number = 1;
      }
      return -1;
    }

    if(input_type == dir) {
      return getNextEventFilenameFromDir(num);
    }
    else if (input_type == live) {
      return getNextEventFilenameFromLive(type);
    }
    else {
      LOG(ERR, "Wrong input type");
      return -1;
    }
  }

  int daqReader::copySummaryInfoIn(SummaryInfo *info)
  {  
    // gbPayload is mostly little endian...
    token = info->token;
    evt_time = info->evt_time;
    detectors = info->detectors;
    detectors64 = info->detectors;
    daqbits_l1 = info->daqbits_l1;
    daqbits_l2 = info->daqbits_l2;
    evpgroups = info->evpgroups;
    daqbits = info->daqbits;
    evp_daqbits = info->evp_daqbits;
    //seq = info->seq;
  
    // event descriptor is big endian...
    trgword = info->trgword;
    trgcmd = info->trgcmd;
    daqcmd = info->daqcmd;
    flags = info->flags;

    memcpy(L1summary, info->L1summary, sizeof(L1summary));
    memcpy(L2summary, info->L2summary, sizeof(L2summary));
    memcpy(L3summary, info->L3summary, sizeof(L3summary));
  
    // 64 bit extension...
    daqbits64 = ((u_longlong)L3summary[1]) << 32;
    daqbits64 += L3summary[0];
    daqbits64_l1 = ((u_longlong)L1summary[1]) << 32;
    daqbits64_l1 += L1summary[0];
    daqbits64_l2 = ((u_longlong)L2summary[1]) << 32;
    daqbits64_l2 += L2summary[0];

    return 0;
  }


  int daqReader::hackSummaryInfo()
  {
    // gbPayload is mostly little endian...
    token = 0;
    evt_time = 0;
    detectors = 0;
    detectors64 = 0;
    daqbits_l1 = 0;
    daqbits_l2 = 0;
    evpgroups = 0;
    daqbits = 0;
    evp_daqbits = 0;

    daqbits64 = 0ll;
    daqbits64_l1 = 0ll;
    daqbits64_l2 = 0ll;

    // event descriptor is big endian...
    trgword = 0;
    trgcmd = 0;
    daqcmd = 0;
  
    memset(L1summary, 0, sizeof(L1summary));
    memset(L2summary, 0, sizeof(L2summary));
    memset(L3summary, 0, sizeof(L3summary));

    return 0;
  }

  int daqReader::fillSummaryInfo(SummaryInfo *info, gbPayload *pay)
  {
    // First, determine which gbPayload:

    LOG(DBG, "pay=0x%x",pay);

    u_int version = pay->gbPayloadVersion;

    LOG(DBG, "version = 0x%x", version);

    if(((version & 0xff000000) != 0xda000000) && ((b2h32(version) & 0x000000ff ) != 0x40)) {   // Version 0x01
      LOG(DBG, "gbPayload version 0x10");

      gbPayload_0x01 *pv = (gbPayload_0x01 *)pay;
      LOG(DBG, "gbPayload 0x01:  v#=0x%x",b2h32(version));    // picked up from big endian evtdes
      return fillSummaryInfo_v01(info, pv);
    }

    if(((version & 0xff000000) != 0xda000000) && ((b2h32(version) & 0x000000ff ) == 0x40)) {   // Version 0x01a
      LOG(DBG, "gbPayload version 0x01a");
      gbPayload_0x01a *pv = (gbPayload_0x01a *)pay;
      LOG(DBG, "gbPayload 0x01a:  v#=0x%x", b2h32(version));  // picked up from big endian evtdesc
      return fillSummaryInfo_v01a(info, pv);
    }
  
    if(version == 0xda000002) {
	LOG(DBG, "gbPayload 0x02: v#=0x%x",version);
	return fillSummaryInfo_v02(info, (gbPayload_0x02 *)pay);
    }

    if(version == 0xda000003) {
	LOG(DBG, "gbPayload 0x03: v=0x%x", version);
	return fillSummaryInfo_v03(info, pay);
    }

    LOG(ERR, "gbPayload Version Unknown: 0x%x vs 0x%x.  Using 02", version, GB_PAYLOAD_VERSION);
    return fillSummaryInfo_v03(info, pay);
  }

int daqReader::fillSummaryInfo_v03(SummaryInfo *info, gbPayload *pay) {
    // gbPayload is mostly little endian...

    LOG(DBG, "gbPayloadVersion=0x%x, trgVersion=0x%x", pay->gbPayloadVersion, pay->EventDescriptor.TrgDataFmtVer);

    info->token = l2h32(pay->token);
    info->evt_time = l2h32(pay->sec);
    info->detectors = l2h32(pay->rtsDetMask);
    info->detectors64 = l2h32(pay->rtsDetMask);
    info->daqbits_l1 = l2h32(pay->L1summary[0]);
    info->daqbits_l2 = l2h32(pay->L2summary[0]);
    info->evpgroups = l2h32(pay->L3summary[3]);
    info->daqbits = l2h32(pay->L3summary[0]);
    info->evp_daqbits = daqbits;
    info->flags = l2h32(pay->flags);

    // event descriptor is big endian...
    //info->trgword = b2h16(pay->EventDescriptor.TriggerWord);
    info->trgcmd = pay->EventDescriptor.actionWdTrgCommand;
    info->daqcmd = pay->EventDescriptor.actionWdDaqCommand;

    for(int i=0;i<2;i++) info->L1summary[i] = l2h32(pay->L1summary[i]);
    for(int i=0;i<2;i++) info->L2summary[i] = l2h32(pay->L2summary[i]);
    for(int i=0;i<4;i++) info->L3summary[i] = l2h32(pay->L3summary[i]);

    return 0;
  }


int daqReader::fillSummaryInfo_v02(SummaryInfo *info, gbPayload_0x02 *pay) {
    // gbPayload is mostly little endian...

    LOG(DBG, "gbPayloadVersion=0x%x, trgVersion=0x%x", pay->gbPayloadVersion, pay->EventDescriptor.TrgDataFmtVer);

    info->token = l2h32(pay->token);
    info->evt_time = l2h32(pay->sec);
    info->detectors = l2h32(pay->rtsDetMask);
    info->detectors64 = l2h32(pay->rtsDetMask);
    info->daqbits_l1 = l2h32(pay->L1summary[0]);
    info->daqbits_l2 = l2h32(pay->L2summary[0]);
    info->evpgroups = l2h32(pay->L3summary[3]);
    info->daqbits = l2h32(pay->L3summary[0]);
    info->evp_daqbits = daqbits;
    info->flags = l2h32(pay->flags);

    // event descriptor is big endian...
    //info->trgword = b2h16(pay->EventDescriptor.TriggerWord);
    info->trgcmd = pay->EventDescriptor.actionWdTrgCommand;
    info->daqcmd = pay->EventDescriptor.actionWdDaqCommand;

    for(int i=0;i<2;i++) info->L1summary[i] = l2h32(pay->L1summary[i]);
    for(int i=0;i<2;i++) info->L2summary[i] = l2h32(pay->L2summary[i]);
    for(int i=0;i<4;i++) info->L3summary[i] = l2h32(pay->L3summary[i]);

    return 0;
  }

  int daqReader::fillSummaryInfo_v01a(SummaryInfo *info, gbPayload_0x01a *pay)
  {  
    LOG(DBG, "gbPayloadVersion=0xda000001, trgVersion=0x%x", pay->EventDescriptor.TrgDataFmtVer);

    // gbPayload is mostly little endian...
    info->token = l2h32(pay->token);
    info->evt_time = l2h32(pay->sec);
    info->detectors = l2h32(pay->rtsDetMask);
    info->detectors64 = l2h32(pay->rtsDetMask);
    info->daqbits_l1 = l2h32(pay->L1summary[0]);
    info->daqbits_l2 = l2h32(pay->L2summary[0]);
    info->evpgroups = l2h32(pay->L3summary[2]);
    info->daqbits = l2h32(pay->L3summary[0]);
    info->evp_daqbits = daqbits;
    info->flags = l2h32(pay->flags);

    // event descriptor is big endian...
    info->trgword = b2h16(pay->EventDescriptor.TriggerWord);
    info->trgcmd = pay->EventDescriptor.actionWdTrgCommand;
    info->daqcmd = pay->EventDescriptor.actionWdDaqCommand;

    for(int i=0;i<2;i++) info->L1summary[i] = l2h32(pay->L1summary[i]);
    for(int i=0;i<2;i++) info->L2summary[i] = l2h32(pay->L2summary[i]);
    for(int i=0;i<4;i++) info->L3summary[i] = l2h32(pay->L3summary[i]);
  
    return 0;
  }

  int daqReader::fillSummaryInfo_v01(SummaryInfo *info, gbPayload_0x01 *pay)
  {  
    LOG(DBG, "gbPayloadVersion=0xda000001, trgVersion=0x%x", pay->EventDescriptor.TrgDataFmtVer);

    // gbPayload is mostly little endian...
    info->token = l2h32(pay->token);
    info->evt_time = l2h32(pay->sec);
    info->detectors = l2h32(pay->rtsDetMask);
    info->detectors64 = l2h32(pay->rtsDetMask);
    info->daqbits_l1 = l2h32(pay->L1summary[0]);
    info->daqbits_l2 = l2h32(pay->L2summary[0]);
    info->evpgroups = l2h32(pay->L3summary[2]);
    info->daqbits = l2h32(pay->L3summary[0]);
    info->evp_daqbits = daqbits;
  
    // event descriptor is big endian...
    info->trgword = b2h16(pay->EventDescriptor.TriggerWord);
    info->trgcmd = pay->EventDescriptor.actionWdTrgCommand;
    info->daqcmd = pay->EventDescriptor.actionWdDaqCommand;

    for(int i=0;i<2;i++) info->L1summary[i] = l2h32(pay->L1summary[i]);
    for(int i=0;i<2;i++) info->L2summary[i] = l2h32(pay->L2summary[i]);
    for(int i=0;i<4;i++) info->L3summary[i] = l2h32(pay->L3summary[i]);

    return 0;
  }


  int daqReader::fillSummaryInfo(SummaryInfo *info, DATAP *datap)
  {
    int swap = (datap->bh.byte_order == 0x04030201) ? 0 : 1;
  
    info->token = qswap32(swap, datap->bh.token);
    info->evt_time = qswap32(swap, datap->time);
    info->detectors = qswap32(swap, datap->detector);
    info->detectors64 = qswap32(swap, datap->detector);
    info->seq = qswap32(swap, datap->seq);
    info->daqbits_l1 = qswap32(swap, datap->TRG_L1_summary[0]);
    info->daqbits_l2 = qswap32(swap, datap->TRG_L2_summary[0]);
    info->evpgroups = qswap32(swap, datap->L3_Summary[2]);
    info->trgword = qswap32(swap, datap->trg_word);
  
    info->trgcmd = (qswap32(swap, datap->trg_in_word) >> 12) & 0xF ;	// _just_ the trigger command
    info->daqcmd = (qswap32(swap, datap->trg_in_word) >> 8) & 0xF ;	// DAQ command


    info->daqbits = qswap32(swap, datap->L3_Summary[0]);
    info->evp_daqbits = daqbits;

    for(int i=0;i<2;i++) info->L1summary[i] = qswap32(swap, datap->TRG_L1_summary[i]);
    for(int i=0;i<2;i++) info->L2summary[i] = qswap32(swap, datap->TRG_L2_summary[i]);
    for(int i=0;i<4;i++) info->L3summary[i] = qswap32(swap, datap->L3_Summary[i]);

    return 0;
  }



  // Only called when data source is directory
  // Fills in "file_name"
  // status = EOR if end of run
  int daqReader::getNextEventFilenameFromDir(int eventNum)
  {
    LOG(DBG, "Getting next event from dir: event_number=%d eventNum=%d",event_number, eventNum);

    if(eventNum==0) eventNum = event_number;
    sprintf(file_name,"%s/%d",fname,eventNum) ;
  
    LOG(DBG, "Getting next event from dir: event_number=%s",file_name);

    event_number = eventNum ;
    return STAT_OK;
  }

  int daqReader::getNextEventFilenameFromLive(int type)
  {
    int ret;
    ic_msg m ;

    // evp no longer requests events to be shipped to the pool... 
    // issue get event only if not issued before
		
    if(issued) {
      if((time(NULL) - last_issued) > 10) {	// reissue
	LOG(DBG,"Re-issueing request...",0,0,0,0,0) ;
	issued = 0 ;
      }
    }

    if(!issued) {

      m.ld.dword[0] = htonl(type) ;	// event type...

      LOG(DBG, "dword[0] is type=%d",type);

      ret = ask(evpDesc,&m) ;
      if(ret != STAT_OK) {	// some error...

	LOG(ERR,"Queue error %d - recheck EVP...",ret,0,0,0,0) ;
	reconnect() ;
	status = EVP_STAT_EVT ;	
	return -1;
      }
      issued = 1 ;
      last_issued = time(NULL) ;
    }
		 

    // wait for the event (unless run is finished but still checking)
    //if((num == 0) || !readall_rundone) {
    if(!readall_rundone) {
      int timedout=0;

      ret = evtwait(evpDesc, &m) ;

      // got some reply here so nix issued
      issued = 0;

      // If queues are broken we have problems...	    
      if((ret != STAT_OK) && (ret != STAT_TIMED_OUT)) {
	reconnect() ;
	LOG(ERR,"Queue error %d - recheck EVP...",ret,0,0,0,0) ;
	status = EVP_STAT_EVT ;	// treat is as temporary...
	return -1;
      }

      if(ret == STAT_TIMED_OUT) {	// retry ...
	timedout = 1;
   
#if defined(__linux__) || defined(__APPLE__)
	sched_yield() ;
#else
	yield() ;		// give up time slice?
#endif
      
	LOG(DBG, "Waiting 1 second, no event yet...");
	usleep(100000) ;		// .1 ms?
	status = EVP_STAT_OK ;	// no error...
	return -1;		// but also noevent - only on wait!	    
      }

      LOG(DBG, "m.head.status = %d  EOR=%d",m.head.status,EVP_STAT_EOR);

      // check misc. 

      switch(m.head.status) {
      case STAT_SEQ:
	{	// end of run!
		      
	  LOG(DBG,"End of Run!",0,0,0,0,0) ;
		     
	  status = EVP_STAT_EOR ;
	  return -1;
	}
	break;

      case STAT_OK:
	{
	  // if run_is_done, never overwrote m!

	  if(!timedout) {
	    evb_type = ntohl(m.ld.dword[2]) ;
	    evb_type_cou = ntohl(m.ld.dword[3]) ;
	    evb_cou = ntohl(m.ld.dword[4]) ;
	    run = ntohl(m.ld.dword[0]) ;
	    readall_run = run;
	    readall_lastevt = ntohl(m.ld.dword[1]);
	    strcpy(_evp_basedir_, (char *)&m.ld.dword[5]);
			
	    event_number = readall_lastevt;  // overwritten by num later...
			
	    sprintf(_last_evp_dir_, "%s%s/%d",evp_disk,_evp_basedir_,run);
	  }
	}
	break;

      default:
	{		// some event failure
	  if(!timedout) {
	    LOG(WARN,"Event in error - not stored...",0,0,0,0,0) ;
	    status = EVP_STAT_EVT ;			// repeat
	    return -1;
	  }
	}
	break;
      }
    }

    /* 
    // A few last checks...
    if(num != 0) {
    
    event_number = num;
    
    if(readall_lastevt == 0) {
    LOG(DBG, "EVP_ readall but don't have any events rundone=%d run=%d",readall_rundone,run);
    //    readall_reset();
    sleep(1);
    status = EVP_STAT_OK;
    return -1;
    }
    
    if(readall_lastevt < event_number) {
    LOG(WARN, "requesting an event that hasn't arrived rundone=%d run=%d",readall_rundone, run);
    sleep(1);
    status = EVP_STAT_OK;
    return -1;
    }
    }
    */

    // Now we've got the event number...
    sprintf(file_name,"%s/%d",_last_evp_dir_, event_number) ;
  
    struct stat64 s;
    if(stat64(file_name, &s) < 0) {
      LOG(DBG, "No file %s, try _DELETE",file_name);
      sprintf(file_name,"%s_DELETE/%d",_last_evp_dir_,event_number);
    }
		
    LOG(DBG,"Live Event: file->%s",file_name,0,0,0,0) ;
    return 0;
  }

  int daqReader::getOfflineId(int bit)
  {
    if(trgIdsNotPresent) return -1;
    if(bit > 63) return -1;

    if(trgIdsSet) {
      return trgIds[bit];
    }

    trgIdsSet = 1;
    
    fs_dirent *trgid_dir = sfs->opendirent("TRGID");
    if(trgid_dir) {
	UINT32 *trgid_buff = (UINT32 *)(memmap->mem + trgid_dir->offset);
	memset(trgIds,0xffffffff,sizeof(trgIds));

	int sz = trgid_dir->sz;
	sz /= 4;
	if(sz > 64) sz = 64;

	for(int i=0;i<sz;i++) {
	    trgIds[i] = trgid_buff[i];
	}
    }
    else { 
      LOG(ERR, "Can't find TRGID bank, can't get the offline id");
      memset(trgIds, 0xffffffff, sizeof(trgIds));
      trgIdsNotPresent = 1;
    }
  
    return trgIds[bit];
  }

  /////// Tonko

  char *daqReader::get_sfs_name(char *right)
    {
      if(sfs == 0) return 0 ;

      if(right == 0) right = "/" ;

      fs_dirent *d = sfs->opendirent(right) ;
      if(d == 0) return 0 ;

      LOG(DBG,"opendirent(%s) returns %s as full name, %s as d_name ",right,d->full_name,d->d_name) ;
      return d->full_name ;

    }

#if 0	// unused	
  /*
    parse the string of the form i.e. "tpc ssd tpx" and
    return a bitlist of RTS detectors
  */
  static u_int parse_det_string(const char *list)
    {
      u_int mask = 0 ;

      //LOG(DBG,"Parsing \"%s\"",list) ;



    reparse:;

      for(int i=0;i<32;i++) {
	const char *name = rts2name(i) ;

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

#endif

  daq_det *daqReader::det(const char *which)
    {
      assert(which) ;


      // for speed, first we try what we already created...
      for(int i=0;i<DAQ_READER_MAX_DETS;i++) {

	if(dets[i]) {
	  if(strcasecmp(which, dets[i]->name)==0) return dets[i] ;
	}
	if(pseudo_dets[i]) {
	  if(strcasecmp(which, pseudo_dets[i]->name)==0) return pseudo_dets[i] ;
	}
      }

      LOG(DBG,"det %s not yet created... attempting through factory...",which) ;
      int id = -1000 ;	// assume not found

      // not yet created; try real dets first...
      for(int i=0;i<DAQ_READER_MAX_DETS;i++) {
	const char *name = rts2name(i) ;
	if(name == 0) continue ;

	if(strcasecmp(name,which)==0) {

	  dets[i] = daq_det_factory::make_det(i) ;
	  dets[i]->managed_by(this) ;

	  return dets[i] ;	// done...
	}
      }

      // not found in DAQ dets, try pseudo dets...
      if(strcasecmp(which,"emc_pseudo")==0) id = -BTOW_ID ;	// by definition...
      if(strcasecmp(which,"hlt")==0) id = -L3_ID ;	// by definition...
      if(strcasecmp(which,"itpc_pseudo")==0) id = -SVT_ID ;	//HACK!!!!
//      if(strcasecmp(which,"l4")==0) id = -L4_ID ;

      if(id < -32) {	// not found even in pseudo
	LOG(CRIT,"Requested det \"%s\" not created -- check spelling!",which) ;
	assert(!"UNKNOWN DET") ;
	return 0 ;
      }

      int wh = -id ;	// make positive for pseudo array...
      pseudo_dets[wh] = daq_det_factory::make_det(id) ;
      pseudo_dets[wh]->managed_by(this) ;
	
      return pseudo_dets[wh] ;
    }

  void daqReader::insert(class daq_det *which, int id)
  {
    assert(which) ;
    LOG(DBG,"calling insert(%d): name %s",id,which->name) ;

    if((id>=0) && (id<DAQ_READER_MAX_DETS)) {
      dets[id] = which ;
      return ;
    }
    else if(id <0) {
      id *= -1 ;
      if(id >= DAQ_READER_MAX_DETS) ;
      else {
	pseudo_dets[id] = which ;
	return ;
      }
    }

    LOG(ERR,"rts_id %d out of bounds for %s",id,which->name) ;

  }

  void daqReader::Make()
  {
    for(int i=0;i<DAQ_READER_MAX_DETS;i++) {
      if(dets[i]) {
	LOG(DBG,"Calling %s make",dets[i]->name) ;
	dets[i]->Make() ;
      }
    }
  }

  void daqReader::de_insert(int id)
  {
    LOG(DBG,"calling de_insert(%d)",id) ;

    if((id>=0) && (id<DAQ_READER_MAX_DETS)) {
      if(dets[id]) {
	LOG(DBG,"Should destruct %d?",id) ;
      }
      dets[id] = 0 ;	// mark as freed
      return ;
    }
    else if(id < 0) {
      id *= -1 ;
      if(id >= DAQ_READER_MAX_DETS) ;
      else {
	if(pseudo_dets[id]) LOG(DBG,"Should destruct %d?",id) ;
	pseudo_dets[id] = 0 ;
	return ;
      }

    }

    LOG(ERR,"rts_id %d out of bounds",id) ;
  }


  /*
    char *daqReader::getSFSEventNumber()
    {
    //fsr_lastevt;

    fs_dir *dir = sfs->opendir("/");
    fs_dirent *ent;
    int evt=0x7fffffff;
    while((ent = sfs->readdir(dir))) {
    // need to skip the "#" ;
    int i ;
    if(ent->d_name[0] == '#') {
    i = atoi(ent->d_name+1);
    }
    else {
    i = atoi(ent->d_name);
    }

    //LOG(DBG,"atoi of %s is %d",ent->d_name,i) ;
    if((i < evt) && (i>sfs_lastevt)) {
    evt = i;
    }
    }
    sfs->closedir(dir);

    // check if no events left...
    if(evt==0x7fffffff) {
    if(isfilelist) {
    LOG(WARN,"File Finished: %d events.", total_events) ;
    status = EVP_STAT_EOR ;
    }
    else {
    LOG(ERR,"No event?") ;
    status = EVP_STAT_EVT ;
    }
    
    return NULL;
    }

    sfs_lastevt = evt;
  
    event_number = evt;
    total_events++;

    bytes = 0;
    // run = ?
  
    evb_type = 0;	
    evb_cou = 0;	 
    evb_type_cou = 0;      
    token = 0;
    trgcmd = 0;
    daqcmd = 0;
    trgword = 0;
    phyword = 0;	
    daqbits = 0;	
    evpgroups = 0;         

    evt_time = 0;
    seq = evt;	
    detectors = 0;

    detsinrun = 0;
    evpgroupsinrun = 0;

    return (char *)this;    
    }
  */




  static int evtwait(int desc, ic_msg *m)
    {
      int ret ;
      static int counter = 0 ;
	
      // wait with no timeout - handled by higher level code...
      ret = msgNQReceive(desc,(char *)m, sizeof(ic_msg),NO_WAIT) ;

      LOG(DBG,"msgNQReceive returned %d",ret,0,0,0,0) ;

      if(ret == MSG_Q_TIMEOUT) {
	LOG(DBG, "read a timeout count=%d",counter);
	counter++ ;
	if(counter >= 100) {
	  counter = 0 ;
	  if(msgNQCheck(desc)) {
	    LOG(DBG, "check returned ok...");
	    return STAT_TIMED_OUT ;
	  }
	  else {
	    LOG(DBG,"EVP_TASK died",0,0,0,0,0) ;
	    return STAT_ERROR ;
	  }
	}
    
	return STAT_TIMED_OUT ;
      }

      counter = 0 ;

      if(ret > 0) {
	int i, *intp ;
	intp = (int *) m ;
	LOG(DBG,"0x%08X 0x%08X 0x%08X; %d %d",*intp,*(intp+1),*(intp+2),m->head.daq_cmd,m->head.status) ;

	for(i=0;i<3;i++) {
	  *(intp+i) = ntohl(*(intp+i)) ;
	}
	LOG(DBG,"0x%08X 0x%08X 0x%08X; %d %d",*intp,*(intp+1),*(intp+2),m->head.daq_cmd,m->head.status) ;
	return STAT_OK ;
      }

      return STAT_ERROR ;	// critical - please reboot
    }

  // Fixes the summary fields for a randomly supplied datap
  // Returns -1 if error
  int daqReader::fixDatapSummary(DATAP *datap)
  {
    DATAP *sumdatap;

    fs_dirent *ent = sfs->opendirent("legacy");
    if(!ent) {
      LOG(ERR, "Can't find legacy data");
      return -1;
    }

    sumdatap = (DATAP *)(memmap->mem + ent->offset);
  
    if(memcmp(sumdatap->bh.bank_type, "DATAP", 5) != 0) {
      char *x = sumdatap->bh.bank_type;
      LOG(ERR, "fixDatapSummary... legacy not DATAP: %c%c%c%c%c",
	  x[0],x[1],x[2],x[3],x[4]);
      return -1;
    }


    datap->len = sumdatap->len;
    datap->time = sumdatap->time;
    datap->seq = sumdatap->seq;
    datap->trg_word = sumdatap->trg_word;
    datap->trg_in_word = sumdatap->trg_in_word;
    datap->detector = sumdatap->detector;
    memcpy(datap->TRG_L1_summary, sumdatap->TRG_L1_summary, sizeof(datap->TRG_L1_summary));
    memcpy(datap->TRG_L2_summary, sumdatap->TRG_L2_summary, sizeof(datap->TRG_L2_summary));
    memcpy(datap->L3_Summary, sumdatap->L3_Summary, sizeof(datap->L3_Summary));
    memcpy(&datap->evtdes, &sumdatap->evtdes, sizeof(datap->evtdes));

    return 0;
  }

  int daqReader::reconnect(void)
  {
    int ret ;
    int retries ;
    ic_msg msg ;

    if(evpDesc != -1) close(evpDesc);

    evpDesc = -1 ;	// mark as disconnected

    retries = 0 ;

    for(;;) {	// until success...

      evpDesc = msgNQCreate(EVP_HOSTNAME,EVP_PORT,120) ;

      if(evpDesc < 0) {
	LOG(ERR,"Can't create connection to %s:%d [%s] - will retry...",EVP_HOSTNAME,EVP_PORT,
	    strerror(errno),0,0) ;
	fprintf(stderr,"CRITICAL: Can't create connection to %s:%d [%s] - will retry...\n",EVP_HOSTNAME,EVP_PORT,
		strerror(errno)) ;
	sleep(1) ;
	return -1;
      }


      if(retries) {
	LOG(WARN,"Connection suceeded!",0,0,0,0,0) ;
      }


      LOG(DBG,"Opened connection to %s, port %d on descriptor %d",EVP_HOSTNAME, EVP_PORT,evpDesc,0,0) ;

      msg.head.daq_cmd = RTS_ETHDOOR_ANNOUNCE ;
      msg.head.status = 0 ;
      msg.ld.dword[0] = htonl(getpid()) ;

      char *user ;

      struct passwd *passwd = getpwuid(getuid()) ;
      if(passwd == NULL) {
	LOG(WARN,"User doesn't exist?",0,0,0,0,0) ;
	user = "????" ;
      }
      else {
	user = passwd->pw_name ;
      }


      strncpy((char *)&msg.ld.dword[1],user,12) ;
      strncpy((char *)&msg.ld.dword[4],getCommand(),12) ;
      msg.head.valid_words = 1+7 ;

#define BABABA
#ifdef BABABA
      {
	int jj ;
	int *intp = (int *) &msg ;
	for(jj=0;jj<3;jj++) {
	  *(intp+jj) = htonl(*(intp+jj)) ;
	}
      }
#endif

      ret = msgNQSend(evpDesc,(char *)&msg,120,60) ;
      if(ret < 0) {
	LOG(ERR,"Can't send data to %s! - will reconnect...",EVP_HOSTNAME,0,0,0,0) ;
	msgNQDelete(evpDesc) ;
	evpDesc = -1 ;
	continue ;
	//return ;
      }

      // all OK...
      status = EVP_STAT_OK ;
      LOG(DBG,"Returning to caller, status %d",status,0,0,0,0) ;
      break  ;	// that's it....
    }




    return 0 ;
  }

  static int ask(int desc, ic_msg *m)
    {

      int ret ;
      time_t tm ;
      int jj ;
      int *intp = (int *) m ;

      m->head.daq_cmd = EVP_REQ_EVENT ;
      m->head.status = STAT_OK ;
      m->head.dst_task = EVP_TASK ;
      m->head.valid_words = 1+1 ;	// reserve one for type...
      m->head.source_id = EVP_NODE ;
      m->head.src_task = EVP_TASK_READER ;

      LOG(DBG,"Sending request to EVP_TASK",0,0,0,0,0) ;
      tm = time(NULL) ;


      for(jj=0;jj<3;jj++) {
	*(intp+jj) = htonl(*(intp+jj)) ;
      }

      ret = msgNQSend(desc, (char *)m, 120,10) ;

      LOG(DBG,"msgNQSend returned %d in %d seconds",ret,time(NULL)-tm,0,0,0) ;
      
      if(ret < 0) {	// communication error
	  return STAT_ERROR ;
      }
      else {	// OK
	return STAT_OK ;
      }
    }


  static const char *getCommand(void)
    {


      static const char *str = "(no-name)" ;
#if defined(__linux__) || defined(__APPLE__)
      FILE *file ;
      static char name[128] ;
      int dummy ;

      file = fopen("/proc/self/stat","r") ;
      if(file==NULL) return str ;

      fscanf(file,"%d %s",&dummy,name) ;
      fclose(file) ;
      *(name+strlen(name)-1) = 0 ;
      return name+1 ;
#else   // solaris
      int fd, ret ;
      static struct psinfo ps ;

      fd = open("/proc/self/psinfo",O_RDONLY,0666) ;
      if(fd < 0) return str ;

      ret = read(fd,(char *)&ps,sizeof(ps)) ;
      close(fd) ;

      if(ret != sizeof(ps)) {
	return str ;
      }

      return ps.pr_fname ;
#endif
    }





  // jml 12/28/07
  //
  // Starts at the 2008 datap, and searches
  // for the "legacy" file in the sfs format...
  // then grabs DATAP from there...

  // This code is NOT to be taken as the "right" way to do anything :-)
  // in general it is not good to ignore the length fields in the various
  // banks...   I make use of the idea that for all affected data files
  // LOGREC and DATAP have the proper sizes...

  // copy of SFS format header, as this hack and I don't want this dependent 
  // on sfs includes
  struct copy_SFS_File {
    char type[4];     // "FILE"
    UINT32 byte_order;
    UINT32 sz;        // any number, but file will be padded to be x4
    UINT8 head_sz;    // must be x4
    UINT8 attr;
    UINT16 reserved;
    char name[4];     // get rid of padding confusions... by alligning
  };


  DATAP *getlegacydatap(char *mem, int bytes) 
    {
      int off = 0;
      char *curr = mem;
  
      LOG(DBG, "off = %d bytes = %d", off, bytes);
      while(off < bytes) {
	// skip many types of banks...

	if(memcmp(curr, "LRHD", 4) == 0) {
	  LOG(DBG, "hop over LRHD");
	  curr += sizeof(LOGREC);
	  off += sizeof(LOGREC);
	}
	else if(memcmp(curr, "DATAP", 5) == 0) {
	  LOG(DBG, "hop over DATAP");
	  curr += sizeof(DATAP);
	  off += sizeof(DATAP);
	}
	else if(memcmp(curr, "SFS", 3) == 0) {
	  LOG(DBG, "hop over SFS volume spec");
	  curr += 12;
	  off += 12;
	}
	else if(memcmp(curr, "HEAD", 4) == 0) {
	  LOG(DBG, "hop over SFS header");
	  curr += 12;
	  off += 12;
	}
	else if(memcmp(curr, "FILE", 4) == 0) {   // sfs bank...
	  copy_SFS_File *file = (copy_SFS_File *)curr;
      
	  int swap = (file->byte_order == 0x04030201) ? 0 : 1;

	  if(strstr(file->name, "legacy")) {
	    LOG(DBG, "Found legacy file");
	    off += file->head_sz;
	    curr += file->head_sz;
	
	    if(memcmp(curr, "DATAP", 5) != 0) {
	      LOG(ERR, "Got to legacy file, but not DATAP? is %c%c%c%c%c",
		  curr[0],curr[1],curr[2],curr[3],curr[4]);
	      return NULL;
	    }

	    return (DATAP *)curr;
	  }
	  else {
	    LOG(DBG, "hop over SFS File (%s)", file->name);
	    off += file->head_sz + ((qswap32(swap, file->sz)+3) & 0xfffffffc);
	    curr += file->head_sz + ((qswap32(swap, file->sz)+3) & 0xfffffffc);
	  }
	}
	else {
	  LOG(DBG, "There is no legacy datap");
	  return NULL;
	}
      }

      LOG(ERR, "no legacy datap");
      return NULL;
    }

int daqReader::getStatusBasedEventDelay()
{

    int delay = 0;

    switch(status) {
    case EVP_STAT_EVT :	// something wrong with last event...
	delay = 100000;
	LOG(DBG, "Delaying for %d usec because of error on last event",delay);
	break ;
    case EVP_STAT_EOR :	// EndOfRun was the last status and yet we are asked again...
	delay = 100000;
	LOG(DBG, "Delaying for %d usec because last event was end of run",delay);
	break ;
    case EVP_STAT_CRIT :
	delay = 1000000;
	LOG(ERR, "Delaying for %d usec because last event had critical status",delay);

	crit_cou++;
	if(crit_cou > 10) {
	    LOG(ERR,"That's IT! Bye...",0,0,0,0,0);
	    sleep(1) ;	// linger...
	    exit(-1) ;
	}
    
    default :	// all OK...
	break ;
    }
  
    if(status != EVP_STAT_CRIT) crit_cou = 0;
  
    return delay;
}


int daqReader::writeCurrentEventToDisk(char *ofilename)
{
    int fdo;
    int ret;

    if(memmap->mem == NULL) {
	LOG(ERR, "Can't write current event.  No event");
	return -1;
    }

  
    fdo = open(ofilename, O_APPEND | O_WRONLY | O_CREAT, 0666);
    if(fdo < 0) {  
	LOG(ERR, "Error  opening output file %s (%s)", ofilename, strerror(errno));
	return -1;
    }

    ret = write(fdo, memmap->mem, event_size);
    if(ret != event_size) {
	LOG(ERR, "Error writing event data (%s)",strerror(errno));
	close(fdo);
	return -1;
    }

    close(fdo);
    return 0;
}

MemMap::MemMap()
{
    mem=NULL; 
    actual_mem_start=NULL; 
    actual_size=0; 
    fd = -1;
    real_mem = 0;
    page_size = sysconf(_SC_PAGESIZE);
}

MemMap::~MemMap()
{
    unmap();    
}

char *MemMap::map_real_mem(char *buffer, int _size)
{
    offset = 0;
    size = _size;
    fd = 0;
    real_mem = 1;
    mem = buffer;
    return mem;
}

void daqReader::setCopyOnWriteMapping() {
    map_prot = PROT_READ | PROT_WRITE;
    map_flags = MAP_PRIVATE | MAP_NORESERVE;
}

char *MemMap::map(int _fd, long long int _offset, int _size, int map_prot, int map_flags)
{
    offset = _offset;
    size = _size;
    fd = _fd;

    LOG(DBG, "Calling mmap fd=%d offset=%d size=%d",
	_fd, _offset, _size);

    int excess = offset % page_size;
    actual_offset = offset - excess;
    actual_size = size + excess;                 // actual size need not be a multiple of pagesize...

    LOG(DBG, "       mmap excess=%d      aoffset=%d asize=%d",
	excess, actual_offset, actual_size);

    actual_mem_start = (char *) mmap64(NULL, actual_size,  map_prot, map_flags, fd, actual_offset) ;
    madvise(actual_mem_start, actual_size, MADV_SEQUENTIAL);
  
    if(((void *)actual_mem_start) == MAP_FAILED) {
	LOG(ERR,"Error in mmap (%s)",strerror(errno),0,0,0,0) ;

	mem=NULL;
	offset=0;
	size=0;
	actual_offset=0;
	actual_mem_start=NULL;
	actual_size=0;
	return NULL;
    }

    mem = actual_mem_start + excess;
    return mem;
}

  void MemMap::unmap()
  {
    if(mem==NULL) return;

    if(real_mem) {
	real_mem = 0;
    }
    else {
	madvise(actual_mem_start, actual_size, MADV_DONTNEED);
	munmap(actual_mem_start, actual_size);
    }

    mem=NULL;
    offset=0;
    size=0;
    actual_offset=0;
    actual_mem_start=NULL;
    actual_size=0;
  }

  // Must be reading from daq file!
  // Must have a current event by calling a previous "get()"
  // Fills the EvtHeader with the "next" events header
  //
  // return 0 if next event end of file
  // return -1 if other error
  int daqReader::readNextFutureSummaryInfo(SummaryInfo *info)
  {
    memset(info, 0, sizeof(SummaryInfo));

    if(input_type != file) {
      LOG(ERR, "Can't read next future evt header unless reading from daq file");
      return -1;
    }

    if(sfs->singleDirMount == 0) {
      LOG(ERR, "Need a current file to read the next one...");
      return -1;
    }

    LOG(DBG, "(sfs) singleDirOffset = %d   singleDirSize = %d", sfs->singleDirOffset, sfs->singleDirSize);

    long long int offset = sfs->singleDirOffset + sfs->singleDirSize;
 
    sfs_index *nsfs = new sfs_index();
    if(!nsfs) {
      LOG(ERR, "Couldn't create sfs_index");
      return -1;
    }

    LOG(DBG, "mounting dir at offset %s:%d",file_name,offset);
    int ret = nsfs->mountSingleDir(file_name, offset);
    if(ret < 0) {
      LOG("ERR", "Error mounting dir at offset %s:%d",file_name,offset);

      delete nsfs;
      return ret;
    }
    if(ret == 0) {
      LOG(DBG, "End of file reading next dir...");
      delete nsfs;
      return ret;
    }

    LOG(DBG, "Mounted dir (nsfs)  off: %d  sz: %d",nsfs->singleDirOffset, nsfs->singleDirSize);


    // Got to open this myself xxxxxxxxxxx
    int fd = open(file_name, O_RDONLY);
    if(fd <=0) {
      LOG(ERR, "No defined file descriptor");
      delete nsfs;
      return -1;
    }
  
    MemMap *nmem = new MemMap();
    char *mymem = nmem->map(fd, nsfs->singleDirOffset, nsfs->singleDirSize, map_prot, map_flags);
    if(!mymem) {
      LOG(ERR, "Couldn't map memory");
      delete nmem;
      delete nsfs;
      close(fd);
      return -1;
    }

    LOG(DBG, "mapped off=%d sz=%d into 0x%x",nsfs->singleDirOffset, nsfs->singleDirSize, mymem);

    fs_dir *rootdir = nsfs->opendir("/");
    for(;;) {
      fs_dirent *ent = nsfs->readdir(rootdir);
      if(!ent) {
	nsfs->closedir(rootdir);
	LOG(ERR, "Error finding event directory in sfs?");

	// Skip directory... go to next
	nsfs->closedir(rootdir);
	delete nmem;
	delete nsfs;
	close(fd);
	return -1;
      }

      if(memcmp(ent->full_name, "/#", 2) == 0) {
	info->seq = atoi(&ent->full_name[2]);

	nsfs->cd(ent->full_name);
	nsfs->closedir(rootdir);
	break;
      }

      if(allnumeric(&ent->full_name[1])) {
	info->seq = atoi(&ent->full_name[1]);
	nsfs->cd(ent->full_name);
	nsfs->closedir(rootdir);
	break;
      }
    
      LOG(DBG, "SFS event directory not yet found: %s",ent->full_name);
    }

    fs_dirent *summary = nsfs->opendirent("EventSummary");
    if(summary) {

      // recal that the memory is mounted from the beginning of the event
      // while the offset here is from the beginning of the file
      int mem_offset = summary->offset-nsfs->singleDirOffset;
      LOG(DBG, "found summary %d:  file[%d-%d] sz=%d corr=%d)",summary->offset,nsfs->singleDirOffset, nsfs->singleDirOffset+nsfs->singleDirSize,nsfs->singleDirSize, summary->offset-nsfs->singleDirOffset);
  
      char *buff = mymem + mem_offset;
      fillSummaryInfo(info,(gbPayload *)buff);
    }
    else { // take it from datap
      LOG(NOTE, "No EventSummary, search for legacy datap");
      summary = nsfs->opendirent("legacy");
      if(!summary) {
	LOG(NOTE, "No EventSummary and no DATAP... hacking summary info");
      }
      else {
	long long int mem_offset = summary->offset - nsfs->singleDirOffset;
	char *buff = mymem + mem_offset;
	fillSummaryInfo(info,(DATAP *)buff);
	copySummaryInfoIn(info);
      }
    }

    delete nsfs;
    delete nmem;
    close(fd);

    return 1;
  }


