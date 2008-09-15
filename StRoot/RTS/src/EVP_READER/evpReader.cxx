#include <stdio.h>
#include <sys/types.h>
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

#ifdef __linux__
#include <sched.h>
// some older Linuxes (notably 6.2) crash and burn
// because of not having madvise so...
// Redhat EL3 crashes the kernel! with madvise... christ...
#define madvise(x,y,z)

#else
#include <procfs.h>
#endif

// MUST BE INCLUDED BEFORE ANY OTHER RTS INCLUDE!
#include <evpReader.hh>

#include <rtsLog.h>
#include <daqFormats.h>
#include <iccp.h>
#include <msgNQLib.h>



#ifndef MADV_DONTNEED
#define madvise(x,y,z)
#endif


u_int evp_daqbits ;



static int parseLRHD(struct LOGREC *lr) ;
static int evtwait(int task, ic_msg *m) ;
static int ask(int desc, ic_msg *m) ;
DATAP *getlegacydatap(char *mem, int bytes);

//static int thisNode = EVP_NODE ;
//static int evpDesc ;
static char *getCommand(void) ;




// CONSTRUCTOR!
evpReader::evpReader(char *name) 
{

//	printf("-%s-\n",'EVP_VERSION') ;

//	rtsLogLevel(WARN) ;
//	rtsLogOutput(RTS_LOG_NET) ;
#ifdef RTS_PROJECT_PP
	rtsLogAddDest("130.199.91.18",RTS_LOG_PORT_READER) ;
#else
	rtsLogAddDest("130.199.60.86",RTS_LOG_PORT_READER) ;	// reader.log to daqman
#endif

	sfs = NULL;
	sfs_lastevt = 0;
	sumdatap = NULL;

	memset(&runconfig,0,sizeof(runconfig));

	rts_rr = 0 ;


	// create the empty sfs object
	sfs = new sfs_index();

	rts_rr = new rts_reader("RR_evp") ;	// call myself for debugging
	rts_rr->enable("*") ;			// enable all dets
	rts_rr->add_input(sfs) ;		// my intput is the above sfs class

	if(name == NULL) {	// use the ACTIVE run...
		LOG(DBG,"Constructor for the ACTIVE run...",0,0,0,0,0) ;

	}
	else {
		strncpy(fname,name,sizeof(fname)-1) ;
		LOG(DBG,"Constructor %s",(uint)fname,0,0,0,0) ;
	}

	// setup...
	do_open = 1 ;
	do_mmap = 1 ;
	strcpy(evp_disk,"") ;



	desc = -1 ;
	mem_mapped = NULL ;
	bytes_mapped = 0 ;

	// need this for mmap....
	page_size = sysconf(_SC_PAGESIZE) ;

	issued = 0 ;
	last_issued = time(NULL) ;
	status = EVP_STAT_CRIT ;
	tot_bytes = 0 ;
	isdir = 0 ;	// not a directory by default...
	isfile = 0 ;	// not even a standalone file
	isevp = 0 ;	// not even the current run???

	event_number = 0 ;	// last known event...
	total_events = 0 ;
	readall_reset();
	bytes = 0 ;

	mem = NULL ;



	file_size = 0 ;		// only if it's a real big file...


	if(name == NULL) {	// EVP
		fname[0] = 0 ;	// mark as "from pool"
		reconnect() ;	// will loop until success or Cntrl-C I guess...
		return ;	// that's it....
	}



	// This code is reached only if the argument was non-NULL
	// file or directory?

	if(stat(fname, &stat_buf) < 0) {	// error
		LOG(ERR,"Can't stat \"%s\" [%s]",(int)fname,(int)strerror(errno),0,0,0) ;
//		fprintf(stderr,"CRITICAL: Can't stat \"%s\" [%s]\n",fname,strerror(errno)) ;
		sleep(1) ;
		return ;
	}

	
	LOG(INFO,"Using file \"%s\"...",(int)fname,0,0,0,0) ;

	// directory?
	if(stat_buf.st_mode & S_IFDIR) {
		LOG(DBG,"Running through a directory %s...",(uint)fname,0,0,0,0) ;

		// change to that directory...
		if(chdir(fname) < 0) {
			LOG(ERR,"Can't chdir %s [%s]",(int)fname,(int)strerror(errno),0,0,0) ;
			sleep(1) ;
			return ;
		}

		status = EVP_STAT_OK ;
		isdir = 1 ;
		return ;
	}	
	
	// If not, it must be a file...
	file_size = stat_buf.st_size ;
       	isfile = 1 ;	// must be a file...
	strcpy(file_name,fname) ;

	// descriptor for ".daq" view of file...
	LOG(DBG,"Running through a file %s of %d bytes",(int)fname,file_size,0,0,0) ;
	
	desc = open(fname,O_RDONLY,0444) ;
	if(desc < 0) {	// error
	  LOG(ERR,"Can't open %s [%s]",(int)fname,(int)strerror(errno),0,0,0) ;
	  sleep(1) ;
	  return ;
	}
	 	  
	// all OK
	//	rts_rr = new rts_reader("RR_evp") ;	// call myself for debugging
	//	rts_rr->enable("*") ;			// enable all dets
	//	rts_rr->add_input(sfs) ;		// my intput is the above sfs class

	status = EVP_STAT_OK ;
	  
	return ;
}

int evpReader::setOpen(int flg)
{	
	int ret ;

	ret = do_open ;
	do_open = flg ;

	return ret ;
}

int evpReader::setMmap(int flg)
{	
	int ret ;

	ret = do_mmap ;
	do_mmap = flg ;

	return ret ;
}

int evpReader::setLog(int flg)
{	

	if(flg) {
		rtsLogOutput(RTS_LOG_STDERR|RTS_LOG_NET) ;
	}
	else {
		rtsLogOutput(RTS_LOG_NET) ;
	}

	return 0 ;
}


char *evpReader::setEvpDisk(char *name)
{
	static char saved[256] ;

	strcpy(saved, evp_disk) ;
	strncpy(evp_disk,name,sizeof(evp_disk)-1) ;

	return saved ;
}

evpReader::~evpReader(void)
{
	LOG(DBG,"Destructor %s",(uint)fname,0,0,0,0) ;

	// clean-up input file...
	if(desc >= 0) close(desc) ;

	// clean up mem mapped stuff...
	if(mem_mapped && bytes_mapped) {
		madvise(mem_mapped, bytes_mapped, MADV_DONTNEED) ;
		munmap(mem_mapped,bytes_mapped) ;
	}
	
	// clean up input queues
	if(isevp) {
		msgNQDelete(evpDesc) ;
	}

	// Tonko -- added...
	if(sfs) delete(sfs) ;
	if(rts_rr) delete(rts_rr) ;

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
char *evpReader::get(int num, int type)
{
	static int crit_cou = 30 ;
	static char evname[256] ;
	static struct LOGREC lrhd ;
	int ret ;
	int data_bytes ;
	int leftover, tmp ;
	u_int offset ;
	char _evp_basedir_[40];
	static char _last_evp_dir_[40];

	sumdatap = NULL;

	LOG(DBG,"get event %d of type %d; file %d, dir %d, evp %d",num,type,isfile,isdir,isevp) ;

	// nix some variables which may have no meaning for all streams...
	evb_type = 0 ;
	evb_type_cou = 0 ;
	evb_cou = 0 ;
	run = 0 ;

	// check status of constructor or previous get()

	switch(status) {
	case EVP_STAT_EVT :	// something wrong with last event...
		usleep(500000) ;
		break ;
	case EVP_STAT_EOR :	// EndOfRun was the last status and yet we are asked again...
		usleep(500000) ;
		break ;
	case EVP_STAT_CRIT :
		if(crit_cou==30) LOG(WARN,"Previous error is unrecoverable! Why are you asking me again?",0,0,0,0,0) ;
		sleep(1) ;
		crit_cou-- ;
		if(crit_cou < 10) {
			if(crit_cou==0) {
				LOG(ERR,"CRITICAL ERROR: That's IT! Bye...",0,0,0,0,0);
				sleep(1) ;	// linger...
				exit(-1) ;
			}
			LOG(ERR,"Anybody there??? Exiting in %2d seconds...",crit_cou,0,0,0,0) ;
		}
		return NULL ;

	default :	// all OK...
		break ;
	}

	// unmap previous
	if(mem_mapped && bytes_mapped) {
		LOG(DBG,"Unmapping previous 0x%X, bytes %d",(uint)mem_mapped,bytes_mapped,0,0,0) ;
		madvise(mem_mapped, bytes_mapped, MADV_DONTNEED) ;
		ret = munmap(mem_mapped,bytes_mapped) ;
		mem_mapped = NULL ;
		bytes_mapped = 0 ;

		if(ret < 0) {
			LOG(ERR,"Error in munmap (%s)",(int)strerror(errno),0,0,0,0) ;
			status = EVP_STAT_CRIT ;
			return NULL ;
		}
	}

	
	if(isdir) {	// we are running in a directory and we are already chdir-ed in the constructor

		if((event_number != 0) && (token == 0)) {	// we read at least one event and it was token==0 thus this is it...
			LOG(NOTE,"Previous event was Token 0 in directory - stopping...",0,0,0,0,0) ;
			status = EVP_STAT_EOR ;
			return NULL ;
		}

		if(num==0) {	// next event
			num = ++event_number ;	// advance event counter

			int max_num = num+10 ;

			ret = 0 ;
			for(;num<max_num;num++) {
				sprintf(evname,"%d",num) ;	// make a file name...
				sprintf(file_name,"%s/%d",fname,num) ;

				errno = 0 ;
				ret = stat(evname,&stat_buf) ;
				if(ret == 0) break ;	// file OK
				else {
					LOG(WARN,"File %s has problems [%s]...",(int)evname,(int)strerror(errno),0,0,0) ;
				}
			}

			if(ret) {	// no success, report end-of0run
				LOG(WARN,"Couldn't find events around this %u... Returning EOR.",max_num,0,0,0,0) ;
				status = EVP_STAT_EOR ;
				return NULL ;
			}

		}
		else {

			sprintf(evname,"%d",num) ;	// make a file name...
			sprintf(file_name,"%s/%d",fname,num) ;

		}

		event_number = num ;

	}
	else if(isevp) {	// ACTIVE RUN
		static ic_msg m ;


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
			ret = ask(evpDesc,&m) ;
			if(ret != STAT_OK) {	// some error...

				LOG(ERR,"Queue error %d - recheck EVP...",ret,0,0,0,0) ;
				reconnect() ;
				status = EVP_STAT_EVT ;	
				return NULL ;
			}
			issued = 1 ;
			last_issued = time(NULL) ;
		}
		 

		// wait for the event (unless run is finished but still checking)
		if((num == 0) || !readall_rundone) {
		  int timedout=0;
		  ret = evtwait(evpDesc, &m) ;

		  // got some reply here so nix issued
		  issued = 0;

		  // If queues are broken we have problems...	    
		  if((ret != STAT_OK) && (ret != STAT_TIMED_OUT)) {
		    reconnect() ;
		    LOG(ERR,"Queue error %d - recheck EVP...",ret,0,0,0,0) ;
		    status = EVP_STAT_EVT ;	// treat is as temporary...
		    return NULL ;
		  }

		  if(ret == STAT_TIMED_OUT) {	// retry ...
		    timedout = 1;
		    if((num == 0) ||
		       (num > int(readall_lastevt))) {
#ifdef __linux
		      sched_yield() ;
#else
		      yield() ;		// give up time slice?
#endif

		      LOG(DBG, "Waiting 1 second, no event yet...");
		      usleep(1000) ;		// 1 ms?
		      status = EVP_STAT_OK ;	// no error...
		      return NULL ;		// but also noevent - only on wait!
		    }
		    
		  }

		  LOG(DBG, "m.head.status = %d",m.head.status);

		  // check misc. 

		  switch(m.head.status) {
		  case STAT_SEQ:
		    {	// end of run!
		      
		      LOG(DBG,"End of Run!",0,0,0,0,0) ;
		      
		      if(num == 0) {
			status = EVP_STAT_EOR ;
			return NULL;
		      }
		      else {
			if(readall_lastevt != 0) {
			  readall_rundone = 1;
			}
		      }
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
		      if((num ==0) || !timedout) {
			LOG(WARN,"Event in error - not stored...",0,0,0,0,0) ;
			status = EVP_STAT_EVT ;			// repeat
			return NULL ;
		      }
		    }
		    break;
		  }
		}
		

		// A few last checks...
		if(num != 0) {

		  event_number = num;

		  if(readall_lastevt == 0) {
		    LOG(DBG, "readall but don't have any events rundone=%d run=%d",readall_rundone,run);
		    //    readall_reset();
		    sleep(1);
		    status = EVP_STAT_OK;
		    return NULL;
		  }

		  if(readall_lastevt < event_number) {
		    LOG(WARN, "requesting an event that hasn't arrived rundone=%d run=%d",readall_rundone, run);
		    sleep(1);
		    status = EVP_STAT_OK;
		    return NULL;
		  }
		}
		      
		sprintf(evname,"%s/%d",_last_evp_dir_, event_number) ;

		strcpy(file_name,evname) ;

		struct stat s;
		if(stat(file_name, &s) < 0) {
		  LOG(DBG, "No file %s, try _DELETE",file_name);
		  sprintf(file_name,"%s_DELETE/%d",_last_evp_dir_,event_number);
		  strcpy(evname,file_name);

		  LOG(DBG, "new file %s",file_name);
		}
		
		LOG(DBG,"Returned event - write it to -%s -",(int)evname,0,0,0,0) ;

	}

	LOG(DBG, "isevp=%d isdir=%d",isevp,isdir,0,0,0);

	// This code gets executed if the file contains just one event
	// because we are going through a directory
	// or else reading via the live event pool...
	//
	// At the end...
	// filesize is set from stat()
	// bytes, total bytes are set to 0
	// desc is opened, but remains at the start of the file...
	// sfs contains the mounted sfs object
	//
	if(isevp || isdir) {
		// single file - full name in evname!
		if(desc > 0) {
			close(desc) ;
			desc = -1 ;
		}

		errno = 0 ;

		if(do_open) {

		  desc = open(evname,O_RDONLY,0666) ;
		  if(desc < 0) {	
		    LOG(ERR,"Error opening file %s [%s] - skipping...",(int)evname,(int)strerror(errno),0,0,0) ;
		    status = EVP_STAT_EVT ;
		    return NULL ; 
		  }

		  // get the file_size ;
		  ret = stat(evname,&stat_buf) ;
		  if(ret < 0) {	//
		    LOG(ERR,"Can't stat %s",(int)evname,0,0,0,0) ;
		    status = EVP_STAT_EVT ;
		    close(desc) ;
		    desc = -1 ;
		    return NULL ;
		  }


		  file_size = stat_buf.st_size ;
		  tot_bytes = 0 ;
		  bytes = 0 ;
		}
		else {
		  file_size = 0 ;
		  tot_bytes = 0 ;
		  bytes = 0 ;
		  status = EVP_STAT_OK ;
		  total_events++ ;
		  return NULL ;
		}
	}
		
	// at this point the file is opened...
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
	if(isfile) event_number++ ;
	


	// Our goal here is to find:
	//   1. The pointer to DATAP.  This will eventually be mapped (where desc points at end)
	//   2. The size to be mapped from DATAP to the end of the legacy data (data_bytes)
	//   3. The run number  (run)
	//

	int badparselrhds = 0;
	do {
	       evt_offset_in_file = lseek(desc, 0, SEEK_CUR);
	       offset = evt_offset_in_file;
	       if(evt_offset_in_file < 0) {
		 LOG(ERR, "Error getting event offset...");
	       }

	       LOG(DBG, "Event number %d starting offset (%d)",
		   event_number, evt_offset_in_file);

	       // read the LRHD first
	       errno = 0 ;
	       ret = read(desc,(char *)&lrhd, sizeof(lrhd)) ;
	       if(ret != sizeof(lrhd)) {	// error
			mem = NULL ;

			if(ret == 0) {	// EOF
				LOG(NOTE,"EOF reading LRHD...",0,0,0,0,0) ;
			}
			else {
				LOG(ERR,"Error reading LRHD [%s], desc %d, got %d, expect %d",(int)strerror(errno),desc,ret,sizeof(lrhd),0) ;
			}


			if(isfile) {
				status = EVP_STAT_EOR ;
			}
			else {
				status = EVP_STAT_EVT ;
			}

			return NULL ;
		}

		tot_bytes += ret ;

		// this also swaps the data...
		data_bytes = parseLRHD(&lrhd) ;

		if(data_bytes == -1) {	// error
		  // try a few advances of 8192 bytes...
		  if(badparselrhds > 5) {
		    LOG(ERR,"Bad LRHD - skipping...",0,0,0,0,0) ;
		    status = EVP_STAT_EVT ;
		    return NULL ;
		  }
		  else {
		    LOG(WARN, "Correcting for extra padding bug #2, should be fixed as of 1/10/2008");

		    badparselrhds++;
		    lseek(desc, -sizeof(lrhd), SEEK_CUR);
		    tot_bytes -= sizeof(lrhd);

		    lseek(desc, 8192, SEEK_CUR);
		    tot_bytes += 8192;
		    data_bytes = 0;
		  }
		}
		else if((data_bytes == -2) && (isdir || isfile)) {	// ENDR in LRHD
			LOG(NOTE,"ENDR record in LRHD in directory - ending run...",0,0,0,0,0) ;
			status = EVP_STAT_EOR ;
			return NULL ;
		}
		else if (data_bytes == -3) { //just DATAP
		  // No lrhd...
		  lseek(desc, -sizeof(lrhd), SEEK_CUR);
		  
		  DATAP _datap;
		  ret = read(desc, &_datap, sizeof(DATAP));
		  if(ret != sizeof(DATAP)) {
		    LOG(ERR, "Bad DATAP - skipping...",0,0,0,0,0);
		    status = EVP_STAT_EVT;
		    return NULL;
		  }

		  lseek(desc, -sizeof(DATAP), SEEK_CUR);
		  data_bytes = swap32(_datap.len) * 4;

		  tot_bytes -= sizeof(lrhd);
		}
		else if (data_bytes == -4) { // a FILE rec
		  // why?  Probably because of the padding bug
		  // evts aligned on 8192 get a 8192 padding record
		  // despite the fact that the LRHD doesn't account for 
		  // the extra padding...
		  // bug fixed about 1/10/2008, but need to skip anyhow...

		  LOG(WARN, "Correcting for extra padding bug #1, should be fixed as of 1/10/2008...");
		  lseek(desc, -sizeof(lrhd), SEEK_CUR);
		  tot_bytes -= sizeof(lrhd);

		  SFS_File sfsfile;
		  ret = read(desc, &sfsfile, sizeof(sfsfile));
		  if(ret != sizeof(sfsfile)) {
		    LOG(ERR, "Bad FILE - skipping...");
		    status = EVP_STAT_EVT;
		    return NULL;
		  }

		  
		  int skip = seeksize(sfsfile.sz) + sfsfile.head_sz - sizeof(sfsfile);
		  sfsfile.name[4] = 0;
		  LOG(DBG, "Skip %s...: %d bytes %d %d",
		      sfsfile.name,skip,
		      sfsfile.sz,sfsfile.head_sz);

		  lseek(desc, skip, SEEK_CUR);

		  tot_bytes += sizeof(sfsfile);
		  tot_bytes += skip;
		  data_bytes = 0;
		}

	} while(data_bytes == 0) ;	// until we get to the DATA...


	evt_datap_offset_in_file = lseek(desc, 0, SEEK_CUR);

	LOG(DBG, "startoffset = %d DATAP offset = %d",evt_offset_in_file, evt_datap_offset_in_file);

	if(isevp) {
	  if((num != 0) && (run == 0)) {
	    run = readall_run;
	  }

	  if(run != lrhd.lh.run) {
	    LOG(ERR,"Run number mismatch: EVP %d vs. data %d",run,lrhd.lh.run,0,0,0) ;
	  }
	}

	run = lrhd.lh.run ;		// set the run number to the one in data!

	// at this point we just read the LRHD corresponding to DATA
	// and we are positioned at DATAP....


	// map the event...
	// ...but we must do %^&*()_ acrobatics because the offset must be alligned on a page
	// boundary arrghhhhhhhhhhhhhhh
	tmp = tot_bytes / page_size ;
	leftover = tot_bytes % page_size ;

	offset = page_size * tmp ;
	bytes_mapped = data_bytes + leftover ;
	

	LOG(DBG, "Event number %d offset in file 0x%x(%d)",
	    event_number, evt_offset_in_file,
	    evt_offset_in_file);


	// sanity check for reads after EOF
	if((offset + bytes_mapped) > file_size) {

		if(isfile) {
			LOG(WARN,"This event is truncated... Good events %d [%d+%d > %d]...", total_events,offset,bytes_mapped,file_size,0) ;
			status = EVP_STAT_EOR ;
		}
		else {
			LOG(ERR,"This event is truncated... Good events %d [%d+%d > %d]...", total_events,offset,bytes_mapped,file_size,0) ;
			status = EVP_STAT_EVT ;
		}

		return NULL ;
	}

	LOG("DBG", "about to map");
	if(do_mmap) {
		mem = (char *) mmap(NULL, bytes_mapped, PROT_READ,MAP_SHARED|MAP_NORESERVE,desc,offset) ;
		if(((void *) mem) == MAP_FAILED) {
			LOG(ERR,"Error in mmap (%s)",(int)strerror(errno),0,0,0,0) ;
			status = EVP_STAT_CRIT ;
			mem = NULL ;
			mem_mapped = NULL ;

			close(desc) ;
			desc = -1 ;

			return NULL ;
		}

		// need to save this for munmap
		mem_mapped = mem ;

		// the memory will most likely be used only once... hmmm...
		madvise(mem_mapped, bytes_mapped, MADV_SEQUENTIAL) ;
		

		// adjust mem
		mem += leftover ;
	}
	else {
		mem_mapped = NULL ;
		bytes_mapped = 0 ;
		mem = NULL ;
	}

	LOG("DBG", "about to mount %s %d 0x%x",file_name, evt_offset_in_file, sfs);
	// Now, mount the sfs file...
	// The mount unmounts and closes previous mount...
	
	sfs->mountSingleDir(file_name, evt_offset_in_file);

	LOG("DBG", "done w mount");
	// at this point mem points to DATAP so let's see...
	struct DATAP *datap = (struct DATAP *) mem ;

	if(checkBank(datap->bh.bank_type,"DATAP") < 0) {	// something very wrong!
		status = EVP_STAT_EVT ;
		return NULL ;
	}

	// endianess mess
	// ext. system
	u_int off = datap->det[EXT_SYSTEM].off ;
	u_int len = datap->det[EXT_SYSTEM].len ;

	token = datap->bh.token ;
	//	printf("token = %d\n",swap32(token));
	
	trgword = datap->trg_word ;
	evt_time = datap->time ;	// UNIX time
	detectors = datap->detector ;
	seq = datap->seq ;


	// summaries need a hack for a bug present 
	// oct to dec 2007
	//sumdatap;  // which datap to use for summaries?

	DATAP *legdatap = getlegacydatap(mem, data_bytes) ;
	if(legdatap) {
	  sumdatap = legdatap;
	}
	else {
	  sumdatap = datap;
	}

	daqbits_l1 = sumdatap->TRG_L1_summary[0];
	daqbits_l2 = sumdatap->TRG_L2_summary[0];
	daqbits = sumdatap->L3_Summary[0] ;
	evpgroups = sumdatap->L3_Summary[2] ;
	// end event summary hack...


	// swaps
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {	// swap

		off = swap32(off) ;
		len = swap32(len) ;

		token = swap32(token) ;
		trgword = swap32(trgword) ;
		evt_time = swap32(evt_time) ;
		detectors = swap32(detectors) ;
		seq = swap32(seq) ;
		daqbits_l1 = swap32(daqbits_l1);
		daqbits_l2 = swap32(daqbits_l2);
		daqbits = swap32(daqbits) ;
		evpgroups = swap32(evpgroups) ;

		trgcmd = (swap32(datap->trg_in_word) >> 12) & 0xF ;	// _just_ the trigger command
		daqcmd = (swap32(datap->trg_in_word) >> 8) & 0xF ;	// DAQ command

		for(int i=0;i<10;i++) {
		  LOG(DBG,"   detp %2d: len %d, off %d",i, swap32(datap->det[i].len),swap32(datap->det[i].off),0,0) ;
		}

	}
	else {

		trgcmd = ((datap->trg_in_word) >> 12) & 0xF ;	// _just_ the trigger command
		daqcmd = ((datap->trg_in_word) >> 8) & 0xF ;	// DAQ command

	}

	evp_daqbits = daqbits ;


	if(len) {	// extended should be there
		struct DATAPX *dx ;

		dx = (struct DATAPX *)(mem + off*4) ;

		if(checkBank(dx->bh.bank_type,CHAR_DATAPX) < 0) {
			status = EVP_STAT_EVT ;
			return NULL ;
		}

		if(dx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) {	// swap

			int i ;
			for(i=0;i<22;i++) {
				LOG(DBG,"   detx %2d: len %d, off %d",i+10, swap32(dx->det[i].len),swap32(dx->det[i].off),0,0) ;
			}
		}
		else {
			int i ;
			for(i=0;i<22;i++) {
				LOG(DBG,"   detx %2d: len %d, off %d",i+10, (dx->det[i].len), (dx->det[i].off),0,0) ;
			}


		}
	}



	// all done - all OK
	status = EVP_STAT_OK ;
	bytes = data_bytes ;	// size of this event...
	tot_bytes += bytes ;
	total_events++ ;


	// move to next event although it may make no sense...
	ret = lseek(desc, bytes, SEEK_CUR) ;
	if(ret < 0) {
		LOG(ERR,"lseek error (%s)",(int)strerror(errno),0,0,0,0) ;
	}

	
	LOG(DBG,"Curr offset %d, bytes %d, tot_bytes %d, file size %d",ret,bytes,tot_bytes,file_size,0) ;


	// Now we want to make sure run info is up to date


	if(run != (unsigned int)runconfig.run) {
	  char rccnf_file[256];

	  runconfig.run = 0;   // set invalid to start...

	  if(isevp) {
	    sprintf(rccnf_file,"%s%s/%d/0",evp_disk,_evp_basedir_,run) ;
	  }
	  else if(isdir) {
	    sprintf(rccnf_file,"%s/0",fname);
	  }
	  else if(isfile) {
	    sprintf(rccnf_file,"rccnf_%d.txt",run);
	  }

	  if(getRccnf(rccnf_file, &runconfig) < 0) {
	    LOG("DBG", "No runconfig file %s",rccnf_file,0,0,0,0);
	  }

	  //printf("rccnf_file = %s %d %d\n",rccnf_file,run,runconfig.run);

	  if(runconfig.run == 0) {
	    detsinrun = 0xffffffff;
	    evpgroupsinrun = 0xffffffff;
	  }
	  else {
	    detsinrun = runconfig.detMask;
	    evpgroupsinrun = runconfig.grpMask;
	  }
	}


	// Tonko: before we return, call rr's Make
	rts_rr->Make() ;

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

char *evpReader::getSFSEventNumber()
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
    if(isfile) {
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


static int parseLRHD(struct LOGREC *lr)
{
  char *c = (char *) lr->lh.bank_type;
  
  LOG(DBG, "In parseLRHD: %c%c%c%c",c[0],c[1],c[2],(c[3] != 0) ? c[3] : ' ');

	if(memcmp(lr->lh.bank_type,"DATAP",5) == 0) return -3;
	if(memcmp(lr->lh.bank_type,"FILE",4) == 0) return -4;

	if(memcmp(lr->lh.bank_type,"LRHD",4) != 0) {
		lr->lh.bank_type[7] = 0 ;
		LOG(ERR,"Bank is not LRHD but is [%s]!",(uint)lr->lh.bank_type,0,0,0,0) ;
	

		return -1 ;
	}


	if(lr->lh.byte_order == DAQ_RAW_FORMAT_ORDER) {	// no swapping necessary
		;
	}
	else {
		// now that I swapped it
		lr->lh.byte_order = swap32(lr->lh.byte_order) ;
		lr->lh.length = swap32(lr->lh.length) ;
		lr->lh.run = swap32(lr->lh.run) ;
		lr->lh.format_ver = swap32(lr->lh.format_ver) ;
		lr->lh.crc = swap32(lr->lh.crc) ;
		
		lr->length = swap32(lr->length) ; 
		lr->blocking = swap32(lr->blocking) ;
		lr->crc = swap32(lr->crc) ;
	}



	if(memcmp(lr->record_type,"BEGR",4) == 0) return 0 ;
	if(memcmp(lr->record_type,"DATA",4) == 0) return (lr->length - lr->lh.length)*4 ;	// size in bytes!
	if(memcmp(lr->record_type,"ENDR",4) == 0) return -2 ;	// END of RUN

	// unknown bank
	lr->record_type[7] = 0 ;
	LOG(CAUTION,"Unknown bank type [%s]!",(uint)lr->record_type,0,0,0,0) ;

	return -1 ;
}


static int evtwait(int desc, ic_msg *m)
{
	int ret ;
	static int counter = 0 ;
	
	// wait with no timeout - handled by higher level code...
	ret = msgNQReceive(desc,(char *)m, sizeof(ic_msg),NO_WAIT) ;

	LOG(DBG,"msgNQReceive returned %d",ret,0,0,0,0) ;

	if(ret == MSG_Q_TIMEOUT) {
		counter++ ;
		if(counter >= 100) {
			counter = 0 ;
			if(msgNQCheck(desc)) return STAT_TIMED_OUT ;
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
int evpReader::fixDatapSummary(DATAP *datap)
{
  if(!sumdatap) return -1;

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

int evpReader::reconnect(void)
{
	int ret ;
	int retries ;
	ic_msg msg ;


	evpDesc = -1 ;	// mark as disconnected

	retries = 0 ;

for(;;) {	// until success...


	evpDesc = msgNQCreate(EVP_HOSTNAME,EVP_PORT,120) ;

	if(evpDesc < 0) {
		LOG(ERR,"Can't create connection to %s:%d [%s] - will retry...",(u_int)EVP_HOSTNAME,EVP_PORT,
		    (u_int)strerror(errno),0,0) ;
		fprintf(stderr,"CRITICAL: Can't create connection to %s:%d [%s] - will retry...\n",EVP_HOSTNAME,EVP_PORT,
			strerror(errno)) ;
		sleep(10) ;
		retries++ ;
		continue ;
	}


	if(retries) {
		LOG(WARN,"Connection suceeded!",0,0,0,0,0) ;
	}


	LOG(DBG,"Opened connection to %s, port %d on descriptor %d",(u_int)EVP_HOSTNAME, EVP_PORT,evpDesc,0,0) ;

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
		LOG(ERR,"Can't send data to %s! - will reconnect...",(u_int)EVP_HOSTNAME,0,0,0,0) ;
		msgNQDelete(evpDesc) ;
		evpDesc = -1 ;
		continue ;
		//return ;
	}

	// all OK...
	isevp = 2  ;	// mark first
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


static char *getCommand(void)
{


        static char *str = "(no-name)" ;
#ifdef __linux__
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


// Assume LHRD at 
//   this->file_name
//   this->evt_offset_in_file
int evpReader::writeCurrentEventToDisk(char *ofilename)
{
  int fdi;
  int fdo;
  int ret;

  int construct_lrhd=0;
  int evt_offset = evt_offset_in_file;
  int evt_size=0;

  fdi = open(file_name, O_RDONLY);
  if(fdi < 0) {
    LOG(ERR, "Error reopening input file %s (%s)", file_name, strerror(errno));
    return -1;
  }

  off_t pos = lseek(fdi, evt_offset, SEEK_SET);
  if(pos == ((off_t)-1)) {
    LOG(ERR, "Error seeking to position %d in file %s (%s)",
	evt_offset, file_name, strerror(errno));
    return -1;
  }

  // Read to DATA LRHD then seek back...
  LOGREC rec;

  for(;;) {
    int ret = read(fdi, &rec, sizeof(rec));
    if(ret < 0) {
      LOG(ERR, "Error reading lrhd %s",strerror(errno));
      close(fdi);
      return -1;
    }

    if(memcmp(rec.lh.bank_type, "LRHD", 4) == 0) {  // Its a LRHD
      if(memcmp(rec.record_type, "BEGR", 4) == 0) {  // but just begin run, skip
	LOG(NOTE, "Got BEGR LRHD");
	evt_offset += sizeof(rec);
	continue;
      }
      else if (memcmp(rec.record_type, "ENDR", 4) == 0) {   // or endrun, skip
	LOG(NOTE, "Got ENDR LRHD");
	evt_offset += sizeof(rec);
	continue;
      }
      if(memcmp(rec.record_type, "DATA", 4) == 0) {   // got it!
	LOG(NOTE, "Got DATA LRHD");
	construct_lrhd = 0;
	evt_size = rec.length * 4;
	break;
      }
    }
    else if (memcmp(rec.lh.bank_type, "DATAP", 4) == 0) { // 
      LOG(NOTE, "DATAP, but no LRHD");
      construct_lrhd = 1;
      DATAP *datap = (DATAP *)&rec;
      evt_size = datap->len * 4;
      break;
    }
    else {
      LOG(ERR, "Got [%c%c%c%c%c] bank... invalid event", 
	  rec.record_type[0], rec.record_type[1], rec.record_type[2], rec.record_type[3],rec.record_type[4]);
      close(fdi);
      return -1;
    }
  }
      
  pos = lseek(fdi, evt_offset, SEEK_SET);
  if(pos == ((off_t)-1)) {
    LOG(ERR, "Error seeking (%s)",strerror(errno));
    close(fdi);
    return -1;
  }

  char *buff = (char *)malloc(evt_size + construct_lrhd * sizeof(LOGREC));
  if(!buff) {
    LOG(ERR, "Error allocating %d bytes (%s)", evt_size + construct_lrhd * sizeof(LOGREC), strerror(errno));
    close(fdi);
    return -1;
  }
 
  if(construct_lrhd) {
    LOG(NOTE, "Constructing LRHD");
    LOGREC *lr = (LOGREC *)buff;
    
    memcpy(lr->lh.bank_type, "LRHD    ",8);
    lr->lh.length = sizeof(LOGREC) / 4;
    lr->lh.format_ver = DAQ_RAW_FORMAT_VERSION;
    lr->lh.byte_order = 0x04030201;
    lr->lh.w7 = 0xdeadface;
    lr->lh.w8 = 0xdeadface;
    lr->lh.w9 = 0xdeadface;
    lr->lh.crc = 0;
    lr->length = (evt_size + sizeof(LOGREC)) / 4;
    lr->blocking = 0;
    memcpy(lr->record_type, "DATA    ",8);
    lr->crc = 0;
  }

  ret = read(fdi, buff + construct_lrhd * sizeof(LOGREC), evt_size);
  if(ret != evt_size) {
    LOG(ERR, "Error reading event data (%s)",strerror(errno));
    close(fdi);
    free(buff);
    return -1;
  }

  fdo = open(ofilename, O_APPEND | O_WRONLY | O_CREAT, 0666);
  if(fdo < 0) {  
    LOG(ERR, "Error  opening output file %s (%s)", ofilename, strerror(errno));
    return -1;
  }

  //LOG("JEFF", "fdo=%d",fdo);

  ret = write(fdo, buff, evt_size + construct_lrhd * sizeof(LOGREC));
  if(ret != evt_size + construct_lrhd * sizeof(LOGREC)) {
    LOG(ERR, "Error writing event data (%s)",strerror(errno));
    close(fdi);
    close(fdo);
    free(buff);
    return -1;
  }

  free(buff);

  close(fdi);
  close(fdo);
  return 0;
}
