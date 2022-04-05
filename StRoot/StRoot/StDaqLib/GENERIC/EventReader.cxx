/***************************************************************************
 * $Id: EventReader.cxx,v 1.66 2014/06/25 15:33:16 jeromel Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: Event reader code common to all DAQ detectors
 *    
 *
 *  change log
 * 06-Jun-99 MJL implement EventReader::getEventInfo()
 * 06-Jun-99 MJL implement EventReader::printEventInfo()
 * 23-Jun-99 MJL add verbose flag and setVerbose() method
 * 23-Jun-99 MJL turn off all printf, cout when verbose=0
 * 24-Jun-99 MJL navigation now reads DATAP without prior 
 *               knowledge of DATAP length
 * 20-Jul-99 MJL added EventReader::fprintError()
 * 20-Jul-99 MJL add alternate constructor for EventReader with name of logfile
 * 20-Jul-99 MJL add alternate getEventReader with name of logfile
 * 20-Jul-99 MJL add overloaded printEventInfo(FILE *)
 * 29-Aug-99 MJL if((MMAPP = (char *) mmap(0, ...  for HP platform
 * 28-Dec-99 MJL add alternate InitEventReaders, mapped and unmapped
 * 31-Jan-00 MJL set runnum  (mapped version)
 * 27-Jun-00 MJL change EventInfo access functions
 *
 ***************************************************************************
 * $Log: EventReader.cxx,v $
 * Revision 1.66  2014/06/25 15:33:16  jeromel
 * Code not used but erradicated use of flush
 *
 * Revision 1.65  2012/11/20 21:25:34  fisyak
 * Make aware that SSD old format fix is valid only till end of 2007
 *
 * Revision 1.64  2012/11/06 21:25:41  fisyak
 * Jeff's fix for SSD
 *
 * Revision 1.63  2012/06/11 16:38:35  fisyak
 * std namespace, remove clash with rtsSystems.h
 *
 * Revision 1.62  2010/01/15 19:51:25  fine
 * RT #1803 Fix side effect for DAT files
 *
 * Revision 1.61  2010/01/13 21:50:45  fine
 * Rt #1803. treat zero DAQ time as error. Print error message
 *
 * Revision 1.60  2009/08/24 20:27:10  jml
 * fixed typos
 *
 * Revision 1.59  2009/08/24 20:17:20  jml
 * remove 1.57, install correct handling of detectors present
 *
 * Revision 1.57  2009/07/22 20:23:57  fine
 *  generate the EventInfo from the daqReader rather from the DATAP structure
 *
 * Revision 1.56  2009/01/08 23:49:23  fine
 * Adjust the EventInfo error message
 *
 * Revision 1.55  2009/01/06 02:55:50  fine
 * Protection against of crash for the new DAQ files withno DATAP  structure
 *
 * Revision 1.54  2008/02/01 21:18:08  fine
 * add strerror() to show the system erro messages. Thanx Matthew Walker
 *
 * Revision 1.53  2008/02/01 19:27:13  fine
 * Replace printf with LOG. Thankx Matthew Walker
 *
 * Revision 1.52  2007/12/27 21:46:40  perev
 * TRG as a part EMC (Pibero)
 *
 * Revision 1.51  2007/12/24 06:04:17  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.50  2007/05/24 20:56:38  jeromel
 * (Pointer to) method returns FALSE instead of NULL fixed (+ one debug statement to remove later)
 *
 * Revision 1.49  2004/09/10 22:08:01  perev
 * more defence agains corrupted DAQ data
 *
 * Revision 1.48  2004/03/01 18:05:47  fisyak
 * Account for new place for rts.h, add osf
 *
 * Revision 1.47  2004/02/18 20:31:14  ward
 * There was a big mess.  I am trying to fix it.
 *
 * Revision 1.45  2003/12/24 21:55:57  perev
 * Cleanup
 *
 * Revision 1.44  2003/10/02 19:39:22  ward
 * Swap header only of DATAP, so Insure++ does not complain about uninitialized data.
 *
 * Revision 1.43  2003/09/02 17:55:31  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.42  2003/07/16 19:58:30  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 1.41  2003/01/29 21:09:31  ward
 * Accomodate nominally zero data words in TOFP.
 *
 * Revision 1.40  2003/01/22 18:13:29  ward
 * Bug in TOF online code, disable corruption check for this bank.
 *
 * Revision 1.39  2002/12/09 18:54:23  ward
 * EMC stuff from Subhassis.
 *
 * Revision 1.38  2002/10/13 20:43:37  ward
 * Support for decoding DAQ100 data and writing it into a table.
 *
 * Revision 1.37  2002/10/10 22:13:58  ward
 * Silence error msg about missing banks in WhereAreThePointers.
 *
 * Revision 1.36  2002/01/17 18:29:54  jeromel
 * After I looked at the code, corrections from Akio (pass2).
 *
 * Revision 1.35  2002/01/17 17:29:26  jeromel
 *
 * Files:  CVS: DetectorReader.cxx EventReader.cxx EventReader.hh CVS: RecHeaderFormats.hh CVS: ----------------------------------------------------------------------
 * Modifications for FPD support
 *
 * Revision 1.34  2001/12/30 23:53:42  ward
 * Fixed FTPC pointer errors, and other cleanup.
 *
 * Revision 1.33  2001/12/29 22:04:31  ward
 * Disabled corruption checks of all FTP banks.
 *
 * Revision 1.32  2001/10/04 19:22:17  ward
 * Disabled corruption check for the EMCP bank and all banks under it.
 *
 * Revision 1.31  2001/07/10 18:12:47  jeromel
 * Changes commited for Frank Geurts (TOF) after approval from Herb Ward
 * on Tue, 10 Jul 2001 11:19:48 and review by Victor.
 * Changes implements TOF DAQ Reader.
 *
 * Revision 1.30  2001/06/26 18:07:38  jcs
 * remove temporary code to skip corruption check for FTPC banks
 *
 * Revision 1.29  2001/06/19 21:07:23  jeromel
 * add FTPC implementation (Janet S.)
 *
 * Revision 1.28  2001/05/14 16:25:58  ward
 * Temporary code to skip corruption check for FTPC banks.
 *
 * Revision 1.27  2000/09/15 21:21:00  fisyak
 * No ulong on HP
 *
 * Revision 1.26  2000/09/12 19:19:06  ward
 * Fixed bug in bank name extraction, added SVTSECP SVTRBP to bank list.
 *
 * Revision 1.25  2000/08/29 17:08:51  ward
 * In corruption detector, temporarily remove L3_P (no doc), and add SVTP.
 *
 * Revision 1.24  2000/08/28 22:19:12  ward
 * Skip corrupted events. StDaqLib/GENERIC/EventReader.cxx & StDAQMaker/StDAQReader.cxx.
 *
 * Revision 1.23  2000/06/27 07:28:07  levine
 * changed EventInfo access functions to fill in, print Token
 *
 * Revision 1.22  2000/06/15 23:05:06  jml
 * I set the number of pointers in DATAP hardcoded to 10.
 * The algorithm for determinining this has been broken by the
 * addition of the trigger data at the end of this bank.
 *
 * Revision 1.21  2000/06/08 12:44:37  jml
 * Added <assert.h> to fix compile error in offline
 *
 * Revision 1.20  2000/06/07 15:06:08  jml
 * Changed exit() calls to assert(0) to aid in debugging
 *
 * Revision 1.19  2000/01/31 19:26:11  levine
 * restore run number to memory-mapped version
 *
 * Revision 1.18  2000/01/11 22:04:40  levine
 * EventReader.hh  // change the header file to include std::string
 * EventReader.cxx // convert string to char* via c_str() member
 * (changes from Brian Lasiuk)
 *
 * Revision 1.17  2000/01/04 20:54:46  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.16  1999/12/07 23:10:30  levine
 * changes to silence the gcc compiler warnings
 *
 * Revision 1.15  1999/12/07 20:24:45  levine
 * add #include <time.h> to make compile warning go away
 *
 * Revision 1.14  1999/12/06 22:53:20  levine
 * Cleaned up information generated on failure to initialize EventReader
 *
 * Revision 1.13  1999/12/03 21:39:22  levine
 * ON encountering end of file, issue INFO message instead of ERROR
 *
 * Revision 1.12  1999/12/02 16:40:23  levine
 * change test on ret value for read (line 230) to prevent looping behavior
 * at end of file
 *
 * Revision 1.11  1999/11/20 00:13:09  fisyak
 * Micheal LeVine update
 *
 * Revision 1.9  1999/07/28 16:08:23  levine
 * modify EventReader so that ENDR does not cause error exit
 *
 * Revision 1.8  1999/07/21 21:33:09  levine
 *
 *
 * changes to include error logging to file.
 *
 * There are now 2 constructors for EventReader:
 *
 *  EventReader();
 *  EventReader(const char *logfilename);
 *
 * Constructed with no argument, there is no error logging. Supplying a file name
 * sends all diagnostic output to the named file (N.B. opens in append mode)
 *
 * See example in client.cxx for constructing a log file name based on the
 * datafile name.
 *
 * It is strongly advised to use the log file capability. You can grep it for
 * instances of "ERROR:" to trap anything noteworthy (i.e., corrupted data files).
 *
 * Revision 1.7  1999/07/10 21:31:17  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.6  1999/07/04 01:47:58  levine
 * minor changes to make solaris CC compiler happy
 *
 * Revision 1.5  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 **************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <time.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/uio.h>
#include <unistd.h>
#include "EventReader.hh"
#include <assert.h>
#include <errno.h>
#include "StMessMgr.h"
//#include "RTS/src/DAQ_READER/daqReader.h"

using namespace OLDEVP;

void EventInfo::printEventInfo(FILE * fd)
{
static const char *detnams[] =
{"TPC ","SVT ","TOF ","BTOW","FPD ","FTPC","EXT ","RICH","TRG ","L3  "
,"SC  ","EXT2","PMD ","SSD ","ETOW","DAQ ","FP2 ","PP  ","BSMD","ESMD"
,"EMC"};

  char ts[128] ;

  sprintf(ts,"%s",ctime((const time_t *)&UnixTime)) ;
  ts[24] = 0 ;
  LOG_INFO<<"===============  Event # "<<EventSeqNo<<"  ============="<<endm;
  LOG_INFO<<"Ev len (wds) "<<EventLength<<endm;
  LOG_INFO<<"Creation Time: "<<ts<<endm;
  LOG_INFO<<"Trigger word "<< std::hex << (void *)TrigWord<<
        "\t\tTrigger Input word "<< std::hex << (void *)TrigInputWord<<endm;
  LOG_INFO<<"Token: "<<Token<<endm;
  LOG_INFO<<"Detectors present: ";
  unsigned const char* p=0; int i=0;
  for (p=&TPCPresent,i=0; p<=&EMCPresent;p++,i++) {
    if (*p) {
      LOG_INFO<<detnams[i]<<" ";
    }
  }
  LOG_INFO<<endm;
  LOG_INFO<<"==========================================="<<endm;
}
EventReader *getEventReader(int fd, long offset, int MMap)
{
  EventReader *er = new EventReader();
  if (MMap) {
    er->InitEventReader(fd, offset, MMap); // invoke the mapped version
    if(er->errorNo()) 
      {
	LOG_ERROR << er->errstr().c_str() << endm;
	LOG_ERROR << (er->err_string[er->errorNo()-1]) << endm;
	delete er;
	return NULL;
      }
  }
  else {
    er->InitEventReader(fd, offset); // invoke the unmapped version
    if(er->errorNo()) 
      {
	LOG_ERROR << er->errstr().c_str() << endm;
	LOG_ERROR << (er->err_string[er->errorNo()-1]) << endm;
	delete er;
	return NULL;
      }
  }

  
  return er;
}

EventReader *getEventReader(int fd, long offset, const char *logfile, int MMap)
{
  EventReader *er = new EventReader(logfile);
  if (MMap) {
    er->InitEventReader(fd, offset, MMap);
  }
  else {
    er->InitEventReader(fd, offset);
  }
  if(er->errorNo()) 
  {
    LOG_ERROR << er->errstr().c_str() << endm;
    LOG_ERROR << (er->err_string[er->errorNo()-1]) << endm;
    delete er;
    return NULL;
  }
  er->printEventInfo(er->logfd);  // print the event information to the log file
  return er;
}
  
EventReader *getEventReader(char *event)
{
  EventReader *er = new EventReader();
  er->InitEventReader(event);
  if(er->errorNo())
  {
    LOG_ERROR << er->errstr().c_str() << endm;
    LOG_ERROR << (er->err_string[er->errorNo()-1]) << endm;
    delete er;
    return NULL;
  }
  
  return er;
}

EventReader::EventReader()
{
  DATAP = NULL;
  MMAPP = NULL;
  errnum = 0;
  runnum = 0;
  memset(errstr0, '\0', sizeof(errstr0));
  event_size = 0;
  fd = -1;
  next_event_offset = -1;
  verbose = 0;
  logfd = NULL; //no error logging
}
EventReader::EventReader(const char *logfile) //pass a string with name of logfile 
{
  DATAP = NULL;
  MMAPP = NULL;
  errnum = 0;
  runnum = 0;
  memset(errstr0, '\0', sizeof(errstr0));
  event_size = 0;
  fd = -1;
  next_event_offset = -1;
  verbose = 0;
  logfd = fopen(logfile,"a");
  if (logfd==NULL) {
    LOG_ERROR<<"EventReader::EventReader() logfile failure"<<endm;
    LOG_ERROR << strerror(errno) << ": " << logfile<< " !!!!!!!"<<endm;;
    assert(0);
  }
  LOG_INFO<<"opening logfile..."<<endm;
}

//Memory mapped version
void EventReader::InitEventReader(int fdes, long offset, int MMap)
{//InitER
#define MX_MAP_SIZE 0x20000000

  off_t FileLength;

  if (verbose) {
    LOG_INFO<< "Initializing EventReader with a MAPPED file" << endm;
  }
  
  //initialize the error strings
  strcpy(err_string[0],"ERROR: FILE");
  strcpy(err_string[1],"ERROR: CRC");
  strcpy(err_string[2],"ERROR: SWAP");
  strcpy(err_string[3],"ERROR: BANK");
  strcpy(err_string[4],"ERROR: MEM");
  strcpy(err_string[5],"ERROR: NOT DATA BANK");
  strcpy(err_string[6],"ERROR: BAD ARG");
  strcpy(err_string[7],"ERROR: ENDR ENCOUNTERED");
  strcpy(err_string[8],"ERROR: BAD HEADER");
  strcpy(err_string[9],"INFO: MISSING BANK");
  strcpy(err_string[10],"INFO: END OF FILE");


  fd = fdes;
  struct stat buf;

  if (fstat(fd,&buf)<0){
   LOG_ERROR << "DaqOpenTag"<< strerror(errno) <<endm;
   ERROR(ERR_FILE);
  }
  FileLength = buf.st_size;

  next_event_offset = 0;
  Logical_Record lr;

  // Calculate the mmap offset - must be aligned to pagesize
  long pagesize = sysconf(_SC_PAGESIZE);
  if (verbose){
    LOG_INFO<<"pagesize = "<<(int)pagesize<<endm;;
  }
  int mmap_offset = (offset/pagesize)*pagesize;

  if(mmap_offset < 0) ERROR(ERR_FILE);

  int mapsize = buf.st_size - offset + pagesize;
                    //round up to the next page boundary
  if (mapsize<=0) {// <0 means previous event size exceeded file length
    if (verbose) {
      LOG_ERROR<<"end of file encountered"<<endm;
    }
    ERROR(INFO_END_FILE) ;
  }
  if (mapsize>MX_MAP_SIZE)    mapsize =  MX_MAP_SIZE;
  event_size = mapsize ; //needed for munmap() in destructor

  DATAP = NULL;
  // map to a file
  if((MMAPP = (char *)mmap(0, mapsize, PROT_READ | PROT_WRITE,
			   MAP_PRIVATE, fd, mmap_offset)) == (caddr_t) -1) { 
    LOG_ERROR<<strerror(errno)<<"mapping file request "
                              <<std::hex<<(void*)mapsize<<" bytes"<<endm;
    ERROR(ERR_MEM);
  }

  // Set DATAP to the real value
  DATAP = MMAPP + (offset-mmap_offset);
  
  if (offset>=buf.st_size) {
    ERROR(ERR_FILE) ;
  }

  while (strncmp(DATAP,"LRHD", 4) == 0) {
    // copy the logical record into local struct lr
    if (memcpy(&lr,DATAP,sizeof(lr))<0) {
       LOG_ERROR<< strerror(errno)<<": error in memcpy"<<endm;
    }
    // check the CRC
    if (!lr.test_CRC()) ERROR(ERR_CRC);
    // swap bytes
    if (lr.swap() < 0) ERROR(ERR_SWAP);
    // zero CRC
    lr.header.CRC = 0; 

    char lcopy[10];

    strncpy(lcopy,lr.RecordType,8);
    lcopy[8] = 0;
    if (verbose) {
      LOG_INFO<<"lr.RecordType: "<<lcopy<<endm;
    }

    if(strncmp(lr.RecordType, "DATA", 4) != 0) { //not DATA
      //skip over this record 
      next_event_offset += 4 * lr.RecordLength;
      if (verbose) {
	LOG_INFO<<"....skipping "<<(unsigned int)next_event_offset<<" bytes"<<endm;
      }
      DATAP += next_event_offset;

      //DATAP now points to beginning of next RECORD
    }
    else { // DATA record. Skip LR
      
      // save run number
      runnum = lr.header.RunNumber;
    
      DATAP += sizeof(lr);
      next_event_offset += sizeof(lr);
      break; //no need to loop any further
    }
  } // while (strncmp(DATAP,"LRHD", 4) == 0)

    // We should now be positioned at DATAP

  if(strncmp(DATAP,"DATAP", 5) != 0)
  {
    if (verbose) {
      LOG_ERROR<<"failed to find DATAP at offset 0x"<<(unsigned int)next_event_offset<<endm;
    }
    ERROR(ERR_BANK);
  }
    
  // read the datap bank 
  Bank_DATAP *datap = (Bank_DATAP *)DATAP;

  // check CRC, swap
  if (!datap->test_CRC()) ERROR(ERR_CRC);
  if (datap->swap() < 0) ERROR(ERR_SWAP);

  if (offset + 4*datap->EventLength > buf.st_size) {
    LOG_ERROR<<"event #"<<datap->EventNumber<<" continues beyond file boundary"<<endm;
    ERROR(ERR_FILE) ;
  }

  //  printf("======= Event number: %d ============\n",datap.EventNumber);
  next_event_offset += 4*datap->EventLength; // needed for nextEventOffset()
  next_event_offset += offset; // now consistent with unmapped behavior
                               // 29-Dec-99 MJL
}

// unmapped version
void EventReader::InitEventReader(int fdes, long offset)
{//InitER
  long c_offset = offset;
  if (verbose) {
    LOG_INFO<< "Initializing EventReader with a file" << endm;
  }
  
  //initialize the error strings
  strcpy(err_string[0],"ERROR: FILE");
  strcpy(err_string[1],"ERROR: CRC");
  strcpy(err_string[2],"ERROR: SWAP");
  strcpy(err_string[3],"ERROR: BANK");
  strcpy(err_string[4],"ERROR: MEM");
  strcpy(err_string[5],"ERROR: NOT DATA BANK");
  strcpy(err_string[6],"ERROR: BAD ARG");
  strcpy(err_string[7],"ERROR: ENDR ENCOUNTERED");
  strcpy(err_string[8],"ERROR: BAD HEADER");
  strcpy(err_string[9],"INFO: MISSING BANK");
  strcpy(err_string[10],"INFO: END OF FILE");


  fd = fdes;
  int DATAPEVENTLENGTH=0;   // hack, the event length is not yet in datap

  char bank[9];
  memset(bank,'\0',sizeof(bank));

  int ret = lseek(fd, offset, SEEK_SET);
  if(ret < 0) ERROR(ERR_FILE);

  ret = read(fd,bank,8);
  if(ret < 0) ERROR(ERR_FILE);
  
  ret = lseek(fd, offset, SEEK_SET); // set pointer back to top of bank
  if(ret < 0) ERROR(ERR_FILE);
//  printf("%s::%d  offset=0x%x  BANK %s\n",__FILE__,__LINE__,offset,bank);

  Logical_Record lr;

  while (strncmp(bank,"LRHD", 4) == 0) {
    
    // read the logical record
    ret = read(fd,&lr,sizeof(lr));
    if(ret < 0) ERROR(ERR_FILE);
    c_offset += sizeof(lr);
//     printf("%s::%d  c_offset=0x%x \n",__FILE__,__LINE__,c_offset);
//     lr.print();

    // check the CRC
    // swap bytes
    // zero CRC
    if (!lr.test_CRC()) ERROR(ERR_CRC);
    if (lr.swap() < 0) ERROR(ERR_SWAP);
    lr.header.CRC = 0;


    // get event length
    DATAPEVENTLENGTH = lr.RecordLength - sizeof(lr)/4;   // Hack for event len
    // save run number
    runnum = lr.header.RunNumber;
    
    // Check version (someday)

    // Check record type
//     printf("%s::%d  c_offset=0x%x \n",__FILE__,__LINE__,c_offset);
//     lr.print();
    char lcopy[10];
    int offset = 0;
    strncpy(lcopy,lr.RecordType,8);
    lcopy[8] = 0;
    if (verbose) {
      LOG_INFO<<"lr.RecordType: "<<lcopy<<endm;
    }
//     if (strncmp(lr.RecordType, "ENDR", 4)==0) { // check for ENDR record
//       if (verbose) 
// 	printf("ENDR encountered. Processing terminated\n"); 
//       next_event_offset = -1;
//       ERROR(ERR_ENDR_ENCOUNTERED);
//     }
    if(strncmp(lr.RecordType, "DATA", 4) != 0) { //not DATA
      //skip over this record 
      offset = 4*lr.RecordLength-sizeof(lr);
//       printf("%s::%d  c_offset=0x%x \n",__FILE__,__LINE__,c_offset);
      if (verbose) {
	LOG_INFO<<"....skipping "<<offset<<" bytes"<<endm;
      }
      lseek(fd,offset,SEEK_CUR);
      c_offset += offset;
      
      ret = read(fd,bank,8);
      lseek(fd,-8,SEEK_CUR); //backspace over BANK TYPE
//       printf("%s::%d  c_offset=0x%x  BANK %s\n",__FILE__,__LINE__,c_offset,bank);
      if(ret < 0) ERROR(ERR_FILE);
      //      if(ret == 0) ERROR(ERR_FILE);
      if(ret == 0) ERROR(INFO_END_FILE);
    }
    else { // DATA record. Skip LR
//       printf("%s::%d  c_offset=0x%x \n",__FILE__,__LINE__,c_offset);
      
      ret = read(fd,bank,8);
      lseek(fd,-8,SEEK_CUR); //backspace over BANK TYPE
//       printf("%s::%d  c_offset=0x%x  BANK %s\n",__FILE__,__LINE__,c_offset,bank);
      break; //no need to loop any further
    }

  }
    // We are now Positioned at DATAP
  memset(bank,'\0',sizeof(bank));
  ret = read(fd,bank,8);
  if(ret < 0) ERROR(ERR_FILE);
//   printf("%s::%d  c_offset=0x%x  BANK %s\n",__FILE__,__LINE__,c_offset,bank);
  
  ret = lseek(fd,-8,SEEK_CUR);// backspace to beginning of Bank
  
  offset += sizeof(lr);  // I want offset to point at DATAP
//   printf("%s::%d  c_offset=0x%x  BANK %s\n",__FILE__,__LINE__,c_offset,bank);

  if(strncmp(bank,"DATAP", 5) != 0)
  {
    ERROR(ERR_BANK);
  }
    
  // read the datap bank then seek back to start of it
  Bank_DATAP datap;
  ret = read(fd,&datap,sizeof(Bank_Header));
  if (datap.header.swap() < 0) ERROR(ERR_SWAP); // There is no data in the body.  Swapping it causes
                                                // Insure++ to complain.  So we swap only the header.
  int len = 4*datap.header.BankLength;
  //  if (len>sizeof(datap)) ERROR(ERR_BANK);
  // why is sizeof(datap) 548 when it should be 138*4 ??
  // according to the RecHeaderFormats.hh declaration??
  ret = lseek(fd,-sizeof(Bank_Header),SEEK_CUR);
  if(ret < 0) ERROR(ERR_FILE);
  ret = read(fd,&datap,len);
  if(ret < 0) ERROR(ERR_FILE);
  ret = lseek(fd,-len,SEEK_CUR);
  if(ret < 0) ERROR(ERR_FILE);

  // check CRC, swap
  if (!datap.test_CRC()) ERROR(ERR_CRC);
  if (datap.swap() < 0) ERROR(ERR_SWAP);

  if(datap.EventLength < DATAPEVENTLENGTH) 
    datap.EventLength = DATAPEVENTLENGTH;  // hack

  next_event_offset = c_offset + datap.EventLength * 4;
//   printf("%s::%d  c_offset=0x%x datap.EventLength * 4  0x%x\n",__FILE__,__LINE__,c_offset,datap.EventLength * 4);

  //  printf("======= Event number: %d ============\n",datap.EventNumber);
  // check that the file contains the entire event
  struct stat statbuf;
  if(fstat(fd, &statbuf) < 0) ERROR(ERR_FILE);
  //  assert(!(datap.EventLength > (statbuf.st_size - offset)/4)); // ERROR(ERR_FILE);
  //if(datap.EventLength > (statbuf.st_size - offset)/4)  ERROR(ERR_FILE);
  // mmap or read the file

  DATAP = (char *)malloc(datap.EventLength * 4);
  if(!DATAP) ERROR(ERR_MEM);
  
  ret = read(fd, DATAP, datap.EventLength*4);
  if(ret < 0) ERROR(ERR_FILE);
  // check CRC, swap
  if (!((Bank_DATAP *)DATAP)->test_CRC()) ERROR(ERR_CRC);
  if (((Bank_DATAP *)DATAP)->swap() < 0) ERROR(ERR_SWAP);
    
 
  datap.EventLength = DATAPEVENTLENGTH;   // hack!
  
  event_size = datap.EventLength;

}

void EventReader::InitEventReader(void *event)
{
  if (verbose){
    LOG_INFO << "Creating EventReader with a pointer" << endm;
  }

  if(strncmp((char *)event,"LRHD",4) == 0)
  {
    Logical_Record *lr=(Logical_Record *)event;
    
    // check CRC, swap, zero CRC
    if (!lr->test_CRC()) ERROR(ERR_CRC);
    if (lr->swap() < 0) ERROR(ERR_SWAP);
    lr->CRC = 0;

    // save run number
    runnum = lr->header.RunNumber;
    
    // Check version (someday)

    // Check Record Type
    if(strncmp(lr->RecordType, "DATA", 4) != 0) ERROR(ERR_NOT_DATA_BANK);

    // position at start of DATAP
    event = (void *)((char *)event + sizeof(Logical_Record));
  }

  if(strncmp((char *)event,"DATAP", 5) != 0) ERROR(ERR_BANK);
  Bank_DATAP *datap = (Bank_DATAP *)event;
  
  // CRC, swap, zero CRC
  if (!datap->test_CRC()) ERROR(ERR_CRC);
  if (datap->swap() < 0) ERROR(ERR_SWAP);
  datap->header.CRC = 0;

  // save event length, DATAP
  DATAP = (char *)datap;
  event_size = datap->EventLength * 4;
}

EventReader::~EventReader()
{
  //  cout << "Destroying event reader" << endl;

  if(fd == -1)              // Pointer construction ... their data
  {
    // Nothing needs done
  }
  else if(MMAPP != NULL)    // Memory Mapped file
  {
    // Unmap memory
    munmap(MMAPP,event_size); //unmap 
  }
  else                      // file buffer
  {
    // free my malloc
    free(DATAP);
  }
  if (logfd!=NULL) fclose(logfd);
}

int EventReader::system_present(Bank_DATAP *datap, int sys)
{
  Pointer *pointer;
  datap->swap();

  if(sys >= 10) {
    pointer = &datap->EXTY_ID;
    if((pointer->offset == 0) || (pointer->length == 0)) {
      return 0;
    }

    Bank_DATAPX *datapx = (Bank_DATAPX *)(((INT32 *)datap) + (pointer->offset));
    datapx->swap();

    pointer = &datapx->EXT_DET[sys-10];
    if((pointer->offset == 0) || (pointer->length == 0)) {
      return 0;
    }
  }
  else {
    pointer = &datap->TPC;
    pointer += sys;
    if((pointer->offset == 0) || (pointer->length == 0)) {
      return 0;
    }
  }
  return 1;
}

EventInfo EventReader::getEventInfo()
{
  enum {
    TPC_SYSTEM  =    0
    ,SVT_SYSTEM  =    1
    ,TOF_SYSTEM  =    2
    ,BTOW_SYSTEM =    3  //EMC Barrel Tower
    ,FPD_SYSTEM  =    4
    ,FTP_SYSTEM  =    5
    ,EXT_SYSTEM  =    6 // ignore
    ,RIC_SYSTEM  =    7
    ,TRG_SYSTEM  =    8
    ,L3_SYSTEM   =    9
    ,SC_SYSTEM   =   10 // reserved for Slow Controls
    ,EXT2_SYSTEM =   11 // ignore
    ,PMD_SYSTEM  =   12
    ,SSD_SYSTEM  =   13
    ,ETOW_SYSTEM =   14 //EMC EndCup Tower
    ,DAQ_SYSTEM  =   15 // ignore
    ,FP2_SYSTEM  =   16 // reserved for future FPD
    ,PP_SYSTEM   =   17 // ignore
    ,BSMD_SYSTEM =   18 //EMC Barrel Shower
    ,ESMD_SYSTEM =   19 //EMC Endcup Shower
  };

  //cout << "Getting event info" << endl;
  EventInfo ei;
  memset(&ei,0,sizeof(EventInfo));
  Bank_DATAP *dp   = (Bank_DATAP *)DATAP; 
  if (dp) {
    ei.Token         = dp->header.Token;
    ei.EventLength   = dp->EventLength;
    ei.UnixTime      = dp->Time;
    ei.EventSeqNo    = dp->EventNumber;
    ei.TrigWord      = dp->TriggerWord;
    ei.TrigInputWord = dp->TriggerInWord;
    int detpre       = dp->DetectorPresence;
    //?yf    ei.UnixTime      = -1;// special case: time was not defined (=0 means : now() )
    ei.EventLength   = -1;// special case: time was not defined (=0 means : now() )
    LOG_INFO<<"EventReader::getEventInfo  detector presence = "<<detpre<<endm;

    int sys = 0;
    for (unsigned char *p = &ei.TPCPresent; p<=&ei.ESMDPresent;p++) {
      *p = 0;
      if(system_present(dp, sys) || 
	 (ei.UnixTime < 1197676800 && (detpre & (1<<sys))) // Dec 15 00:00:00 2007 fix for SSD
	 ) *p = 1;
      sys++;
    }
    ei.EMCPresent = (ei.BTOWPresent|ei.ETOWPresent|ei.BSMDPresent|ei.ESMDPresent|ei.TRGPresent);
  } else {
    LOG_ERROR << "EventReader::getEventInfo: No DATAP exists" << endm;
  }
  return ei;
}

void EventReader::printEventInfo(FILE * fd)
{
  EventInfo ei = getEventInfo();
  ei.printEventInfo(fd);
}

long EventReader::NextEventOffset()
{
  return next_event_offset;
}

int EventReader::MemUsed()
{
  return event_size;
}

void EventReader::setVerbose(int v)
{
  verbose = v;
}

char * EventReader::findBank(char *bankid)
{
  // Fix up DATAP
  Bank_DATAP *pBankDATAP = (Bank_DATAP *)getDATAP();
  if (!pBankDATAP) {
    LOG_ERROR<<"DATAP not found: "<<__FILE__<<" "<<__LINE__<<endm;
    return NULL;
  }
  if (!pBankDATAP->test_CRC()) {
    LOG_ERROR<<"CRC error in DATAP: "<<__FILE__<<" "<<__LINE__<<endm;
    return NULL;
  }
  if (pBankDATAP->swap() < 0){
    LOG_ERROR<<"swap error in DATAP: "<<__FILE__<<" "<<__LINE__<<endm;
    return NULL;
  }
  pBankDATAP->header.CRC = 0;
  
// position independent pointers to lower banks, variable DATAP length
//   int len = pBankDATAP->header.BankLength - sizeof(Bank_Header)/4;
//   len -= ((INT32 )&pBankDATAP->TPC -  (INT32 )&pBankDATAP->EventLength)/4;
//   len /= sizeof(Pointer)/4;
  // JML - the length is now hard coded again as DATAP now contains
  // non-pointer data after the pointer data
  int len = 10;
  int ext_len=22; // For Extended Detector ( e.g PMD ) ; Added by Susanta on 6th Nov, 2002

  Pointer *ptr = &pBankDATAP->TPC;
  
  Bank_Header *pBank=0;
  Bank_DATAPX *pBankDATAPX=0; // Added by Susanta for PMD on 6th Nov, 2002

  int i,j=0;

  for (i=0; i<len; i++, ptr++) {
    if (ptr->length==0) continue;//invalid entry
    if ((unsigned int)ptr->length== 0xfeedf00d) continue; // EVB fills DATAP with this
    pBank = (Bank_Header *)(((INT32 *)pBankDATAP)+ (ptr->offset)); // the INT32 cast has 
                                   // the effect of multiplying the offset by 4
   
    // staff Added by Susanta for PMD on 6th Nov, 2002.--- BEGIN
        if(!strncmp("DATAPX",pBank->BankType,6)){
	  pBankDATAPX = (Bank_DATAPX *)(((INT32 *)pBankDATAP)+ (ptr->offset));
	  pBankDATAPX->swap();
  	  Pointer *ptr1 = &pBankDATAPX->EXT_DET[0];
	  for(j=0; j < ext_len; j++, ptr1++){
               if (ptr1->length==0) continue;//invalid entry
               pBank = (Bank_Header *)(((INT32 *)pBankDATAPX)+ ptr1->offset); // To find Extended Detector ID 
               if(!strncmp(bankid,pBank->BankType,4)) break;
	  }
        }
	// ---ENDS
        if(!strncmp(bankid,pBank->BankType,4)) break;
  }
  if (i==len)  return NULL;
  if (!pBank)  return NULL;

  if(strncmp(pBank->BankType,bankid,4)) {
    LOG_ERROR<<"detector "<<bankid<<" not found in DATAP"<<endm;
    return NULL;
  }
  return (char *)pBank;
  
}

void EventReader::fprintError(int err, char *file, int line, char *userstring)
{
  if (logfd==NULL) return; //no file designated
  if (err<0 || err>MX_MESSAGE) return; //protect against bad error code
  fprintf(logfd,"%s  %s::%d   %s\n",err_string[err-1],file,line,userstring);
  fflush(logfd);
}

///////// Stuff below here is from Herb Ward, Aug 28 2000,  /////////////////////////////////
///////// for detection of corruption in .daq files. www   //////////////////////////////////
#define PP printf(
char EventReader::eventIsCorrupted(int herbFd,long offset) {
  char returnValue;
  offset+=60; // Skip the LRHD, and go directly to DATAP.
  strcpy(mLastBank,"???"); mWordIndex=-1;
  returnValue = BankOrItsDescendentsIsBad(herbFd,offset);
  if(returnValue) {
    PP"StDaqLib/EventReader::eventIsCorrupted: bank pointed to by data word %d of bank %s.\n",mWordIndex+1,mLastBank);
  }
  return returnValue;
}
char *EventReader::ConvertToString(unsigned long  *input) {
  static char rv[20];
  char *cc,*dd;
  cc=(char*)input;
  dd=rv;
  while(isupper(*cc)||isdigit(*cc)||*cc=='_') *(dd++)=*(cc++); *dd=0;
  if(strlen(rv)>8) rv[8]=0; // Max len of a bank name is 8 chars (2 words = 8 bytes)
  return rv;
}
void EventReader::WhereAreThePointers(int *beg,int *end,char *xx) {
  *beg=-123; *end=-123;
  assert(strcmp(xx,  "TPCMZP"));

  // Dec 30 2001.  The FTP pointers were messed up.  If you want to correct them, use
  // either ~ward/corruptionDetector/corrDet.c or the code in StDaqLib/FTPC.  In the
  // meantime, the line below disables all FTPC checks.
  if(xx[0]=='F'&&xx[1]=='T'&&xx[2]=='P') { *beg=0; *end= 0; return; }

  if(!strcmp(xx,   "DATAP")) { *beg=7; *end=26; }
  if(!strcmp(xx,    "RICP")) { *beg=1; *end=36; }
  if(!strcmp(xx,"RICCRAMP")) { *beg=1; *end=16; }
  if(!strcmp(xx,    "TRGP")) { *beg=1; *end= 2; }
  if(!strcmp(xx,    "TPCP")) { *beg=1; *end=48; }
  if(!strcmp(xx, "TPCSECP")) { *beg=1; *end=24; }
  if(!strcmp(xx,  "TPCRBP")) { *beg=1; *end= 6; }
  if(!strcmp(xx,"L3_SECTP")) { *beg=9; *end=14; }
  if(!strcmp(xx, "L3_SECP")) { *beg=6; *end=11; }
  if(!strcmp(xx,    "L3_P")) { *beg=0; *end= 0; return; } // I don't have good doc for L3_P yet.
  if(!strcmp(xx,    "SVTP")) { *beg=1; *end= 8; }
  if(!strcmp(xx, "SVTSECP")) { *beg=1; *end=24; }
  if(!strcmp(xx,  "SVTRBP")) { *beg=1; *end= 6; }
  assert(strcmp(xx,  "FTPMZP"));
  if(!strcmp(xx,    "FTPP")) { *beg=1; *end=4; } // Changed from 48 to 4, Dec 30 2001 by H Ward.
  if(!strcmp(xx, "FTPSECP")) { *beg=1; *end= 24; }
  if(!strcmp(xx,  "FTPRBP")) { *beg=1; *end=6; }
  // There is a bug in TOF online code. if(!strcmp(xx,    "TOFP")) { *beg=1; *end=8; }
  if(!strcmp(xx,    "FPDP")) { *beg=0; *end=0; } // This is for chekcing data corruptions. Skip for now
  (*beg)--; (*end)--;
  if((*beg)<0||(*end)<0) {
    // printf("Please add code to WhereAreThePointers for '%s'.\n",xx); Commented Oct 10 2002.
    *beg=0; *end=0; return;
  }
  if(*beg>=*end) assert(0);
  if((*end-*beg)%2!=1) assert(0);
}
void EventReader::Swap4(unsigned long *data) {
  char *hh,temp[4];
  hh=(char*)data;
  temp[0]=hh[3]; temp[1]=hh[2]; temp[2]=hh[1]; temp[3]=hh[0];
  *data=*((unsigned long*)temp);
}
#define DATA 400 /* number of words */
/* comment 12c  It is not a pointer bank.  We've already looked at the header, that's all we can do
for a generic bank. */
char EventReader::BankOrItsDescendentsIsBad(int herbFd,long currentOffset) { // Boolean value.
  unsigned long header[10],data[DATA];
  char doTheByteSwap=FALSE,lastletter,bankname[25];
  int i,beg,end,numberOfDataWords,bytesRead;


   
  if(lseek(herbFd,currentOffset,SEEK_SET)!=currentOffset) return TRUE;
   
  bytesRead=read(herbFd,header,10*sizeof(unsigned long));
   
  if(bytesRead!=10*sizeof(unsigned long)) return TRUE;
   

  strcpy(bankname,ConvertToString(header));
  // PP"BBB bankname = %s\n",bankname);
  if(strlen(bankname)<3) return TRUE;
  lastletter=bankname[strlen(bankname)-1];

  if(header[5]==0x01020304) doTheByteSwap=TRUE; else if(header[5]!=0x04030201) return TRUE;

  /* KEEP ALL CHECKS OF THE HEADER ABOVE THIS LINE. see comment 12c */

  if(lastletter!='P') return FALSE; /* see comment 12c */

  if(doTheByteSwap) for(i=0;i<10;i++) Swap4(header+i);
  assert(header[5]==0x04030201); /* We have enought corruption checks above that this shouldn't happen. */

  numberOfDataWords=header[2]-10;
  if(numberOfDataWords>DATA) { LOG_INFO<<numberOfDataWords<<" "<<DATA<<", bankname="<<bankname<<endm; }
  assert(numberOfDataWords<=DATA);
  if(!strcmp(bankname,"TPCMZP")) { beg=0; end=numberOfDataWords-1; }
  else if(!strcmp(bankname,"EMCP")) { beg=0; end=0; }
  else if(!strcmp(bankname,"PMDP")) { beg=0; end=0; }
  else WhereAreThePointers(&beg,&end,bankname); 
  if(end>=numberOfDataWords&&numberOfDataWords>0) {//29jan03 numberOfDataWords>0 because of bank errs
    PP"end=%d, numberOfDataWords=%d, bankname=%s.\n",end,numberOfDataWords,bankname);
    assert(end<numberOfDataWords);
  }

  bytesRead=read(herbFd,data,numberOfDataWords*sizeof(unsigned long));
  if(bytesRead!=int(numberOfDataWords*sizeof(unsigned long))) return TRUE;
  if(doTheByteSwap) for(i=0;i<numberOfDataWords;i++) Swap4(data+i);

  if(!strcmp(bankname,"TPCMZP")||!strcmp(bankname,"FTPMZP")) {
    if(data[5]!=0&&(data[1]==0||data[3]==0)) return TRUE; /* TPCADCX should be present only if SEQD & ADCD are too. */
  }

  for(i=beg;i<end;i+=2) {
    if(data[i+1]==0) continue; /* len is 0 */
    if(data[i]==0) {
      PP"Bank '%s' (at offset %ld bytes in the .daq file) points to itself (data word %d counting from 1).\n",
          bankname,currentOffset,i+1);
      return TRUE; 
    }
    strncpy(mLastBank,bankname,25);
    mWordIndex=i; // mWordIndex is 1 off from the DAQ Data Format document (starts from 0 instead of 1).
    if(BankOrItsDescendentsIsBad(herbFd,4*data[i]+currentOffset)) return TRUE;
  }
  return FALSE;
}
