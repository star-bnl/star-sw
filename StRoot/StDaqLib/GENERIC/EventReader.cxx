/***************************************************************************
 * $Id: EventReader.cxx,v 1.20 2000/06/07 15:06:08 jml Exp $
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
 *
 ***************************************************************************
 * $Log: EventReader.cxx,v $
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


EventReader *getEventReader(int fd, long offset, int MMap)
{
  EventReader *er = new EventReader();
  if (MMap) {
    er->InitEventReader(fd, offset, MMap); // invoke the mapped version
    if(er->errorNo()) 
      {
	cout << er->errstr().c_str() << endl;
	cout << (er->err_string[er->errorNo()-1]) << endl;
	delete er;
	return NULL;
      }
  }
  else {
    er->InitEventReader(fd, offset); // invoke the unmapped version
    if(er->errorNo()) 
      {
	cout << er->errstr().c_str() << endl;
	cout << (er->err_string[er->errorNo()-1]) << endl;
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
    cout << er->errstr().c_str() << endl;
    cout << (er->err_string[er->errorNo()-1]) << endl;
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
    cout << er->errstr().c_str() << endl;
    cout << (er->err_string[er->errorNo()-1]) << endl;
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
    perror("EventReader::EventReader() logfile failure");
    printf("ERR: failed to open log file %s !!!!!!!\n",logfile);
    assert(0);
  }
  fprintf(logfd,"opening logfile...\n");
}

//Memory mapped version
void EventReader::InitEventReader(int fdes, long offset, int MMap)
{//InitER
#define MX_MAP_SIZE 0x20000000

  off_t FileLength;

  if (verbose) cout << "Initializing EventReader with a MAPPED file" << endl;
  
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
   perror("error in DaqOpenTag");
   ERROR(ERR_FILE);
  }
  FileLength = buf.st_size;

  next_event_offset = 0;
  Logical_Record lr;

  // Calculate the mmap offset - must be aligned to pagesize
  long pagesize = sysconf(_SC_PAGESIZE);
  if (verbose) printf( "pagesize = %d\n",(int)pagesize);
  int mmap_offset = (offset/pagesize)*pagesize;

  if(mmap_offset < 0) ERROR(ERR_FILE);

  int mapsize = buf.st_size - offset + pagesize;
                    //round up to the next page boundary
  if (mapsize<=0) {// <0 means previous event size exceeded file length
    if (verbose) printf("end of file encountered\n");
    ERROR(INFO_END_FILE) ;
  }
  if (mapsize>MX_MAP_SIZE)    mapsize =  MX_MAP_SIZE;
  event_size = mapsize ; //needed for munmap() in destructor

  DATAP = NULL;
  // map to a file
  if((MMAPP = (char *)mmap(0, mapsize, PROT_READ | PROT_WRITE,
			   MAP_PRIVATE, fd, mmap_offset)) == (caddr_t) -1) { 
    char myerr[100];
    sprintf(myerr,"mapping file request 0x%x bytes",mapsize);
    perror(myerr); 
    ERROR(ERR_MEM);
  }

  // Set DATAP to the real value
  DATAP = MMAPP + (offset-mmap_offset);
  
  if (offset>=buf.st_size) {
    ERROR(ERR_FILE) ;
  }

  while (strncmp(DATAP,"LRHD", 4) == 0) {
    // copy the logical record into local struct lr
    if (memcpy(&lr,DATAP,sizeof(lr))<0) perror("error in memcpy");
    // check the CRC
    if (!lr.test_CRC()) ERROR(ERR_CRC);
    // swap bytes
    if (lr.swap() < 0) ERROR(ERR_SWAP);
    // zero CRC
    lr.header.CRC = 0; 

    char lcopy[10];

    strncpy(lcopy,lr.RecordType,8);
    lcopy[8] = 0;
    if (verbose) printf("lr.RecordType: %s\n",lcopy);

    if(strncmp(lr.RecordType, "DATA", 4) != 0) { //not DATA
      //skip over this record 
      next_event_offset += 4 * lr.RecordLength;
      if (verbose) printf("....skipping %d bytes\n",
			  (unsigned int)next_event_offset);
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
    if (verbose) printf("failed to find DATAP at offset 0x%x\n",
			(unsigned int)next_event_offset);
    ERROR(ERR_BANK);
  }
    
  // read the datap bank 
  Bank_DATAP *datap = (Bank_DATAP *)DATAP;

  // check CRC, swap
  if (!datap->test_CRC()) ERROR(ERR_CRC);
  if (datap->swap() < 0) ERROR(ERR_SWAP);

  if (offset + 4*datap->EventLength > buf.st_size) {
    printf("event #%d continues beyond file boundary\n",datap->EventNumber);
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
  if (verbose) cout << "Initializing EventReader with a file" << endl;
  
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
    if (verbose) printf("lr.RecordType: %s\n",lcopy);
//     if (strncmp(lr.RecordType, "ENDR", 4)==0) { // check for ENDR record
//       if (verbose) 
// 	printf("ENDR encountered. Processing terminated\n"); fflush(stdout);
//       next_event_offset = -1;
//       ERROR(ERR_ENDR_ENCOUNTERED);
//     }
    if(strncmp(lr.RecordType, "DATA", 4) != 0) { //not DATA
      //skip over this record 
      offset = 4*lr.RecordLength-sizeof(lr);
//       printf("%s::%d  c_offset=0x%x \n",__FILE__,__LINE__,c_offset);
      if (verbose) printf("....skipping %d bytes\n",offset);
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
  if (datap.swap() < 0) ERROR(ERR_SWAP);
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
  if (verbose) cout << "Creating EventReader with a pointer" << endl;

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

EventInfo EventReader::getEventInfo()
{
  //cout << "Getting event info" << endl;
  EventInfo ei;
  Bank_DATAP *dp = (Bank_DATAP *)DATAP; 
  ei.EventLength = dp->EventLength;
  ei.UnixTime = dp->Time;
  ei.EventSeqNo = dp->EventNumber;
  ei.TrigWord = dp->TriggerWord;
  ei.TrigInputWord = dp->TriggerInWord;
  ei.TPCPresent = (dp->DetectorPresence & 0x1);
  ei.SVTPresent = (dp->DetectorPresence & 0x2);
  ei.TOFPresent = (dp->DetectorPresence & 0x4);
  ei.EMCPresent = (dp->DetectorPresence & 0x8);
  ei.SMDPresent = (dp->DetectorPresence & 0x10);
  ei.FTPCPresent = (dp->DetectorPresence & 0x20);
  ei.Reserved = (dp->DetectorPresence & 0x40);
  ei.RICHPresent = (dp->DetectorPresence & 0x80);
  ei.TRGDetectorsPresent = (dp->DetectorPresence & 0x100);
  ei.L3Present = (dp->DetectorPresence & 0x200);
  return ei;
}

void EventReader::printEventInfo()
{
  char ts[128] ;

  EventInfo ei = getEventInfo();
  sprintf(ts,"%s",ctime((const long int*)&ei.UnixTime)) ;
  ts[24] = 0 ;
  printf("===============  Event # %d  =============\n",ei.EventSeqNo);
  printf("Ev len (wds) %d\n",ei.EventLength);
  printf("Creation Time: %s \n",ts);
  printf("Trigger word 0x%X\t\tTrigger Input word 0x%X\n",ei.TrigWord,ei.TrigInputWord);
  printf("Detectors present: ");
  if (ei.TPCPresent) printf("TPC ");
  if (ei.SVTPresent) printf("SVT ");
  if (ei.TOFPresent) printf("TOF ");
  if (ei.EMCPresent) printf("EMC ");
  if (ei.SMDPresent) printf("SMD ");
  if (ei.FTPCPresent) printf("FTPC ");
  if (ei.RICHPresent) printf("RICH ");
  if (ei.TRGDetectorsPresent) printf("TRG ");
  if (ei.L3Present) printf("L3 ");
  printf("\n");
  printf("===========================================\n");
}

void EventReader::printEventInfo(FILE * fd)
{
  char ts[128] ;

  EventInfo ei = getEventInfo();
  sprintf(ts,"%s",ctime((const long int*)&ei.UnixTime)) ;
  ts[24] = 0 ;
  fprintf(fd,"===============  Event # %d  =============\n",ei.EventSeqNo);
  fprintf(fd,"Ev len (wds) %d\n",ei.EventLength);
  fprintf(fd,"Creation Time: %s \n",ts);
  fprintf(fd,"Trigger word 0x%X\t\tTrigger Input word 0x%X\n",ei.TrigWord,ei.TrigInputWord);
  fprintf(fd,"Detectors present: ");
  if (ei.TPCPresent) fprintf(fd,"TPC ");
  if (ei.SVTPresent) fprintf(fd,"SVT ");
  if (ei.TOFPresent) fprintf(fd,"TOF ");
  if (ei.EMCPresent) fprintf(fd,"EMC ");
  if (ei.SMDPresent) fprintf(fd,"SMD ");
  if (ei.FTPCPresent) fprintf(fd,"FTPC ");
  if (ei.RICHPresent) fprintf(fd,"RICH ");
  if (ei.TRGDetectorsPresent) fprintf(fd,"TRG ");
  if (ei.L3Present) fprintf(fd,"L3 ");
  fprintf(fd,"\n");
  fprintf(fd,"===========================================\n");
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
  Bank_DATAP *pBankDATAP = (Bank_DATAP *)this->getDATAP();

  if (!pBankDATAP->test_CRC()) {
    printf("CRC error in DATAP: %s %d\n",__FILE__,__LINE__) ;
    return FALSE;
  }
  if (pBankDATAP->swap() < 0){
    printf("swap error in DATAP: %s %d\n",__FILE__,__LINE__) ;
    return FALSE;
  }
  pBankDATAP->header.CRC = 0;
  
    // position independent pointers to lower banks, variable DATAP length
  int len = pBankDATAP->header.BankLength - sizeof(Bank_Header)/4;
  len -= ((INT32 )&pBankDATAP->TPC -  (INT32 )&pBankDATAP->EventLength)/4;
  len /= sizeof(Pointer)/4;
  Bank_Header *pBank;
  Pointer *ptr = &pBankDATAP->TPC;
  int i;
  for (i=0; i<len; i++, ptr++) {
    if (ptr->length==0) continue;//invalid entry
    if ((unsigned int)ptr->length== 0xfeedf00d) continue; // EVB fills DATAP with this
    pBank = (Bank_Header *)(((INT32 *)pBankDATAP)+ (ptr->offset)); 
    if(!strncmp(bankid,pBank->BankType,4)) break;
  }
  if (i==len)  return FALSE;
  if(strncmp(pBank->BankType,bankid,4)) {
    printf("detector %s not found in DATAP\n",bankid);
    return FALSE;
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
