/***************************************************************************
 * $Id: EventReader.cxx,v 1.6 1999/07/04 01:47:58 levine Exp $
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
 *
 ***************************************************************************
 * $Log: EventReader.cxx,v $
 * Revision 1.6  1999/07/04 01:47:58  levine
 * minor changes to make solaris CC compiler happy
 *
 * Revision 1.5  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/uio.h>
#include <unistd.h>
#include "EventReader.hh"


EventReader *getEventReader(int fd, long offset, int MMap)
{
  EventReader *er = new EventReader();
  er->InitEventReader(fd, offset, MMap);
  if(er->errorNo()) 
  {
    cout << er->errstr() << endl;
    delete er;
    return NULL;
  }
  
  return er;
}
  
EventReader *getEventReader(char *event)
{
  EventReader *er = new EventReader();
  er->InitEventReader(event);
  if(er->errorNo())
  {
    cout << er->errstr() << endl;
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
}

// Here lies the event reader code
void EventReader::InitEventReader(int fdes, long offset, int MMap)
{//InitER
  long c_offset = offset;
  if (verbose) cout << "Initializing EventReader with a file" << endl;
  
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
    if (strncmp(lr.RecordType, "ENDR", 4)==0) { // check for ENDR record
      if (verbose) 
	printf("ENDR encountered. Processing terminated\n"); fflush(stdout);
      next_event_offset = -1;
      ERROR(ERR_ENDR_ENCOUNTERED);
    }
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
  if(MMap == 0)              // Read the file into a buffer
  {
    MMAPP = NULL;
    DATAP = (char *)malloc(datap.EventLength * 4);
    if(!DATAP) ERROR(ERR_MEM);

    ret = read(fd, DATAP, datap.EventLength*4);
    if(ret < 0) ERROR(ERR_FILE);
  // check CRC, swap
    if (!((Bank_DATAP *)DATAP)->test_CRC()) ERROR(ERR_CRC);
    if (((Bank_DATAP *)DATAP)->swap() < 0) ERROR(ERR_SWAP);

  }
  else if(MMap == 1)
  {
    // Calculate the mmap offset - must be aligned to pagesize
    long pagesize = sysconf(_SC_PAGESIZE);
    if (verbose) cout << "pagesize = " << pagesize << endl;
    int mmap_offset = (offset/pagesize)*pagesize;

    if(ret < 0) ERROR(ERR_FILE);

    DATAP = NULL;
    if((MMAPP = mmap(0, datap.EventLength * 4, PROT_READ | PROT_WRITE,
		      MAP_PRIVATE, fd, mmap_offset)) == (caddr_t) -1)
      ERROR(ERR_MEM);
    
    // Set DATAP to the real value
    DATAP = MMAPP + (offset-mmap_offset);
    fd = -1;
  } 
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
    munmap(MMAPP,event_size);
  }
  else                      // file buffer
  {
    // free my malloc
    free(DATAP);
  }
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

