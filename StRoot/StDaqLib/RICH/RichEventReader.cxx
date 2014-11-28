/***************************************************************************
 * $Id: RichEventReader.cxx,v 1.5 2007/12/24 06:04:23 fine Exp $
 * Author: Zhangbu Xu 
 ***************************************************************************
 * Description: Rich Event reader code for standalone data file
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
 * 21-Apr-00 Adopted by Z. Xu to read Rich standalone events 
 *
 ***************************************************************************
 * $Log: RichEventReader.cxx,v $
 * Revision 1.5  2007/12/24 06:04:23  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.4  2004/02/18 20:17:52  ward
 * Access SSD data in makers.
 *
 * Revision 1.3  2003/12/24 21:55:57  perev
 * Cleanup
 *
 * Revision 1.2  2000/04/25 15:18:08  xzb
 * Fix the compiler warnings.
 *
 * Revision 1.1  2000/04/25 14:55:28  xzb
 * Clean up RICH_Reader array, add RichEventReader for standalone data file
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
#include "RichEventReader.hh"
#include "RICH_Reader.hh"

using namespace OLDEVP;

RichEventReader *getRichEventReader(int fd, long offset, int MMap)
{
  RichEventReader *er = new RichEventReader();
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
    er->InitEventReader(fd, offset); // invoke the mapped version
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


//Memory mapped version
void RichEventReader::InitEventReader(int fdes, long offset, int MMap)
{//InitER
  //#define MX_MAP_SIZE 0x20000000
#define MX_MAP_SIZE 32768*4 /*max event size in RICH*/

  off_t FileLength;

  //  cout << "Initializing EventReader with a MAPPED file" << endl;
  
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
  next_event_offset = 0;
  if (fstat(fd,&buf)<0){
    perror("error in DaqOpenTag");
    ERROR(ERR_FILE);
    next_event_offset = -1;
  }
  FileLength = buf.st_size;
  
  Bank_RICP lr;

  // Calculate the mmap offset - must be aligned to pagesize
  long pagesize = sysconf(_SC_PAGESIZE);
  int mmap_offset = (offset/pagesize)*pagesize;

  if(mmap_offset < 0) ERROR(ERR_FILE);

  int mapsize = buf.st_size - offset + pagesize;
                    //round up to the next page boundary
  if (mapsize<=0) {// <0 means previous event size exceeded file length
    if (verbose) printf("end of file encountered\n");
    ERROR(INFO_END_FILE) ;
    next_event_offset= -1;
  }
  //  if (verbose) printf( "pagesize = %d  mapsize %d offset %d\n",(int)pagesize, mapsize, offset);
  if (mapsize>MX_MAP_SIZE)    mapsize =  MX_MAP_SIZE;
  event_size = mapsize ; //needed for munmap() in destructor

  DATAP = NULL;
  // map to a file
  if((MMAPP = (char *)mmap(0, mapsize, PROT_READ | PROT_WRITE,
			   MAP_PRIVATE, fd, mmap_offset)) == (caddr_t) -1) { 
    char myerr[100];
    sprintf(myerr,"mapping file request 0x%x bytes 0x%x",mapsize,mmap_offset);
    perror(myerr); 
    ERROR(ERR_MEM);
    next_event_offset=-1;
  }
  //  if (verbose) printf("mapping file request 0x%x bytes 0x%x with page 0x%x at 0x%x\n",mapsize,mmap_offset,pagesize,MMAPP);

  // Set DATAP to the real value
  DATAP = MMAPP + (offset-mmap_offset);
  
  if (offset>=buf.st_size) {
    ERROR(ERR_FILE) ;
    next_event_offset = -1;
  }

  if (strncmp(DATAP,"RICP", 4) == 0) {
    // copy the logical record into local struct lr
    if (memcpy(&lr,DATAP,sizeof(lr))<0) perror("error in memcpy");
    // check the CRC
    if (!lr.test_CRC()) ERROR(ERR_CRC);
    // swap bytes
    if (lr.swap() < 0) ERROR(ERR_SWAP);
    // zero CRC
    lr.header.CRC = 0;
    
    char lcopy[10];
    
    strncpy(lcopy,lr.header.BankType,8);
    lcopy[8] = 0;
    //    printf("lr.header.BankType: %s\n",lcopy);

    if(strncmp(lr.header.BankType, "RICP", 4) != 0) { //not DATA
      perror("Have problem will the file\n");
      next_event_offset = -1;
    }
    else { // DATA record. Skip LR
      
      // save run number
      runnum = 0;
      //    DATAP +=sizeof(lr);
      next_event_offset = 4 * lr.Reserved[0].off+offset;

      if (lr.header.Token==0){ // hack
	next_event_offset = -1;
	printf("hack: do not know how to deal with token==0\n");
	DATAP = NULL;
      } 
      
      //   break; //no need to loop any further
    }
  }    
  printf("======= Event number: %d  with size %d============\n",lr.header.Token,lr.Reserved[0].off);
}

RichEventReader::RichEventReader()
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

// unmapped version
void RichEventReader::InitEventReader(int fdes, long offset)
{//InitER
  long c_offset = offset;
  next_event_offset = -1;

  if (verbose) cout << "Initializing RichEventReader with a file" << endl;

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
  if(ret < 0) {
     next_event_offset = -1;
    ERROR(ERR_FILE);
  }

  ret = read(fd,bank,8);
  if(ret < 0) {
    next_event_offset = -1;
    ERROR(ERR_FILE);
  }

  ret = lseek(fd, offset, SEEK_SET); // set pointer back to top of bank
  if(ret < 0) ERROR(ERR_FILE);
  //  printf("%s::%d  offset=0x%x  BANK %s\n",__FILE__,__LINE__,offset,bank);

  Bank_RICP lr;
  
  if (strncmp(bank,"RICP", 4) == 0) {
    // read the logical record
    ret = read(fd,&lr,sizeof(lr));
    if(ret < 0) ERROR(ERR_FILE);
    ret = lseek(fd, offset, SEEK_SET); // set pointer back to top of bank
    if(ret < 0) ERROR(ERR_FILE);
    c_offset += sizeof(lr);
    if (!lr.test_CRC()) ERROR(ERR_CRC);
    if (lr.swap() < 0) ERROR(ERR_SWAP);
    lr.header.CRC = 0;
    
    
    // get event length
    DATAPEVENTLENGTH = lr.Reserved[0].off;   // Hack for event len
    // save run number
    runnum = 0;
    next_event_offset = offset+lr.Reserved[0].off * 4;
    
  
    // check that the file contains the entire event
    struct stat statbuf;
    if(fstat(fd, &statbuf) < 0) ERROR(ERR_FILE);
    
    DATAP = (char *)malloc(DATAPEVENTLENGTH * 4);
    if(!DATAP) ERROR(ERR_MEM);
    
    ret = read(fd, DATAP, DATAPEVENTLENGTH*4);
    if(ret < 0) ERROR(ERR_FILE);
    // check CRC, swap
    if (!((Bank_RICP *)DATAP)->test_CRC()) ERROR(ERR_CRC);
    if (((Bank_RICP *)DATAP)->swap() < 0) ERROR(ERR_SWAP);
    
    event_size = DATAPEVENTLENGTH;
    
  }
  if (lr.header.Token==0){ // hack
    next_event_offset = -1;
    printf("hack: do not know how to deal with token==0\n");
    free(DATAP); 
    DATAP = NULL;
  } 
  printf("======= Event number: %d  size %d============\n",lr.header.Token, DATAPEVENTLENGTH);
  
}

RichEventReader::~RichEventReader()
{
  //  cout << "Destroying event reader" << endl;

  if(fd == -1)              // Pointer construction ... their data
  {
    // Nothing needs done
  }
  else if(MMAPP != NULL)    // Memory Mapped file
  {
    // Unmap memory
    //    if (verbose) printf("0x%x  of size 0x%x\n",MMAPP,event_size);
    munmap(MMAPP,event_size); //unmap 
  }
  else                      // file buffer
  {
    // free my malloc
    free(DATAP);
  }
  if (logfd!=NULL) fclose(logfd);
}

EventInfo RichEventReader::getEventInfo()
{
  //cout << "Getting event info" << endl;
  EventInfo ei;
  Bank_RICP *dp = (Bank_RICP *)DATAP; 
  ei.EventLength = dp->Reserved[0].off;
  ei.EventSeqNo = dp->header.Token;
  ei.RICHPresent = (0x80);
  return ei;
}

void RichEventReader::printEventInfo(FILE * fd)
{
  EventInfo ei = getEventInfo();
  // ei.printEventInfo(fd);
}

long RichEventReader::NextEventOffset()
{
  return next_event_offset;
}

int RichEventReader::MemUsed()
{
  return event_size;
}

void RichEventReader::setVerbose(int v)
{
  verbose = v;
}

char * RichEventReader::findBank(char *bankid)
{
  // Fix up DATAP
  Bank_RICP *pBankDATAP = (Bank_RICP *)this->getDATAP();
  return (char *)pBankDATAP;
  
}

void RichEventReader::fprintError(int err, char *file, int line, char *userstring)
{
  if (logfd==NULL) return; //no file designated
  if (err<0 || err>MX_MESSAGE) return; //protect against bad error code
  fprintf(logfd,"%s  %s::%d   %s\n",err_string[err-1],file,line,userstring);
  fflush(logfd);
}
