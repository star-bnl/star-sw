/***************************************************************************
 * $Id: RichEventReader.hh,v 1.3 2003/12/24 21:55:57 perev Exp $
 * Author: Zhangbu Xu
 ***************************************************************************
 * Description: definition for Rich Standalone event reader
 *      
 *
 *   change log
 * 06-June-99 MJL added EventInfo struct, changed method getEventInfo()
 * 06-June-99 MJL added printEventInfo()
 * 17-June-99 Herb Ward changed the dimension of errstr0 from 50 to 250
 * 23-Jun-99 MJL add verbose flag and setVerbose() method
 * 25-Jun-99 MJL added TPCV2P0_CPP_SR::getAsicParams(ASIC_params *);
 * 09-Jul-99 MJL added EventReader::findBank()
 * 20-Jul-99 MJL added EventReader::fprintError()
 * 20-Jul-99 MJL add alternate getEventReader with name of logfile
 * 20-Jul-99 MJL add overloaded printEventInfo(FILE *)
 * 28-Dec-99 MJL add alternate InitEventReaders, mapped and unmapped
 * 31-Jan-00 MJL change to #if !defined ST_NO_NAMESPACES
 * 21-Apr-00 Adopted by Z. Xu to read Rich standalone events 
 *
 ***************************************************************************
 * $Log: RichEventReader.hh,v $
 * Revision 1.3  2003/12/24 21:55:57  perev
 * Cleanup
 *
 * Revision 1.2  2003/09/02 17:55:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/04/25 14:55:28  xzb
 * Clean up RICH_Reader array, add RichEventReader for standalone data file
 *
 * Revision 1.10  2000/02/15 23:24:27  fisyak
 * Force to compile StPadMonitorMaker
 *
 * Revision 1.9  2000/01/31 19:38:51  levine
 * chamge to #if !defined ST_NO_NAMESPACES
 *
 * Revision 1.8  2000/01/11 22:04:40  levine
 * EventReader.hh  // change the header file to include std::string
 * EventReader.cxx // convert string to char* via c_str() member
 * (changes from Brian Lasiuk)
 *
 * Revision 1.7  2000/01/04 20:54:47  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.6  1999/07/26 17:00:03  levine
 * changes to RICH file organization
 *
 * Revision 1.5  1999/07/21 21:33:08  levine
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
 * Revision 1.4  1999/07/10 21:31:17  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.3  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/
#ifndef RICHEVENTREADER_HH
#define RICHEVENTREADER_HH

#include <Stiostream.h>

#include <sys/types.h>

#include <string>
#if !defined ST_NO_NAMESPACES
using std::string;
#endif

#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/Error.hh"

#define TRUE 1
#define FALSE 0



// Event Reader header files
// This file is included by Offline programs

class RichEventReader;

// Support Structures


// Event Reader Class
class RichEventReader
{
public:
  RichEventReader();

  void InitEventReader(int fd, long offset, int mmap);  
                             // takes open file descripter-offset
                             // works on MAPPED file
  void InitEventReader(int fd, long offset);  

  long NextEventOffset();
  void setVerbose(int); // 0 turns off all internal printout
  char * findBank(char *bankid); // navigates to pointer bnk below DATAP
  int verbose;

  ~RichEventReader();

  char *getDATAP() { return DATAP; };
  struct EventInfo getEventInfo();
  void printEventInfo(FILE *fd = stdout);
  void fprintError(int err, char *file, int line, char *userstring);

  int runno() { return runnum; }
  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };
  FILE *logfd; //file handle for log file
  char err_string[MX_MESSAGE][30];

  int MemUsed();              

protected:
  char *DATAP;             // Pointer to the memory mapped buffer
  int event_size;

private:
  int fd;            // -1 if the event is in memory
  char *MMAPP;        // Begining of memory mapping
  
  long next_event_offset;

  int errnum;
  char errstr0[250];
  int runnum;
  // later storage for detector buffers
};


// Declaration for the factories
RichEventReader *getRichEventReader(int fd, long offset, int MMap=1);

#endif
