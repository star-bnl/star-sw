/***************************************************************************
 * $Id: EventReader.hh,v 1.25 2009/08/24 20:17:20 jml Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for all detectors
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
 * 27-Jun-00 MJL add token to EventInfo, change access functions
 *
 ***************************************************************************
 * $Log: EventReader.hh,v $
 * Revision 1.25  2009/08/24 20:17:20  jml
 * remove 1.57, install correct handling of detectors present
 *
 * Revision 1.24  2009/08/24 20:04:43  jml
 * changing back
 *
 * Revision 1.23  2009/08/19 19:06:37  jeromel
 * Basic fix for gcc 4 (explicit .h include needed)
 *
 * Revision 1.22  2009/01/08 22:14:49  fine
 * teach EventReader tp provide the new daqReader pointer
 *
 * Revision 1.21  2004/02/18 20:31:14  ward
 * There was a big mess.  I am trying to fix it.
 *
 * Revision 1.19  2003/12/24 21:55:57  perev
 * Cleanup
 *
 * Revision 1.18  2003/09/02 17:55:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.17  2002/12/09 18:54:23  ward
 * EMC stuff from Subhassis.
 *
 * Revision 1.16  2002/10/13 20:43:37  ward
 * Support for decoding DAQ100 data and writing it into a table.
 *
 * Revision 1.15  2002/01/17 18:29:55  jeromel
 * After I looked at the code, corrections from Akio (pass2).
 *
 * Revision 1.14  2002/01/17 17:29:26  jeromel
 *
 * Files:  CVS: DetectorReader.cxx EventReader.cxx EventReader.hh CVS: RecHeaderFormats.hh CVS: ----------------------------------------------------------------------
 * Modifications for FPD support
 *
 * Revision 1.13  2000/09/15 21:21:17  fisyak
 * No ulong on HP
 *
 * Revision 1.12  2000/08/28 22:19:12  ward
 * Skip corrupted events. StDaqLib/GENERIC/EventReader.cxx & StDAQMaker/StDAQReader.cxx.
 *
 * Revision 1.11  2000/06/27 07:27:25  levine
 * Added Token to EventInfo struct
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
#ifndef EVENTREADER_HH
#define EVENTREADER_HH

#include <Stiostream.h>

#include <sys/types.h>

#include <string>
#if !defined ST_NO_NAMESPACES
using std::string;
#endif

#if (__GNUC__ >= 4)
#   include <string.h>
#   include <stdlib.h>
#endif

#include "RecHeaderFormats.hh"
#include "Error.hh"

#define TRUE  1
#define FALSE 0



// Event Reader header files
// This file is included by Offline programs

class EventReader;

// Support Structures

// Information regarding the event
struct EventInfo // return from EventReader::getEventInfo()
{
  int EventLength;
  int Token;
  unsigned int  UnixTime;
  unsigned int  EventSeqNo;
  unsigned int  TrigWord;
  unsigned int  TrigInputWord;
  unsigned char TPCPresent;	 //   0
  unsigned char SVTPresent;	 //   1
  unsigned char TOFPresent;	 //   2
  unsigned char BTOWPresent;	 //   3  EMC Barrel Tower
  unsigned char FPDPresent;	 //   4
  unsigned char FTPCPresent;	 //   5
  unsigned char EXTPresent;	 //   6 //   ignore
  unsigned char RICHPresent;	 //   7
  unsigned char TRGPresent;	 //   8
  unsigned char L3Present;	 //   9
  unsigned char SCPresent;	 //   10 //   reserved for Slow Controls
  unsigned char EXT2Present;	 //   11 //   ignore
  unsigned char PMDPresent;	 //   12
  unsigned char SSDPresent;	 //   13
  unsigned char ETOWPresent;	 //   14 EMC EndCup Tower
  unsigned char DAQPresent;	 //   15 //   ignore
  unsigned char FP2Present;	 //   16 //   reserved for future FPD
  unsigned char PPPresent;	 //   17//         ignore
  unsigned char BSMDPresent;	 //   18 EMC Barrel Shower
  unsigned char ESMDPresent;	 //   19 EMC Endcup Shower

  unsigned char EMCPresent;      //  BTOW || ETOW || BSMD || ESMD

  void printEventInfo(FILE *fd=stdout);
};

// Each sequence contains one hit (zero suppressed data)
struct Sequence
{
  u_short startTimeBin;
  u_short Length;
  u_char *FirstAdc;
};

// Each pad contains an array of hits for that pad (zero suppressed data)
struct Pad
{
  u_char nseq;
  Sequence *seq;
};

// A pad row contains an array of pads (zero suppressed data)
struct PadRow
{
  u_short npads;
  struct Pad *pad;
};


// Gain structure
struct Gain
{
  int t0;          // t0 * 16
  int t0_rms;      // t0_rms * 16
  int rel_gain;    // rel_gain * 64
};

struct ASIC_Cluster
{
  short start_time_bin;
  short stop_time_bin;
};


struct Centroids {
  unsigned short x; // units: 1/64 pad 
  unsigned short t; // units: 1/64 timebin
}; 

struct SpacePt {
  Centroids centroids;
  unsigned short flags;
  unsigned short q;
};



// The sector reader virtual classes
class ZeroSuppressedReader
{
public:
  virtual int getPadList(int PadRow, u_char **padList)=0;
      // Fills (*padList[]) with the list of pad numbers containing hits
      // returns number of pads in (*padList)[]
      // or negative if call fails

  virtual int getSequences(int PadRow, int Pad, int *nSeq,
			   Sequence **SeqData)=0;
      //  Fills (*SeqData)[] along with the ADC
      // buffers pointed to by (*SeqData)[]
      // Set nSeq to the # of elements in the (*SeqData)[] array
      // returns 0 if OK.
      // or negative if call fails

// Read the clusters (space points) found in the mezzanine cluster-finder
  virtual int getSpacePts(int PadRow, int *nSpacePts, SpacePt **SpacePts)=0;
      // Fills (*SpacePts)[] along with the 
      // buffers pointed to by (*SpacePts)[]
      // Set nSpacePts to the # of elements in the (*SpacePts)[] array
      // returns 0 if OK.
      // or negative if call fails

  virtual int MemUsed()=0;
  virtual ~ZeroSuppressedReader() {};
};

// Reads Raw ADC values
class ADCRawReader
{
public:
  virtual int getPadList(int PadRow, unsigned char **padList)=0;
	// As for Zero suppressed data, this returns
 	// the list of pads for which data can be obtained
	// Therefore, the padList will always contain all of the
	// pads in the specified PadRow regardless of the data
	
  virtual int getSequences(int PadRow, int Pad, int *nArray, u_char **Array)=0;
	// Fills (*Array)[] with Raw data
	// Fills nArray with the # of elements in (*Array)[] (512 bytes / TPC)
	// returns 0 if OK.
	// returns negative if call fails

  virtual int MemUsed()=0;
  virtual ~ADCRawReader() {};
};

// Reads the Pedestal values
class PedestalReader
{
public:
  virtual int getPadList(int PadRow, unsigned char **padList)=0;
	// As for Zero suppressed data, this returns
 	// the list of pads for which data can be obtained
	// Therefore, the padList will always contain all of the
	// pads in the specified PadRow regardless of the data
	
  virtual int getSequences(int PadRow, int Pad, int *nArray, u_char **Array)=0;
	// Fills (*Array)[] with Pedestal data
	// Fills nArray with the # of elements in Array (512 bytes for TPC)
	// returns 0 if OK.
	// returns negative if call fails

  virtual int getNumberOfEvents()=0;
 	// returns the number of events the pedestal run based on

  virtual int MemUsed()=0;
  virtual ~PedestalReader() {};
};

// The RMS pedestal values
class PedestalRMSReader
{
public:
  virtual int getPadList(int PadRow, u_char **padList)=0;
	// As for Zero suppressed data, this returns
 	// the list of pads for which data can be obtained
	// Therefore, the (*padList)[] will always contain all of the
	// pads in the specified PadRow regardless of the data
	
  virtual int getSequences(int PadRow, int Pad, int *nArray, u_char **Array)=0;
	// Fills (*Array)[] with Pedestal RMS data * 16
	// Fills nArray with the # of elements in (*Array)[] (512 bytes / TPC)
	// returns 0 if OK.
	// returns negative if call fails

  virtual int getNumberOfEvents()=0;
 	// returns the number of events the pedestal run based on

  virtual int MemUsed()=0;
  virtual ~PedestalRMSReader() {};
};

// The gain reader
class GainReader
{
public:
  virtual int getGain(int PadRow, int Pad, struct Gain **gain)=0;
	// sets (*gain) to a valid gain structure pointer
	// returns 0 if OK
	// returns negative if call fails

  virtual int getMeanGain()=0;
      // returns mean gain

  virtual int getNumberOfEvents()=0;
	// returns the number of events the calculation is based upon

  virtual int MemUsed()=0;
  virtual ~GainReader() {};
};

// Reads Cluster Pointer Pairs from the ASIC
class CPPReader
{
public:
  virtual int getClusters(int PadRow, int Pad, int *nClusters, 
			  struct ASIC_Cluster **clusters)=0;
	// sets (*clusters) to beginning of array of clusters
	// sets nClusters to the length of the array
	// returns 0 if OK
	// returns negative if call fails

  virtual int getAsicParams(ASIC_params *)=0;

  virtual int MemUsed()=0;
  virtual ~CPPReader() {};
};

// Reads the bad channels
class BadChannelReader
{
public:
  virtual int IsBad(int PadRow, int Pad)=0;
	// returns true if the pad is bad.  
	// returns false if the pad is not bad.
	
  virtual int MemUsed()=0;
  virtual ~BadChannelReader() {};
};

// Read the front end electronics configuration
class ConfigReader
{
public:
  virtual int FEE_id(int PadRow, int Pad) = 0;
	// returns FEE_id

  virtual int MemUsed()=0;
  virtual ~ConfigReader() {};
};


// Detector Reader Virtual Class
class DetectorReader
{
  friend class EventReader;
  friend class StTPCReader; // Herb Oct 2002 for DAQ100.

public:
  virtual ZeroSuppressedReader *getZeroSuppressedReader(int sector)=0;
  virtual ADCRawReader *getADCRawReader(int sector)=0;
  virtual PedestalReader *getPedestalReader(int sector)=0;
  virtual PedestalRMSReader *getPedestalRMSReader(int sector)=0;
  virtual GainReader *getGainReader(int sector)=0;
  virtual CPPReader *getCPPReader(int sector)=0;
  virtual BadChannelReader *getBadChannelReader(int sector)=0;

  virtual ~DetectorReader() { };

  virtual int MemUsed()=0;

  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };

protected:

  // Buffer and index functions for the various readers.
  // Initially these will do nothing.  Add functionality 
  // to increase performance
  virtual int InformBuffers(ZeroSuppressedReader *, int sector)=0;
  virtual int InformBuffers(ADCRawReader *,int sector)=0;
  virtual int InformBuffers(PedestalReader *,int sector)=0;
  virtual int InformBuffers(PedestalRMSReader *,int sector)=0;
  virtual int InformBuffers(GainReader *,int sector)=0;
  virtual int InformBuffers(CPPReader *,int sector)=0;
  virtual int InformBuffers(BadChannelReader *,int sector)=0;
  virtual int InformBuffers(ConfigReader *,int sector)=0;

  virtual int AttachBuffers(ZeroSuppressedReader *, int sector)=0;
  virtual int AttachBuffers(ADCRawReader *, int sector)=0;
  virtual int AttachBuffers(PedestalReader *, int sector)=0;
  virtual int AttachBuffers(PedestalRMSReader *, int sector)=0;
  virtual int AttachBuffers(GainReader *, int sector)=0;
  virtual int AttachBuffers(CPPReader *, int sector)=0;
  virtual int AttachBuffers(BadChannelReader *, int sector)=0;
  virtual int AttachBuffers(ConfigReader *, int sector)=0;

  int errnum;
  char errstr0[250];
  void *motherPointerBank; // Herb Oct 2002 for DAQ100.

private:
  EventReader *er;
};

class daqReader;
// Event Reader Class
class EventReader
{
public:
  EventReader();
  EventReader(const char *logfilename);

  void InitEventReader(int fd, long offset, int mmap);  
                             // takes open file descripter-offset
                             // works on MAPPED file
  void InitEventReader(int fd, long offset);  
                             // takes open file descripter-offset
                             // works on file
  void InitEventReader(void *event);           // pointer to the event
      //  There is an ambiguity here.  The specifier may point to
      //  A logical record header, or it may point to a DATAP Bank
      //  This ambiguity must be resolved by these functions before
      //  They store the DATAP pointer

  long NextEventOffset();
  void setVerbose(int); // 0 turns off all internal printout
  char * findBank(char *bankid); // navigates to pointer bnk below DATAP
  int verbose;

  ~EventReader();

  char *getDATAP() { return DATAP; };
  daqReader *getDaqReader() { return fDaqReader; };
  void setDaqReader(daqReader *rdr) { fDaqReader = rdr; }
  struct EventInfo getEventInfo();
  int system_present(Bank_DATAP *datap, int sys);

  void printEventInfo(FILE *fd=stdout);
  void fprintError(int err, char *file, int line, char *userstring);

  int runno() { return runnum; }
  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };
  FILE *logfd; //file handle for log file
  char err_string[MX_MESSAGE][30];

  int MemUsed();              
  char eventIsCorrupted(int fdes,long offsetInFile); // Herb, Aug 28 2000

protected:
  char *DATAP;             // Pointer to the memory mapped buffer
  int event_size;
  daqReader *fDaqReader;   // the new reader instance
  // Detector Buffering Functions
  int InformBuffers(DetectorReader *) { return FALSE; };
        // returns false.  
        // later will be used to give EventReader a detectors buffers
  int AttachBuffers(DetectorReader *) { return FALSE; };
        // returns false.
        // later will be used to give buffers back to DetectorReader

private:
  int fd;            // -1 if the event is in memory
  char *MMAPP;        // Begining of memory mapping
  
  long next_event_offset;

  int errnum;
  char errstr0[250];
  int runnum;
  char *ConvertToString(unsigned long  *input); // Herb, Aug 28 2000
  void WhereAreThePointers(int *beg,int *end,char *xx); // Herb, Aug 28 2000
  void Swap4(unsigned long *data); // Herb, Aug 28 2000
  char BankOrItsDescendentsIsBad(int fd,long currentOffset); // Herb, Aug 28 2000
  char mLastBank[30]; //!
  int mWordIndex; //!
  // later storage for detector buffers
};

//#include "../RICH/RICH_Reader.hh"

// Declaration for the factories
DetectorReader *getDetectorReader(EventReader *, string);
EventReader *getEventReader(int fd, long offset, int MMap=1);
EventReader *getEventReader(int fd, long offset, const char *logfile, int MMap=1);
EventReader *getEventReader(char *event);
// declared in RICH_Reader.hh
// RICH_Reader *getRichReader(EventReader *er);



#endif
