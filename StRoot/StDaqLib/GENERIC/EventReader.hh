#ifndef EVENTREADER_HH
#define EVENTREADER_HH


//   change log
// 06-June-99 MJL added EventInfo struct, changed method getEventInfo()
// 06-June-99 MJL added printEventInfo()

#include <sys/types.h>

#include <string>

#include "RecHeaderFormats.hh"
#include "Error.hh"

#define TRUE 1
#define FALSE 0

// Event Reader header files
// This file is included by Offline programs

class EventReader;

// Support Structures

// Information regarding the event
struct EventInfo // return from EventReader::getEventInfo()
{
  int EventLength;
  unsigned int UnixTime;
  unsigned int EventSeqNo;
  unsigned int TrigWord;
  unsigned int TrigInputWord;
  unsigned char TPCPresent;
  unsigned char SVTPresent;
  unsigned char TOFPresent;
  unsigned char EMCPresent;
  unsigned char SMDPresent;
  unsigned char FTPCPresent;
  unsigned char Reserved;
  unsigned char RICHPresent;
  unsigned char TRGDetectorsPresent;
  unsigned char L3Present;
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
  Pad *pad;
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
  char errstr0[50];

private:
  EventReader *er;
};

// Event Reader Class
class EventReader
{
public:
  EventReader();

  void InitEventReader(int fd, long offset, int mmap=1);  
                             // takes open file descripter-offset
  void InitEventReader(void *event);           // pointer to the event
      //  There is an ambiguity here.  The specifier may point to
      //  A logical record header, or it may point to a DATAP Bank
      //  This ambiguity must be resolved by these functions before
      //  They store the DATAP pointer

  long NextEventOffset();

  ~EventReader();

  char *getDATAP() { return DATAP; };
  struct EventInfo getEventInfo();
  void printEventInfo();

  int runno() { return runnum; }
  int errorNo() { return errnum; };
  string errstr() { return string(errstr0); };

  int MemUsed();              

protected:
  char *DATAP;             // Pointer to the memory mapped buffer
  int event_size;

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
  char errstr0[50];
  int runnum;
  // later storage for detector buffers
};

// Declaration for the factories
DetectorReader *getDetectorReader(EventReader *, string);
EventReader *getEventReader(int fd, long offset, int MMap=1);
EventReader *getEventReader(char *event);

#endif


