#ifndef _StDAQReader_
#define _StDAQReader_

//		Forward declarations
struct  EventInfo;
typedef  EventInfo DAQEventInfo;
class  EventReader;
class  DetectorReader;
class  ZeroSuppressedReader;
class  ADCRawReader;
class  PedestalReader;
class  PedestalRMSReader;
class  GainReader;
class  CPPReader;
class  BadChannelReader;
//


typedef struct TPCSequence
{
  unsigned short startTimeBin;
  unsigned short Length;
  unsigned char *FirstAdc;

}TPCSequence;

typedef struct TPCCluster
{
  short start_time_bin;
  short stop_time_bin;
}TPCCluster;



typedef struct TPCGain
{
  int t0;          // t0 * 16
  int t0_rms;      // t0_rms * 16
  int rel_gain;    // rel_gain * 64
}TPCGain;

class StTPCReader;
 
class StDAQReader 
{
friend class StTPCReader;
public:
  StDAQReader(const char *file=0);
  virtual ~StDAQReader();

  virtual int open(const char *file);
  virtual int close();
  virtual int readEvent();
  virtual int getRunNumber() const;
  virtual int getEventNumber() const;
  virtual unsigned int getUnixTime() const;
  virtual unsigned int getTrigWord() const;
  virtual unsigned int getTrigInputWord() const;
  
  virtual int TPCPresent()  const;  
  virtual int SVTPresent()  const; 
  virtual int TOFPresent()  const;
  virtual int EMCPresent()  const;
  virtual int SMDPresent()  const;
  virtual int FTPCPresent() const;
  virtual int RICHPresent() const;
  virtual int TRGDetectorsPresent() const;
  virtual int L3Present()   const;

  virtual void setTPCVersion(const char* vers = "TPCV2P0") {strcpy(fTPCVersion,vers);} 
  StTPCReader *getTPCReader(); 
  virtual void printEventInfo();

protected:
int fFd;	//File descriptor
EventReader *fEventReader;  
StTPCReader *fTPCReader;  
long fOffset;
DAQEventInfo *fEventInfo;
char *fFile;
char fTPCVersion[12];

};

class  StTPCReader
{
  public:
  friend class StDAQReader;

  StTPCReader(StDAQReader *rd);
  virtual ~StTPCReader();

  int getMaxPad(int PadRow) const;	//Number of pads in padrow

  virtual int getPadList(int Sector, int PadRow, unsigned char *&padList);
      // Fills (*padList[]) with the list of pad numbers containing hits
      // returns number of pads in (*padList)[]
      // or negative if call fails
  
  virtual int getSequences(int Sector, int PadRow, int Pad, int &nSeq,
			   TPCSequence *&SeqData);
      	//  Fills (*SeqData)[] along with the ADC
      	// buffers pointed to by (*SeqData)[]
      	// Set nSeq to the # of elements in the (*SeqData)[] array
      	// returns 0 if OK.
      	// or negative if call fails

  virtual int getRawADC(int Sector,int PadRow, int Pad, int &nArray,
                        unsigned char *&Array);
	// Fills (*Array)[] with Raw data
	// Fills nArray with the # of elements in (*Array)[] (512 bytes / TPC)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getPedestals(int Sector,int PadRow, int Pad, int &nArray,
                           unsigned char *&Array);

	// Fills (*Array)[] with Pedestal data
	// Fills nArray with the # of elements in Array (512 bytes for TPC)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getRMSPedestals(int Sector,int PadRow, int Pad, int &nArray,
                              unsigned char *&Array);

	// Fills (*Array)[] with Pedestal RMS data * 16
	// Fills nArray with the # of elements in (*Array)[] (512 bytes / TPC)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getGain(int Sector, int PadRow, int Pad, TPCGain *&gain);
	// sets (*gain) to a valid gain structure pointer
	// returns 0 if OK
	// returns negative if call fails


//  virtual int getMeanGain();
      // returns mean gain

//  virtual int getGainEvents();
	// returns the number of events the calculation is based upon

  virtual int getClusters(int Sector, int PadRow, int Pad, int &nClusters, 
			  TPCCluster *&clusters);
	// sets (*clusters) to beginning of array of clusters
	// sets nClusters to the length of the array
	// returns 0 if OK
	// returns negative if call fails

  virtual int IsBad(int Sector, int PadRow, int Pad);
	// returns true if the pad is bad.  
	// returns false if the pad is not bad.

protected:
  virtual void Update();
  virtual void setSector(int sector);

  StDAQReader 		*fDAQReader;
  DetectorReader 	*fTPCImpReader;
  ZeroSuppressedReader 	*fZeroSuppressedReader;
  ADCRawReader 		*fADCRawReader;
  PedestalReader 	*fPedestalReader;
  PedestalRMSReader 	*fPedestalRMSReader;
  GainReader 		*fGainReader;
  CPPReader 		*fCPPReader;
  BadChannelReader 	*fBadChannelReader;

  int fSector;
};
#endif
