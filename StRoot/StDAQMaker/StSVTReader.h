/***************************************************************************
 *
 * $Id: 
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SVT reader classes
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
#ifndef _StSVTReader_
#define _StSVTReader_

#ifndef __CINT__
#include "StDAQReader.h" 
#include "StTPCReader.h" 
#endif /*__CINT__*/

class  StSVTReader
{
  public:

  StSVTReader(StDAQReader *rd);
  virtual ~StSVTReader();
  virtual int close();

  virtual int getAnodeList(int Barrel, int Ladder, int Wafer, int Hybrid, unsigned char *&anodeList);
      // Fills (*anodeList[]) with the list of anode numbers containing hits
      // returns number of anodes in (*anodeList)[]
      // or negative if call fails
  
  virtual int getSequences(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, int &nSeq,
			   TPCSequence *&SeqData);
      	//  Fills (*SeqData)[] along with the ADC
      	// buffers pointed to by (*SeqData)[]
      	// Set nSeq to the # of elements in the (*SeqData)[] array
      	// returns 0 if OK.
      	// or negative if call fails

  virtual int getRawADC(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, int &nArray,
                        unsigned char *&Array);
	// Fills (*Array)[] with Raw data
	// Fills nArray with the # of elements in (*Array)[] (128 bytes / SVT)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getPedestals(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, int &nArray,
                           unsigned char *&Array);

	// Fills (*Array)[] with Pedestal data
	// Fills nArray with the # of elements in Array (128 bytes for SVT)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getRMSPedestals(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, int &nArray,
                              unsigned char *&Array);

	// Fills (*Array)[] with Pedestal RMS data * 16
	// Fills nArray with the # of elements in (*Array)[] (128 bytes / SVT)
	// returns 0 if OK.
	// returns negative if call fails


  virtual int getGain(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, TPCGain *&gain);
	// sets (*gain) to a valid gain structure pointer
	// returns 0 if OK
	// returns negative if call fails


//  virtual int getMeanGain();
      // returns mean gain

//  virtual int getGainEvents();
	// returns the number of events the calculation is based upon

  virtual int getClusters(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode, int &nClusters, 
			  TPCCluster *&clusters);
	// sets (*clusters) to beginning of array of clusters
	// sets nClusters to the length of the array
	// returns 0 if OK
	// returns negative if call fails

  virtual int IsBad(int Barrel, int Ladder, int Wafer, int Hybrid, int Anode);
	// returns true if the anode is bad.  
	// returns false if the anode is not bad.

  int getSCAZero(){return mSCAZero;}
  int getTimeZero(){return mTimeZero;}
  virtual void Update();
protected:

  virtual int setWafer(int Barrel, int Ladder, int Wafer);
  int getWaferIndex(int Barrel, int Ladder, int Wafer); 

  StDAQReader 		*fDAQReader;
  DetectorReader 	*fSVTImpReader;
  ZeroSuppressedReader 	*fZeroSuppressedReader;
  ADCRawReader 		*fADCRawReader;
  PedestalReader 	*fPedestalReader;
  PedestalRMSReader 	*fPedestalRMSReader;
  GainReader 		*fGainReader;
  CPPReader 		*fCPPReader;
  BadChannelReader 	*fBadChannelReader;

  int fBarrel, fLadder, fWafer, fHybrid;

  int mSCAZero;
  int mTimeZero;
};

#endif
