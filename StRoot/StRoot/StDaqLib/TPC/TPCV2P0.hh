/***************************************************************************
 * $Id: TPCV2P0.hh,v 1.4 2004/03/04 21:51:29 ward Exp $
 * Author: Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description: declarations For TPC version 2.0
 *      
 *
 *   change log
 * 22-Jun-99 MJL added TPCV2P0_CPP_SR::getAsicParams(ASIC_params *);
 * 25-Jun-99 MJL remove #include "MemMan.hh"
 * 20-Jul-99 MJL add #include "Error.hh"
 *
 ***************************************************************************
 * $Log: TPCV2P0.hh,v $
 * Revision 1.4  2004/03/04 21:51:29  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.3  1999/07/21 21:32:38  levine
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
 * Revision 1.2  1999/07/02 04:43:24  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/

#ifndef TPCV2P0_HH
#define TPCV2P0_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/Error.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "TPCV2P0.Banks.hh"
//#include "MemMan.hh"
#include "TPCV2P0_Reader.hh"

class TPCV2P0_Reader;
class TPCV2P0_PADK_SR;

// The sector reader virtual classes

// Zero-suppressed reader
class TPCV2P0_ZS_SR : public ZeroSuppressedReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nSeq, Sequence **SeqData);
  int getSpacePts(int PadRow, int *nSpacePts, SpacePt **SpacePts);

  int MemUsed();

  TPCV2P0_ZS_SR(int sector, TPCV2P0_Reader *,char mergeSequences);
  int initialize();
  int getFeeSequences(int Fee, int Pin, int *nSeq, Sequence **SeqData);
  
  ~TPCV2P0_ZS_SR();


private:
  classname(Bank_TPCADCD) *adcd_p[6][3];
  classname(Bank_TPCADCX) *adcx_p[6][3];
  classname(Bank_TPCSEQD) *seqd_p[6][3];
  classname(Bank_TPCMZCLD) *cld_p[6][3];
  
  char mMergeSequences;
  int sector;
  int nsptrow[TPC_PADROWS]; // num space pts each pad row
  SpacePt *RowSpacePts[TPC_PADROWS]; // pointer to SpacePt array for each padrow 
  struct Pad Pad_array[TPC_PADROWS][TPC_MAXPADS];
  struct PadRow Row_array[TPC_PADROWS];
  u_char padlist[TPC_PADROWS][TPC_MAXPADS];

  TPCV2P0_Reader *detector;
  TPCV2P0_PADK_SR *padkr;

};


// Reads Raw ADC values
class TPCV2P0_ADCR_SR : public ADCRawReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int MemUsed();
  
  TPCV2P0_ADCR_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_ADCR_SR();

private:
  int sector;
  TPCV2P0_Reader *detector;
  TPCV2P0_PADK_SR *padkr;
  u_char padlist[TPC_PADROWS][TPC_MAXPADS];

  classname(Bank_TPCADCR) *banks[6][3];
};

// Reads the Pedestal values
class TPCV2P0_PEDR_SR : public PedestalReader
{
private:
  int sector;
  TPCV2P0_Reader *detector;
  TPCV2P0_PADK_SR *padkr;
  classname(Bank_TPCPEDR) *banks[6][3];
  u_char padlist[TPC_PADROWS][TPC_MAXPADS];
  int numEvents;

public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  TPCV2P0_PEDR_SR(int sector, TPCV2P0_Reader *);  int initialize();
  ~TPCV2P0_PEDR_SR();
};

// The RMS pedestal values
class TPCV2P0_PRMS_SR : public PedestalRMSReader
{
private:
  int sector;
  TPCV2P0_Reader *detector;
  TPCV2P0_PADK_SR *padkr;
  classname(Bank_TPCRMSR) *banks[6][3];
  u_char padlist[TPC_PADROWS][TPC_MAXPADS];
  int numEvents;

public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  TPCV2P0_PRMS_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_PRMS_SR();
};

// The gain reader
class TPCV2P0_G_SR : public GainReader
{
private:
  int sector;
  TPCV2P0_Reader *detector;

public:
  int getGain(int PadRow, int Pad, struct Gain **gain);
  int getMeanGain();
  int getNumberOfEvents();

  int MemUsed();

  TPCV2P0_G_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_G_SR();
};

// Reads Cluster Pointer Pairs from the ASIC
class TPCV2P0_CPP_SR : public CPPReader
{
public:
  int getClusters(int PadRow, int Pad, int *nClusters, 
			  struct ASIC_Cluster **clusters);
  int getAsicParams(ASIC_params *);
  int MemUsed();
  
  TPCV2P0_CPP_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_CPP_SR();

private:
  int sector;
  TPCV2P0_Reader *detector;
  TPCV2P0_PADK_SR *padkr;
  classname(Bank_TPCCPPR) *banks[6][3];
};

// Reads the bad channels
class TPCV2P0_BC_SR : public BadChannelReader
{
private:
  int sector;

public:
  int IsBad(int PadRow, int Pad);
	
  int MemUsed();

  TPCV2P0_BC_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_BC_SR();
};

// Read the front end electronics configuration
class TPCV2P0_CR_SR : public ConfigReader
{
private:
  int sector;

public:
  int FEE_id(int PadRow, int Pad) = 0;
	// returns FEE_id

  int MemUsed();
  
  TPCV2P0_CR_SR(int sector, TPCV2P0_Reader *);
  int initialize();
  ~TPCV2P0_CR_SR();
};

struct PADK_entry
{
  short offset;
  short mz;        // mz and rb are counted up from 1
  short rb;
};

class TPCV2P0_PADK_SR
{
public:
  TPCV2P0_PADK_SR(int sector, TPCV2P0_Reader *);
  int initialize();

  void place(short padrow, short pad, PADK_entry *);
  void get(short padrow, short pad, PADK_entry *);
  
  int getADCBytes() { return 512; };
  int getCPPBytes() { return 128; };
  int getPEDBytes() { return 512; };
  int getRMSBytes() { return 512; };
  int getCFGBytes() { return 1; };
  int getGAINBytes() { return 4; };

private:
  int sector;
  TPCV2P0_Reader *detector;

  short packed_address[TPC_PADROWS][TPC_MAXPADS];  
  
  short pack(short rcb, short mz, short offset);
  void unpack(PADK_entry *, short paddress);
}; 

#endif





