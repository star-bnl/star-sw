/***************************************************************************
 * $Id: FTPV1P0.hh,v 1.1 2000/01/18 18:01:20 levine Exp $
 * Authors: M.J.LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description:  Declarations For FTPC version 1.0
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0.hh,v $
 * Revision 1.1  2000/01/18 18:01:20  levine
 * Hummler's implementaiton of FTPC reader. Note that method
 *
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,
 * 				   Sequence **SeqData)
 *
 * causes exit() since the required #include file has not yet been
 * (correctly) implemented.
 *
 *
 **************************************************************************/

#ifndef FTPCV1P0_HH
#define FTPCV1P0_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "FTPV1P0.Banks.hh"
//#include "MemMan.hh"
#include "FTPV1P0_Reader.hh"

class FTPV1P0_Reader;
class FTPV1P0_PADK_SR;

// The sector reader virtual classes
class FTPV1P0_ZS_SR : public ZeroSuppressedReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nSeq, Sequence **SeqData);
  int getSpacePts(int PadRow, int *nSpacePts, SpacePt **SpacePts);
  int MemUsed();

  FTPV1P0_ZS_SR(int s, FTPV1P0_Reader *det);
  int initialize();
  int getFeeSequences(int Fee, int Pin, int *nSeq, Sequence **SeqData);
  
  ~FTPV1P0_ZS_SR();

private:
  classname(Bank_FTPADCD) *adcd_p;
  classname(Bank_FTPADCX) *adcx_p;
  classname(Bank_FTPSEQD) *seqd_p;

  int sector;
  struct Pad Pad_array[FTP_PADROWS][FTP_MAXPADS];
  struct PadRow Row_array[FTP_PADROWS];
  u_char padlist[FTP_PADROWS][FTP_MAXPADS];

  FTPV1P0_Reader *detector;
  FTPV1P0_PADK_SR *padkr;

};

// Reads Raw ADC values
class FTPV1P0_ADCR_SR : public ADCRawReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int MemUsed();
  
  FTPV1P0_ADCR_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_ADCR_SR();

private:
  int sector;
  FTPV1P0_Reader *detector;
  FTPV1P0_PADK_SR *padkr;
  u_char padlist[FTP_PADROWS][FTP_MAXPADS];

  classname(Bank_FTPADCR) *bank;
};

// Reads the Pedestal values
class FTPV1P0_PEDR_SR : public PedestalReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  FTPV1P0_PEDR_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_PEDR_SR();

private:
  int sector;
  int numEvents;
  FTPV1P0_Reader *detector;
  FTPV1P0_PADK_SR *padkr;
  u_char padlist[FTP_PADROWS][FTP_MAXPADS];

  classname(Bank_FTPPEDR) *bank;
};

// The RMS pedestal values
class FTPV1P0_PRMS_SR : public PedestalRMSReader
{
public:
  int getPadList(int PadRow, unsigned char **padList);
  int getSequences(int PadRow, int Pad, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  FTPV1P0_PRMS_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_PRMS_SR();

private:
  int sector;
  int numEvents;
  FTPV1P0_Reader *detector;
  FTPV1P0_PADK_SR *padkr;
  u_char padlist[FTP_PADROWS][FTP_MAXPADS];

  classname(Bank_FTPRMSR) *bank;
};

// The gain reader
class FTPV1P0_G_SR : public GainReader
{
public:
  int getGain(int PadRow, int Pad, struct Gain **gain);
  int getMeanGain();
  int getNumberOfEvents();

  int MemUsed();

  FTPV1P0_G_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_G_SR();
};

// Reads Cluster Pointer Pairs from the ASIC
class FTPV1P0_CPP_SR : public CPPReader
{
public:
  int getClusters(int PadRow, int Pad, int *nClusters, 
			  struct ASIC_Cluster **clusters);
  int getAsicParams(ASIC_params *){return FALSE;};

  int MemUsed();
  
  FTPV1P0_CPP_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_CPP_SR();

private:
  int sector;
  FTPV1P0_Reader *detector;
  FTPV1P0_PADK_SR *padkr;
  classname(Bank_FTPCPPR) *bank;
};

// Reads the bad channels
class FTPV1P0_BC_SR : public BadChannelReader
{
public:
  int IsBad(int PadRow, int Pad);
	
  int MemUsed();

  FTPV1P0_BC_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_BC_SR();
};

// Read the front end electronics configuration
class FTPV1P0_CR_SR : public ConfigReader
{
public:
  int FEE_id(int PadRow, int Pad) = 0;
	// returns FEE_id

  int MemUsed();
  
  FTPV1P0_CR_SR(int sector, FTPV1P0_Reader *);
  int initialize();
  ~FTPV1P0_CR_SR();
};

struct FTPPADK_entry
{
  short offset;
};

class FTPV1P0_PADK_SR
{
public:
  FTPV1P0_PADK_SR(int sector, FTPV1P0_Reader *);
  int initialize();

  void place(short padrow, short pad, FTPPADK_entry *);
  void get(short padrow, short pad, FTPPADK_entry *);
  
  int getADCBytes() { return 512; };
  int getCPPBytes() { return 128; };
  int getPEDBytes() { return 512; };
  int getRMSBytes() { return 512; };
  int getCFGBytes() { return 1; };
  int getGAINBytes() { return 4; };

private:
  int sector;
  FTPV1P0_Reader *detector;

  short packed_address[FTP_PADROWS][FTP_MAXPADS];  
  
  short pack(short offset);
  void unpack(FTPPADK_entry *, short paddress);
}; 

#endif





