/***************************************************************************
 * $Id: TPCV2P0_Reader.hh,v 1.5 1999/07/22 17:56:27 levine Exp $
 * Author: Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description: common definitions for TPC
 *      
 *
 *   change log
 * 09-Jul-99 MJL added pointer to Bank_TPCP as arg to TPC_Reader constructor
 *               removed navigation code from TPC_Reader
 *
 ***************************************************************************
 * $Log: TPCV2P0_Reader.hh,v $
 * Revision 1.5  1999/07/22 17:56:27  levine
 * add TPCMZCLD (mezz cluster pointer bank) description
 *
 * Revision 1.4  1999/07/10 21:31:26  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.3  1999/07/04 01:43:25  levine
 * minor changes to make solaris CC compiler happy
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
#ifndef TPCV2P0_READER_HH
#define TPCV2P0_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"

// actual number of pads in each row
#define TPC_ROWS_PER_SECTOR	45

static const unsigned char tpcRowLen[TPC_ROWS_PER_SECTOR] = {
88,96,104,112,118,126,134,142,150,158,166,174,182,98,100,102,104,106,
106,108,110,112,112,114,116,118,120,122,122,124,126,128,128,130,132,134,
136,138,138,140,142,144,144,144,144} ;


//embed version number in bank name
#ifdef classname
#undef classname
#endif
#define classname(x) x ## V2P0 

// Detector Reader Virtual Class

struct  classname(Bank_TPCP);
struct  classname(Bank_TPCSECP);
struct  classname(Bank_TPCRBP);
struct  classname(Bank_TPCMZP);

class  TPCV2P0_PADK_SR ;

struct classname(Bank_TPCADCD) ;
struct classname(Bank_TPCSEQD) ;
struct classname(Bank_TPCADCX) ;
struct classname(Bank_TPCPADK) ;
struct classname(Bank_TPCCPPR) ;
struct classname(Bank_TPCADCR) ;
struct classname(Bank_TPCCFGR) ;
struct classname(Bank_TPCPEDR) ;
struct classname(Bank_TPCRMSR) ;
struct classname(Bank_TPCGAINR) ;
struct classname(Bank_TPCBADR) ;
struct classname(Bank_TPCMZCLD) ;


class TPCV2P0_Reader : public DetectorReader
{
  friend class EventReader;
  friend class TPCV2P0_ZS_SR;
  friend class TPCV2P0_ADCR_SR;
  friend class TPCV2P0_PEDR_SR;
  friend class TPCV2P0_PRMS_SR;
  friend class TPCV2P0_G_SR;
  friend class TPCV2P0_CPP_SR;
  friend class TPCV2P0_BC_SR;
  friend class TPCV2P0_CR_SR;
  friend class TPCV2P0_PADK_SR;

public:
  ZeroSuppressedReader *getZeroSuppressedReader(int sector);
  ADCRawReader *getADCRawReader(int sector);
  PedestalReader *getPedestalReader(int sector);
  PedestalRMSReader *getPedestalRMSReader(int sector);
  GainReader *getGainReader(int sector);
  CPPReader *getCPPReader(int sector);
  BadChannelReader *getBadChannelReader(int sector);
  ConfigReader *getConfigReader(int sector);

  TPCV2P0_Reader(EventReader *er, classname(Bank_TPCP) *ptpc);
  ~TPCV2P0_Reader(); 

  int MemUsed();

protected:
  //  MemoryManager MemMan;

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  classname(Bank_TPCP) *pBankTPCP;
  classname(Bank_TPCSECP) *getBankTPCSECP(int hypersector);
  classname(Bank_TPCRBP) *getBankTPCRBP(int interleaved_rb, classname(Bank_TPCSECP) *pBank_TPCSECP);
  classname(Bank_TPCMZP) *getBankTPCMZP(int mz, classname(Bank_TPCRBP) *pBank_TPCRBP);
  classname(Bank_TPCMZP) *getBankTPCMZP(int sector, int rb, int mz);
  classname(Bank_TPCMZCLD) *getBankTPCMZCLD(int sector, int rb, int mz);
  // Sector Reader Buffers
  TPCV2P0_PADK_SR *getPADKReader(int sector);
  TPCV2P0_PADK_SR *padk[24];   // One PADK for each sector

  // Useful functions
  classname(Bank_TPCADCD) *getBankTPCADCD(int sector, int rb, int mz);
  classname(Bank_TPCSEQD) *getBankTPCSEQD(int sector, int rb, int mz);
  classname(Bank_TPCADCX) *getBankTPCADCX(int sector, int rb, int mz);
  classname(Bank_TPCPADK) *getBankTPCPADK(int sector, int rb, int mz);
  classname(Bank_TPCCPPR) *getBankTPCCPPR(int sector, int rb, int mz);
  classname(Bank_TPCADCR) *getBankTPCADCR(int sector, int rb, int mz);
  classname(Bank_TPCCFGR) *getBankTPCCFGR(int sector, int rb, int mz);
  classname(Bank_TPCPEDR) *getBankTPCPEDR(int sector, int rb, int mz);
  classname(Bank_TPCRMSR) *getBankTPCRMSR(int sector, int rb, int mz);
  classname(Bank_TPCGAINR) *getBankTPCGAINR(int sector, int rb, int mz);
  classname(Bank_TPCBADR) *getBankTPCBADR(int sector, int rb, int mz);

  // Buffer and index functions for the various readers.
  // Initially these will do nothing.  Add functionality 
  // to increase performance
  int InformBuffers(ZeroSuppressedReader *, int sector) { return FALSE; };
  int InformBuffers(ADCRawReader *,int sector) { return FALSE; };
  int InformBuffers(PedestalReader *,int sector) { return FALSE; };
  int InformBuffers(PedestalRMSReader *,int sector) { return FALSE; };
  int InformBuffers(GainReader *,int sector) { return FALSE; };
  int InformBuffers(CPPReader *,int sector) { return FALSE; };
  int InformBuffers(BadChannelReader *,int sector) { return FALSE; };
  int InformBuffers(ConfigReader *,int sector) { return FALSE; };

  int AttachBuffers(ZeroSuppressedReader *, int sector) { return FALSE; };
  int AttachBuffers(ADCRawReader *, int sector) { return FALSE; };
  int AttachBuffers(PedestalReader *, int sector) { return FALSE; };
  int AttachBuffers(PedestalRMSReader *, int sector) { return FALSE; };
  int AttachBuffers(GainReader *, int sector) { return FALSE; };
  int AttachBuffers(CPPReader *, int sector) { return FALSE; };
  int AttachBuffers(BadChannelReader *, int sector) { return FALSE; };
  int AttachBuffers(ConfigReader *, int sector) { return FALSE; };
};

#endif
