/***************************************************************************
 * $Id: FTPV1P0_Reader.hh,v 1.3 2001/06/25 22:59:27 jcs Exp $
 * Author: Jeff Landgraf, M.J. LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description: common definitions for FTPC
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0_Reader.hh,v $
 * Revision 1.3  2001/06/25 22:59:27  jcs
 * create copy of EventReader pointer
 *
 * Revision 1.2  2001/06/19 20:51:22  jeromel
 * Commited for Janet S.
 *
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
 *
 **************************************************************************/
#ifndef FTPV1P0_READER_HH
#define FTPV1P0_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
//embed version number in bank name
#ifdef classname
#undef classname
#endif
#define classname(x) x ## V1P0 

struct  classname(Bank_FTPP);
struct  classname(Bank_FTPCHAP);
struct  classname(Bank_FTPRBP);
struct  classname(Bank_FTPAZIP);
struct  classname(Bank_FTPMZP);
struct  classname(Bank_FTPMZP);

class FTPV1P0_PADK_SR;

struct  classname(Bank_FTPADCD);
struct  classname(Bank_FTPSEQD);
struct  classname(Bank_FTPADCX);
struct  classname(Bank_FTPPADK);
struct  classname(Bank_FTPCPPR);
struct  classname(Bank_FTPADCR);
struct  classname(Bank_FTPCFGR);
struct  classname(Bank_FTPPEDR);
struct  classname(Bank_FTPRMSR);
struct  classname(Bank_FTPGAINR);
struct  classname(Bank_FTPBADR);

// Detector Reader Virtual Class
class FTPV1P0_Reader : public DetectorReader
{
  friend class EventReader;
  friend class FTPV1P0_ZS_SR;
  friend class FTPV1P0_ADCR_SR;
  friend class FTPV1P0_PEDR_SR;
  friend class FTPV1P0_PRMS_SR;
  friend class FTPV1P0_G_SR;
  friend class FTPV1P0_CPP_SR;
  friend class FTPV1P0_BC_SR;
  friend class FTPV1P0_CR_SR;
  friend class FTPV1P0_PADK_SR;

public:
  ZeroSuppressedReader *getZeroSuppressedReader(int sector);
  ADCRawReader *getADCRawReader(int sector);
  PedestalReader *getPedestalReader(int sector);
  PedestalRMSReader *getPedestalRMSReader(int sector);
  GainReader *getGainReader(int sector);
  CPPReader *getCPPReader(int sector);
  BadChannelReader *getBadChannelReader(int sector);
  ConfigReader *getConfigReader(int sector);

  FTPV1P0_Reader(EventReader *er, classname(Bank_FTPP) *pftp);
  ~FTPV1P0_Reader(); 

  int MemUsed();

protected:
  //  MemoryManager MemMan;

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  classname(Bank_FTPP) *pBankFTPP;
  classname(Bank_FTPCHAP) *getBankFTPCHAP(int sector);
  classname(Bank_FTPRBP) *getBankFTPRBP(int sector, classname(Bank_FTPCHAP) *pBank_FTPCHAP);
  classname(Bank_FTPAZIP) *getBankFTPAZIP(int sector, classname(Bank_FTPRBP) *pBank_FTPRBP);
  classname(Bank_FTPMZP) *getBankFTPMZP(int sector, classname(Bank_FTPAZIP) *pBank_FTPAZIP);
  classname(Bank_FTPMZP) *getBankFTPMZP(int sector, classname(Bank_FTPRBP) *pBank_FTPRBP);
  classname(Bank_FTPMZP) *getBankFTPMZP(int sector);

  // Sector Reader Buffers
  FTPV1P0_PADK_SR *getPADKReader(int sector);
  FTPV1P0_PADK_SR *padk[FTP_SECTORS];   // One PADK for each sector

  // Useful functions
  classname(Bank_FTPADCD) *getBankFTPADCD(int sector);
  classname(Bank_FTPSEQD) *getBankFTPSEQD(int sector);
  classname(Bank_FTPADCX) *getBankFTPADCX(int sector);
  classname(Bank_FTPPADK) *getBankFTPPADK(int sector);
  classname(Bank_FTPCPPR) *getBankFTPCPPR(int sector);
  classname(Bank_FTPADCR) *getBankFTPADCR(int sector);
  classname(Bank_FTPCFGR) *getBankFTPCFGR(int sector);
  classname(Bank_FTPPEDR) *getBankFTPPEDR(int sector);
  classname(Bank_FTPRMSR) *getBankFTPRMSR(int sector);
  classname(Bank_FTPGAINR) *getBankFTPGAINR(int sector);
  classname(Bank_FTPBADR) *getBankFTPBADR(int sector);

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





