/***************************************************************************
 *
 * $Id: SVTV1P0_Reader.hh,v 1.2 2001/04/18 19:47:25 ward Exp $ 
 *
 * Author: M.J. LeVine, Marcelo Munhoz, J. Schambach
 ***************************************************************************
 *
 * Description: common definitions for SVT
 *
 ***************************************************************************
 *
 * $Log: SVTV1P0_Reader.hh,v $
 * Revision 1.2  2001/04/18 19:47:25  ward
 * StDaqLib/SVT stuff from Jo Schambach.
 *
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 * 
 *
 **************************************************************************/
#ifndef SVTV1P0_READER_HH
#define SVTV1P0_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"

//embed version number in bank name
#ifdef classname
#undef classname
#endif
#define classname(x) x ## V1P0

// Detector Reader Virtual Class

struct  classname(Bank_SVTP);
struct  classname(Bank_SVTSECP);
struct  classname(Bank_SVTRBP);
struct  classname(Bank_SVTMZP);

class  SVTV1P0_ANODK_SR ;

struct classname(Bank_SVTADCD) ;
struct classname(Bank_SVTSEQD) ;
struct classname(Bank_SVTADCX) ;
struct classname(Bank_SVTANODK) ;
struct classname(Bank_SVTCPPR) ;
struct classname(Bank_SVTADCR) ;
struct classname(Bank_SVTCFGR) ;
struct classname(Bank_SVTPEDR) ;
struct classname(Bank_SVTRMSR) ;
struct classname(Bank_SVTGAINR) ;
struct classname(Bank_SVTBADR) ;
struct classname(Bank_SVTMZCLD) ;


class SVTV1P0_Reader : public DetectorReader
{
  friend class EventReader;
  friend class SVTV1P0_ZS_SR;
  friend class SVTV1P0_ADCR_SR;
  friend class SVTV1P0_PEDR_SR;
  friend class SVTV1P0_PRMS_SR;
  friend class SVTV1P0_G_SR;
  friend class SVTV1P0_CPP_SR;
  friend class SVTV1P0_BC_SR;
  friend class SVTV1P0_CR_SR;
  friend class SVTV1P0_ANODK_SR;

public:
  ZeroSuppressedReader *getZeroSuppressedReader(int wafer);
  ADCRawReader         *getADCRawReader(int wafer);
  PedestalReader       *getPedestalReader(int wafer);
  PedestalRMSReader    *getPedestalRMSReader(int wafer);
  GainReader           *getGainReader(int wafer);
  CPPReader            *getCPPReader(int wafer);
  BadChannelReader     *getBadChannelReader(int wafer);
  ConfigReader         *getConfigReader(int wafer);

  ZeroSuppressedReader *getZeroSuppressedReader(int barrel, int ladder, int wafer);
  ADCRawReader         *getADCRawReader(int barrel, int ladder, int wafer);
  PedestalReader       *getPedestalReader(int barrel, int ladder, int wafer);
  PedestalRMSReader    *getPedestalRMSReader(int barrel, int ladder, int wafer);
  GainReader           *getGainReader(int barrel, int ladder, int wafer);
  CPPReader            *getCPPReader(int barrel, int ladder, int wafer);
  BadChannelReader     *getBadChannelReader(int barrel, int ladder, int wafer);
  ConfigReader         *getConfigReader(int barrel, int ladder, int wafer);
  
  SVTV1P0_Reader(EventReader *er, classname(Bank_SVTP) *psvt);
  ~SVTV1P0_Reader(); 

  int getSCAZero(){return mSCAZero;}
  int getTimeZero(){return mTimeZero;}

  int MemUsed();

protected:
  //  MemoryManager MemMan;

  // Number of SCA capacitor corresponding to time bucket zero
  int mSCAZero;
  // Time zero given by the read out
  int mTimeZero;

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  classname(Bank_SVTP)     *pBankSVTP;
  classname(Bank_SVTSECP)  *getBankSVTSECP(int hypersector); // hypersector = 1 or 2 
                                                            // (each corresponds to 12 sectors or RDO systems or RB)
  classname(Bank_SVTRBP)   *getBankSVTRBP(int interleaved_rb, classname(Bank_SVTSECP) *pBank_SVTSECP);
  classname(Bank_SVTMZP)   *getBankSVTMZP(int mz, classname(Bank_SVTRBP) *pBank_SVTRBP);
  classname(Bank_SVTMZP)   *getBankSVTMZP(int hypersector, int rb, int mz); // each rb corresponds to one sector or RDO box
  classname(Bank_SVTMZCLD) *getBankSVTMZCLD(int hypersector, int rb, int mz);
  // sector Reader Buffers
  SVTV1P0_ANODK_SR *getANODKReader();
  SVTV1P0_ANODK_SR *anodk;   // One ANODK for the SVT

  // Useful functions
  classname(Bank_SVTADCD)  *getBankSVTADCD(int hypersector, int rb, int mz);
  classname(Bank_SVTSEQD)  *getBankSVTSEQD(int hypersector, int rb, int mz);
  classname(Bank_SVTADCX)  *getBankSVTADCX(int hypersector, int rb, int mz);
  classname(Bank_SVTANODK) *getBankSVTANODK(int hypersector, int rb, int mz);
  classname(Bank_SVTCPPR)  *getBankSVTCPPR(int hypersector, int rb, int mz);
  classname(Bank_SVTADCR)  *getBankSVTADCR(int hypersector, int rb, int mz);
  classname(Bank_SVTCFGR)  *getBankSVTCFGR(int hypersector, int rb, int mz);
  classname(Bank_SVTPEDR)  *getBankSVTPEDR(int hypersector, int rb, int mz);
  classname(Bank_SVTRMSR)  *getBankSVTRMSR(int hypersector, int rb, int mz);
  classname(Bank_SVTGAINR) *getBankSVTGAINR(int hypersector, int rb, int mz);
  classname(Bank_SVTBADR)  *getBankSVTBADR(int hypersector, int rb, int mz);

  // Buffer and index functions for the various readers.
  // Initially these will do nothing.  Add functionality 
  // to increase performance
  int InformBuffers(ZeroSuppressedReader *, int wafer) { return FALSE; };
  int InformBuffers(ADCRawReader *,int wafer) { return FALSE; };
  int InformBuffers(PedestalReader *,int wafer) { return FALSE; };
  int InformBuffers(PedestalRMSReader *,int wafer) { return FALSE; };
  int InformBuffers(GainReader *,int wafer) { return FALSE; };
  int InformBuffers(CPPReader *,int wafer) { return FALSE; };
  int InformBuffers(BadChannelReader *,int wafer) { return FALSE; };
  int InformBuffers(ConfigReader *,int wafer) { return FALSE; };

  int InformBuffers(ZeroSuppressedReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(ADCRawReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(PedestalReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(PedestalRMSReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(GainReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(CPPReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(BadChannelReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int InformBuffers(ConfigReader *, int barrel, int ladder, int wafer) { return FALSE; };

  int AttachBuffers(ZeroSuppressedReader *, int wafer) { return FALSE; };
  int AttachBuffers(ADCRawReader *, int wafer) { return FALSE; };
  int AttachBuffers(PedestalReader *, int wafer) { return FALSE; };
  int AttachBuffers(PedestalRMSReader *, int wafer) { return FALSE; };
  int AttachBuffers(GainReader *, int wafer) { return FALSE; };
  int AttachBuffers(CPPReader *, int wafer) { return FALSE; };
  int AttachBuffers(BadChannelReader *, int wafer) { return FALSE; };
  int AttachBuffers(ConfigReader *, int wafer) { return FALSE; };

  int AttachBuffers(ZeroSuppressedReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(ADCRawReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(PedestalReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(PedestalRMSReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(GainReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(CPPReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(BadChannelReader *, int barrel, int ladder, int wafer) { return FALSE; };
  int AttachBuffers(ConfigReader *, int barrel, int ladder, int wafer) { return FALSE; };
};

#endif
