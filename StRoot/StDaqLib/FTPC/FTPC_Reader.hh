/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for FTPC (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to FTPCP bank
 *
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef FTPC_READER_HH
#define FTPC_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Detector Reader Virtual Class


struct  Bank_FTPP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class FTPC_Reader : public DetectorReader
{
  friend class EventReader;

public:
  FTPC_Reader *getFTPCReader(int sector){cout <<"DUMMY implementation"<<endl; return FALSE;};
  ZeroSuppressedReader *getZeroSuppressedReader(int sector){cout<<sector<<endl; return FALSE;};
  ADCRawReader *getADCRawReader(int sector){cout<<sector<<endl; return FALSE;};
  PedestalReader *getPedestalReader(int sector){cout<<sector<<endl; return FALSE;};
  PedestalRMSReader *getPedestalRMSReader(int sector){cout<<sector<<endl; return FALSE;};
  GainReader *getGainReader(int sector){cout<<sector<<endl; return FALSE;};
  CPPReader *getCPPReader(int sector){cout<<sector<<endl; return FALSE;};
  BadChannelReader *getBadChannelReader(int sector){cout<<sector<<endl; return FALSE;};
  FTPC_Reader(EventReader *er, Bank_FTPP *pftp){
    pBankFTPP = pftp ; // copy arg into class variable
    ercpy = er; // squirrel away pointer eventreader for our friends
    if (!pBankFTPP->test_CRC()) ERROR(ERR_CRC);
    if (pBankFTPP->swap() < 0) ERROR(ERR_SWAP);
    pBankFTPP->header.CRC = 0;
  };

  ~FTPC_Reader(){}; 

  int MemUsed(){return FALSE;};


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_FTPP *pBankFTPP;


  // Useful functions
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

