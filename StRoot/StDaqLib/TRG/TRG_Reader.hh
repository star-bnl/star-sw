/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for TRG (dummy placeholder)
 *      
 *
 *   change log
 *
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef TRG_READER_HH
#define TRG_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"

// Detector Reader Virtual Class

struct  Bank_TRGP;

class TRG_Reader : public DetectorReader
{
  friend class EventReader;

public:
  TRG_Reader *getTRGReader(int sector){cout <<"DUMMY implementation"<<endl;};
  ZeroSuppressedReader *getZeroSuppressedReader(int sector){};
  ADCRawReader *getADCRawReader(int sector){};
  PedestalReader *getPedestalReader(int sector){};
  PedestalRMSReader *getPedestalRMSReader(int sector){};
  GainReader *getGainReader(int sector){};
  CPPReader *getCPPReader(int sector){};
  BadChannelReader *getBadChannelReader(int sector){};
  TRG_Reader(EventReader *er){cout <<"DUMMY implementation"<<endl;};
  ~TRG_Reader(){}; 

  int MemUsed(){};


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_TRGP *pBankTRGP;


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
