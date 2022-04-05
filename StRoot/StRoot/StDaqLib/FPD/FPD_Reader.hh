/***************************************************************************
* $Id: FPD_Reader.hh,v 1.1 2002/01/17 17:19:59 akio Exp $
* Author: Akio Ogawa
***************************************************************************
* Description:  FPD Event Reader
***************************************************************************
* $Log: FPD_Reader.hh,v $
* Revision 1.1  2002/01/17 17:19:59  akio
* First version of FPD daq reader
*
**************************************************************************/
#ifndef FPD_READER_HH
#define FPD_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Number of channels for each bank
#define FPDP_NUM_ADC_CHANNELS 256
#define FPDP_NUM_TDC_CHANNELS 8
#define FPDP_NUM_REG_CHANNELS 3
#define FPDP_NUM_PED_CHANNELS 256
#define FPDP_NUM_SCL_CHANNELS 128
#define FPDP_BBC_NUM_ADC_CHANNELS 72
#define FPDP_BBC_NUM_PED_CHANNELS 128
#define FPDP_BBC_NUM_SCL_CHANNELS 32

// unsigned typedefs from /RTS/include/daqFormats.h
#ifndef _DAQ_FORMATS_H
typedef unsigned int   UINT32;
typedef unsigned short UINT16;
typedef unsigned char  UINT8 ;
#endif


// FPDp Raw Data Banks
struct Bank_FPDP: public Bank {
  Pointer AdcPTR;
  Pointer TdcPTR;
  Pointer RegPTR;
  Pointer PedPTR;
  Pointer SclPTR;
  Pointer BbcAdcPTR;
  Pointer BbcPedPTR;
  Pointer BbcSclPTR;
};

struct FPDDATA: public Bank {
  unsigned short data[1];
};
struct FPDSCLDATA: public Bank {
  unsigned int data[1];
};

// FPDp Processed Raw Data structure
struct FpdDATA {
  char * BankType;
  unsigned int ByteSwapped ; // Should be 0x04030201
  unsigned int EventNumber;  //Token number
  unsigned short AdcData[FPDP_NUM_ADC_CHANNELS];
  unsigned short TdcData[FPDP_NUM_TDC_CHANNELS];
  unsigned short RegData[FPDP_NUM_REG_CHANNELS];
  unsigned int   PedData[FPDP_NUM_PED_CHANNELS];
  unsigned short SclData[FPDP_NUM_SCL_CHANNELS];
  unsigned short BbcAdcData[FPDP_BBC_NUM_ADC_CHANNELS];
  unsigned short BbcPedData[FPDP_BBC_NUM_PED_CHANNELS];
  unsigned int   BbcSclData[FPDP_BBC_NUM_SCL_CHANNELS];
};

class StFpdReaderInterface{
public:
  virtual ~StFpdReaderInterface(){}
  virtual unsigned short GetAdc(int)=0;
  virtual unsigned short GetTdc(int)=0;
  virtual unsigned short GetReg(int)=0;
  virtual unsigned short GetPed(int)=0;
  virtual unsigned int   GetScl(int)=0;
  virtual unsigned short GetBbcAdc(int)=0;
  virtual unsigned short GetBbcPed(int)=0;
  virtual unsigned int   GetBbcScl(int)=0;
  virtual unsigned int   GetEventNumber()=0;
  virtual void printRawData()=0;
};

class FPD_Reader : public StFpdReaderInterface {
private:
  void ProcessEvent(const Bank_FPDP * FpdPTR);
  FpdDATA mTheFpdArray;
protected:
  EventReader *ercpy; // copy of EventReader pointer
  struct Bank_FPDP *pBankFPDP;  // Bank Pointers
public:
  FPD_Reader(EventReader *er, Bank_FPDP *pFPDP);
  ~FPD_Reader(){};
  unsigned short GetAdc(int);
  unsigned short GetTdc(int);
  unsigned short GetReg(int);
  unsigned short GetPed(int);
  unsigned int   GetScl(int);
  unsigned short GetBbcAdc(int);
  unsigned short GetBbcPed(int);
  unsigned int   GetBbcScl(int);
  unsigned int   GetEventNumber();  
  void printRawData();
};

FPD_Reader *getFPDReader(EventReader *er);

#endif
