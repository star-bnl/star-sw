/***************************************************************************
* $Id: TOF_Reader.hh,v 1.2 2001/07/13 21:12:34 geurts Exp $
* Author: Frank Geurts
***************************************************************************
* Description:  TOF Event Reader
***************************************************************************
* $Log: TOF_Reader.hh,v $
* Revision 1.2  2001/07/13 21:12:34  geurts
* changed type of A2D data in struct TofDATA to allow negative values
*
* Revision 1.1  2001/07/08 21:43:41  geurts
* First release
*
**************************************************************************/
#ifndef TOF_READER_HH
#define TOF_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Number of channels for each bank
#define TOFP_NUM_ADC_CHANNELS 48
#define TOFP_NUM_TDC_CHANNELS 48
#define TOFP_NUM_A2D_CHANNELS 32
#define TOFP_NUM_SCA_CHANNELS 12

// unsigned typedefs from /RTS/include/daqFormats.h
#ifndef _DAQ_FORMATS_H
typedef unsigned int   UINT32;
typedef unsigned short UINT16;
typedef unsigned char  UINT8 ;
#endif


// TOFp raw data structures
#ifdef UNIX_LITTLE_ENDIAN
typedef union {
  unsigned int data;
  struct {
    UINT8 slot;
    UINT8 channel;
    UINT16 data;
  } adc;
} tofadc;
typedef union {
  unsigned int data;
  struct {
    UINT8 slot;
    UINT8 channel;
    UINT16 data;
  } tdc;
} toftdc;
typedef union {
  unsigned int data;
  struct {
    UINT8 slot;
    UINT8 channel;
    short data;  // can be negative
  } a2d;
} tofa2d;
typedef union {
  unsigned int data;
  struct {
    unsigned int channel:8;
    unsigned int data:24;
  } sca;
} tofsca;
#else
typedef union {
  unsigned int data;
  struct {
    UINT16 data;
    UINT8 channel;
    UINT8 slot;
  } adc;
} tofadc;
typedef union {
  unsigned int data;
  struct {
    UINT16 data;
    UINT8 channel;
    UINT8 slot;
  } tdc;
} toftdc;
typedef union {
  unsigned int data;
  struct {
    short data;  // can be negative
    UINT8 channel;
    UINT8 slot;
  } a2d;
} tofa2d;
typedef union {
  unsigned int data;
  struct {
    unsigned int data:24;
    unsigned int channel:8;
  } sca;
} tofsca;
#endif

// TOFp Raw Data Banks
struct Bank_TOFP: public Bank {
  Pointer AdcPTR;
  Pointer TdcPTR;
  Pointer A2dPTR;
  Pointer ScaPTR;
};
struct TOFADCD: public Bank {
  tofadc data[1];
};
struct TOFTDCD: public Bank {
  //unsigned int packedData[1];
  toftdc data[1];
};
struct TOFA2DD: public Bank {
  tofa2d data[1];
};
struct TOFSCAD: public Bank {
  tofsca data[1];
};


// TOFp Processed Raw Data structure
struct TofDATA {
  char * BankType;
  unsigned int ByteSwapped ; // Should be 0x04030201
  unsigned int EventNumber; //Token number
  unsigned short AdcData[TOFP_NUM_ADC_CHANNELS];
  unsigned short TdcData[TOFP_NUM_TDC_CHANNELS];
  short A2dData[TOFP_NUM_A2D_CHANNELS];
  unsigned short ScaData[TOFP_NUM_SCA_CHANNELS];
};


class StTofReaderInterface {
public:
  virtual ~StTofReaderInterface(){}
  virtual unsigned short GetAdcFromSlat(int)=0;
  virtual unsigned short GetTdcFromSlat(int)=0;
  virtual unsigned int GetEventNumber()=0;
  // virtual void printRawData();
};


class TOF_Reader : public StTofReaderInterface {
private:
  void ProcessEvent(const Bank_TOFP * TofPTR);
  TofDATA mTheTofArray;
protected:
  EventReader *ercpy; // copy of EventReader pointer
  struct Bank_TOFP *pBankTOFP;  // Bank Pointers
public:
  TOF_Reader(EventReader *er, Bank_TOFP *pTOFP);
  ~TOF_Reader(){};
  unsigned short GetAdcFromSlat(int slatId);
  unsigned short GetTdcFromSlat(int slatId);
  unsigned int   GetEventNumber();  
  void printRawData();
};

TOF_Reader *getTOFReader(EventReader *er);

#endif
