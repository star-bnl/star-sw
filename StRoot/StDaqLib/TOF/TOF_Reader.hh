/***************************************************************************
* $Id: TOF_Reader.hh,v 2.3 2012/06/11 16:36:22 fisyak Exp $
* Author: Frank Geurts
***************************************************************************
* Description:  TOF Event Reader
***************************************************************************
* $Log: TOF_Reader.hh,v $
* Revision 2.3  2012/06/11 16:36:22  fisyak
* std namespace
*
* Revision 2.2  2005/04/12 17:22:36  dongx
* Update for year 5 new data format, written by Jing Liu.
* Previous interfaces are separated out for convenience.
*
* Revision 2.1  2004/01/28 02:47:45  dongx
* change for year4 run (pVPD+TOFp+TOFr')
*  - Addtional TOFr' ADCs and TDCs put in
*  - Add TOTs of TOFr' in, combined in TDCs
*
*
* Revision 2.0  2003/01/29 05:27:25  geurts
* New TOF reader capable of reading TOF year3 data (pVPD, TOFp and TOFr).
* - Added dedicated retrieval methods for different parts of the data.
* - Reader is still capable of dealing year2 (pVPD and TOFp) data.
*
* Revision 1.3  2001/09/28 18:45:43  llope
* modified for compatibility w/ updated StTofMaker
*
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
#include <vector>

// Number of channels for each bank
// - Year2 Maxima
//#define TOFP_NUM_ADC_CHANNELS 48
//#define TOFP_NUM_TDC_CHANNELS 48
// - Year3 Maxima
// #define TOF_MAX_ADC_CHANNELS (48+12+72)
// #define TOF_MAX_TDC_CHANNELS (48+72)
// #define TOF_MAX_A2D_CHANNELS 32
// #define TOF_MAX_SCA_CHANNELS 12
// - Year4 Maxima
#define TOF_MAX_ADC_CHANNELS (48+12+120)
//Jing Liu #define TOF_MAX_TDC_CHANNELS (48+120+16)
#define TOF_MAX_TDC_CHANNELS (198)
#define TOF_MAX_TOT_CHANNELS 10
#define TOF_MAX_A2D_CHANNELS 32
#define TOF_MAX_SCA_CHANNELS 12

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
  //Jing Liu, 02/17/2005, for tofr5
  Pointer DDLRPTR[4];
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
//Jing Liu, for tofr5
struct TOFDDLR : public Bank {
  unsigned int data[1];
};


// Jing Liu, tofr5 raw data structure.
//----------------------------------------------------
struct TofRawHit {
  unsigned int EventNumber;
  unsigned short fiberid;           // 0 1 2
  unsigned short globaltdcchan;        // 0,1,...,191,...,198       192 tray channels + 6 pvpd channels
  unsigned int tdc;                     // tdc value
};
const int LEADING=4;
const int TRAILING=5;
//
// TOFp Processed Raw Data structure
struct TofDATA {
  char * BankType;
  unsigned int ByteSwapped ; // Should be 0x04030201
  unsigned int EventNumber; //Token number
  unsigned short AdcData[TOF_MAX_ADC_CHANNELS];
  unsigned short TdcData[TOF_MAX_TDC_CHANNELS];
  	   short A2dData[TOF_MAX_A2D_CHANNELS];
  unsigned short ScaData[TOF_MAX_SCA_CHANNELS];
  // Jing Liu, use std::vector to save all hits(include multi-hits)
  std::vector<TofRawHit> TofLeadingHits;
  std::vector<TofRawHit> TofTrailingHits;
  // use array to save hits, w/o multi-hits
  unsigned int LdTdcData[TOF_MAX_TDC_CHANNELS];
  unsigned int TrTdcData[TOF_MAX_TDC_CHANNELS];
  unsigned short LdNHit[TOF_MAX_TDC_CHANNELS];
  unsigned short TrNHit[TOF_MAX_TDC_CHANNELS];
  // end tofr5, Jing Liu
};

//-----------------------------------------------------

class StTofReaderInterface {
public:
  virtual ~StTofReaderInterface(){}
  // old virtual access members
  virtual unsigned short GetAdcFromSlat(int)=0;
  virtual unsigned short GetTdcFromSlat(int)=0;
  virtual          short GetTc(int)=0;
  virtual unsigned short GetSc(int)=0;
  virtual unsigned int GetEventNumber()=0;
  virtual unsigned short GetTofpAdc(int)=0;
  virtual unsigned short GetTofpTdc(int)=0;
  virtual unsigned short GetTofrAdc(int)=0;
  virtual unsigned short GetTofrTdc(int)=0;

  // Jing Liu FY05 --- some new virtual access members
  // get the tdc for each channel
  virtual unsigned int GetLdTdc(int)=0;
  virtual unsigned int GetTrTdc(int)=0;
  virtual unsigned int GetLdmTdc(int,int)=0;
  virtual unsigned int GetTrmTdc(int,int)=0;
  //
  virtual unsigned int GetPvpdLdTdc(int)=0;
  virtual unsigned int GetPvpdTrTdc(int)=0;
  virtual unsigned int GetPvpdLdmTdc(int,int)=0;
  virtual unsigned int GetPvpdTrmTdc(int,int)=0;
  //
  virtual unsigned short GetNLdHits(int)=0;
  virtual unsigned short GetNTrHits(int)=0;
  //
  virtual unsigned int GetNLeadingHits()=0;
  virtual unsigned int GetLeadingEventNumber(int)=0;
  virtual unsigned short GetLeadingFiberId(int)=0;
  virtual unsigned short GetLeadingGlobalTdcChan(int)=0;
  virtual unsigned int GetLeadingTdc(int)=0;
  virtual unsigned int GetNTrailingHits()=0;
  virtual unsigned int GetTrailingEventNumber(int)=0;
  virtual unsigned short GetTrailingFiberId(int)=0;
  virtual unsigned short GetTrailingGlobalTdcChan(int)=0;
  virtual unsigned int GetTrailingTdc(int)=0;
};


class TOF_Reader : public StTofReaderInterface {
private:
  void ProcessEvent(const Bank_TOFP * TofPTR);
  int mTofRawDataVersion;
  int mMaxAdcChannels, mMaxTdcChannels;
  int mMaxA2dChannels, mMaxScaChannels;
  TofDATA mTheTofArray;
  // unpack function, Jing Liu, 03/15/2005
  int UnpackYear2to4Data(const Bank_TOFP * TofPTR);
  int UnpackYear5Data(const Bank_TOFP * TofPTR);


protected:
  EventReader *ercpy; // copy of EventReader pointer
  struct Bank_TOFP *pBankTOFP;  // Bank Pointers
public:
  TOF_Reader(EventReader *er, Bank_TOFP *pTOFP);
  ~TOF_Reader(){};
  // old access member
  unsigned short GetAdcFromSlat(int slatId);
  unsigned short GetTdcFromSlat(int slatId);
  // new access members
  unsigned short GetAdc(int id);
  unsigned short GetTdc(int id);
  unsigned short GetTofrAdc(int padId);
  unsigned short GetTofrTdc(int padId);
  unsigned short GetTofpAdc(int SlatId);
  unsigned short GetTofpTdc(int SlatId);
  unsigned short GetPvpdAdcHigh(int id);
  unsigned short GetPvpdAdc(int id);
  unsigned short GetPvpdTdc(int id);
  unsigned short GetClockAdc();
  unsigned short GetTofrTOT(int totId);
  //
  // Jing Liu, FY05 new access members.
  unsigned int GetLdTdc(int id);
  unsigned int GetTrTdc(int id);
  unsigned int GetLdmTdc(int id ,int n);
  unsigned int GetTrmTdc(int id,int n);
  //
  unsigned int GetPvpdLdTdc(int id);
  unsigned int GetPvpdTrTdc(int id );
  unsigned int GetPvpdLdmTdc(int id,int n);
  unsigned int GetPvpdTrmTdc(int id,int n);
  //
  unsigned short GetNLdHits(int id);
  unsigned short GetNTrHits(int id);
  //
  unsigned int GetNLeadingHits();
  unsigned int GetLeadingEventNumber(int ihit);
  unsigned short GetLeadingFiberId(int ihit);
  unsigned short GetLeadingGlobalTdcChan(int ihit);
  unsigned int GetLeadingTdc(int ihit);
  unsigned int GetNTrailingHits();
  unsigned int GetTrailingEventNumber(int ihit);
  unsigned short GetTrailingFiberId(int ihit);
  unsigned short GetTrailingGlobalTdcChan(int ihit);
  unsigned int GetTrailingTdc(int ihit);

  bool year2Data();
  bool year3Data();
  bool year4Data();
  //Jing Liu
  bool year5Data();

  short GetTc(int chanId);
  unsigned short GetSc(int chanId);
  unsigned int   GetEventNumber();  
  void printRawData();
};

TOF_Reader *getTOFReader(EventReader *er);

#endif
