/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for SSD (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to SSDP bank
 *
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef SSD_READER_HH
#define SSD_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Detector Reader Virtual Class


struct  Bank_SSDP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class SSD_Reader : public DetectorReader
{
  friend class EventReader;

public:
  SSD_Reader *getSSDReader(int sector){cout <<"DUMMY implementation"<<endl;};
  ZeroSuppressedReader *getZeroSuppressedReader(int sector){cout<<sector<<endl; return FALSE;};
  ADCRawReader *getADCRawReader(int sector){cout<<sector<<endl; return FALSE;};
  PedestalReader *getPedestalReader(int sector){cout<<sector<<endl; return FALSE;};
  PedestalRMSReader *getPedestalRMSReader(int sector){cout<<sector<<endl; return FALSE;};
  GainReader *getGainReader(int sector){cout<<sector<<endl; return FALSE;};
  CPPReader *getCPPReader(int sector){cout<<sector<<endl; return FALSE;};
  BadChannelReader *getBadChannelReader(int sector){cout<<sector<<endl; return FALSE;};
  SSD_Reader(EventReader *er){
    cout <<"DUMMY implementation"<<endl;
    ercpy = er; // squirrel away pointer eventreader for our friends
  // Fix up DATAP
    pBankDATAP = (Bank_DATAP *)er->getDATAP();

    if (!pBankDATAP->test_CRC()) ERROR(ERR_CRC);
    if (pBankDATAP->swap() < 0) ERROR(ERR_SWAP);
    pBankDATAP->header.CRC = 0;

    // position independent pointers to lower banks, variable DATAP length
    int len = pBankDATAP->header.BankLength - sizeof(Bank_Header)/4;
    Pointer *ptr = &pBankDATAP->reserved_det; // change RecHeaderFormats.hh someday
    for (int i=0; i<len; i++, ptr++) {
      if (ptr->length==0) continue;//invalid entry
      pBankSSDP = (Bank_SSDP *)(((INT32 *)pBankDATAP)+ (ptr->offset)); 
      if(!strncmp(pBankSSDP->header.BankType,"SSDP",4)) break;
    }
    if(strncmp(pBankSSDP->header.BankType,"SSDP",4)) {
      printf("detector SSD not found in DATAP\n");
      exit(0);
    }

    if (!pBankSSDP->test_CRC()) ERROR(ERR_CRC);
    if (pBankSSDP->swap() < 0) ERROR(ERR_SWAP);
    pBankSSDP->header.CRC = 0;
  };

  ~SSD_Reader(){}; 

  int MemUsed(){return FALSE;};


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_SSDP *pBankSSDP;


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
