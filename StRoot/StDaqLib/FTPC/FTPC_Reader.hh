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


struct  Bank_FTPCP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class FTPC_Reader : public DetectorReader
{
  friend class EventReader;

public:
  FTPC_Reader *getFTPCReader(int sector){cout <<"DUMMY implementation"<<endl;};
  ZeroSuppressedReader *getZeroSuppressedReader(int sector){};
  ADCRawReader *getADCRawReader(int sector){};
  PedestalReader *getPedestalReader(int sector){};
  PedestalRMSReader *getPedestalRMSReader(int sector){};
  GainReader *getGainReader(int sector){};
  CPPReader *getCPPReader(int sector){};
  BadChannelReader *getBadChannelReader(int sector){};
  FTPC_Reader(EventReader *er){
    cout <<"DUMMY implementation"<<endl;
    ercpy = er; // squirrel away pointer eventreader for our friends
  // Fix up DATAP
    pBankDATAP = (Bank_DATAP *)er->getDATAP();

    if (!pBankDATAP->test_CRC()) ERROR(ERR_CRC);
    if (pBankDATAP->swap() < 0) ERROR(ERR_SWAP);
    pBankDATAP->header.CRC = 0;

    // position independent pointers to lower banks, variable DATAP length
    int len = pBankDATAP->header.BankLength - sizeof(Bank_Header)/4;
    Pointer *ptr = &pBankDATAP->FTPC;
    for (int i=0; i<len; i++, ptr++) {
      if (ptr->length==0) continue;//invalid entry
      pBankFTPCP = (Bank_FTPCP *)(((INT32 *)pBankDATAP)+ (ptr->offset)); 
      if(!strncmp(pBankFTPCP->header.BankType,"FTPCP",4)) break;
    }
    if(strncmp(pBankFTPCP->header.BankType,"FTPCP",4)) {
      printf("detector FTPC not found in DATAP\n");
      exit(0);
    }

    if (!pBankFTPCP->test_CRC()) ERROR(ERR_CRC);
    if (pBankFTPCP->swap() < 0) ERROR(ERR_SWAP);
    pBankFTPCP->header.CRC = 0;
  };

  ~FTPC_Reader(){}; 

  int MemUsed(){};


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_FTPCP *pBankFTPCP;


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
