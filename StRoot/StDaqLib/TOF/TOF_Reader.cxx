/***************************************************************************
* $Id: TOF_Reader.cxx,v 2.0 2003/01/29 05:27:24 geurts Exp $
* Author: Frank Geurts
***************************************************************************
* Description:  TOF Event Reader
***************************************************************************
* $Log: TOF_Reader.cxx,v $
* Revision 2.0  2003/01/29 05:27:24  geurts
* New TOF reader capable of reading TOF year3 data (pVPD, TOFp and TOFr).
* - Added dedicated retrieval methods for different parts of the data.
* - Reader is still capable of dealing year2 (pVPD and TOFp) data.
*
* Revision 1.2  2001/09/28 18:45:43  llope
* modified for compatibility w/ updated StTofMaker
*
* Revision 1.1  2001/07/08 21:41:58  geurts
* Revision 1.2  2001/09/28 13:13:13  llope
* First release
*
**************************************************************************/
#include "TOF_Reader.hh"

bool TOF_Reader::year2Data(){return (mTofRawDataVersion==1);}
bool TOF_Reader::year3Data(){return (mTofRawDataVersion==2);}

void TOF_Reader::ProcessEvent(const Bank_TOFP * TofPTR) {
  unsigned short numberOfDataWords, slot, channel;
  int dataDWord, value;
  //TofPTR->print();
  unsigned short Token = TofPTR->header.Token;
  if (Token==0){
    cout << "TOF_Reader: do not know how to handle token==0"<<endl;
    //return;
  }
  mTheTofArray.EventNumber = Token;
  mTheTofArray.ByteSwapped = 0x04030201;

  int tofRawDataVersion = TofPTR->header.FormatNumber;
  if ((tofRawDataVersion <1) || (tofRawDataVersion >2)){
    cout << "TOF_Reader: ERROR unknown raw data version " << tofRawDataVersion << endl;
    return;
  }
  //fg 1. introduce rawdata consistency checks below ...

  //Read ADC Bank
  TOFADCD * TofAdcD;
  if (TofPTR->AdcPTR.length>0) {
    TofAdcD = (TOFADCD *) ((unsigned long *)TofPTR + TofPTR->AdcPTR.offset);
    //TofAdcD->print();
    TofAdcD->swap();
    if (TofAdcD->header.Token!=Token){
      cout << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " ADCD " << TofAdcD->header.Token << endl;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofAdcD->header.BankLength - (INT32)sizeof(TofAdcD->header)/4;
    if (numberOfDataWords!=mMaxAdcChannels){
      cout << "TOF_Reader: ADCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>mMaxAdcChannels) numberOfDataWords=mMaxAdcChannels;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      //slot    = (int)TofAdcD->data[dataDWord].adc.slot;
      //channel = (int)TofAdcD->data[dataDWord].adc.channel; 
      //value   = (int)TofAdcD->data[dataDWord].adc.data;
      slot    = int( TofAdcD->data[dataDWord].data & 0x000000FF);
      channel = int((TofAdcD->data[dataDWord].data & 0x0000FF00) >> 8);
      value   = int((TofAdcD->data[dataDWord].data & 0xFFFF0000) >> 16);
      mTheTofArray.AdcData[dataDWord]=value;
    }    
  }

  //Read TDC Bank
  TOFTDCD * TofTdcD;
  if (TofPTR->TdcPTR.length>0) {
    TofTdcD = (TOFTDCD *) ((unsigned long *)TofPTR + TofPTR->TdcPTR.offset);
    //TofTdcD->print();
    TofTdcD->swap();
    if (TofTdcD->header.Token!=Token){
      cout << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " TDCD " << TofTdcD->header.Token << endl;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofTdcD->header.BankLength - (INT32)sizeof(TofTdcD->header)/4;
    if (numberOfDataWords!=mMaxTdcChannels){
      cout << "TOF_Reader: TDCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>mMaxTdcChannels) numberOfDataWords=mMaxTdcChannels;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      //slot    = (int)TofTdcD->data[dataDWord].tdc.slot;
      //channel = (int)TofTdcD->data[dataDWord].tdc.channel; 
      //value   = (int)TofTdcD->data[dataDWord].tdc.data;
      slot    = int( TofTdcD->data[dataDWord].data & 0x000000FF);
      channel = int((TofTdcD->data[dataDWord].data & 0x0000FF00) >> 8);
      value   = int((TofTdcD->data[dataDWord].data & 0xFFFF0000) >> 16);
      mTheTofArray.TdcData[dataDWord]=value;
    }    
  }

  //Read A2D Bank...
  TOFA2DD * TofA2dD;
  if (TofPTR->A2dPTR.length>0) {
    TofA2dD = (TOFA2DD *) ((unsigned long *)TofPTR + TofPTR->A2dPTR.offset);
    //TofA2dD->print();
    TofA2dD->swap();
    if (TofA2dD->header.Token!=Token){
      cout << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " A2DD " << TofA2dD->header.Token << endl;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofA2dD->header.BankLength - (INT32)sizeof(TofA2dD->header)/4;
    if (numberOfDataWords!=mMaxA2dChannels){
      cout << "TOF_Reader: A2DD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>mMaxA2dChannels) numberOfDataWords=mMaxA2dChannels;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      //slot    = (int)TofA2dD->data[dataDWord].a2d.slot;
      //channel = (int)TofA2dD->data[dataDWord].a2d.channel; 
      //value   = (int)TofA2dD->data[dataDWord].a2d.data;
      slot    = int( TofA2dD->data[dataDWord].data & 0x000000FF);
      channel = int((TofA2dD->data[dataDWord].data & 0x0000FF00) >> 8);
      value   = int(int(TofA2dD->data[dataDWord].data) >> 16);// A2D values might be negative
      mTheTofArray.A2dData[dataDWord]=value;
    }    
  }

  //Read SCA Bank
  TOFSCAD * TofScaD;
  if (TofPTR->ScaPTR.length>0) {
    TofScaD = (TOFSCAD *) ((unsigned long *)TofPTR + TofPTR->ScaPTR.offset);
    //TofScaD->print();
    TofScaD->swap();
    if (TofScaD->header.Token!=Token){
      cout << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " SCAD " << TofScaD->header.Token << endl;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofScaD->header.BankLength - (INT32)sizeof(TofScaD->header)/4;
    if (numberOfDataWords!=mMaxScaChannels){
      cout << "TOF_Reader: SCAD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>mMaxScaChannels) numberOfDataWords=mMaxScaChannels;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      //channel = (int)TofScaD->data[dataDWord].sca.channel; 
      //value   = (int)TofScaD->data[dataDWord].sca.data;
      channel = int( TofScaD->data[dataDWord].data & 0x000000FF);
      value   = int((TofScaD->data[dataDWord].data & 0xFFFFFF00) >> 8);
      mTheTofArray.ScaData[dataDWord]=value;
    }
  }
}


TOF_Reader::TOF_Reader(EventReader *er, Bank_TOFP *pTOFP)
{
  pBankTOFP = pTOFP; 
  ercpy = er;
  if (!pBankTOFP->test_CRC()) {
    printf("CRC error in TOFP: %s %d\n",__FILE__,__LINE__) ;
  }
  if (pBankTOFP->swap() < 0) {
    printf("swap error in TOFP: %s %d\n",__FILE__,__LINE__) ;
  }

  pBankTOFP->header.CRC = 0;
  int Token = pBankTOFP->header.Token;
  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP();
  if(Token !=dp->header.Token){
    printf("Token mismatch between global %d and TOF %d\n",dp->header.Token,Token);
  }

  mTofRawDataVersion = pBankTOFP->header.FormatNumber;

  // Year2: pVPD+TOFp; Year3:pVPD+TOFp+TOFr
  if (year2Data()){
    mMaxAdcChannels=48;
    mMaxTdcChannels=48;
  }
  else {
    mMaxAdcChannels=TOF_MAX_ADC_CHANNELS;
    mMaxTdcChannels=TOF_MAX_TDC_CHANNELS;
  }
  mMaxScaChannels = TOF_MAX_SCA_CHANNELS;
  mMaxA2dChannels = TOF_MAX_A2D_CHANNELS;

  ProcessEvent(pBankTOFP);
}


unsigned short TOF_Reader::GetAdc(int daqId){
  if ((daqId<0) || (daqId>mMaxAdcChannels-1)){
    cout << "TOF_Reader::GetAdc slatId out of range " << daqId << endl;
    return 0;
  }
  return mTheTofArray.AdcData[daqId];
}
unsigned short TOF_Reader::GetAdcFromSlat(int slatId){return GetAdc(slatId);}


unsigned short TOF_Reader::GetTdc(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    cout << "TOF_Reader::GetTdc daqId out of range " << daqId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[daqId];
}
unsigned short TOF_Reader::GetTdcFromSlat(int slatId){return GetTdc(slatId);}


unsigned short TOF_Reader::GetTofpAdc(int slatId){
  if ((slatId<0) || (slatId>41)){
    cout << "TOF_Reader::GetTofpAdc slatId out of range " << slatId << endl;
    return 0;
  }
  return mTheTofArray.AdcData[slatId];
}


unsigned short TOF_Reader::GetTofpTdc(int slatId){
  if ((slatId<0) || (slatId>41)){
    cout << "TOF_Reader::GetTofpTdc slatId out of range " << slatId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[slatId];
}


unsigned short TOF_Reader::GetTofrAdc(int padId){
  if (year2Data()){
    cout << "TOF_Reader: TOFr ADC data not available for year2 data" << endl;
    return 0;
  }
  if ((padId<0) || (padId>71)){
    cout << "TOF_Reader::GetTofrAdc padId out of range " << padId << endl;
    return 0;
  }
  return mTheTofArray.AdcData[60+padId];
}


unsigned short TOF_Reader::GetTofrTdc(int padId){
  if (year2Data()){
    cout << "TOF_Reader: TOFr TDC data not available for year2 data" << endl;
    return 0;
  }
  if ((padId<0) || (padId>71)){
    cout << "TOF_Reader::GetTofrTdc padId out of range " << padId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[48+padId];
}


unsigned short TOF_Reader::GetPvpdAdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    cout << "TOF_Reader::GetPvpdAdc pvpdId out of range " << pvpdId << endl;
    return 0;
  }
  return mTheTofArray.AdcData[43+pvpdId];
}


unsigned short TOF_Reader::GetPvpdAdcHigh(int pvpdId){
  if (year2Data()){
    cout << "TOF_Reader: pVPD high gain data not available for year2 data" << endl;
    return 0;
  }
  if ((pvpdId<0) || (pvpdId>5)){
    cout << "TOF_Reader::GetPvpdAdcHigh pvpdId out of range " << pvpdId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[54+pvpdId];
}

unsigned short TOF_Reader::GetPvpdTdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    cout << "TOF_Reader::GetPvpdTdc pvpdId out of range " << pvpdId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[43+pvpdId];
}


unsigned short TOF_Reader::GetClockAdc(){return mTheTofArray.AdcData[42];}


short TOF_Reader::GetTc(int chanId){
  if ((chanId<0) || (chanId>mMaxA2dChannels-1)){
    cout << "TOF_Reader::GetTc chanId out of range " << chanId << endl;
    return 0;
  }
  return mTheTofArray.A2dData[chanId];
}


unsigned short TOF_Reader::GetSc(int chanId){
  if ((chanId<0) || (chanId>mMaxScaChannels-1)){
    cout << "TOF_Reader::GetSc chanId out of range " << chanId << endl;
    return 0;
  }
  return mTheTofArray.ScaData[chanId];
}


unsigned int TOF_Reader::GetEventNumber(){
  return mTheTofArray.EventNumber;
}

void TOF_Reader::printRawData(){
  cout << "StDaqLib/TOF/TOF_Reader  Printing Raw Data...";
  cout << "\n --- ADCD: ";
  for (int i=0;i<mMaxAdcChannels;i++) cout << " " << mTheTofArray.AdcData[i];
  cout << "\n --- TDCD: ";
  for (int i=0;i<mMaxTdcChannels;i++) cout << " " << mTheTofArray.TdcData[i];
  cout << "\n --- A2DD: ";
  for (int i=0;i<mMaxA2dChannels;i++) cout << " " << mTheTofArray.A2dData[i];
  cout << "\n --- SCAD: ";
  for (int i=0;i<mMaxScaChannels;i++) cout << " " << mTheTofArray.ScaData[i];
  cout << "\nStDaqLib/TOF/TOF_Reader  Done Printing Raw Data..." << endl;
}

