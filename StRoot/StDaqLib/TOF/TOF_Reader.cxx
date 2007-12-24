/***************************************************************************
* $Id: TOF_Reader.cxx,v 2.6 2007/12/24 06:04:29 fine Exp $
* Author: Frank Geurts
***************************************************************************
* Description:  TOF Event Reader
***************************************************************************
* $Log: TOF_Reader.cxx,v $
* Revision 2.6  2007/12/24 06:04:29  fine
* introduce OLDEVP namespace to allow ole and new EVP library concurrently
*
* Revision 2.5  2007/04/17 23:00:16  dongx
* replaced with standard STAR Loggers
*
* Revision 2.4  2005/04/13 16:02:38  dongx
* update for a warning message on the return value
*
* Revision 2.3  2005/04/12 17:22:36  dongx
* Update for year 5 new data format, written by Jing Liu.
* Previous interfaces are separated out for convenience.
*
* Revision 2.2  2004/08/07 02:43:32  perev
*  more test for corruption added
*
* Revision 2.1  2004/01/28 02:47:45  dongx
* change for year4 run (pVPD+TOFp+TOFr')
*  - Addtional TOFr' ADCs and TDCs put in
*  - Add TOTs of TOFr' in, combined in TDCs
*
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
#include <assert.h>
#include "TOF_Reader.hh"
#include "StMessMgr.h"
#include "TString.h"

using namespace OLDEVP;

bool TOF_Reader::year2Data(){return (mTofRawDataVersion==1);}
bool TOF_Reader::year3Data(){return (mTofRawDataVersion==2);}
bool TOF_Reader::year4Data(){return (mTofRawDataVersion==3);}
//Jing Liu
bool TOF_Reader::year5Data(){return (mTofRawDataVersion==0);}

void TOF_Reader::ProcessEvent(const Bank_TOFP * TofPTR) {

  int unpackerr=0;
  if(year2Data()||year3Data()||year4Data()) {
     unpackerr=UnpackYear2to4Data(TofPTR);
     if(unpackerr>0) LOG_INFO<<"TOF_READER::UnPack Year2-4 data ERROR!"<<endm;
  }

  if(year5Data()){ 
     unpackerr=UnpackYear5Data(TofPTR);
     if(unpackerr>0) LOG_INFO<<"TOF_READER::UnPack Year5 Data ERROR!"<<endm;
  }

}


TOF_Reader::TOF_Reader(EventReader *er, Bank_TOFP *pTOFP)
{
  pBankTOFP = pTOFP; 
  ercpy = er;
  if (!pBankTOFP->test_CRC()) {
    LOG_DEBUG << Form("CRC error in TOFP: %s %d",__FILE__,__LINE__) << endm;
  }
  if (pBankTOFP->swap() < 0) {
    LOG_DEBUG << Form("swap error in TOFP: %s %d",__FILE__,__LINE__) << endm;
  }

  pBankTOFP->header.CRC = 0;
  int Token = pBankTOFP->header.Token;
  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP();
  if(Token !=dp->header.Token){
    LOG_DEBUG << Form("Token mismatch between global %d and TOF %d",dp->header.Token,Token) << endm;
  }

  mTofRawDataVersion = pBankTOFP->header.FormatNumber;

  // Year2: pVPD+TOFp; Year3:pVPD+TOFp+TOFr(72); Year4: pVPD+TOFp+TOFr'(120)
  if (year2Data()){
    mMaxAdcChannels=48;
    mMaxTdcChannels=48;
  }
  else if (year3Data()) {
    mMaxAdcChannels=48+12+72;
    mMaxTdcChannels=48+72;
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
    LOG_INFO << "TOF_Reader::GetAdc slatId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.AdcData[daqId];
}
unsigned short TOF_Reader::GetAdcFromSlat(int slatId){return GetAdc(slatId);}


unsigned short TOF_Reader::GetTdc(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.TdcData[daqId];
}

unsigned short TOF_Reader::GetTdcFromSlat(int slatId){return GetTdc(slatId);}


unsigned short TOF_Reader::GetTofpAdc(int slatId){
  if ((slatId<0) || (slatId>41)){
    LOG_INFO << "TOF_Reader::GetTofpAdc slatId out of range " << slatId << endm;
    return 0;
  }
  return mTheTofArray.AdcData[slatId];
}


unsigned short TOF_Reader::GetTofpTdc(int slatId){
  if ((slatId<0) || (slatId>41)){
    LOG_INFO << "TOF_Reader::GetTofpTdc slatId out of range " << slatId << endm;
    return 0;
  }
  return mTheTofArray.TdcData[slatId];
}


unsigned short TOF_Reader::GetTofrAdc(int padId){
  if (year2Data()){
    LOG_INFO << "TOF_Reader: TOFr ADC data not available for year2 data" << endm;
    return 0;
  }
  if (year3Data()) {
    if((padId<0) || (padId>71)) {
      LOG_INFO << "TOF_Reader::GetTofrAdc padId out of range " << padId << endm;
      return 0;
    }
  }
  if (year4Data()) {
    if ((padId<0) || (padId>119)){
      LOG_INFO << "TOF_Reader::GetTofrAdc padId out of range " << padId << endm;
      return 0;
    }
  }
  return mTheTofArray.AdcData[60+padId];
}


unsigned short TOF_Reader::GetTofrTdc(int padId){
  if (year2Data()){
    LOG_INFO << "TOF_Reader: TOFr TDC data not available for year2 data" << endm;
    return 0;
  }
  if (year3Data()) {
    if((padId<0) || (padId>71)) {
      LOG_INFO << "TOF_Reader::GetTofrTdc padId out of range " << padId << endm;
      return 0;
    }
    return mTheTofArray.TdcData[48+padId];
  }
  if (year4Data()) {
    if ((padId<0) || (padId>119)){
      LOG_INFO << "TOF_Reader::GetTofrTdc padId out of range " << padId << endm;
      return 0;
    }
    int offset = 0;
    if(padId<24) {
      offset = 0;
    } else if(padId<48) {
      offset = 2;
    } else if(padId<72) {
      offset = 4;
    } else {
      offset = 8;
    }
    return mTheTofArray.TdcData[48+offset+padId];
  }
  assert(0); return 0;
}

unsigned short TOF_Reader::GetTofrTOT(int totId){
  if(year2Data()||year3Data()) {
    LOG_INFO << "TOF_Reader:: TOFr TOT data not available for year3 and year3 data" << endm;
    return 0;
  }
  if((totId<0) || (totId>9)) {
    LOG_INFO << "TOF_Reader::GetTofrTOT totId out of range " << totId << endm;
  }
  unsigned short tdcId[10] = {72, 73, 98, 99, 124, 125, 176, 177, 178, 179};

  return mTheTofArray.TdcData[tdcId[totId]];
}
   
unsigned short TOF_Reader::GetPvpdAdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdAdc pvpdId out of range " << pvpdId << endm;
    return 0;
  }
  return mTheTofArray.AdcData[43+pvpdId];
}


unsigned short TOF_Reader::GetPvpdAdcHigh(int pvpdId){
  if (year2Data()){
    LOG_INFO << "TOF_Reader: pVPD high gain data not available for year2 data" << endm;
    return 0;
  }
  if ((pvpdId<0) || (pvpdId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdAdcHigh pvpdId out of range " << pvpdId << endm;
    return 0;
  }
  return mTheTofArray.TdcData[54+pvpdId];
}

unsigned short TOF_Reader::GetPvpdTdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdTdc pvpdId out of range " << pvpdId << endm;
    return 0;
  }
  return mTheTofArray.TdcData[43+pvpdId];
}

unsigned short TOF_Reader::GetClockAdc(){return mTheTofArray.AdcData[42];}


short TOF_Reader::GetTc(int chanId){
  if ((chanId<0) || (chanId>mMaxA2dChannels-1)){
    LOG_INFO << "TOF_Reader::GetTc chanId out of range " << chanId << endm;
    return 0;
  }
  return mTheTofArray.A2dData[chanId];
}


unsigned short TOF_Reader::GetSc(int chanId){
  if ((chanId<0) || (chanId>mMaxScaChannels-1)){
    LOG_INFO << "TOF_Reader::GetSc chanId out of range " << chanId << endm;
    return 0;
  }
  return mTheTofArray.ScaData[chanId];
}


unsigned int TOF_Reader::GetEventNumber(){
  return mTheTofArray.EventNumber;
}

void TOF_Reader::printRawData(){
  LOG_INFO << "StDaqLib/TOF/TOF_Reader  Printing Raw Data...";
  LOG_INFO << "\n --- ADCD: ";
  for (int i=0;i<mMaxAdcChannels;i++) LOG_INFO << " " << mTheTofArray.AdcData[i];
  LOG_INFO << "\n --- TDCD: ";
  for (int i=0;i<mMaxTdcChannels;i++) LOG_INFO << " " << mTheTofArray.TdcData[i];
  LOG_INFO << "\n --- A2DD: ";
  for (int i=0;i<mMaxA2dChannels;i++) LOG_INFO << " " << mTheTofArray.A2dData[i];
  LOG_INFO << "\n --- SCAD: ";
  for (int i=0;i<mMaxScaChannels;i++) LOG_INFO << " " << mTheTofArray.ScaData[i];
  LOG_INFO << "\nStDaqLib/TOF/TOF_Reader  Done Printing Raw Data..." << endm;
}


//Jing Liu, FY05--- tofr5-------------------
unsigned int TOF_Reader::GetLdTdc(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.LdTdcData[daqId];
}

unsigned int TOF_Reader::GetTrTdc(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.TrTdcData[daqId];
}

unsigned int TOF_Reader::GetLdmTdc(int daqId,int n){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  int chan=0;
  for(unsigned int i=0;i<mTheTofArray.TofLeadingHits.size();i++){
    chan = mTheTofArray.TofLeadingHits[i].globaltdcchan;
    if(chan == daqId) break;
  }
  return mTheTofArray.TofLeadingHits[chan+n].tdc;
}

unsigned int TOF_Reader::GetTrmTdc(int daqId,int n){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  int chan=0;
  for(unsigned int i=0;i<mTheTofArray.TofTrailingHits.size();i++){
    chan = mTheTofArray.TofTrailingHits[i].globaltdcchan;
    if(chan == daqId) break;
  }
  return mTheTofArray.TofTrailingHits[chan+n].tdc;
}

unsigned short TOF_Reader::GetNLdHits(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.LdNHit[daqId];
}

unsigned short TOF_Reader::GetNTrHits(int daqId){
  if ((daqId<0) || (daqId>mMaxTdcChannels-1)){
    LOG_INFO << "TOF_Reader::GetTdc daqId out of range " << daqId << endm;
    return 0;
  }
  return mTheTofArray.TrNHit[daqId];
}

unsigned int TOF_Reader::GetPvpdLdTdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdTdc pvpdId out of range " << pvpdId << endm;
    return 0;
  }
  return mTheTofArray.LdTdcData[192+pvpdId];
}

unsigned int TOF_Reader::GetPvpdTrTdc(int pvpdId){
  if ((pvpdId<0) || (pvpdId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdTdc pvpdId out of range " << pvpdId << endm;
    return 0;
  }
  return mTheTofArray.TrTdcData[192+pvpdId];
}

unsigned int TOF_Reader::GetPvpdLdmTdc(int daqId,int n){
  if ((daqId<0) || (daqId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdTdc daqId out of range " << daqId << endm;
    return 0;
  }
  int chan=0;
  for(unsigned int i=0;i<mTheTofArray.TofLeadingHits.size();i++){
    chan = mTheTofArray.TofLeadingHits[i].globaltdcchan;
    if(chan-192 == daqId) break;
  }
  return mTheTofArray.TofLeadingHits[chan+n].tdc;
}

unsigned int TOF_Reader::GetPvpdTrmTdc(int daqId,int n){
  if ((daqId<0) || (daqId>5)){
    LOG_INFO << "TOF_Reader::GetPvpdTdc daqId out of range " << daqId << endm;
    return 0;
  }
  int chan=0;
  for(unsigned int i=0;i<mTheTofArray.TofTrailingHits.size();i++){
    chan = mTheTofArray.TofTrailingHits[i].globaltdcchan;
    if(chan-192 == daqId) break;
  }
  return mTheTofArray.TofTrailingHits[chan+n].tdc;
}

unsigned int TOF_Reader::GetNLeadingHits() {
  return (unsigned int)(mTheTofArray.TofLeadingHits.size());
}

unsigned int TOF_Reader::GetLeadingEventNumber(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofLeadingHits.size()) return 0;
  else
    return mTheTofArray.TofLeadingHits[ihit].EventNumber;
}

unsigned short TOF_Reader::GetLeadingFiberId(int ihit) {
  if(ihit<0 || ihit >= (int )mTheTofArray.TofLeadingHits.size()) return 9999;
  else
    return mTheTofArray.TofLeadingHits[ihit].fiberid;
}

unsigned short TOF_Reader::GetLeadingGlobalTdcChan(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofLeadingHits.size()) return 9999;
  else
    return mTheTofArray.TofLeadingHits[ihit].globaltdcchan;
}

unsigned int TOF_Reader::GetLeadingTdc(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofLeadingHits.size()) return 0;
  else
    return mTheTofArray.TofLeadingHits[ihit].tdc;
}

unsigned int TOF_Reader::GetNTrailingHits() {
  return (unsigned int)(mTheTofArray.TofTrailingHits.size());
}

unsigned int TOF_Reader::GetTrailingEventNumber(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofTrailingHits.size()) return 0;
  else
    return mTheTofArray.TofTrailingHits[ihit].EventNumber;
}

unsigned short TOF_Reader::GetTrailingFiberId(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofTrailingHits.size()) return 9999;
  else
    return mTheTofArray.TofTrailingHits[ihit].fiberid;
}

unsigned short TOF_Reader::GetTrailingGlobalTdcChan(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofTrailingHits.size()) return 9999;
  else
    return mTheTofArray.TofTrailingHits[ihit].globaltdcchan;
}

unsigned int TOF_Reader::GetTrailingTdc(int ihit) {
  if(ihit<0 || ihit >= (int) mTheTofArray.TofTrailingHits.size()) return 0;
  else
    return mTheTofArray.TofTrailingHits[ihit].tdc;
}

int TOF_Reader::UnpackYear2to4Data(const Bank_TOFP * TofPTR) {

  unsigned short numberOfDataWords, slot, channel;
  int dataDWord, value;
  //TofPTR->print();
  unsigned short Token = TofPTR->header.Token;
  if (Token==0){
    LOG_INFO << "TOF_Reader: do not know how to handle token==0"<<endm;
    //return;
  }
  mTheTofArray.EventNumber = Token;
//  LOG_INFO << " Token = " << Token << endm;
  mTheTofArray.ByteSwapped = 0x04030201;

  int tofRawDataVersion = TofPTR->header.FormatNumber;
//  LOG_INFO << " Raw Data Versions = " << tofRawDataVersion << endm;

  if ((tofRawDataVersion <1) || (tofRawDataVersion >3)){
    LOG_INFO << "TOF_Reader: ERROR unknown raw data version " << tofRawDataVersion << endm;
    return 1;
  }

  //fg 1. introduce rawdata consistency checks below ...

  //Read ADC Bank

  TOFADCD * TofAdcD;
  if (TofPTR->AdcPTR.length>0) {
    TofAdcD = (TOFADCD *) ((unsigned long *)TofPTR + TofPTR->AdcPTR.offset);
    //TofAdcD->print();
    TofAdcD->swap();
    if (TofAdcD->header.Token!=Token){
      LOG_INFO << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " ADCD " << TofAdcD->header.Token << endm;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofAdcD->header.BankLength - (INT32)sizeof(TofAdcD->header)/4;
    if (numberOfDataWords!=mMaxAdcChannels){
      LOG_INFO << "TOF_Reader: ADCD #channels mismatch " << numberOfDataWords  << endm;
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
      LOG_INFO << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " TDCD " << TofTdcD->header.Token << endm;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofTdcD->header.BankLength - (INT32)sizeof(TofTdcD->header)/4;
    if (numberOfDataWords!=mMaxTdcChannels){
      LOG_INFO << "TOF_Reader: TDCD #channels mismatch " << numberOfDataWords  << endm;
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
      LOG_INFO << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " A2DD " << TofA2dD->header.Token << endm;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofA2dD->header.BankLength - (INT32)sizeof(TofA2dD->header)/4;
    if (numberOfDataWords!=mMaxA2dChannels){
      LOG_INFO << "TOF_Reader: A2DD #channels mismatch " << numberOfDataWords  << endm;
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
      LOG_INFO << "TOF_Reader: Token mismatch TOFP "<< Token 
	   << " SCAD " << TofScaD->header.Token << endm;
      mTheTofArray.EventNumber=0;
    }
    numberOfDataWords=TofScaD->header.BankLength - (INT32)sizeof(TofScaD->header)/4;
    if (numberOfDataWords!=mMaxScaChannels){
      LOG_INFO << "TOF_Reader: SCAD #channels mismatch " << numberOfDataWords  << endm;
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
  return -1;
}

int TOF_Reader::UnpackYear5Data(const Bank_TOFP * TofPTR) {

  //LOG_INFO<<"TOF READER 2005! "<<endm;
  //LOG_INFO<<"run no "<<ercpy->runno()<<endm;
  //TofPTR->print();

  unsigned short Token = TofPTR->header.Token;
  if (Token==0){
    LOG_INFO << "TOF_Reader: do not know how to handle token==0"<<endm;
    //return;
  }
  mTheTofArray.EventNumber = Token;
  //  LOG_INFO << " Token = " << Token << endm;
  mTheTofArray.ByteSwapped = 0x04030201;

  int tofRawDataVersion = TofPTR->header.FormatNumber;
//  LOG_INFO << " Raw Data Versions = " << tofRawDataVersion << endm;

  // run 5 - dongx
  if ((tofRawDataVersion <0) || (tofRawDataVersion >3)){
    LOG_INFO << "TOF_Reader: ERROR unknown raw data version " << tofRawDataVersion << endm;
    //Jing Liu, the value of rawdataversion is 0 now, should change according somewhere.... 
   //return;
  }

  //fg 1. introduce rawdata consistency checks below ...
  // Jing Liu, read tofr5 FY05 raw data here!! 02/16/2005.
  // Initialize raw hits vector first.
  mTheTofArray.TofLeadingHits.clear();
  mTheTofArray.TofTrailingHits.clear();

  for(int ifib=0;ifib<4;ifib++) {   // raw data are read out from 4 fibers.
    if(ifib>2) continue;          // for year5 run, only 3 fibers used.
    //LOG_INFO << " offset = " << TofPTR->DDLRPTR[ifib].offset << endm;
    //LOG_INFO << " length = " << dec << TofPTR->DDLRPTR[ifib].length-10 << endm;
    if (TofPTR->DDLRPTR[ifib].length<=0) {
      LOG_INFO<<"No data words in this fiber! "<<endm;
      continue;
    }
    TOFDDLR *TofDdlr = (TOFDDLR *)((unsigned long *)TofPTR + TofPTR->DDLRPTR[ifib].offset);
    int nword = TofPTR->DDLRPTR[ifib].length-10;
    int halftrayid=0;
    int timeinbin=0;

    int runnumber = ercpy->runno();
    int fiboffset1[6]={1,2,3,4,5,6};
    int fiboffset2[6]={4,5,6,1,2,3};  // we swith fiber 2,3 after run 6055081;
    int fiboffset[6];
    if(runnumber<6055081)  for(int i=0;i<6;i++){fiboffset[i]=fiboffset1[i];}
    if(runnumber>=6055081) for(int i=0;i<6;i++){fiboffset[i]=fiboffset2[i];}
 
    for (int iword=0;iword<nword;iword++) {
       //LOG_INFO << hex << TofDdlr->data[iword] << endm;
       int dataword=TofDdlr->data[iword];
       // now process data word seperately, get TDC information from data words.
       if( (dataword&0xF0000000)>>28 == 0xe) continue;   // separator words, skip it.            
       if( (dataword&0xF0000000)>>28 == 0xc) halftrayid = dataword&0x01;    // get halftray id!
       // now get tdc chan, time from trailing and leading edge.
       // some triger words will be skipped.
       int edgeid =int( (dataword & 0xf0000000)>>28 );
       if((edgeid !=LEADING) && (edgeid!=TRAILING)) continue;   // if not leading edge or trailing data, skip it.
       int tdcid = (dataword & 0x0f000000)>>24;   // tdcid here is 0-15 
       int tdcchan=0;
       //LOG_INFO<<"ifib="<<ifib<<" dataword=0x"<<hex<<dataword<<endm;
       if(edgeid == LEADING) {     // leading edge data
         TofRawHit templdhit;
         templdhit.fiberid=ifib;
         tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
	 timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
         templdhit.tdc=timeinbin;
         // global channel number here !
         if(ifib==0)templdhit.globaltdcchan=tdcchan + (tdcid&0x03)*8+(tdcid>>2)*24+halftrayid*96; // 0-191 for tray
         if(ifib>=1) {   // pvpd leading edge read out from tdc 0-chan 0, tdc 1-chan 0, tdc 1-chan 2
           if(tdcid==0&&tdcchan==0)templdhit.globaltdcchan=191 + fiboffset[0+3*(ifib-1)];
           if(tdcid==1&&tdcchan==0)templdhit.globaltdcchan=191 + fiboffset[1+3*(ifib-1)];
           if(tdcid==1&&tdcchan==2)templdhit.globaltdcchan=191 + fiboffset[2+3*(ifib-1)];
         }
         mTheTofArray.TofLeadingHits.push_back(templdhit);
       } else if (edgeid==TRAILING){     // trailing edge data
         TofRawHit temptrhit;
         temptrhit.fiberid=ifib;
         tdcchan=(dataword&0x0F80000)>>19;  // tdcchan is 0-23 here.
         timeinbin = dataword & 0x7ffff;
         temptrhit.tdc=timeinbin;
         if(ifib==0)temptrhit.globaltdcchan=tdcchan +(tdcid>>2)*24+halftrayid*96;   // 0-191 for tray
         if(ifib>=1) {
            if(tdcid==3&&tdcchan==0)temptrhit.globaltdcchan=191 + fiboffset[0+3*(ifib-1)];
            if(tdcid==3&&tdcchan==8)temptrhit.globaltdcchan=191 + fiboffset[1+3*(ifib-1)];
            if(tdcid==3&&tdcchan==10)temptrhit.globaltdcchan=191+ fiboffset[2+3*(ifib-1)];
         } 
         mTheTofArray.TofTrailingHits.push_back(temptrhit);
       }  else {
         LOG_INFO<<" UNKNOWN TDC data ! "<<endm;
         return 1;
       }
       //LOG_INFO<<"ifib="<<ifib<<" dataword="<<hex<<dataword<<" tdcid="<<tdcid<<" tdcchan="<<tdcchan<<" time="<<dec<<timeinbin<<endm;
    }   // end loop data words
  }     // end loop fibers
  // dump out info. for check
/*
  for(unsigned int i=0;i<mTheTofArray.TofLeadingHits.size();i++){
    LOG_INFO<<"leading: fiber="<<mTheTofArray.TofLeadingHits[i].fiberid<<" channel="<<mTheTofArray.TofLeadingHits[i].globaltdcchan<<" time="<<mTheTofArray.TofLeadingHits[i].tdc<<endm;
  }
  for(unsigned int i=0;i<mTheTofArray.TofTrailingHits.size();i++){
    LOG_INFO<<"trailing: fiber="<<mTheTofArray.TofTrailingHits[i].fiberid<<" channel="<<mTheTofArray.TofTrailingHits[i].globaltdcchan<<" time="<<mTheTofArray.TofTrailingHits[i].tdc<<endm;
  }
*/
  // count multi hit for each channels
  for(unsigned int i=0;i<TOF_MAX_TDC_CHANNELS;i++){mTheTofArray.LdNHit[i]=0;mTheTofArray.LdTdcData[i]=0;}
  for(unsigned int i=0;i<TOF_MAX_TDC_CHANNELS;i++){mTheTofArray.TrNHit[i]=0;mTheTofArray.TrTdcData[i]=0;}

  for(unsigned int i=0;i<mTheTofArray.TofLeadingHits.size();i++){
    int chan = mTheTofArray.TofLeadingHits[i].globaltdcchan;
    if(chan <=0) continue; // possible?
    mTheTofArray.LdNHit[chan]++;
  }
  for(unsigned int i=0;i<mTheTofArray.TofTrailingHits.size();i++){
    int chan = mTheTofArray.TofTrailingHits[i].globaltdcchan;
    if(chan <=0) continue; // possible?
    mTheTofArray.TrNHit[chan]++;
  }
  // Use only the first LE and TE time in the fixed array!!!!
  int oldchan=0;
  for(unsigned int i=0;i<mTheTofArray.TofLeadingHits.size();i++){
    int chan = mTheTofArray.TofLeadingHits[i].globaltdcchan;
     if(chan == oldchan) continue;
     mTheTofArray.LdTdcData[chan]=mTheTofArray.TofLeadingHits[i].tdc;
     oldchan=chan;
  }
  oldchan=0;
  for(unsigned int i=0;i<mTheTofArray.TofTrailingHits.size();i++){
    int chan = mTheTofArray.TofTrailingHits[i].globaltdcchan;
    if(chan == oldchan) continue;
    mTheTofArray.TrTdcData[chan]=mTheTofArray.TofTrailingHits[i].tdc;
    oldchan=chan;
  }
  /*
  for(int i=0;i<TOF_MAX_TDC_CHANNELS;i++){
    if(mTheTofArray.LdTdcData[i]>0)LOG_INFO<<" ld chan ="<<i<<" time="<<mTheTofArray.LdTdcData[i]<<endm;
    if(mTheTofArray.TrTdcData[i]>0)LOG_INFO<<" tr chan ="<<i<<" time="<<mTheTofArray.TrTdcData[i]<<endm;
  }
  */
  return -1;
}

//Jing Liu, end-----------------------
