/***************************************************************************
* $Id: TOF_Reader.cxx,v 1.2 2001/09/28 18:45:43 llope Exp $
* Author: Frank Geurts
***************************************************************************
* Description:  TOF Event Reader
***************************************************************************
* $Log: TOF_Reader.cxx,v $
* Revision 1.2  2001/09/28 18:45:43  llope
* modified for compatibility w/ updated StTofMaker
*
* Revision 1.1  2001/07/08 21:41:58  geurts
* Revision 1.2  2001/09/28 13:13:13  llope
* First release
*
**************************************************************************/
#include "TOF_Reader.hh"

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
    if (numberOfDataWords!=TOFP_NUM_ADC_CHANNELS){
      cout << "TOF_Reader: ADCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>TOFP_NUM_ADC_CHANNELS) numberOfDataWords=TOFP_NUM_ADC_CHANNELS;
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
    if (numberOfDataWords!=TOFP_NUM_TDC_CHANNELS){
      cout << "TOF_Reader: TDCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>TOFP_NUM_TDC_CHANNELS) numberOfDataWords=TOFP_NUM_TDC_CHANNELS;
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
    if (numberOfDataWords!=TOFP_NUM_A2D_CHANNELS){
      cout << "TOF_Reader: A2DD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>TOFP_NUM_A2D_CHANNELS) numberOfDataWords=TOFP_NUM_A2D_CHANNELS;
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
    if (numberOfDataWords!=TOFP_NUM_SCA_CHANNELS){
      cout << "TOF_Reader: SCAD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>TOFP_NUM_SCA_CHANNELS) numberOfDataWords=TOFP_NUM_SCA_CHANNELS;
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

  ProcessEvent(pBankTOFP);
}


unsigned short TOF_Reader::GetAdcFromSlat(int slatId){
  if ((slatId<0) || (slatId>TOFP_NUM_ADC_CHANNELS-1)){
    cout << "TOF_Reader::GetAdcFromSlat slatId out of range " << slatId << endl;
    return 0;
  }
  return mTheTofArray.AdcData[slatId];
}


unsigned short TOF_Reader::GetTdcFromSlat(int slatId){
  if ((slatId<0) || (slatId>TOFP_NUM_TDC_CHANNELS-1)){
    cout << "TOF_Reader::GetTdcFromSlat slatId out of range " << slatId << endl;
    return 0;
  }
  return mTheTofArray.TdcData[slatId];
}

short TOF_Reader::GetTc(int chanId){
  if ((chanId<0) || (chanId>TOFP_NUM_A2D_CHANNELS-1)){
    cout << "TOF_Reader::GetTc chanId out of range " << chanId << endl;
    return 0;
  }
  return mTheTofArray.A2dData[chanId];
}


unsigned short TOF_Reader::GetSc(int chanId){
  if ((chanId<0) || (chanId>TOFP_NUM_SCA_CHANNELS-1)){
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
  for (int i=0;i<TOFP_NUM_ADC_CHANNELS;i++) cout << " " << mTheTofArray.AdcData[i];
  cout << "\n --- TDCD: ";
  for (int i=0;i<TOFP_NUM_TDC_CHANNELS;i++) cout << " " << mTheTofArray.TdcData[i];
  cout << "\n --- A2DD: ";
  for (int i=0;i<TOFP_NUM_A2D_CHANNELS;i++) cout << " " << mTheTofArray.A2dData[i];
  cout << "\n --- SCAD: ";
  for (int i=0;i<TOFP_NUM_SCA_CHANNELS;i++) cout << " " << mTheTofArray.ScaData[i];
  cout << "\nStDaqLib/TOF/TOF_Reader  Done Printing Raw Data..." << endl;
}

