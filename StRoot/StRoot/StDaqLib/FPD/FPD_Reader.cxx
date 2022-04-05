/***************************************************************************
* $Id: FPD_Reader.cxx,v 1.4 2007/12/24 06:04:11 fine Exp $
* Author: Akio Ogawa
***************************************************************************
* Description:  FPD Event Reader
***************************************************************************
* $Log: FPD_Reader.cxx,v $
* Revision 1.4  2007/12/24 06:04:11  fine
* introduce OLDEVP namespace to allow ole and new EVP library concurrently
*
* Revision 1.3  2003/10/02 22:33:35  jeromel
* Cosmetic change
*
* Revision 1.2  2003/05/22 20:48:17  perev
* method added to remove unprintef chars
*
* Revision 1.1  2002/01/17 17:19:58  akio
* First version of FPD daq reader
*
**************************************************************************/
#include "FPD_Reader.hh"

using namespace OLDEVP;

void FPD_Reader::ProcessEvent(const Bank_FPDP * FpdPTR) {
  unsigned short numberOfDataWords;
  int dataDWord;
  //FpdPTR->print();
  unsigned short Token = FpdPTR->header.Token;
  if (Token==0){
    cout << "FPD_Reader: do not know how to handle token==0"<<endl;
    //return;
  }
  mTheFpdArray.EventNumber = Token;
  mTheFpdArray.ByteSwapped = 0x04030201;

  //Read ADC Bank
  FPDDATA * FpdAdcD;
  if (FpdPTR->AdcPTR.length>0) {
    FpdAdcD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->AdcPTR.offset);
    //FpdAdcD->print();

    FpdAdcD->swap();
    cout << "FPD_Reader(FPDADCD): Got a bank named " << FpdAdcD->bankTypeString() << endl;
    if (FpdAdcD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token<<" "
	   << " FPDADCD " << FpdAdcD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=FpdAdcD->header.BankLength - (INT32)sizeof(FpdAdcD->header)/4;
    if (numberOfDataWords!=FPDP_NUM_ADC_CHANNELS/2){
      cout << "FPD_Reader: ADCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_NUM_ADC_CHANNELS/2) numberOfDataWords=FPDP_NUM_ADC_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.AdcData[dataDWord]=FpdAdcD->data[dataDWord];
    }    
  }

  //Read TDC Bank
  FPDDATA * FpdTdcD;
  if (FpdPTR->TdcPTR.length>0) {
    FpdTdcD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->TdcPTR.offset);
    //FpdTdcD->print();
    FpdTdcD->swap();
    cout << "FPD_Reader(FPDTDCD): Got a bank named " << FpdTdcD->bankTypeString() << endl;
    if (FpdTdcD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " FPDTDCD " << FpdTdcD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=FpdTdcD->header.BankLength - (INT32)sizeof(FpdTdcD->header)/4;
    if (numberOfDataWords!=FPDP_NUM_TDC_CHANNELS/2){
      cout << "FPD_Reader: TDCD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_NUM_TDC_CHANNELS/2) numberOfDataWords=FPDP_NUM_TDC_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.TdcData[dataDWord]=FpdTdcD->data[dataDWord];
    }    
  }

  //Read REG Bank...
  FPDDATA * FpdRegD;
  if (FpdPTR->RegPTR.length>0) {
    FpdRegD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->RegPTR.offset);
    //FpdRegD->print();
    FpdRegD->swap();
    cout << "FPD_Reader(FPDREGD): Got a bank named " << FpdRegD->bankTypeString() << endl;
    if (FpdRegD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " FPDREGD " << FpdRegD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=FpdRegD->header.BankLength - (INT32)sizeof(FpdRegD->header)/4;
    if (numberOfDataWords!=FPDP_NUM_REG_CHANNELS/2){
      cout << "FPD_Reader: REGD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_NUM_REG_CHANNELS/2) numberOfDataWords=FPDP_NUM_REG_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.RegData[dataDWord]=FpdRegD->data[dataDWord];
    }    
  }

  //Read PED Bank
  FPDDATA * FpdPedD;
  if (FpdPTR->PedPTR.length>0) {
    FpdPedD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->PedPTR.offset);
    //FpdPedD->print();
    FpdPedD->swap();
    cout << "FPD_Reader(FPDPEDD): Got a bank named " << FpdPedD->bankTypeString() << endl;
    if (FpdPedD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " FPDPEDD " << FpdPedD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=FpdPedD->header.BankLength - (INT32)sizeof(FpdPedD->header)/4;
    if (numberOfDataWords!=FPDP_NUM_PED_CHANNELS/2){
      cout << "FPD_Reader: PEDD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_NUM_PED_CHANNELS/2) numberOfDataWords=FPDP_NUM_PED_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.PedData[dataDWord]=FpdPedD->data[dataDWord];
    }
  }

  //Read SCL Bank
  FPDSCLDATA * FpdSclD;
  if (FpdPTR->SclPTR.length>0) {
    FpdSclD = (FPDSCLDATA *) ((unsigned long *)FpdPTR + FpdPTR->SclPTR.offset);
    //FpdSclD->print();
    FpdSclD->swap();
    cout << "FPD_Reader(FPDSCL): Got a bank named " << FpdSclD->bankTypeString() << endl;
    if (FpdSclD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " FPDSCL " << FpdSclD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=FpdSclD->header.BankLength - (INT32)sizeof(FpdSclD->header)/4;
    if (numberOfDataWords!=FPDP_NUM_SCL_CHANNELS){
      cout << "FPD_Reader: SCLD #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_NUM_SCL_CHANNELS) numberOfDataWords=FPDP_NUM_SCL_CHANNELS;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      mTheFpdArray.SclData[dataDWord]=FpdSclD->data[dataDWord];
    }
  }

  //Read BBC ADC Bank
  FPDDATA * BbcAdcD;
  if (FpdPTR->BbcAdcPTR.length>0) {
    BbcAdcD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->BbcAdcPTR.offset);
    //FpdPedD->print();
    BbcAdcD->swap();
    cout << "FPD_Reader(BBCDAT): Got a bank named " << BbcAdcD->bankTypeString() << endl;
    if (BbcAdcD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " BBCDAT " << BbcAdcD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=BbcAdcD->header.BankLength - (INT32)sizeof(BbcAdcD->header)/4;
    if (numberOfDataWords!=FPDP_BBC_NUM_ADC_CHANNELS/2){
      cout << "FPD_Reader: BBCDAT #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_BBC_NUM_ADC_CHANNELS/2) numberOfDataWords=FPDP_BBC_NUM_ADC_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.BbcAdcData[dataDWord]=BbcAdcD->data[dataDWord];
    }
  }

  //Read BBC Pedestal Bank
  FPDDATA * BbcPedD;
  if (FpdPTR->BbcPedPTR.length>0) {
    BbcPedD = (FPDDATA *) ((unsigned long *)FpdPTR + FpdPTR->BbcPedPTR.offset);
    //FpdPedD->print();
    BbcPedD->swap();
    cout << "FPD_Reader(BBCPED): Got a bank named " << BbcPedD->bankTypeString() << endl;
    if (BbcPedD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " BBCPED " << BbcPedD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=BbcPedD->header.BankLength - (INT32)sizeof(BbcPedD->header)/4;
    if (numberOfDataWords!=FPDP_BBC_NUM_PED_CHANNELS/2){
      cout << "FPD_Reader: BBCPED #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_BBC_NUM_PED_CHANNELS/2) numberOfDataWords=FPDP_BBC_NUM_PED_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords*2; dataDWord++) {
      mTheFpdArray.BbcPedData[dataDWord]=BbcPedD->data[dataDWord];
    }
  }
  //Read BBC Scalar Bank
  FPDSCLDATA * BbcSclD;
  if (FpdPTR->BbcSclPTR.length>0) {
    BbcSclD = (FPDSCLDATA *) ((unsigned long *)FpdPTR + FpdPTR->BbcSclPTR.offset);
    //FpdPedD->print();
    BbcSclD->swap();
    cout << "FPD_Reader(BBCSCL): Got a bank named " << BbcSclD->bankTypeString() << endl;
    if (BbcSclD->header.Token!=Token){
      cout << "FPD_Reader: Token mismatch FPDP "<< Token 
	   << " BBCSCLD " << BbcSclD->header.Token << endl;
      mTheFpdArray.EventNumber=0;
    }
    numberOfDataWords=BbcSclD->header.BankLength - (INT32)sizeof(BbcSclD->header)/4;
    if (numberOfDataWords!=FPDP_BBC_NUM_SCL_CHANNELS){
      cout << "FPD_Reader: BBCSCL #channels mismatch " << numberOfDataWords  << endl;
      if (numberOfDataWords>FPDP_BBC_NUM_SCL_CHANNELS) numberOfDataWords=FPDP_BBC_NUM_SCL_CHANNELS/2;
    }
    // decode and fill data structure
    for (dataDWord=0; dataDWord < numberOfDataWords; dataDWord++) {
      mTheFpdArray.BbcSclData[dataDWord]=BbcSclD->data[dataDWord];
    }
  }

}

FPD_Reader::FPD_Reader(EventReader *er, Bank_FPDP *pFPDP)
{
  pBankFPDP = pFPDP; 
  ercpy = er;
  if (!pBankFPDP->test_CRC()) {
    printf("CRC error in FPDP: %s %d\n",__FILE__,__LINE__) ;
  }
  if (pBankFPDP->swap() < 0) {
    printf("swap error in FPDP: %s %d\n",__FILE__,__LINE__) ;
  }

  pBankFPDP->header.CRC = 0;
  int Token = pBankFPDP->header.Token;
  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP();
  if(Token != dp->header.Token){
    printf("Token mismatch between global %d and FPD %d\n",dp->header.Token,Token);
  }
  ProcessEvent(pBankFPDP);
}


unsigned short FPD_Reader::GetAdc(int id){
  if ((id<0) || (id>FPDP_NUM_ADC_CHANNELS-1)){
    cout << "FPD_Reader::GetAdc id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.AdcData[id];
}


unsigned short FPD_Reader::GetTdc(int id){
  if ((id<0) || (id>FPDP_NUM_TDC_CHANNELS-1)){
    cout << "FPD_Reader::GetTdc id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.TdcData[id];
}

unsigned short FPD_Reader::GetReg(int id){
  if ((id<0) || (id>FPDP_NUM_REG_CHANNELS-1)){
    cout << "FPD_Reader::GetReg id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.RegData[id];
}

unsigned short FPD_Reader::GetPed(int id){
  if ((id<0) || (id>FPDP_NUM_PED_CHANNELS-1)){
    cout << "FPD_Reader::GetPed id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.PedData[id];
}

unsigned int FPD_Reader::GetScl(int id){
  if ((id<0) || (id>FPDP_NUM_SCL_CHANNELS-1)){
    cout << "FPD_Reader::GetScl id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.SclData[id];
}

unsigned short FPD_Reader::GetBbcAdc(int id){
  if ((id<0) || (id>FPDP_BBC_NUM_ADC_CHANNELS-1)){
    cout << "FPD_Reader::GetBbcAdc id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.BbcAdcData[id];
}

unsigned short FPD_Reader::GetBbcPed(int id){
  if ((id<0) || (id>FPDP_BBC_NUM_PED_CHANNELS-1)){
    cout << "FPD_Reader::GetBbcPed id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.BbcPedData[id];
}

unsigned int FPD_Reader::GetBbcScl(int id){
  if ((id<0) || (id>FPDP_BBC_NUM_SCL_CHANNELS-1)){
    cout << "FPD_Reader::GetBbcScl id out of range " << id << endl;
    return 0;
  }
  return mTheFpdArray.BbcSclData[id];
}

unsigned int FPD_Reader::GetEventNumber(){
  return mTheFpdArray.EventNumber;
}

void FPD_Reader::printRawData(){
  cout << "StDaqLib/FPD/FPD_Reader  Printing Raw Data...";
  cout << "\n --- ADCD: ";
  for (int i=0;i<FPDP_NUM_ADC_CHANNELS;i++) cout << " " << mTheFpdArray.AdcData[i];
  cout << "\n --- TDCD: ";
  for (int i=0;i<FPDP_NUM_TDC_CHANNELS;i++) cout << " " << mTheFpdArray.TdcData[i];
  cout << "\n --- REG: ";
  for (int i=0;i<FPDP_NUM_REG_CHANNELS;i++) cout << " " << mTheFpdArray.RegData[i];
  cout << "\n --- PED: ";
  for (int i=0;i<FPDP_NUM_PED_CHANNELS;i++) cout << " " << mTheFpdArray.PedData[i];
  cout << "\n --- SCL: ";
  for (int i=0;i<FPDP_NUM_SCL_CHANNELS;i++) cout << " " << mTheFpdArray.SclData[i];
  cout << "\n --- BBC ADC: ";
  for (int i=0;i<FPDP_BBC_NUM_ADC_CHANNELS;i++) cout << " " << mTheFpdArray.BbcAdcData[i];
  cout << "\n --- BBC Ped: ";
  for (int i=0;i<FPDP_BBC_NUM_PED_CHANNELS;i++) cout << " " << mTheFpdArray.BbcPedData[i];
  cout << "\n --- BBC ADC: ";
  for (int i=0;i<FPDP_BBC_NUM_SCL_CHANNELS;i++) cout << " " << mTheFpdArray.BbcSclData[i];
  cout << "\nStDaqLib/FPD/FPD_Reader  Done Printing Raw Data..." << endl;
}
