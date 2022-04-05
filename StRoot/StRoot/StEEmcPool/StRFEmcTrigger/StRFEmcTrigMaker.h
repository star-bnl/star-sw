// *-- Author : Renee Fatemi
// 
// $Id: StRFEmcTrigMaker.h,v 1.5 2014/08/06 11:43:02 jeromel Exp $


#ifndef STAR_StRFEmcTrigMaker
#define STAR_StRFEmcTrigMaker

/*!
 *                                                                     
 * \class  StRFEmcTrigMaker
 * \author rfatemi
 * \date   9/10/03
 * 
 * Reads in MuDst members and creates TP and Jet Trigger output
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#include "TrigDims.h"
#endif


class TH1F;
class StMuDst;
class StMuEmcCollection;
class StEmcGeom;
class StEvent;
class StMuEvent;
class StBbcTriggerDetector;
class StEmcCollection;

class StRFEmcTrigMaker : public StMaker {

 private:
 StEmcGeom *emcGeom; 
 StMuEvent *muEvent;
 StEvent *stEvent;
 StEmcCollection *stEmcCol;
 StMuEmcCollection *muEmcCol;
 StBbcTriggerDetector *bbcCol;

 int DataMode;//0 for MuDst, 1 for StEvent
 int det;//detector number =1 for BTOW
 int Bmod;//BEMC module from 1-120
 int Bsub;//BEMC submodule 1- 2
 int Beta;//BEMC tower 1-20
 int BTowADC;//BEMC tower ADC
 int NumETow;//#Endcap towers
 int Esec;//EEMC sector 1-12
 int Esub;//EEMC subsector 1-5
 int Eeta;//EEMC eta bin 1-12
 int ETowADC;//EEMC Tower ADC
 int bbcTrig;//1 if true, 0 if false
 int BBCadc[BBCadcNum];
 
 int BHTmaxt;  //Hold HT for whole Barrel
 int BJPmaxt;  //Holds max JP sum for whole Barrel
 int BJPsumt;  //Holds sum of all JP in Barrel
 int EHTmaxt;  //Hold HT for whole EEMC
 int EJPmaxt;  //Holds max JP sum for whole EEMC
 int EJPsumt;  //Holds sum of all JP in EEMC
 
 int jpBsum[BemcJP];//Barrel -Holds jet patch energy sum
 int jpBmax[BemcJP];//Holds jet patch HT max
 int jpB_hit_num[BemcJP];//Holds number of hits per JP
 int tpBsum[BemcTP];// Holds Trigger Patch energy sum
 int tpBmax[BemcTP];// Holds Trigger Patch HT 
 
 int jpEsum[EemcJP];//Endcap -Holds jet patch energy sum
 int jpEmax[EemcJP];//Holds jet patch HT max
 int jpE_hit_num[EemcJP];//Holds number of hits per JP
 int tpEsum[EemcTP];// Holds Trigger Patch energy sum
 int tpEmax[EemcTP];// Holds Trigger Patch HT 
 
 TH1F *ha[8];
 void  initHisto();
 void  fillHisto();

 void Sum(int *,int *);
 void Max(int *,int *);
 void unpackEmcFromMu();
 void unpackEmcFromSt();
 void unpackBBC();

 bool activeBBC;
 
 protected:
 
 public: 
  StRFEmcTrigMaker(const char *name="RFTrig");
  virtual       ~StRFEmcTrigMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void Clear(const char *opt=0);

  void useMuDst() {  DataMode=0;} 
  void useStEvent() {  DataMode=1;} 
  Int_t getBBCtrig();
  Int_t getBEMCtrigHT(int);
  Int_t getBEMCtrigJP(int);
  Int_t getBEMCtrigTOT(int);
  Int_t getEEMCtrigHT(int);
  Int_t getEEMCtrigJP(int);
  Int_t getEEMCtrigTOT(int);
  Int_t getBEMC_HT_ADC();
  Int_t getBEMC_JP_ADC();
  Int_t getBEMC_TOT_ADC();
  Int_t getEEMC_HT_ADC();
  Int_t getEEMC_JP_ADC();
  Int_t getEEMC_TOT_ADC();

  void requireBBC(){ activeBBC=true;}

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StRFEmcTrigMaker.h,v 1.5 2014/08/06 11:43:02 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StRFEmcTrigMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StRFEmcTrigMaker.h,v $
// Revision 1.5  2014/08/06 11:43:02  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2004/08/18 19:52:49  balewski
// works for BBC
//
// Revision 1.3  2004/08/18 14:56:35  balewski
// trying to get BBC working
//
// Revision 1.2  2004/08/17 00:13:53  rfatemi
// Update to include BEMC in StEvent trigger
//
