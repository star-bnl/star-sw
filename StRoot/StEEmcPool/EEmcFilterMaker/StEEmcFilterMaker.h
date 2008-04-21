// $Id: StEEmcFilterMaker.h,v 1.1 2008/04/21 15:47:09 balewski Exp $

#ifndef STAR_StEEmcFilterMaker
#define STAR_StEEmcFilterMaker

/*!
 *                                                                     
 * \class  StEEmcFilterMaker
 * \author Jan Balewski
 * \date   April 2008
 * \brief  aborts events based on Endcap response
 *  cuts: reco vertex in some Z-range and EEMC 3x3 cluster event-eta ET>thres 
 *  instruction how to use in in BFC is a the end of this .h file
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TH1F;
class EEmcGeomSimple;
class StEEmcTower;

class StEEmcFilterMaker : public StMaker {
 private:
  int nInpEve, nRecVert,nZverOK, nAccEve;
  TH1F * mH0;
  EEmcGeomSimple*  mGeomE;
  Float_t transverseNRG(Float_t vertexPosZ, StEEmcTower * tower);
  Int_t triggerCondition( Float_t vertexPosZ, StEEmcTower *highTow,  Float_t &patchEt);
  float par_Et_thres, par_Z0_vert, par_delZ_vert;
  
 public: 
  void setEtThres(float x) { par_Et_thres=x;}  
  void setZvertCut(float z0, float dz) {  par_Z0_vert=z0; par_delZ_vert=dz;}
  StEEmcFilterMaker(const char *name="EEfilter");
  virtual       ~StEEmcFilterMaker();
  virtual Int_t Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber);

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcFilterMaker.h,v 1.1 2008/04/21 15:47:09 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StEEmcFilterMaker,0)  
};

#endif


/* ===================================

#if 1 
ADD those lines to BFC to make use of this maker

  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("EEmcFilterMaker");

  StEEmcA2EMaker *EEa2eMK=new StEEmcA2EMaker("EE_A2E");
  EEa2eMK->database("eeDb");   // sets db connection
  EEa2eMK->source("StEventMaker",2);
  EEa2eMK->scale(1.0);      // scale reco Endcap energy by a factor

  StEEmcFilterMaker *eeFltMk=new StEEmcFilterMaker; // aborts events
  eeFltMk->setEtThres(15.);// (GeV), event-eta
  eeFltMk->setZvertCut(-60.,15.); // (cm), Z0, delatZ
  chain->AddBefore("MuDst",EEa2eMK);// WARN, order is important
  chain->AddBefore("MuDst",eeFltMk);

  chain->ls(3);

  ========================== */
// $Log: StEEmcFilterMaker.h,v $
// Revision 1.1  2008/04/21 15:47:09  balewski
// star
//
