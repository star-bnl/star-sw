// $Id: StEEmcFilterMaker.h,v 1.5 2014/08/06 11:42:57 jeromel Exp $

#ifndef STAR_StEEmcFilterMaker
#define STAR_StEEmcFilterMaker

/*!
 *                                                                     
 * \class  StEEmcFilterMaker
 * \author Jan Balewski
 * \date   April 2008
 * \brief  aborts events based on Endcap response
   cuts: reco vertex in some Z-range and EEMC 2x1 cluster event-eta ET>thres
  there are 2 modes of operation:
  1) setFixedVertex( float zVert) forces use of external Z-vertex to calculate event-eta ET and alows to run this code before TPC tracking
  2) setZvertCut(float z0, float dz) uses reco vertex and decision is more accurate.
  WARN: us must set one of the 2 methods or code will abort.  
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
  enum {kUnknown=1, kUseFixedVertex, kUseRecoVertex};
  int myMode;
  int nInpEve, nRecVert,nZverOK, nAccEve;
  TH1F * mH0;
  EEmcGeomSimple*  mGeomE;
  Float_t transverseNRG(Float_t vertexPosZ, StEEmcTower * tower);
  Int_t triggerCondition( Float_t vertexPosZ, StEEmcTower *highTow,  Float_t &patchEt);
  float par_Et_thres, par_Z0_vert, par_delZ_vert;
  
 public: 
  void setEtThres(float x) { par_Et_thres=x;}  
  void setZvertCut(float z0, float dz) {  par_Z0_vert=z0; par_delZ_vert=dz;myMode=kUseRecoVertex;}
  void setFixedVertex(float z0) {  par_Z0_vert=z0; myMode=kUseFixedVertex;}

  StEEmcFilterMaker(const char *name="EEmcFilter");
  virtual       ~StEEmcFilterMaker();
  virtual Int_t Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  virtual Int_t FinishRun(int runumber);

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcFilterMaker.h,v 1.5 2014/08/06 11:42:57 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StEEmcFilterMaker,0)  
};

#endif


/* ===================================

#if 1 
  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcPoolEEmcFilterMaker");

  StEEmcA2EMaker *EEa2eMK=new StEEmcA2EMaker("EE_A2E");
  EEa2eMK->database("eeDb");   // sets db connection
  EEa2eMK->source("StEventMaker",2);
  EEa2eMK->scale(1.0);      // scale reco Endcap energy by a factor

  //  this filter-1 _assumes_ fixed Zvertex,
  //    needs only Endcap slow simu 
  StEEmcFilterMaker *eeFlt1Mk=new StEEmcFilterMaker("EEmcFilterPreTPC"); 
  eeFlt1Mk->setEtThres(14.);// (GeV), event-eta used 
  eeFlt1Mk->setFixedVertex(-60.); // (cm), Z0

  //  this filter-2 requires reco vertex
  StEEmcFilterMaker *eeFlt2Mk=new StEEmcFilterMaker("EEmcFilterPostTPC"); 
  eeFlt2Mk->setEtThres(16.);// (GeV), event-eta used 
  eeFlt2Mk->setZvertCut(-60.,15.); // (cm), Z0, delatZ

  // position makers at the right location in the chain
  // WARN, order of operations below is important
  StMaker *tpcMk = chain->GetMaker("tpcChain");
  chain->AddAfter("eefs",tpcMk); // now TPC-chain is after EMC-makers
 
  chain->AddAfter("eefs",eeFlt1Mk);
  chain->AddAfter("eefs",EEa2eMK); // filter 1 is before TPC-chain
  chain->AddBefore("MuDst",eeFlt2Mk); // filter 2 is after vertex finder
  chain->ls(3);
 #endif

 
  ========================== */
// $Log: StEEmcFilterMaker.h,v $
// Revision 1.5  2014/08/06 11:42:57  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2008/05/12 15:33:01  balewski
// *** empty log message ***
//
// Revision 1.3  2008/05/11 18:49:18  balewski
// merged 2 makers, now one can us it before and after TPC tracking, run 2 independent copies
//
// Revision 1.2  2008/05/09 22:14:35  balewski
// new there are 2 filters
//
// Revision 1.1  2008/04/21 15:47:09  balewski
// star
//
