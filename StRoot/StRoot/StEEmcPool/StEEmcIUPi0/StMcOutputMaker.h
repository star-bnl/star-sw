// $Id: StMcOutputMaker.h,v 1.2 2014/08/06 11:43:00 jeromel Exp $

#ifndef STAR_StMcOutputMaker
#define STAR_StMcOutputMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

class TH1;
class TObjArray  ;
class EEmcGeomSimple;
#include "StLorentzVectorF.hh"
#include "StMcEvent/StMcTrack.hh"

class StMcOutputMaker : public StMaker {
 private:
  enum {mxHa=16};
  TH1 *hA[mxHa];
  EEmcGeomSimple *geomE;

 public:
  StLorentzVectorF probTr;
  TObjArray  *HList; /// output histo access point
  void SetHList(TObjArray * x){HList=x;} 

   StMcOutputMaker(const char *name="mcRead");
 
   virtual       ~StMcOutputMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   float TgenEoftower();
   float TgenEta();
   float TgenPhi();
   float genZgg();
   float genE;
   float genEta;
   float genPhi;
   float zgg;
   void Clear(const Option_t*);
   // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
   // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
   
   /// Displayed on session exit, leave it as-is please ...
   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StMcOutputMaker.h,v 1.2 2014/08/06 11:43:00 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
     return cvs;
   }
   vector <StMcTrack *> gTr;  
   std::vector<Float_t> geemcEta;
   std::vector<Float_t> genZZ;
   std::vector<Float_t> genXX;
   std::vector<Float_t> genYY;
   ClassDef(StMcOutputMaker,0)   //StAF chain virtual base class for Makers
};
 
inline float StMcOutputMaker::TgenEoftower() { return (float) genE; }  
inline float StMcOutputMaker::TgenEta() { return (float) genEta; }  
inline float StMcOutputMaker::TgenPhi() { return (float) genPhi; }  
inline Float_t StMcOutputMaker::genZgg(){ return (float)zgg; } 
#endif


// $Log: StMcOutputMaker.h,v $
// Revision 1.2  2014/08/06 11:43:00  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2008/10/30 15:52:15  balewski
// oryginal version prepared by Weihong, IUCF, results shown on SPIN08
//
