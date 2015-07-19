#ifndef StiKalmanTrackFinderParameters_h
#define StiKalmanTrackFinderParameters_h
#include <string.h>
#include "TChair.h"
#include "tables/St_KalmanTrackFinderParameters_Table.h"

class StiKalmanTrackFinderParameters : public TChair {
 public:
  static StiKalmanTrackFinderParameters* 	instance();
  KalmanTrackFinderParameters_st 	*Struct(Int_t i = 0) 	{return ((St_KalmanTrackFinderParameters*) Table())->GetTable()+i;}
  UInt_t        getNumRows()                    {return GetNRows();}
  Int_t         useMcAsRec(Int_t i = 0)         {return Struct(i)->useMcAsRec;}
  Int_t         useTrackFilter(Int_t i = 0)     {return Struct(i)->useTrackFilter;}
  Int_t         elossCalculated(Int_t i = 0)    {return Struct(i)->elossCalculated;}
  Int_t         mcsCalculated(Int_t i = 0)      {return Struct(i)->mcsCalculated;}
  //  Double_t      field(Int_t i = 0)      {return Struct(i)->field;}
  Int_t         maxNullCount(Int_t i = 0)       {return Struct(i)->maxNullCount;}
  Int_t         maxContigNullCount(Int_t i = 0) {return Struct(i)->maxContigNullCount;}
  Int_t         minCountForReset(Int_t i = 0)   {return Struct(i)->minCountForReset;}
  Int_t         HitRegions(Int_t i = 0)         {return Struct(i)->mHitRegions;}
  Int_t         HitWeights(Int_t i = 0)         {return Struct(i)->mHitWeights;}
  Double_t      maxChi2Vertex(Int_t i = 0)      {return Struct(i)->maxChi2Vertex;}
  Double_t      massHypothesis(Int_t i = 0)     {return Struct(i)->massHypothesis;}
  Double_t      maxDca2dZeroXY(Int_t i = 0)     {return Struct(i)->maxDca2dZeroXY;}
  Double_t      maxDca3dVertex(Int_t i = 0)     {return Struct(i)->maxDca3dVertex;}
  Double_t      getMassHypothesis()             {return massHypothesis();}
  Int_t         maxContiguousNullCount()        {return maxContigNullCount();}
  Int_t         minContiguousHitCountForNullReset() {return minCountForReset();}
  void          setHitRegions(Int_t rs) {
    Int_t i=0;
    for (i=0;rs&&i<4;i++) {mHitRegions[i]=rs%100; rs/=100;}
    mHitRegions[i]=10000;
  }
  void          setHitWeights(Int_t ws) {
    Int_t i=0;
    for (i=0;ws&&i<4;i++) {mHitWeights[i]=ws%100; ws/=100;}
    mHitWeights[i]=0;
  }
  Int_t         hitWeight(Int_t rxy) const {
    if (rxy>50) return 0;
    Int_t i=0; for (i=0;rxy>mHitRegions[i]&&i<4;i++) {}
    return mHitWeights[i];
  }
  Int_t         sumWeight()	  const {return 20;}
  
 protected:
  StiKalmanTrackFinderParameters(St_KalmanTrackFinderParameters *table=0) : 
    TChair(table) {
    memset(mBeg,0,mEnd-mBeg+1);
    setHitRegions(HitRegions());
    setHitWeights(HitWeights());
  }
  virtual ~StiKalmanTrackFinderParameters() {fgInstance = 0;}
  Char_t   mBeg[1];
  Int_t    mHitRegions[5];	//20,50 means 0<svtHit<20, 20<ssdHit<50
  Int_t    mHitWeights[5];	//Coeffs of nhits. sum must be >=20
  Char_t   mEnd[1];
 private:
  static StiKalmanTrackFinderParameters* fgInstance;
  ClassDefineChair(StiKalmanTrackFinderParameters,St_KalmanTrackFinderParameters, KalmanTrackFinderParameters_st )
  ClassDef(StiKalmanTrackFinderParameters,1)  //C++ TChair for KalmanTrackFinderParameters table class
};
#endif
