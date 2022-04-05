#ifndef StiKalmanTrackFitterParameters_h
#define StiKalmanTrackFitterParameters_h

#include "TChair.h"
#include "tables/St_KalmanTrackFitterParameters_Table.h"

class StiKalmanTrackFitterParameters : public TChair {
 public:
  static StiKalmanTrackFitterParameters* 	instance();
  KalmanTrackFitterParameters_st 	*Struct(Int_t i = 0) const {return ((St_KalmanTrackFitterParameters*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()             const  {return GetNRows();}
  Double_t 	maxChi2(Int_t i = 0) 	 const  {return Struct(i)->maxChi2;}
  Double_t 	maxChi2Vtx(Int_t i = 0)  const	{return Struct(i)->maxChi2Vtx;}
  Double_t      getMaxChi2()             const	{return maxChi2();}
  Double_t      getMaxChi2Vtx()          const 	{return maxChi2Vtx();}
  
 protected:
  StiKalmanTrackFitterParameters(St_KalmanTrackFitterParameters *table=0) : TChair(table) {}
  virtual ~StiKalmanTrackFitterParameters() {fgInstance = 0;}
 private:
  static StiKalmanTrackFitterParameters* fgInstance;
  ClassDefineChair(StiKalmanTrackFitterParameters,St_KalmanTrackFitterParameters, KalmanTrackFitterParameters_st )
  ClassDef(StiKalmanTrackFitterParameters,1) //C++ TChair for KalmanTrackFitterParameters table class
};
#endif
