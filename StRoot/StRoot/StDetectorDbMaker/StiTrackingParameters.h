#ifndef StiTrackingParameters_h
#define StiTrackingParameters_h

#include "TChair.h"
#include "tables/St_TrackingParameters_Table.h"

class StiTrackingParameters : public TChair {
 public:
  TrackingParameters_st   *Struct(Int_t i = 0)  const {return ((St_TrackingParameters*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Double_t 	minSearch(Int_t i = 0)   	const {return Struct(i)->minSearch;}
  Double_t 	maxSearch(Int_t i = 0)   	const {return Struct(i)->maxSearch;}
  Double_t 	scaling(Int_t i = 0)     	const {return Struct(i)->scaling;}
  Double_t 	maxChi2(Int_t i = 0)     	const {return Struct(i)->maxChi2;}
  Double_t      getMinSearchWindow()            const {return minSearch();}
  Double_t      getMaxSearchWindow()            const {return maxSearch();}
  Double_t      getSearchWindowScale()          const {return scaling();}
  Double_t      getMaxChi2ForSelection()        const {return maxChi2();}
  
 protected:
  StiTrackingParameters(St_TrackingParameters *table=0) : TChair(table) {}
  virtual ~StiTrackingParameters() {}
 private:
  ClassDefineChair(StiTrackingParameters,St_TrackingParameters, TrackingParameters_st )
  ClassDef(StiTrackingParameters,1) //C++ TChair for TrackingParameters table class
};
#endif
