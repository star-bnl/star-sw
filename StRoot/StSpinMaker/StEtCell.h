//////////////////////////////////////////////////////////////////////
//
// $Id: StEtCell.h,v 1.1 2002/02/11 20:30:48 akio Exp $
// $Log: StEtCell.h,v $
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StEtCell 
//
//////////////////////////////////////////////////////////////////////
//
// StEtCell
//
// Event class for a EtCell
//
//////////////////////////////////////////////////////////////////////
#ifndef StEtCell_h
#define StEtCell_h

#include "TObject.h"
#include "math.h"
#include "stdio.h"

class StEtCell : public TObject {
public:
  StEtCell(int k, float e1, float e2, float p1, float p2) {
    key=k;etaMin=e1;etaMax=e2;phiMin=p1;phiMax=p2; nEntry=0; et=0.0;
  }
  StEtCell(int k, float e1, float e2, float p1, float p2, float v) {
    key=k;etaMin=e1;etaMax=e2;phiMin=p1;phiMax=p2; nEntry=1; et=v;
  }
  virtual ~StEtCell(){};
  
  int key;
  float etaMin;
  float etaMax;
  float phiMin;
  float phiMax;
  unsigned int nEntry;
  float et;
  
  float eta() const {return (etaMin+etaMax)/2.0;}
  float phi() const {return (phiMin+phiMax)/2.0;}
  
  void add(float v) {et+=v; nEntry++;}
  void remove(float v) {et-=v; nEntry--;}
  void removeBelowThreshold(float t) {et = et>t ? et : -et;}
  void flipSign() {et = -et;}
  
  int check(int, float, float, float, float) const;
  void print() const; 
  Int_t Compare(const TObject*) const;
  float distanceEta(const StEtCell*) const;
  float distancePhi(const StEtCell*) const;
  float distance(const StEtCell*) const;

  Bool_t IsSortable() const { return kTRUE; }

  ClassDef(StEtCell,1)
};

#endif
