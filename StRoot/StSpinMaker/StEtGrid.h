//////////////////////////////////////////////////////////////////////
//
// $Id: StEtGrid.h,v 1.2 2002/06/24 13:22:59 akio Exp $
// $Log: StEtGrid.h,v $
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StEtGrid 
//
//////////////////////////////////////////////////////////////////////
//
// StEtGrid
//
// Event class for a EtGrid
//
//////////////////////////////////////////////////////////////////////
#ifndef StEtGrid_h
#define StEtGrid_h

#include "TObject.h"
#include "TClonesArray.h"
#include "math.h"
#include "stdio.h"
#include "StEtCell.h"
#include "StJet.h"

class StTrack;
class StppTrack;
class StMuTrack;

class StEtGrid : public TObject {
public:  
  StEtGrid();
  virtual ~StEtGrid();  
  
  virtual void createKeys(){};
  virtual int findKey(float eta,float phi){return 0;};
  
  StEtCell* cell(int i) {return (StEtCell*)(*mGrid)[i];}

  int add(int key, float et); 
  int add(float eta ,float phi ,float et); 
  int add(StTrack*); 
  int add(StppTrack*); 
  int add(StMuTrack*); 

  void print();
  void clear() {mGrid->Clear(); mState=1;}
  int   nCell() const {return mGrid->GetEntries();}
  float cellEta(int i)    {return cell(i)->eta();}
  float cellEtaMin(int i) {return cell(i)->etaMin;}
  float cellEtaMax(int i) {return cell(i)->etaMax;}
  float cellPhi(int i)    {return cell(i)->phi();}
  float cellPhiMin(int i) {return cell(i)->phiMin;}
  float cellPhiMax(int i) {return cell(i)->phiMax;}
  float cellNEntry(int i) {return cell(i)->nEntry;}
  float cellEt(int i)     {return cell(i)->et;}
  float cellDistance(int i,int j) {return cell(i)->distance(cell(j));}

  void  sort() {mGrid->Sort(); mState=3;}
  void  cutAtThreshold(float);
  StJet* findJet(float radius, float minEtSeed, float minEtCell, int id);

protected:  
  UInt_t mState;  // created empty=0, cleared=1, grid added=2, sort updated=3.
  TClonesArray *mGrid;
  int nKeys;
  float *etaMinKey;
  float *etaMaxKey;
  float *phiMinKey;
  float *phiMaxKey;

private:  
  ClassDef(StEtGrid,1)
};

#endif
