#ifndef __StEEmcTower_h__
#define __StEEmcTower_h__

#include "StEEmcElement.h"

#include <vector>

class StEEmcTower;

typedef std::vector<StEEmcTower*> StEEmcTowerPtrVec_t;

class StEEmcTower : public StEEmcElement {

 public:
  
  StEEmcTower();
  ~StEEmcTower(){ /* nada */ };

  /// copy constructor
  StEEmcTower( const StEEmcTower &other );

  /// Sets the index for this tower, pre- or postshower element.
  /// See StEEmcAnalysisMaker.  This single index will define the
  /// position of the element on the endcap, as well as its 
  /// neighbors.
  void index(Int_t i);
  /// Sets the layer, [0-4]=[TPQR]
  void layer(Int_t l);
  /// Sets the equivalent "E_T" response, assuming the particle
  /// originated from the nominal vertex of 0,0,0.
  void et(Float_t e);

  /// Returns index of this tower, pre- or postshower element
  Int_t index();
  /// Returns layer of this tower element, [0-4]=[TPQR]
  Int_t layer();
  /// Returns sector of this tower, pre- or postshower element
  Int_t sector();
  /// Returns subsector of this tower, pre- or postshower element
  Int_t subsector();
  /// Returns the etabin of this tower, pre- or postshower element
  Int_t etabin();
  /// Returns the phibin of this tower
  Int_t phibin();
  /// Returns the "E_T" response of the tower
  Float_t et();

  /// add a tower to list of neighbors
  void neighbor(StEEmcTower *n);
  /// get the number of neighboring towers
  Int_t numberOfNeighbors();
  /// returns a specific neighbor [0,numberOfNeighbors)
  StEEmcTower neighbor(Int_t i);

  /// returns true if the specified tower neighbors this one
  /// or is equal to this tower.
  Bool_t isNeighbor( StEEmcTower t );
  

  /// Print a summary of this tower
  void print();
  void printLine();

  Bool_t operator<( const StEEmcTower &other ) const { return (this->energy() < other.energy()); }

 private:
 protected:

  Int_t mIndex;
  Int_t mLayer;
  Int_t mSector;
  Int_t mSubsector;
  Int_t mEtabin;
  Int_t mPhibin;
  
  Float_t mET;

  StEEmcTowerPtrVec_t mNeighbors; //! 

  ClassDef(StEEmcTower,1);  

};

inline void  StEEmcTower::layer(Int_t l){mLayer=l;}

inline Int_t StEEmcTower::index(){return mIndex;}
inline Int_t StEEmcTower::layer(){return mLayer;}
inline Int_t StEEmcTower::sector(){return mSector;}
inline Int_t StEEmcTower::subsector(){return mSubsector;}
inline Int_t StEEmcTower::etabin(){return mEtabin;}
inline Int_t StEEmcTower::phibin(){return mPhibin;}

inline void StEEmcTower::neighbor(StEEmcTower *t){mNeighbors.push_back(t);}
inline StEEmcTower StEEmcTower::neighbor(Int_t i){ return *mNeighbors[i]; }
inline Int_t StEEmcTower::numberOfNeighbors(){ return (Int_t)mNeighbors.size();}

inline void StEEmcTower::et(Float_t e){mET=e;}
inline Float_t StEEmcTower::et(){return mET;}

typedef std::vector<StEEmcTower>  StEEmcTowerVec_t;

#endif
