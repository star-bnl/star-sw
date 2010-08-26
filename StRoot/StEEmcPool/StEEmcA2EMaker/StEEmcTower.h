#ifndef __StEEmcTower_h__
#define __StEEmcTower_h__

#include "StEEmcElement.h"
#include "TVector3.h"
#include <vector>

class StEEmcTower;
typedef std::vector<StEEmcTower*> StEEmcTowerPtrVec_t;

class StEEmcTower : public StEEmcElement {
public:
  /// Constructor
  StEEmcTower();
  /// Destructor
  virtual ~StEEmcTower(){ /* nada */ };

  /// copy constructor
  StEEmcTower( const StEEmcTower &other );

  /// Returns a direction vector pointing from the specified vertex
  /// to the center of the tower
  TVector3 direction( Float_t zvertex=0.0 ) const; 

  /// Sets the index for this tower, pre- or postshower element.
  /// See StEEmcAnalysisMaker.  This single index will define the
  /// position of the element on the endcap, as well as its 
  /// neighbors.
  void index(Int_t i);
  /// Sets the layer, [0-4]=[TPQR]
  void layer(Int_t l) {mLayer=l;}
  /// Sets the equivalent "E_T" response, assuming the particle
  /// originated from the nominal vertex of 0,0,0.
  void et(Float_t e) {mET=e;}

  /// Returns index of this tower, pre- or postshower element
  Int_t index()const {return mIndex;}
  /// Returns layer of this tower element, [0-4]=[TPQR]
  Int_t layer()const {return mLayer;}
  /// Returns sector of this tower, pre- or postshower element
  Int_t sector()const {return mSector;}
  /// Returns subsector of this tower, pre- or postshower element
  Int_t subsector()const {return mSubsector;}
  /// Returns the etabin of this tower, pre- or postshower element
  Int_t etabin()const {return mEtabin;}
  /// Returns the phibin of this tower
  Int_t phibin()const {return mPhibin;}
  /// Returns the "E_T" response of the tower
  Float_t et()const {return mET;}

  /// add a tower to list of neighbors
  void neighbor(StEEmcTower *n) {mNeighbors.push_back(n);}
  /// get the number of neighboring towers
  Int_t numberOfNeighbors()const { return (Int_t)mNeighbors.size();}
  /// returns a specific neighbor [0,numberOfNeighbors)
  StEEmcTower &neighbor(Int_t i) { return *mNeighbors[i]; }
  const StEEmcTower &neighbor(Int_t i)const { return *mNeighbors[i]; }

  /// returns true if the specified tower neighbors this one
  /// or is equal to this tower.
  Bool_t isNeighbor( const StEEmcTower &t ) const;
  
  /// Print a summary of this tower
  void print() const;
  /// Print a one line summary of this tower
  void printLine() const;

  /// A tower is "less than" another tower if its energy is less than it
  Bool_t operator<( const StEEmcTower &other ) const { return (this->energy() < other.energy()); }

  /// Clears the tower
  virtual void Clear(Option_t *opts=""){ mET=0.; StEEmcElement::Clear(opts); } 

protected:

  Int_t mIndex;      /**<- Index of the tower [0,720) */
  Int_t mLayer;      /**<- Layer of the tower element 0=T,1=P,2=Q,3=R */
  Int_t mSector;     /**<- Sector of the tower [0,12) */
  Int_t mSubsector;  /**<- Subsector of the tower [0,5) = A-E */
  Int_t mEtabin;     /**<- Etabin of the tower [0,12) */
  Int_t mPhibin;     /**<- Phibin of the tower [0,60) */
  
  /// Equivalent transverse energy, assuming a vertex of 0,0,0
  Float_t mET;

  /// List of pointers to neighboring towers.  User never sees
  /// the pointer.
  StEEmcTowerPtrVec_t mNeighbors; //! 

  /// Makes class available to root
  ClassDef(StEEmcTower,1);  

};
typedef std::vector<StEEmcTower>  StEEmcTowerVec_t;

#endif
