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
  ~StEEmcTower(){ /* nada */ };

  /// copy constructor
  StEEmcTower( const StEEmcTower &other );

  /// Returns a direction vector pointing from the specified vertex
  /// to the center of the tower
  TVector3 direction( Float_t zvertex=0.0 ); 

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
  Int_t index()const;
  /// Returns layer of this tower element, [0-4]=[TPQR]
  Int_t layer();
  Int_t layer()const;
  /// Returns sector of this tower, pre- or postshower element
  Int_t sector();
  Int_t sector()const;
  /// Returns subsector of this tower, pre- or postshower element
  Int_t subsector();
  Int_t subsector()const;
  /// Returns the etabin of this tower, pre- or postshower element
  Int_t etabin();
  Int_t etabin()const;
  /// Returns the phibin of this tower
  Int_t phibin();
  Int_t phibin()const;
  /// Returns the "E_T" response of the tower
  Float_t et();
  Float_t et()const;

  /// add a tower to list of neighbors
  void neighbor(StEEmcTower *n);
  /// get the number of neighboring towers
  Int_t numberOfNeighbors();
  Int_t numberOfNeighbors()const;
  /// returns a specific neighbor [0,numberOfNeighbors)
  StEEmcTower neighbor(Int_t i);
  StEEmcTower neighbor(Int_t i)const;

  /// returns true if the specified tower neighbors this one
  /// or is equal to this tower.
  Bool_t isNeighbor( StEEmcTower t );
  

  /// Print a summary of this tower
  void print();
  /// Print a one line summary of this tower
  void printLine();

  /// A tower is "less than" another tower if its energy is less than it
  Bool_t operator<( const StEEmcTower &other ) const { return (this->energy() < other.energy()); }

  /// Clears the tower
  void Clear(Option_t *opts=""){ mET=0.; StEEmcElement::Clear(opts); } 


 private:
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

/* inline set methods */
inline void StEEmcTower::layer(Int_t l){mLayer=l;}
inline void StEEmcTower::et(Float_t e){mET=e;}
inline void StEEmcTower::neighbor(StEEmcTower *t){mNeighbors.push_back(t);}

/* inline get methods*/
inline Int_t StEEmcTower::index(){return mIndex;}
inline Int_t StEEmcTower::layer(){return mLayer;}
inline Int_t StEEmcTower::sector(){return mSector;}
inline Int_t StEEmcTower::subsector(){return mSubsector;}
inline Int_t StEEmcTower::etabin(){return mEtabin;}
inline Int_t StEEmcTower::phibin(){return mPhibin;}
inline Float_t StEEmcTower::et(){return mET;}
inline Int_t StEEmcTower::numberOfNeighbors(){ return (Int_t)mNeighbors.size();}
inline StEEmcTower StEEmcTower::neighbor(Int_t i){ return *mNeighbors[i]; }

inline Int_t StEEmcTower::index()const{return mIndex;}
inline Int_t StEEmcTower::layer()const{return mLayer;}
inline Int_t StEEmcTower::sector()const{return mSector;}
inline Int_t StEEmcTower::subsector()const{return mSubsector;}
inline Int_t StEEmcTower::etabin()const{return mEtabin;}
inline Int_t StEEmcTower::phibin()const{return mPhibin;}
inline Float_t StEEmcTower::et()const{return mET;}
inline Int_t StEEmcTower::numberOfNeighbors()const{ return (Int_t)mNeighbors.size();}
inline StEEmcTower StEEmcTower::neighbor(Int_t i)const{ return *mNeighbors[i]; }









typedef std::vector<StEEmcTower>  StEEmcTowerVec_t;

#endif
