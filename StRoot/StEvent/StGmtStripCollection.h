/**
 * \class StGmtCollection
 * \brief Holds collections of GMT strips
 * 
 * Collection of GMT strips for StEvent. Basically a wrapper 
 * for an StSPtrVecGmtStrip (based on StFgtStripCollection)
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtStripCollection_hh
#define StGmtStripCollection_hh

// STAR headers
#include "StObject.h"
#include "StContainers.h"
#include "StGmtStrip.h"

//________________
class StGmtStripCollection : public StObject {
 public:
  /// Constructer
  StGmtStripCollection( short module = 0 );
  // StGmtStripCollection( const StGmtStripCollection& other );            ---> use default
  // StGmtStripCollection& operator=( const StGmtStripCollection& other ); ---> use default 
    
  /// Destructor
  ~StGmtStripCollection();
    
  // WARNING: never use getStripVec().push_back() or equivelants. 
  // Instead use StGmtStripCollection::getStrip to add a new strip.

  /// Access vector with strips
  StSPtrVecGmtStrip& getStripVec()             { return mStripVec; }
  /// Access vector with strips
  const StSPtrVecGmtStrip& getStripVec() const { return mStripVec; }
    
    // sort internal vector by geoId
    void sortByGeoId();

    // sort internal vector by coordinate number
    void sortByCoord();
    // sort internal vector by coordinate number
    void partialSortByCoord();

    // sort internal vector by layer (X first then Y)
    void sortByLayer();

    // remove all hits with negative geoIds
    void removeFlagged();
    
    // size of internal vector
    size_t getNumStrips() const;
    
    // modify/access the moduleId
    short getModule() const;
    void setModule( short module );
    
    // Clear
    void Clear( Option_t *opt = "" );
    
    // Get pointer to a strip -- note: this is the only way to modify a
    // strip.  New strip is created if it does not exist, but only
    // using StGmtStrip() constructor.  Ownership is retained by the
    // collection.

    StGmtStrip* getStrip( int Id );
    StGmtStrip* getSortedStrip( int Id );
    
 protected:  
  /// Function used for sorting strips by geoId
  static bool hitGeoIdLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ); 
  /// Function used for sorting strips by coordinate number
  static bool hitCoordLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ); 
  /// Function used for sorting strips by X then Y
  static bool hitLayerLessThan( const StGmtStrip* h1, const StGmtStrip* h2 ); 
     
  /// Module ID
  Short_t mModule;
  /// Vector with strips
  StSPtrVecGmtStrip mStripVec;    
    
  /// Temporary copy of the pointers, indexed by ElecId 
  /// used for the addStripInfo class
  StPtrVecGmtStrip mStripElecIdVec;
  /// Temporary copy of the pointers, indexed by GeoId 
  /// used for the addStripInfo class
  StPtrVecGmtStrip mStripGeoIdVec; 
    
 private:   
  ClassDef(StGmtStripCollection,1)
}; 


// inline functions
inline StGmtStripCollection::StGmtStripCollection( short module ) : StObject(), mModule( module ) {
    mStripGeoIdVec.resize( kGmtNumGeoIds );
    for (unsigned int i=0; i<mStripGeoIdVec.size(); i++)
        mStripGeoIdVec[i] = static_cast< StGmtStrip* >(0);
};

// sort by geoId
inline void StGmtStripCollection::sortByGeoId(){
    sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitGeoIdLessThan );
    return;
};

// sort by layer (X first then Y)
inline void StGmtStripCollection::sortByLayer(){
    sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitLayerLessThan );
    return;
};

// sort by coordinate number
inline void StGmtStripCollection::partialSortByCoord(){
    partial_sort( mStripVec.begin(), mStripVec.begin()+kGmtNumStrips, mStripVec.begin()+kGmtNumStrips, &StGmtStripCollection::hitCoordLessThan );
    return;
};

// sort by coordinate number
inline void StGmtStripCollection::sortByCoord(){
    sort( mStripVec.begin(), mStripVec.end(), &StGmtStripCollection::hitCoordLessThan );
    return;
};


inline size_t StGmtStripCollection::getNumStrips() const {
    return mStripVec.size();
};

inline void StGmtStripCollection::setModule( short moduleId ) {
    mModule = moduleId;
};

inline short StGmtStripCollection::getModule() const {
    return mModule;
};

#endif // #define StGmtStripCollection_hh
