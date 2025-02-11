/**
 * \class StGmtHitCollection
 * \brief Holds collection of GMT hits in the module
 * 
 * A collection of StGmtHit classes for StEvent.
 * Basically a wrapper for an StSPtrVecGmtHit. Note, one instance of
 * this class corresponds to one module.
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtHitCollection_hh
#define StGmtHitCollection_hh

// STAR headers
#include "StObject.h"
#include "StContainers.h"

// Forward declaration
class StGmtHit;

//________________
class StGmtHitCollection : public StObject {
 public:
  /// Constructor
  StGmtHitCollection( short moduleId = -1 );
  // StGmtHitCollection( const StGmtHitCollection& other );            ---> use default
  // StGmtHitCollection& operator=( const StGmtHitCollection& other ); ---> use default 
    
  /// Deconstructor
  ~StGmtHitCollection();
    
  /// Vector of hits that belong to the module
  StSPtrVecGmtHit& getHitVec()             { return mHitVec; }
  /// Vector of hits that belong to the module
  const StSPtrVecGmtHit& getHitVec() const { return mHitVec; }
  
  /// Number of hits in the module
  size_t getNumHits() const                { return mHitVec.size(); }
  /// Module number
  short getModule() const                  { return mModule; }
  /// Set module number
  void setModule( short moduleId )         { mModule = moduleId; }
    
  /// Clear
  void Clear( Option_t *opt = "" );
    
 protected:
  /// Module index
  Short_t mModule;
  /// Vector of hits that belong to the current module
  StSPtrVecGmtHit mHitVec;
    
 private:   
  ClassDef(StGmtHitCollection,1)
}; 

#endif // #define StGmtHitCollection_hh

