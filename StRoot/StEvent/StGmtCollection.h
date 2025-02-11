/**
 * \class StGmtCollection
 * \brief Holds collections of GMT data
 * 
 * GMT data collection for StEvent (based on StFgtCollection)
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtCollection_hh
#define StGmtCollection_hh

// StRoot headers
#include "StObject.h"
#include "StGmtStripCollection.h"
#include "StGmtHitCollection.h"
#include "StGmtPointCollection.h"
#include "StEnumerations.h"

//________________
class StGmtCollection : public StObject {
 public:
  /// Constructor
  StGmtCollection();
  // StGmtCollection( const StGmtCollection& other );            ---> use default
  // StGmtCollection& operator=( const StGmtCollection& other ); ---> use default 
    
  /// Destructor
  ~StGmtCollection();

  /// Number of modules
  size_t getNumModules() const                             { return kGmtNumModules; }
  /// Number total number of strips
  size_t getNumStrips() const;
  /// Number of strips in the i-th module
  size_t getNumStrips( unsigned short moduleIdx) const
  { return (moduleIdx < kGmtNumModules ? mStripCollection[moduleIdx].getNumStrips() : 0 ); }
  /// Number total number of hits
  size_t getNumHits() const;
  /// Number of hits in the i-th module
  size_t getNumHits( unsigned short moduleIdx ) const
  { return (moduleIdx < kGmtNumModules ? mHitCollection[moduleIdx].getNumHits() : 0 ); }
  /// Number of points
  size_t getNumPoints() const
  { return mPointCollection.getNumPoints(); }
    
  // note: ownership of all pointers is retained by the containers.
  // Do not delete any pointers received from this class.
  
  /// Pointer to the GMT strip collection in the i-th module
  StGmtStripCollection* getStripCollection( unsigned short moduleIdx )
  { return (moduleIdx < kGmtNumModules ? &mStripCollection[moduleIdx] : 0 ); }
  /// Pointer to the GMT strip collection in the i-th module  
  const StGmtStripCollection* getStripCollection( unsigned short moduleIdx ) const
  { return (moduleIdx < kGmtNumModules ? &mStripCollection[moduleIdx] : 0 ); }

  /// Pointer to the GMT hit collection in the i-th module  
  StGmtHitCollection* getHitCollection( unsigned short moduleIdx )
  { return (moduleIdx < kGmtNumModules ? &mHitCollection[moduleIdx] : 0 ); }
  /// Pointer to the GMT hit collection in the i-th module  
  const StGmtHitCollection* getHitCollection( unsigned short moduleIdx ) const
  { return (moduleIdx < kGmtNumModules ? &mHitCollection[moduleIdx] : 0 ); }
  
  /// Pointer to the GMT point collection
  StGmtPointCollection* getPointCollection()             { return &mPointCollection; }
  /// Pointer to the GMT point collection
  const StGmtPointCollection* getPointCollection() const { return &mPointCollection; }
  
  /// Clear method  
  void Clear( Option_t *opts = "" );
    
 protected:

  /// Friend class of the StMuDSTMaker
  friend class StMuDstMaker;

  /// GMT strip collections for all modules
  StGmtStripCollection mStripCollection[kGmtNumModules];
  /// GMT hit collections for all modules
  StGmtHitCollection mHitCollection[kGmtNumModules];
  /// GMT point collection
  StGmtPointCollection mPointCollection;
    
 private:   
  ClassDef(StGmtCollection,1)
}; 

#endif // #define StGmtCollection_hh
