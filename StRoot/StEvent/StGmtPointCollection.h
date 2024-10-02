/**
 * \class StGmtCollection
 * \brief Holds collections of GMT points
 * 
 * Collection of GMT points for StEvent. Basically a wrapper 
 * for an StSPtrVecGmtPoint (based on StFgtPointCollection)
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtPointCollection_hh
#define StGmtPointCollection_hh

// STAR headers
#include "StObject.h"
#include "StContainers.h"

// Forward declaration
class StGmtPoint;

//________________
class StGmtPointCollection : public StObject {
 public:
  /// Constructor
  StGmtPointCollection();
  // StGmtPointCollection( const StGmtPointCollection& other );            ---> use default
  // StGmtPointCollection& operator=( const StGmtPointCollection& other ); ---> use default 
    
  /// Destructor
  ~StGmtPointCollection();
    
  /// Vector with points
  StSPtrVecGmtPoint& getPointVec()             { return mPointVec; }
  /// Vector with points
  const StSPtrVecGmtPoint& getPointVec() const { return mPointVec; }
  /// Number of points (size of the vector)    
  size_t getNumPoints() const                  { return mPointVec.size(); }
    
  /// Clear
  void Clear( Option_t *opt = "" )             { mPointVec.clear(); }
    
 protected:
  /// Vector of GMT points
  StSPtrVecGmtPoint mPointVec;
    
 private:
  ClassDef(StGmtPointCollection,1)
}; 

#endif // #define StGmtPointCollection_hh
