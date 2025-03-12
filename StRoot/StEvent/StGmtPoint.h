/**
 * \class StGmtPoint
 * \brief Holds data for the point (a.k.a. cluster) in GMT
 * 
 * Description: data for individual ``point'' on the GMT, i.e. a pair
 * of 1D clusters.  Note, if errors during construction, the key will
 * be set to -999. Based on StFgtHit.
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

#ifndef StGmtPoint_hh
#define StGmtPoint_hh

// StEvent headers
#include "StHit.h"
#include "StGmtHit.h"

//________________
class StGmtPoint : public StHit {
  public:
    /// Default consturctor
    StGmtPoint();
    /// Parametrized constructor:
    /// \param hitX - hit in X axis
    /// \param hitY - hit in Y axis
    /// \param key - unique label
    StGmtPoint(StGmtHit* hitX, StGmtHit* hitY, int key);
    /// Copy constructor
    StGmtPoint(const StGmtPoint&);
    // StGmtPoint& operator=(const StGmtPoint&);  --> use default

    /// Destructor
    ~StGmtPoint();

    /// Unique detector ID
    virtual StDetectorId detector() const { return kGmtId; }
    /// Unique label
    Int_t getKey()                        { return mKey; }
    /// Module
    Int_t getModule() 
    { return static_cast< Int_t >(mHardwarePosition/8); /* FIX ME */} 
    /// Return hit in X axis
    const StGmtHit* getHitLocalX() const  { return mHitLocalX; }
    /// Local Y coordinate
    const StGmtHit* getHitLocalY() const  { return mHitLocalY; }

    /// Hit coordinate in X axis
    Float_t getPositionLocalX() const     { return (mHitLocalX) ? mHitLocalX->getLocalX() : -999.f; }
    /// Hit coordinate in Y axis
    Float_t getPositionLocalY() const     { return (mHitLocalY) ? mHitLocalY->getLocalY() : -999.f; }
    /// Volume ID
    Int_t volumeID() const                { return 0; }
    
  protected:
    /// Unique label
    Int_t mKey;
    /// Hit in X axis
    StGmtHit *mHitLocalX;
    /// Hit in Y axis
    StGmtHit *mHitLocalY;

 private:   
    ClassDef(StGmtPoint,1)
}; 

#endif // #define StGmtPoint_hh
