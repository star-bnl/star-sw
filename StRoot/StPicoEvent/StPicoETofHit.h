/**
 * \class StPicoETofHit
 * \brief Stores eTOF hit information
 *
 * The StPicoETofHit holds inofmation about hits in eTOF
 */

#ifndef StPicoETofHit_h
#define StPicoETofHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"
//#include "TVector3.h"

//_________________
class StPicoETofHit : public TObject {

 public:
 
  /// Default consturctor
  StPicoETofHit();
  /// Copy constructor
  StPicoETofHit(const StPicoETofHit& hit );
  /// Destructor
  virtual ~StPicoETofHit();
  /// Print hit information
  virtual void Print( const Char_t* option = "" ) const;

  //
  // Getters
  //

  /// Return geometry ID of the hit
  Int_t    geomId()      const       { return mGeomId; }
  /// Return eTOF sector number (equal to TPC sector numbering)
  Int_t    sector()      const       { return (mGeomId-1) / 9 + 13; }
  /// Return eTOF zPlane number:
  /// \param 1 closest to interaction point
  /// \param 3 closest to pole tip
  Int_t    zPlane()      const       { return ( ( (mGeomId-1) % 9) / 3) + 1; }
  /// Return eTOF counter number:
  /// \param 1 closest to beamline
  /// \param 3 farthest from beamline
  Int_t    counter()     const       { return ( (mGeomId-1) % 3) + 1; }
  /// Return local X coordinate (cm) across strips w.r.t. center of eTOF counter volume
  Float_t  localX()      const       { return (Float_t)mLocalX / 800.; }
  /// Return local Y coordinate (cm) along strips w.r.t. center of eTOF counter volume
  Float_t  localY()      const       { return (Float_t)mLocalY / 800.; }
  /// Return cluster size of eTOF hit (number of "hits" on different strips clustered into one hit)
  Int_t    clusterSize() const       { return mClusterSize; }
  /// Return leading edge time (ns) of eTOF hit
  Float_t  time()        const       { return mLeadingEdgeTime; }
  /// Return leading edge time (ns) of eTOF hit
  Float_t  leadingEdgeTime() const   { return time(); }
  /// Return time over threshold (ns) of eTOF hit
  Float_t  timeOverThreshold() const { return tot(); }
  /// Return time over threshold (ns) of eTOF hit
  Float_t  tot() const               { return (Float_t)mTimeOverThreshold / 250.; }

  /*
  /// Return global X coordinate (cm) of eTOF hit
  Float_t  globalX()     const       { return (Float_t)mGlobalX / 100.; }
  /// Return global Y coordinate (cm) of eTOF hit
  Float_t  globalY()     const       { return (Float_t)mGlobalY / 100.; }
  /// Return global Z coordinate (cm) of eTOF hit
  Float_t  globalZ()     const       { return (Float_t)mGlobalZ / 100.; }
  /// Return global position (cm) of eTOF hit
  TVector3 globalPos()   const       { return TVector3( globalX() , globalY() , globalZ() ); }
  */

  //
  // Setters
  //

  /// Set geometry ID of the hit
  void setGeomId(Int_t geomId);
  /// Set geometry ID of the hit using sector, plane and counter
  void setGeomId(Int_t sector, Int_t plane, Int_t counter);
  /// Set local X coordinate (cm) across strips w.r.t. center of eTOF counter volume
  void setLocalX(Float_t localX);
  /// Set local Y coordinate (cm) along strips w.r.t. center of eTOF counter volume
  void setLocalY(Float_t localY);
  /// Set cluster size of eTOF hit
  void setClusterSize(Int_t clusterSize);
  /// Set leading edge time (ns) of eTOF hit
  void setTime(Float_t time);
  /// Set leading edge time (ns) of eTOF hit
  void setLeadingEdgeTime(Float_t time) { setTime(time); }
  /// Set time over threshold (ns) of eTOF hit
  void setTimeOverThreshold(Float_t tot);
  /// Set time over threshold (ns) of eTOF hit
  void setTot(Float_t tot);

  /*
  /// Set global X coordinate (cm) of eTOF hit
  void setGlobalX( const Float_t& x );
  /// Set global Y coordinate (cm) of eTOF hit
  void setGlobalY( const Float_t& y );
  /// Set global Z coordinate (cm) of eTOF hit
  void setGlobalZ( const Float_t& z );
  /// Set global position of eTOF hit (x,y,z)
  void setGlobalPos( const Float_t& x, const Float_t& y, const Float_t& z );
  */

 private:

  /// Geometry id encodes (sector-13) * 9 + (plane-1) * 3 + (counter-1) + 1:
  UChar_t   mGeomId;
  /// Local X coordinate of eTOF hit * 800
  Short_t   mLocalX;
  /// Local Y coordinate of eTOF hit * 800
  Short_t   mLocalY;
  /// Cluster size of eTOF hit
  UChar_t   mClusterSize;
  /// Leading edge time of eTOF hit (modulo bTOF clock range)
  Float_t   mLeadingEdgeTime;
  /// Time over threshold of eTOF hit * 250
  UShort_t  mTimeOverThreshold;

  /*
  /// global X position of eTOF hit * 100
  Short_t   mGlobalX;
  /// global Y position of eTOF hit * 100
  Short_t   mGlobalY;
  /// global Z position of eTOF hit * 100
  Short_t   mGlobalZ;
  */

  ClassDef( StPicoETofHit, 1 )
};

#endif
