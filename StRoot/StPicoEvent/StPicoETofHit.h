/**
 * \class StPicoETofHit
 * \brief Stores BTOF hit information
 *
 * The StPicoETofHit holds inofmation about hits in ETOF
 */

#ifndef StPicoETofHit_h
#define StPicoETofHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class StPicoETofHit : public TObject {

 public:
  /// Default consturctor
  StPicoETofHit();
  /// Copy constructor
  StPicoETofHit( const StPicoETofHit& hit );
  /// Destructor
  virtual ~StPicoETofHit();
  /// Print hit information
  virtual void Print( const Char_t* option = "" ) const;  ///< Print info

  //
  // Getters
  //

  /// Return geometry ID of the hit
  Int_t    geomId()      const;
  /// Return eTOF sector number (equal to TPC sector numbering)
  Int_t    sector()      const;
  /// Return eTOF zPlane number (1 - closest to interaction point, 3 - closest to pole tip)
  Int_t    zPlane()      const;
  /// Return eTOF counter number (1 - closest to beamline, 3 - farthest from beamline)
  Int_t    counter()     const;
  /// Return local X coordinate (cm) across strips w.r.t. center of eTOF counter volume
  Float_t  localX()      const;
  /// Return local Y coordinate (cm) along strips w.r.t. center of eTOF counter volume
  Float_t  localY()      const;
  /// Return cluster size of eTOF hit (number of "hits" on different strips clustered into one hit)
  Int_t    clusterSize() const;
  /// Return leading edge time (ns) of eTOF hit
  Float_t  time()        const;
  /// Return time over threshold (ns) of eTOF hit
  Float_t  tot()         const;

  /*
  /// Return global X coordinate (cm) of eTOF hit
  Float_t  globalX()     const;
  /// Return global Y coordinate (cm) of eTOF hit
  Float_t  globalY()     const;
  /// Return global Z coordinate (cm) of eTOF hit
  Float_t  globalZ()     const;
  /// Return global position (cm) of eTOF hit
  TVector3 globalPos()   const;
  */

  //
  // Setters
  //

  /// Set geometry ID of the hit
  void setGeomId( const Int_t geomId );
  /// Set geometry ID of the hit using sector, plane and counter
  void setGeomId( const Int_t sector, const Int_t plane, const Int_t counter );
  /// Set local X coordinate (cm) across strips w.r.t. center of eTOF counter volume
  void setLocalX( const Float_t& localX );
  /// Set local Y coordinate (cm) along strips w.r.t. center of eTOF counter volume
  void setLocalY( const Float_t& localY );
  /// Set cluster size of eTOF hit
  void setClusterSize( const Int_t clusterSize );
  /// Set leading edge time (ns) of eTOF hit
  void setTime( const Float_t& time );
  /// Set time over threshold (ns) of eTOF hit
  void setTot( const Float_t& tot );

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

  /// geometry id encodes (sector-13) * 9 + (plane-1) * 3 + (counter-1) + 1:
  UChar_t   mGeomId;
  /// local X coordinate of eTOF hit * 800
  Short_t   mLocalX;
  /// local Y coordinate of eTOF hit * 800
  Short_t   mLocalY;
  /// cluster size of eTOF hit
  UChar_t   mClusterSize;
  /// leading edge time of eTOF hit (modulo bTOF clock range)
  Float_t   mTime;
  /// time over threshold of eTOF hit * 250
  UShort_t  mTot;

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

//
// Getters
//
inline Int_t    StPicoETofHit::geomId()      const { return     mGeomId;                   }
inline Int_t    StPicoETofHit::sector()      const { return    (mGeomId-1) / 9       + 13; }
inline Int_t    StPicoETofHit::zPlane()      const { return  (((mGeomId-1) % 9) / 3) +  1; }
inline Int_t    StPicoETofHit::counter()     const { return   ((mGeomId-1) % 3)      +  1; }

inline Float_t  StPicoETofHit::localX()      const { return (Float_t)mLocalX / 800.; }
inline Float_t  StPicoETofHit::localY()      const { return (Float_t)mLocalY / 800.; }
inline Int_t    StPicoETofHit::clusterSize() const { return mClusterSize;            }
inline Float_t  StPicoETofHit::time()        const { return mTime;                   }
inline Float_t  StPicoETofHit::tot()         const { return (Float_t)mTot  / 250.;   }

/*
inline Float_t  StPicoETofHit::globalX()     const { return (Float_t)mGlobalX / 100.; }
inline Float_t  StPicoETofHit::globalY()     const { return (Float_t)mGlobalY / 100.; }
inline Float_t  StPicoETofHit::globalZ()     const { return (Float_t)mGlobalZ / 100.; }
inline TVector3 StPicoETofHit::globalPos()   const
{ return TVector3( globalX() , globalY() , globalZ() ); }
*/

#endif