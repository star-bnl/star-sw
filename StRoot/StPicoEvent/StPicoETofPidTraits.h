/**
 * \class StPicoETofPidTraits
 * \brief Hold information about eTOF-matched tracks
 *
 * The class stores information about tracks that matched
 * the endcap Time-of-Flight detector
 */

#ifndef StPicoETofPidTraits_h
#define StPicoETofPidTraits_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class StPicoETofPidTraits : public TObject {

 public:
  /// Default constructor
  StPicoETofPidTraits();
  /// Copy constructor
  StPicoETofPidTraits( const StPicoETofPidTraits& traits );
  /// Destructor
  virtual ~StPicoETofPidTraits();
  /// Print eTOF PID traits information
  virtual void Print( const Char_t* option = "" ) const;

  //
  // Getters
  //

  /// Return index of the associated track
  Int_t    trackIndex()   const  { return mTrackIndex; }
  /// Return index of corrsponding eTOF hit
  Int_t    hitIndex()     const  { return mHitIndex; }
  /// Return match flag:
  /// \param 0 no match
  /// \param 1 one-to-one
  /// \param 2 one-to-multiple
  Int_t    matchFlag()    const  { return (Int_t)mMatchFlag; }
  /// Return time of flight (ns)
  Float_t  tof()          const  { return mTimeOfFlight; }
  /// Return beta
  Float_t  beta()         const  { return (Float_t)mBeta / 20000.; }
  /// Return difference between track intersection and eTOF hit in local X coordinate (cm) across strips
  Float_t  deltaX()       const  { return (Float_t)mDeltaX / 800.; }
  /// Return difference between track intersection and eTOF hit in local Y coordinate (cm) along strips
  Float_t  deltaY()       const  { return (Float_t)mDeltaY / 800.; }
  /// Return global X coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingX()    const  { return (Float_t)mCrossingX / 100.; }
  /// Return global Y coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingY()    const  { return (Float_t)mCrossingY / 100.; }
  /// Return global Z coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingZ()    const  { return (Float_t)mCrossingZ / 100.; }
  /// Return global position (cm) of the track intersection with an eTOF volume
  TVector3 crossingPos()  const  { return TVector3( crossingX() , crossingY() , crossingZ() ); }

  //
  // Setters
  //

  /// Set associated track index
  void setTrackIndex( const Int_t index2PicoTrack )
  { mTrackIndex = (index2PicoTrack > std::numeric_limits<short>::max()) ? -1 : (Short_t)index2PicoTrack; }
  /// Set index of corrsponding eTOF hit
  void setHitIndex( const Int_t index2Hit )
  { mHitIndex = (index2Hit > std::numeric_limits<short>::max()) ? -1 : (Short_t)index2Hit; }
  /// Set match flag
  void setMatchFlag( const Char_t flag )  { mMatchFlag    = flag; }
  /// Set time of flight
  void setTof( const Float_t& tof )       { mTimeOfFlight = tof;  }
  /// Set beta
  void setBeta( const Float_t& beta );
  /// Set difference between track intersection and eTOF hit in local X coordinate (cm) across strips
  void setDeltaX( const Float_t& deltaX );
  /// Set difference between track intersection and eTOF hit in local Y coordinate (cm) along strips
  void setDeltaY( const Float_t& deltaY );
  /// Set global X coordinate (cm) of the track intersection with an eTOF volume
  void setCrossingX( const Float_t& x );
  /// Set global Y coordinate (cm) of the track intersection with an eTOF volume
  void setCrossingY( const Float_t& y );
  /// Set global Z coordinate (cm) of the track intersection with an eTOF volume
  void setCrossingZ( const Float_t& z );
  /// Set global position (cm) of the track intersection with an eTOF volume
  void setCrossingPos( const Float_t& x, const Float_t& y, const Float_t& z )
  { setCrossingX( x ); setCrossingY( y ); setCrossingZ( z ); }


 private:

  /// Index to the associated picoTrack in the event
  Short_t   mTrackIndex;
  /// Index to the corresponding eTOF hit in the event
  Short_t   mHitIndex;
  /// Match flag (0 - no match, 1 - one-to-one, 2 - one-to-multiple)
  Char_t    mMatchFlag;
  /// Time of flight (ns)
  Float_t   mTimeOfFlight;
  /// Beta * 20000
  UShort_t  mBeta;
  /// Delta X between matched track-hit pair * 800
  Short_t   mDeltaX;
  /// Delta Y between matched track-hit pair * 800
  Short_t   mDeltaY;
  /// Global X position of track intersection with eTOF * 100
  Short_t   mCrossingX;
  /// Global Y position of track intersection with eTOF * 100
  Short_t   mCrossingY;
  /// Global Z position of track intersection with eTOF * 100
  Short_t   mCrossingZ;

  ClassDef( StPicoETofPidTraits, 1 );
};

#endif
