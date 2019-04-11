/**
 * \class StPicoETofPidTraits
 * \brief Hold information about ETOF-matched tracks
 *
 * The class stores information about tracks that matched
 * the Barrel Time-of-Flight detector
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
  /// Print ETOF PID traits information
  virtual void Print( const Char_t* option = "" ) const;

  //
  // Getters
  //

  /// Return index of the associated track
  Int_t    trackIndex()   const;
  /// Return index of corrsponding eTOF hit
  Int_t    hitIndex()     const;
  /// Return match flag (0 - no match, 1 - one-to-one, 2 - one-to-multiple)
  Int_t    matchFlag()    const;
  /// Return time of flight (ns)
  Float_t  tof()          const;
  /// Return beta
  Float_t  beta()         const;
  /// Return difference between track intersection and eTOF hit in local X coordinate (cm) across strips
  Float_t  deltaX()       const;
  /// Return difference between track intersection and eTOF hit in local Y coordinate (cm) along strips
  Float_t  deltaY()       const;
  /// Return global X coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingX()    const;
  /// Return global Y coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingY()    const;
  /// Return global Z coordinate (cm) of the track intersection with an eTOF volume
  Float_t  crossingZ()    const;
  /// Return global position (cm) of the track intersection with an eTOF volume
  TVector3 crossingPos()  const;

  //
  // Setters
  //

  /// Set associated track index
  void setTrackIndex( const Int_t inx2PicoTrack );
  /// Set index of corrsponding eTOF hit
  void setHitIndex( const Int_t index2Hit );
  /// Set match flag
  void setMatchFlag( const Char_t flag );
  /// Set time of flight
  void setTof( const Float_t& tof );
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
  void setCrossingPos( const Float_t& x, const Float_t& y, const Float_t& z );


 private:

  /// index to the associated picoTrack in the event
  Short_t   mTrackIndex;
  /// index to the corresponding eTOF hit in the event
  Short_t   mHitIndex;
  /// match flag (0 - no match, 1 - one-to-one, 2 - one-to-multiple)
  Char_t    mMatchFlag;
  /// time of flight (ns)
  Float_t   mTimeOfFlight;
  /// beta * 20000
  UShort_t  mBeta;
  /// delta X between matched track-hit pair * 800
  Short_t   mDeltaX;
  /// delta Y between matched track-hit pair * 800
  Short_t   mDeltaY;
  /// global X position of track intersection with eTOF * 100
  Short_t   mCrossingX;
  /// global Y position of track intersection with eTOF * 100
  Short_t   mCrossingY;
  /// global Z position of track intersection with eTOF * 100
  Short_t   mCrossingZ;


  ClassDef( StPicoETofPidTraits, 1 );
};

//
// Getters
//
inline Int_t    StPicoETofPidTraits::trackIndex()   const { return mTrackIndex; }
inline Int_t    StPicoETofPidTraits::hitIndex()     const { return mHitIndex;   }
inline Int_t    StPicoETofPidTraits::matchFlag()    const { return (Int_t)mMatchFlag; }

inline Float_t  StPicoETofPidTraits::tof()          const { return mTimeOfFlight; }
inline Float_t  StPicoETofPidTraits::beta()         const { return (Float_t)mBeta / 20000.; }
inline Float_t  StPicoETofPidTraits::deltaX()       const { return (Float_t)mDeltaX / 800.; }
inline Float_t  StPicoETofPidTraits::deltaY()       const { return (Float_t)mDeltaY / 800.; }

inline Float_t  StPicoETofPidTraits::crossingX()    const { return (Float_t)mCrossingX / 100.; }
inline Float_t  StPicoETofPidTraits::crossingY()    const { return (Float_t)mCrossingY / 100.; }
inline Float_t  StPicoETofPidTraits::crossingZ()    const { return (Float_t)mCrossingZ / 100.; }
inline TVector3 StPicoETofPidTraits::crossingPos()  const
{ return TVector3( crossingX() , crossingY() , crossingZ() ); }

//
// Setters
//
inline void StPicoETofPidTraits::setTrackIndex( const Int_t index2PicoTrack ) 
{ mTrackIndex = (index2PicoTrack > std::numeric_limits<short>::max()) ? -1 : (Short_t) index2PicoTrack; }
inline void StPicoETofPidTraits::setHitIndex( const Int_t index2Hit ) 
{ mHitIndex = (index2Hit > std::numeric_limits<short>::max()) ? -1 : (Short_t) index2Hit; }
inline void StPicoETofPidTraits::setMatchFlag( const Char_t flag )  { mMatchFlag    = flag; }
inline void StPicoETofPidTraits::setTof(       const Float_t& tof ) { mTimeOfFlight = tof;  }

#endif
