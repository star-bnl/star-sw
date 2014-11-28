/***********************************************************************
 *
 * $Id: StHbtKink.hh,v 1.4 2003/09/02 17:58:32 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, 23May2001
 *
 ***********************************************************************
 *
 * Description: Kink class with information gotten from the StKinkVertex
 *              of Wenshen Deng and Spiros Margetis
 *
 ***********************************************************************
 *
 * $Log: StHbtKink.hh,v $
 * Revision 1.4  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/11/14 21:07:21  lisa
 * Fixed several small things (mostly discarded const) that caused fatal errors with gcc2.95.3
 *
 * Revision 1.2  2001/06/21 19:15:46  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 * Revision 1.1  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * 
 *
 ***********************************************************************/
#ifndef StHbtKink_hh
#define StHbtKink_hh

class StKinkVertex;
class StHbtTTreeEvent;
class StHbtTTreeKink;
//#include "StEvent/StKinkVertex.h"  // from StEvent
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"

#include "Stiostream.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh" //same as in StHbtTrack.hh

class StHbtKink {
public:
  StHbtKink(){/* no-op */}
  StHbtKink( const StHbtKink&); // copy constructor
#ifdef __ROOT__
  StHbtKink( const StKinkVertex&, StHbtThreeVector PrimaryVertex); // create a StHbtKink from a StKinkVertex
  StHbtKink( const StHbtTTreeEvent*, const StHbtTTreeKink*); 
#endif
  ~StHbtKink(){/* no-op */}

  // Get's
  float        DcaParentDaughter() const;
  float        DcaDaughterPrimaryVertex() const;
  float        DcaParentPrimaryVertex() const;
  float        HitDistanceParentDaughter() const;
  float        HitDistanceParentVertex() const;
  float        DeltaEnergy(int i=0) const;
  float        DecayAngle() const;
  float        DecayAngleCM() const;
  StHbtTrack   Daughter() const;
  StHbtTrack   Parent() const;
  StHbtThreeVector Position() const; 

  
  friend ostream& operator<<(ostream& out, StHbtKink& kink);
  friend istream& operator>>(istream& in,  StHbtKink& kink);

  friend class StHbtIOBinary;
  friend class StHbtTTreeKink;

protected:

  float        mDcaParentDaughter;           // from StKinkVertex class directly 
  float        mDcaDaughterPrimaryVertex;    // from StKinkVertex class directly 
  float        mDcaParentPrimaryVertex;      // from StKinkVertex class directly 
  float        mHitDistanceParentDaughter;   // from StKinkVertex class directly 
  float        mHitDistanceParentVertex;     // from StKinkVertex class directly 
  float        mDeltaEnergy[3];              // from StKinkVertex class directly 
  float        mDecayAngle;                  // from StKinkVertex class directly 
  float        mDecayAngleCM;                // from StKinkVertex class directly 
  StHbtTrack   mDaughter;                    // from StKinkVertex class directly 
  StHbtTrack   mParent;                      // from StVertex class (which StKinkVertex inherits from)
  StHbtThreeVector mPosition;                // from StMeasuredPoint class (which StVertex inherits from)

};

// Get's
inline float        StHbtKink::DcaParentDaughter() const {return mDcaParentDaughter;}
inline float        StHbtKink::DcaDaughterPrimaryVertex() const {return mDcaDaughterPrimaryVertex;}
inline float        StHbtKink::DcaParentPrimaryVertex() const {return mDcaParentPrimaryVertex;}
inline float        StHbtKink::HitDistanceParentDaughter() const {return mHitDistanceParentDaughter;}
inline float        StHbtKink::HitDistanceParentVertex() const {return mHitDistanceParentVertex;}
inline float        StHbtKink::DeltaEnergy(int i) const {return mDeltaEnergy[i];}
inline float        StHbtKink::DecayAngle() const {return mDecayAngle;}
inline float        StHbtKink::DecayAngleCM() const {return mDecayAngleCM;}
inline StHbtTrack   StHbtKink::Daughter() const {return mDaughter;}
inline StHbtTrack   StHbtKink::Parent() const {return mParent;}
inline StHbtThreeVector StHbtKink::Position() const {return mPosition;}




#endif


















