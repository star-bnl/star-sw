/***********************************************************************
 *
 * $Id: StHbtKink.hh,v 1.2 2001/06/21 19:15:46 laue Exp $
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

#include <fstream.h>
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
  float        DcaParentDaughter();
  float        DcaDaughterPrimaryVertex();
  float        DcaParentPrimaryVertex();
  float        HitDistanceParentDaughter();
  float        HitDistanceParentVertex();
  float        DeltaEnergy(int i=0);
  float        DecayAngle();
  float        DecayAngleCM();
  StHbtTrack   Daughter();
  StHbtTrack   Parent();
  StHbtThreeVector Position();

  
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
inline float        StHbtKink::DcaParentDaughter(){return mDcaParentDaughter;}
inline float        StHbtKink::DcaDaughterPrimaryVertex(){return mDcaDaughterPrimaryVertex;}
inline float        StHbtKink::DcaParentPrimaryVertex(){return mDcaParentPrimaryVertex;}
inline float        StHbtKink::HitDistanceParentDaughter(){return mHitDistanceParentDaughter;}
inline float        StHbtKink::HitDistanceParentVertex(){return mHitDistanceParentVertex;}
inline float        StHbtKink::DeltaEnergy(int i){return mDeltaEnergy[i];}
inline float        StHbtKink::DecayAngle(){return mDecayAngle;}
inline float        StHbtKink::DecayAngleCM(){return mDecayAngleCM;}
inline StHbtTrack   StHbtKink::Daughter(){return mDaughter;}
inline StHbtTrack   StHbtKink::Parent(){return mParent;}
inline StHbtThreeVector StHbtKink::Position(){return mPosition;}




#endif


















