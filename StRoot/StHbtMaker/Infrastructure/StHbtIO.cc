/**********************************************************
 *
 * Here are defined the input/output stream operators for
 *  StHbtEvent and associated classes
 *
 *********************************************************/

#ifndef StHbtIO_cc
#define StHbtIO_cc

#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"

#include <float.h>    // these tell maximum values of types so we
#include <limits.h>   // don't write the characters "inf" to our microDST


//------------------------- StHbtTrack -----------------------------------
ostream& operator<<(ostream& out, StHbtTrack& trk){
  // sometimes ChiSqXY is infinity, so output largest number instead
  float ChiSqXYtemp = (fabs(trk.mChiSqXY)>FLT_MAX) ? FLT_MAX : trk.mChiSqXY;

  return (
	  out
	  << trk.mTrackId << " " 
	  << trk.mCharge << " " 
	  << trk.mNHits << " " 
	  << trk.mNHitsPoss << " " 
	  << trk.mNSigmaElectron << " " 
	  << trk.mNSigmaPion << " " 
	  << trk.mNSigmaKaon << " " 
	  << trk.mNSigmaProton << " "
	  << trk.mdEdx << " " 
	  << trk.mDCAxy << " "
	  << trk.mDCAz << " " 
	  << ChiSqXYtemp << " "
	  << trk.mChiSqZ << " " 
	  << trk.mP.x() << " "
	  << trk.mP.y() << " " 
	  << trk.mP.z() << " "
	  // now for the StPhysicalHelixD...
	  << trk.mHelix.curvature() << " " 
	  << trk.mHelix.dipAngle() << " "
	  << trk.mHelix.phase() << " " 
	  << trk.mHelix.origin().x() << " " 
	  << trk.mHelix.origin().y() << " " 
	  << trk.mHelix.origin().z() << " "
	  << trk.mHelix.h() << " " 
          << trk.mMap[0] << " " 
	  << trk.mMap[1]
	  );
}

istream& operator>>(istream& in,  StHbtTrack& trk){
  double x,y,z;
  double curvature, dipAngle, phase;  // for PhysicalHelix
  double xorigin,yorigin,zorigin;     // for PhysicalHelix
  int h;                              // for PhysicalHelix
  in
    >> trk.mTrackId 
    >> trk.mCharge     
    >> trk.mNHits       
    >> trk.mNHitsPoss  
    >> trk.mNSigmaPion  
    >> trk.mNSigmaKaon 
    >> trk.mNSigmaProton
    >> trk.mdEdx       
    >> trk.mDCAxy       
    >> trk.mDCAz       
    >> trk.mChiSqXY     
    >> trk.mChiSqZ     
    >> x
    >> y               
    >> z
    // now for the StPhysicalHelixD...
    >> curvature       
    >> dipAngle
    >> phase           
    >> xorigin
    >> yorigin         
    >> zorigin
    >> h
    >> trk.mMap[0]
    >> trk.mMap[1];
  // set up P
  trk.mP.setX(x); 
  trk.mP.setY(y); 
  trk.mP.setZ(z); 
  // Pt is derived
  trk.mPt = ::sqrt(x*x+y*y);
  // now set up the StPhysicalHelixD
  StThreeVectorD origin(xorigin,yorigin,zorigin);
  trk.mHelix.setParameters(curvature,dipAngle,phase,origin,h);
  return in;
}

//------------------------- StHbtV0 -----------------------------------
ostream& operator<<(ostream& out, StHbtV0& v0){

  return (out
	  << v0.mDecayLengthV0        << " " << v0.mDecayVertexV0.x()  << " "
	  << v0.mDecayVertexV0.y()    << " " << v0.mDecayVertexV0.z()  << " "
	  << v0.mDcaV0Daughters       << " " << v0.mDcaV0ToPrimVertex  << " "
	  << v0.mDcaPosToPrimVertex   <<" "<< v0.mDcaNegToPrimVertex << " "
	  << v0.mMomPos.x()           << " " << v0.mMomPos.y()         << " "
          << v0.mMomPos.z()           << " " << v0.mMomNeg.x()         << " "
	  << v0.mMomNeg.y()           << " " << v0.mMomNeg.z()         << " " 
	  << v0.mTrackTopologyMapPos[0]  << " " <<  v0.mTrackTopologyMapPos[1] << " " 
	  << v0.mTrackTopologyMapNeg[0]  << " " <<  v0.mTrackTopologyMapNeg[1] << " " 
	  << v0.mRapLambda            << " " << v0.mRapK0Short         << " "
          << v0.mCTauLambda           << " " << v0.mCTauK0Short        << " "
          << v0.mKeyNeg                << " " << v0.mKeyPos     );
}

//------------------------- StHbtV0 -----------------------------------
  istream& operator>>(istream& in, StHbtV0& v0){
   in 
	  >> v0.mDecayLengthV0        >>  v0.mDecayVertexV0   
	  >> v0.mDcaV0Daughters       >>  v0.mDcaV0ToPrimVertex 
	  >> v0.mDcaPosToPrimVertex   >>  v0.mDcaNegToPrimVertex
	  >> v0.mMomPos               >>  v0.mMomNeg      
	  >> v0.mTrackTopologyMapPos[0] >>  v0.mTrackTopologyMapPos[1]                 
	  >> v0.mTrackTopologyMapNeg[0] >>  v0.mTrackTopologyMapNeg[1]                 
	  >> v0.mRapLambda            >>  v0.mRapK0Short 
          >> v0.mCTauLambda           >>  v0.mCTauK0Short 
          >> v0.mKeyNeg                >>  v0.mKeyPos;
   v0.UpdateV0();
   return (in) ;
}

//------------------------- StHbtEvent -----------------------------------
ostream& operator<<(ostream& out, StHbtEvent& ev){
  out
    << ev.mEventNumber         << " " << ev.mRunNumber << " "
    << ev.mCtbMultiplicity     << " " << ev.mZdcAdc[0]           << " " 
    << ev.mZdcAdc[1]           << " " << ev.mTpcNhits            << " " 
    << ev.mNumberOfTracks      << " " << ev.mNumberOfGoodTracks  << " " 
    << ev.mReactionPlane[0]    << " " << ev.mReactionPlane[1]    << " " 
    << ev.mPrimVertPos.x()     << " " << ev.mPrimVertPos.y()      << " " 
    << ev.mPrimVertPos.z()     << " " << ev.mMagneticField << endl;
  out << ev.mTrackCollection->size() << endl;;
  StHbtTrack trk;
  for (StHbtTrackIterator iter=ev.mTrackCollection->begin();
       iter != ev.mTrackCollection->end(); iter++){
    trk= **iter;              // correct???
    out << trk << endl;   // don't forget the endl to seperate them...
  } 
  // now we do the v0 collection...
  out << ev.mV0Collection->size() << endl;;
  StHbtV0 v0;
  for (StHbtV0Iterator iterv0=ev.mV0Collection->begin();
       iterv0 != ev.mV0Collection->end(); iterv0++){
    v0 = **iterv0;              // correct???
    out << v0 << endl;   // don't forget the endl to seperate them...
  } 

  out << endl; // blank-line delimiter between events
  return out;
}

istream& operator>>(istream& in,  StHbtEvent& ev){ 

  double x,y,z; 
  in >> ev.mEventNumber;
  if (in.eof()) {
    cout << "Hit end of file " << endl;
    return in;
  }
  in >> ev.mRunNumber
     >> ev.mCtbMultiplicity 
     >> ev.mZdcAdc[0]           >> ev.mZdcAdc[1]         
     >> ev.mTpcNhits            >> ev.mNumberOfTracks  
     >> ev.mNumberOfGoodTracks  >> ev.mReactionPlane[0]
     >> ev.mReactionPlane[1]    >> x
     >> y                       >> z
     >> ev.mMagneticField;
  ev.mPrimVertPos.setX(x);
  ev.mPrimVertPos.setY(y);
  ev.mPrimVertPos.setZ(z);
  // 
  //  OK, time to read in Track and V0 collections
  // 
  long NtracksInCollection;
  in >> NtracksInCollection;
  if (!(in.good())){
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return in;
  }
  //  ev.mTrackCollection = new StHbtTrackCollection; <-- NO!
  //  the TrackCollection is instantiated by constructor!!
  //
  // since this should *overwrite* any StHbtTracks in the
  // StHbtTrackCollection, let's erase any that might be there
  //
  StHbtTrackIterator iter;
  for (iter=ev.mTrackCollection->begin();iter!=ev.mTrackCollection->end();iter++){
    delete *iter;
  }
  // ok, now we have gotten rid of the tracks themselves.  Let's lose the pointers to those deleted tracks
  ev.mTrackCollection->clear();  // if this doesn't work then just delete the collection and make a new one.

  for (int itrk=0; itrk<NtracksInCollection; itrk++){
    StHbtTrack* trk = new StHbtTrack;
    if ( !(in >> (*trk))){
      cout << "StHbtEvent input operator finds stream in bad state during track read ! ";
      cout << itrk << " of " << NtracksInCollection << " intended" << endl;
      return in;
    }
    ev.mTrackCollection->push_back(trk);  // ?ok?
    //cout << " " << itrk;
  }
  // now we should do the v0 collection...
 
 long NV0sInCollection;
  in >> NV0sInCollection;
  if (!(in.good())){
    cout << "StHbtEvent input operator finds stream in bad state ! " << endl;
    return in;
  }
  // since this should *overwrite* any StHbtV0s in the
  // StHbtV0Collection, let's erase any that might be there
  //
  StHbtV0Iterator iterv0;
  for (iterv0=ev.mV0Collection->begin();iterv0!=ev.mV0Collection->end();iterv0++){
    delete *iterv0;
  }
  // ok, now we have gotten rid of the v0s themselves.  Let's lose the pointers to those deleted v0ss
  ev.mV0Collection->clear();  // if this doesn't work then just delete the collection and make a new one.

  for (int iv0=0; iv0<NV0sInCollection; iv0++){
    StHbtV0* v0 = new StHbtV0;
    if ( !(in >> (*v0))){
      cout << "StHbtEvent input operator finds stream in bad state during v0 read ! ";
      cout << iv0 << " of " << NV0sInCollection << " intended" << endl;
      return in;
    }
    ev.mV0Collection->push_back(v0);  // ?ok?
    //cout << " " << iv0;
  }

  return in;
}

//------------------------- StHbtKink -------------------------------
ostream& operator<<(ostream& out, StHbtKink& knk){
  return (
	  out
	  << knk.mDcaParentDaughter << " "
	  << knk.mDcaDaughterPrimaryVertex << " "
	  << knk.mDcaParentPrimaryVertex << " "
	  << knk.mHitDistanceParentDaughter << " "
	  << knk.mHitDistanceParentVertex << " "
	  << knk.mDeltaEnergy[0] << " "
	  << knk.mDeltaEnergy[1] << " "
	  << knk.mDeltaEnergy[2] << " "
	  << knk.mDecayAngle << " "
	  << knk.mDecayAngleCM << " "
	  << knk.mDaughter << " "
	  << knk.mParent << " "
	  << knk.mPosition.x() << " "
	  << knk.mPosition.y() << " "
	  << knk.mPosition.z() 
	  );
}
istream& operator>>(istream& in,  StHbtKink& knk){
  double x,y,z;
  in 
    >> knk.mDcaParentDaughter 
    >> knk.mDcaDaughterPrimaryVertex 
    >> knk.mDcaParentPrimaryVertex 
    >> knk.mHitDistanceParentDaughter
    >> knk.mHitDistanceParentVertex 
    >> knk.mDeltaEnergy[0] 
    >> knk.mDeltaEnergy[1] 
    >> knk.mDeltaEnergy[2] 
    >> knk.mDecayAngle 
    >> knk.mDecayAngleCM 
    >> knk.mDaughter 
    >> knk.mParent 
    >> x
    >> y
    >> z;
  knk.mPosition.setX(x);
  knk.mPosition.setY(y);
  knk.mPosition.setZ(z);

  return in;
}


#endif
