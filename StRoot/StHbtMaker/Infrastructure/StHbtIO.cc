/**********************************************************
 *
 * Here are defined the input/output stream operators for
 *  StHbtEvent and associated classes
 *
 *********************************************************/

#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"


//------------------------- StHbtTrack -----------------------------------
ostream& operator<<(ostream& out, StHbtTrack& trk){
  return (out
	  << trk.mCharge     << " " << trk.mNHits        << " "
	  << trk.mNHitsPoss  << " " << trk.mNSigmaPion   << " "
	  << trk.mNSigmaKaon << " " << trk.mNSigmaProton << " "
	  << trk.mdEdx       << " " << trk.mDCAxy        << " "
	  << trk.mDCAz       << " " << trk.mChiSqXY      << " "
	  << trk.mChiSqZ     << " " << trk.mP.x()        << " "
	  << trk.mP.y()      << " " << trk.mP.z()        << " "
	  // now for the StPhysicalHelixD...
	  << trk.mHelix.curvature() << " " << trk.mHelix.dipAngle() << " "
	  << trk.mHelix.phase()     << " " 
	  << trk.mHelix.origin().x() << " " << trk.mHelix.origin().y() << " " << trk.mHelix.origin().z() << " "
	  << trk.mHelix.h());
}

istream& operator>>(istream& in,  StHbtTrack& trk){
  double x,y,z;
  double curvature, dipAngle, phase;  // for PhysicalHelix
  double xorigin,yorigin,zorigin;     // for PhysicalHelix
  int h;                              // for PhysicalHelix
  in
    >> trk.mCharge     >> trk.mNHits       
    >> trk.mNHitsPoss  >> trk.mNSigmaPion  
    >> trk.mNSigmaKaon >> trk.mNSigmaProton
    >> trk.mdEdx       >> trk.mDCAxy       
    >> trk.mDCAz       >> trk.mChiSqXY     
    >> trk.mChiSqZ     >> x
    >> y               >> z
    // now for the StPhysicalHelixD...
    >> curvature       >> dipAngle
    >> phase           >> xorigin
    >> yorigin         >> zorigin
    >> h;
  // set up P
  trk.mP.setX(x); 
  trk.mP.setY(y); 
  trk.mP.setZ(z); 
  // Pt is derived
  trk.mPt = sqrt(x*x+y*y);
  // now set up the StPhysicalHelixD
  StThreeVectorD origin(xorigin,yorigin,zorigin);
  trk.mHelix.setParameters(curvature,dipAngle,phase,origin,h);
  return in;
}


//------------------------- StHbtEvent -----------------------------------
ostream& operator<<(ostream& out, StHbtEvent& ev){
  out
    << ev.mEventNumber         << " " << ev.mCtbMultiplicity  << " "
    << ev.mZdcAdc[0]           << " " << ev.mZdcAdc[1]         << " "
    << ev.mTpcNhits            << " " << ev.mNumberOfTracks   << " "
    << ev.mNumberOfGoodTracks  << " " << ev.mReactionPlane[0] << " "
    << ev.mReactionPlane[1]    << " " << ev.mPrimVertPos.x()   << " " 
    << ev.mPrimVertPos.y()      << " " << ev.mPrimVertPos.z()   << endl;
  out << ev.mTrackCollection->size() << endl;;
  StHbtTrack trk;
  for (StHbtTrackIterator iter=ev.mTrackCollection->begin();
       iter != ev.mTrackCollection->end(); iter++){
    trk = **iter;              // correct???
    out << trk << endl;   // don't forget the endl to seperate them...
  } 
  // now we should do the v0 collection...
  out << endl; // blank-line delimiter between events
  return out;
}

istream& operator>>(istream& in,  StHbtEvent& ev){ 
  double x,y,z; 
  in 
    >> ev.mEventNumber         >> ev.mCtbMultiplicity 
    >> ev.mZdcAdc[0]           >> ev.mZdcAdc[1]         
    >> ev.mTpcNhits            >> ev.mNumberOfTracks  
    >> ev.mNumberOfGoodTracks  >> ev.mReactionPlane[0]
    >> ev.mReactionPlane[1]    >> x
    >> y                       >> z;
  ev.mPrimVertPos.setX(x);
  ev.mPrimVertPos.setY(y);
  ev.mPrimVertPos.setZ(z);
  // 
  //  OK, time to read in Track and V0 collections
  // 
  long NtracksInCollection;
  in >> NtracksInCollection;
  ev.mTrackCollection = new StHbtTrackCollection;
  for (int itrk=0; itrk<NtracksInCollection; itrk++){
    StHbtTrack* trk = new StHbtTrack;
    in >> *trk;                        // ? ok?
    ev.mTrackCollection->push_back(trk);  // ?ok?
  }
  // now we should do the v0 collection...
  ev.mV0TrackCollection = 0;  // but for now just make it a null pointer
  return in;
}

