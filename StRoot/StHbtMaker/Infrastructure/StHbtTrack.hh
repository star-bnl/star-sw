/***************************************************************************
 *
 * $Id: StHbtTrack.hh,v 1.13 2000/05/03 17:44:43 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    Intermediate format for particle.  This is built from the
 *    input particle format (e.g. StTrack of StEvent) and presented to
 *    the Analyses for ParticleCuts.
 *
 ***************************************************************************
 *
 * $Log: StHbtTrack.hh,v $
 * Revision 1.13  2000/05/03 17:44:43  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.12  2000/04/03 16:21:51  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.11  2000/02/18 21:32:24  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.10  2000/01/25 17:35:17  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.9  2000/01/07 22:16:14  laue
 * missing 'const' in Helix() added
 *
 * Revision 1.8  2000/01/06 17:36:38  laue
 * mNHits changed from 'int' to 'unsigned short'
 * 'int' is inconsistend with the corresponding getters and setters
 *
 * Revision 1.7  1999/09/03 22:39:16  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.6  1999/09/01 19:04:54  lisa
 * update Particle class AND add parity cf and Randys Coulomb correction
 *
 * Revision 1.5  1999/07/27 20:21:10  lisa
 * Franks fixes of StTrack and subsequent changes to particleCut and EventReader
 *
 * Revision 1.4  1999/07/19 14:24:06  hardtke
 * modifications to implement uDST
 *
 * Revision 1.3  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtTrack_hh
#define StHbtTrack_hh

#include <fstream.h>

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StPhysicalHelixD.hh"

class StHbtTrack{
public:
  StHbtTrack(){/* no-op*/};
  StHbtTrack(const StHbtTrack&);// copy constructor
  ~StHbtTrack(){/* no-op*/};

  char Charge() const;
  unsigned short NHits() const;
  unsigned short NHitsPossible() const;
  float NSigmaElectron() const;
  float NSigmaPion() const;
  float NSigmaKaon() const;
  float NSigmaProton() const;
  float dEdx() const;
  float DCAz() const;
  float DCAxy() const;
  float ChiSquaredXY() const;
  float ChiSquaredZ() const;
  StHbtThreeVector P() const;
  float Pt() const;
  const StPhysicalHelixD& Helix() const;
  unsigned long TopologyMap(const int word) const;
  unsigned long TrackId() const;

  void SetCharge(const char&);
  void SetNHits(const unsigned short&);
  void SetNHitsPossible(const unsigned short&);
  void SetNSigmaElectron(const float&);
  void SetNSigmaPion(const float&);
  void SetNSigmaKaon(const float&);
  void SetNSigmaProton(const float&);
  void SetdEdx(const float&);
  void SetDCAxy(const float&);
  void SetDCAz(const float&);
  void SetChiSquaredXY(const float&);
  void SetChiSquaredZ(const float&);
  void SetP(const StHbtThreeVector&);
  void SetPt(const float&);
  void SetHelix(const StPhysicalHelixD&);
  void SetTopologyMap(const int word, const unsigned long map);
  void SetTrackId(const unsigned long&);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StHbtTrack& trk);
  friend istream& operator>>(istream& in,  StHbtTrack& trk);

private:
  char mCharge;
  unsigned short mNHits;
  unsigned short mNHitsPoss; 
  float mNSigmaElectron;
  float mNSigmaPion;
  float mNSigmaKaon;
  float mNSigmaProton;
  float mdEdx;
  float mDCAxy;
  float mDCAz; 
  float mChiSqXY;
  float mChiSqZ;
  unsigned long mMap[2];
  unsigned long mTrackId;


  StHbtThreeVector mP;
  float mPt;
  StPhysicalHelixD mHelix;

  friend class StHbtIOBinary;
};

inline void StHbtTrack::SetNHits(const unsigned short& nh){mNHits=nh;}
inline void StHbtTrack::SetNHitsPossible(const unsigned short& nh){mNHitsPoss=nh;}
inline void StHbtTrack::SetCharge(const char& ch){mCharge=ch;}
inline void StHbtTrack::SetNSigmaElectron(const float& x){mNSigmaElectron = x;}
inline void StHbtTrack::SetNSigmaPion(const float& x){mNSigmaPion = x;}
inline void StHbtTrack::SetNSigmaKaon(const float& x){mNSigmaKaon = x;}
inline void StHbtTrack::SetNSigmaProton(const float& x){mNSigmaProton = x;}
inline void StHbtTrack::SetdEdx(const float& x){mdEdx = x;}

inline void StHbtTrack::SetDCAxy(const float& x){mDCAxy = x;}
inline void StHbtTrack::SetDCAz(const float& x){mDCAz = x;}
inline void StHbtTrack::SetChiSquaredXY(const float& x){mChiSqXY = x;} 
inline void StHbtTrack::SetChiSquaredZ(const float& x){mChiSqZ = x;}   
inline void StHbtTrack::SetP(const StHbtThreeVector& p){mP = p;}
inline void StHbtTrack::SetPt(const float& pt){mPt = pt;}              
inline void StHbtTrack::SetHelix(const StPhysicalHelixD& h){mHelix = h;}
inline void StHbtTrack::SetTopologyMap(const int word, const unsigned long map) { mMap[word]=map;}
inline void StHbtTrack::SetTrackId(const unsigned long& id) { mTrackId=id;}

inline char StHbtTrack::Charge() const {return mCharge;}
inline unsigned short StHbtTrack::NHits() const {return mNHits;}
inline unsigned short StHbtTrack::NHitsPossible() const {return mNHitsPoss;}
inline float StHbtTrack::NSigmaElectron() const {return mNSigmaElectron;}
inline float StHbtTrack::NSigmaPion() const {return mNSigmaPion;}
inline float StHbtTrack::NSigmaKaon() const {return mNSigmaKaon;}
inline float StHbtTrack::NSigmaProton() const {return mNSigmaProton;}
inline float StHbtTrack::dEdx() const {return mdEdx;}

inline float StHbtTrack::DCAxy() const {return mDCAxy;}          
inline float StHbtTrack::DCAz() const {return mDCAz;}            
inline float StHbtTrack::ChiSquaredXY() const {return mChiSqXY;} 
inline float StHbtTrack::ChiSquaredZ() const {return mChiSqZ;}   
inline StHbtThreeVector StHbtTrack::P() const {return mP;}
inline float StHbtTrack::Pt() const {return mPt;}                
inline const StPhysicalHelixD& StHbtTrack::Helix() const {return mHelix;}
inline unsigned long StHbtTrack::TopologyMap(const int word) const { return mMap[word];}
inline unsigned long StHbtTrack::TrackId() const { return mTrackId; }
#endif
