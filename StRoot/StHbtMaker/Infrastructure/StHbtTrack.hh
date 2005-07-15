/***************************************************************************
 *
 * $Id: StHbtTrack.hh,v 1.24 2005/07/15 17:40:57 kopytin Exp $
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
 * Revision 1.24  2005/07/15 17:40:57  kopytin
 * initialized mHiddenInfo to null in StHbtTrack()
 *
 * Revision 1.23  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.22  2003/03/18 14:41:48  kisiel
 * Bugfix update for the theoretical part of the code. Reverts the changes to the Lednicky weight calculator, as the previuos one had problems with strong interaction
 *
 * Revision 1.21  2002/03/21 18:49:31  laue
 * updated for new MuDst reader
 *
 * Revision 1.20  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.17  2001/06/21 19:15:48  laue
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
 * Revision 1.16  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.15  2001/04/05 22:25:15  kisiel
 *   Correct the problems detected by Insure++.
 *
 * Revision 1.14  2001/04/03 21:04:36  kisiel
 *
 *
 *   Changes needed to make the Theoretical code
 *   work. The main code is the ThCorrFctn directory.
 *   The most visible change is the addition of the
 *   HiddenInfo to StHbtPair.
 *
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

#include "Stiostream.h"

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StPhysicalHelixD.hh"
/* Th stuff */
#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
/***/

class StEvent;
class StTrack;
class StHbtTTreeTrack;
class StHbtTTreeEvent;
class StMuDst;
class StMuTrack;

class StHbtTrack{
public:
  StHbtTrack();
  StHbtTrack(const StHbtTrack&);// copy constructor
#ifdef __ROOT__
  StHbtTrack(const StTrack*, StHbtThreeVector);   // c-tor from StTrack of STAR DSTs
  StHbtTrack(const StHbtTTreeEvent* ev, const StHbtTTreeTrack* t);
  StHbtTrack(const StMuDst* dst, const StMuTrack* t);
#endif
  StHbtTrack(const StEvent*, const StTrack*);
  ~StHbtTrack();
//    ~StHbtTrack(){/* no-op*/};

  short TrackType() const;
  short Charge() const;
  short NHits() const;
  short NHitsPossible() const;
  short NHitsDedx() const;
  const float* NSigma() const; // Fab private
  float NSigmaElectron() const;
  float NSigmaPion() const;
  float NSigmaKaon() const;
  float NSigmaProton() const;
  float PidProbElectron() const;
  float PidProbPion() const;
  float PidProbKaon() const;
  float PidProbProton() const;
  float dEdx() const;
  float DCAz() const;
  float DCAxy() const;
  float DCAzGlobal() const;
  float DCAxyGlobal() const;
  float ChiSquaredXY() const;
  float ChiSquaredZ() const;
  StHbtThreeVector P() const;
  float Pt() const;
  StHbtThreeVector PGlobal() const;
  float PtGlobal() const;
  const StPhysicalHelixD& Helix() const;
  const StPhysicalHelixD& HelixGlobal() const;
  unsigned int TopologyMap(const unsigned int word) const;
  short TrackId() const;

  void SetTrackType(const short&);
  void SetCharge(const short&);
  void SetNHits(const short&);
  void SetNHitsPossible(const short&);
  void SetNHitsDedx(const short&);
  void SetNSigmaElectron(const float&);
  void SetNSigmaPion(const float&);
  void SetNSigmaKaon(const float&);
  void SetNSigmaProton(const float&);
  void SetPidProbElectron(const float&);
  void SetPidProbPion(const float&);
  void SetPidProbKaon(const float&);
  void SetPidProbProton(const float&);
  void SetdEdx(const float&);
  void SetDCAxy(const float&);
  void SetDCAz(const float&);
  void SetDCAxyGlobal(const float&);
  void SetDCAzGlobal(const float&);
  void SetChiSquaredXY(const float&);
  void SetChiSquaredZ(const float&);
  void SetP(const StHbtThreeVector&);
  void SetPt(const float&);
  void SetPGlobal(const StHbtThreeVector&);
  void SetPtGlobal(const float&);
  void SetHelix(const StPhysicalHelixD&);
  void SetHelixGlobal(const StPhysicalHelixD&);
  void SetTopologyMap(const int word, const unsigned int map);
  void SetTrackId(const short&);

  /* Th stuff */
  void SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo);
  bool ValidHiddenInfo() const;
  // Fab private : (official : const StHbtHiddenInfo* HiddenInfo() const;
  StHbtHiddenInfo* getHiddenInfo() const;
  /***/

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StHbtTrack& trk);
  friend istream& operator>>(istream& in,  StHbtTrack& trk);

private:
  short mTrackType;
  char mCharge;
  unsigned short mNHits;
  unsigned short mNHitsPoss; 
  unsigned short mNHitsDedx;
  float mNSigmaElectron;
  float mNSigmaPion;
  float mNSigmaKaon;
  float mNSigmaProton;
  float mPidProbElectron; // new
  float mPidProbPion; // new
  float mPidProbKaon; // new
  float mPidProbProton; // new
  float mdEdx;
  float mDCAxy;
  float mDCAz; 
  float mDCAxyGlobal;
  float mDCAzGlobal; 
  float mChiSqXY;
  float mChiSqZ;
  unsigned int mMap[2];
  unsigned int mTrackId;


  StHbtThreeVector mP;
  float mPt;
  StHbtThreeVector mPGlobal;
  float mPtGlobal;
  StPhysicalHelixD mHelix;
  StPhysicalHelixD mHelixGlobal;

  /* Th stuff */
  // Fab private : add mutable
  //  mutable 
  StHbtHiddenInfo* mHiddenInfo; //!
  /***/

  friend class StHbtIOBinary;
  friend class StHbtTTreeEvent;
  friend class StHbtTTreeTrack;
};

inline const float* StHbtTrack::NSigma() const 
{return &mNSigmaElectron;} // Fab private 
inline float StHbtTrack::PidProbElectron() const {return mPidProbElectron;}
inline float StHbtTrack::PidProbPion() const {return mPidProbPion;}
inline float StHbtTrack::PidProbKaon() const {return mPidProbKaon;}
inline float StHbtTrack::PidProbProton() const {return mPidProbProton;}
inline StHbtThreeVector StHbtTrack::PGlobal() const {return mPGlobal;}
inline float StHbtTrack::PtGlobal() const {return mPtGlobal;}   

#endif
