/***************************************************************************
 *
 * $Id: StHbtParticle.hh,v 1.12 2000/10/05 23:09:05 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Particle objects are part of the PicoEvent, which is what is
 *   stored in the EventMixingBuffers
 *   A Track object gets converted to a Particle object if it
 *   passes the ParticleCut of an Analysis
 *
 ***************************************************************************
 *
 * $Log: StHbtParticle.hh,v $
 * Revision 1.12  2000/10/05 23:09:05  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.11  2000/07/17 20:03:17  lisa
 * Implemented tools for addressing and assessing trackmerging
 *
 * Revision 1.10  2000/07/16 21:38:23  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.9  2000/05/03 17:44:43  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.8  2000/04/03 16:21:51  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.6  2000/02/26 19:04:52  laue
 * Some unnecessary includes removed.
 * StThreeVectorD replace by StHbtThreeVector.
 * StHbtCoulomb modified to compile without Root (ClassDef embraced into
 *   #ifdef __ROOT__  ..... #endif)
 * StHbtParticle now returns references (FourMomentum(),Helix(),
 *   DecayVertexPosiion())
 *
 * Revision 1.5  1999/12/11 15:58:29  lisa
 * Add vertex decay position datum and accessor to StHbtParticle to allow pairwise cuts on seperation of V0s
 *
 * Revision 1.4  1999/09/17 22:38:02  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.3  1999/09/01 19:04:54  lisa
 * update Particle class AND add parity cf and Randys Coulomb correction
 *
 * Revision 1.2  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtParticle_hh
#define StHbtParticle_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StPhysicalHelixD.hh"

class StHbtParticle{
public:
  StHbtParticle();
  StHbtParticle(const StHbtTrack* const hbtTrack, const double& mass);
  StHbtParticle(const StHbtV0* const hbtV0, const double& mass);
  ~StHbtParticle();

  const StHbtLorentzVector& FourMomentum() const;

  StPhysicalHelixD& Helix();

  const StHbtThreeVector DecayVertexPosition() const;

  unsigned long TopologyMap(const int word) const;
  int NumberOfHits() const;

  unsigned long TrackId() const;         // only for particles from tracks 
  unsigned short   NegTrackId() const;   // only for particles from v0 
  unsigned short   PosTrackId() const;   // only for particles from v0 

  StHbtTrack* Track() const;
  StHbtV0* V0() const;


  const StHbtThreeVector& NominalTpcExitPoint() const;     // position track exits TPC assuming start at (0,0,0)
  const StHbtThreeVector& NominalTpcEntrancePoint() const; // position track crosses IFC assuming start at (0,0,0)

  // the following method is for explicit internal calculation to fill datamembers.
  // It is invoked automatically if StHbtParticle constructed from StHbtTrack
  void CalculateNominalTpcExitAndEntrancePoints(); // NOTE - this requires the mHelix, so be sure this is filled


  StHbtThreeVector mNominalPosSample[11];  // I make this public for convenience and speed of StHbtPair()



private:
  StHbtTrack* mTrack;  // copy of the track the particle was formed of, else Null
  StHbtV0* mV0;        // copy of the v0 the particle was formed of, else Null

  StHbtLorentzVector mFourMomentum;
  StPhysicalHelixD mHelix;
  unsigned long  mMap[2]; 
  int mNhits;
  StHbtThreeVector mNominalTpcExitPoint;
  StHbtThreeVector mNominalTpcEntrancePoint;


};

inline StHbtTrack* StHbtParticle::Track() const { return mTrack; }
inline unsigned long  StHbtParticle::TrackId() const { return mTrack->TrackId(); }; 
inline const StHbtLorentzVector& StHbtParticle::FourMomentum() const {return mFourMomentum;}
inline StPhysicalHelixD& StHbtParticle::Helix() {return mHelix;}
inline unsigned long StHbtParticle::TopologyMap(const int word) const {return mMap[word];}
inline int StHbtParticle::NumberOfHits() const {return mNhits;}

inline StHbtV0* StHbtParticle::V0() const { return mV0; }
inline unsigned short StHbtParticle::NegTrackId() const { return mV0->idNeg(); }
inline unsigned short StHbtParticle::PosTrackId() const { return mV0->idPos(); }
inline const StHbtThreeVector StHbtParticle::DecayVertexPosition() const {return mV0->decayVertexV0(); }
#endif
