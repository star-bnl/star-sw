/***************************************************************************
 *
 * $Id: StHbtParticle.hh,v 1.19 2003/01/14 09:41:16 renault Exp $
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
 * Revision 1.19  2003/01/14 09:41:16  renault
 * changes on average separation calculation, hit shared finder and memory optimisation
 * for Z,U and Sectors variables.
 *
 * Revision 1.18  2002/12/12 17:01:50  kisiel
 * Hidden Information handling and purity calculation
 *
 * Revision 1.17  2002/11/19 23:35:52  renault
 * Enable calculation of exit/entrance separation for V0 daughters
 *
 * Revision 1.16  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.15  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.14  2001/05/23 00:19:05  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 * Revision 1.13  2001/04/03 21:04:36  kisiel
 *
 *
 *   Changes needed to make the Theoretical code
 *   work. The main code is the ThCorrFctn directory.
 *   The most visible change is the addition of the
 *   HiddenInfo to StHbtPair.
 *
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
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Infrastructure/StHbtXi.hh"
#include "StPhysicalHelixD.hh"
// ***
class StHbtHiddenInfo;
// ***
class StHbtParticle{
public:
  StHbtParticle();
  StHbtParticle(const StHbtTrack* const hbtTrack, const double& mass);
  StHbtParticle(const StHbtV0* const hbtV0, const double& mass);
  StHbtParticle(const StHbtKink* const hbtKink, const double& mass);
  StHbtParticle(const StHbtXi* const hbtXi, const double& mass);
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
  StHbtKink* Kink() const;

  const StHbtThreeVector& NominalTpcExitPoint() const;     // position track exits TPC assuming start at (0,0,0)
  const StHbtThreeVector& NominalTpcEntrancePoint() const; // position track crosses IFC assuming start at (0,0,0)
  const StHbtThreeVector& TpcV0PosExitPoint() const;  
  const StHbtThreeVector& TpcV0PosEntrancePoint() const;
  const StHbtThreeVector& TpcV0NegExitPoint() const;  
  const StHbtThreeVector& TpcV0NegEntrancePoint() const;

  // the following method is for explicit internal calculation to fill datamembers.
  // It is invoked automatically if StHbtParticle constructed from StHbtTrack
  //void CalculateNominalTpcExitAndEntrancePoints(); // NOTE - this requires the mHelix, so be sure this is filled


  StHbtThreeVector mNominalPosSample[11];  // I make this public for convenience and speed of StHbtPair()
  float mZ[45];
  float mU[45];
  int mSect[45];

  void ResetFourMomentum(const StHbtLorentzVector& fourMomentum);

  const StHbtHiddenInfo*  HiddenInfo() const;
  // Fab private
  StHbtHiddenInfo*  getHiddenInfo() const;
  void SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo);
  void CalculatePurity();
  double GetPionPurity();
  double GetKaonPurity();
  double GetProtonPurity();
  void CalculateTpcExitAndEntrancePoints( StPhysicalHelixD* tHelix,
					  StHbtThreeVector* PrimVert,
					  StHbtThreeVector* SecVert,
					  StHbtThreeVector* tmpTpcEntrancePoint,
					  StHbtThreeVector* tmpTpcExitPoint,
					  StHbtThreeVector* tmpPosSample,
					  float* tmpZ,float* tmpU,int* tmpSect);

  // For V0 Neg Daugthers TpcEntrance/ExitPoints
  StHbtThreeVector* mTpcV0NegPosSample;
  float* mV0NegZ;
  float* mV0NegU;
  int* mV0NegSect;
 
private:
  StHbtTrack* mTrack;  // copy of the track the particle was formed of, else Null
  StHbtV0* mV0;        // copy of the v0 the particle was formed of, else Null
  StHbtKink* mKink;        // copy of the v0 the particle was formed of, else Null
  StHbtXi* mXi;

  StHbtLorentzVector mFourMomentum;
  StPhysicalHelixD mHelix;
  unsigned long  mMap[2]; 
  int mNhits;
  StHbtThreeVector mNominalTpcExitPoint;
  StHbtThreeVector mNominalTpcEntrancePoint;
  StHbtHiddenInfo* mHiddenInfo;  // Fab private

  double mPurity[6];

  static double mPrimPimPar0;
  static double mPrimPimPar1;
  static double mPrimPimPar2;
  static double mPrimPipPar0;
  static double mPrimPipPar1;
  static double mPrimPipPar2;
  static double mPrimPmPar0;
  static double mPrimPmPar1;
  static double mPrimPmPar2;
  static double mPrimPpPar0;
  static double mPrimPpPar1;
  static double mPrimPpPar2;

   // For V0 Daugthers TpcEntrance/ExitPoints
  StHbtThreeVector mPrimaryVertex;
  StHbtThreeVector mSecondaryVertex;

  StPhysicalHelixD mHelixV0Pos;
  StHbtThreeVector mTpcV0PosEntrancePoint;
  StHbtThreeVector mTpcV0PosExitPoint;

  StPhysicalHelixD mHelixV0Neg;
  StHbtThreeVector mTpcV0NegEntrancePoint;
  StHbtThreeVector mTpcV0NegExitPoint;
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
// ***
inline StHbtHiddenInfo* StHbtParticle::getHiddenInfo() const
{return mHiddenInfo;}
inline const StHbtHiddenInfo* StHbtParticle::HiddenInfo() const
{return mHiddenInfo;}
inline void StHbtParticle::SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo)
{ mHiddenInfo = aHiddenInfo->clone();}
// ***

inline void StHbtParticle::ResetFourMomentum(const StHbtLorentzVector& vec){mFourMomentum = vec;}

inline StHbtKink* StHbtParticle::Kink() const { return mKink; }

#endif
