/***************************************************************************
 *
 * $Id: StHbtTrack.cc,v 1.19 2005/07/15 17:41:43 kopytin Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * Implementation of methods
 *
 ***************************************************************************
 * $Log: StHbtTrack.cc,v $
 * Revision 1.19  2005/07/15 17:41:43  kopytin
 * initialized mHiddenInfo in StHbtTrack() to null
 *
 * Revision 1.18  2005/07/10 02:17:21  chajecki
 * Ftpc (Eeat+West) hits included in nHits()
 *
 * Revision 1.17  2005/06/17 21:41:12  chajecki
 * Two bugs fixed:
 * 1. wrong value of mNHits set in one of the constructors
 * StHbtTrack::StHbtTrack(const StTrack* ST, StHbtThreeVector PrimaryVertex)
 *
 * 2. since year 4 the StMuTrack::nHits() returns the total number of hits.
 * We're interested in TPC hits only so this value can be taken by
 *  StMuTrack::topologyMap().numberOfHits(kTpcId);
 * This change is backwards compatible so the code should work also
 * for data <=Y3
 *
 * Revision 1.16  2003/03/18 14:41:48  kisiel
 * Bugfix update for the theoretical part of the code. Reverts the changes to the Lednicky weight calculator, as the previuos one had problems with strong interaction
 *
 * Revision 1.15  2003/01/31 19:57:15  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.14  2003/01/21 17:26:33  magestro
 * Added condition to globalTrack() call so GlobalTracks branch in MuDst can be disabled
 *
 * Revision 1.13  2002/03/21 18:49:31  laue
 * updated for new MuDst reader
 *
 * Revision 1.12  2002/02/04 18:58:33  laue
 * *** empty log message ***
 *
 * Revision 1.11  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.4  2001/06/21 19:15:48  laue
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
 * Revision 1.3  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 *
 ****************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtTrack.hh" 
#ifdef __ROOT__
#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h" 
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h" 
#include "StEvent/StEnumerations.h"
#include "StHbtMaker/Infrastructure/StHbtAihongPid.h"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"
#endif

StHbtTrack::StHbtTrack()
{
  mHiddenInfo = NULL;
}


StHbtTrack::StHbtTrack(const StHbtTrack& t) { // copy constructor
  mTrackType = t.mTrackType;
  mCharge = t.mCharge;
  mNHits = t.mNHits;
  mNHitsPoss = t.mNHitsPoss;
  mNHitsDedx = t.mNHitsDedx;
  mNSigmaElectron = t.mNSigmaElectron;
  mNSigmaPion = t.mNSigmaPion;
  mNSigmaKaon = t.mNSigmaKaon;
  mNSigmaProton = t.mNSigmaProton;
  mPidProbElectron = t.mPidProbElectron;
  mPidProbPion = t.mPidProbPion;
  mPidProbKaon = t.mPidProbKaon;
  mPidProbProton = t.mPidProbProton;
  mdEdx = t.mdEdx;
  mDCAxy = t.mDCAxy;
  mDCAz = t.mDCAz; 
  mDCAxyGlobal = t.mDCAxyGlobal;
  mDCAzGlobal = t.mDCAzGlobal; 
  mChiSqXY = t.mChiSqXY;
  mChiSqZ = t.mChiSqZ;
  mP = t.mP;
  mPt = t.mPt;
  mPGlobal = t.mPGlobal;
  mPtGlobal = t.mPtGlobal;
  mHelix = t.mHelix;
  mHelixGlobal = t.mHelixGlobal;
  mMap[0] = t.mMap[0];
  mMap[1] = t.mMap[1];
  mTrackId = t.mTrackId;
  if (t.ValidHiddenInfo())
    mHiddenInfo = t.getHiddenInfo()->clone();
  else 
    mHiddenInfo = NULL;
};
//___________________________________________________
#ifdef __ROOT__
#include "StEventTypes.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StEvent/StEnumerations.h"
#include "StarClassLibrary/SystemOfUnits.h"   // has "tesla" in it
#include "StarClassLibrary/StParticleTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h" 
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h" 

StHbtTrack::StHbtTrack(const StTrack* ST, StHbtThreeVector PrimaryVertex)
{
  StTpcDedxPidAlgorithm PidAlgorithm;

  // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
  // pointers to track and pidTraits are set 
  // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINE
  // StParticleDefinition* BestGuess = (StParticleDefinition*)ST->pidTraits(PidAlgorithm);

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();

  // OK let's go...
  mHiddenInfo = 0;
  mTrackType = ST->type();
  mCharge = ST->geometry()->charge();
  mNHits = ST->detectorInfo()->numberOfPoints(kTpcId);
  mNHitsPoss = ST->numberOfPossiblePoints(kTpcId);
  mNHitsDedx = ST->fitTraits().numberOfFitPoints(kTpcId);

  mNSigmaElectron = PidAlgorithm.numberOfSigma(Electron);
  mNSigmaPion = PidAlgorithm.numberOfSigma(Pion);
  mNSigmaKaon = PidAlgorithm.numberOfSigma(Kaon);
  mNSigmaProton = PidAlgorithm.numberOfSigma(Proton);
  if ( PidAlgorithm.traits() )
    mdEdx = PidAlgorithm.traits()->mean();
  else 
    cout << "StHbtTrack(const StTrack* ST, StHbtThreeVector PrimaryVertex) - no traits " << endl;

  mChiSqXY = ST->fitTraits().chi2(0);
  mChiSqZ = ST->fitTraits().chi2(1);

  mP = ST->geometry()->momentum();
  mPt = mP.perp();
  mHelix = ST->geometry()->helix();
  double pathlength = ST->geometry()->helix().pathLength(PrimaryVertex);
  StHbtThreeVector  DCAxyz = ST->geometry()->helix().at(pathlength)-PrimaryVertex;
  mDCAxy = DCAxyz.perp();
  mDCAz = DCAxyz.z();

  mHelixGlobal = ST->node()->track(global)->geometry()->helix();
  double pathlengthGlobal = mHelixGlobal.pathLength(PrimaryVertex);
  StHbtThreeVector  DCAxyzGlobal = mHelixGlobal.at(pathlengthGlobal)-PrimaryVertex;
  mDCAxyGlobal = DCAxyzGlobal.perp();
  mDCAzGlobal = DCAxyzGlobal.z();
  mPGlobal = ST->node()->track(global)->geometry()->momentum();
  mPtGlobal = mPGlobal.perp();

  mMap[0] = ST->topologyMap().data(0);
  mMap[1] = ST->topologyMap().data(1);
  mTrackId = ST->key();
}

StHbtTrack::StHbtTrack(const StEvent* EV, const StTrack* ST) {
  StTpcDedxPidAlgorithm PidAlgorithm;
  // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
  // pointers to track and pidTraits are set 
  // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINE
  ST->pidTraits(PidAlgorithm);

  // the following just point to particle definitions in StEvent
  StElectron* Electron = StElectron::instance();
  StPionPlus* Pion = StPionPlus::instance();
  StKaonPlus* Kaon = StKaonPlus::instance();
  StProton* Proton = StProton::instance();

   // OK let's go...
  mHiddenInfo = 0;
  mTrackType = ST->type();
  mCharge = ST->geometry()->charge();
  mNHits = ST->detectorInfo()->numberOfPoints(kTpcId);
  mNHitsPoss = ST->numberOfPossiblePoints(kTpcId);
  mNHitsDedx = ST->fitTraits().numberOfFitPoints(kTpcId);

  mNSigmaElectron = PidAlgorithm.numberOfSigma(Electron);
  mNSigmaPion = PidAlgorithm.numberOfSigma(Pion);
  mNSigmaKaon = PidAlgorithm.numberOfSigma(Kaon);
  mNSigmaProton = PidAlgorithm.numberOfSigma(Proton);
  mdEdx = PidAlgorithm.traits()->mean();


  mChiSqXY = ST->fitTraits().chi2(0);
  mChiSqZ = ST->fitTraits().chi2(1);

  StHbtThreeVector primaryVertex = EV->primaryVertex()->position();
  
  mHelix = ST->geometry()->helix();
  double pathlength = mHelix.pathLength(primaryVertex);
  StHbtThreeVector  DCAxyz = mHelix.at(pathlength)-primaryVertex;
  mDCAxy = DCAxyz.perp();
  mDCAz = DCAxyz.z();
  mP = mHelix.momentumAt(pathlength,EV->summary()->magneticField()*kilogauss);
  mPt = mP.perp();

  if(dynamic_cast<const StPrimaryTrack*>(ST)){
    mHelixGlobal = ST->node()->track(global)->geometry()->helix();
  }
  else{
    mHelixGlobal = mHelix;
  }
  double pathlengthGlobal = mHelixGlobal.pathLength(primaryVertex);
  StHbtThreeVector  DCAxyzGlobal = mHelixGlobal.at(pathlengthGlobal)-primaryVertex;
  mDCAxyGlobal = DCAxyzGlobal.perp();
  mDCAzGlobal = DCAxyzGlobal.z();
  mPGlobal = mHelixGlobal.momentumAt(pathlengthGlobal,EV->summary()->magneticField()*kilogauss);
  mPtGlobal = mPGlobal.perp();
  
  mMap[0] = ST->topologyMap().data(0);
  mMap[1] = ST->topologyMap().data(1);
  mTrackId = ST->key();
  
  // On the fly pid probability calculation
  static int previousEventNumber = 0;
  static StHbtAihongPid* hbtAihongPid = StHbtAihongPid::Instance();
  static StuProbabilityPidAlgorithm* aihongPid = hbtAihongPid->aihongPid();
  if( (mPidProbElectron+mPidProbPion+mPidProbKaon+mPidProbProton) <= 0.){
    if (previousEventNumber != EV->info()->id()) {
      // must do the below because uncorrectedNumberOfPositivePrimaries() function does not have const argument
      StEvent* TempEv = (StEvent*)EV;
      hbtAihongPid->updateEvent(uncorrectedNumberOfPositivePrimaries(*TempEv));
      //      hbtAihongPid->updateEvent(uncorrectedNumberOfPositivePrimaries(*EV));
      previousEventNumber = EV->info()->id();
    }
    hbtAihongPid->updateTrack( (int)mCharge, mP.mag(), mP.pseudoRapidity(), mNHitsDedx, mdEdx);
    mPidProbElectron= (mCharge>0) ? aihongPid->beingPositronProb() : aihongPid->beingElectronProb() ;
    mPidProbPion= (mCharge>0) ? aihongPid->beingPionPlusProb() : aihongPid->beingPionMinusProb();
    mPidProbKaon= (mCharge>0) ? aihongPid->beingKaonPlusProb() : aihongPid->beingKaonMinusProb(); 
    mPidProbProton=  (mCharge>0) ? aihongPid->beingProtonProb() : aihongPid->beingAntiProtonProb(); 
  }
}

// StHbtTrack::StHbtTrack(const StTrack* ST, StHbtThreeVector PrimaryVertex)
// {
//   StTpcDedxPidAlgorithm* PidAlgorithm = new StTpcDedxPidAlgorithm();
//   if (!PidAlgorithm) cout << " StHbtTrack::StHbtTrack(StTrack*) - Whoa!! No PidAlgorithm!! " << endl;
//   // while getting the bestGuess, the pidAlgorithm (StTpcDedxPidAlgorithm) is set up.
//   // pointers to track and pidTraits are set 
//   // So, even though BestGuess will generate a "variable not used" warning, DO NOT DELETE THE NEXT LINE
//   StParticleDefinition* BestGuess = (StParticleDefinition*)ST->pidTraits(*PidAlgorithm);

//   // the following just point to particle definitions in StEvent
//   StElectron* Electron = StElectron::instance();
//   StPionPlus* Pion = StPionPlus::instance();
//   StKaonPlus* Kaon = StKaonPlus::instance();
//   StProton* Proton = StProton::instance();
 
 

//   // OK let's go...
//   mHiddenInfo = 0;
//   mTrackType = ST->type();
//   mCharge = ST->geometry()->charge();
//   mNHits = ST->detectorInfo()->numberOfPoints(kTpcId);
//   mNHitsPoss = ST->numberOfPossiblePoints(kTpcId);
//   mNSigmaElectron = PidAlgorithm->numberOfSigma(Electron);
//   mNSigmaPion = PidAlgorithm->numberOfSigma(Pion);
//   mNSigmaKaon = PidAlgorithm->numberOfSigma(Kaon);
//   mNSigmaProton = PidAlgorithm->numberOfSigma(Proton);
//   const StDedxPidTraits* tTrait = PidAlgorithm->traits();
//   if(tTrait) {mdEdx = tTrait->mean();}
//   else{
//     mdEdx = 0;
//     cerr << "No pid trait!!!" << endl;
//   }

//   mP = ST->geometry()->momentum();
//   mPt = mP.perp();

//   double pathlength = ST->geometry()->helix().pathLength(PrimaryVertex);
//   StHbtThreeVector  DCAxyz = ST->geometry()->helix().at(pathlength)-PrimaryVertex;
//   mDCAxy = DCAxyz.perp();
//   mDCAz = DCAxyz.z();

//   mChiSqXY = ST->fitTraits().chi2(0);
//   mChiSqZ = ST->fitTraits().chi2(1);
//   mHelix = ST->geometry()->helix();
//   mHelixGlobal = ST->node()->track(global)->geometry()->helix();
//   mMap[0] = ST->topologyMap().data(0);
//   mMap[1] = ST->topologyMap().data(1);
//   mTrackId = ST->key();

// }

StHbtTrack::StHbtTrack(const StHbtTTreeEvent* ev, const StHbtTTreeTrack* t) { // copy constructor
  mTrackType = t->mTrackType;
  mTrackId = t->mTrackId;
  mNHits = t->mNHits;
  mNHitsPoss = t->mNHitsPoss; 
  mNHitsDedx = t->mNHitsDedx; 
  mNSigmaElectron = t->mNSigmaElectron;
  mNSigmaPion = t->mNSigmaPion;
  mNSigmaKaon = t->mNSigmaKaon;
  mNSigmaProton = t->mNSigmaProton;
  mPidProbElectron = t->mPidProbElectron/1000.;
  mPidProbPion = t->mPidProbPion/1000.;
  mPidProbKaon = t->mPidProbKaon/1000.;
  mPidProbProton = t->mPidProbProton/1000.;

  mdEdx = t->mdEdx;
  mChiSqXY = t->mChiSqXY;
  mChiSqZ = t->mChiSqZ;
  mMap[0] = t->mMap[0];
  mMap[1] = t->mMap[1];
  
  mHelix = StPhysicalHelixD(t->mHelixC,t->mHelixDip,t->mHelixPhase,
			    StThreeVectorD(t->mHelixX,t->mHelixY,t->mHelixZ),
			    t->mHelixH);
  mHelixGlobal = StPhysicalHelixD(t->mHelixGlobalC,t->mHelixGlobalDip,t->mHelixGlobalPhase,
			    StThreeVectorD(t->mHelixGlobalX,t->mHelixGlobalY,t->mHelixGlobalZ),
			    t->mHelixGlobalH);
  //ev->mMagneticField
  //  cout << mHelix << endl;
  mCharge =mHelix.charge(ev->mMagneticField*kilogauss);
  StHbtThreeVector vertex(ev->mVertexX,ev->mVertexY,ev->mVertexZ);
  double pathlength = mHelix.pathLength(vertex);
  //  cout << pathlength << endl;
  mP = mHelix.momentumAt(pathlength,ev->mMagneticField*kilogauss);
  mPt = mP.perp();
  //  cout << mP << endl;
  mDCAxy = (mHelix.at(pathlength) - vertex).perp();
  mDCAz = (mHelix.at(pathlength) - vertex).z();
  
  double pathlengthGlobal = mHelixGlobal.pathLength(vertex);
  mDCAxyGlobal = (mHelixGlobal.at(pathlengthGlobal) - vertex).perp();
  mDCAzGlobal = (mHelixGlobal.at(pathlengthGlobal) - vertex).z();
  mPGlobal = mHelixGlobal.momentumAt(pathlengthGlobal,ev->mMagneticField*kilogauss);
  mPtGlobal = mPGlobal.perp();

  // On the fly pid probability calculation
  static unsigned int previousEventNumber = 0;
  static StHbtAihongPid* hbtAihongPid = StHbtAihongPid::Instance();
  static StuProbabilityPidAlgorithm* aihongPid = hbtAihongPid->aihongPid();
  if( (mPidProbElectron+mPidProbPion+mPidProbKaon+mPidProbProton) <= 0.){
    if (previousEventNumber != ev->mEventNumber) {
      hbtAihongPid->updateEvent((int)ev->mUncorrectedNumberOfNegativePrimaries);
      previousEventNumber = ev->mEventNumber;
    }
    hbtAihongPid->updateTrack( (int)mCharge, mP.mag(), mP.pseudoRapidity(), mNHitsDedx, mdEdx);
    mPidProbElectron= (mCharge>0) ? aihongPid->beingPositronProb() : aihongPid->beingElectronProb() ;
    mPidProbPion= (mCharge>0) ? aihongPid->beingPionPlusProb() : aihongPid->beingPionMinusProb();
    mPidProbKaon= (mCharge>0) ? aihongPid->beingKaonPlusProb() : aihongPid->beingKaonMinusProb(); 
    mPidProbProton=  (mCharge>0) ? aihongPid->beingProtonProb() : aihongPid->beingAntiProtonProb(); 
 }


  // Onfly pid probability calculation
  int dontskip=0;
  if(dontskip &&
     (mPidProbElectron+mPidProbPion+mPidProbKaon+mPidProbProton) <= 0.){
    static int NotFirst;
    static StuProbabilityPidAlgorithm AihongsPID;
    if(!NotFirst){
      NotFirst=1;
      StuProbabilityPidAlgorithm::readParametersFromFile("PIDTable.root");
    }
    static const StHbtTTreeEvent* evPrev;
    if(evPrev!=ev){
      // to be changed
      int tRefMult = ev->mUncorrectedNumberOfNegativePrimaries;
      double tCent;
      if (tRefMult > 225 ) tCent = 0.03;
      else if (tRefMult > 215 ) tCent = 0.05;
      else if (tRefMult > 200 ) tCent = 0.07;
      else if (tRefMult > 180 ) tCent = 0.10;
      else if (tRefMult > 140 ) tCent = 0.18;
      else if (tRefMult > 130 ) tCent = 0.20; 
      else if (tRefMult > 120 ) tCent = 0.23;
      else if (tRefMult > 115 ) tCent = 0.24;
      else if (tRefMult > 100 ) tCent = 0.28;
      else tCent = 0.99;
      evPrev=ev;
    }
    // AihongsPID.processPIDAsFunction(tCent, 0., (int) mCharge,mP.mag(),
    //			    mP.pseudoRapidity(),mNHitsDedx,mdEdx);
    mPidProbElectron= mCharge>0 ? AihongsPID.beingPositronProb() : 
      AihongsPID.beingElectronProb();
    mPidProbPion= mCharge>0 ? AihongsPID.beingPionPlusProb(): 
      AihongsPID.beingPionMinusProb();
    mPidProbKaon= mCharge>0 ? AihongsPID.beingKaonPlusProb(): 
      AihongsPID.beingKaonMinusProb(); 
    mPidProbProton=  mCharge>0 ? AihongsPID.beingProtonProb(): 
      AihongsPID.beingAntiProtonProb(); 
  }
  mHiddenInfo = 0;
};

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
StHbtTrack::StHbtTrack(const StMuDst* dst, const StMuTrack* t) { // copy constructor
  StMuEvent* ev = dst->event();
   mTrackType = t->type();
   mTrackId = t->id();


   mNHits = t->topologyMap().numberOfHits(kTpcId) + t->topologyMap().numberOfHits(kFtpcEastId) + t->topologyMap().numberOfHits(kFtpcWestId);

   mNHitsPoss = t->nHitsPoss(); 
   mNHitsDedx = t->nHitsDedx(); 
   mNSigmaElectron = t->nSigmaElectron();
   mNSigmaPion = t->nSigmaPion();
   mNSigmaKaon = t->nSigmaKaon();
   mNSigmaProton = t->nSigmaProton();
   mPidProbElectron = t->pidProbElectron();
   mPidProbPion = t->pidProbPion();
   mPidProbKaon = t->pidProbKaon();
   mPidProbProton = t->pidProbProton();
   mdEdx = t->dEdx();
   mChiSqXY = t->chi2xy();
   mChiSqZ = t->chi2z();
   mMap[0] = t->topologyMap().data(0);
   mMap[1] = t->topologyMap().data(1);
   mHelix = t->helix();
   if(t->globalTrack()) mHelixGlobal = t->globalTrack()->helix();
   mCharge = t->charge();

   
   double pathlength = mHelix.pathLength(ev->primaryVertexPosition());
   mP = mHelix.momentumAt(pathlength,ev->magneticField()*kilogauss);
   mPt = mP.perp();
   StThreeVectorD dca(mHelix.at(pathlength) - ev->primaryVertexPosition());
   mDCAxy = dca.perp();
   mDCAz =  dca.z();

   if(t->globalTrack()) {  
     double pathlengthGlobal = mHelixGlobal.pathLength(ev->primaryVertexPosition());
     mPGlobal = mHelixGlobal.momentumAt(pathlengthGlobal,ev->magneticField()*kilogauss);
     mPtGlobal = mPGlobal.perp();
     StThreeVectorD dcaGlobal(mHelixGlobal.at(pathlengthGlobal) - ev->primaryVertexPosition());
     mDCAxyGlobal = dcaGlobal.perp();
     mDCAzGlobal =  dcaGlobal.z();
   }

//    cout << "*******************" << endl;
//    cout << " vertex " << ev->primaryVertexPosition() << endl;
//    cout << " helix " << mHelix << endl;
//    cout << " helix " << mHelixGlobal << endl;
//    cout << " p " << mP << " pmag " << mP.mag() << "     " << t->p() << endl;
//    cout << " p " << mPGlobal << " pmag " << mPGlobal.mag() << endl;
//    cout << " l " << pathlength << " dca " << dca << " dcamag " << dca.mag() << endl;
//    cout << " l " << pathlengthGlobal << " dca " << dcaGlobal << " dcamag " << dcaGlobal.mag() << endl;

  
   mHiddenInfo = 0;
};

#endif

void StHbtTrack::SetTrackType(const short& t){mTrackType=t;}
void StHbtTrack::SetCharge(const short& ch){mCharge=ch;}
void StHbtTrack::SetNHits(const short& nh){mNHits=nh;}
void StHbtTrack::SetNHitsPossible(const short& nh){mNHitsPoss=nh;}
void StHbtTrack::SetNHitsDedx(const short& nh){mNHitsDedx=nh;}
void StHbtTrack::SetNSigmaElectron(const float& x){mNSigmaElectron = x;}
void StHbtTrack::SetNSigmaPion(const float& x){mNSigmaPion = x;}
void StHbtTrack::SetNSigmaKaon(const float& x){mNSigmaKaon = x;}
void StHbtTrack::SetNSigmaProton(const float& x){mNSigmaProton = x;}
void StHbtTrack::SetPidProbElectron(const float& x){mPidProbElectron = x;}
void StHbtTrack::SetPidProbPion(const float& x){mPidProbPion = x;}
void StHbtTrack::SetPidProbKaon(const float& x){mPidProbKaon = x;}
void StHbtTrack::SetPidProbProton(const float& x){mPidProbProton = x;}

void StHbtTrack::SetdEdx(const float& x){mdEdx = x;}

void StHbtTrack::SetDCAxy(const float& x){mDCAxy = x;}
void StHbtTrack::SetDCAz(const float& x){mDCAz = x;}
void StHbtTrack::SetDCAxyGlobal(const float& x){mDCAxyGlobal = x;}
void StHbtTrack::SetDCAzGlobal(const float& x){mDCAzGlobal = x;}
void StHbtTrack::SetChiSquaredXY(const float& x){mChiSqXY = x;} 
void StHbtTrack::SetChiSquaredZ(const float& x){mChiSqZ = x;}   
void StHbtTrack::SetP(const StHbtThreeVector& p){mP = p;}
void StHbtTrack::SetPt(const float& pt){mPt = pt;} 
void StHbtTrack::SetPGlobal(const StHbtThreeVector& p){mP = p;}
void StHbtTrack::SetPtGlobal(const float& pt){mPt = pt;} 
void StHbtTrack::SetHelix(const StPhysicalHelixD& h){mHelix = h;}
void StHbtTrack::SetHelixGlobal(const StPhysicalHelixD& h){mHelixGlobal = h;}
void StHbtTrack::SetTopologyMap(const int word, const unsigned int map) { mMap[word]=map;}
void StHbtTrack::SetTrackId(const short & id) { mTrackId=id;}

short StHbtTrack::TrackType() const {return mTrackType;}
short StHbtTrack::Charge() const {return mCharge;}
short StHbtTrack::NHits() const {return mNHits;}
short StHbtTrack::NHitsPossible() const {return mNHitsPoss;}
short StHbtTrack::NHitsDedx() const {return mNHitsDedx;}
float StHbtTrack::NSigmaElectron() const {return mNSigmaElectron;}
float StHbtTrack::NSigmaPion() const {return mNSigmaPion;}
float StHbtTrack::NSigmaKaon() const {return mNSigmaKaon;}
float StHbtTrack::NSigmaProton() const {return mNSigmaProton;}
float StHbtTrack::dEdx() const {return mdEdx;}

float StHbtTrack::DCAxy() const {return mDCAxy;}          
float StHbtTrack::DCAz() const {return mDCAz;}            
float StHbtTrack::DCAxyGlobal() const {return mDCAxyGlobal;}          
float StHbtTrack::DCAzGlobal() const {return mDCAzGlobal;}
float StHbtTrack::ChiSquaredXY() const {return mChiSqXY;} 
float StHbtTrack::ChiSquaredZ() const {return mChiSqZ;}   
StHbtThreeVector StHbtTrack::P() const {return mP;}
float StHbtTrack::Pt() const {return mPt;}              
const StPhysicalHelixD& StHbtTrack::Helix() const {return mHelix;}
const StPhysicalHelixD& StHbtTrack::HelixGlobal() const {return mHelixGlobal;}
unsigned int StHbtTrack::TopologyMap(const unsigned int word) const { return mMap[word];}
short StHbtTrack::TrackId() const { return mTrackId; }

void StHbtTrack::SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo) {mHiddenInfo=aHiddenInfo;}
bool StHbtTrack::ValidHiddenInfo() const { if (mHiddenInfo) return true; else return false; }
StHbtHiddenInfo* StHbtTrack::getHiddenInfo() const {return mHiddenInfo;}

StHbtTrack::~StHbtTrack()
{
   if (mHiddenInfo)
     delete mHiddenInfo;
}
