// MergingHooks.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file is written by Stefan Prestel.
// Header file to allow user access to program at different stages.
// HardProcess: Container class for the hard process to be merged. Holds the
//              bookkeeping of particles not be be reclustered
// MergingHooks: Steering class for matrix element merging. Some functions can
//               be redefined in a derived class to have access to the merging

#ifndef Pythia8_MergingHooks_H
#define Pythia8_MergingHooks_H

#include "Basics.h"
#include "BeamParticle.h"
#include "Event.h"
#include "Info.h"
#include "ParticleData.h"
#include "PartonSystems.h"
#include "PythiaStdlib.h"
#include "Settings.h"

namespace Pythia8 {

//==========================================================================

// Declaration of hard process class
// This class holds information on the desired hard 2->2 process 
// for the merging.
// This class is a container class for History class use.

class HardProcess {

public:

   // Flavour of the first incoming particle
  int hardIncoming1;
  // Flavour of the second incoming particle
  int hardIncoming2;
  // Flavours of the outgoing particles
  vector<int> hardOutgoing1;
  vector<int> hardOutgoing2;
  // Flavour of intermediate bosons in the hard 2->2
  vector<int> hardIntermediate;

  // Current reference event
  Event state;
  // Potential positions of outgoing particles in reference event
  vector<int> PosOutgoing1;
  vector<int> PosOutgoing2;
  // Potential positions of intermediate bosons in reference event
  vector<int> PosIntermediate;

  // Information on merging scale read from LHE file
  double tms;
  // Type of ME generator
  int meGenType;

  // Default constructor
  HardProcess(){}
  // Default destructor
  ~HardProcess(){}

  // Copy constructor
  HardProcess( const HardProcess& hardProcessIn ) : state(hardProcessIn.state),
      tms(hardProcessIn.tms), meGenType(hardProcessIn.meGenType) {
    hardIncoming1 = hardProcessIn.hardIncoming1;
    hardIncoming2 = hardProcessIn.hardIncoming2;
    for(int i =0; i < int(hardProcessIn.hardOutgoing1.size());++i)
      hardOutgoing1.push_back( hardProcessIn.hardOutgoing1[i]);
    for(int i =0; i < int(hardProcessIn.hardOutgoing2.size());++i)
      hardOutgoing2.push_back( hardProcessIn.hardOutgoing2[i]);
    for(int i =0; i < int(hardProcessIn.hardIntermediate.size());++i)
      hardIntermediate.push_back( hardProcessIn.hardIntermediate[i]);
    for(int i =0; i < int(hardProcessIn.PosOutgoing1.size());++i)
      PosOutgoing1.push_back( hardProcessIn.PosOutgoing1[i]);
    for(int i =0; i < int(hardProcessIn.PosOutgoing2.size());++i)
      PosOutgoing2.push_back( hardProcessIn.PosOutgoing2[i]);
    for(int i =0; i < int(hardProcessIn.PosIntermediate.size());++i)
      PosIntermediate.push_back( hardProcessIn.PosIntermediate[i]);
  }

  // Constructor with path to LHE file
  HardProcess( string LHEfile, ParticleData* particleData) {
    state = Event();
    state.init("(hard process)", particleData);
    translateLHEFString(LHEfile);
  }

  // Constructor with core process input
  void initOnProcess( string process, ParticleData* particleData);

  // Constructor with path to LHE file input
  void initOnLHEF( string LHEfile, ParticleData* particleData);

  // Function to access the LHE file and read relevant information
  void translateLHEFString( string LHEpath);

  // Function to translate the process string (in MG/ME notation)
  void translateProcessString( string process);

  // Function to clear hard process information
  void clear();

  // Function to identify the hard subprocess in the current event
  void storeCandidates( const Event& event);
  // Function to check if the particle event[iPos] matches any of
  // the stored outgoing particles of the hard subprocess
  bool matchesAnyOutgoing(int iPos, const Event& event);
  // Function to return the type of the ME generator
  int MEgenType();
  // Function to get the number of coloured final state partons in the
  // hard process
  int nQuarksOut();
  // Function to get the number of uncoloured final state particles in the 
  // hard process
  int nLeptonOut();
  // Function to get the number of coloured initial state partons in the 
  // hard process
  int nQuarksIn();
  // Function to get the number of uncoloured initial state particles in the 
  // hard process
  int nLeptonIn();
  // Function to report if a resonace decay was found in the 2->2 sub-process 
  // of the  current state
  int hasResInCurrent();
  // Function to report the number of resonace decays in the 2->2 sub-process 
  // of the  current state
  int nResInCurrent();
  // Function to report if a resonace decay was found in the 2->2 hard process
  bool hasResInProc();
  // Function to print the hard process (for debug)
  void list() const;
  // Function to print the hard process candidates in the
  // Matrix element state (for debug)
  void listCandidates() const;

};

//==========================================================================

// MergingHooks is base class for user input to the merging procedure.

class MergingHooks {

public:

  // Constructor.
  MergingHooks() {}
  // Destructor.
  virtual ~MergingHooks() {}
  // Function encoding the functional definition of the merging scale
  virtual double tmsDefinition( const Event& event){ return event[0].e();}

  // Function returning the value of the merging scale.
  double tms() { return tmsValueSave;}
  // Function returning the value of the maximal number of merged jets.
  int nMaxJets() { return nJetMaxSave;}

  // Function to return the number of outgoing partons in the core process
  int nHardOutPartons(){ return hardProcess.nQuarksOut();}
  // Function to return the number of outgoing leptons in the core process
  int nHardOutLeptons(){ return hardProcess.nLeptonOut();}
  // Function to return the number of incoming partons (hadrons) in the core
  // process
  int nHardInPartons(){ return hardProcess.nQuarksIn();}
  // Function to return the number of incoming leptons in the core process
  int nHardInLeptons(){ return hardProcess.nLeptonIn();}
  // Function to report the number of resonace decays in the 2->2 sub-process 
  // of the  current state
  int nResInCurrent(){ return hardProcess.nResInCurrent();}

  // Function to return the number of clustering steps for the current event
  int getNumberOfClusteringSteps(const Event& event);

  // Function to determine if user defined merging should be applied
  bool doUserMerging(){ return doUserMergingSave;}
  // Function to determine if automated MG/ME merging should be applied
  bool doMGMerging() { return doMGMergingSave;}
  // Function to determine if KT merging should be applied
  bool doKTMerging() { return doKTMergingSave;}

  // Function to dampen weights calculated from histories with lowest 
  // multiplicity reclustered events that do not pass the ME cuts
  virtual double dampenIfFailCuts( const Event& inEvent ) {
    // Dummy statement to avoid compiler warnings
    if(false) cout << inEvent[0].e();
    return 1.;
  }

  // Hooks to disallow states in the construction of all histories, e.g.
  // because jets are below the merging scale or fail the matrix element cuts
  // Function to allow interference in the construction of histories 
  virtual bool canCutOnRecState() { return false; }
  // Function to check reclustered state while generating all possible
  // histories
  // Function implementing check of reclustered events while constructing
  // all possible histories
  virtual bool doCutOnRecState( const Event& event ) {
    // Dummy statement to avoid compiler warnings
    if(false) cout << event[0].e();
    return false;
  }

  // Make History class friend to allow access to advanced switches
  friend class History;
  // Make Pythia class friend
  friend class Pythia;
  // Make PartonLevel class friend
  friend class PartonLevel;

protected:

  // Functions for internal use inside Pythia source code
  // Initialize.
  void init(  Settings& settings, Info* infoPtrIn, 
    ParticleData* particleDataPtrIn, ostream& os = cout);
  // Function storing candidates for the hard process in the current event
  // Needed in order not to cluster members of the core process
  void storeHardProcessCandidates(const Event& event){
    hardProcess.storeCandidates(event);
  }
  // Function to set the path to the LHE file, so that more automated merging
  // can be used 
  void setLHEInputFile( string lheFile);

  // Function to save the current CKKWL weight
  void setWeight(double wgt){ weightSave = wgt;}

  // Function to calculate the minimal kT in the event
  double kTms(const Event & event);

  // Function to get the CKKWL weight for the current event
  double getWeight() { return weightSave;}

  // Helper class doing all the core process book-keeping
  HardProcess hardProcess;

  // Pointer to various information on the generation.
  Info*          infoPtr;

  // Pointer to the particle data table.
  ParticleData*  particleDataPtr;

  // AlphaS objects for alphaS reweighting use
  AlphaStrong AlphaS_FSRSave;
  AlphaStrong AlphaS_ISRSave;

  // Return AlphaStrong objects
  AlphaStrong* AlphaS_FSR() { return &AlphaS_FSRSave;}
  AlphaStrong* AlphaS_ISR() { return &AlphaS_ISRSave;}

  // Saved path to LHE file for more automated merging
  string lheInputFile;

  bool   doUserMergingSave, doMGMergingSave, doKTMergingSave, 
         includeMassiveSave, enforceStrongOrderingSave, orderInRapiditySave, 
         pickByFullPSave, pickByPoPT2Save, includeRedundantSave, 
         pickBySumPTSave, allowColourShufflingSave;
  int    unorderedScalePrescipSave, unorderedASscalePrescipSave,
         incompleteScalePrescipSave, ktTypeSave;
  double scaleSeparationFactorSave, nonJoinedNormSave,
         fsrInRecNormSave, herwigAcollFSRSave, herwigAcollISRSave,
         pT0ISRSave, pTcutSave;

  // Functions to return advanced merging switches
  // Include masses in definition of evolution pT and splitting kernels
  bool includeMassive() { return includeMassiveSave;}
  // Prefer strongly ordered histories
  bool enforceStrongOrdering() { return enforceStrongOrderingSave;}
  // Prefer histories ordered in rapidity and evolution pT
  bool orderInRapidity() { return orderInRapiditySave;}
  // Pick history probabilistically by full (correct) splitting probabilities
  bool pickByFull() { return pickByFullPSave;}
  // Pick history probabilistically, with easier form of probabilities
  bool pickByPoPT2() { return pickByPoPT2Save;}
  // Include redundant terms (e.g. PDF ratios) in the splitting probabilities
  bool includeRedundant(){ return includeRedundantSave;}
  // Pick by winner-takes-it-all, with minimum sum of scalar evolution pT
  bool pickBySumPT(){ return pickBySumPTSave;}

  // Prescription for combined scale of unordered emissions
  // 0 : use larger scale
  // 1 : use smaller scale
  int unorderedScalePrescip() { return unorderedScalePrescipSave;}
  // Prescription for combined scale used in alpha_s for unordered emissions
  // 0 : use combined emission scale in alpha_s weight for both (!) splittings
  // 1 : use original reclustered scales of each emission in alpha_s weight
  int unorderedASscalePrescip() { return unorderedASscalePrescipSave;}
  // Prescription for starting scale of incomplete histories
  // 0: use factorization scale
  // 1: use sHat
  // 2: use s
  int incompleteScalePrescip() { return incompleteScalePrescipSave;}

  // Allow swapping one colour index while reclustering
  bool allowColourShuffling() { return allowColourShufflingSave;}

  // Factor by which two scales should differ to be classified strongly ordered
  double scaleSeparationFactor() { return scaleSeparationFactorSave;}
  // Absolute normalization of splitting probability for non-joined
  // evolution
  double nonJoinedNorm() { return nonJoinedNormSave;}
  // Absolute normalization of splitting probability for final state
  // splittings with initial state recoiler
  double fsrInRecNorm() { return fsrInRecNormSave;}
  // Factor multiplying scalar evolution pT for FSR splitting, when picking 
  // history by minimum scalar pT (see Jonathan Tully's thesis)
  double herwigAcollFSR() { return herwigAcollFSRSave;}
  // Factor multiplying scalar evolution pT for ISR splitting, when picking 
  // history by minimum scalar pT (see Jonathan Tully's thesis)
  double herwigAcollISR() { return herwigAcollISRSave;}
  // ISR regularisation scale
  double pT0ISR() { return pT0ISRSave;}
  // Shower cut-off scale
  double pTcut() { return pTcutSave;}

  // Function to calculate the kT separation between two particles
  double kTdurham(const Particle& RadAfterBranch,
    const Particle& EmtAfterBranch, int Type, double D );

  // Saved members. Not used at the moment
  double tmsValueSave;
  int nJetMaxSave;
  string processSave;
  double weightSave;

};

//==========================================================================

} // end namespace Pythia8

#endif // Pythia8_MergingHooks_H
