/*! \class StDijetFilter
\author Matthew Walker

This class implements a dijet filter using the StMCFilter framework for running PYTHIA filters in starsim. The code implements the midpoint cone algorithm with split merge on final state particles. There are additional dijet selection criteria on the jets.

There is also a section that makes a simple correction for reconstruction effects to try and account for the effects of detector resolution. This results in upward shifts of jet energies and allows the acceptance of some events from below threshold. Some of these events reconstruct in the signal phase space.

The filter passes the event at the GT stage if it finds either a signal dijet or a dijet that might be found after detector simulation and reconstruction ("reco dijet")

There is a also Four Vector class JetFourVec written to allow for usual four vec functionality.

*/

#ifndef STAR_StDijetFilter
#define STAR_StDijetFilter

#include <vector>
#include <string>
#include "StMCFilter.h"

class StGenParticleMaster;
class StGenParticle;

class JetFourVec
{

 private:
  float px; ///px of four vector
  float py; ///py of four vector
  float pz; ///pz of four vector
  float en; ///energy of four vector
  int code; ///pdg code of four vector
 
 public:
  JetFourVec(); ///blank constructor
  JetFourVec(JetFourVec*); ///copy constructor
  JetFourVec(StGenParticle*); ///construct from StGenParticle
  ~JetFourVec(){} ///destructor

  JetFourVec operator +(JetFourVec); ///addition operator
  bool operator ==(JetFourVec); ///equality operator

  void setCode(int x){code = x;}; ///setter for pdg code
  void setPx(float x){px = x;} ///setter for px
  void setPy(float x){py = x;} ///setter for py
  void setPz(float x){pz = x;} ///setter for pz
  void setEn(float x){en = x;} ///setter for en
  void setPxPyPzEn(float,float,float,float); ///four element setter
  void setPtEtaPhiM(float,float,float,float); ///alternative four element setter
  void setPtEtaPhiE(float,float,float,float); ///alternative four element setter

  int getCode(){return code;} ///getter for pdg code
  float getPx(){return px;} ///getter for px
  float getPy(){return py;} ///getter for py
  float getPz(){return pz;} ///getter for pz
  float getEn(){return en;} ///getter for en

  float Pt(); ///calculate vector pt
  float Eta(); ///calculate vector pseudorapidity
  float Phi(); ///calculate vector phi
  float Theta(); ///calculate vector theta
  float M(); ///calculate vector mass
  float P(); ///calculate vector total momentum
};


class StDijetFilter : public StMCFilter
{
 private:

  float mRBTOW; ///BEMC tower radius
  float mRcone; ///jetfinder cone radius
  float mSeed; ///jetfinder minimum ET to be seed
  float mAssoc; ///jet finder minimum ET be associated with jet
  float mSplitfraction; ///fraction for splitting jets
  bool mAddmidpoints; ///set use of midpoints
  bool mStablemidpoints; ///set require stability of mid points (not implemented)
  bool mSplitmerge; ///set use of split merge
  float mParticleEtaRange; ///range of particles to jet find on
  float mJetEtaHigh; ///high range of jet eta to use for finding dijets
  float mJetEtaLow; ///low range of jet eta to use for finding dijets
  float mDPhi; ///deltaPhi cut for dijets
  float mDEta; ///deltaEta cut for dijets
  float mPtLow; ///low pT cut for dijets (asymmetric pt cut)
  float mPtHigh; ///high pT cut for dijets (asymmetric pt cut)
  float mMinJetPt; ///minimum jet pT required for looking for dijets

  float mRecohadron; ///amount hadron energy is shifted in reco stage
  float mRecolepton; ///amount lepton energy is shifted in reco stage
  int *nEvents;
  double mVertex[3]; ///vertex location

  float dR(StGenParticle*,StGenParticle*) const; ///distance between particles in eta x phi space
  float dR(StGenParticle*,JetFourVec*) const; ///distance between particles in eta x phi space
  float dR(JetFourVec*,StGenParticle*) const; ///distance between particles in eta x phi space
  float dR(JetFourVec*,JetFourVec*) const; ///distance between particles in eta x phi space

  JetFourVec* combineTracks(std::vector<JetFourVec*>) const; ///function to combine a list of four vectors to a single four vector
  JetFourVec* recoJet(std::vector<JetFourVec*>, double*) const; ///shifts the energy of particles to account for reconstruction effects and forms a jet
  std::vector<JetFourVec*> EtOrderedList(std::vector<JetFourVec*>) const; ///returns and EtOrdered list of JetFourVecs
  std::vector< std::vector<JetFourVec*> > EtOrderedList(std::vector< std::vector<JetFourVec*> >) const; ///returns an EtOrderList of a list of four vector lists
  std::vector< std::vector<JetFourVec*> > RemoveDuplicates(std::vector< std::vector<JetFourVec*> >) const; ///removes duplicate jets from a list

  float overlapEnergy(std::vector<JetFourVec*>,std::vector<JetFourVec*>) const;
  std::vector< std::vector<JetFourVec*> > doSplitMerge(std::vector< std::vector<JetFourVec*> >) const; ///calculate the overlap energy of two jets

  std::vector<JetFourVec*> merge(std::vector<JetFourVec*>,std::vector<JetFourVec*>) const; ///merge two jets
  void split(std::vector<JetFourVec*> &v1,std::vector<JetFourVec*> &v2) const; ///split two jets

  std::vector<JetFourVec*> addMidPoints(std::vector<JetFourVec*>) const; ///add midpoint seeds

  void readConfig(); ///read a config file to adjust parameters

 public:
  StDijetFilter(); ///constructor
  virtual ~StDijetFilter();///destructor

  int RejectEG(const StGenParticleMaster &ptl) const;

  int RejectGT(const StGenParticleMaster &ptl) const;

  int RejectGE(const StGenParticleMaster &ptl) const;

  void parseConfig(std::string, float); ///change parameters
};

#endif
