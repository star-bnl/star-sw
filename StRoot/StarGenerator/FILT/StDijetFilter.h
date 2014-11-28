/*! \class StDijetFilter

JFN 4/24/13-
  This is almost an exact copy of the old StDijetFilter. Most of it can stay the way it is.

  The important bits are:
    *change it so that it inherits from StarFilterMaker, not StMCFilter
    *replace RejectGE() with Filter()
    *fix the filter function so that it takes a StarGenEvent rather than a StarGenParticleMaster
*/

#ifndef STAR_StDijetFilter
#define STAR_StDijetFilter

#include <vector>
#include <string>
#include "StarGenerator/FILT/StarFilterMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

class StarGenParticleMaster;
class StarGenParticle;
class StarGenEvent;

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
  JetFourVec(StarGenParticle*); ///construct from StarGenParticle
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

class StDijetFilter : public StarFilterMaker
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

  float dR(StarGenParticle*,StarGenParticle*) const; ///distance between particles in eta x phi space
  float dR(StarGenParticle*,JetFourVec*) const; ///distance between particles in eta x phi space
  float dR(JetFourVec*,StarGenParticle*) const; ///distance between particles in eta x phi space
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

  Int_t Filter( StarGenEvent *mEvent );

  void parseConfig(std::string, float); ///change parameters

  ClassDef(StDijetFilter,1);
  
};

#endif
