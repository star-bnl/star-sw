#include "StMuTofHit.h"
//#include "StTrack.h"
//#include "StParticleDefinition.hh"

ClassImp(StMuTofHit)

StMuTofHit::StMuTofHit()
{
  mIconf = 0;
  mTrayIndex = 0;
  mModuleIndex = 0;
  mCellIndex = 0;
  mDaqIndex = 255; 
  mADC = 0;
  mTimeOfFlight = 0;
  mPathLength = 0;
  mBeta = 0;
  mTOFExpectedAsElectron = 0;
  mTOFExpectedAsPion = 0; 
  mTOFExpectedAsKaon = 0;
  mTOFExpectedAsProton = 0; 
  mSigmaElectron = 999.; 
  mSigmaPion = 999.; 
  mSigmaKaon = 999.; 
  mSigmaProton = 999.;

  StThreeVectorF mProjectedPoint(0.,0.,0.);  
  mAssociatedTrackId = 0;
  mParticleHypothesis = 0;
}

StMuTofHit::~StMuTofHit()
{}

int StMuTofHit::Iconf() const { return mIconf; }

int StMuTofHit::trayIndex() const { return mTrayIndex; }

int StMuTofHit::moduleIndex() const { return mModuleIndex; }

int StMuTofHit::cellIndex() const { return mCellIndex; }

int StMuTofHit::daqIndex() const { return mDaqIndex; }

int StMuTofHit::adc() const { return mADC; }

float StMuTofHit::timeOfFlight() const { return mTimeOfFlight; }

float StMuTofHit::pathLength() const { return mPathLength; }

float StMuTofHit::beta() const { return mBeta; }

//StTrack* StMuTofHit::associatedTrack() { return mAssociatedTrack; }

//const StTrack* StMuTofHit::associatedTrack() const { return mAssociatedTrack; }

int StMuTofHit::associatedTrackId() const { return mAssociatedTrackId; }

StThreeVectorF StMuTofHit::projectedPoint() const { return mProjectedPoint; }

float StMuTofHit::tofExpectedAsElectron() const { return mTOFExpectedAsElectron; }

float StMuTofHit::tofExpectedAsPion() const { return mTOFExpectedAsPion; }

float StMuTofHit::tofExpectedAsKaon() const { return mTOFExpectedAsKaon; }

float StMuTofHit::tofExpectedAsProton() const { return mTOFExpectedAsProton; }

float StMuTofHit::sigmaElectron() const { return mSigmaElectron; }

float StMuTofHit::sigmaPion() const { return mSigmaPion; }

float StMuTofHit::sigmaKaon() const { return mSigmaKaon; }

float StMuTofHit::sigmaProton() const { return mSigmaProton; }

//StParticleDefinition* StMuTofHit::particleHypothesis() { return mParticleHypothesis; }

//const StParticleDefinition* StMuTofHit::particleHypothesis() const { return mParticleHypothesis; }

int StMuTofHit::particleHypothesis() const { return mParticleHypothesis; }


void StMuTofHit::setIconf(int iconf) { mIconf = iconf; }

void StMuTofHit::setTrayIndex(int trayId) { mTrayIndex = trayId; }

void StMuTofHit::setModuleIndex(int moduleId) { mModuleIndex = moduleId; }

void StMuTofHit::setCellIndex(int cellId) { mCellIndex = cellId; }

void StMuTofHit::setDaqIndex(int daqId) { mDaqIndex = daqId; }

void StMuTofHit::setADC(int adc) { mADC = adc; }

void StMuTofHit::setTimeOfFlight(float tof) { mTimeOfFlight = tof; }

void StMuTofHit::setPathLength(float length) { mPathLength = length; }

void StMuTofHit::setBeta(float b) { mBeta = b; }

// void StMuTofHit::setAssociatedTrack(StTrack* val)
// {
//   if (mAssociatedTrack) delete mAssociatedTrack;
//   mAssociatedTrack = val;
// }

void StMuTofHit::setAssociatedTrackId(int trkId) { mAssociatedTrackId = trkId; }

void StMuTofHit::setProjectedPoint(const StThreeVectorF& val) { mProjectedPoint = val; }

void StMuTofHit::settofExpectedAsElectron(float tofexp) { mTOFExpectedAsElectron = tofexp; }

void StMuTofHit::settofExpectedAsPion(float tofexp) { mTOFExpectedAsPion = tofexp; }

void StMuTofHit::settofExpectedAsKaon(float tofexp) { mTOFExpectedAsKaon = tofexp; }

void StMuTofHit::settofExpectedAsProton(float tofexp) { mTOFExpectedAsProton = tofexp; }

void StMuTofHit::setsigmaElectron(float sigma) { mSigmaElectron = sigma; }

void StMuTofHit::setsigmaPion(float sigma) { mSigmaPion = sigma; }

void StMuTofHit::setsigmaKaon(float sigma) { mSigmaKaon = sigma; }

void StMuTofHit::setsigmaProton(float sigma) { mSigmaProton = sigma; }

// void StMuTofHit::setparticleHypothesis(StParticleDefinition* val)
// {
//   if (mParticleHypothesis) delete mParticleHypothesis;
//   mParticleHypothesis = val;
// }

void StMuTofHit::setparticleHypothesis(int val) { mParticleHypothesis = val; }

