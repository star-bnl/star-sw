/**********************************************************
 * $Id: StRichRingCalculator.cxx,v 1.1 2000/04/03 19:36:08 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.cxx,v $
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *

 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  initial revision

  mInnerRing = new StRichRingPoint(track,eInnerRing);
 
  mInnerMinimization = new StRichMinimization(mInnerRing);
  mMeanMinimization  = new StRichMinimization(mMeanRing);
  setParticleType(particle);
  if (mInnerRing) delete mInnerRing;
  if (mOuterRing) delete mOuterRing;

  if (mInnerMinimization) delete mInnerMinimization;
  if (mOuterMinimization) delete mOuterMinimization;
  delete mOuterRing;
  delete mMeanRing;
void StRichRingCalculator::calculateArea(double cut) {
  StRichArea fastArea(mInnerRing,mOuterRing);
  fastArea.calculateArea(cut);
  mTotalArea    = fastArea.getTotalArea();
  mPadPlaneArea = fastArea.getPadPlaneArea();

  // clear vector of points
double StRichRingCalculator::getTotalArea() {
  return mTotalArea;
    vectorOfPtsToDraw.clear();
    vectorOfPtsToDraw.resize(0);
double StRichRingCalculator::getPadPlaneArea() {
  return mPadPlaneArea;
}
  return area;
  return mMonteCarloPoints;
}
void StRichRingCalculator::setMonteCarloSwitch(bool set) {

  return NULL;

StRichRingPoint* StRichRingCalculator::getRing(StRichRingDefinition ringType) {
  if (ringType==eInnerRing)  return  mInnerRing;
  return  (closestInnerRingPoint - closestOuterRingPoint).mag();
  if (ringType==eMeanRing)   return  mMeanRing;
  return 0;
void StRichRingCalculator::setParticleType(StParticleDefinition* particle) {
   mInnerRing->setParticleType(particle);
   mOuterRing->setParticleType(particle);
}
double StRichRingCalculator::getRingWidth() const {
double StRichRingCalculator::getInnerDistance(StThreeVector<double>& testPoint,
					                     double& innerAngle) {
  mOuterRing->setParticleType(particle);
  mMeanRing->setParticleType(particle);
  return (closestInnerRingPoint - testPoint).mag();

    mMeanRing->setParticleType(particle);
double StRichRingCalculator::getOuterDistance(StThreeVector<double>& testPoint,
					                     double& outerAngle) {

  innerAngle            = mInnerMinimization->getPsi();
  return (closestInnerRingPoint - testPoint).perp();
  return (closestOuterRingPoint - testPoint).mag();
}
StThreeVectorF StRichRingCalculator::getMeanRingPoint()  { return closestMeanRingPoint;}

double StRichRingCalculator::getConstantAreaAngle() {return mConstantAreaAngle;}
double StRichRingCalculator::getTotalArea() { return mTotalArea;}
double StRichRingCalculator::getTotalAngle() { return mTotalAngleOnPadPlane;}
double StRichRingCalculator::getPadPlaneArea() { return mPadPlaneArea;}


vector<StThreeVectorF >& StRichRingCalculator::getPtsToDraw() { return vectorOfPtsToDraw;}
  }

  return normalArea;
}

















