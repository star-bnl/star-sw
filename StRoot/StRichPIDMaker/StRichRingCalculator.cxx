/**********************************************************
 * $Id: StRichRingCalculator.cxx,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.cxx,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision


  mInnerRing = new StRichRingPoint(track,eInnerRing);
 
  mInnerMinimization = new StRichMinimization(mInnerRing);
  mMeanMinimization  = new StRichMinimization(mMeanRing);
  setParticleType(particle);
  mRichMaterialsDb = StRichMaterialsDb::getDb();




  mMonteCarloArea.Set(18);
  delete mInnerRing;
  delete mOuterRing;
  delete mMeanRing;
double StRichRingCalculator::calculateArea(double cut) {

  StRichArea areaCalc(mInnerRing,mOuterRing);
    mInnerMinimization = 0;
  double area = areaCalc.calculateArea(cut);
  
  }

  // clear vector of points
  vectorOfPtsToDraw     = areaCalc.getPtsToDraw();
  mTotalAngleOnPadPlane = areaCalc.getTotalAngleOnPadPlane();
double StRichRingCalculator::getTotalArea() {
  return mTotalArea;
    vectorOfPtsToDraw.clear();
    vectorOfPtsToDraw.resize(0);
    vectorOfPtsToDraw     = areaCalc.getPtsToDraw();
double StRichRingCalculator::getTotalAngle() {
  return mTotalAngleOnPadPlane;
}
  */

double StRichRingCalculator::getPadPlaneArea() {
  return mPadPlaneArea;
}
  return area;
  return mMonteCarloPoints;
}
void StRichRingCalculator::setMonteCarloSwitch(bool set) {




StRichRingPoint* StRichRingCalculator::getRing(StRichRingDefinition ringType) {
  if (ringType==eInnerRing)  return  mInnerRing;
  return  (closestInnerRingPoint - closestOuterRingPoint).mag();
  if (ringType==eMeanRing)   return  mMeanRing;
  return 0;

void StRichRingCalculator::setParticleType(StParticleDefinition* particle) {
   mInnerRing->setParticleType(particle);
   mOuterRing->setParticleType(particle);
   mMeanRing->setParticleType(particle);
}
double StRichRingCalculator::getRingWidth() const {
void StRichRingCalculator::setParticleType(StParticleDefinition* particle) { 
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

    return (closestInnerRingPoint - testPoint).perp();
double StRichRingCalculator::getOuterDistance(StThreeVectorF& testPoint,double& outerAngle) {
double StRichRingCalculator::getMeanDistance(StThreeVector<double>& testPoint,
					     double& meanAngle) {

  outerAngle            = mOuterMinimization->getPsi();
  return (closestOuterRingPoint - testPoint).perp();
  return (closestMeanRingPoint - testPoint).mag();
double StRichRingCalculator::getMeanDistance(StThreeVectorF& testPoint,double& meanAngle) {
  closestMeanRingPoint = mMeanMinimization->rotatedMin(testPoint);
  mMeanPathInQuartz   = mMeanMinimization->getMeanPathInQuartz();
  closestInnerRingPoint.setX(0);
  closestInnerRingPoint.setY(0);
  closestInnerRingPoint.setZ(0);

  closestOuterRingPoint.setX(0);
  closestOuterRingPoint.setY(0);
  closestOuterRingPoint.setZ(0);

  closestMeanRingPoint.setX(0);
  closestMeanRingPoint.setY(0);
  closestMeanRingPoint.setZ(0);
}

  closestInnerRingPoint = temp;
StThreeVector<double> StRichRingCalculator::getOuterRingPoint() {
  return closestOuterRingPoint;
}

StThreeVectorF StRichRingCalculator::getOuterRingPoint() { return closestOuterRingPoint;}
StThreeVector<double> StRichRingCalculator::getInnerRingPoint() {
  return closestInnerRingPoint;
}


StThreeVector<double> StRichRingCalculator::getMeanRingPoint() {
  return closestMeanRingPoint;
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

















