#include "StiMaterial.h"
#include "StiDetector.h" 
#include "StiShape.h" 
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiPlacement.h" 
#include "StiKalmanTrackNode.h"
#include "StiHit.h"
#include "StiMaterialInteraction.h"
#include "StiGeometryTransform.h" 

#include "StHelix.hh"
#include "StThreeVector.hh"

#include <float.h>

// For now, these methods just test intersection with the central plane
// of the detector using a straight line projection of the track node.
StiIntersection StiMaterialInteraction::findIntersection(
    StiKalmanTrackNode *pNode, StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){

  switch(pDetector->getShape()->getShapeCode()){
    case kPlanar:  
        return findPlanarIntersection(pNode, pDetector, 
                                      dXlocal, dThickness, dDensity);
    case kCylindrical:
        return findCylindricalIntersection(pNode, pDetector, 
                                           dXlocal, dThickness, dDensity);
    case kConical:
        return findConicalIntersection(pNode, pDetector, 
                                       dXlocal, dThickness, dDensity);
    default:
        return kFailed;
  }

} // findIntersection

StiIntersection StiMaterialInteraction::findPlanarIntersection(
    StiKalmanTrackNode *pNode, StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){

  StiPlacement *pPlacement = pDetector->getPlacement();
  StiPlanarShape *pShape = dynamic_cast<StiPlanarShape *>(
      pDetector->getShape());
  StiDetector *pDetectorNode = pNode->getDetector();

  //---------------------------------------------------
  // first, do the intersection of the node's momentum 
  // vector with the detector plane in global coords.

  // get momentum of node in global coords
  double adMomentum[3];
  pNode->getMomentum(adMomentum);
  StThreeVectorD momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pNode->fAlpha);
  
  // get normals to plane in global coords
  StThreeVectorD normal(1., 0., 0.);
  normal.rotateZ(pPlacement->getNormalRefAngle());
  StThreeVectorD nodeNormal(1., 0., 0.);
  nodeNormal.rotateZ(pDetectorNode->getPlacement()->getNormalRefAngle());

  // get vector to detector center in global coords
  double dDetectorX = pPlacement->getCenterRadius() *
      cos(pPlacement->getCenterRefAngle());
  double dDetectorY = pPlacement->getCenterRadius() *
      sin(pPlacement->getCenterRefAngle());
  StThreeVectorD center(dDetectorX, dDetectorY, pPlacement->getZcenter());

  // get vector to node in global coords
  StThreeVectorD node(pNode->fX, pNode->fP0, pNode->fP1);
  node.rotateZ(pNode->fAlpha);

  // find intersection in global coords
  StThreeVectorD nodeToIntersection = momentum*((center - node).dot(normal)/
                                                momentum.dot(normal));
  StThreeVectorD intersection = node + nodeToIntersection;

  //--------------------------------
  // determine thickness and density
  // of the region travsersed

  // just use the cosine between the momentum and the detector normal to
  // calculate thickness traversed
  double dPathLengthDetector = pShape->getThickness()*
      fabs(momentum.dot(normal)/momentum.mag());
  double dPathLengthNodeDetector = pDetectorNode->getShape()->getThickness()*
      fabs(momentum.dot(nodeNormal)/momentum.mag());

  // subtract off the half-thicknesses traversed of in both detectors
  // to get the gap thickness traversed
  double dPathLengthGap = nodeToIntersection.mag() - dPathLengthDetector/2. -
      dPathLengthNodeDetector/2.;

  double dPathLength = dPathLengthDetector + dPathLengthGap;

  cout << "pathLengthDetector=" << dPathLengthDetector
       << ", pathLengthNodeDetector=" << dPathLengthNodeDetector
       << ", pathLengthGap-=" << dPathLengthGap
       << ", pathLength=" << dPathLength << endl;

  // get the weighted density average
  StiMaterial *pGas = pDetector->getGas();
  StiMaterial *pMaterial = pDetector->getMaterial();
  dDensity = (pGas->getDensity()*dPathLengthGap +
              pMaterial->getDensity()*dPathLengthDetector)/
      dPathLength;

  dThickness = (dPathLengthGap/dPathLength/pGas->getRadLength() +
                dPathLengthDetector/dPathLength/pMaterial->getRadLength());

  //--------------------------------
  // rotate to local and determine 
  // if & where it hit the detector

  intersection.rotateZ(-pPlacement->getNormalRefAngle());
  dXlocal = intersection.x();

  // get offsets of intersection from center
  double dYoffset = intersection.y() - pPlacement->getNormalYoffset();
  double dZoffset = intersection.z() - pPlacement->getZcenter();

  // find limits of various regions
  double dInnerY = pShape->getHalfWidth() - EDGE_HALF_WIDTH;
  double dOuterY = dInnerY + 2.*EDGE_HALF_WIDTH;
  double dInnerZ = pShape->getHalfDepth() - EDGE_HALF_WIDTH;
  double dOuterZ = dInnerZ + 2.*EDGE_HALF_WIDTH;
  
  // direct hit
  if(fabs(dYoffset)<dInnerY && fabs(dZoffset)<dInnerZ){ return kCenter; }

  // outside detector to positive or negative y
  if(dYoffset>dOuterY && fabs(dYoffset)>fabs(dZoffset)){  return kNorthOut; }
  if(dYoffset<-dOuterY && fabs(dYoffset)>fabs(dZoffset)){ return kSouthOut; }

  // outside detector to positive or negative z (west or east)
  if(dZoffset>dOuterZ){  return kWestOut; }
  if(dZoffset<-dOuterZ){ return kEastOut; }

  // on positive or negative y edge
  if(dYoffset>0 && dYoffset>fabs(dZoffset)){  return kNorthEdge; }
  if(dYoffset<0 && dYoffset<-fabs(dZoffset)){ return kSouthEdge; }

  // on positive or negative z edge
  if(dZoffset>0){ return kWestEdge; }
  if(dZoffset<0){ return kEastEdge; }

  return kFailed;

} // findPlanarIntersection

StiIntersection StiMaterialInteraction::findCylindricalIntersection(
    StiKalmanTrackNode *pNode, StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){
  return kFailed;
} // findCylindricalIntersection

StiIntersection StiMaterialInteraction::findConicalIntersection(
    StiKalmanTrackNode *pNode, StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){
  return kFailed;
} // findConicalIntersection



