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
#include <string>

void StiMaterialInteraction::nameForIntersection(
    StiIntersection &intersection, string &name){
  switch(intersection){
    case kFailed: name = "failed"; break;
    case kHit: name = "hit"; break;
    case kEdgePhiPlus: name = "edgePhiPlus"; break;
    case kEdgeZminus: name = "edgeZminus"; break;
    case kEdgePhiMinus: name = "edgePhiMinus"; break;
    case kEdgeZplus: name = "edgeZplus"; break;
    case kMissPhiPlus: name = "missPhiPlus"; break;
    case kMissZminus: name = "missZminus"; break;
    case kMissPhiMinus: name = "missPhiMinus"; break;
    case kMissZplus: name = "missZplus"; break;
  }
} // nameForIntersection

// For now, these methods just test intersection with the central plane
// of the detector using a straight line projection of the track node.
StiIntersection StiMaterialInteraction::findIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
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
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){

  StiPlacement *pPlacement = pDetector->getPlacement();
  StiPlanarShape *pShape = dynamic_cast<StiPlanarShape *>(
      pDetector->getShape());
  const StiDetector *pNodeDetector = pNode->getDetector();

  //---------------------------------------------------
  // first, do the intersection of the node's momentum 
  // vector with the detector plane in global coords.
  /*
double x1=fX, x2=x1+(xk-x1), dx=x2-x1, y1=fP0, z1=fP1;
  double c1=fP3*x1 - fP2, r1=sqrt(1.- c1*c1);
  double c2=fP3*x2 - fP2, r2=sqrt(1.- c2*c2);
  fP0 = fP0 + dx*(c1+c2)/(r1+r2);
  fP1 = fP1 + dx*(c1+c2)/(c1*r2 + c2*r1)*fP4; 
	*/
  // get momentum of node in global coords
  double adMomentum[3];
  pNode->getMomentum(adMomentum);
  StThreeVectorD momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pNode->fAlpha);

  // get normals to plane in global coords
  StThreeVectorD normal(1., 0., 0.);
  normal.rotateZ(pPlacement->getNormalRefAngle());

  // get vector to node in global coords
  StThreeVectorD node(pNode->fX, pNode->fP0, pNode->fP1);
  node.rotateZ(pNode->fAlpha);

  // find the coefficent for momentum vector which extends it to
  // the intersection point, then the intersection.
  double momentumCoefficient = 
      (pPlacement->getNormalRadius() - node.dot(normal))/
      momentum.dot(normal);
  // If we were only interested in whether or not (and where) there was
  // an intersection with the given detector, we would require that the
  // momentum coefficient be negative because we're tracking outside-in.
  // Since we're intersted in the relative position of any misses, we don't
  // make the cut:
  //
  // if(momentumCoefficient>0.){ return kFailed; }
  StThreeVectorD nodeToIntersection = momentumCoefficient*momentum;
  StThreeVectorD intersection = node + nodeToIntersection;

  //--------------------------------
  // determine thickness and density
  // of the region travsersed

  // just use the cosine between the momentum and the detector normal to
  // calculate thickness traversed
  double dPathLengthDetector = findThickness(pDetector, &intersection, 
                                             &momentum);
  double dPathLengthNodeDetector = findThickness(pNodeDetector, &node, 
                                                 &momentum);

  // subtract off the half-thicknesses traversed of in both detectors
  // to get the gap thickness traversed
  double dPathLengthGap = nodeToIntersection.mag() - dPathLengthDetector/2. -
      dPathLengthNodeDetector/2.;

  double dPathLength = dPathLengthDetector + dPathLengthGap;

  // get the weighted density average
  StiMaterial *pGas = pDetector->getGas();
  StiMaterial *pMaterial = pDetector->getMaterial();
  dDensity = (pGas->getDensity()*dPathLengthGap +
              pMaterial->getDensity()*dPathLengthDetector)/dPathLength;

  dThickness = (dPathLengthGap/pGas->getRadLength() +
                dPathLengthDetector/pMaterial->getRadLength());

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

	/*  cout << "y,z:" << dYoffset << "\t" << dZoffset << endl
			 << "dInnerY:" << dInnerY
			 << " dOuterY:" << dOuterY
			 << " dInnerZ:" << dInnerZ
			 << " dOuterZ:" << dOuterZ << endl;
	*/
  // direct hit
  if(fabs(dYoffset)<dInnerY && fabs(dZoffset)<dInnerZ){ return kHit; }

  // outside detector to positive or negative y
  if(dYoffset>dOuterY && fabs(dYoffset)>fabs(dZoffset)){
    return kMissPhiPlus; 
  }
  if(dYoffset<-dOuterY && fabs(dYoffset)>fabs(dZoffset)){
    return kMissPhiMinus;
  }

  // outside detector to positive or negative z (west or east)
  if(dZoffset>dOuterZ){  return kMissZplus; }
  if(dZoffset<-dOuterZ){ return kMissZminus; }

  // on positive or negative y edge
  if(dYoffset>0 && dYoffset>fabs(dZoffset)){  return kEdgePhiPlus; }
  if(dYoffset<0 && dYoffset<-fabs(dZoffset)){ return kEdgePhiMinus; }

  // on positive or negative z edge
  if(dZoffset>0){ return kEdgeZplus; }
  if(dZoffset<0){ return kEdgeZminus; }

  return kFailed;

} // findPlanarIntersection

StiIntersection StiMaterialInteraction::findCylindricalIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){

  StiPlacement *pPlacement = pDetector->getPlacement();
  StiCylindricalShape *pShape = dynamic_cast<StiCylindricalShape *>(
      pDetector->getShape());
  const StiDetector *pNodeDetector = pNode->getDetector();

  //---------------------------------------------------
  // first, do the intersection of the node's momentum 
  // vector with the detector cylinder in global coords.

  // get momentum of node in global coords
  double adMomentum[3];
  pNode->getMomentum(adMomentum);
  StThreeVectorD momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pNode->fAlpha);
  
  // get vector to node in global coords
  StThreeVectorD node(pNode->fX, pNode->fP0, pNode->fP1);
  node.rotateZ(pNode->fAlpha);

  // get "transverse" vectors used in calculating intersection
  StThreeVectorD zHat(0., 0., 1.);
  StThreeVectorD momentumPerp = momentum.cross(zHat);
  StThreeVectorD nodePerp = node.cross(zHat);

  // first, test for any intersection at all
  double radius = pPlacement->getNormalRadius();
  double determinant = radius*radius*momentumPerp.mag2() -
      node.dot(momentumPerp)*node.dot(momentumPerp);
  if(determinant<0.){ return kFailed; }

  // find the coefficent for momentum vector which extends it to
  // the intersection point, then the intersection.
  double momentumCoefficient = 
      (-nodePerp.dot(momentumPerp) + sqrt(determinant))/momentumPerp.mag2();
  double momentumCoefficientAlternate = 
      (-nodePerp.dot(momentumPerp) - sqrt(determinant))/momentumPerp.mag2();
  // for now, take smaller root...this may not always be right
  if(fabs(momentumCoefficient)>fabs(momentumCoefficientAlternate)){
    momentumCoefficient = momentumCoefficientAlternate; 
  }

  // physically, we only want a negative coefficient.  We are tracking
  // outside-in, which means we should be tracking in the negative
  // momentum direction.
  //if(momentumCoefficient>0.){ return kFailed; }
  StThreeVectorD nodeToIntersection = momentumCoefficient*momentum;
  StThreeVectorD intersection = node + nodeToIntersection;

  //--------------------------------
  // determine thickness and density
  // of the region travsersed

  // just use the cosine between the momentum and the detector normal to
  // calculate thickness traversed
  double dPathLengthDetector = findThickness(pDetector, &intersection, 
                                             &momentum);
  double dPathLengthNodeDetector = findThickness(pNodeDetector, &node, 
                                                 &momentum);

  // subtract off the half-thicknesses traversed of in both detectors
  // to get the gap thickness traversed
  double dPathLengthGap = nodeToIntersection.mag() - dPathLengthDetector/2. -
      dPathLengthNodeDetector/2.;

  double dPathLength = dPathLengthDetector + dPathLengthGap;

  // get the weighted density average
  StiMaterial *pGas = pDetector->getGas();
  StiMaterial *pMaterial = pDetector->getMaterial();
  dDensity = (pGas->getDensity()*dPathLengthGap +
              pMaterial->getDensity()*dPathLengthDetector)/dPathLength;

  dThickness = (dPathLengthGap/dPathLength/pGas->getRadLength() +
                dPathLengthDetector/dPathLength/pMaterial->getRadLength());

  //--------------------------------
  // rotate to local and determine 
  // if & where it hit the detector

  intersection.rotateZ(-pPlacement->getNormalRefAngle());
  dXlocal = intersection.x();

  // get offsets of intersection from center
  double dPhiOffset = intersection.phi();
  double dZoffset = intersection.z() - pPlacement->getZcenter();

  // find limits of various regions
  double dInnerPhi = pShape->getOpeningAngle()/2. - EDGE_HALF_WIDTH/radius;
  double dOuterPhi = dInnerPhi + 2.*EDGE_HALF_WIDTH/radius;
  double dInnerZ = pShape->getHalfDepth() - EDGE_HALF_WIDTH;
  double dOuterZ = dInnerZ + 2.*EDGE_HALF_WIDTH;
  
  // direct hit
  if(fabs(dPhiOffset)<dInnerPhi && fabs(dZoffset)<dInnerZ){ return kHit; }

  // outside detector to positive or negative y
  if(dPhiOffset>dOuterPhi && fabs(dPhiOffset)>fabs(dZoffset)){  
    return kMissPhiPlus; 
  }
  if(dPhiOffset<-dOuterPhi && fabs(dPhiOffset)>fabs(dZoffset)){ 
    return kMissPhiMinus; 
  }

  // outside detector to positive or negative z (west or east)
  if(dZoffset>dOuterZ){  return kMissZplus; }
  if(dZoffset<-dOuterZ){ return kMissZminus; }

  // on positive or negative y edge
  if(dPhiOffset>0 && dPhiOffset>fabs(dZoffset)){  return kEdgePhiPlus; }
  if(dPhiOffset<0 && dPhiOffset<-fabs(dZoffset)){ return kEdgePhiMinus; }

  // on positive or negative z edge
  if(dZoffset>0){ return kEdgeZplus; }
  if(dZoffset<0){ return kEdgeZminus; }

  return kFailed;
} // findCylindricalIntersection

StiIntersection StiMaterialInteraction::findConicalIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity){
  return kFailed;
} // findConicalIntersection

// returns thickness*(direction dot normal at point).
// all coordinates are assumed to be global.  pDirection does not have to
// be normalized.
// normalized.
// returns -1 on failure.
double StiMaterialInteraction::findThickness(const StiDetector *pDetector,
                                             const StThreeVectorD *pPoint,
                                             const StThreeVectorD *pDirection){

  double dAlpha = pDetector->getPlacement()->getNormalRefAngle();
  double dThickness = pDetector->getShape()->getThickness();

  StThreeVectorD normal(1., 0., 0.);
  normal.rotateZ(dAlpha);
  StThreeVectorD point = *pPoint;

  switch(pDetector->getShape()->getShapeCode()){
    case kCylindrical:
        // get local azimuthal angle of point
        point.rotateZ(-dAlpha);
        // rotate normal by this angle
        normal.rotateZ(point.phi());
    case kPlanar:
        dThickness *= normal.dot(*pDirection)/pDirection->mag();
        break;
    case kConical:
    default:
        dThickness = -1.;
        break;
  }

  return dThickness;
}



