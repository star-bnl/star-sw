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
#include "Messenger.h"

#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h" 

#include <float.h>
#include <string>

// can't initialize this to the actual messenger until after
// Messenger::init() is called in StiMaker, certainly not at dll load time.
Messenger *StiMaterialInteraction::s_pMessenger = NULL;
StiExtrapolationType StiMaterialInteraction::s_extrapolationType = kLinearExtrapolation;
//StiExtrapolationType StiMaterialInteraction::s_extrapolationType = kHelixExtrapolation;

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
    double &dXlocal, double &dThickness, double &dDensity, double &dDistance){
  // make sure we have our output device initialized
  if(s_pMessenger==NULL){
    s_pMessenger = Messenger::instance(MessageType::kGeometryMessage);
    MessageType::getTypeByCode(MessageType::kGeometryMessage)->setOstream(
        new ofstream("GeometryMessageFile"));
    //Messenger::setRoutingBits(MessageType::kGeometryMessage);
  }

  switch(pDetector->getShape()->getShapeCode()){
  case kPlanar:  
    return findPlanarIntersection(pNode, pDetector, 
				  dXlocal, dThickness, dDensity, dDistance);
    case kCylindrical:
      return findCylindricalIntersection(pNode, pDetector, 
					 dXlocal, dThickness, dDensity,
                                         dDistance);
  case kConical:
    return findConicalIntersection(pNode, pDetector, 
				   dXlocal, dThickness, dDensity, dDistance);
  default:
    return kFailed;
  }
	

} // findIntersection

StiIntersection StiMaterialInteraction::findPlanarIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity, double &dDistance){

  // first extrapolate to find the intersection point and
  // thicknesses traversed.
  double dNodeHalfThickness, dDetectorHalfThickness, dGapThickness;
  StThreeVector<double> intersection;
  bool bExtrapolated = false;
  switch(s_extrapolationType){
    case kHelixExtrapolation:
//    case kLinearExtrapolation:
//    default:
        bExtrapolated = extrapolateHelixToPlane(
            pNode, pDetector, &intersection,
            dNodeHalfThickness, dGapThickness, dDetectorHalfThickness);
        break;
    case kLinearExtrapolation:
    default:
        bExtrapolated = extrapolateLineToPlane(
            pNode, pDetector, &intersection,
            dNodeHalfThickness, dGapThickness, dDetectorHalfThickness);
        break;
  }
  if(bExtrapolated==false){ return kFailed; }
  dDistance = 2.*dDetectorHalfThickness + dGapThickness;

  //-------------------------------------------
  // get the weighted densities & thicknesses
  StiMaterial *pGas = pDetector->getGas();
  StiMaterial *pMaterial = pDetector->getMaterial();
  dDensity = (pGas->getDensity()*dGapThickness +
              pMaterial->getDensity()*2.*dDetectorHalfThickness)/dDistance;

  dThickness = (dGapThickness/pGas->getRadLength() +
                2.*dDetectorHalfThickness/pMaterial->getRadLength());

  //--------------------------------
  // rotate to local and determine 
  // if & where it hit the detector
  StiPlacement *pPlacement = pDetector->getPlacement();
  intersection.rotateZ(-pPlacement->getNormalRefAngle());
  dXlocal = intersection.x();

  // get offsets of intersection from detector center
  double dYoffset = intersection.y() - pPlacement->getNormalYoffset();
  double dZoffset = intersection.z() - pPlacement->getZcenter();

  // find limits of various regions
  StiPlanarShape *pShape = dynamic_cast<StiPlanarShape *>(
      pDetector->getShape());
  double dEdgeHalfWidth = (strstr(pDetector->getName().c_str(), "Tpc")==NULL) ?
      SVG_EDGE_HALF_WIDTH : TPC_EDGE_HALF_WIDTH;
  double dInnerY = pShape->getHalfWidth() - dEdgeHalfWidth;
  double dOuterY = dInnerY + 2.*dEdgeHalfWidth;
  double dInnerZ = pShape->getHalfDepth() - dEdgeHalfWidth;
  double dOuterZ = dInnerZ + 2.*dEdgeHalfWidth;

  *s_pMessenger << pDetector->getName() << ":" << endl
                << "  innerY:" << dInnerY
                << " outerY:" << dOuterY
                << " innerZ:" << dInnerZ
                << " outerZ:" << dOuterZ << endl
                << "  yOffset:" << dYoffset 
                << " zOffset:" << dZoffset << endl;

  StiIntersection iIntersection = kFailed;

  // direct hit
  if(fabs(dYoffset)<dInnerY && fabs(dZoffset)<dInnerZ){ 
    iIntersection = kHit; 
  }else if(fabs(dYoffset)>dOuterY && 
           (fabs(dYoffset) - dOuterY)>(fabs(dZoffset) - dOuterZ)){
    // outside detector to positive or negative y (phi)
    iIntersection = dYoffset>0 ? kMissPhiPlus : kMissPhiMinus;
  }else if(fabs(dZoffset)>dOuterZ && 
           (fabs(dZoffset) - dOuterZ)>(fabs(dYoffset) - dOuterY)){
    // outside detector to positive or negative z (west or east)
    iIntersection = dZoffset>0 ? kMissZplus : kMissZminus;
  }else if((fabs(dYoffset) - dInnerY)>(fabs(dZoffset) - dInnerZ)){
    // positive or negative phi edge
    iIntersection = dYoffset>0 ? kEdgePhiPlus : kEdgePhiMinus;
  }else{
    // positive or negative z edge
    iIntersection = dZoffset>0 ? kEdgeZplus : kEdgeZminus;
  }

  string name; nameForIntersection(iIntersection, name);
  *s_pMessenger << "  intersection (" << dXlocal << ", " << dYoffset
                << ", " << dZoffset << ") = " << name << endl;

  return iIntersection;

} // findPlanarIntersection

StiIntersection StiMaterialInteraction::findCylindricalIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity, double &dDistance){

  // first extrapolate to find the intersection point and
  // thicknesses traversed.
  double dNodeHalfThickness, dDetectorHalfThickness, dGapThickness;
  StThreeVector<double> intersection;
  bool bExtrapolated = false;
  switch(s_extrapolationType){
    case kHelixExtrapolation:
        bExtrapolated = extrapolateHelixToCylinder(
            pNode, pDetector, &intersection,
            dNodeHalfThickness, dGapThickness, dDetectorHalfThickness);
        break;
    case kLinearExtrapolation:
    default:
        bExtrapolated = extrapolateLineToCylinder(
            pNode, pDetector, &intersection,
            dNodeHalfThickness, dGapThickness, dDetectorHalfThickness);
        break;
  }
  if(bExtrapolated==false){ return kFailed; }
  dDistance = 2.*dDetectorHalfThickness + dGapThickness;

  // get the weighted density average
  StiMaterial *pGas = pDetector->getGas();
  StiMaterial *pMaterial = pDetector->getMaterial();
  dDensity = (pGas->getDensity()*dGapThickness +
              pMaterial->getDensity()*2.*dDetectorHalfThickness)/dDistance;

  dThickness = (dGapThickness/pGas->getRadLength() +
                2.*dDetectorHalfThickness/pMaterial->getRadLength());

  //--------------------------------
  // rotate to local and determine 
  // if & where it hit the detector
  StiPlacement *pPlacement = pDetector->getPlacement();
  intersection.rotateZ(-pPlacement->getNormalRefAngle());
  dXlocal = intersection.x();

  // get offsets of intersection from center
  double dPhiOffset = intersection.phi();
  double dZoffset = intersection.z() - pPlacement->getZcenter();

  // find limits of various regions
  StiCylindricalShape *pShape = dynamic_cast<StiCylindricalShape *>(
      pDetector->getShape());
  double radius = pPlacement->getNormalRadius();
  double dEdgeHalfWidth = (strstr(pDetector->getName().c_str(), "Tpc")==NULL) ?
      SVG_EDGE_HALF_WIDTH : TPC_EDGE_HALF_WIDTH;
  double dInnerPhi = pShape->getOpeningAngle()/2. - dEdgeHalfWidth/radius;
  double dOuterPhi = dInnerPhi + 2.*dEdgeHalfWidth/radius;
  double dInnerZ = pShape->getHalfDepth() - dEdgeHalfWidth;
  double dOuterZ = dInnerZ + 2.*dEdgeHalfWidth;
  
  StiIntersection iIntersection = kFailed;

  // direct hit
  if(fabs(dPhiOffset)<dInnerPhi && fabs(dZoffset)<dInnerZ){ 
    iIntersection = kHit; 
  }else if(fabs(dPhiOffset)>dOuterPhi && 
           (fabs(dPhiOffset) - dOuterPhi)>(fabs(dZoffset) - dOuterZ)){
    // outside detector to positive or negative y (phi)
    iIntersection = dPhiOffset>0 ? kMissPhiPlus : kMissPhiMinus;
  }else if(fabs(dZoffset)>dOuterZ && 
           (fabs(dZoffset) - dOuterZ)>(fabs(dPhiOffset) - dOuterPhi)){
    // outside detector to positive or negative z (west or east)
    iIntersection = dZoffset>0 ? kMissZplus : kMissZminus;
  }else if((fabs(dPhiOffset) - dInnerPhi)>(fabs(dZoffset) - dInnerZ)){
    // positive or negative phi edge
    iIntersection = dPhiOffset>0 ? kEdgePhiPlus : kEdgePhiMinus;
  }else{
    // positive or negative z edge
    iIntersection = dZoffset>0 ? kEdgeZplus : kEdgeZminus;
  }

  return iIntersection;
} // findCylindricalIntersection

StiIntersection StiMaterialInteraction::findConicalIntersection(
    const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
    double &dXlocal, double &dThickness, double &dDensity, double &dDistance){
  return kFailed;
} // findConicalIntersection

double StiMaterialInteraction::findThickness(const StiDetector *pDetector,
                                             const StThreeVector<double> *pPoint,
                                             const StThreeVector<double> *pDirection){

  double dAlpha = pDetector->getPlacement()->getNormalRefAngle();
  double dThickness = pDetector->getShape()->getThickness();

  // we must find the normal based on the detector type.
  StThreeVector<double> normal(1., 0., 0.);
  normal.rotateZ(dAlpha);  // this is fine for a plane
  StThreeVector<double> point = *pPoint;

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

bool StiMaterialInteraction::extrapolateHelixToPlane(
    const StiKalmanTrackNode *pNode,
    const StiDetector *pDetector,
    StThreeVector<double> *pIntersection,
    double &dNodeHalfThickness,
    double &dGapThickness,
    double &dDetectorHalfThickness){

  // first get the helix for the track node (uses node as origin)
  StThreeVector<double> dummy(1., 0., 0.);
  StPhysicalHelix helix(dummy, dummy, 0, 1);
  StiGeometryTransform *pTransform = StiGeometryTransform::instance();
  (*pTransform)(pNode, &helix);

  // get normal to plane in global
  StThreeVector<double> normal(1., 0., 0.);
  StiPlacement *pPlacement = pDetector->getPlacement();
  normal.rotateZ(pPlacement->getNormalRefAngle());

  // get point on plane in global
  StThreeVector<double> point(pPlacement->getNormalRadius(), 0., 0.);
  point.rotateZ(pPlacement->getNormalRefAngle());

  // get the path length between node center & detector center
  dGapThickness = helix.pathLength(point, normal); //tracking outside->in
  if(dGapThickness == DBL_MAX){ 
    return false; 
  }
  *pIntersection = helix.at(dGapThickness);

  // For the node & detector pathlengths, we still use a line approximation.
  StThreeVector<double> momentum = helix.momentumAt(
      dGapThickness, StiKalmanTrackNode::getFieldConstant());
  StThreeVector<double> node(pNode->fX, pNode->fP0, pNode->fP1);
  dDetectorHalfThickness = 0.5*findThickness(pDetector, pIntersection, 
                                             &momentum);
  dNodeHalfThickness = 0.5*findThickness(pNode->getDetector(), &node,  
                                         &momentum);
  dGapThickness -= (dDetectorHalfThickness + dNodeHalfThickness);

  return true;

} // extrapolateHelixToPlane

bool StiMaterialInteraction::extrapolateHelixToCylinder(
    const StiKalmanTrackNode *pNode,
    const StiDetector *pDetector,
    StThreeVector<double> *pIntersection,
    double &dNodeHalfThickness,
    double &dGapThickness,
    double &dDetectorHalfThickness){

  // first get the helix for the track node (uses node as origin)
  StThreeVector<double> dummy(1., 0., 0.);
  StPhysicalHelix helix(dummy, dummy, 0, 1);
  StiGeometryTransform *pTransform = StiGeometryTransform::instance();
  (*pTransform)(pNode, &helix);

  // get the path length between node center & detector center
  StiPlacement *pPlacement = pDetector->getPlacement();
  pair<double, double> pathLengths = helix.pathLength(
      pPlacement->getNormalRadius());
  dGapThickness = (fabs(pathLengths.first) > fabs(pathLengths.second)) ? 
      pathLengths.second : pathLengths.first;
  StThreeVector<double> intersection = helix.at(dGapThickness);
  
  // For the node & detector pathlengths, we still use a line approximation.
  StThreeVector<double> momentum = helix.momentumAt(
      dGapThickness, StiKalmanTrackNode::getFieldConstant());
  StThreeVector<double> node(pNode->fX, pNode->fP0, pNode->fP1);
  dDetectorHalfThickness = 0.5*findThickness(pDetector, pIntersection, 
                                             &momentum);
  dNodeHalfThickness = 0.5*findThickness(pNode->getDetector(), &node,
                                         &momentum);
  dGapThickness -= (dDetectorHalfThickness + dNodeHalfThickness);

  return true;

} // extrapolateHelixToCylinder

bool StiMaterialInteraction::extrapolateLineToPlane(
    const StiKalmanTrackNode *pNode,
    const StiDetector *pDetector,
    StThreeVector<double> *pIntersection,
    double &dNodeHalfThickness,
    double &dGapThickness,
    double &dDetectorHalfThickness){

  StiPlacement *pPlacement = pDetector->getPlacement();
  const StiDetector *pNodeDetector = pNode->getDetector();

  //---------------------------------------------------
  // first, do the intersection of the node's momentum 
  // vector with the detector plane in global coords.

  // get momentum of node in global coords
  double adMomentum[3];
  pNode->getMomentum(adMomentum);
  StThreeVector<double> momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pNode->fAlpha);

  // get normal to plane in global coords
  StThreeVector<double> normal(1., 0., 0.);
  normal.rotateZ(pPlacement->getNormalRefAngle());

  // get vector to node in global coords
  StThreeVector<double> node(pNode->fX, pNode->fP0, pNode->fP1);
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
  StThreeVector<double> nodeToIntersection = momentumCoefficient*momentum;
  *pIntersection = node + nodeToIntersection;

  //--------------------------------
  // determine thicknesses traversed

  // just use the cosine between the momentum and the detector normal to
  // calculate thickness traversed
  dDetectorHalfThickness = 0.5*findThickness(pDetector, pIntersection,  
                                             &momentum);
  dNodeHalfThickness = 0.5*findThickness(pNodeDetector, &node, &momentum);

  // subtract off the half-thicknesses traversed of in both detectors
  // to get the gap thickness traversed
  dGapThickness = nodeToIntersection.mag() - dDetectorHalfThickness -
      dNodeHalfThickness;

  return true;
} // extrapolateLineToPlane    

bool StiMaterialInteraction::extrapolateLineToCylinder(
    const StiKalmanTrackNode *pNode,
    const StiDetector *pDetector,
    StThreeVector<double> *pIntersection,
    double &dNodeHalfThickness,
    double &dGapThickness,
    double &dDetectorHalfThickness){

  StiPlacement *pPlacement = pDetector->getPlacement();
  const StiDetector *pNodeDetector = pNode->getDetector();

  //---------------------------------------------------
  // first, do the intersection of the node's momentum 
  // vector with the detector cylinder in global coords.

  // get momentum of node in global coords
  double adMomentum[3];
  pNode->getMomentum(adMomentum);
  StThreeVector<double> momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pNode->fAlpha);
  
  // get vector to node in global coords
  StThreeVector<double> node(pNode->fX, pNode->fP0, pNode->fP1);
  node.rotateZ(pNode->fAlpha);

  // get "transverse" vectors used in calculating intersection
  StThreeVector<double> zHat(0., 0., 1.);
  StThreeVector<double> momentumPerp = momentum.cross(zHat);
  StThreeVector<double> nodePerp = node.cross(zHat);

  // first, test for any intersection at all
  double radius = pPlacement->getNormalRadius();
  double determinant = radius*radius*momentumPerp.mag2() -
      node.dot(momentumPerp)*node.dot(momentumPerp);
  if(determinant<0.){ return false; }

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
  StThreeVector<double> nodeToIntersection = momentumCoefficient*momentum;
  StThreeVector<double> intersection = node + nodeToIntersection;

  //--------------------------------
  // determine thicknesses travsersed

  // just use the cosine between the momentum and the detector normal to
  // calculate thickness traversed
  dDetectorHalfThickness = 0.5*findThickness(pDetector, pIntersection, 
                                             &momentum);
  dNodeHalfThickness = 0.5*findThickness(pNodeDetector, &node, &momentum);

  // subtract off the half-thicknesses traversed of in both detectors
  // to get the gap thickness traversed
  dGapThickness = nodeToIntersection.mag() - dDetectorHalfThickness -
      dNodeHalfThickness;

  return true;
} // extrapolateLineToCylinder
    
