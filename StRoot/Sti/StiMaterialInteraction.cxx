#include "StiMaterial.h"
#include "StiDetector.h" 
#include "StiShape.h" 
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiPlacement.h" 
#include "StiTrackNode.h"
#include "StiMaterialInteraction.h"
#include "StiGeometryTransform.h" 

#include "StHelix.hh"
#include "StThreeVector.hh"

#include <float.h>

// Tries to return the thickness of material(in radiation lengths)
// traversed in the detector belonging to nodeEnd when coming from nodeBegin.
//
// If the geometrical intersection fails, returns the thickness traversed
// by a normally incident particle.
//
// If something really goes wrong, returns <0  
//
// TODO:
//
// 1) So far, this only tests intersections with the outer and inner boundaries
// (not edges perpendicular to z).
//
// 2) Ignores kConical
//
// 3) Does not test intersection point to ensure that it is within the 
//    physical extent of the detector
float StiMaterialInteraction::getEquivalentThickness(StiTrackNode *nodeBegin,
                                                     StiTrackNode *nodeEnd){

  // make StHelix from local helix params
  StHelix helix(1., 1., 1., StThreeVector<double>(0., 0., 0.), 1);
  StiGeometryTransform *pGeometryTransform = StiGeometryTransform::instance();
  (*pGeometryTransform)(nodeBegin, &helix);

  // get (hopefully) 2 intersection points with the detector
  StiShapeCode iCode = nodeEnd->getDetector()->getShape()->getShapeCode();
  StiPlacement *pPlacement = nodeEnd->getDetector()->getPlacement();
  if(iCode==kPlanar){
    StiPlanarShape *pShape = dynamic_cast<StiPlanarShape *>(
        nodeEnd->getDetector()->getShape());
    
    // find intersections with both inner & outer boundaries
    double dRadius = pPlacement->getNormalRadius() + pShape->getThickness()/2.;
    double dRefAngle = pPlacement->getNormalRefAngle();
    StThreeVector<double> normalToPlane(cos(dRefAngle), sin(dRefAngle), 0.);
    StThreeVector<double> pointInPlane(dRadius*cos(dRefAngle),
                                       dRadius*sin(dRefAngle), 0.);
    double dPathLength1 = helix.pathLength(pointInPlane, normalToPlane);
    
    dRadius -= pShape->getThickness();
    pointInPlane.setMag(dRadius);
    double dPathLength2 = helix.pathLength(pointInPlane, normalToPlane);

    // for now, just bail if we don't get 2 legit intersections
    if(dPathLength1==DBL_MAX || dPathLength2==DBL_MAX){ return -1; }
    
    // otherwise, return the equivalent thickness
    return(fabs(dPathLength1 - dPathLength2) /
           nodeEnd->getDetector()->getMaterial()->getRadLength());

  }else if(iCode==kCylindrical){
    StiCylindricalShape *pShape = dynamic_cast<StiCylindricalShape *>(
        nodeEnd->getDetector()->getShape());

    // find intersections with both inner and outer boundaries
    double dRadius = pShape->getOuterRadius();
    double dPathLength1 = helix.pathLength(dRadius).first;
    dRadius -= pShape->getThickness();
    double dPathLength2 = helix.pathLength(dRadius).first;
    
    // for now, just bail if we don't get 2 legit intersections
    if(dPathLength1==DBL_MAX || dPathLength2==DBL_MAX){ return -1; }
    
    // otherwise, return the equivalent thickness
    return(fabs(dPathLength1 - dPathLength2) /
           nodeEnd->getDetector()->getMaterial()->getRadLength());

  }else if(iCode==kConical){
    return -1;  // to be implemented
  }

  return -1;
} // getEquivalentThickness()

