// StiMaterialInteraction
//
// 7 Aug 01, Ben Norman, Kent State
//
// Provides functionality for determining thicknesses of materials
// traversed by tracks, etc.

#ifndef STI_MATERIAL_INTERACTION_H
#define STI_MATERIAL_INTERACTION_H

// uncertainty along detector edges (in cm)
#define EDGE_HALF_WIDTH .5

class StiKalmanTrackNode;
class StThreeVectorD;

// indicate where an intersection occurs
enum StiIntersection {kFailed = -1,         // could not find intersection
                      kHit,                                
                      kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
                      kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus};

/*
ostream& operator<<(ostream& os, StiIntersection &intersection){
  switch(intersection){
    case kFailed: os << "failed"; break;
    case kHit: os << "hit"; break;
    case kEdgePhiPlus: os << "edgePhiPlus"; break;
    case kEdgeZminus: os << "edgeZminus"; break;
    case kEdgePhiMinus: os << "edgePhiMinus"; break;
    case kEdgeZplus: os << "edgeZplus"; break;
    case kMissPhiPlus: os << "missPhiPlus"; break;
    case kMissZminus: os << "missZminus"; break;
    case kMissPhiMinus: os << "missPhiMinus"; break;
    case kMissZplus: os << "missZplus"; break;
  }
  return os; 
}
*/

class StiMaterialInteraction{

public:
    StiMaterialInteraction(){}
    virtual ~StiMaterialInteraction(){}

    // returns an integer value indicating whether & where the extrapolation
    // of the given track node intersects the given detector.  Also returns
    // (via reference arguments) the local x coordinate of the intersection
    // with the detector active plane,
    // the thickness in radiation lengths between the node (exclusive) & 
    // the detector (inclusive), and the equivalent density.
    static StiIntersection findIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
protected:
    static StiIntersection findPlanarIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
    static StiIntersection findCylindricalIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
    static StiIntersection findConicalIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);

    static double findThickness(const StiDetector *pDetector, 
                                const StThreeVectorD *pPoint,
                                const StThreeVectorD *pDirection);
};

#endif // defined STI_MATERIAL_INTERACTION_H

