// StiMaterialInteraction
//
// 7 Aug 01, Ben Norman, Kent State
//
// Provides functionality for determining thicknesses of materials
// traversed by tracks, etc.

#ifndef STI_MATERIAL_INTERACTION_H
#define STI_MATERIAL_INTERACTION_H

// uncertainty along detector edges (in cm)
#define TPC_EDGE_HALF_WIDTH 2.0
#define SVG_EDGE_HALF_WIDTH 0.5

class StiKalmanTrackNode;
class StThreeVectorD;
class Messenger;

// indicate where an intersection occurs
typedef enum {
    kFailed = -1,         // could not find intersection
    kHit,                                
    kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
    kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus
} StiIntersection;



class StiMaterialInteraction{

public:
    StiMaterialInteraction(){}
    virtual ~StiMaterialInteraction(){}

    static void nameForIntersection(StiIntersection &intersection, 
                                    string &name);

    // returns an integer value indicating whether & where the extrapolation
    // of the given track node intersects the given detector.  Also returns
    // (via reference arguments) the local x coordinate of the intersection
    // with the detector active plane,
    // the thickness in radiation lengths between the node (exclusive) & 
    // the detector (inclusive), and the equivalent density.
    //
    // Note:  this will not return thickness info of other detectors between
    // the node and the detector.  It is the caller's responsibility to 
    // only go one step at a time.
    static StiIntersection findIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity,
        double &dDistance);
protected:
    static StiIntersection findPlanarIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity,
        double &dDistance);
    static StiIntersection findCylindricalIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity,
        double &dDistance);
    static StiIntersection findConicalIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity,
        double &dDistance);

    static double findThickness(const StiDetector *pDetector, 
                                const StThreeVectorD *pPoint,
                                const StThreeVectorD *pDirection);

    static Messenger *s_pMessenger;
};

#endif // defined STI_MATERIAL_INTERACTION_H



