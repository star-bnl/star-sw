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

#include "StThreeVector.hh"
class StiKalmanTrackNode;
class Messenger;

// indicate where an intersection occurs
typedef enum {
    kFailed = -1,         // could not find intersection
    kHit,                                
    kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
    kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus
} StiIntersection;

// determine which type of extrapolation to use for tracks
typedef enum { 
    kLinearExtrapolation = 1, kHelixExtrapolation 
} StiExtrapolationType;

class StiMaterialInteraction{

public:
    StiMaterialInteraction(){}
    virtual ~StiMaterialInteraction(){}

    static void nameForIntersection(StiIntersection &intersection, 
                                    string &name);

    /// sets the extrapolation to kLinearExtrapolation or
    /// kHelixExtrapolation
    static void setExtrapolationType(StiExtrapolationType type){
      s_extrapolationType = type;
    }
    /// returns the extrapolation type
    static StiExtrapolationType getExtrapolationType(){
      return s_extrapolationType;
    }

    /// returns an integer value indicating whether & where the extrapolation
    /// of the given track node intersects the given detector.  Also returns
    /// (via reference arguments) the (detector) local x coordinate of the 
    /// intersection with the detector active plane,
    /// the thickness in radiation lengths between the node (exclusive) & 
    /// the detector (inclusive), and the equivalent density.
    ///
    /// Note:  this will not return thickness info of other detectors between
    /// the node and the detector.  It is the caller's responsibility to 
    /// only go one step at a time.
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
    /// NOT IMPLEMENTED YET!
    static StiIntersection findConicalIntersection(
        const StiKalmanTrackNode *pNode, const StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity,
        double &dDistance);

    /// Given the last known position and direction of a particle
    /// in global coordinates, does a straight line extrapolation 
    /// and returns the particle's position at the 
    /// point of intersection with the given detector's central surface.
    /// Also returns 3 path lengths: that traversed between the node and the
    /// edge of the node's detector, that traversed between the node's
    /// detector and the detector, and that traversed between the detector
    /// edge and the intersection point.  Returns true on success,
    /// false on failure.
    static bool extrapolateLineToPlane(const StiKalmanTrackNode *pNode,
                                       const StiDetector *pDetector,
                                       StThreeVector<double> *pIntersection,
                                       double &dNodeThickness,
                                       double &dGapThickness,
                                       double &dDetectorThickness);
    
    /// see extrapolateLineToPlane(...)
    static bool extrapolateHelixToPlane(const StiKalmanTrackNode *pNode,
                                        const StiDetector *pDetector,
                                        StThreeVector<double> *pIntersection,
                                        double &dNodeThickness,
                                        double &dGapThickness,
                                        double &dDetectorThickness);
    
    /// see extrapolateLineToPlane(...)
    static bool extrapolateLineToCylinder(const StiKalmanTrackNode *pNode,
                                          const StiDetector *pDetector,
                                          StThreeVector<double> *pIntersection,
                                          double &dNodeThickness,
                                          double &dGapThickness,
                                          double &dDetectorThickness);
    
    /// see extrapolateLineToPlane(...)
    static bool extrapolateHelixToCylinder(const StiKalmanTrackNode *pNode,
                                           const StiDetector *pDetector,
                                           StThreeVector<double> *pIntersection,
                                           double &dNodeThickness,
                                           double &dGapThickness,
                                           double &dDetectorThickness);
    
    /// returns thickness*(direction dot normal at point).
    /// All coordinates are assumed to be global.  pDirection need not be 
    /// normalized.
    /// returns -1 on failure.
    static double findThickness(const StiDetector *pDetector, 
                                const StThreeVector<double> *pPoint,
                                const StThreeVector<double> *pDirection);

    static Messenger *s_pMessenger;

    static StiExtrapolationType s_extrapolationType;
};

#endif // defined STI_MATERIAL_INTERACTION_H



