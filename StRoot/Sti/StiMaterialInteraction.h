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

// here, "north" & "south" are correct for sector 12 coordinates (top of TPC).
// I.e., north => Y_local>0 and west => Z>0
enum StiIntersection {kFailed = -1,         // could not find intersection
                      kCenter,                                      // hit
                      kNorthEdge, kEastEdge, kSouthEdge, kWestEdge, // on edge
                      kNorthOut, kEastOut, kSouthOut, kWestOut};    // outside

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
        StiKalmanTrackNode *pNode, StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
protected:
    static StiIntersection findPlanarIntersection(
        StiKalmanTrackNode *pNode, StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
    static StiIntersection findCylindricalIntersection(
        StiKalmanTrackNode *pNode, StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);
    static StiIntersection findConicalIntersection(
        StiKalmanTrackNode *pNode, StiDetector *pDetector,
        double &dXlocal, double &dThickness, double &dDensity);

};

#endif // defined STI_MATERIAL_INTERACTION_H

