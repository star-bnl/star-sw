// StiMaterialInteraction
//
// 7 Aug 01, Ben Norman, Kent State
//
// Provides functionality for determining thicknesses of materials
// traversed by tracks, etc.

#ifndef STI_MATERIAL_INTERACTION_H
#define STI_MATERIAL_INTERACTION_H

class StiTrackNode;

class StiMaterialInteraction{

public:
    StiMaterialInteraction(){}
    virtual ~StiMaterialInteraction(){}

    // returns the thickness in radiation lengths between the detector 
    // of the beginning node and the detector of the ending node.
    // Includes thickness of any intermediate gas, plus the thickness
    float getEquivalentThickness(StiTrackNode *nodeBegin,
                                 StiTrackNode *nodeEnd);

protected:

};

#endif // defined STI_MATERIAL_INTERACTION_H
