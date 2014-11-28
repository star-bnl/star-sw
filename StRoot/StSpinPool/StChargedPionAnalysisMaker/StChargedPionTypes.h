#ifndef STAR_StChargedPionTypes
#define STAR_StChargedPionTypes

// $Id: StChargedPionTypes.h,v 1.1 2008/07/17 17:06:31 kocolosk Exp $

#include "Math/PxPyPzE4D.h"
#include "Math/LorentzVector.h"
#include "Math/LorentzRotation.h"
#include "Math/RotationY.h"
#include "Math/RotationZ.h"
#include "Math/Point3D.h"
#include "Math/Boost.h"
using namespace ROOT::Math;

typedef PositionVector3D< Cartesian3D<Double32_t>, DefaultCoordinateSystemTag > 
        StChargedPion3Vector;
        
typedef LorentzVector< PxPyPzE4D<Double32_t> >
        StChargedPionLorentzVector;

#include "StMiniMcEvent/StTinyMcTrack.h"
typedef StTinyMcTrack StChargedPionMcTrack;

#include "StChargedPionJet.h"
typedef StChargedPionJet StChargedPionMcJet;

#include "StMiniMcEvent/StMiniMcPair.h"
typedef StMiniMcPair StChargedPionTrackPair;

class StChargedPionPythiaRow {
public:
    virtual ~StChargedPionPythiaRow() {}
    short id;
    StChargedPionLorentzVector vec;
    ClassDef(StChargedPionPythiaRow, 1)
};
ClassImp(StChargedPionPythiaRow)
#endif

/*****************************************************************************
 * $Log: StChargedPionTypes.h,v $
 * Revision 1.1  2008/07/17 17:06:31  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
