//StiTPolyMarker3D.h
//M.L. Miller (Yale Software)
//06/01

/* This class derives from TPolyMarker3D and is used to eliminate some functions that we don't want
 */

#ifndef StiTPolyMarker3D_HH
#define StiTPolyMarker3D_HH
#include "TPolyMarker3D.h"

class StiTPolyMarker3D : public TPolyMarker3D
{
public:
    StiTPolyMarker3D();
    StiTPolyMarker3D(Int_t n, Marker_t marker = 1, Option_t* option="");
    StiTPolyMarker3D(Int_t n, Float_t* p, Marker_t marker = 1, Option_t* option="");
    StiTPolyMarker3D(Int_t n, Double_t* p, Marker_t marker=1, Option_t* option="");
    virtual ~StiTPolyMarker3D();
    virtual Int_t DistancetoPrimitive(Int_t px, Int_t py);
};

#endif
