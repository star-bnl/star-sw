//StiTPolyMarker3D.cxx

#include "StiTPolyMarker3D.h"

StiTPolyMarker3D::StiTPolyMarker3D()
{
}

StiTPolyMarker3D::StiTPolyMarker3D(Int_t n, Marker_t marker, Option_t* option)
    : TPolyMarker3D(n, marker, option)
{
}

StiTPolyMarker3D::StiTPolyMarker3D(Int_t n, Float_t* p, Marker_t marker, Option_t* option)
    : TPolyMarker3D(n, p, marker, option)
{
}

StiTPolyMarker3D::StiTPolyMarker3D(Int_t n, Double_t* p, Marker_t marker, Option_t* option)
    : TPolyMarker3D(n, p, marker, option)
{
}

StiTPolyMarker3D::~StiTPolyMarker3D()
{
}

Int_t StiTPolyMarker3D::DistancetoPrimitive(Int_t px, Int_t py)
{
    Int_t val = 9999999;
    //val = 0;
    return val;
}
