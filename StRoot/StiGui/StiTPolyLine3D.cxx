//StiTPolyLine3D.cxx

#include "StiTPolyLine3D.h"

StiTPolyLine3D::StiTPolyLine3D()
{
}

StiTPolyLine3D::StiTPolyLine3D(Int_t n, Option_t* option)
    : TPolyLine3D(n, option)
{
}

StiTPolyLine3D::StiTPolyLine3D(Int_t n, Float_t* p, Option_t* option)
    : TPolyLine3D(n, p, option)
{
}

StiTPolyLine3D::StiTPolyLine3D(Int_t n, Double_t* p, Option_t* option)
    : TPolyLine3D(n, p, option)
{
}

StiTPolyLine3D::~StiTPolyLine3D()
{
}

Int_t StiTPolyLine3D::DistancetoPrimitive(Int_t px, Int_t py)
{
    Int_t val = 9999999;
    //val = 0;
    return val;
}
