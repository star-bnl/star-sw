//StiTPolyLine3D.h
//M.L. Miller (Yale Software)
//06/01

/* This class derives from TPolyLine3D and is used to eliminate some functions that we don't want
 */

#ifndef StiTPolyLine3D_HH
#define StiTPolyLine3D_HH

#include "TPolyLine3D.h"

class StiTPolyLine3D : public TPolyLine3D
{
public:
    StiTPolyLine3D();
    StiTPolyLine3D(Int_t n, Option_t* option="");
    StiTPolyLine3D(Int_t n, Float_t* p, Option_t* option="");
    StiTPolyLine3D(Int_t n, Double_t* p, Option_t* option="");
    StiTPolyLine3D(Int_t n, Float_t* x, Float_t* y, Float_t* z, Option_t* option="");
    StiTPolyLine3D(Int_t n, Double_t* x, Double_t* y, Double_t* z, Option_t* option="");
    virtual ~StiTPolyLine3D();
    virtual Int_t DistancetoPrimitive(Int_t px, Int_t py);
};

#endif
