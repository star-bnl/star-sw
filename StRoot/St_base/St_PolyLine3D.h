
#ifndef ROOT_St_PolyLine3D
#define ROOT_St_PolyLine3D

//+SEQ,CopyRight,T=NOINCLUDE.

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_PolyLine3D                                                        //
//                                                                      //
// A 3-D PolyLine.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_Points3D.h"
#include "TAttLine.h"
#include "X3DBuffer.h"
#include "St_AttributesABC.h"


class St_PolyLine3D : public St_Points3D, public St_AttributesABC {
protected:        
        St_AttributesABC   *fAttributes; //!

public:
        St_PolyLine3D(St_Points3DABC *points=0);
        St_PolyLine3D(Int_t n, Option_t *option="");
        St_PolyLine3D(Int_t n, Float_t *p, Option_t *option="");
        St_PolyLine3D(Int_t n, Float_t *x, Float_t *y, Float_t *z, Option_t *option="");
        St_PolyLine3D(const St_PolyLine3D &polylin);
        virtual ~St_PolyLine3D();

        virtual void      Copy(TObject &polyline);
        virtual Int_t     DistancetoPrimitive(Int_t px, Int_t py);
        virtual void      Draw(Option_t *option="");
        virtual void      DrawPolyLine(Int_t n, Float_t *p, Option_t *option="");
        virtual Color_t   GetColorAttribute() const;
        virtual Width_t   GetSizeAttribute() const ;
        virtual Style_t   GetStyleAttribute() const;
        virtual void      Paint(Option_t *option="");
        virtual void      PaintPoints(Int_t n, Float_t *p=0,Option_t *option="");
        virtual void      PaintPolyLine(Int_t n, Float_t *p, Option_t *option="");
        virtual void      SavePrimitive(ofstream &out, Option_t *option);
        virtual Color_t   SetColorAttribute(Color_t color);
        virtual Width_t   SetSizeAttribute(Width_t  size);
        virtual Style_t   SetStyleAttribute(Style_t style);
        virtual void      SetPolyLine(Int_t n, Float_t *p=0, Option_t *option="");
        virtual void      Sizeof3D() const;
        virtual Int_t     Size() const {return fPoints? fPoints->Size():0;}

        virtual Int_t     GetAttributeI(const Char_t *attribName) const;
        virtual Float_t   GetAttributeF(const Char_t *attribName) const;
        virtual Double_t  GetAttributeD(const Char_t *attribName) const;
        virtual Int_t     SetAttribute(const Char_t *attribName,Int_t    attrib);
        virtual Float_t   SetAttribute(const Char_t *attribName,Float_t  attrib);
        virtual Double_t  SetAttribute(const Char_t *attribName,Double_t attrib);
        virtual Int_t     GetNumberOfAttributes() const;
        virtual Int_t     SetNumberOfAttributes(Int_t n);


        ClassDef(St_PolyLine3D,1)  //A 3-D PolyLine
};

#endif

