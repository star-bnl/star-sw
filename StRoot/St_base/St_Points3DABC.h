#ifndef ROOT_St_Points3DABC
#define ROOT_St_Points3DABC

//+SEQ,CopyRight,T=NOINCLUDE.

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Points3DABC                                                       //
//                                                                      //
// Abstract class to define Arrays of 3D points                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TObject
#include "TObject.h"
#endif

class St_Points3DABC : public TObject {

public:
        St_Points3DABC(){;}
        virtual ~St_Points3DABC(){;}

        static  Int_t     DistancetoLine(Int_t px, Int_t py, Float_t x1, Float_t y1, Float_t x2, Float_t y2, Int_t lineWidth = 1 );

        virtual Int_t     Add(Float_t x, Float_t y, Float_t z);
        virtual Int_t     AddLast(Float_t x, Float_t y, Float_t z);
        virtual Int_t     GetLastPosition()const    =0;
        virtual Int_t     GetN() const              =0;
        virtual Float_t  *GetP() const              =0;
        virtual Float_t   GetX(Int_t idx)  const    =0;
        virtual Float_t   GetY(Int_t idx)  const    =0;
        virtual Float_t   GetZ(Int_t idx)  const    =0;
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx,Int_t num=1)  const     =0;
        virtual const Float_t  *GetXYZ(Int_t idx)   =0;
        virtual Option_t *GetOption()      const    =0;
        virtual void      PaintPoints(Int_t n, Float_t *p,Option_t *option="")  =0;
        virtual Int_t     SetLastPosition(Int_t idx)=0;
        virtual Int_t     SetNextPoint(Float_t x, Float_t y, Float_t z);
        virtual void      SetOption(Option_t *option="")=0;
        virtual Int_t     SetPoint(Int_t point, Float_t x, Float_t y, Float_t z)=0;
        virtual Int_t     SetPoints(Int_t n, Float_t *p=0, Option_t *option="") =0;
        virtual Int_t     Size() const              =0;

        ClassDef(St_Points3DABC,0)  //A 3-D Points

};

#endif
