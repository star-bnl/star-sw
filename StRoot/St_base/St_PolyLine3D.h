#ifndef ROOT_St_PolyLine3D
#define ROOT_St_PolyLine3D

// ***********************************************************************
// $Id: St_PolyLine3D.h,v 1.8 1999/12/17 23:28:40 fine Exp $ 
// ***********************************************************************
// * Defines 3D polyline base class to construct STAR "event" geometry
// * Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// * Author                  Valerie Fine  (fine@bnl.gov)
// * Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// *
// * Permission to use, copy, modify and distribute this software and its
// * documentation for any purpose is hereby granted without fee,
// * provided that the above copyright notice appear in all copies and
// * that both that copyright notice and this permission notice appear
// * in supporting documentation.  Brookhaven National Laboratory makes no
// * representations about the suitability of this software for any
// * purpose.  It is provided "as is" without express or implied warranty.
// ************************************************************************
 

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_PolyLine3D                                                        //
//                                                                      //
// A 3-D PolyLine.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "St_PolyLineShape.h"
#include "X3DBuffer.h"
#include <TPoints3DABC.h>

class St_PolyLine3D : public St_PolyLineShape {

public:
        St_PolyLine3D(TPoints3DABC *points=0);
        St_PolyLine3D(Int_t n, Option_t *option="L");
        St_PolyLine3D(Int_t n, Float_t *p, Option_t *option="L");
        St_PolyLine3D(Int_t n, Float_t *x, Float_t *y, Float_t *z, Option_t *option="L");
        St_PolyLine3D(const St_PolyLine3D &polylin);
        virtual ~St_PolyLine3D();

        static  void      Axis(TVirtualPad *p=0, Float_t width=0.5, Float_t axisFactor=0.25);
        virtual void      Copy(TObject &polyline);
        virtual Int_t     DistancetoPrimitive(Int_t px, Int_t py);
        virtual void      DrawPolyLine(Int_t n, Float_t *p, Option_t *option="");
        virtual void      SavePrimitive(ofstream &out, Option_t *option);
        virtual void      SetPolyLine(Int_t n, Float_t *p=0, Option_t *option="");
        virtual Int_t     GetAttributeI(const Char_t *attribName) const;
        virtual Float_t   GetAttributeF(const Char_t *attribName) const;
        virtual Double_t  GetAttributeD(const Char_t *attribName) const;
        virtual Int_t     SetAttribute(const Char_t *attribName,Int_t    attrib);
        virtual Float_t   SetAttribute(const Char_t *attribName,Float_t  attrib);
        virtual Double_t  SetAttribute(const Char_t *attribName,Double_t attrib);
        virtual Int_t     GetNumberOfAttributes() const;
        virtual Int_t     SetNumberOfAttributes(Int_t n);

        virtual Int_t     Add(Float_t x, Float_t y, Float_t z)     {return m_Points ? m_Points->Add(x,y,z) : 0;}
        virtual Int_t     AddLast(Float_t x, Float_t y, Float_t z) {return m_Points ? m_Points->AddLast(x,y,z):0;}
        virtual Int_t     GetLastPosition()const                   {return m_Points ? m_Points->GetLastPosition(): -1;}
        virtual Int_t     GetN() const                             {return m_Points ? m_Points->GetN(): 0;}
        virtual Float_t  *GetP() const                             {return m_Points ? m_Points->GetP(): 0;}
        virtual Float_t   GetX(Int_t idx)  const                   {return m_Points ? m_Points->GetX(idx): 0;}
        virtual Float_t   GetY(Int_t idx)  const                   {return m_Points ? m_Points->GetY(idx): 0;}
        virtual Float_t   GetZ(Int_t idx)  const                   {return m_Points ? m_Points->GetZ(idx): 0;}
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx,Int_t num=1)  const {return m_Points ? m_Points->GetXYZ(xyz,idx,num):0;}
        virtual const Float_t  *GetXYZ(Int_t idx)                  {return m_Points ? m_Points->GetXYZ(idx):0;}
        virtual Option_t *GetOption()      const                   {return m_Points ? m_Points->GetOption():0;}
        virtual Int_t     SetNextPoint(Float_t x, Float_t y, Float_t z) {return m_Points ? m_Points->SetNextPoint(x,y,z) : 0;}
        virtual void      SetOption(Option_t *option="")           { if(m_Points) m_Points->SetOption(option);}
        virtual Int_t     SetPoint(Int_t point, Float_t x, Float_t y, Float_t z){return m_Points ? m_Points->SetPoint(point,x,y,z): 0;}
        virtual Int_t     SetPoints(Int_t n, Float_t *p=0, Option_t *option="") {return m_Points ? m_Points->SetPoints(n,p,option): 0;}


        ClassDef(St_PolyLine3D,1)  //Defines 3D polyline base class to construct STAR "event" geometry
};
//__________________________________________________________________________
// $Log: St_PolyLine3D.h,v $
// Revision 1.8  1999/12/17 23:28:40  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced
//
//__________________________________________________________________________
#endif

