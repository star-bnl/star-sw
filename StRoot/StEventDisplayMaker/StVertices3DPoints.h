//*-- Author :    Valery Fine   03/08/99  (E-mail: fine@bnl.gov)
// $Id: StVertices3DPoints.h,v 1.1 1999/11/02 01:49:52 fine Exp $
// $Log: StVertices3DPoints.h,v $
// Revision 1.1  1999/11/02 01:49:52  fine
// The primitives moved from StEvent
//
// Revision 1.1  1999/08/03 18:52:16  fine
// New class StVertices3DPoints to draw StVetex collections has been introduced
//
#ifndef STAR_StVertices3DPoints
#define STAR_StVertices3DPoints
#include <TPoints3DABC.h>

class StObjArray;

class StVertices3DPoints : public TPoints3DABC {
private:
        StObjArray *m_VertexCollection;
public:
        StVertices3DPoints(StObjArray *vertexCollection=0);
        virtual ~StVertices3DPoints();
        virtual Float_t  *GetP() const { return 0;}
        virtual Int_t     GetN() const { return 0;}
        virtual Int_t     GetLastPosition()const;
        virtual Float_t   GetAnyPoint(Int_t idx,Int_t iAxis)  const;
        virtual Float_t   GetX(Int_t idx)  const { return GetAnyPoint(idx,0);}
        virtual Float_t   GetY(Int_t idx)  const { return GetAnyPoint(idx,1);}
        virtual Float_t   GetZ(Int_t idx)  const { return GetAnyPoint(idx,2);}
        virtual const Float_t  *GetXYZ(Int_t) {return 0;}
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx=0,Int_t num=1)  const;
        virtual Option_t *GetOption()      const ;
        virtual void      PaintPoints(Int_t n, Float_t *p,Option_t *option="");
        virtual Int_t     SetLastPosition(Int_t idx);
        virtual void      SetOption(Option_t *option="");
        virtual Int_t     SetPoint(Int_t point, Float_t x, Float_t y, Float_t z);
        virtual Int_t     SetPoints(Int_t n, Float_t *p=0, Option_t *option="");
        virtual Int_t     Size() const {  return GetLastPosition()+1; }
 
        ClassDef(StVertices3DPoints,0)  //A 3-D Points
};
 

#endif
