//*-- Author :    Valery Fine   18/05/99  (E-mail: fine@bnl.gov)
// $Id: StHits3DPoints.h,v 1.2 1999/07/13 23:32:25 fine Exp $
// $Log: StHits3DPoints.h,v $
// Revision 1.2  1999/07/13 23:32:25  fine
// Methods GetXYZ has been removed since it is defined by base class
//
// Revision 1.2  1999/07/13 23:32:25  fine
// Methods GetXYZ has been removed since it is defined by base class
//
// Revision 1.1  1999/05/19 22:46:39  fine
//  New class to 3D drawing operation for StHit commections have been introduced
//
#ifndef STAR_StHits3DPoints
#define STAR_StHits3DPoints
#include <TPoints3DABC.h>

class StObjArray;

class StHits3DPoints : public TPoints3DABC {
private:
        StObjArray *m_HitCollection;
public:
        StHits3DPoints(StObjArray *hitCollection=0);
        virtual ~StHits3DPoints();
        virtual Float_t *GetP() const { return 0;}
        virtual Int_t     GetN() const { return 0;}
        virtual Int_t     GetLastPosition()const;
        virtual Float_t   GetAnyPoint(Int_t idx,Int_t iAxis)  const;
        virtual Float_t   GetX(Int_t idx)  const { return GetAnyPoint(idx,0);}
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
 
        ClassDef(StHits3DPoints,0)  //A 3-D Points
};
 

#endif
