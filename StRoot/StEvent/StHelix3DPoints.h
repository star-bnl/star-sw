//*-- Author :    Valery Fine   21/05/99  (E-mail: fine@bnl.gov)
// $Id: StHelix3DPoints.h,v 1.1 1999/05/22 18:59:31 fine Exp $
// $Log: StHelix3DPoints.h,v $
// Revision 1.1  1999/05/22 18:59:31  fine
// New class to draw StHelix3D and StTrack has been introduced
//
// Revision 1.1  1999/05/22 18:59:31  fine
// New class to draw StHelix3D and StTrack has been introduced
//
#ifndef STAR_StHelix3DPoints
#define STAR_StHelix3DPoints
#include <TPoints3DABC.h>

class StHelixD;
class StTrack;

class StHelix3DPoints : public TPoints3DABC {
private:
        StHelixD *m_Helix;
        Int_t     m_N;
        StHelix3DPoints(StTrack *track,        Float_t step=0,Int_t lastPosition=50);
        StHelix3DPoints(StTrack *track);
        StHelix3DPoints(StHelixD *trackHelix=0,Float_t step=0,Int_t lastPosition=50);
        StHelix3DPoints(StTrack *track,        Float_t length,Int_t lastPosition=50);
        virtual ~StHelix3DPoints();
        virtual Float_t  *GetP() const { return 0;}
        virtual Int_t     GetN() const { return 0;}
        virtual Int_t     GetLastPosition()const;
        virtual Float_t   GetAnyPoint(Int_t idx,Int_t iAxis)  const;
        virtual Float_t   GetX(Int_t idx)  const { return GetAnyPoint(idx,0);}
        virtual Float_t   GetZ(Int_t idx)  const { return GetAnyPoint(idx,2);}
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx, Int_t num)const;
        virtual const Float_t  *GetXYZ(Int_t) {return 0;}
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx=0,Int_t num=1)  const;
        virtual Float_t   GetStep() const { return m_Step; }
        virtual Option_t *GetOption()      const ;
        virtual void      PaintPoints(Int_t n, Float_t *p,Option_t *option="");
        virtual Int_t     SetLastPosition(Int_t idx);
        virtual void      SetOption(Option_t *option="");
        virtual Int_t     Size() const {  return GetLastPosition()+1; }
        virtual Int_t     SetPoints(Int_t n, Float_t *p=0, Option_t *option="");
        virtual Float_t   SetStep(Float_t step){Float_t s = m_Step; m_Step = step; return s;} 
        virtual Int_t     Size() const {  return m_N; }
 
        ClassDef(StHelix3DPoints,0)  //A 3-D Points
};
 

#endif
