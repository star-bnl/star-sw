//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_Table3Points.h,v 1.2 1999/11/04 18:03:04 fine Exp $
// $Log: St_Table3Points.h,v $
// Revision 1.2  1999/11/04 18:03:04  fine
// new ctor for tablepoints introduced to make EventDiplay happy
//
// Revision 1.1  1999/05/18 20:21:24  fine
// New class to 3D table viewer
//  

#ifndef STAR_St_Table3Points
#define STAR_St_Table3Points

#include <TObjArray.h>
#include "St_TablePoints.h"

class St_TableElementDescriptor;

class St_Table3Points :  public St_TablePoints 
{
  private:
       TObjArray   *m_ArrayOfColumnDesciptors;

  public:
        enum EPointDirection {kXPoints,kYPoints,kZPoints,kTotalSize};
        St_Table3Points();
        St_Table3Points(St_TableSorter *sorter,const void *key, const Char_t *xName="x", 
                        const Char_t *yName="y", const Char_t *zName="z",Option_t *opt="");
        St_Table3Points(St_TableSorter *sorter,Int_t keyIndex, const Char_t *xName="x", 
                        const Char_t *yName="y", const Char_t *zName="z",Option_t *opt="");
        ~St_Table3Points();
        virtual void    SetAnyColumn(const Char_t *anyName, EPointDirection indx);
        virtual void    SetXColumn(const Char_t *xName){ SetAnyColumn(xName,kXPoints);}
        virtual void    SetYColumn(const Char_t *yName){ SetAnyColumn(yName,kYPoints);}
        virtual void    SetZColumn(const Char_t *zName){ SetAnyColumn(zName,kZPoints);}
        virtual St_TableElementDescriptor *GetDescriptor(EPointDirection idx) const;
        virtual Int_t   GetTotalKeys(){ return -1;}
        virtual Int_t   GetKey(Int_t ){return -1;}
        virtual Int_t   SetKeyByIndx(Int_t ){return -1;}
        virtual Int_t   SetKeyByValue(Int_t){return -1;}

        virtual Float_t   GetAnyPoint(Int_t idx, EPointDirection xAxis) const;
        virtual Float_t   GetX(Int_t idx)  const {return GetAnyPoint(idx,kXPoints);}
        virtual Float_t   GetY(Int_t idx)  const {return GetAnyPoint(idx,kYPoints);}
        virtual Float_t   GetZ(Int_t idx)  const {return GetAnyPoint(idx,kZPoints);}
 //-- abstract methods
         virtual void PaintPoints(int, float*, const char*) {}
         virtual const Float_t *GetXYZ(Int_t) {return 0;}
         virtual Float_t *GetXYZ(Float_t *xyz,Int_t idx , Int_t num=1 )const;
         virtual Float_t *GetP() const {return 0;}
         virtual Int_t    GetN() const {return -1;} 

 //
        ClassDef(St_Table3Points,0)  //A 3-D Points
};
 
#endif

