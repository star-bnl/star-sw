//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_PointsArray3D                                                     //
//                                                                      //
// A 3-D PolyLine.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef ROOT_St_PointsArray3D
#define ROOT_St_PointsArray3D

// $Id: St_PointsArray3D.h,v 1.3 1999/12/21 18:57:14 fine Exp $

#include "TPoints3DABC.h"

#ifndef ROOT_TString
//*KEEP,TSTRING.
#include "TString.h"
//*KEND.
#endif


class St_PointsArray3D : public TPoints3DABC {

protected:
        Int_t        fN;            // Number of points
        Float_t     *fP;            // Array of 3-D coordinates  (x,y,z)
        TString      fOption;       // options
        UInt_t       fGLList;       // The list number for OpenGL view
        Int_t        fLastPoint;    // The index of the last filled point

public:
        St_PointsArray3D();
        St_PointsArray3D(Int_t n, Option_t *option="");
        St_PointsArray3D(Int_t n, Float_t *p, Option_t *option="");
        St_PointsArray3D(Int_t n, Float_t *x, Float_t *y, Float_t *z, Option_t *option="");
        St_PointsArray3D(const St_PointsArray3D &points);
        virtual ~St_PointsArray3D();

        virtual void      Copy(TObject &points);
        virtual Int_t     DistancetoPrimitive(Int_t px, Int_t py);
        virtual void      ExecuteEvent(Int_t event, Int_t px, Int_t py);
        virtual Int_t     GetLastPosition()       const;
        virtual Int_t     GetN() const;
        virtual Float_t  *GetP() const;
        virtual Float_t   GetX(Int_t idx)  const;
        virtual Float_t   GetY(Int_t idx)  const;
        virtual Float_t   GetZ(Int_t idx)  const;
        virtual Float_t  *GetXYZ(Float_t *xyz,Int_t idx,Int_t num=1)  const;
        virtual const Float_t  *GetXYZ(Int_t idx);
        virtual Option_t *GetOption() const ;
        virtual Bool_t    Is3D();
        virtual void      ls(Option_t *option="");
        virtual void      PaintPoints(Int_t , Float_t *,Option_t *){;}
        virtual void      Print(Option_t *option="");
        virtual Int_t     SetLastPosition(Int_t idx);
        virtual void      SetOption(Option_t *option="");
        virtual Int_t     SetPoint(Int_t point, Float_t x, Float_t y, Float_t z); // *MENU*
        virtual Int_t     SetPoints(Int_t n, Float_t *p=0, Option_t *option="");
        virtual Int_t     Size() const;

        ClassDef(St_PointsArray3D,1)  //A 3-D PolyLine
};


inline Int_t     St_PointsArray3D::GetLastPosition()  const                   {return fLastPoint;}
inline Int_t     St_PointsArray3D::GetN()  const                              {return fN;}
inline Float_t  *St_PointsArray3D::GetP()  const                              {return fP;}
inline Float_t   St_PointsArray3D::GetX(Int_t idx)  const                     {return fP[3*idx+0];}
inline Float_t   St_PointsArray3D::GetY(Int_t idx)  const                     {return fP[3*idx+1];}
inline Float_t   St_PointsArray3D::GetZ(Int_t idx)  const                     {return fP[3*idx+2];}
inline const Float_t  *St_PointsArray3D::GetXYZ(Int_t idx)                    {return  &fP[3*idx+0];}
inline Float_t  *St_PointsArray3D::GetXYZ(Float_t *xyz,Int_t idx,Int_t num)  const
                          {return (Float_t  *)memcpy(xyz,&fP[3*idx],3*num*sizeof(Float_t));}
inline Option_t *St_PointsArray3D::GetOption() const                          {return fOption.Data();}
inline Bool_t    St_PointsArray3D::Is3D()                                     {return kTRUE;}
inline void      St_PointsArray3D::SetOption(Option_t *option)                {fOption = option;}

inline Int_t     St_PointsArray3D::Size() const                               {return fLastPoint+1;}

// $Log: St_PointsArray3D.h,v $
// Revision 1.3  1999/12/21 18:57:14  fine
// compilation warning plus new type for SizeAttribute
//
#endif
