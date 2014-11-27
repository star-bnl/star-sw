// @(#)root/gtgl:$Name:  $:$Id: TObject3DViewFactory.h,v 1.5 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      24/04/05

#ifndef ROOT_TObject3DViewFactory
#define ROOT_TObject3DViewFactory

/****************************************************************************
**
** TObject3DViewFactory

** An unterface of the class visitor to convert the ROOT 3D objets into 
** the concrete "viewer" representation like OpenGL, OpenInventor, x3d etc
**
** Copyright (C) 2005 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TObject3DViewFactoryABC.h"


class TShape3DPolygonView;
//________________________________________________________________
//
//  TShape forward declarations
//________________________________________________________________
class TBRIK;
class TPARA;
class TTUBE;
class TTUBS;
class TCONE;
class TCONS;
class TPCON;
class TPGON;
class TELTU;
class TTRAP;
class TCTUB;
class TTRD1;
class TTRD2;
class TSPHE;
class TGTRA;
class THYPE;
class TXTRU;
//________________________________________________________________
//
//  TGeoShape forward declarations
//________________________________________________________________
class TGeoBBox;
class TGeoPara;
class TGeoTube;
class TGeoTubeSeg;
class TGeoCone;
class TGeoConeSeg;
class TGeoPcon;
class TGeoPgon;
class TGeoEltu;
class TGeoTrap;
class TGeoCtub;
class TGeoTrd1;
class TGeoTrd2;
class TGeoSphere;
class TGeoGtra;
class TGeoArb8;

class TGeoParaboloid;
class TGeoTorus;
class TGeoHype;
class TGeoXtru;

class TShape;
class TGeoShape;
//________________________________________________________________
//
//  Misc 3D ROOT classes
//________________________________________________________________
class TPolyLine3D;
class TPolyMarker3D;
class TPolyLineShape;

class TObject3DViewFactory : public TObject3DViewFactoryABC {
protected:
   enum {kPlain, kCyl, kSphere};
   virtual TObject3DView *MakeShape(TShape3DPolygonView &shapeView, const Float_t *rgba)=0;

   template <class S> TObject3DView *MakeBrikShape(const S  &shp, const Float_t *rgba);
   template <class S> TObject3DView *MakeXtruShape(const S  &shp, const Float_t *rgba, int nDiv, int nstacks);
   template <class S> TObject3DView *MakeConeShape(const S  &shp, const Float_t *rgba,  int nDiv, int nstacks,Int_t plain=kPlain);
   //________________________________________________________________
   //
   //   Make the TShape
   //________________________________________________________________
   TObject3DView *MakeShape(const TBRIK &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TPARA &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TTUBE &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TTUBS &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TCONE &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TCONS &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TPCON &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TPGON &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TELTU &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TTRAP &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TCTUB &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TTRD1 &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TTRD2 &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TSPHE &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGTRA &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const THYPE &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TXTRU &shp, const Float_t *rgba);
   //________________________________________________________________
   //
   //   Make the TGeoShape
   //________________________________________________________________
   TObject3DView *MakeShape(const TGeoBBox    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoPara    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTube    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTubeSeg &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoCone    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoConeSeg &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoPcon    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoPgon    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoEltu    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTrap    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoCtub    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTrd1    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTrd2    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoSphere  &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoGtra    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoArb8    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoTorus   &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoHype    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoXtru    &shp, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoParaboloid &shp, const Float_t *rgba);

   TObject3DView *MakeShape(const TShape    *shape, const Float_t *rgba);
   TObject3DView *MakeShape(const TGeoShape *shape, const Float_t *rgba);

   TObject3DView *MakeShape(const TPolyLine3D   *shape, const Float_t *rgba);
   TObject3DView *MakeShape(const TPolyMarker3D *shape, const Float_t *rgba);
   TObject3DView *MakeShape(const TPolyLineShape *shape, const Float_t *rgba);

public:
   TObject3DViewFactory();
   virtual ~TObject3DViewFactory();
   virtual void AddNormal(TObject3DView *, const Double_t *normal);
//   virtual TObject3DView *CreateMatrix(const Double_t *traslation, const Double_t *rotation, Bool_t isReflection);
//   virtual TObject3DView *CreateNormal(const Double_t *normal);
   virtual TObject3DView *CreateShape (const TObject *shape,const Float_t *rgba);
   virtual void    CompileViewLevel(TObject3DView *,ERenderType type=kNormal);
   virtual ULong_t GetViewerId(TObject3DView *) const ;
   virtual Bool_t  NeedCompilation() const ;
   virtual void    Release(TObject3DView *);
};
#endif
