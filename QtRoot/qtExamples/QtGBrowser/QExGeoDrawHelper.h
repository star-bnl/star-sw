#ifndef ROOT_QExGeoDrawHelper
#define ROOT_QExGeoDrawHelper

// Author: Valeri Fine   19/01/2004
/****************************************************************************
** $Id: QExGeoDrawHelper.h,v 1.4 2013/08/30 16:00:06 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.      
**
*****************************************************************************/
#include <map>
#include "TObject.h"

class TGeoVolume;
class TVolume;
class TShape;
class TMap;

class TGeoShape;
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
class TGeoGtra;
class TGeoSphere;
class TGeoArb8;
class TGeoCompositeShape;


class TGeoDrawHelper : public TObject {
private:
protected:
   TGeoVolume *fGeoVolume; //!
   TVolume    *fVolume;    //!
public:
   TGeoDrawHelper(TGeoVolume *geoVolume = 0);
   virtual ~TGeoDrawHelper();
   virtual Int_t       DistancetoPrimitive(Int_t px, Int_t py);
   virtual void        Draw(Option_t *option="");
   virtual const char *GetName() const;
   virtual const char *GetIconName() const;
   virtual void        Paint(Option_t *option="");

   virtual TVolume    *GetVolume()    const { return fVolume;   }
   virtual TGeoVolume *GetGeoVolume() const { return fGeoVolume;}

#ifndef __CINT__
   static TVolume* MakeVolume( TGeoVolume *top, std::map<TGeoVolume *,TVolume *> *volumeMap = 0);
#endif
//   static TVolume* MakeVolume( TGeoVolume *top, TMap *volumeMap = 0);
   static TVolume* MakeCompositeShape(const TGeoCompositeShape *shape);
   static TShape* MakeShape(const TGeoShape *shape);
   static TShape* MakeShape(const TGeoBBox  &shp);
   static TShape* MakeShape(const TGeoPara  &shp);
   static TShape* MakeShape(const TGeoTube  &shp);
   static TShape* MakeShape(const TGeoTubeSeg &shp);
   static TShape* MakeShape(const TGeoCone  &shp);
   static TShape* MakeShape(const TGeoConeSeg &shp);
   static TShape* MakeShape(const TGeoPcon  &shp);
   static TShape* MakeShape(const TGeoPgon  &shp);
   static TShape* MakeShape(const TGeoEltu  &shp);
   static TShape* MakeShape(const TGeoTrap  &shp);
   static TShape* MakeShape(const TGeoCtub  &shp);
   static TShape* MakeShape(const TGeoTrd1  &shp);
   static TShape* MakeShape(const TGeoTrd2  &shp);
   static TShape* MakeShape(const TGeoGtra  &shp);
   static TShape* MakeShape(const TGeoSphere &shp);
   static TShape* MakeShape(const TGeoArb8  &shp);
   
  //  ClassDef(TGeoDrawHelper,0)
};



#endif
