/***************************************************************************
 *
 * $Id: StSvtWaferGeometry.hh,v 1.2 2007/03/21 17:22:21 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Geometry object. It makes the link with the Data Base
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferGeometry.hh,v $
 * Revision 1.2  2007/03/21 17:22:21  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/

#ifndef STSVTWAFERGEOMETRY_HH
#define STSVTWAFERGEOMETRY_HH

#include "StSvtHybrid.h"
#include "TGeoMatrix.h"
class StSvtWaferGeometry: public TGeoHMatrix, public StSvtHybrid
{
public:
  StSvtWaferGeometry(Int_t barrel = 0, Int_t ladder = 0, Int_t wafer = 0);
  StSvtWaferGeometry(Int_t barrel, Int_t ladder, Int_t wafer, TGeoHMatrix &martix);
  virtual ~StSvtWaferGeometry() {}

  StSvtWaferGeometry& operator = (const StSvtWaferGeometry&) {return *this;}

  void setID(Int_t i){id = i;}
  void setDriftDirection(Double_t x1, Double_t x2, Double_t x3)      
  {Double_t *r = GetRotationMatrix();  r[0] = x1; r[3] = x2; r[6] = x3;}
  void setTransverseDirection(Double_t x1, Double_t x2, Double_t x3) 
  {Double_t *r = GetRotationMatrix();  r[1] = x1; r[4] = x2; r[7] = x3;};
  void setNormalDirection(Double_t x1, Double_t x2, Double_t x3)     
  {Double_t *r = GetRotationMatrix();  r[2] = x1; r[5] = x2; r[8] = x3;};
  void setCenterPosition(Double_t x1, Double_t x2, Double_t x3)      {Double_t *t = GetTranslation(); t[0] = x1; t[1] = x2; t[2] = x3;}
  
  Int_t getID(){return id;}
  Double_t d(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i];  }
  Double_t t(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+1];}
  Double_t n(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+2];}
  Double_t x(Int_t i){Double_t *t = GetTranslation();    return t[i];    }
  void setName();
  void print(Option_t *option=""); 

private:

  Int_t id;

  ClassDef(StSvtWaferGeometry,1)
};

#endif
