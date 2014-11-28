/***************************************************************************
 *
 * $Id: StSvtWaferGeometry.cc,v 1.2 2007/03/21 17:22:21 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Geometry
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferGeometry.cc,v $
 * Revision 1.2  2007/03/21 17:22:21  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Geometry object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////
#include "Stiostream.h"
#include "StSvtWaferGeometry.hh"
#include "StMessMgr.h"
#include "TString.h"

ClassImp(StSvtWaferGeometry);

//________________________________________________________________________________
StSvtWaferGeometry::StSvtWaferGeometry(int barrel, int ladder, int wafer) : 
  TGeoHMatrix(), StSvtHybrid(barrel, ladder, wafer, 0) {setName();}
//________________________________________________________________________________
StSvtWaferGeometry::StSvtWaferGeometry(int barrel, int ladder, int wafer, TGeoHMatrix &martix) : 
  TGeoHMatrix(martix), StSvtHybrid(barrel, ladder, wafer, 0) {setName();}
//________________________________________________________________________________
void StSvtWaferGeometry::setName() {
  id    = 1000*getLayerID()  + 100*getWaferID() + getLadderID() ;
  SetName(Form("R%04i",id));
}
//________________________________________________________________________________
void StSvtWaferGeometry::print(Option_t *option) {
  cout << "Barrel " << getBarrelID()
       << "\tLayer " << getLayerID()
       << "\tLadder " << getLadderID()
       << "\tWafer " << getWaferID()
       << "\tHybrid " << getHybridID() << "\t";
  ((TGeoHMatrix *) this)->Print(option);
}
