/*******************************************************************
 *
 * $Id: StEEmcSmdGeom.h,v 1.10 2010/08/26 22:48:55 ogrebeny Exp $
 *
 * Author: Wei-Ming Zhang
 *
 * Revisions:
 *
 * 01/28/04 Jason Webb -- StRoot independent code moved to a
 * separate EEmcSmdGeom class.  StEEmcSmdGeom now derives from 
 * that class, implementing functions useful for integrating
 * with Star.  See EEmcSmdGeom.{h,cxx} for further documentation.
 *
 *****************************************************************
 *
 * Description: Interface to EEMC-SMD database
 *
 *****************************************************************
 * 
 * The following demensions are defined for SMD in EEmcGeomDefs.h
 * EEmcNumSectors     = 12 (The order follows numbering scheme of TPC sectors)
 * kEEmcNumSmdPlanes  =  3 (1: the innermost and 3: the outermost) 
 * kEEmcNumStrips     =288 (1: the shortes inner and 288: the shortest outer) 
 * kEEmcNumEdgeStrips =283 (1: the shortes inner and 283: the shortest outer)
 * kEEmcNumSmdLayers  =  2 (1: U and 2: V) 
 *
 *****************************************************************/
#ifndef STEEMCSMDGEOM_H
#define STEEMCSMDGEOM_H

#include "EEmcSmdGeom.h"

// StRoot classes
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

class StEEmcSmdGeom : public EEmcSmdGeom {
public:
  StEEmcSmdGeom();
  virtual ~StEEmcSmdGeom();

protected:
  // The single allowed instance of the class
  static StEEmcSmdGeom *sInstance;

public:
  // Method(s) to return the single allowed instance of this class
  static StEEmcSmdGeom *instance();
  static StEEmcSmdGeom *instance(intVec sectorIdVec);


  Int_t getEEmcISec(const Int_t iPlane, const StThreeVectorD& point) const;

  const StructEEmcStrip* getDcaStripPtr(const Int_t iPlane, StThreeVectorD& point, Float_t* dca) const;
  const StructEEmcStrip* getDcaStripPtr(const Int_t iPlane, const Int_t iSec, const StThreeVectorD& point, Float_t* dca) const;

  StThreeVectorD getIntersection ( Int_t iSec, Int_t iUStrip, Int_t iVStrip ) const;
  StThreeVectorD getIntersection ( const StructEEmcStrip *u, const StructEEmcStrip *v ) const; 

  StThreeVectorD getstripEnd(const StructEEmcStrip &strip, const Int_t endId) const;

  //
  // three methods for ITTF
  //

  // return phiMin and phiMax of a sector including empty sector 
  pairD getEEmcSmdPhiMinMax(const Int_t iPlane, const Int_t iSec) const;

  // return delta_phi of a sector including empty sector 
  float getEEmcSmdDelPhi(const Int_t iPlane, const Int_t iSec) const;

  // return center phi of a sector including empty sector 
  float getEEmcSmdCenterPhi(const Int_t iPlane, const Int_t iSec) const;  

  //
  // Additional ITTF print function
  //
  void printSectorPhis(const Int_t iPlane, const Int_t iSec,ostream& os = cout) const;  

  ClassDef(StEEmcSmdGeom,1);
};

#endif

/********************************************************************
 * $Log: StEEmcSmdGeom.h,v $
 * Revision 1.10  2010/08/26 22:48:55  ogrebeny
 * Improved constness
 *
 * Revision 1.9  2007/01/25 22:33:22  balewski
 * add:
 * - better writeup
 * - new simpler to use method calculating dca fo track to strip, it is just a wrapper, some approximations were used, may fail at the sector boundary
 *
 * Revision 1.8  2004/02/03 22:57:55  jwebb
 * Added StEEmcSmdGeom::instance(), which is sort of needed...
 *
 * Revision 1.7  2004/01/29 15:26:10  jwebb
 * The StEEmcSmdGeom class was split into two classes.  All StRoot-independent
 * code has been moved to EEmcSmdGeom.  TVector3 replaces StThreeVectorD in
 * all function calls in EEmcSmdGeom.  StThreeVectorD wrappers are provided
 * in StEEmcSmdGeom, for integration into Star framework.
 *
 * Revision 1.6  2003/12/05 00:06:11  jwebb
 * Member function added to return a vector pointing to the intersection of
 * two strips.
 *
 * Revision 1.5  2003/10/15 15:26:03  wzhang
 * improved and reorganized
 *
 * Revision 1.4  2003/08/22 15:14:03  wzhang
 * Added ClassDef and method stripEnd
 *
 * Revision 1.3  2003/06/11 18:58:15  wzhang
 * added geometry methods for StiEEmc
 *
 * Revision 1.2  2003/04/04 15:33:31  wzhang
 * included EEmcGeomDefs.h & improved codes
 *
 * Revision 1.1  2003/03/28 15:50:00  balewski
 * first
 *
 *
 *******************************************************************/
