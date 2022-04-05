/*!
 * \class EEmcSmdGeom
 * \author Wei-Ming Zhangg, Jason Webb
 * 
 *****************************************************************************
 *
 * $Id: EEmcSmdGeom.h,v 1.10 2010/08/26 22:48:55 ogrebeny Exp $
 *
 * 
 *
 *****************************************************************************
 *
 * Description: Interface to EEMC-SMD database
 * 
 * The following demensions are defined for SMD in EEmcGeomDefs.h
 * EEmcNumSectors     = 12 (The order follows numbering scheme of TPC sectors)
 * kEEmcNumSmdPlanes  =  3 (1: the innermost and 3: the outermost) 
 * kEEmcNumStrips     =288 (1: the shortes inner and 288: the shortest outer) 
 * kEEmcNumEdgeStrips =283 (1: the shortes inner and 283: the shortest outer)
 * kEEmcNumSmdLayers  =  2 (1: U and 2: V) 
 *
 *
 *  Valid range of arguments for ~all input params for methods in this class 
 *    iSec=[0,11], maps sectors [1,12]
 *    iUV=[0,1], maps SMD planes [U,V]
 *    iPlane=[0,1,2] - experts only, changes meaning form sector to sector
 *    iUStrip, iVStrip=[0,287], maps SMD strip ID [1,288]
 *
 *****************************************************************************/


#ifndef EEMCSMDGEOM_H
#define EEMCSMDGEOM_H

#include <iostream>

#include "TObject.h"
#include "TVector3.h"
#include "TString.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::cout;
#endif

struct StructEEmcSmdParam {
  float zPlane[kEEmcNumSmdPlanes];  // Z of planes
  float rOffset[kEEmcNumSmdPlanes]; // radius offsets
  float stripWidth;                 // width of strip
};

struct StructEEmcStripId{
  int  stripId;                     // strip Id  1-288 (283 for edge sector)
  int  UVId;                        // 1 for U and 2 for V
  int  sectorId;                    // sector Id 1-12 
  int  planeId;                     // plane Id (depth) 1-3
};

struct StructEEmcStrip{
  StructEEmcStripId stripStructId; // including 4 Ids
  TVector3    end1;          // one end of strip         
  TVector3    end2;          // the other end of strip
  float length;                    // length of strip 
};

ostream& operator<<(ostream &os, const StructEEmcStrip &strip);

// Define vector of strip pointers and its Iterator
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<int> intVec;  
typedef vector<StructEEmcStrip*> EEmcStripPtrVec;  
#else
typedef vector<int,allocator<int>> intVec; 
typedef vector<StructEEmcStrip*,allocator<StructEEmcStrip*>> EEmcStripPtrVec; 
#endif   
typedef vector<StructEEmcStrip*>::iterator EEmcStripPtrVecIter; 


struct StructEEmcSmdSector {
  int   sectorId;	
  int   planeId;        
  float phiMin;    // minimun phi
  float phiMax;    // maximum phi
  float rMin;      // minimum radius
  float rMax;      // maximum radius
  EEmcStripPtrVec stripPtrVec;  
};

class EEmcSmdGeom : public TObject {
public:  

  EEmcSmdGeom();
  virtual ~EEmcSmdGeom(); 

protected:

  StructEEmcSmdParam     mEEmcSmdParam;  //! general geometry variables
  StructEEmcSmdSector    mEEmcSector[kEEmcNumSmdUVs][kEEmcNumSectors]; //! storage for 2*12 sectors    
  //! storage for all strip pointers  
  EEmcStripPtrVec       mStripPtrVector; //! storage for all strip pointers  
  bool                   mIsSectorIn[kEEmcNumSectors];    //! sector status. 
  int   kEEmcSmdMap_iPlane[kEEmcNumSmdUVs][kEEmcNumSectors];

  void buildSmdGeom();

  static EEmcSmdGeom* sInstance;

public:
  /// iPlane=[0,1,2] - experts only, changes meaning form sector to sector
  /// return a DCA strip pointer from a point (float *dca carries sign)  
  const StructEEmcStrip* getDcaStripPtr(const Int_t iPlane, const TVector3& point, Float_t* dca) const;
  const StructEEmcStrip* getDcaStripPtr(const Int_t iPlane, const Int_t iSec, const TVector3& point, Float_t* dca) const;
  
  
  static EEmcSmdGeom* instance();   // handle the only instance
  static EEmcSmdGeom* instance(intVec sectorIdVec);   
  
  void init();         // init the dbase
  
  /// build mStripPtrVector   
  void buildStripPtrVector();

  /// set sectors for partial EEMC
  void setSectors(const intVec sectorIdVec); 
  
  /// return sector status   
  bool IsSectorIn(const Int_t iSec) const {return mIsSectorIn[iSec];}
  
  /// instance and initialize a strip 
  StructEEmcStrip initStrip() const;

  /// return SMD geometry parameters  
  StructEEmcSmdParam &getEEmcSmdParam() {return mEEmcSmdParam;}
  const StructEEmcSmdParam &getEEmcSmdParam() const {return mEEmcSmdParam;}

  /// return structure-sector from iUV and iSec 
  StructEEmcSmdSector &getEEmcSector(const Int_t iUV, const Int_t iSec) {return mEEmcSector[iUV][iSec];}
  const StructEEmcSmdSector &getEEmcSector(const Int_t iUV, const Int_t iSec) const {return mEEmcSector[iUV][iSec];}

  /// return index of a sector from a point in a plane 
  Int_t getEEmcISec(const Int_t iPlane, const TVector3& point) const;
  
  /// return a strip pointer from indices   
  StructEEmcStrip* getStripPtr(const Int_t iStrip, const Int_t iUV, const Int_t iSec);
  const StructEEmcStrip* getStripPtr(const Int_t iStrip, const Int_t iUV, const Int_t iSec) const;
  
  /// Returns a pointer to the eemc smd strip which is closest to the
  /// given point in the specified SMD plane.
  /// @param iUV 0=smd-u plane, 1=smd-v plane
  /// @param point TVector3 specifying a point on the endcap
  /// @param dca distance of closest approach to the strip
  /// @And priori we do not know if the track is charged or not and how to extrapolate it.
  /// @User must takes care to provide 'point' at the z-location he/she needs the cross point.
  /// @Now I realize it is the chicken and egg problem if we want sub-mm accuracy. May be solved by iterations.
  const  StructEEmcStrip* getDca2Strip(const Int_t iUV, const TVector3& point, Float_t* dca) const;

  
  /// In a given sector, return the vector from the specified vertex
  /// along the line defined by the intersection of the two planes
  /// defined by the two SMD strips.  If two strips do not intersect,
  /// a warning is issued and -999 returned in the third component.
  /// @param iSec sector number [0,12)
  /// @param iUStrip index of the U strip [0,nstrips)
  /// @param iVStrip index of the V strip [0,nstrips)
  /// @param vertex the event vertex
  TVector3 getIntersection ( Int_t iSec, Int_t iUStrip, Int_t iVStrip, const TVector3 &vertex ) const;

  /// Return the vector from the specified vertex
  /// along the line defined by the intersection of the two planes
  /// defined by the two SMD strips.  If two strips do not intersect,
  /// a warning is issued and -999 returned in the third component.
  /// @param u a pointer to the structure which defines the smd-u strip
  /// @param v a pointer to the structure which defines the smd-v strip
  /// @param vertex the event vertex
  TVector3 getIntersection ( const StructEEmcStrip *u, const StructEEmcStrip *v, const TVector3 &vertex ) const;

  /// Assumes nominal vertex (0,0,0)
  TVector3 getIntersection ( Int_t iSec, Int_t iUStrip, Int_t iVStrip ) const;
  /// Assumes nominal vertex (0,0,0)
  TVector3 getIntersection ( Int_t iSec, Float_t iUStrip, Float_t iVStrip ) const
    { return getIntersection( iSec, (Int_t)iUStrip, (Int_t)iVStrip ); }
  /// Assumes nominal vertex (0,0,0)
  TVector3 getIntersection ( const StructEEmcStrip *u, const StructEEmcStrip *v ) const;


  /// Returns the number of SMD strips in the specified sector and plane
  /// @param iSec eemc sector number [0,12)
  /// @param iUV eemc smd plane number 0=U 1=V
  Int_t getNStrips ( Int_t iSec, Int_t iUV ) const { return getEEmcSector(iUV,iSec).stripPtrVec.size();  }

  /// match two strips 
  bool matchStrips(const StructEEmcStripId &stripStructId1, const StructEEmcStripId &stripStructId2, Int_t nTolerance) const;

  // mehtod for C-scripts

  /// return strip-end of 3D-vector   
  TVector3  getstripEnd(const StructEEmcStrip &strip, const Int_t endId) const;

  //
  // methods of printout 
  //

  void printGeom(ostream& os = cout) const;
  void printSector(const StructEEmcSmdSector Sector, ostream& os = cout) const;
  void printStrip(const StructEEmcStrip Strip, ostream& os = cout) const;
  void printStripId(const StructEEmcStripId StripId, ostream& os = cout) const;
  //void printSectorPhis(const Int_t iPlane, const Int_t iSec,ostream& os = cout);

  ClassDef(EEmcSmdGeom,1)  // STAR Endcap Electromagnetic Calorimeter SMD Geometry Class
};

#endif
 
/*******************************************************************
 *
 *
 * $Log: EEmcSmdGeom.h,v $
 * Revision 1.10  2010/08/26 22:48:55  ogrebeny
 * Improved constness
 *
 * Revision 1.9  2007/07/12 19:30:15  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.8  2007/02/01 13:47:39  balewski
 * bug fix in getDca2Strip(), more methodhs are public
 *
 * Revision 1.7  2007/01/26 00:51:08  balewski
 * too strong protection
 *
 * Revision 1.6  2007/01/25 22:33:21  balewski
 * add:
 * - better writeup
 * - new simpler to use method calculating dca fo track to strip, it is just a wrapper, some approximations were used, may fail at the sector boundary
 *
 * Revision 1.5  2007/01/12 23:53:14  jwebb
 * Fix applied to eliminate parralax error in the EEmcSmdGeom::getIntersection()
 * method.
 *
 * Revision 1.3  2004/07/01 15:35:50  jwebb
 * Added placeholder method getIntersection(Int_t,Float_t,Float_t).  For now
 * it just calls the getIntersection with strips converted to integers.
 * Later we will write code to handle fractional strips.
 *
 * Revision 1.2  2004/02/03 22:57:54  jwebb
 * Added StEEmcSmdGeom::instance(), which is sort of needed...
 *
 * Revision 1.1  2004/01/29 15:26:10  jwebb
 * The StEEmcSmdGeom class was split into two classes.  All StRoot-independent
 * code has been moved to EEmcSmdGeom.  TVector3 replaces StThreeVectorD in
 * all function calls in EEmcSmdGeom.  StThreeVectorD wrappers are provided
 * in StEEmcSmdGeom, for integration into Star framework.
 *
 * Revisions:
 *
 * 01/28/04 Jason Webb -- Renamed to EEmcSmdGeom, StRoot dependent code moved 
 * to a derived class StEEmcSmdGeom.  The user interface for StEEmcSmdGeom
 * should remain unchanged.  Revision history for StEEmcSmdGeom moved  to end
 * of header file.  
 *
 * Log: StEEmcSmdGeom.h,v 
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
 ****************************************************************/
