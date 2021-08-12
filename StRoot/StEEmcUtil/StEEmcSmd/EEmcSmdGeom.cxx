/*!
 * \class EEmcSmdGeom
 * \author Wei-Ming Zhangg, Jason Webb
 * 
 ****************************************************************************
 *
 * $Id: EEmcSmdGeom.cxx,v 1.17 2015/07/21 17:10:02 jeromel Exp $
 *
 * Author: Wei-Ming Zhang
 * 
 * Revisions:
 *
 * 01/28/04 Jason Webb -- Renamed to EEmcSmdGeom, StRoot dependent code moved 
 * to a derived class StEEmcSmdGeom.  The user interface for StEEmcSmdGeom
 * should remain unchanged.  Revision history for StEEmcSmdGeom moved  to end
 * of header file.  
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
 *****************************************************************************/

/*! \class StEEmcSmdGeom
    \author Wei-Ming Zhang

*/
#include "Stiostream.h"
#include "EEmcSmdGeom.h"
#include "EEmcStripGeom.h"
#include <assert.h>
#include <TMath.h>

// decouple from StarClassLibrary
//#include "PhysicalConstants.h"  
#ifndef HEP_SYSTEM_OF_UNITS_H
static const double     radian      = 1.;
static const double     pi          = M_PI; // from <math.h>
static const double     degree      = (M_PI/180.0)*radian;
#endif


#include "StMessMgr.h"


/// defaulty constructor
ClassImp(EEmcSmdGeom)

EEmcSmdGeom::EEmcSmdGeom()
    : TObject()
{ 
  for(int iSec = 0; iSec < kEEmcNumSectors; iSec++) mIsSectorIn[iSec] = true;
};
/// default empty destructor
EEmcSmdGeom::~EEmcSmdGeom(){ 
  delete sInstance;
  sInstance = 0;
}

/// Initialize geometry class 
void EEmcSmdGeom::init(){ 
	buildSmdGeom(); 
}

EEmcSmdGeom* EEmcSmdGeom::sInstance = 0;	
// all setctors
EEmcSmdGeom* EEmcSmdGeom::instance() {	
    if(!sInstance){
        sInstance = new EEmcSmdGeom();
	sInstance->init();
   }
   return sInstance;
}     

// selected sectors
EEmcSmdGeom* EEmcSmdGeom::instance(intVec sectorIdVec) {	
    if(!sInstance){
        sInstance = new EEmcSmdGeom();
        sInstance->setSectors(sectorIdVec);    
	sInstance->init();
   }
   return sInstance;
}     

// build a glabal geometry database from local coordinates
void EEmcSmdGeom::buildSmdGeom(){
  mEEmcSmdParam.stripWidth = 0.5;
  for(int iPlane=0; iPlane<kEEmcNumSmdPlanes; iPlane++) 
	         mEEmcSmdParam.rOffset[iPlane] = kEEmcSmdROffset[iPlane];

  float x0[kEEmcNumStrips];
  float y1[kEEmcNumStrips];
  float y2[kEEmcNumStrips]; 
  float length[kEEmcNumStrips];
  float x0Edge[kEEmcNumEdgeStrips];
  float y1Edge[kEEmcNumEdgeStrips];
  float y2Edge[kEEmcNumEdgeStrips];
  float lengthEdge[kEEmcNumEdgeStrips];

// fill variable arrays with data in EmcStripGeom.h 
  for (int i = 0; i < kEEmcNumStrips; i++) {
       x0[i] = EEmcStripGeomData[i].x0;
       y1[i] = EEmcStripGeomData[i].y1;
       y2[i] = EEmcStripGeomData[i].y2;
       length[i] = EEmcStripGeomData[i].length;
  }
  for (int i = 0; i < kEEmcNumEdgeStrips; i++) {
       x0Edge[i] = EEmcEdgeStripGeomData[i].x0;
       y1Edge[i] = EEmcEdgeStripGeomData[i].y1;
       y2Edge[i] = EEmcEdgeStripGeomData[i].y2;
       lengthEdge[i] = EEmcEdgeStripGeomData[i].length;
  }
/*
// write x, y, and length to a file for a check
  FILE *fp;
  fp = fopen("arrCheck.lis", "w");
  fprintf(fp, " Reqular\n");
  for (int i = 0; i < kEEmcNumStrips; i++) {
       fprintf(fp, " strip %d: x0, y1, y2, length = %f %f %f %f\n",
                                i, x0[i], y1[i],  y2[i], length[i]);
  }
  fprintf(fp, " Edge\n");
  for (int i = 0; i < kEEmcNumEdgeStrips; i++) {
       fprintf(fp, " strip %d: x0, y1, y2, length = %f %f %f %f\n",
                  i, x0Edge[i], y1Edge[i],  y2Edge[i], lengthEdge[i]);
  }
  fclose(fp);
*/
  float delPhi = 2*pi/degree/kEEmcNumSectors;
  float PhiRotation[kEEmcNumSmdUVs][kEEmcNumSectors];

// calculate rotation angles to prepare for local-global transformation
  for(int iUV = 0; iUV < kEEmcNumSmdUVs; iUV++) {
    for(int iSec=0; iSec<kEEmcNumSectors; iSec++){
       PhiRotation[iUV][iSec]=(-15.0 + iSec*delPhi)*degree; 
       if(iUV == 1) PhiRotation[iUV][iSec] = -1.0*PhiRotation[iUV][iSec];  
    }
  }

// loop over planes
  for (int iPlane = 0; iPlane < kEEmcNumSmdPlanes; iPlane++) {
    float globalX1, globalY1, globalX2, globalY2;
    float x0Corr, y1Corr, y2Corr, lengthCorr; 
    float phi1, phi2, phiMin, phiMax;
    float r, rMin, rMax;
  
    mEEmcSmdParam.zPlane[iPlane] = kEEmcZSMD + 
	             (iPlane - kEEmcNumSmdPlanes + 2) * kEEmcSmdZPlaneShift ;
// loop over UV
    for(int iUV = 0; iUV < kEEmcNumSmdUVs; iUV++) { 

// loop over sectors
      for(int iUVSec=iPlane+1-iUV; iUVSec<kEEmcNumSectors+1-iUV; 
		                     iUVSec=iUVSec+kEEmcNumSmdPlanes) {
        int iSec;
        if(iUVSec == 12) iSec = 0;	    
        else iSec = iUVSec;

        if(IsSectorIn(iSec)) {

          rMin = 1000.0;
          rMax = 0.0;
          phiMin = pi; 
          phiMax = -pi;

// loop over strips     
          for (int iStrip = 0; iStrip < kEEmcNumStrips; iStrip++) {   
            if(kEEmcSmdMapEdge[iPlane][iSec] && iStrip > kEEmcNumEdgeStrips-1) 
		                                                    break;      
// Id = index + 1 in all cases
            StructEEmcStripId  stripStructId;
            stripStructId.sectorId = iSec+1;
	    stripStructId.UVId = iUV+1;
	    stripStructId.stripId = iStrip + 1;
	    stripStructId.planeId = iPlane + 1;

	    StructEEmcStrip*    stripPtr = new StructEEmcStrip;
	    stripPtr->stripStructId = stripStructId; 

// correct for radius offset 
	    x0Corr = x0[iStrip] - kEEmcSmdROffset[iPlane]*::sqrt(0.5);
	    y2Corr = y2[iStrip] - kEEmcSmdROffset[iPlane]*::sqrt(0.5);
            if(kEEmcSmdMapEdge[iPlane][iSec]) {      
	      y1Corr = y1Edge[iStrip] - kEEmcSmdROffset[iPlane]*::sqrt(0.5);
	      lengthCorr = lengthEdge[iStrip];
            }
	    else { 
	      y1Corr = y1[iStrip] - kEEmcSmdROffset[iPlane]*::sqrt(0.5);
	      lengthCorr = length[iStrip];
            }
// Transform local to gloabal by rotation. Local X axis is perpendicular to
// strip, Y is parallel. After rotation, x & y should be swapped for V sector. 
	    
            if(iUV == 0) {
              globalX1 = x0Corr*cos(PhiRotation[iUV][iSec])+ 
		                     y1Corr*sin(PhiRotation[iUV][iSec]);
              globalY1 = y1Corr*cos(PhiRotation[iUV][iSec]) - 
		                     x0Corr*sin(PhiRotation[iUV][iSec]);
              globalX2 = x0Corr*cos(PhiRotation[iUV][iSec]) + 
		                     y2Corr*sin(PhiRotation[iUV][iSec]);
              globalY2 = y2Corr*cos(PhiRotation[iUV][iSec]) - 
		                     x0Corr*sin(PhiRotation[iUV][iSec]);
            }
            else {
              globalX1 = y1Corr*cos(PhiRotation[iUV][iSec]) - 
	                              x0Corr*sin(PhiRotation[iUV][iSec]);
              globalY1 = x0Corr*cos(PhiRotation[iUV][iSec]) + 
			              y1Corr*sin(PhiRotation[iUV][iSec]);
              globalX2 = y2Corr*cos(PhiRotation[iUV][iSec]) - 
	                              x0Corr*sin(PhiRotation[iUV][iSec]);
              globalY2 = x0Corr*cos(PhiRotation[iUV][iSec]) + 
		                      y2Corr*sin(PhiRotation[iUV][iSec]);
            }
	    r = ::sqrt(globalX1*globalX1 + globalY1*globalY1);
	    if(r < rMin) rMin = r;
	    r = ::sqrt(globalX2*globalX2 + globalY2*globalY2);
	    if(r > rMax) rMax = r;

//Fill StripPtrVec 
	    stripPtr->end1.SetX(globalX1) ;
	    stripPtr->end1.SetY(globalY1) ;
	    stripPtr->end1.SetZ(mEEmcSmdParam.zPlane[iPlane]);
	    stripPtr->end2.SetX(globalX2) ;
	    stripPtr->end2.SetY(globalY2) ;
	    stripPtr->end2.SetZ(mEEmcSmdParam.zPlane[iPlane]);
            stripPtr->length = lengthCorr;

	    phi1 = stripPtr->end1.Phi();
	    phi2 = stripPtr->end2.Phi();
 
	    if(iSec !=  kEEmcSmdSectorIdPhiCrossPi - 1) {
              if(phi1 < phiMin) phiMin = phi1; 
              if(phi1 > phiMax) phiMax = phi1; 
              if(phi2 < phiMin) phiMin = phi2; 
              if(phi2 > phiMax) phiMax = phi2; 
            }
            else {
              if(phi1 > 0)  if(phi1 < phiMin) phiMin = phi1; 
	      if(phi1 < 0 ) if(phi1 > phiMax) phiMax = phi1; 
              if(phi2 > 0)  if(phi2 < phiMin) phiMin = phi2; 
              if(phi2 < 0)  if(phi2 > phiMax) phiMax = phi2; 
            }
            mEEmcSector[iUV][iSec].stripPtrVec.push_back(stripPtr);
          } // loop over iStrip

// Fill mEEmcSectors 
          mEEmcSector[iUV][iSec].sectorId = iSec+1;
          mEEmcSector[iUV][iSec].planeId = iPlane+1;
          mEEmcSector[iUV][iSec].phiMin = phiMin;
          mEEmcSector[iUV][iSec].phiMax = phiMax;
          mEEmcSector[iUV][iSec].rMin = rMin;
          mEEmcSector[iUV][iSec].rMax = rMax;

        } // if sector selected
      } // end of iUVSec loop 
    } // end of iUV loop
  } // end of iPlane loop


  // build revers mapping (iUV,iSec) --> iPlane
  memset(kEEmcSmdMap_iPlane,0,sizeof(kEEmcSmdMap_iPlane));
  for (int iPlane = 0; iPlane < kEEmcNumSmdPlanes; iPlane++) 
    for(int iSec=0; iSec<kEEmcNumSectors; iSec++) {
      int iuv=kEEmcSmdMapUV[iPlane][iSec];
      if(iuv<0 ) continue;
      assert(iuv<kEEmcNumSmdUVs);
      kEEmcSmdMap_iPlane[iuv][iSec]=iPlane;
    }

  buildStripPtrVector();

} // end of buildSmdGeom 
//===========================================================
//===========================================================

// build mStripPtrVector with getEEmcSector()
void EEmcSmdGeom::buildStripPtrVector() {
   StructEEmcStrip *dummyStripPtr = new StructEEmcStrip;
   *dummyStripPtr = initStrip();
   EEmcStripPtrVec stripPtrVec;
   EEmcStripPtrVecIter p;  
   for(int iSec = 0; iSec < kEEmcNumSectors; iSec++) {
      if(mIsSectorIn[iSec]) {
        for(int iUV = 0; iUV < kEEmcNumSmdUVs; iUV++) {
          stripPtrVec = getEEmcSector(iUV,iSec).stripPtrVec;
          p = stripPtrVec.begin();
          int PlaneId = getEEmcSector(iUV,iSec).planeId;
	  while(p !=stripPtrVec.end()) {
	     mStripPtrVector.push_back(*p);
	     p++;

	  } 
	  if(kEEmcSmdMapEdge[PlaneId-1][iSec]) {
               for(int i=0; i < kEEmcNumStrips - kEEmcNumEdgeStrips; i++)   
	          mStripPtrVector.push_back(dummyStripPtr);
	  } 
        }
      }
      else {
        for(int iUV = 0; iUV < kEEmcNumSmdUVs; iUV++) {
          for(int iStrip = 0; iStrip < kEEmcNumStrips; iStrip++) 
	     mStripPtrVector.push_back(dummyStripPtr);
        }
      }
   }
}

// set status of selected sectors
void EEmcSmdGeom::setSectors(const intVec sectorIdVec) {
       for(int iSec = 0; iSec< kEEmcNumSectors; iSec++) 
	                           mIsSectorIn[iSec] = false;
       for(unsigned int i = 0; i < sectorIdVec.size(); i++) { 
          for(int iSec = 0; iSec< kEEmcNumSectors; iSec++) {
            if (sectorIdVec[i] == iSec+1) mIsSectorIn[iSec] = true;
          }
       }				       
}

// instance and initialize a strip
StructEEmcStrip EEmcSmdGeom::initStrip() const {
    TVector3  zero(0,0,0);
    StructEEmcStrip strip; 
    strip.stripStructId.stripId = 0;
    strip.stripStructId.UVId = 0;
    strip.stripStructId.sectorId = 0;
    strip.stripStructId.planeId = 0;
    strip.end1 = zero;
    strip.end2 = zero;
    strip.length = 0.0;
    return strip;
}

// return index of a sector from a global point in a plane 
Int_t EEmcSmdGeom::getEEmcISec(const Int_t iPlane, 
		           const TVector3& point) const {
     Int_t indexSec = -1;
     float phiMin, phiMax, rMin, rMax;
     float phi = point.Phi();
     float r = ::sqrt(point.x()*point.x() + point.y()*point.y());

     for (int iSec = 0; iSec < kEEmcNumSectors; iSec++) {
       int iUV = kEEmcSmdMapUV[iPlane][iSec];
       if(iUV >= 0 && IsSectorIn(iSec)) {       
         phiMin = mEEmcSector[iUV][iSec].phiMin;  
         phiMax = mEEmcSector[iUV][iSec].phiMax;  
         rMin = mEEmcSector[iUV][iSec].rMin;  
         rMax = mEEmcSector[iUV][iSec].rMax;
         if(iSec !=  kEEmcSmdSectorIdPhiCrossPi - 1) {
           if (phi >= phiMin && phi < phiMax && r > rMin && r < rMax) {
              indexSec = iSec;
              break;
           }
         }
// sector9 between 165 deg (Min) and -165 deg (Max)
         else { 
           if(((phi > 0.0 && phi >= phiMin) || (phi < 0.0 && phi < phiMax))
                                                 && r > rMin && r < rMax){
               indexSec = iSec;
               break;
           }
         }
       }
     }
     return indexSec; 
}

// return a strip pointer from indices   
StructEEmcStrip* EEmcSmdGeom::getStripPtr(const Int_t iStrip, const Int_t iUV, const Int_t iSec) {
    int i = iStrip + iUV*kEEmcNumStrips + iSec*kEEmcNumStrips*kEEmcNumSmdUVs;
    return mStripPtrVector[i];
}

// return a strip pointer from indices   
const StructEEmcStrip* EEmcSmdGeom::getStripPtr(const Int_t iStrip, const Int_t iUV, const Int_t iSec) const {
    int i = iStrip + iUV*kEEmcNumStrips + iSec*kEEmcNumStrips*kEEmcNumSmdUVs;
    return mStripPtrVector[i];
}


//==================================================================
// get DCA strip pointer from a point  
// iPlane=[0,1,2] - experts only, changes meaning form sector to sector
const StructEEmcStrip* EEmcSmdGeom::getDcaStripPtr(const Int_t iPlane, 
               const Int_t iSec, const TVector3& point, Float_t* dca) const
{
  //    StructEEmcStrip* stripPtr;
  //    stripPtr = new StructEEmcStrip;
    *dca = 1000.0;
    int iStrip = -1;
    //$$$    int iUV;
    float x1,y1,x2,y2,mu,d;
    EEmcStripPtrVec stripPtrVec;  
    EEmcStripPtrVecIter p;  

    
    Int_t iUV = kEEmcSmdMapUV[iPlane][iSec]; // jcw 2/6/04 moved here from $$$
                                             // iUV may be used uninitialized
                                             // below, otherwise...
//  int iSec = getEEmcISec(iPlane, point);
    if(iSec >= 0 && IsSectorIn(iSec)) {
      //$$$iUV = kEEmcSmdMapUV[iPlane][iSec];
      stripPtrVec =  getEEmcSector(iUV,iSec).stripPtrVec;
      p =  stripPtrVec.begin();
      while(p != stripPtrVec.end()) {
	x1 = (*p)->end1.x();
	y1 = (*p)->end1.y();
	x2 = (*p)->end2.x();
	y2 = (*p)->end2.y();
	mu = -1.0/::sqrt((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) *
	  ((x2*y1-x1*y2)/fabs(x2*y1-x1*y2));
// distance d carries a sign
	d = ((y2-y1)*point.x() + (x1-x2)*point.y() + (x2*y1-x1*y2))*mu;
	
	if(fabs(d) < fabs(*dca)) {
	  *dca = d;
	  iStrip = (*p)->stripStructId.stripId - 1;
	}
	if(d < 0) break;
	p++;
      }
    }
    if(iStrip >=0) {
      //stripPtr = getStripPtr(iStrip,iUV,iSec);
      //return stripPtr;
      return getStripPtr(iStrip,iUV,iSec);
    }
    else {
      StructEEmcStrip *stripPtr = new StructEEmcStrip;
      (*stripPtr) = initStrip();
      //    std::cout << "NO dca strip found in plane (sector empty or not in)" << std::endl;  //silentium

      return stripPtr;
    }
}

//==================================================================
// get DCA strip pointer from a point  
// iPlane=[0,1,2] - experts only, changes meaning form sector to sector
const StructEEmcStrip* EEmcSmdGeom::getDcaStripPtr(const Int_t iPlane, 
		         const TVector3& point, Float_t* dca) const {
  int iSec = getEEmcISec(iPlane, point);
  return  getDcaStripPtr(iPlane, iSec, point, dca); 
}


//==================================================================
// get DCA strip pointer from a point  
// iUV=[0,1] , maps [U,V]
// Warn, this code does not handle well sector boundaries, it ignores possible tripple overlaps and uses only nominal U or V planes in a given sector
const StructEEmcStrip* EEmcSmdGeom::getDca2Strip(const Int_t iUV, 
					   const TVector3& point, Float_t* dca) const {
  assert(iUV>=0 || iUV<kEEmcNumSmdUVs); 
  float phiDeg=atan2(point.y(),point.x())/3.1316*180.;
  //printf("phiDeg=%.1f  \n",phiDeg);
  int iSec= ((int) ( 12.-(phiDeg-75.)/30.) )%12;
  assert(iSec>=0);
  assert( iSec<kEEmcNumSectors);
  int iPlane= kEEmcSmdMap_iPlane[iUV][iSec];// now find  mapping iUV --> iPlane
  assert(iPlane>=0 && iPlane<kEEmcNumSmdPlanes);
  return  getDcaStripPtr(iPlane, iSec, point, dca); 
}


//==================================================================
// match two strips  
  bool EEmcSmdGeom::matchStrips(const StructEEmcStripId &stripStructId1, 
		                      const StructEEmcStripId &stripStructId2,
			              Int_t nTolerance) const {
    bool match = false;
    if(stripStructId1.UVId == stripStructId2.UVId &&
       stripStructId1.sectorId == stripStructId2.sectorId) {
         if((TMath::Abs(stripStructId1.stripId - stripStructId2.stripId) <= nTolerance))
           match = true;
    }
    return match;
}



TVector3  EEmcSmdGeom::getstripEnd(const StructEEmcStrip &strip, 
		                                    const Int_t endId) const {
      TVector3 end;
      if(endId == 1) end = strip.end1;
      else end = strip.end2;

      return end;
}

// methods of printout
/// printout global geometry parameters
void EEmcSmdGeom::printGeom(ostream& os) const {
  os << "------EEmcSmdGeom::printGeom()------" << endl;
  os << " " << "z[3]          = " 
     << " " << getEEmcSmdParam().zPlane[0] 
     << " " << getEEmcSmdParam().zPlane[1] 
     << " " << getEEmcSmdParam().zPlane[2] << endl;
  os << " " << "rOffset[3]    = "
     << " " << getEEmcSmdParam().rOffset[0]
     << " " << getEEmcSmdParam().rOffset[1]
     << " " << getEEmcSmdParam().rOffset[2] << endl;
  os << " " << "stripWidth    = "
     << " " << getEEmcSmdParam().stripWidth << endl;
  os << "---------------------------------------" << endl;
}

/// printout sector-specific geometry parameters
void EEmcSmdGeom::printSector(const StructEEmcSmdSector sector, ostream& os) const {
  float delPhi;
  int iUV = kEEmcSmdMapUV[sector.planeId-1][sector.sectorId-1];
  delPhi = (sector.phiMax - sector.phiMin)/degree;
  if(sector.sectorId == kEEmcSmdSectorIdPhiCrossPi) 
	                     delPhi = 2*pi/degree + delPhi;
  
  os << "------EEmcSmdGeom::printSector()------" << endl;
  os << kEEmcSmdUVChar[iUV] << " Sector:  sectorId, planeId, nStrips      = " 
     << " " << sector.sectorId 
     << " " << sector.planeId 
     << " " << sector.stripPtrVec.size() << endl;
  os << "           phiMin, phiMax, delPhi  = " 
     << " " << sector.phiMin/degree 
     << " " << sector.phiMax/degree
     << " " << delPhi << endl;
  os << "           rMin, rMax delR         = "  
     << " " << sector.rMin 
     << " " << sector.rMax 
     << " " << sector.rMax - sector.rMin << endl;
  os << "------------------------------------" << endl;
}

/// printout strip-specific geometry parameters
void EEmcSmdGeom::printStrip(const StructEEmcStrip strip, ostream& os) const {
  char UVChar; 	
  if(strip.stripStructId.sectorId == 0) UVChar = 'X';
  else
    UVChar = kEEmcSmdUVChar[strip.stripStructId.UVId - 1];

  os << "------EEmcSmdGeom::printStrip()------" << endl;

    os << "Strip:  sectorId, planeUV, stripId, planeId    = "
       << " " << strip.stripStructId.sectorId
       << " " << UVChar 
       << " " << strip.stripStructId.stripId
       << " " << strip.stripStructId.planeId << endl;
    os << "        x1, y1, x2, y2, z     = "
       << " " << strip.end1.x() 
       << " " << strip.end1.y() 
       << " " << strip.end2.x() 
       << " " << strip.end2.y() 
       << " " << strip.end2.z() << endl; 
    os << "        phi1, phi2, length    = "
       << " " << strip.end1.Phi()/degree
       << " " << strip.end2.Phi()/degree
       << " " << strip.length << endl;
    os << "------------------------------------" << endl;
}

/// printout stripStructId
void EEmcSmdGeom::printStripId(const StructEEmcStripId stripStructId, ostream& os) const {
  char UVChar; 	
  if(stripStructId.sectorId == 0) UVChar = 'X';
  else
    UVChar = kEEmcSmdUVChar[stripStructId.UVId - 1];

  os << "------EEmcSmdGeom::printStripId()------" << endl;
    os << "Strip:  sectorId, stripId, planeId    = "
       << " " << stripStructId.sectorId
              << UVChar 
       << " " << stripStructId.stripId
       << " " << stripStructId.planeId << endl;
    os << "------------------------------------" << endl;
}



// Move to StEEmcSmdGeom
#if 0
// printout delPhi and centerPhi used in ITTF
void EEmcSmdGeom::printSectorPhis(const Int_t iPlane, const Int_t iSec,
                                                              ostream& os ) {
  int iUV;
  iUV = kEEmcSmdMapUV[iPlane][iSec];

  os << "------EEmcSmdGeom::printPhis()------" << endl;
  os << " planeId = " << iPlane + 1 << " sectorId = " << iSec + 1 << endl;
  if(iUV >= 0) 
    os << " " <<  kEEmcSmdUVChar[iUV] << " Sector" << endl; 
  else  
    os << " Empty" << endl; 
  os << " delPhi = " << getEEmcSmdDelPhi(iPlane, iSec)/degree <<
    " " << "centerPhi = " << getEEmcSmdCenterPhi(iPlane, iSec)/degree 
     << endl;
  
}
#endif


/////////////////////////////////////////////////////////////////////////////

//
// Function(s) to return the position of the crossing of two strips, given
//   either the sectorID and strip ID's, or the StructEEmcStrips.
//

// Wrapper function when we don't have the actual strips, just sector and
//   strip ID's.
TVector3 EEmcSmdGeom::getIntersection ( Int_t sector, Int_t uId, Int_t vId ) const {

  Int_t nU = getNStrips( sector, 0 );
  Int_t nV = getNStrips( sector, 1 );
  if ( uId >= nU || vId >= nV ) {
    LOG_DEBUG<<"::getIntersection(...) passed invalid strip ID sector="<<sector<<" uId="<<uId<<" vId="<<vId<<std::endl;
    return TVector3(1.,1.,-999.0);
  }

  return getIntersection ( getStripPtr( uId, 0, sector ),
			   getStripPtr( vId, 1, sector ) );

}


TVector3 EEmcSmdGeom::getIntersection ( Int_t sector, Int_t uId, Int_t vId, const TVector3 &vert ) const {

  Int_t nU = getNStrips( sector, 0 );
  Int_t nV = getNStrips( sector, 1 );
  if ( uId >= nU || vId >= nV ) {
    LOG_DEBUG<<"::getIntersection(...) passed invalid strip ID sector="<<sector<<" uId="<<uId<<" vId="<<vId<<std::endl;
    return TVector3(1.,1.,-999.0);
  }

  return getIntersection ( getStripPtr( uId, 0, sector ),
			   getStripPtr( vId, 1, sector ), vert );

}


/*********************************************************/ 
TVector3 EEmcSmdGeom::getIntersection ( const StructEEmcStrip *u,
					const StructEEmcStrip *v,
					const TVector3 &vertex ) const
{
  // The strips are arranged sensibly, so that the ID's and the
  // widths of the strips basically tell us the position of the
  // crossing point.  However, we need to know a few things:
                                                                             
  Int_t uSectorId = u -> stripStructId.sectorId;   // This would be easier if
  Int_t vSectorId = v -> stripStructId.sectorId;   // we were dealing with a
  //Int_t uPlaneId  = u -> stripStructId.planeId;  // class instead of a struct
  //Int_t vPlaneId  = v -> stripStructId.planeId;  //

  Int_t uId = u -> stripStructId.stripId;
  Int_t vId = v -> stripStructId.stripId;

  // Get vectors pointing to the start of the u and v strips in question,
  //   as well as the ends.  NOTE: We pass uId - 1 to getStripPtr, because
  //   that routine expects the c++ _index_... (the convention in this
  //   class is that quantities beginning with an "i" correspond to a
  //   c++ index numbered from 0, rather than a fortran index numbered
  //   from 1...)
  TVector3 u0 = getStripPtr ( uId-1, 0, uSectorId-1 ) -> end1;
  TVector3 v0 = getStripPtr ( vId-1, 1, vSectorId-1 ) -> end1;
  

  TVector3 uF = getStripPtr ( uId-1, 0, uSectorId-1 ) -> end2;
  TVector3 vF = getStripPtr ( vId-1, 1, vSectorId-1 ) -> end2;
  

  TVector3 u0p=(u0-vertex);  // uOp is vector from vertex to beginning of first strip from vertex 
  TVector3 uFp=(uF-vertex);  // uFp is vector from vertex to end of first strip from vertex
  TVector3 v0p=(v0-vertex);  // vOp is vector from vertex to beginning of first strip from vertex
  TVector3 vFp=(vF-vertex);  // vFp is vector from vertex to end of first strip from vertex

  ///////////////////////////////////////////////////
  //          U then V!  return point on V strip
  ///////////////////////////////////////////////////

  // N is normal vector to plane
  TVector3 Nv = u0p.Cross(uFp);

  // use components of N to establish D in plane of form A(x-x0)+B(y-y0)+C(z-z0)+D=0, use vertex for point
  Float_t Dv = (Nv.X()*vertex.X() + Nv.Y()*vertex.Y() + Nv.Z()*vertex.Z());

  // scale determines how far along vector from v0 to vF to go to get point 
  // of intersection with plane.  If it's <0 or >1, then the point of 
  // intersection is off the physical size of the strip.  Return an error
  // value
  Float_t scale_numerv = (Nv.X()*v0.X() + Nv.Y()*v0.Y() + Nv.Z()*v0.Z() - Dv);
  Float_t scale_denomv = (Nv.X()*(v0.X()-vF.X()) + Nv.Y()*(v0.Y()-vF.Y()) + Nv.Z()*(v0.Z()-vF.Z()));
  Float_t scalev=scale_numerv/scale_denomv;
  if (scalev < 0 || scalev > 1) {
    TVector3 ErrorVector(1.,1.,-999.);
    LOG_DEBUG<<GetName()<<"::getIntersection( ) passed non-intersecting SMD strips " << *u << " " << *v << endm;
    return ErrorVector;
  }  
  
  
  // Rv is the vector to the final point from the origin ON V STRIP
  TVector3 Rv = (v0 + scalev*(vF-v0));


    
  // S is the vector that would go from vertex to end point
  // TVector3 S = (R-z0);
  
  ///////////////////////////////////////////////
  //       V THEN U! returns point on u strip
  ///////////////////////////////////////////////
  // N is normal vector to plane
  TVector3 Nu = v0p.Cross(vFp);
  
  // use components of N to establish D in plane of form A(x-x0)+B(y-y0)+C(z-z0)+D=0, use vertex for point
  Float_t Du = (Nu.X()*vertex.X() + Nu.Y()*vertex.Y() + Nu.Z()*vertex.Z());
  
  // scale determines how far along vector from v0 to vF to go to get point of intersection with plane
  Float_t scale_numeru = (Nu.X()*u0.X() + Nu.Y()*u0.Y() + Nu.Z()*u0.Z() - Du);
  Float_t scale_denomu = (Nu.X()*(u0.X()-uF.X()) + Nu.Y()*(u0.Y()-uF.Y()) + Nu.Z()*(u0.Z()-uF.Z()));
  Float_t scaleu=scale_numeru/scale_denomu;
  if (scaleu < 0 || scaleu > 1) {
    TVector3 ErrorVector(1.,1.,-999.);
    return ErrorVector;
  }
  
  
  // Ru is the vector to the final point from the origin ON U STRIP
  TVector3 Ru = (u0 + scaleu*(uF-u0));
  
  
  // gives us vector direction FROM THE VERTEX
  TVector3 R = ((Rv+Ru)*0.5);
  
  return R;
                                                                                                                                                  


}

/*********************************************************/ 


TVector3 EEmcSmdGeom::getIntersection ( const StructEEmcStrip *u,
					const StructEEmcStrip *v
					) const {

  return getIntersection( u, v, TVector3(0,0,0) );

}


/****************************************************************************/

// output strip for smd strips
ostream& operator<<(ostream &os, const StructEEmcStrip &strip)
{
  const Char_t *nameUV[]={"U","V"};
  TString stripname="";
  if ( strip.stripStructId.sectorId < 10 ) stripname+="0"; stripname+=strip.stripStructId.sectorId;
  stripname += nameUV[ strip.stripStructId.UVId-1 ];
  if ( strip.stripStructId.stripId < 10 ) stripname+="0";
  if ( strip.stripStructId.stripId < 100 ) stripname+="0";
  stripname+=strip.stripStructId.stripId;
  os << stripname << " depth=" << strip.stripStructId.planeId;
  return os;
}


/////////////////////////////////////////////////////////////////////////////
/*
 * $Log: EEmcSmdGeom.cxx,v $
 * Revision 1.17  2015/07/21 17:10:02  jeromel
 * Wrong init of TVector3 zero corrected
 *
 * Revision 1.16  2010/08/26 22:48:55  ogrebeny
 * Improved constness
 *
 * Revision 1.15  2009/08/25 18:33:11  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.14  2008/02/17 17:37:32  balewski
 * demote warning about strips intersecting beyond modle to DEBUG
 *
 * Revision 1.13  2007/07/12 19:30:15  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.12  2007/05/01 20:22:25  jwebb
 * Added error handling to EEmcSmdGeom::getIntersection().
 *
 * Revision 1.11  2007/02/17 01:29:05  balewski
 * less printout
 *
 * Revision 1.10  2007/02/02 02:11:11  balewski
 * simplification of  EEmcSmdGeom::getDca2Strip(..)
 *
 * Revision 1.9  2007/02/01 20:33:46  wzhang
 * dca initialized in getDcaStripPtr()
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
 * Revision 1.4  2004/06/03 23:01:06  jwebb
 * Fixed memory leak reported by Bob.  Fixed another memory leak.
 *
 * Revision 1.3  2004/02/06 22:33:08  jwebb
 * Moved statement to fix warning.
 *
 * Revision 1.2  2004/01/29 16:37:25  jwebb
 * Removed dependence on StMaker.h and PhysicalConstants.h.  Should be fully
 * decoupled from Star environment now.
 *
 * Revision 1.1  2004/01/29 15:26:10  jwebb
 * The StEEmcSmdGeom class was split into two classes.  All StRoot-independent
 * code has been moved to EEmcSmdGeom.  TVector3 replaces StThreeVectorD in
 * all function calls in EEmcSmdGeom.  StThreeVectorD wrappers are provided
 * in StEEmcSmdGeom, for integration into Star framework.
 *
 *
 *****************************************************************************
 * Log: StEEmcSmdGeom.cxx,v 
 * Revision 1.8  2004/01/12 14:34:09  wzhang
 * Corrected the usage of EEmcStripPtrVecIter
 *
 * Revision 1.7  2003/12/05 00:06:10  jwebb
 * Member function added to return a vector pointing to the intersection of
 * two strips.
 *
 * Revision 1.6  2003/10/15 15:26:08  wzhang
 * improved and reorganized
 *
 * Revision 1.5  2003/09/02 17:57:56  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2003/08/22 15:14:26  wzhang
 * Added ClassImp and method stripEnd
 *
 * Revision 1.3  2003/06/11 18:58:19  wzhang
 * added geometry methods for StiEEmc
 *
 * Revision 1.2  2003/04/04 15:33:32  wzhang
 * included EEmcGeomDefs.h & improved codes
 *
 * Revision 1.1  2003/03/28 15:49:59  balewski
 * first
 * 
 *
 *******************************************************************/
