

/*****************************************************************************
 *
 * $Id: EEmcSmdGeom.cxx,v 1.3 2004/02/06 22:33:08 jwebb Exp $
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
 *****************************************************************************
 *
 * $Log: EEmcSmdGeom.cxx,v $
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
 *****************************************************************************/

/*! \class StEEmcSmdGeom
    \author Wei-Ming Zhang

*/
#include "EEmcSmdGeom.h"
#include "EEmcStripGeom.h"

// decouple from StarClassLibrary
//#include "PhysicalConstants.h"  
#ifndef HEP_SYSTEM_OF_UNITS_H
#include <math.h>
static const double     radian      = 1.;
static const double     pi          = M_PI; // from <math.h>
static const double     degree      = (M_PI/180.0)*radian;
#endif

/// defaulty constructor
ClassImp(EEmcSmdGeom)

EEmcSmdGeom::EEmcSmdGeom(){ 
  for(int iSec = 0; iSec < kEEmcNumSectors; iSec++) mIsSectorIn[iSec] = true;
};
/// default empty destructor
EEmcSmdGeom::~EEmcSmdGeom(){ 
  delete sInstance;
  sInstance = 0;
};

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

  buildStripPtrVector();

} // end of buildSmdGeom 


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
StructEEmcStrip EEmcSmdGeom::initStrip() {
    TVector3  zero = 0.0;
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
StructEEmcStrip* EEmcSmdGeom::getStripPtr(const Int_t iStrip,  
		               const Int_t iUV, const Int_t iSec) {
//    StructEEmcStrip *stripPtr = new StructEEmcStrip;
    int i = iStrip + iUV*kEEmcNumStrips 
		             + iSec*kEEmcNumStrips*kEEmcNumSmdUVs;
    return  mStripPtrVector[i];
}

// get DCA strip pointer from a point  
StructEEmcStrip* EEmcSmdGeom::getDcaStripPtr(const Int_t iPlane, 
               const Int_t iSec, const TVector3& point, Float_t* dca) {
    StructEEmcStrip* stripPtr;
    stripPtr = new StructEEmcStrip;
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
      stripPtr = getStripPtr(iStrip,iUV,iSec);
      return stripPtr;
    }
    else {
      *stripPtr = initStrip();
      std::cout << "NO dca strip found in plane (sector empty or not in)" 
                                                                 << std::endl;
      return stripPtr;
    }
}

// get DCA strip pointer from a point  
StructEEmcStrip* EEmcSmdGeom::getDcaStripPtr(const Int_t iPlane, 
		         const TVector3& point, Float_t* dca) {
    StructEEmcStrip* stripPtr;
    stripPtr = new StructEEmcStrip;

    int iSec = getEEmcISec(iPlane, point);
    stripPtr = getDcaStripPtr(iPlane, iSec, point, dca); 
    return stripPtr;
}

// match two strips  
  bool EEmcSmdGeom::matchStrips(const StructEEmcStripId stripStructId1, 
		                      const StructEEmcStripId stripStructId2,
			              Int_t nTolerance) {
    bool match = false;
    if(stripStructId1.UVId == stripStructId2.UVId &&
       stripStructId1.sectorId == stripStructId2.sectorId) {
         if((abs(stripStructId1.stripId - stripStructId2.stripId) <= nTolerance))
           match = true;
    }
    return match;
}


#if 0

// methods for ITTF
// return phiMax and phiMax of a sector including empty sector 
pairD EEmcSmdGeom::getEEmcSmdPhiMinMax(const Int_t iPlane, const Int_t iSec) 
{
     pairD phiMinMax;
     float phiMin, phiMax;
//     int iUV, antiClockUVId, clockUVId;
     int iUV, antiClockIUV, clockIUV;
     int antiClockISec, clockISec;

     iUV = kEEmcSmdMapUV[iPlane][iSec];

     if(iUV >= 0) {
           phiMin = getEEmcSector(iUV, iSec).phiMin;
           phiMax = getEEmcSector(iUV, iSec).phiMax;
     }
     else {  // emtry sector
// find phiMax in anticlockwise adjacent sector 
	  if(iSec != 0) antiClockISec = iSec - 1;
	  else antiClockISec = 11; 
	  antiClockIUV = kEEmcSmdMapUV[iPlane][antiClockISec];

          phiMax = getEEmcSector(antiClockIUV,antiClockISec).phiMin;
// find phiMin in clockwise adjacent sector 
	  if(iSec != 11) clockISec = iSec + 1;
	  else clockISec = 0; 
	  clockIUV = kEEmcSmdMapUV[iPlane][clockISec];
	  phiMin=getEEmcSector(clockIUV,clockISec).phiMax;
     }
     phiMinMax.first = (double) phiMin;	     
     phiMinMax.second = (double) phiMax;	     

     return phiMinMax;
}

// return delta_phi of a sector including empty sector 
float EEmcSmdGeom::getEEmcSmdDelPhi(const Int_t iPlane, const Int_t iSec) 
{
     float delPhi;
     pairD  phiMinMax = getEEmcSmdPhiMinMax(iPlane, iSec);
     delPhi = (float) phiMinMax.second - (float)phiMinMax.first;
     if(iSec  == kEEmcSmdSectorIdPhiCrossPi - 1) delPhi = 2*pi + delPhi; 

     return delPhi;
}

// return center phi of a sector including empty sector 
float EEmcSmdGeom::getEEmcSmdCenterPhi(const Int_t iPlane, 
		                               const Int_t iSec)
{
     float centerPhi;
     pairD phiMinMax = getEEmcSmdPhiMinMax(iPlane, iSec);
     centerPhi = 0.5*((float) phiMinMax.second + (float)phiMinMax.first);
     if(iSec  == kEEmcSmdSectorIdPhiCrossPi - 1) {
	     if(centerPhi <= 0) centerPhi= M_PI + centerPhi; 
	     else centerPhi = M_PI - centerPhi; 
     }

     return centerPhi;
}
#endif




TVector3  EEmcSmdGeom::getstripEnd(const StructEEmcStrip strip, 
		                                    const Int_t endId) {
      TVector3 end;
      if(endId == 1) end = strip.end1;
      else end = strip.end2;

      return end;
}

// methods of printout
/// printout global geometry parameters
void EEmcSmdGeom::printGeom(ostream& os) const {
  os << "------EEmcSmdGeom::printGeom()------" << std::endl;
  os << " " << "z[3]          = " 
     << " " << getEEmcSmdParam().zPlane[0] 
     << " " << getEEmcSmdParam().zPlane[1] 
     << " " << getEEmcSmdParam().zPlane[2] << std::endl;
  os << " " << "rOffset[3]    = "
     << " " << getEEmcSmdParam().rOffset[0]
     << " " << getEEmcSmdParam().rOffset[1]
     << " " << getEEmcSmdParam().rOffset[2] << std::endl;
  os << " " << "stripWidth    = "
     << " " << getEEmcSmdParam().stripWidth << std::endl;
  os << "---------------------------------------" << std::endl;
}

/// printout sector-specific geometry parameters
void EEmcSmdGeom::printSector(const StructEEmcSmdSector sector, ostream& os) const {
  float delPhi;
  int iUV = kEEmcSmdMapUV[sector.planeId-1][sector.sectorId-1];
  delPhi = (sector.phiMax - sector.phiMin)/degree;
  if(sector.sectorId == kEEmcSmdSectorIdPhiCrossPi) 
	                     delPhi = 2*pi/degree + delPhi;
  
  os << "------EEmcSmdGeom::printSector()------" << std::endl;
  os << kEEmcSmdUVChar[iUV] << " Sector:  sectorId, planeId, nStrips      = " 
     << " " << sector.sectorId 
     << " " << sector.planeId 
     << " " << sector.stripPtrVec.size() << std::endl;
  os << "           phiMin, phiMax, delPhi  = " 
     << " " << sector.phiMin/degree 
     << " " << sector.phiMax/degree
     << " " << delPhi << std::endl;
  os << "           rMin, rMax delR         = "  
     << " " << sector.rMin 
     << " " << sector.rMax 
     << " " << sector.rMax - sector.rMin << std::endl;
  os << "------------------------------------" << std::endl;
}

/// printout strip-specific geometry parameters
void EEmcSmdGeom::printStrip(const StructEEmcStrip strip, ostream& os) const {
  char UVChar; 	
  if(strip.stripStructId.sectorId == 0) UVChar = 'X';
  else
    UVChar = kEEmcSmdUVChar[strip.stripStructId.UVId - 1];

  os << "------EEmcSmdGeom::printStrip()------" << std::endl;

    os << "Strip:  sectorId, planeUV, stripId, planeId    = "
       << " " << strip.stripStructId.sectorId
       << " " << UVChar 
       << " " << strip.stripStructId.stripId
       << " " << strip.stripStructId.planeId << std::endl;
    os << "        x1, y1, x2, y2, z     = "
       << " " << strip.end1.x() 
       << " " << strip.end1.y() 
       << " " << strip.end2.x() 
       << " " << strip.end2.y() 
       << " " << strip.end2.z() << std::endl; 
    os << "        phi1, phi2, length    = "
       << " " << strip.end1.Phi()/degree
       << " " << strip.end2.Phi()/degree
       << " " << strip.length << std::endl;
    os << "------------------------------------" << std::endl;
}

/// printout stripStructId
void EEmcSmdGeom::printStripId(const StructEEmcStripId stripStructId, ostream& os) const {
  char UVChar; 	
  if(stripStructId.sectorId == 0) UVChar = 'X';
  else
    UVChar = kEEmcSmdUVChar[stripStructId.UVId - 1];

  os << "------EEmcSmdGeom::printStripId()------" << std::endl;
    os << "Strip:  sectorId, stripId, planeId    = "
       << " " << stripStructId.sectorId
              << UVChar 
       << " " << stripStructId.stripId
       << " " << stripStructId.planeId << std::endl;
    os << "------------------------------------" << std::endl;
}



// Move to StEEmcSmdGeom
#if 0
// printout delPhi and centerPhi used in ITTF
void EEmcSmdGeom::printSectorPhis(const Int_t iPlane, const Int_t iSec,
                                                              ostream& os ) {
  int iUV;
  iUV = kEEmcSmdMapUV[iPlane][iSec];

  os << "------EEmcSmdGeom::printPhis()------" << std::endl;
  os << " planeId = " << iPlane + 1 << " sectorId = " << iSec + 1 << std::endl;
  if(iUV >= 0) 
    os << " " <<  kEEmcSmdUVChar[iUV] << " Sector" << std::endl; 
  else  
    os << " Empty" << std::endl; 
  os << " delPhi = " << getEEmcSmdDelPhi(iPlane, iSec)/degree <<
    " " << "centerPhi = " << getEEmcSmdCenterPhi(iPlane, iSec)/degree 
     << std::endl;
  
}
#endif


/////////////////////////////////////////////////////////////////////////////

//
// Function(s) to return the position of the crossing of two strips, given
//   either the sectorID and strip ID's, or the StructEEmcStrips.
//

// Wrapper function when we don't have the actual strips, just sector and
//   strip ID's.
TVector3 EEmcSmdGeom::getIntersection ( Int_t sector, Int_t uId, Int_t vId ) {

  return getIntersection ( getStripPtr( uId, 0, sector ),
			   getStripPtr( vId, 1, sector ) );

}

TVector3 EEmcSmdGeom::getIntersection ( StructEEmcStrip *u,
						StructEEmcStrip *v ) {


 

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

  TVector3 uu = (uF - u0);
  TVector3 vv = (vF - v0);

  Double_t u_x = uu * TVector3(1.,0.,0.);
  Double_t u_y = uu * TVector3(0.,1.,0.);
  Double_t v_x = vv * TVector3(1.,0.,0.); // This is wrong somehow...
  Double_t v_y = vv * TVector3(0.,1.,0.);

  Double_t d_x = ( v0 - u0 ) * TVector3(1.,0.,0.);
  Double_t d_y = ( v0 - u0 ) * TVector3(0.,1.,0.);

  // Calculate the positions along u and v (i.e. a*u and b*v) where the
  //   two vectors are at closest approach.  Since the two SMD planes
  //   are nominally parallel, we can get away with doing this in only
  //   two dimensions.  Just work out the algebra for the condition
  //   u0 + au = v0 + bv, for the x and y components to obtain:


  Double_t b = ( d_y * u_x / u_y - d_x ) / ( v_x - v_y * u_x / u_y );
  Double_t a = ( d_x + b * v_x ) / u_x;

  TVector3 uCross = u0 + a*uu;
  TVector3 vCross = v0 + b*vv;

  TVector3 uv;

  for ( Int_t i = 0; i < 3; i++ ) 
    uv[i] = 0.5 * ( uCross[i] + vCross[i] );

  return uv;

}

/////////////////////////////////////////////////////////////////////////////
/*******************************************************************
 *
 * Id: StEEmcSmdGeom.cxx,v 1.8 2004/01/12 14:34:09 wzhang Exp 
 *
 * Author: Wei-Ming Zhang 
 *****************************************************************
 *
 * Description: Geometry definitions and utility class for EEMC-SMD
 *
 *****************************************************************
 *
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
