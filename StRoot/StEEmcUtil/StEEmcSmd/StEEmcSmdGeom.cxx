
/*******************************************************************
 *
 * $Id: StEEmcSmdGeom.cxx,v 1.5 2003/09/02 17:57:56 perev Exp $
 *
 * Author: Wei-Ming Zhang 
 *****************************************************************
 *
 * Description: Geometry definitions and utility class for EEMC-SMD
 *
 *****************************************************************
 *
 * $Log: StEEmcSmdGeom.cxx,v $
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
/*! \class StEEmcSmdGeom
    \author Wei-Ming Zhang

*/
/* In the following codes, the strip index etaId goes from 1 to 288, the
 * module index moduleId goes from 1 to 12. The loop index iMod for modules
 * also goes from 1 to 12. This follows the convention in codes of EEgeo.cxx.
 * While, the loop index iEta for strips goes from 0-287.
*/ 
#include "StEEmcSmdGeom.h"
#include "PhysicalConstants.h"
#include "StMaker.h"

/// defaulty constructor
ClassImp(StEEmcSmdGeom)

StEEmcSmdGeom::StEEmcSmdGeom(){ 
  for(int iMod = 1; iMod <= kEEmcNumSectors; iMod++) mIsSectorIn[iMod-1] = true;
};
/// default empty destructor
StEEmcSmdGeom::~StEEmcSmdGeom(){ 
  delete sInstance;
  sInstance = 0;
};

/// Initialize geometry class from database file
void StEEmcSmdGeom::init(){ 
	initGeomFromFile(); 
}

StEEmcSmdGeom* StEEmcSmdGeom::sInstance = 0;	
// all setctors
StEEmcSmdGeom* StEEmcSmdGeom::instance() {	
    if(!sInstance){
        sInstance = new StEEmcSmdGeom();
	sInstance->init();
   }
   return sInstance;
}     

// selected sectors
StEEmcSmdGeom* StEEmcSmdGeom::instance(intVec sIdVec) {	
    if(!sInstance){
        sInstance = new StEEmcSmdGeom();
        sInstance->setSectors(sIdVec);    
	sInstance->init();
   }
   return sInstance;
}     

/// initialize EEMC-SMD parameters from strip_geom database file
void StEEmcSmdGeom::initGeomFromFile(const Char_t* InputFile){

// Fill EEMC-SMD parameters first 
   for(int iPlane=0; iPlane<kEEmcNumSmdPlanes; iPlane++) 
	         mEEmcSmdParam.rOffset[iPlane] = kEEmcSmdROffset[iPlane];

   cout << "StEEmcSmdGeom: loading dBase from " << InputFile << endl;

// following codes are modifiled from EEgeo.cxx in eeDisp/code
// open file
  FILE *fd=0;
  fd=fopen(InputFile,"r");
  assert(fd);

// variables for reading file
  const int mx=1000;
  int num;
  char  buf[mx];
  float x0[kEEmcNumStrips];
  float y1[kEEmcNumStrips];
  float y2[kEEmcNumStrips]; 
  float length[kEEmcNumStrips];
  float x0Edge[kEEmcNumEdgeStrips];
  float y1Edge[kEEmcNumEdgeStrips];
  float y2Edge[kEEmcNumEdgeStrips];
  float lengthEdge[kEEmcNumEdgeStrips];
  
// read header information
  next1: // skip header
  fgets(buf,mx,fd);
  //  printf("buf= %s \n",buf);
  if(buf[0]=='#') goto next1;

  for (int i = 0; i < kEEmcNumStrips; i++) {
    fscanf(fd,"%d%f%f%f%f",     &num, &x0[i], &y1[i], &y2[i], &length[i]);
    //    if(i < 3||i>280) cout << num  << " " << x0[i << " " 
    //            << y1[i] << " "  << y2[i] << " " << length[i] << endl;
  }
 fgets(buf,mx,fd);
 nextlEdge:
  fgets(buf,mx,fd);
  //  printf("buf= %s\n",buf);
  if(buf[0]=='#') goto nextlEdge;

  for (int i = 0; i < kEEmcNumEdgeStrips; i++) {
    fscanf(fd,"%d%f%f%f%f",     &num, &x0Edge[i], &y1Edge[i], &y2Edge[i], &lengthEdge[i]);
    //     if(i < 3||i>280) cout << num << " " << x0Edge[i] << " " 
    //       << y1Edge[i] << " " <<  y2Edge[i] << " " << lengthEdge[i] << endl;
    
  }

// Loop over 3 planes. there are 4 U and 4 V layers filled in each plane.   
// Local coordinates are transformed to global coordinates by rotations.
// Local coordinates: X axis is perpendicular to strip, Y is parallel.
// Rotation angle for U/V layer is -/+( [45 - (moduleId - 1)*30] deg.
// After rotation, x and y should be swapped for V layer   

  float   delPhi = 2*pi/degree/kEEmcNumSectors;
  float vPhiRotation[kEEmcNumSectors], uPhiRotation[kEEmcNumSectors];
  
  for(int iPhi=1; iPhi<=kEEmcNumSectors; iPhi++){
    int iMod = iPhi%kEEmcNumSectors;
    if(iMod == 0) iMod = kEEmcNumSectors; // iMod goes from 1 to 12

// 12 rotation angles for coordinates transform from local to global for U & V 
    uPhiRotation[iMod-1]=(-45.0 + iMod*delPhi)*degree; 
    vPhiRotation[iMod-1]=( 45.0 - iMod*delPhi)*degree; 
  }
  
  mEEmcSmdParam.stripWidth = 0.5;

  for (int iPlane = 1; iPlane <= kEEmcNumSmdPlanes; iPlane++) {
    float globalX1, globalY1, globalX2, globalY2;
    float x0Corr, y1Corr, y2Corr, lengthCorr; 
    float phi1, phi2, phiMin, phiMax;
    float r, rMin, rMax;
    mEEmcSmdParam.zPlane[iPlane-1] = kEEmcZSMD + 
	             (iPlane - kEEmcNumSmdPlanes + 1) * kEEmcSmdZPlaneShift ;

// V layer
    
    for(int iPhi=iPlane; iPhi<=kEEmcNumSectors; iPhi=iPhi+kEEmcNumSmdPlanes) {
      int iMod = iPhi%kEEmcNumSectors;
      if(iMod == 0) iMod = kEEmcNumSectors;
      if(IsSectorIn(iMod)) {
          
        EEmcStripPtrVec vStripPtrVec;

        rMin = 1000.0;
        rMax = 0.0;
        phiMin = pi; 
        phiMax = -pi;
     
        for (int iEta = 0; iEta < kEEmcNumStrips; iEta++) {   
          if(kEEmcSmdMapEdge[iPlane-1][iMod-1] && iEta > kEEmcNumEdgeStrips-1) 
		                                                    break;      
          StructEEmcStripId  stripId;
 	  if(iMod != 0) stripId.moduleId = iMod;
	  else stripId.moduleId = kEEmcNumSectors;
	  stripId.layerId = 2;
	  stripId.etaId = iEta + 1;
	  stripId.planeId = iPlane;
	  StructEEmcStrip*    stripPtr = new StructEEmcStrip;
	  stripPtr->stripId = stripId; 

	  x0Corr = x0[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	  y2Corr = y2[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
          if(kEEmcSmdMapEdge[iPlane-1][iMod-1]) {      
	    y1Corr = y1Edge[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	    lengthCorr = lengthEdge[iEta];
          }
	  else { 
	    y1Corr = y1[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	    lengthCorr = length[iEta];
          }

          globalX1 = y1Corr*cos(vPhiRotation[iMod-1]) - 
	                              x0Corr*sin(vPhiRotation[iMod-1]);
          globalY1 = x0Corr*cos(vPhiRotation[iMod-1]) + 
			              y1Corr*sin(vPhiRotation[iMod-1]);
          globalX2 = y2Corr*cos(vPhiRotation[iMod-1]) - 
	                              x0Corr*sin(vPhiRotation[iMod-1]);
          globalY2 = x0Corr*cos(vPhiRotation[iMod-1]) + 
		                      y2Corr*sin(vPhiRotation[iMod-1]);

	  r = ::sqrt(globalX1*globalX1 + globalY1*globalY1);
	  if(r < rMin) rMin = r;
	  r = ::sqrt(globalX2*globalX2 + globalY2*globalY2);
	  if(r > rMax) rMax = r;

//Fill vStripPtrVec 
	  stripPtr->end1.setX(globalX1) ;
	  stripPtr->end1.setY(globalY1) ;
	  stripPtr->end1.setZ(mEEmcSmdParam.zPlane[iPlane-1]);
	  stripPtr->end2.setX(globalX2) ;
	  stripPtr->end2.setY(globalY2) ;
	  stripPtr->end2.setZ(mEEmcSmdParam.zPlane[iPlane-1]);
          stripPtr->length = lengthCorr;

	  phi1 = stripPtr->end1.phi();
	  phi2 = stripPtr->end2.phi();
 
	  if(iMod !=  kEEmcSmdModuleIdPhiCrossPi) {
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
          vStripPtrVec.push_back(stripPtr);
        } // loop over iEta

// Fill V Modules
        mEEmcVModule[iMod-1].moduleId = iMod;
        mEEmcVModule[iMod-1].planeId = iPlane;
        mEEmcVModule[iMod-1].phiMin = phiMin;
        mEEmcVModule[iMod-1].phiMax = phiMax;
        mEEmcVModule[iMod-1].rMin = rMin;
        mEEmcVModule[iMod-1].rMax = rMax;
        mEEmcVModule[iMod-1].stripPtrVec = vStripPtrVec;

      } // if selected sectors
    } // end of V iPhi

// U layer
    for(int iPhi=iPlane+1;iPhi<=kEEmcNumSectors+1;iPhi=iPhi+kEEmcNumSmdPlanes) {
      int iMod = iPhi%kEEmcNumSectors;
      if(iMod == 0) iMod = kEEmcNumSectors;
      if(IsSectorIn(iMod)) {
        EEmcStripPtrVec uStripPtrVec;

        rMin = 1000.0;
        rMax = 0.0; 
        phiMin = pi; 
        phiMax = -pi;

        for (int iEta = 0; iEta < kEEmcNumStrips; iEta++) {   
          if(kEEmcSmdMapEdge[iPlane-1][iMod-1] && iEta > kEEmcNumEdgeStrips-1) 
		                                                      break; 
          StructEEmcStripId  stripId;
	  if(iMod != 0) stripId.moduleId = iMod;
	  else stripId.moduleId = kEEmcNumSectors;
	  stripId.layerId = 1; 
	  stripId.etaId = iEta + 1;
	  stripId.planeId = iPlane;
	  StructEEmcStrip*    stripPtr = new StructEEmcStrip;
	  stripPtr->stripId = stripId; 

	  x0Corr = x0[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	  y2Corr = y2[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	    
          if(kEEmcSmdMapEdge[iPlane-1][iMod-1]) {      
	    y1Corr = y1Edge[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	    lengthCorr = lengthEdge[iEta];
	  }
	  else { 
	    y1Corr = y1[iEta] - kEEmcSmdROffset[iPlane - 1]*::sqrt(0.5);
	    lengthCorr = length[iEta];
          }
	   
          globalX1 = x0Corr*cos(uPhiRotation[iMod-1])+ 
		                     y1Corr*sin(uPhiRotation[iMod-1]);
          globalY1 = y1Corr*cos(uPhiRotation[iMod-1]) - 
		                     x0Corr*sin(uPhiRotation[iMod-1]);
          globalX2 = x0Corr*cos(uPhiRotation[iMod-1]) + 
		                     y2Corr*sin(uPhiRotation[iMod-1]);
          globalY2 = y2Corr*cos(uPhiRotation[iMod-1]) - 
		                     x0Corr*sin(uPhiRotation[iMod-1]);

	  r = ::sqrt(globalX1*globalX1 + globalY1*globalY1);
	  if(r < rMin) rMin = r;
	  r = ::sqrt(globalX2*globalX2 + globalY2*globalY2);
	  if(r > rMax) rMax = r;

//Fill uStripPtrVec 
	  stripPtr->end1.setX(globalX1) ;
	  stripPtr->end1.setY(globalY1) ;
	  stripPtr->end1.setZ(mEEmcSmdParam.zPlane[iPlane-1]);
	  stripPtr->end2.setX(globalX2) ;
	  stripPtr->end2.setY(globalY2) ;
	  stripPtr->end2.setZ(mEEmcSmdParam.zPlane[iPlane-1]);
          stripPtr->length = lengthCorr;

	  phi1 = stripPtr->end1.phi();
	  phi2 = stripPtr->end2.phi();

	  if(iMod !=  kEEmcSmdModuleIdPhiCrossPi) {
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

          uStripPtrVec.push_back(stripPtr);
        } // loop over iEta 

//Fill U Modules	 
        mEEmcUModule[iMod-1].moduleId = iMod;
        mEEmcUModule[iMod-1].planeId = iPlane;
        mEEmcUModule[iMod-1].phiMin = phiMin;
        mEEmcUModule[iMod-1].phiMax = phiMax;
        mEEmcUModule[iMod-1].rMin = rMin;
        mEEmcUModule[iMod-1].rMax = rMax;
        mEEmcUModule[iMod-1].stripPtrVec = uStripPtrVec;

      } // if selected sectors
    } // end of U iPhi
  } // end of iPlane
} // end of initGeomFromFile
     

  // set status of selectoed sectors
  void StEEmcSmdGeom::setSectors(const intVec sIdVec) {
       for(int iMod = 1; iMod<= kEEmcNumSectors; iMod++) mIsSectorIn[iMod - 1] = false;
       for(unsigned int i = 0; i < sIdVec.size(); i++) { 
          for(int iMod = 1; iMod<= kEEmcNumSectors; iMod++) {
            if (sIdVec[i] == iMod) mIsSectorIn[iMod - 1] = true;
          }
       }				       
}

  // instance and initialize a strip
  StructEEmcStrip StEEmcSmdGeom::EEmcInitStrip() {
    StThreeVectorD  zero = 0.0;
    StructEEmcStrip strip; 
    strip.stripId.moduleId = 0;
    strip.stripId.layerId = 0;
    strip.stripId.etaId = 0;
    strip.stripId.planeId = 0;
    strip.end1 = zero;
    strip.end2 = zero;
    strip.length = 0.0;
    return strip;
}
  // return module Id of a global point 
  Int_t StEEmcSmdGeom::EEmcModuleId(const Int_t planeId, const StThreeVectorD& point) const {
     Int_t moduleId = 0;
     float phiMin, phiMax, rMin, rMax;
     float phi = point.phi();
     float r = ::sqrt(point.x()*point.x() + point.y()*point.y());

     for (int iMod = 1; iMod <= kEEmcNumSectors; iMod++) {
       if(kEEmcSmdMapUV[planeId-1][iMod-1] > 0 && IsSectorIn(iMod)) {       
         if(kEEmcSmdMapUV[planeId-1][iMod-1] == 1) {
           phiMin = mEEmcUModule[iMod-1].phiMin;  
           phiMax = mEEmcUModule[iMod-1].phiMax;  
           rMin = mEEmcUModule[iMod-1].rMin;  
           rMax = mEEmcUModule[iMod-1].rMax;
         }
         else if(kEEmcSmdMapUV[planeId-1][iMod-1] == 2) { 
           phiMin = mEEmcVModule[iMod-1].phiMin;  
           phiMax = mEEmcVModule[iMod-1].phiMax;  
           rMin = mEEmcVModule[iMod-1].rMin;  
           rMax = mEEmcVModule[iMod-1].rMax; 
         } 
         if(iMod !=  kEEmcSmdModuleIdPhiCrossPi) {
           if (phi >= phiMin && phi < phiMax && r > rMin && r < rMax) {
              moduleId = iMod;
              break;
           }
         }
// module9 between 165 deg (Min) and -165 deg (Max)
         else { 
           if(((phi > 0.0 && phi >= phiMin) || (phi < 0.0 && phi < phiMax))
                                                 && r > rMin && r < rMax){
               moduleId = iMod;
               break;
           }
         }
       }
     }
     return moduleId; 
}
// return  DCA strip from a global point  
  StructEEmcStrip StEEmcSmdGeom::EEmcStrip(const Int_t layerId, 
		         const Int_t moduleId, const Int_t etaId) {
    EEmcStripPtrVec stripPtrVec;
    if(layerId == 1)
       stripPtrVec = EEmcUModule(moduleId).stripPtrVec;
    else if(layerId == 2)
       stripPtrVec = EEmcVModule(moduleId).stripPtrVec;
    else 
       cout << "EEmcStrip: Wrong moduleId!" << endl;
    
    StructEEmcStrip strip = *stripPtrVec[etaId-1];
    return strip;
}

// return  DCA strip from a global point  
  StructEEmcStrip StEEmcSmdGeom::EEmcStrip(const Int_t planeId, const StThreeVectorD& point, Float_t* dca) {
    StructEEmcStrip strip = EEmcInitStrip();
    EEmcStripPtrVec stripPtrVec;
    EEmcStripPtrVecIter p;  
    float x1,y1,x2,y2,mu,d;
    int moduleId = EEmcModuleId(planeId, point);
    if(moduleId > 0 && IsSectorIn(moduleId)) {
       int layerId = kEEmcSmdMapUV[planeId-1][moduleId-1];
       if(layerId == 1) {
         stripPtrVec = EEmcUModule(moduleId).stripPtrVec;  
       }
       else 
         stripPtrVec = EEmcVModule(moduleId).stripPtrVec;  

       p = stripPtrVec.begin();  
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
	     strip = *(*p);
          }
	  if(d < 0) break;
	  p++;
      }
    }
    return strip;
}

  StThreeVectorD  StEEmcSmdGeom::stripEnd(const StructEEmcStrip strip, 
		                                    const Int_t endId) {
      StThreeVectorD end;
      if(endId == 1) 
	      end = strip.end1;
      else
              end = strip.end2;

      return end;
}

// match two strips  
  bool StEEmcSmdGeom::EEmcMatchStrips(const StructEEmcStripId stripId1, 
		                      const StructEEmcStripId stripId2,
			              Int_t nTolerance) {
    bool match = false;
    if(stripId1.layerId == stripId2.layerId &&
       stripId1.moduleId == stripId2.moduleId) {
         if((abs(stripId1.etaId - stripId2.etaId) <= nTolerance))
           match = true;
    }
    return match;
}

// return delta_phi of a sector including empty sector 
float StEEmcSmdGeom::EEmcSmdDelPhi(const Int_t planeId, const Int_t sectorId) 
{
     float delPhi, phiMin, phiMax;
     int layerId, antiClockLayerId, clockLayerId;
     int antiClockSectorId, clockSectorId;

     layerId = kEEmcSmdMapUV[planeId - 1][sectorId - 1];
     if(layerId == 1) {
           phiMin = EEmcUModule(sectorId).phiMin;
           phiMax = EEmcUModule(sectorId).phiMax;
     }
     else if(layerId == 2) { 
           phiMin = EEmcVModule(sectorId).phiMin;
           phiMax = EEmcVModule(sectorId).phiMax;
     }
     else {  // emtry sector
// find phiMax in anticlockwise adjacent sector 
	  if(sectorId != 1) antiClockSectorId = sectorId - 1;
	  else antiClockSectorId = 12; 
	  antiClockLayerId = kEEmcSmdMapUV[planeId - 1][antiClockSectorId - 1];
	  if(antiClockLayerId == 1) 
                phiMax = EEmcUModule(antiClockSectorId).phiMin;
	  else if(antiClockLayerId == 2)  
                phiMax = EEmcVModule(antiClockSectorId).phiMin;
// find phiMin in clockwise adjacent sector 
	  if(sectorId != 12) clockSectorId = sectorId + 1;
	  else clockSectorId = 1; 
	  clockLayerId = kEEmcSmdMapUV[planeId - 1][clockSectorId - 1];
	  if(clockLayerId == 1) phiMin=EEmcUModule(clockSectorId).phiMax;
	  else if(clockLayerId == 2)  phiMin=EEmcVModule(clockSectorId).phiMax;
     }

     delPhi = phiMax - phiMin;
     if(sectorId  == kEEmcSmdModuleIdPhiCrossPi) delPhi = 2*pi + delPhi; 

     return delPhi;
}

// return center phi of a sector including empty sector 
float StEEmcSmdGeom::EEmcSmdCenterPhi(const Int_t planeId, const Int_t sectorId)
{
     float centerPhi, phiMin, phiMax;
     int layerId, antiClockLayerId, clockLayerId;
     int antiClockSectorId, clockSectorId;

     layerId = kEEmcSmdMapUV[planeId - 1][sectorId - 1];
     if(layerId == 1) {
           phiMin = EEmcUModule(sectorId).phiMin;
           phiMax = EEmcUModule(sectorId).phiMax;
     }
     else if(layerId == 2) { 
           phiMin = EEmcVModule(sectorId).phiMin;
           phiMax = EEmcVModule(sectorId).phiMax;
     }
     else {  // emtry sector
// find phiMax in anticlockwise adjacent sector 
	  if(sectorId != 1) antiClockSectorId = sectorId - 1;
	  else antiClockSectorId = 12; 
	  antiClockLayerId = kEEmcSmdMapUV[planeId - 1][antiClockSectorId - 1];
	  if(antiClockLayerId == 1) 
                phiMax = EEmcUModule(antiClockSectorId).phiMin;
	  else if(antiClockLayerId == 2)  
                phiMax = EEmcVModule(antiClockSectorId).phiMin;
// find phiMin in clockwise adjacent sector 
	  if(sectorId != 12) clockSectorId = sectorId + 1;
	  else clockSectorId = 1; 
	  clockLayerId = kEEmcSmdMapUV[planeId - 1][clockSectorId - 1];
	  if(clockLayerId == 1) phiMin=EEmcUModule(clockSectorId).phiMax;
	  else if(clockLayerId == 2)  phiMin=EEmcVModule(clockSectorId).phiMax;
     }

     centerPhi = 0.5*(phiMax + phiMin);
     if(sectorId  == kEEmcSmdModuleIdPhiCrossPi) {
	     if(centerPhi <= 0) centerPhi= M_PI + centerPhi; 
	     else centerPhi = M_PI - centerPhi; 
     }

     return centerPhi;
}

// print delPhi and centerPhi used in ITTF
void StEEmcSmdGeom::printSectorPhis(const Int_t planeId, const Int_t sectorId,
                                                              ostream& os ) {
  int layerId;
  layerId = kEEmcSmdMapUV[planeId-1][sectorId-1];

  os << "------StEEmcSmdGeom::printPhis()------" << endl;
  os << " planeId = " << planeId << " sectorId = " << sectorId << endl;
  if(layerId != 0) 
    os << " " <<  kEEmcSmdLayerChar[layerId -1] << " Module" << endl; 
  else  
    os << " Empty" << endl; 
  os << " delPhi = " << EEmcSmdDelPhi(planeId, sectorId)/degree <<
        " " << "centerPhi = " << EEmcSmdCenterPhi(planeId, sectorId)/degree 
     << endl;
}

/// print global geometry parameters
void StEEmcSmdGeom::printGeom(ostream& os) const {
  os << "------StEEmcSmdGeom::printGeom()------" << endl;
  os << " " << "z[3]          = " 
     << " " << mEEmcSmdParam.zPlane[0] 
     << " " << mEEmcSmdParam.zPlane[1] 
     << " " << mEEmcSmdParam.zPlane[2] << endl;
  os << " " << "rOffset[3]    = "
     << " " << mEEmcSmdParam.rOffset[0]
     << " " << mEEmcSmdParam.rOffset[1]
     << " " << mEEmcSmdParam.rOffset[2] << endl;
  os << " " << "stripWidth    = "
     << " " << mEEmcSmdParam.stripWidth << endl;
  os << "---------------------------------------" << endl;
}

/// print module-specific geometry parameters
void StEEmcSmdGeom::printModule(const StructEEmcSmdModule module, ostream& os) const {
  float delPhi;
  int layerId = kEEmcSmdMapUV[module.planeId-1][module.moduleId-1];
  delPhi = (module.phiMax - module.phiMin)/degree;
  if(module.moduleId == kEEmcSmdModuleIdPhiCrossPi) 
	                     delPhi = 2*pi/degree + delPhi;
  
  os << "------StEEmcSmdGeom::printModule()------" << endl;
  os << kEEmcSmdLayerChar[layerId -1] << " Module:  Id, plane, nStrips      = " 
     << " " << module.moduleId 
     << " " << module.planeId 
     << " " << module.stripPtrVec.size() << endl;
  os << "           phiMin, phiMax, delPhi  = " 
     << " " << module.phiMin/degree 
     << " " << module.phiMax/degree
     << " " << delPhi << endl;
  os << "           rMin, rMax delR         = "  
     << " " << module.rMin 
     << " " << module.rMax 
     << " " << module.rMax - module.rMin << endl;
  os << "------------------------------------" << endl;
}

/// print strip-specific geometry parameters
void StEEmcSmdGeom::printStrip(const StructEEmcStrip strip, ostream& os) const {
  char layerChar; 	
  if(strip.stripId.moduleId == 0) layerChar = 'X';
  else
    layerChar = kEEmcSmdLayerChar[strip.stripId.layerId - 1];

  os << "------StEEmcSmdGeom::printStrip()------" << endl;

    os << "Strip:  module, eta, plane    = "
       << " " << layerChar 
              << strip.stripId.moduleId
       << " " << strip.stripId.etaId
       << " " << strip.stripId.planeId << endl;
    os << "        x1, y1, x2, y2, z     = "
       << " " << strip.end1.x() 
       << " " << strip.end1.y() 
       << " " << strip.end2.x() 
       << " " << strip.end2.y() 
       << " " << strip.end2.z() << endl; 
    os << "        phi1, phi2, length    = "
       << " " << strip.end1.phi()/degree
       << " " << strip.end2.phi()/degree
       << " " << strip.length << endl;
    os << "------------------------------------" << endl;
}

/// print stripId
void StEEmcSmdGeom::printStripId(const StructEEmcStripId stripId, ostream& os) const {
  char layerChar; 	
  if(stripId.moduleId == 0) layerChar = 'X';
  else
    layerChar = kEEmcSmdLayerChar[stripId.layerId - 1];

  os << "------StEEmcSmdGeom::printStripId()------" << endl;
    os << "Strip:  module, eta, plane    = "
       << " " << stripId.moduleId
              << layerChar 
       << " " << stripId.etaId
       << " " << stripId.planeId << endl;
    os << "------------------------------------" << endl;
}
