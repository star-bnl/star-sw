
/*******************************************************************
 *
 * $Id: StEEmcSmdGeom.cxx,v 1.1 2003/03/28 15:49:59 balewski Exp $
 *
 * Author: Wei-Ming Zhang 
 *****************************************************************
 *
 * Description: Geometry definitions and utility class for EEMC-SMD
 *
 *****************************************************************
 *
 * $Log: StEEmcSmdGeom.cxx,v $
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

const float zPlane[3] = {278.327,279.542,280.757};
const float rOffset[3]={1.850,0.925,0.0};
const int   mapUV[3][12] = {{2,1,0,2,1,0,2,1,0,2,1,0},
	                    {0,2,1,0,2,1,0,2,1,0,2,1},
			    {1,0,2,1,0,2,1,0,2,1,0,2}};

/// defaulty constructor
StEEmcSmdGeom::StEEmcSmdGeom(){ 
	for(int iMod = 1; iMod <= nMods; iMod++) mIsSectorIn[iMod-1] = true;
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
   for(int iPlane=0; iPlane<3; iPlane++) 
	         mEEmcSmdParam.rOffset[iPlane] = rOffset[iPlane];

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
  float x0[nStrips];
  float y1[nStrips];
  float y2[nStrips]; 
  float length[nStrips];
  float x0Edge[nStrips];
  float y1Edge[nStrips];
  float y2Edge[nStrips];
  float lengthEdge[nStrips];
  
// read header information
  next1: // skip header
  fgets(buf,mx,fd);
  //  printf("buf= %s \n",buf);
  if(buf[0]=='#') goto next1;

  for (int i = 0; i < nStrips; i++) {
    fscanf(fd,"%d%f%f%f%f",     &num, &x0[i], &y1[i], &y2[i], &length[i]);
    //    if(i < 3||i>280) cout << num  << " " << x0[i << " " 
    //            << y1[i] << " "  << y2[i] << " " << length[i] << endl;
  }
 fgets(buf,mx,fd);
 nextlEdge:
  fgets(buf,mx,fd);
  //  printf("buf= %s\n",buf);
  if(buf[0]=='#') goto nextlEdge;

  for (int i = 0; i < nStrips-5; i++) {
    fscanf(fd,"%d%f%f%f%f",     &num, &x0Edge[i], &y1Edge[i], &y2Edge[i], &lengthEdge[i]);
    //     if(i < 3||i>280) cout << num << " " << x0Edge[i] << " " 
    //       << y1Edge[i] << " " <<  y2Edge[i] << " " << lengthEdge[i] << endl;
    
  }

// Loop over 3 planes. there are 4 U and 4 V layers filled in each plane.   
// Local coordinates are transformed to global coordinates by rotations.
// Local coordinates: X axis is perpendicular to strip, Y is parallel.
// Rotation angle for U/V layer is -/+( [45 - (moduleId - 1)*30] deg.
// After rotation, x and y should be swapped for V layer   

  int   delPhi = 30;
  float vPhiRad[nMods], uPhiRad[nMods];
  
  for(int iPhi=1; iPhi<=nMods; iPhi++){
    int iMod = iPhi%nMods;
    int phiDeg=iMod*delPhi;
    if(iMod == 0) iMod = 12;    // To match the order of EEMC sectors

    vPhiRad[iMod-1]=( 45.0 - phiDeg)*degree;  // 12 rotation angles for V
    uPhiRad[iMod-1]=(-45.0 + phiDeg)*degree;  // 12 rotation angles for U
  }
  
  for (int iPlane = 1; iPlane <= 3; iPlane++) {
    float gX1, gY1, gX2, gY2;
    float x0Corr, y1Corr, y2Corr, lengthCorr; 
    float gPhi1, gPhi2, gPhiMin, gPhiMax;
    float gR, gRMin, gRMax;

    mEEmcSmdParam.zPlane[iPlane-1] = zPlane[iPlane-1];

// V layer
    
    for(int iPhi=iPlane; iPhi<=nMods; iPhi=iPhi+3) {
      int iMod = iPhi%nMods;
      if(iMod == 0) iMod = 12;
      if(IsSectorIn(iMod)) {
          
        EEmcStripPtrVec vStripPtrVec;

        gRMin = 300.0;
        gRMax = 0.0;
        gPhiMin = pi; 
        gPhiMax = -pi;
     
        for (int iEta = 0; iEta < nStrips; iEta++) {   
          if(iPlane==1 && (iMod==4 || iMod==10)&& iEta > 282) break;      
          StructEEmcStripId  stripId;
 	  if(iMod != 0) stripId.moduleId = iMod;
	  else stripId.moduleId = 12;
	  stripId.layerId = 2;
	  stripId.etaId = iEta + 1;
	  stripId.planeId = iPlane;
	  StructEEmcStrip*    stripPtr = new StructEEmcStrip;
	  stripPtr->stripId = stripId; 

	  x0Corr = x0[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	  y2Corr = y2[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	  if(iPlane == 1 && (iMod == 4 || iMod == 10)) { 
	    y1Corr = y1Edge[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	    lengthCorr = lengthEdge[iEta];
          }
	  else { 
	    y1Corr = y1[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	    lengthCorr = length[iEta];
          }

          gX1 = y1Corr*cos(vPhiRad[iMod-1]) - 
	                              x0Corr*sin(vPhiRad[iMod-1]);
          gY1 = x0Corr*cos(vPhiRad[iMod-1]) + 
			              y1Corr*sin(vPhiRad[iMod-1]);
          gX2 = y2Corr*cos(vPhiRad[iMod-1]) - 
	                              x0Corr*sin(vPhiRad[iMod-1]);
          gY2 = x0Corr*cos(vPhiRad[iMod-1]) + 
		                      y2Corr*sin(vPhiRad[iMod-1]);

	  gR = sqrt(gX1*gX1 + gY1*gY1);
	  if(gR < gRMin) gRMin = gR;
	  gR = sqrt(gX2*gX2 + gY2*gY2);
	  if(gR > gRMax) gRMax = gR;

//Fill vStripPtrVec 
	  stripPtr->end1.setX(gX1) ;
	  stripPtr->end1.setY(gY1) ;
	  stripPtr->end1.setZ(zPlane[iPlane-1]);
	  stripPtr->end2.setX(gX2) ;
	  stripPtr->end2.setY(gY2) ;
	  stripPtr->end2.setZ(zPlane[iPlane-1]);
          stripPtr->length = lengthCorr;

	  gPhi1 = stripPtr->end1.phi();
	  gPhi2 = stripPtr->end2.phi();
 
	  if(iMod != 9) {
            if(gPhi1 < gPhiMin) gPhiMin = gPhi1; 
            if(gPhi1 > gPhiMax) gPhiMax = gPhi1; 
            if(gPhi2 < gPhiMin) gPhiMin = gPhi2; 
            if(gPhi2 > gPhiMax) gPhiMax = gPhi2; 
          }
          else {
            if(gPhi1 > 0)  if(gPhi1 < gPhiMin) gPhiMin = gPhi1; 
	    if(gPhi1 < 0 )  if(gPhi1 > gPhiMax) gPhiMax = gPhi1; 
            if(gPhi2 > 0)  if(gPhi2 < gPhiMin) gPhiMin = gPhi2; 
            if(gPhi2 < 0)  if(gPhi2 > gPhiMax) gPhiMax = gPhi2; 
          }
          vStripPtrVec.push_back(stripPtr);
        } // loop over iEta

// Fill V Modules
        mEEmcVModule[iMod-1].moduleId = iMod;
        mEEmcVModule[iMod-1].planeId = iPlane;
        mEEmcVModule[iMod-1].phiMin = gPhiMin;
        mEEmcVModule[iMod-1].phiMax = gPhiMax;
        mEEmcVModule[iMod-1].rMin = gRMin;
        mEEmcVModule[iMod-1].rMax = gRMax;
        mEEmcVModule[iMod-1].stripPtrVec = vStripPtrVec;

      } // if selected sectors
    } // end of V iPhi

// U layer
    for(int iPhi=iPlane+1; iPhi<=13;iPhi=iPhi+3) {
      int iMod = iPhi%nMods;
      if(iMod == 0) iMod = 12;
      if(IsSectorIn(iMod)) {
        EEmcStripPtrVec uStripPtrVec;

        gRMin = 300.0;
        gRMax = 0.0; 
        gPhiMin = pi; 
        gPhiMax = -pi;

        for (int iEta = 0; iEta < nStrips; iEta++) {   
	  if(iPlane==2 && (iMod==3 || iMod==9) && iEta > 282) break; 
          StructEEmcStripId  stripId;
	  if(iMod != 0) stripId.moduleId = iMod;
	  else stripId.moduleId = 12;
	  stripId.layerId = 1; 
	  stripId.etaId = iEta + 1;
	  stripId.planeId = iPlane;
	  StructEEmcStrip*    stripPtr = new StructEEmcStrip;
	  stripPtr->stripId = stripId; 

	  x0Corr = x0[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	  y2Corr = y2[iEta] - rOffset[iPlane - 1]*sqrt(0.5);

	  if(iPlane == 2 && (iMod == 3 || iMod == 9)) {
	    y1Corr = y1Edge[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	    lengthCorr = lengthEdge[iEta];
	  }
	  else { 
	    y1Corr = y1[iEta] - rOffset[iPlane - 1]*sqrt(0.5);
	    lengthCorr = length[iEta];
          }
	   
          gX1 = x0Corr*cos(uPhiRad[iMod-1])+ 
		                     y1Corr*sin(uPhiRad[iMod-1]);
          gY1 = y1Corr*cos(uPhiRad[iMod-1]) - 
		                     x0Corr*sin(uPhiRad[iMod-1]);
          gX2 = x0Corr*cos(uPhiRad[iMod-1]) + 
		                     y2Corr*sin(uPhiRad[iMod-1]);
          gY2 = y2Corr*cos(uPhiRad[iMod-1]) - 
		                     x0Corr*sin(uPhiRad[iMod-1]);

	  gR = sqrt(gX1*gX1 + gY1*gY1);
	  if(gR < gRMin) gRMin = gR;
	  gR = sqrt(gX2*gX2 + gY2*gY2);
	  if(gR > gRMax) gRMax = gR;

//Fill uStripPtrVec 
	  stripPtr->end1.setX(gX1) ;
	  stripPtr->end1.setY(gY1) ;
	  stripPtr->end1.setZ(zPlane[iPlane-1]);
	  stripPtr->end2.setX(gX2) ;
	  stripPtr->end2.setY(gY2) ;
	  stripPtr->end2.setZ(zPlane[iPlane-1]);
          stripPtr->length = lengthCorr;

	  gPhi1 = stripPtr->end1.phi();
	  gPhi2 = stripPtr->end2.phi();

	  if(iMod != 9) {
            if(gPhi1 < gPhiMin) gPhiMin = gPhi1; 
            if(gPhi1 > gPhiMax) gPhiMax = gPhi1; 
            if(gPhi2 < gPhiMin) gPhiMin = gPhi2; 
            if(gPhi2 > gPhiMax) gPhiMax = gPhi2; 
          }
          else {
            if(gPhi1 > 0)  if(gPhi1 < gPhiMin) gPhiMin = gPhi1; 
	    if(gPhi1 < 0 )  if(gPhi1 > gPhiMax) gPhiMax = gPhi1; 
            if(gPhi2 > 0)  if(gPhi2 < gPhiMin) gPhiMin = gPhi2; 
            if(gPhi2 < 0)  if(gPhi2 > gPhiMax) gPhiMax = gPhi2; 
          }

          uStripPtrVec.push_back(stripPtr);
        } // loop over iEta 

//Fill U Modules	 
        mEEmcUModule[iMod-1].moduleId = iMod;
        mEEmcUModule[iMod-1].planeId = iPlane;
        mEEmcUModule[iMod-1].phiMin = gPhiMin;
        mEEmcUModule[iMod-1].phiMax = gPhiMax;
        mEEmcUModule[iMod-1].rMin = gRMin;
        mEEmcUModule[iMod-1].rMax = gRMax;
        mEEmcUModule[iMod-1].stripPtrVec = uStripPtrVec;

      } // if selected sectors
    } // end of U iPhi
  } // end of iPlane
} // end of initGeomFromFile
     

  // set status of selectoed sectors
  void StEEmcSmdGeom::setSectors(const intVec sIdVec) {
       for(int iMod = 1; iMod<= nMods; iMod++) mIsSectorIn[iMod - 1] = false;
       for(unsigned int i = 0; i < sIdVec.size(); i++) { 
          for(int iMod = 1; iMod<= nMods; iMod++) {
            if (sIdVec[i] == iMod) mIsSectorIn[iMod - 1] = true;
          }
       }				       
}

  // instance and initialize a strip
  StructEEmcStrip StEEmcSmdGeom::EEmcInitStrip() {
    StThreeVectorD  zero = 0.0;
    StructEEmcStrip strip; 
    strip.stripId.moduleId = 0;
    strip.stripId.layerId = 3;
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
     float r = sqrt(point.x()*point.x() + point.y()*point.y());

     for (int iMod = 1; iMod <= nMods; iMod++) {
       if(mapUV[planeId-1][iMod-1] > 0 && IsSectorIn(iMod)) {       
         if(mapUV[planeId-1][iMod-1] == 1) {
           phiMin = mEEmcUModule[iMod-1].phiMin;  
           phiMax = mEEmcUModule[iMod-1].phiMax;  
           rMin = mEEmcUModule[iMod-1].rMin;  
           rMax = mEEmcUModule[iMod-1].rMax;
         }
         else if(mapUV[planeId-1][iMod-1] == 2) { 
           phiMin = mEEmcVModule[iMod-1].phiMin;  
           phiMax = mEEmcVModule[iMod-1].phiMax;  
           rMin = mEEmcVModule[iMod-1].rMin;  
           rMax = mEEmcVModule[iMod-1].rMax; 
         } 
         if(iMod != 9) {
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
  StructEEmcStrip StEEmcSmdGeom::EEmcStrip(const Int_t planeId, const StThreeVectorD& point, Float_t* dca) {
    StructEEmcStrip strip = EEmcInitStrip();
    EEmcStripPtrVec stripPtrVec;
    EEmcStripPtrVecIter p;  
    float x1,y1,x2,y2,mu,d;
    int moduleId = EEmcModuleId(planeId, point);
    if(moduleId > 0 && IsSectorIn(moduleId)) {
       int layerId = mapUV[planeId-1][moduleId-1];
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
	  mu = -1.0/sqrt((y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)) *
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

// match two strips  
  bool StEEmcSmdGeom::EEmcMatchStrips(const StructEEmcStripId stripId1, 
		                      const StructEEmcStripId stripId2,
			              Int_t nTolerance) {
    bool match = false;
    if(stripId1.layerId == stripId2.layerId &&
       stripId1.moduleId == stripId2.moduleId) {
         if((fabs(stripId1.etaId - stripId2.etaId) <= nTolerance))
           match = true;
    }
    return match;
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
  os << "---------------------------------------" << endl;
}

/// print module-specific geometry parameters
void StEEmcSmdGeom::printModule(const StructEEmcSmdModule module, ostream& os) const {
  char layer[2];
  layer[0] = 'U';
  layer[1] = 'V';
  float delPhi;
  int layerId = mapUV[module.planeId-1][module.moduleId-1];
  delPhi = (module.phiMax - module.phiMin)/degree;
  if(module.moduleId == 9) delPhi = 360.0 + delPhi;
  
  os << "------StEEmcSmdGeom::printModule()------" << endl;
  os << layer[layerId - 1] << " Module:  Id, plane, nStrips      = " 
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
  char layer[3];
  layer[0] = 'U';
  layer[1] = 'V';
  layer[2] = 'X';
  os << "------StEEmcSmdGeom::printStrip()------" << endl;

    os << "Strip:  module, eta, plane    = "
       << " " << layer[strip.stripId.layerId-1] 
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
  char layer[2];
  layer[0] = 'U';
  layer[1] = 'V';
  os << "------StEEmcSmdGeom::printStripId()------" << endl;
    os << "Strip:  module, eta, plane    = "
       << " " << stripId.moduleId
              << layer[stripId.layerId-1]
       << " " << stripId.etaId
       << " " << stripId.planeId << endl;
    os << "------------------------------------" << endl;
}
