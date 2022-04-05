/*******************************************************************
 *
 * $Id: StTofGeometry.cxx,v 1.13 2018/01/03 21:07:14 smirnovd Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Geometry definitions and utility class for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofGeometry.cxx,v $
 * Revision 1.13  2018/01/03 21:07:14  smirnovd
 * StTofGeometry: Get rid of unsupported xdf and outdated ctf tables
 *
 * Revision 1.12  2007/04/17 23:01:51  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.11  2005/05/13 19:37:36  perev
 * Defence agains 1/0 added
 *
 * Revision 1.10  2004/06/10 15:53:50  dongx
 * -head file "StHelixD.hh" included
 * -simplify the macro definition
 *
 * Revision 1.9  2004/06/09 21:26:32  dongx
 * add function projTrayVector(..) to increase the matching speed
 *
 * Revision 1.8  2004/04/28 18:58:16  dongx
 * fix a bug on the mis-matching by the opposite direction tracks
 *
 * Revision 1.7  2003/09/13 00:42:32  perev
 * XDF obsolete + small fixes
 *
 * Revision 1.6  2003/09/02 17:59:10  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2003/08/06 23:45:36  geurts
 * refer to tofPathLength.hh for function definitions
 *
 * Revision 1.4  2003/04/15 03:24:17  geurts
 * many, many changes:
 * . updated and extended StructSlatHit, introduced tofSlatHitVector and Iterator
 * . generalize (2+1)-D slat model from 3 layers to n layers, default n=5
 * . introduced new member functions which identify 3x3 and 5x5 neighbours
 * . introduced SetDebug() option.
 * . minor updates in mTofParam parameters
 *
 * Revision 1.3  2002/01/23 17:30:45  geurts
 * removed const
 *
 * Revision 1.2  2002/01/22 01:55:05  geurts
 * STAR dBase access routine, bugfixes and doxygenized
 *
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
//! Time-of-Flight Geometry Utilities
/*! \class StTofGeometry
    \author Frank Geurts

    A package of geometry routines for the STAR Time-of-Flight detector.
    Methods include initialization from STAR dbase or local table (xdf-file)
    and extrapolation routines of tracks to the TOF slats.<p>

    Also mappings for DAQ-id to Slat-Id and vice-versa are available
    as through member functions.

    To do:
    <ul>
    <li>Move away from ctf-tables by either introducing a local version or
    discarding xdf use all together.</li>
    <li>Calculate default values for slats from GSTAR parameters.<li>
    <li>Include ADC-TDC mapping to correct for wrong cabling</li>
    </ul>

*/
//VP#include "St_XDFFile.h"
#include "StMessMgr.h"
#include "St_DataSetIter.h"
#include "PhysicalConstants.h"
#include "tables/St_tofSlatGeom_Table.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StMaker.h"
#include "StTofUtil/StTofGeometry.h"
#include "StTofUtil/tofPathLength.hh"

#include <utility>
using std::pair;


/// defaulty constructor
StTofGeometry::StTofGeometry(){
  mDebug = false;
}

/// default empty destructor
StTofGeometry::~StTofGeometry(){ /* nope */};



/// initialize geometry class from XDF file and set-up DAQ/Slat mappings
void StTofGeometry::init(){
  initGeomFromXdf();
  initDaqMap();
}



/// initialize geometry class from STAR dBase and set-up DAQ/Slat mappings
void StTofGeometry::init(StMaker* maker){
  initGeomFromDbase(maker);
  initDaqMap();
}



/// initialize TOF Slat parameters from XDF file
void StTofGeometry::initGeomFromXdf(const Char_t* InputXdfFile){
}



/// initialize TOF Slat parameters from STAR dBase
void StTofGeometry::initGeomFromDbase(StMaker *maker){
  if (!maker){
    LOG_INFO << "StTofGeometry: unable to initialize without maker" << endm;
    return;
  }
  LOG_INFO << "StTofGeometry: eventtime = ";
  maker->GetDateTime().Print();
  
  TDataSet* mDbDataSet;
  mDbDataSet= maker->GetDataBase("Geometry/tof");

  assert(mDbDataSet);
  St_tofSlatGeom* mSlatGeom = static_cast<St_tofSlatGeom*>(mDbDataSet->Find("tofSlatGeom"));
  assert(mSlatGeom);
  tofSlatGeom_st *mslats= static_cast<tofSlatGeom_st*>(mSlatGeom->GetArray());
  int numRows=mSlatGeom->GetNRows(); // should be 41 rows...
  LOG_INFO << "StTofGeometry: numRows = " << numRows << endm;
  for (int i=0;i<numRows;i++) mTofSlatVec.push_back(mslats[i]);


  mTofParam.i_eta_max         = 10;
  mTofParam.i_eta_min         =  1;
  mTofParam.i_phi_max         =  5;
  mTofParam.i_phi_min         =  1;
  mTofParam.n_counter_eta     = 10;
  mTofParam.n_counter_phi     =  5;
  mTofParam.n_tray_eta        =  1;
  mTofParam.n_tray_phi        =  1;
  mTofParam.counter_thickness =  2; 
  mTofParam.counter_width     =  4;
  mTofParam.r                 = 213.95;
  mTofParam.tray_height       =   4.7;
  mTofParam.tray_width        =  10.795;
  mTofParam.tray_length       = 120.81;
  mTofParam.tray_phi_zero     =   0;
   
}



/// Calculate and return SlatId from GEANT (ieta,iphi) coordinates
int StTofGeometry::calcSlatId(const int iphi, const int ieta) const {
  return ((ieta - 1) * 4 + iphi);
}



/// return slat geometry structure for slatId
tofSlatGeom_st StTofGeometry::tofSlat(const Int_t slatId) const {
  tofSlatGeom_st thisSlat;
  if(slatId > 0) 
    thisSlat = mTofSlatVec[slatId-1];
  else 
    LOG_INFO << "StTofGeometry:  Warning: slatId ("<< slatId <<") seriously out of range" << endm;
  return thisSlat;
}



/// set-up the default Daq-to-SlatId and Slat-to-DaqId mappings
void StTofGeometry::initDaqMap(){
  LOG_INFO << "StTofGeometry: Initializing default DAQ and SlatId mappings" << endm;

  // tray...
  mTofDaqMap[ 0]=37; mTofDaqMap[ 1]=38; mTofDaqMap[ 2]=39; mTofDaqMap[ 3]=40; mTofDaqMap[ 4]=41; 
  mTofDaqMap[ 5]=33; mTofDaqMap[ 6]=34; mTofDaqMap[ 7]=35; mTofDaqMap[ 8]=36; 
  mTofDaqMap[ 9]=29; mTofDaqMap[10]=30; mTofDaqMap[11]=31; mTofDaqMap[12]=32; 
  mTofDaqMap[13]=25; mTofDaqMap[14]=26; mTofDaqMap[15]=27; mTofDaqMap[16]=28; 
  mTofDaqMap[17]=21; mTofDaqMap[18]=22; mTofDaqMap[19]=23; mTofDaqMap[20]=24; 
  mTofDaqMap[21]=17; mTofDaqMap[22]=18; mTofDaqMap[23]=19; mTofDaqMap[24]=20; 
  mTofDaqMap[25]=13; mTofDaqMap[26]=14; mTofDaqMap[27]=15; mTofDaqMap[28]=16; 
  mTofDaqMap[29]= 9; mTofDaqMap[30]=10; mTofDaqMap[31]=11; mTofDaqMap[32]=12; 
  mTofDaqMap[33]= 5; mTofDaqMap[34]= 6; mTofDaqMap[35]= 7; mTofDaqMap[36]= 8; 
  mTofDaqMap[37]= 1; mTofDaqMap[38]= 2; mTofDaqMap[39]= 3; mTofDaqMap[40]= 4;

  // ramp and pvpd....
  mTofDaqMap[41]=99;
  mTofDaqMap[42]=51;
  mTofDaqMap[43]=52;
  mTofDaqMap[44]=53;
  mTofDaqMap[45]=54;
  mTofDaqMap[46]=55;
  mTofDaqMap[47]=56; 

  // determine  Slat-to-DaqId
  for (int i=0;i<41;i++) mTofSlatMap[mTofDaqMap[i]]=i+1;
  mTofSlatMap[0]=99;
}



///  calculate the normal vector <r> to a slat
StThreeVectorD StTofGeometry::tofSlatNormPoint(const Int_t slatId) const {
  tofSlatGeom_st thisSlat = tofSlat(slatId);
  double cosAng = thisSlat.cosang;
  double sinAng = ::sqrt(1.0 - cosAng*cosAng);                 
  double tanAng = fabs(sinAng/cosAng);
  double r = (fabs(thisSlat.z) + thisSlat.r/tanAng) * sinAng;
  double x = r * fabs(cosAng) * cos(thisSlat.phi);
  double y = r * fabs(cosAng) * sin(thisSlat.phi);
  double z = r * sinAng * cosAng/fabs(cosAng)*(-1.0);
  StThreeVectorD slatNormPoint = StThreeVectorD(x,y,z);
  return slatNormPoint;
}



/// calculate the normal vector to a slats-plane
/*!
  tofSlatNormPoint and tofPlaneNormPoint do not always match each other
  because of uncertainties in phi measurments. but the difference is small.   
*/
StThreeVectorD StTofGeometry::tofPlaneNormPoint(Int_t slatId) const {
  StThreeVectorD planeNormPoint(0.0, 0.0, 0.0);  
  tofSlatGeom_st thisSlat = tofSlat(slatId);
  int iEta = thisSlat.ieta;
  int iPhi, centerSlatId;
  if (iEta==10){  // for 5w rows take the centre (iphi=3)
    iPhi=3;
    centerSlatId=calcSlatId(iPhi,iEta);
    planeNormPoint = tofSlatNormPoint(centerSlatId);
  }
  else {        // in case of 4w rows average over slats iphi=2,4
    for (iPhi=2;iPhi<4;iPhi++){
      centerSlatId=calcSlatId(iPhi,iEta);
      planeNormPoint += tofSlatNormPoint(centerSlatId)/2;
    }
    
  }
  return planeNormPoint;
}



/// print global geometry parameters
void StTofGeometry::printGeo(ostream& os) const {
  os << "------StTofGeometry::printGeo()------" << endm;
  os << "eta id max & min        = " << mTofParam.i_eta_max << " "
                                     << mTofParam.i_eta_min << endm;
  os << "phi id max & min        = " << mTofParam.i_phi_max << " "
                                     << mTofParam.i_phi_min << endm;
  os << "counters/trays/eta(phi) = " << mTofParam.n_counter_eta  << " "
                                     << mTofParam.n_counter_phi << endm;
  os << "trays in eta & phi      = " << mTofParam.n_tray_eta << " "
                                     << mTofParam.n_tray_phi << endm;
  os << "slat thickness & width  = " << mTofParam.counter_thickness << " "
                                     << mTofParam.counter_width << endm;
  os << "mean counter radius     = " << mTofParam.r  << endm;
  os << "tray height & width     = " << mTofParam.tray_height << " "
                                     << mTofParam.tray_width << endm;
  os << "tray length & phi0      = " << mTofParam.tray_length << " "
                                     << mTofParam.tray_phi_zero << endm;
  LOG_INFO << "---------------------------------------" << endm;
}



/// print slat-specific geometry parameters
void StTofGeometry::printSlat(const Int_t slatId, ostream& os) const {
  os << "------StTofGeometry::printSlat()------" << endm;
  os << "Slat: id, tray, eta, phi    = " << " " << slatId << " "
     << tofSlat(slatId).trayId  << " " << tofSlat(slatId).ieta
     << " " << tofSlat(slatId).iphi << endm;
  os << "ieta, cosang                = "<< tofSlat(slatId).ieta<< " "
       << tofSlat(slatId).cosang<< endm;
  os << "eta, eta_max, etamin        = " << tofSlat(slatId).eta<< " "
                                         << tofSlat(slatId).eta_max<< " " 
                                         << tofSlat(slatId).eta_min<< endm;
  os << "r, z, z_max, z_min          = " << tofSlat(slatId).r<< " "
                                         << tofSlat(slatId).z<< " " 
                                         << tofSlat(slatId).z_max<< " " 
                                         << tofSlat(slatId).z_min<< endm;
  os << "iphi, phi, phi_max, phi_min  = "<< tofSlat(slatId).iphi<< " "
                                         << tofSlat(slatId).phi<< " " 
                                         << tofSlat(slatId).phi_max << " " 
                                         << tofSlat(slatId).phi_min<< endm;
  LOG_INFO << "------------------------------------" << endm;
}



///  check if a point is in a slat
int StTofGeometry::tofSlatCross(const StThreeVectorD& point, const tofSlatGeom_st tofSlat) const {
  int slatCross=0;
  float phi = point.phi();

  // swap eta min and max in STAF database for slats with eta > 0.     
  float etaMin, etaMax;
  if(tofSlat.eta < 0) { 
    etaMin = tofSlat.eta_min;
    etaMax = tofSlat.eta_max;
  }
  else { 
    etaMin = tofSlat.eta_max;
    etaMax = tofSlat.eta_min;
  }

  // start to check
  if(point.pseudoRapidity() >= etaMin &&
     point.pseudoRapidity() <= etaMax) { 

    if (phi >= tofSlat.phi_min && phi <= tofSlat.phi_max)   
      slatCross = 1;
  }
  return slatCross;
}



///  decode the volumeId and return a constructed slatId
int StTofGeometry::tofSlatCrossId(const int volumeId) const {
  int phiId = -1;
  int etaId = -1;

  int trayEta    = volumeId/100000;
  int trayPhi    = static_cast<int>(fmod(volumeId,100000.)/1000) ;
  int counterPhi = static_cast<int>(fmod(volumeId,1000.)/100) ;
  int counterEta = static_cast<int>(fmod(volumeId,100.)) ;

  if (trayEta==1) {
    phiId = 14 - trayPhi ;
    if (phiId<1) phiId = phiId + 60 ;
    if (counterEta==1)
      phiId = phiId * 5 - counterPhi + 1;   // 5w row
    else
      phiId = phiId * 4 - counterPhi + 1;   // 4w rows
    etaId = counterEta + 10 ;
    LOG_INFO << "StTofGeometry: WARNING TOFp tray not in EAST barrel" << endm;
  }
  else if (trayEta==2) {
    phiId = trayPhi - 42 ;
    if (phiId<1) phiId = phiId + 60 ;
    if (counterEta==1)
      phiId = phiId * 5 + counterPhi - 5;   // 5w row
    else
      phiId = phiId * 4 + counterPhi - 4;   // 4w rows
    etaId = 11 - counterEta ;
  }
  else
    LOG_INFO<<" StTofGeometry::tofSlatCrossId  unknown trayId  "<<trayEta<<endm ;
  
  int slatId = calcSlatId(counterPhi,etaId);
  return slatId;
}



/// return the index of a slat if the point is in the slat 
int StTofGeometry::tofSlatCrossId(const StThreeVectorD& point) const {
  int etaId = -1;
  int phiId = -1;

  for (unsigned int i=0;i<mTofSlatVec.size();i++){
    if(point.z()   >= mTofSlatVec[i].z_min   &&
       point.z()   <= mTofSlatVec[i].z_max   &&
       point.phi() >= mTofSlatVec[i].phi_min &&
       point.phi() <= mTofSlatVec[i].phi_max) {
      etaId = mTofSlatVec[i].ieta;
      phiId = mTofSlatVec[i].iphi;
      break;
    }
    if (etaId!=-1 && phiId!=-1) break;
  }

  // calculate slat index (negative index indicates that the point is out of all slats)
  int slatId;
  if(etaId > 0 && phiId > 0) slatId = calcSlatId(phiId,etaId);
  else slatId = -1;

  return slatId;
}



/// finds slats in an array of trays which are crossed by a track-helix.
tofSlatHitVector StTofGeometry::tofHelixToArray(const StPhysicalHelixD& helix, 
						idVector slatIdVec) {
  idVector     idErasedVec = slatIdVec;
  idVectorIter slatIdIter, idErasedIter;

  double pathLength;
  
  StructSlatHit slatHit;
  tofSlatHitVector slatHitVec;
  slatHitVec.clear();

  // determine which stats in cluster are crossed by the track-helix.
  while (slatIdVec.size() != 0) {
    
    // the first slat in the cluster
    slatIdIter = slatIdVec.begin();
    
    int   trayId = this->tofSlat(*slatIdIter).trayId;
    float cosang = this->tofSlat(*slatIdIter).cosang;
    int   iEta   = this->tofSlat(*slatIdIter).ieta;
    
    // layer[mMax-1] closest to STAR center, layer[0] farthest.
    // typically hitprof  [msb|...|lsb] = [innermost| ... | outermost]
    unsigned int idMiddleLayer = (mMaxSlatLayers-1)/2;
    float layerSeperation = mTofParam.counter_thickness/(mMaxSlatLayers-1);
    StThreeVectorD slatNormLayer[mMaxSlatLayers];
    slatNormLayer[idMiddleLayer] =  this->tofPlaneNormPoint(*slatIdIter);
    StThreeVectorD slatNormalVec = slatNormLayer[idMiddleLayer]/slatNormLayer[idMiddleLayer].mag();
    for (unsigned int ll=1;ll<(idMiddleLayer+1);ll++){
	slatNormLayer[idMiddleLayer+ll] = slatNormLayer[idMiddleLayer]
	  *((slatNormLayer[idMiddleLayer].mag() - ll*layerSeperation )
	    /slatNormLayer[idMiddleLayer].mag());
	slatNormLayer[idMiddleLayer-ll] = slatNormLayer[idMiddleLayer]
	  *((slatNormLayer[idMiddleLayer].mag() + ll*layerSeperation )
	    /slatNormLayer[idMiddleLayer].mag());
    }
    StThreeVectorD hitAtLayer[mMaxSlatLayers];
    for (unsigned int ll=0;ll<mMaxSlatLayers;ll++){
      pathLength = helix.pathLength(slatNormLayer[ll], slatNormalVec);
      pathLength = (pathLength>0) ? pathLength : 1.0e+33;
      hitAtLayer[ll] = helix.at(pathLength);
    }

    // loop over all slats in idErasedVec (=slatIdVec at the begining of while)
    idErasedIter = idErasedVec.begin();
    while (idErasedIter != idErasedVec.end()) {
      
      // check if any slat in the plane where the first slat lies
      if(this->tofSlat(*idErasedIter).cosang == cosang &&
	 this->tofSlat(*idErasedIter).ieta   == iEta   &&
	 this->tofSlat(*idErasedIter).trayId == trayId) {

	  bool layer[mMaxSlatLayers];
	  int numberOfHitLayers(0);
	  for (unsigned int ll=0;ll<mMaxSlatLayers;ll++){
	    layer[ll] = tofSlatCross(hitAtLayer[ll], this->tofSlat(*idErasedIter));
	    if (layer[ll]) numberOfHitLayers++;
	  }
	
 	// save fired slat in slatHitVec which will be returned at the end  
	  if (numberOfHitLayers>0) {
	    slatHit.hitProfile = 0;
	    if (Debug()) LOG_INFO << "L(0-"<<mMaxSlatLayers-1<<"): ";
	    for  (unsigned int ll=0;ll<mMaxSlatLayers;ll++){
	      slatHit.hitProfile = 2*slatHit.hitProfile + layer[ll]*1;
	      if (Debug()) LOG_INFO <<layer[ll];
	    }
	    if (Debug()) LOG_INFO << endm;

	    unsigned int innerLayer(0), outerLayer(mMaxSlatLayers-1);
	    // boundary control not really necessary since anyLayer guarantees at least one layer
	    while (!layer[innerLayer] && (innerLayer<=mMaxSlatLayers)) innerLayer++;
	    while (!layer[outerLayer] && (outerLayer>= 0            )) outerLayer--;
	    if (Debug()) LOG_INFO << "Li="<<innerLayer<< " Lo="<<outerLayer<<" nLx="<<numberOfHitLayers<<endm;


	    float s(0);
	    StThreeVectorD distance(0,0,0);

	    if (innerLayer!=outerLayer){
	      s = tofPathLength(&hitAtLayer[innerLayer],&hitAtLayer[outerLayer],helix.curvature());
	      distance = hitAtLayer[outerLayer] - hitAtLayer[innerLayer];	    
	    }

	    float theta_xy = atan2(distance.x(),distance.y());
	    float theta_zr = atan2(distance.z(),distance.perp());

	    // make angles local to the slat
	    theta_zr -= acos(this->tofSlat(*idErasedIter).cosang);
	    theta_xy += pi/2 + this->tofSlat(*idErasedIter).phi;

	    slatHit.s = s;
	    slatHit.theta_xy = theta_xy;
	    slatHit.theta_zr = theta_zr;

	    slatHit.slatIndex = *idErasedIter;

	    // calculate average hit position, based on all hit layers
	    StThreeVectorD averageHitPos(0,0,0);
	    for (unsigned int i=0;i<mMaxSlatLayers;i++)
	      if (layer[i]){
		averageHitPos += hitAtLayer[i]/numberOfHitLayers;
		slatHit.layerHitPositions.push_back(hitAtLayer[i]);
	      }
	    slatHit.hitPosition = averageHitPos;

	    slatHitVec.push_back(slatHit);
	    slatHit.layerHitPositions.clear();
	  }
	//erase the slat entry which has been checked (preparing for the next round)
	idErasedVec.erase(idErasedIter);
	idErasedIter--;
      }
      idErasedIter++;
    }
    slatIdVec = idErasedVec;
  }
  return slatHitVec;
}


/// returns 1-D hit position on the TOFp slat (Zhit)
float StTofGeometry::slatHitPosition(StThreeVectorD* hitPoint){
  int slatId = this->tofSlatCrossId(*hitPoint);
  if (slatId>0){
    float zmin   = this->tofSlat(slatId).z_min;
    float zmax   = this->tofSlat(slatId).z_max;
    float cosang = this->tofSlat(slatId).cosang;
    
    // pmt is on the zmax side.
    float length = (zmax-hitPoint->z())/cosang ;
    float max_distance = (zmax-zmin)/cosang ;

    if (length>max_distance || length<0){
      LOG_INFO <<  "HitPositionCorrection:  length="<<length<<" max="<<max_distance
	   << " zmin="<<zmin<<" zmax="<<zmax<<" cosang="<<cosang<<endm;
      this->printSlat(slatId);
    }

    return length;
  }

  // return very negative value to indicate bad slatId.
  LOG_INFO << "slatHitPosition: hit point out of range;"
       << " phi=" << hitPoint->phi() << "  z=" << hitPoint->z() << endm;
  return -100.;
}


/// returns idVector of 3x3 (max) neighbouring slatIds
idVector StTofGeometry::slatNeighbours(int slatId){
  tofSlatGeom_st middleSlat = this->tofSlat(slatId);
  int slatPhi = middleSlat.iphi;
  int slatEta = middleSlat.ieta;

  idVector neighbours;
  neighbours.clear();
  int thisSlatId, ieta, iphi;

  // for all the 4w rows life is simple ...
  if (slatEta<9){
    for (ieta= slatEta-1;ieta<=slatEta+1;ieta++)
      for (iphi= slatPhi-1;iphi<=slatPhi+1;iphi++){
	if ((ieta==slatEta) && (iphi==slatPhi)) continue;
	if ((ieta <1) || (iphi<1) || (iphi>4)) continue;
	thisSlatId = calcSlatId(iphi,ieta);
	neighbours.push_back(thisSlatId);
      }
  }
  // the 4w row next to the 5w row needs a little bit attention ...
  else if (slatEta==9){
    for (ieta= slatEta-1;ieta<=slatEta;ieta++)
      for (iphi= slatPhi-1;iphi<=slatPhi+1;iphi++){
	if ((ieta==slatEta) && (iphi==slatPhi)) continue;
	if ((iphi<1) || (iphi>4)) continue;
	thisSlatId = calcSlatId(iphi,ieta);
	neighbours.push_back(thisSlatId);
      }
    ieta = slatEta+1; iphi = slatPhi;
    thisSlatId = calcSlatId(iphi,ieta);
    neighbours.push_back(thisSlatId);
    iphi = slatPhi+1;
    thisSlatId = calcSlatId(iphi,ieta);
    neighbours.push_back(thisSlatId);
  }
  // the 5w row needs special treatment ...
  else if (slatEta==10){
    for (iphi= slatPhi-1;iphi<=slatPhi+1;iphi++){
      if (iphi==slatPhi) continue;
      if ((iphi<1) || (iphi>5)) continue;
      ieta = slatEta;
      thisSlatId = calcSlatId(iphi,ieta); 
      neighbours.push_back(thisSlatId);
    }
    ieta=slatEta-1;
    for (iphi=slatPhi-1;iphi<=slatPhi;iphi++){
      if ((iphi<1) || (iphi>4)) continue;
      thisSlatId = calcSlatId(iphi,ieta);
      neighbours.push_back(thisSlatId);
    }
  }

  return neighbours;
}


/// returns idVector of 5x5 (max) neighbouring slatIds
idVector StTofGeometry::slatNeighboursWide(int slatId){
  // consider a wider range around this slatId (5x5)
  tofSlatGeom_st middleSlat = this->tofSlat(slatId);
  int slatPhi = middleSlat.iphi;
  int slatEta = middleSlat.ieta;

  idVector neighbours;
  neighbours.clear();
  int thisSlatId, ieta, iphi;

  for (ieta=slatEta-2;ieta<=slatEta+2;ieta++){
    if ((ieta<1) || (ieta>10)) continue;
    int iphiMax = (ieta==10)?5:4;
    for (iphi = 1; iphi<=iphiMax; iphi++){
      if ((ieta==slatEta) && (iphi==slatPhi)) continue;
      thisSlatId = calcSlatId(iphi,ieta);
      neighbours.push_back(thisSlatId);
    }
  }

  return neighbours;
}


/// returns the local Phi angle of a track inside a slat
float StTofGeometry::slatPhiPosition(StThreeVectorD* hitPoint){
  int slatId = this->tofSlatCrossId(*hitPoint);
  if (slatId>0){
    float phimin = this->tofSlat(slatId).phi_min;
    float phimax = this->tofSlat(slatId).phi_max;
    
    float philoc = hitPoint->phi() - phimin;
    float max_deltaphi = (phimax-phimin);

    if (philoc>max_deltaphi || philoc<0){
      LOG_INFO <<  "slatHitPhiPosition:  phi="<<philoc<<" max="<<max_deltaphi
	   << " phimin="<<phimin<<" phimax="<<phimax<<endm;
      this->printSlat(slatId);
    }

    return philoc;
  }

  // return very negative value to indicate bad slatId.
  LOG_INFO << "slatHitPhiPosition: hit point out of range;"
       << " phi=" << hitPoint->phi() << "  z=" << hitPoint->z() << endm;
  return -100.;
}

//---------------------------------------------------------------------------
// estimate the possible projection on the TOF tray
Bool_t StTofGeometry::projTrayVector(const StHelixD &helix, idVector &trayVec) const {

  trayVec.clear();
  double R_tof = 220.;
  double res = 5.0;
  double s1 = helix.pathLength(R_tof).first;
  if(s1<0.) s1 = helix.pathLength(R_tof).second;
  StThreeVectorD point = helix.at(s1);
  double phi = point.phi()*180/3.14159;

  // east ring, start from 108 deg (id=61) , clock-wise from east facing west
  int itray_east = (255+(int)phi)%360/6+61;
  trayVec.push_back(itray_east);

  int itray_east1 = (255+(int)(phi+res))%360/6+61;
  int itray_east2 = (255+(int)(phi-res))%360/6+61;
  if(itray_east1!=itray_east) {
    trayVec.push_back(itray_east1);
  }
  if(itray_east2!=itray_east&&itray_east2!=itray_east1) {
    trayVec.push_back(itray_east2);
  }
  
  // west ring, start from 72 deg (id=1) , clock-wise from west facing east
  int itray_west = (435-(int)phi)%360/6+1;
  trayVec.push_back(itray_west);

  int itray_west1 = (435-(int)(phi+res))%360/6+1;
  int itray_west2 = (435-(int)(phi-res))%360/6+1;
  if(itray_west1!=itray_west) {
    trayVec.push_back(itray_west1);
  }
  if(itray_west2!=itray_west&&itray_west2!=itray_west1) {
    trayVec.push_back(itray_west2);
  }

//   LOG_INFO << " proj tray id = ";
//   for(size_t it=0;it<trayVec.size();it++) {
//     LOG_INFO << trayVec[it] << " ";
//   }
//   LOG_INFO << endm;
  
  if(trayVec.size()>0) return kTRUE;
  else return kFALSE;
}
