/*******************************************************************
 *
 * $Id: StTofGeometry.cxx,v 1.1 2001/09/28 19:09:40 llope Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Geometry definitions and utility class for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofGeometry.cxx,v $
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#include "StTofGeometry.h"
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "PhysicalConstants.h"
#include "ctf/St_ctg_Module.h"

StTofGeometry::StTofGeometry(){ /* nope */};
StTofGeometry::~StTofGeometry(){ /* nope */};

void StTofGeometry::init(){
  initGeomFromXdf();
  initDaqMap();
}


void StTofGeometry::initGeomFromXdf(){
  Char_t *InputXdfFile = "/afs/rhic/star/users/geurts/public/dbase/ctg_pars.xdf";
  cout << "StTofGeometry: loading dBase from " << InputXdfFile << endl;
  St_XDFFile xdf(InputXdfFile);
  St_DataSet *ctfg = xdf.NextEventGet();
  St_DataSetIter gime(ctfg);

  St_ctg_geo*      stafTofParam(0);
  St_ctg_slat_phi* stafSlatPhi(0);
  St_ctg_slat_eta* stafSlatEta(0);
  St_ctg_slat*     stafSlatParam(0);

  stafTofParam   = (St_ctg_geo      *) gime("tof");
  stafSlatPhi    = (St_ctg_slat_phi *) gime("tof_slat_phi");
  stafSlatEta    = (St_ctg_slat_eta *) gime("tof_slat_eta");
  stafSlatParam  = (St_ctg_slat     *) gime("tof_slat");

  ctg_geo_st *geo = stafTofParam->GetTable();
  //mTofParam.detector          = geo->detector;
  mTofParam.i_eta_max         = geo->i_eta_max;
  mTofParam.i_eta_min         = geo->i_eta_min;
  mTofParam.i_phi_max         = geo->i_phi_max;
  mTofParam.i_phi_min         = geo->i_phi_min;
  mTofParam.n_counter_eta     = geo->n_counter_eta;
  mTofParam.n_counter_phi     = geo->n_counter_phi;
  mTofParam.n_tray_eta        = geo->n_tray_eta;
  mTofParam.n_tray_phi        = geo->n_tray_phi;
  mTofParam.counter_thickness = geo->counter_thickness;
  mTofParam.counter_width     = geo->counter_width;
  mTofParam.r                 = geo->r;
  mTofParam.tray_height       = geo->tray_height;
  mTofParam.tray_width        = geo->tray_width;
  mTofParam.tray_length       = geo->tray_length;
  mTofParam.tray_phi_zero     = geo->tray_phi_zero;



  // copy eta and phi database into two vectors first
  ctg_slat_eta_st *eta = stafSlatEta->GetTable();
    for(int iRow=0; iRow<stafSlatEta->GetNRows(); iRow++,eta++){
      StructTofSlatEta tofSlatEta;
      tofSlatEta.ieta = eta->ieta;
      tofSlatEta.cosang  = eta->cosang;
      tofSlatEta.eta     = eta->eta;
      tofSlatEta.eta_max = eta->eta_max;
      tofSlatEta.eta_min = eta->eta_min;
      tofSlatEta.r       = eta->r;
      tofSlatEta.z       = eta->z;
      tofSlatEta.z_max   = eta->z_max;
      tofSlatEta.z_min   = eta->z_min;
      mTofSlatEtaVec.push_back(tofSlatEta);
  }
  ctg_slat_phi_st *phi = stafSlatPhi->GetTable();
  for(int iRow=0; iRow<stafSlatPhi->GetNRows(); iRow++,phi++){
    StructTofSlatPhi tofSlatPhi;
    tofSlatPhi.iphi    = phi->iphi;
    // convert XDF 0..360 degrees to STAR -pi..pi range
    tofSlatPhi.phi     = ((phi->phi>180)?phi->phi-360:phi>phi)*degree;
    tofSlatPhi.phi_max = ((phi->phi_max>180)?phi->phi_max-360:phi->phi_max)*degree;
    tofSlatPhi.phi_min = ((phi->phi_min>180)?phi->phi_min-360:phi->phi_min)*degree;
    mTofSlatPhiVec.push_back(tofSlatPhi);
  }

  // build the database vector.
  for (int i=0; i<stafSlatEta->GetNRows();i++){
    int iPhiMin, iPhiMax;
    int iEta = mTofSlatEtaVec[i].ieta;
    if      (iEta==10) {iPhiMin= 1 ; iPhiMax= 5;}  // 1st 5-wide row
    else if (iEta== 9) {iPhiMin= 6 ; iPhiMax= 9;}  // 2nd 4-wide row
    else if (iEta < 9) {iPhiMin=10 ; iPhiMax=13;}  // all other 4-wide rows
    else {cout << "StTofGeometry: slat eta out of range " << iEta << endl;
          iPhiMin=0; iPhiMax=-1;}
    for (int j=iPhiMin-1;j<iPhiMax;j++){
      StructSlatGeom* tofSlat = new StructSlatGeom;
      tofSlat->ieta    = mTofSlatEtaVec[i].ieta;
      tofSlat->z       = mTofSlatEtaVec[i].z;
      tofSlat->z_min   = mTofSlatEtaVec[i].z_min;
      tofSlat->z_max   = mTofSlatEtaVec[i].z_max;
      tofSlat->cosang  = mTofSlatEtaVec[i].cosang;
      tofSlat->r       = mTofSlatEtaVec[i].r;
      tofSlat->eta     = mTofSlatEtaVec[i].eta;
      tofSlat->eta_min = mTofSlatEtaVec[i].eta_min;
      tofSlat->eta_max = mTofSlatEtaVec[i].eta_max;
      tofSlat->iphi    = mTofSlatPhiVec[j].iphi;
      tofSlat->phi     = mTofSlatPhiVec[j].phi;
      tofSlat->phi_min = mTofSlatPhiVec[j].phi_min;
      tofSlat->phi_max = mTofSlatPhiVec[j].phi_max;
      tofSlat->trayId  = 32;
      mTofSlatVec.push_back(*tofSlat);
      delete tofSlat;
    }
  }
}


int StTofGeometry::calcSlatId(int iphi, int ieta) const {
  return ((ieta - 1) * 4 + iphi);
}


StructSlatGeom StTofGeometry::tofSlat(Int_t slatId) const {
  StructSlatGeom thisSlat;
  if(slatId > 0) 
    thisSlat = mTofSlatVec[slatId-1];
  else 
    cout << "StTofGeometry:  Warning: slatId ("<< slatId <<") seriously out of range" << endl;
  return thisSlat;
}

void StTofGeometry::initDaqMap(){
  /*
    set-up the default DaqId to SlatId Map.
  */
  cout << "StTofGeometry: Initializing default DaqMap" << endl;
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
}


StThreeVectorD StTofGeometry::tofSlatNormPoint(Int_t slatId) const {
  /*
    calculate the normal vector <r> to a slat
  */
  StructSlatGeom thisSlat = tofSlat(slatId);
  double cosAng = thisSlat.cosang;
  double sinAng = sqrt(1.0 - cosAng*cosAng);                 
  double tanAng = fabs(sinAng/cosAng);
  double r = (fabs(thisSlat.z) + thisSlat.r/tanAng) * sinAng;
  double x = r * fabs(cosAng) * cos(thisSlat.phi);
  double y = r * fabs(cosAng) * sin(thisSlat.phi);
  double z = r * sinAng * cosAng/fabs(cosAng)*(-1.0);
  StThreeVectorD slatNormPoint = StThreeVectorD(x,y,z);
  return slatNormPoint;
}


StThreeVectorD StTofGeometry::tofPlaneNormPoint(Int_t slatId) const {
  /*
    calculate the normal vector to a slats-plane. 
    tofSlatNormPoint and tofPlaneNormPoint do not always match each other
    because of uncertainties in phi measurments. but the difference is small.   
  */
  StThreeVectorD planeNormPoint(0.0, 0.0, 0.0);  
  StructSlatGeom thisSlat = tofSlat(slatId);
  int iEta = thisSlat.ieta;
  int iPhi, centerSlatId;
  if (iEta==10){  // for 5w rows take the centre (iphi=3)
    iPhi=3;
    //centerSlatId = (iPhi - 1) * nEtas + iEta; 
    centerSlatId=calcSlatId(iPhi,iEta);
    planeNormPoint = tofSlatNormPoint(centerSlatId);
  }
  else {        // in case of 4w rows average over slats iphi=2,4
    for (iPhi=2;iPhi<4;iPhi++){
      //centerSlatId = (iPhi - 1) * nEtas + iEta; 
      centerSlatId=calcSlatId(iPhi,iEta);
      planeNormPoint += tofSlatNormPoint(centerSlatId)/2;
    }
    
  }
  return planeNormPoint;
}


void StTofGeometry::printGeo(ostream& os) const {
  os << "------StTofGeometry::printGeo()------" << endl;
  os << "eta id max & min        = " << mTofParam.i_eta_max << " "
                                     << mTofParam.i_eta_min << endl;
  os << "phi id max & min        = " << mTofParam.i_phi_max << " "
                                     << mTofParam.i_phi_min << endl;
  os << "counters/trays/eta(phi) = " << mTofParam.n_counter_eta  << " "
                                     << mTofParam.n_counter_phi << endl;
  os << "trays in eta & phi      = " << mTofParam.n_tray_eta << " "
                                     << mTofParam.n_tray_phi << endl;
  os << "slat thickness & width  = " << mTofParam.counter_thickness << " "
                                     << mTofParam.counter_width << endl;
  os << "mean counter radius     = " << mTofParam.r  << endl;
  os << "tray height & width     = " << mTofParam.tray_height << " "
                                     << mTofParam.tray_width << endl;
  os << "tray length & phi0      = " << mTofParam.tray_length << " "
                                     << mTofParam.tray_phi_zero << endl;
  cout << "---------------------------------------" << endl;
}



void StTofGeometry::printSlat(Int_t slatId, ostream& os) const {
  os << "------StTofGeometry::printSlat()------" << endl;
  os << "Slat: id, tray, eta, phi    = " << " " << slatId << " "
     << tofSlat(slatId).trayId  << " " << tofSlat(slatId).ieta
     << " " << tofSlat(slatId).iphi << endl;
  os << "ieta, cosang                = "<< tofSlat(slatId).ieta<< " "
       << tofSlat(slatId).cosang<< endl;
  os << "eta, eta_max, etamin        = " << tofSlat(slatId).eta<< " "
                                         << tofSlat(slatId).eta_max<< " " 
                                         << tofSlat(slatId).eta_min<< endl;
  os << "r, z, z_max, z_min          = " << tofSlat(slatId).r<< " "
                                         << tofSlat(slatId).z<< " " 
                                         << tofSlat(slatId).z_max<< " " 
                                         << tofSlat(slatId).z_min<< endl;
  os << "iphi, phi, phi_max, phi_min  = "<< tofSlat(slatId).iphi<< " "
                                         << tofSlat(slatId).phi<< " " 
                                         << tofSlat(slatId).phi_max << " " 
                                         << tofSlat(slatId).phi_min<< endl;
  cout << "------------------------------------" << endl;
}



int StTofGeometry::tofSlatCross(StThreeVectorD& point, StructSlatGeom tofSlat) const {
  /*
    check if a point is in a slat
  */
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
  //  slatCross = 0;
  if((float) point.pseudoRapidity() >= etaMin &&
     (float) point.pseudoRapidity() <= etaMax) { 

    // the 3rd phi of 5W slat can be special (phi_max = 0.5, phi_min = 359.5 degree) 
    if((tofSlat.ieta == 1) &&
        tofSlat.iphi == 3) {
      if ((phi <= tofSlat.phi_max && phi >= 0.0) ||
	  (phi >= tofSlat.phi_min && phi < twopi))
	slatCross = 1;
    }  
    else  {      // all others
      if (phi >= tofSlat.phi_min  &&
	  phi <= tofSlat.phi_max)   
	slatCross = 1;
    }
  }
  return slatCross;
}


int StTofGeometry::tofSlatCrossId(int volumeId) const {
  /*
    decode the volumeId and return a constructed slatId
  */
  int phiId=-1;
  int etaId=-1;

  int trayEta    = int(volumeId/100000) ;
  int trayPhi    = (short)fmod((double)volumeId,100000.)/1000 ;
  int counterPhi = (short)fmod((double)volumeId,1000.)/100 ;
  int counterEta = (short)fmod((double)volumeId,100.) ;

  if (trayEta==1) {
    phiId = 14 - trayPhi ;
    if (phiId<1) phiId = phiId + 60 ;
    if (counterEta==1)
      phiId = phiId * 5 - counterPhi + 1;   // 5w row
    else
      phiId = phiId * 4 - counterPhi + 1;   // 4w rows
    etaId = counterEta + 10 ;
    cout << "StTofGeometry: WARNING TOFp tray not in EAST barrel" << endl;
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
    cout<<" StTofGeometry::tofSlatCrossId  unknown trayId  "<<trayEta<<endl ;
  
  int slatId = calcSlatId(counterPhi,etaId);
  return slatId;
}


int StTofGeometry::tofSlatCrossId(StThreeVectorD& point) const {
  /*
    return the index of a slat if the point is in the slat 
  */
  int etaId = -1;
  int phiId = -1;

//FG  // rearrange database of STAF which does not meet StRoot standard
//FG  //   phi in StThreeVectorD is between -pi to +pi (radius)
//FG  //   phi in TOF database is between 0 to 360 (degree)
//FG  //   the constant degree = pi/180 = 0.01745
//FG
//FG  float phi = point.phi();
//FG  if(phi < 0) phi = (phi + twopi)/degree;
//FG  else phi = phi/degree;
//FG  
//FG  int nEtas = mTofSlatEtaVec.size();
//FG  for(int i=0; i<nEtas; i++) {
//FG	float etaMin, etaMax;
//FG	// swap eta min and max in STAF database for slats with eta > 0.     
//FG	if(mTofSlatEtaVec[i].eta < 0) { 
//FG	  etaMin = mTofSlatEtaVec[i].eta_min;
//FG	  etaMax = mTofSlatEtaVec[i].eta_max;
//FG	}
//FG	else { 
//FG	  etaMin = mTofSlatEtaVec[i].eta_max;
//FG	  etaMax = mTofSlatEtaVec[i].eta_min;
//FG	}
//FG
//FG	// get eta index if any
//FG	if((float) point.pseudoRapidity() >= etaMin &&
//FG	   (float) point.pseudoRapidity() <= etaMax)  {
//FG	  etaId = mTofSlatEtaVec[i].ieta;
//FG	  break;
//FG	}
//FG  }
//FG	
//FG  // determine the boundaries of a loop over phi for 5W and 4W slats
//FG  int iPhiBegin, iPhiEnd;
//FG  //fg  if(etaId == 10 || etaId == 11) {             // 5W rows
//FG  if(etaId == 10) {             // 5W rows
//FG	iPhiBegin = 1;
//FG	//fg iPhiEnd = nPhiRows5w;
//FG	iPhiEnd = 5;
//FG  }
//FG  //fg  else if (etaId == 9 || etaId == 12) {        // 4W[2] rows
//FG  //fg  iPhiBegin = nPhiRows5w;
//FG  //fg  iPhiEnd = offsetMult4w;
//FG  else if (etaId == 9) {        // 4W[2] rows
//FG	iPhiBegin = 6;
//FG	iPhiEnd = 9;
//FG  }
//FG  else {                                         // 4W[3-10] rows
//FG	//fg iPhiBegin =offsetMult4w;
//FG	//fg iPhiEnd=offsetMult4w + nPhiRows4w;
//FG	iPhiBegin =10;
//FG	iPhiEnd=13;
//FG  }
//FG  
//FG  // get phi index if any
//FG  for(int i = iPhiBegin-1; i <iPhiEnd; i++) {
//FG	// the 3rd phi of 5W slat is a special (phi_max = 0.5, phi_min = 359.5 degree) 
//FG	//fg if((etaId == 10 || etaId == 11) && i == 2) {
//FG	//if((etaId == 1) && i == 2) {
//FG	//	if ((phi <= mTofSlatPhiVec[i].phi_max) ||
//FG	//	    (phi >= mTofSlatPhiVec[i].phi_min)) {
//FG	//	  phiId = mTofSlatPhiVec[i].iphi; 
//FG	//	  break;
//FG	//	}
//FG	//}  
//FG
//FG	// all others
//FG	//else  {
//FG	  if (phi >= mTofSlatPhiVec[i].phi_min &&
//FG	      phi <= mTofSlatPhiVec[i].phi_max) {  
//FG	    phiId = mTofSlatPhiVec[i].iphi; 
//FG	    break;
//FG	  }
//FG   //}
//FG  }

  for (unsigned int i=0;i<mTofSlatVec.size();i++){
    if((float) point.pseudoRapidity() >= mTofSlatVec[i].eta_min &&
       (float) point.pseudoRapidity() <= mTofSlatVec[i].eta_max &&
       (float) point.phi() >= mTofSlatVec[i].phi_min &&
       (float) point.phi() <= mTofSlatVec[i].phi_max) {
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

tofSlatHitVector StTofGeometry::tofHelixToArray(StPhysicalHelixD& helix, 
                                            idVector slatIdVec) {
  /*
    this member function finds slats in an array of trays which are
    crossed by a track-helix. 
  */
  idVector     idErasedVec = slatIdVec;
  idVectorIter slatIdIter, idErasedIter;

  bool   middle, inner, outer;
  double pathLength;
  
  float slatThickness = mTofParam.counter_thickness;

  StructSlatHit slatHit;
  tofSlatHitVector slatHitVec;
  slatHitVec.clear();

  // determine which stats in cluster are crossed by the track-helix.
  while (slatIdVec.size() != 0) {
    
    // the first slat in the cluster
    slatIdIter = slatIdVec.begin();
    
    int trayId = this->tofSlat(*slatIdIter).trayId;
    float cosang = this->tofSlat(*slatIdIter).cosang;
    int iEta = this->tofSlat(*slatIdIter).ieta;
    
    // middle of the slat
    StThreeVectorD slatNormMiddle = this->tofPlaneNormPoint(*slatIdIter);
    
    // get normal vector of the plane
    StThreeVectorD slatNormal = slatNormMiddle/slatNormMiddle.mag();

    // two edges (inner and outer) of the slat
    StThreeVectorD slatNormInner = slatNormMiddle*((slatNormMiddle.mag() -
						    slatThickness)/ slatNormMiddle.mag());
    StThreeVectorD slatNormOuter = slatNormMiddle*((slatNormMiddle.mag() +
						    slatThickness)/ slatNormMiddle.mag());
    // points at three planes
    pathLength = helix.pathLength(slatNormMiddle, slatNormal);
    StThreeVectorD hitAtMiddle = helix.at(pathLength);
    pathLength = helix.pathLength(slatNormInner, slatNormal);
    StThreeVectorD hitAtInner = helix.at(pathLength);
    pathLength = helix.pathLength(slatNormOuter, slatNormal);
    StThreeVectorD hitAtOuter = helix.at(pathLength);

    // loop over all slats in idErasedVec (=slatIdVec at the begnining of while)
    idErasedIter = idErasedVec.begin();
    while (idErasedIter != idErasedVec.end()) {
      
      // check if any slat in the plane where the first slat lies
      if(this->tofSlat(*idErasedIter).cosang == cosang &&
	 this->tofSlat(*idErasedIter).ieta == iEta &&
	 this->tofSlat(*idErasedIter).trayId == trayId) {

	// check if any slat in the plane is fired
	middle=tofSlatCross(hitAtMiddle, this->tofSlat(*idErasedIter));
	inner=tofSlatCross(hitAtInner, this->tofSlat(*idErasedIter));
	outer=tofSlatCross(hitAtOuter, this->tofSlat(*idErasedIter));
	
	// fill histogram of hit pattern 
	// if(middle && inner && outer) hist->Fill(1.0); 
	// if(!middle && inner && outer) hist->Fill(2.0); 
	// if(middle && !inner && outer) hist->Fill(3.0); 
	// if(middle && inner && !outer) hist->Fill(4.0); 
	// if(!middle && !inner && outer) hist->Fill(5.0); 
	// if(!middle && inner && !outer) hist->Fill(6.0); 
	// if(middle && !inner && !outer) hist->Fill(7.0); 
 
	// save fired slat in slatHitVec which will be returned at the end  
	if(middle||inner||outer) {
	  slatHit.slatIndex = *idErasedIter;
	  if(middle) slatHit.hitPosition = hitAtMiddle; 
	  else if(inner) slatHit.hitPosition = hitAtInner; 
	  else slatHit.hitPosition = hitAtOuter; 
	  slatHitVec.push_back(slatHit);

	  //cout << "fired? Ids, and angles = " 
	  //	 << middle << " " << inner <<" "<< outer <<
	  //  " " << slatNormal.x() <<
	  //  " " << slatNormal.y() <<
	  //  " " << slatNormal.z() <<
	  //  " " << this->tofSlat(*idErasedIter)->ieta <<
	  //  " " << this->tofSlat(*idErasedIter)->iphi <<
	  //  " " << this->tofSlat(*idErasedIter)->eta <<
	  //  " " << this->tofSlat(*idErasedIter)->phi << endl;
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
