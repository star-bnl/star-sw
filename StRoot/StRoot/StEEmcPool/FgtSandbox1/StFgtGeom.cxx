// $Id: StFgtGeom.cxx,v 1.1 2011/04/07 19:31:22 balewski Exp $
#include <cmath>
#include <assert.h>
#include <StMessMgr.h>

#include "StFgtGeom.h"

ClassImp(StFgtGeom)

//===============================
//===============================
StFgtGeom::StFgtGeom() {
  printf("inFGT geom\n");
// More parameters are printed out with method printParam()
//  LOG_INFO<<Form("StFgtGeom::Pitch   RadStrip =%f   PhiStrip/cm =%f ",radStrip_pitch(), phiStrip_pitch()*Rout())<<endm;

  initialize(); 
}

//===============================
//===============================
void 
StFgtGeom::initialize() {

  // predefine Z_location of disks
  double zDisk[kFgtMxDisk+1]={69.08, 79.08, 89.08, 99.08, 109.08, 119.08, 168.68, 218.68, 66.98};// UPGR16
  for(int i=0;i<kFgtMxDisk+1;i++)mDiskZ[i]=zDisk[i];
//  for(int i=0;i<kFgtMxDisk+1;i++)mDiskZ[i]=zDisk[i]+15.0;

  // calculate numbers of strips
  pi=2.*acos(0.);
  dpi=2.*pi; // double pi
  halfpi=0.5*pi; // halfphi

  mRadStripOff = ((Rout()-Rin())/radStrip_pitch() - 
                     int( (Rout()-Rin())/radStrip_pitch())) * radStrip_pitch();
 
  mRadStripLOCId_number = int ((Rout()-Rin()-radStripOff())/radStrip_pitch()) + 5;

  mPhiStripOff = (halfpi/phiStrip_pitch() - 
                     int( halfpi/phiStrip_pitch())) * phiStrip_pitch();
  mPhiStripLOCId_number = int ((halfpi-phiStripOff())/phiStrip_pitch()) + 5;
 
  mRadStripGBLId_number = kFgtMxQuad * radStripLOCId_number();
  mPhiStripGBLId_number = kFgtMxQuad * phiStripLOCId_number();

}

//===============================
//===============================
bool
StFgtGeom::inDisk(TVector3 r){ // 'r' in LAB ref
  float Rxy=r.Perp();
  if(Rxy<Rin()) return false;
  if(Rxy>Rout()) return false;
  if(fabs(r.y())>yLimit()) return false;
  return true;
}

//===============================
//===============================
int
StFgtGeom::getQuad(double phiLab){
  //printf("aa %10f  %f\n",phiLab,pi);
  assert(phiLab<=pi);
  assert(phiLab>=-pi);
  if(phiLab>phiQuadXaxis(0) && phiLab<=phiQuadXaxis(1)) return 0;
  if(phiLab>phiQuadXaxis(1) && phiLab<=phiQuadXaxis(2)) return 1;
  if(phiLab>phiQuadXaxis(3) && phiLab<=phiQuadXaxis(0)) return 3;
  return 2;
} 

//===============================
//===============================
double
StFgtGeom::phiQuadXaxis(int iquad) {
 
  switch(iquad) {
/*
  case 0: return -0.261799387;  // - 15 deg
  case 1: return  1.308996939;  // +75 deg
  case 2: return  2.879793266;  // +165 deg
  case 3: return -1.832595715;  // -105 deg
*/
  case 0: return -15.0*pi/180.0;  // - 15 deg
  case 1: return  75.0*pi/180.0;  // +75 deg
  case 2: return  165.0*pi/180.0;  // +165 deg
  case 3: return -105.0*pi/180.0;  // -105 deg
  default:
    assert(2==3);
  }
}

//===============================
//===============================
bool
StFgtGeom::localXYtoStripID( int iquad, double xLoc, double yLoc,int &iRadID,int &iPhiID, int dbg){ // returns false if out of range
  
  iRadID= iPhiID=-1;
  int locRadID, locPhiID;
  double r=sqrt(xLoc*xLoc+yLoc*yLoc); 
  //.............. trim outside of active area
  if(r< Rin() ) return false;
  if(r> Rout() ) return false;

  //............. find phi in lab ref
  double  phiLoc=atan2(yLoc,xLoc); // [0,pi/2] in local ref frame

  // assume all 9 disks have identical strip numbering scheme
  double radBinFrac,phiBinFrac; // optional , for QA
  locRadID=rad2LocalStripId(r,&radBinFrac);     // no quad/disk dependence
  locPhiID=phiLoc2LocalStripId(phiLoc,&phiBinFrac);// no quad/disk dependence

  iRadID = radIdLocal2Global(iquad, locRadID);
  iPhiID = phiIdLocal2Global(iquad, locPhiID);

  if(dbg) {
     printf("strip:  yLoc=%f xLoc=%f phi=%f, lradID=%d, lphiID=%d\n",yLoc,xLoc,phiLoc*57.3,locRadID,locPhiID); 
     printf("strip:  radID=%d +(%.2f) phiID=%d +(%.2f)\n",iRadID,radBinFrac, iPhiID,phiBinFrac);
  }
  return true;
}

//===============================
//===============================
double
StFgtGeom::stripID2Rxy(double fRadBin){

  int iRadId = (int)fRadBin;
  if (iRadId < 0) return -1;
  double eps=fRadBin-iRadId;
  int localId = radIdGlobal2Local(iRadId);
  double pos = Rout()-(localId + eps)*radStrip_pitch();

  return pos;
}

//===============================
//===============================
double
StFgtGeom::stripID2PhiLoc( double fPhiBin){

  int iPhiId = (int)fPhiBin;
  if (iPhiId < 0) return -999.0;
  double eps=fPhiBin-iPhiId;
  int localId = phiIdGlobal2Local(iPhiId);
  double phiLocal = (localId+eps)*phiStrip_pitch();

  return phiLocal;
}

//===============================
//===============================
double
StFgtGeom::stripID2PhiLab(double fPhiBin){

  double phiLab;
  double phiLocal = stripID2PhiLoc(fPhiBin);
  int iPhiId = (int)fPhiBin;
  int iquad = iPhiId/phiStripLOCId_number();

//  offset for quad1-3 is added here
  phiLab = (phiLocal != -999.0)? 
// Bug of adding iquad*phiStripOff() fixed    WMZ 8/14/09
//       phiLocal + phiQuadXaxis(iquad) + iquad*phiStripOff(): phiLocal; 
       phiLocal + phiQuadXaxis(iquad): phiLocal; 

  return phiLab;
}

//===============================
//===============================
void
StFgtGeom::printParam(){ 
   cout << " Parementers of StFgtGeom" << endl;
   cout << " -------------------------" << endl;
   cout << "\tRout, Rmid, Rin = " <<  Rout() << " cm, " <<  Rmid() << " cm, " 
                                <<  Rin() << " cm" << endl;
   cout << "\tRad/Phi Strip  pitch = "   << radStrip_pitch() << " cm, "
                                       << kFgtPhiPitch << " cm" << endl;
   cout << "\tOffset of Rad/Phi  = "  << radStripOff() << " cm, "
                             << phiStripOff()*Rout() << " cm"<< endl;
   cout << "\tNumber of Rad/Phi Strip Ids = "  << radStripLOCId_number() << " "
                                          << phiStripLOCId_number() << endl;
   cout << "\tZ0 - Z4 = "  << mDiskZ[0] << " " << mDiskZ[1] << " " 
                           << mDiskZ[2] << " " << mDiskZ[3] << " " 
                           << mDiskZ[4] << " cm" << endl; 
   cout << "\tZ5 - Z8 = "  << mDiskZ[5] << " " << mDiskZ[6] << " " 
                           << mDiskZ[7] << " " << mDiskZ[8] << " cm" << endl; 
   cout << endl;
}

// mothods involving local IDs
//===============================
//===============================
int
StFgtGeom::rad2LocalStripId( double rad, double *binFrac){ 
  // returns -1 on error
  // 2nd argument is optional, returns fraction of the bin size [0,1.)
   double ratio=(Rout()-rad)/radStrip_pitch();
   int  irad=(int) ratio;
   if(binFrac) *binFrac=ratio-irad;
   if(irad >= 0 && irad < radStripLOCId_number()) 
     return irad;
   else
     return -1;
}

//===============================
//===============================
int
StFgtGeom::phiLoc2LocalStripId(double phiLoc, double *binFrac){ // return -1 on error
  // 2nd argument is optional, returns fraction of the bin size [0,1.)
  // phiLoc is in range [0,pi/2) 
  double ratio=phiLoc/phiStrip_pitch();
  int  iphi=(int) ratio;  
  if(binFrac) *binFrac=ratio-iphi;
  if(iphi >=0 && iphi < phiStripLOCId_number()) 
    return iphi;
  else
    return -1;
}

//===============================
//===============================
int
StFgtGeom::phiLab2LocalStripId(int iquad, double phiLab, double *binFrac){ // return -1 on error
  // 2nd argument is optional, returns fraction of the bin size [0,1.)
  
  double phi=phiLab-phiQuadXaxis(iquad);
  while(phi<0.)    phi+=dpi;
  while(phi>=dpi)  phi-=dpi;
  double phiBinFrac;
  int iphi = phiLoc2LocalStripId(phi, &phiBinFrac);
  *binFrac=phiBinFrac;
  return iphi;
}

// $Log: StFgtGeom.cxx,v $
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//
