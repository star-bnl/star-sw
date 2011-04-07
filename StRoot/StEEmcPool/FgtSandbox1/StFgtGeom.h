// \class  StFgtGeom

// \author Jan Balewski
// \date   July 2007 
// Modified Oct 2008 WMZ
#ifndef StFgtGeom_h
#define StFgtGeom_h
/*********************************************************************
 * $Id: StFgtGeom.h,v 1.1 2011/04/07 19:31:22 balewski Exp $
 *********************************************************************
 * Description:
 * STAR Forward Gem Tracker  Geometry Class
 * across all FGT internal code we will use indexing of disks, quadrants, strips from 0 to N-1.
 * The outside world will count disks 1 to 6, quads A to D, strips 1 to Nphi and 1 to Nrad. 
*********************************************************************
 */

#include <TObject.h>
#include <TVector3.h>

//global constants
enum { kFgtMxDisk=8,    /* # of normal FGT disks @ STAR */
       kFgtMxQuad=4,    /* # of quadrants in single FGT disk */
};

// Strip geometry config
   //---------------- 800(rad)/600(phi) strips
const double kFgtRout           = 37.1;   // cm
const double kFgtRmid           = 18.8;   // cm
const double kFgtRin            = 11.5;   // cm
const double kFgtRadPitch       =  0.08;  // 800 mu
const double kFgtPhiPitch       =  0.06;  // 600 mu
const double kFgtDeadQuadEdge   =  0.1;   //  cm


class StFgtGeom : public TObject  {
  // WARN: use double for all coordinates operation!
  // phi-strips deliver phi-coordinate
  // rad-strips deliver Rxy info

  double  pi,dpi, halfpi;

  // include the 9th disk
  double  mDiskZ[kFgtMxDisk+1];

  double  mRadStripOff;          // strip offset in Rad
  double  mPhiStripOff;          // strip offset in Phi

  int     mRadStripLOCId_number; // calculated in class init 
  int     mPhiStripLOCId_number; // calculated in class init 
  int     mRadStripGBLId_number,  mPhiStripGBLId_number; // in a disk


  void initialize();

 public:
  StFgtGeom();
  ~StFgtGeom(){};

  bool inDisk(TVector3 rLab);
  int getQuad(double phiLab);

  double Rin()  { return kFgtRin; } 
  double Rmid() { return kFgtRmid; } 
  double Rout() { return kFgtRout; } 

  double radStrip_pitch() { return kFgtRadPitch;}          // cm
  double phiStrip_pitch() { return kFgtPhiPitch/Rout();}   // rad

  double yLimit() { return kFgtRout; } 

  double deadQuadEdge() { return kFgtDeadQuadEdge; } // cm, local ref frame
  double maxTof()  {return 80.e-9; } //sec
  double minPmag() {return 0.005; }  //GeV of the track momentum
  double diskZ(int iDisk) { return mDiskZ[iDisk]; }

  double radStripOff() { return mRadStripOff;}
  double phiStripOff() { return mPhiStripOff;}

  int    radStripLOCId_number() { return mRadStripLOCId_number;} // in a quad 
  int    phiStripLOCId_number() { return mPhiStripLOCId_number;} // in a quad 


  int radStripGBLId_number(){ return mRadStripGBLId_number;} // in a disk, used in digRad 
  int phiStripGBLId_number(){ return mPhiStripGBLId_number;} // in a disk  used in digPhi


  double phiQuadXaxis(int iquad);// orientation of local X-axis in LAB,[-pi,pi]

  bool localXYtoStripID( int iquad, double x, double y,int &iRadID,int & iPhiID,  int dbg=0); // false if out of range

  // reverse functions
  double stripID2Rxy( double fRadBin );// interpolates between bins
  double stripID2PhiLoc( double fPhiBin );// interpolates between bins
  double stripID2PhiLab( double fPhiBin );// interpolates between bins

  void printParam();

  // methods involving local IDs
 private:
  int rad2LocalStripId( double rad, double *binFrac=0); // return -1 on error
  int phiLoc2LocalStripId( double phiLoc, double *binFrac=0); // return -1 on error
  int phiLab2LocalStripId( int iquad, double phiLab, double *binFrac=0); // return -1 on error

  int radIdGlobal2Local( int radId ) { return radId % radStripLOCId_number(); }  
  int phiIdGlobal2Local( int phiId ) { return phiId % phiStripLOCId_number(); }
  int radIdLocal2Global( int iquad, int radId ) {
    return radId + radStripLOCId_number() * iquad;
  } 
  int phiIdLocal2Global( int iquad, int phiId ) {
    return phiId + phiStripLOCId_number() * iquad;
  } 

  ClassDef(StFgtGeom,1)  
    
};

#endif


/*********************************************************************
 * $Log: StFgtGeom.h,v $
 * Revision 1.1  2011/04/07 19:31:22  balewski
 * start
 *
 * Revision 1.24  2005/07/15 20:53:09  balewski
 * more get methods
 *
 */
