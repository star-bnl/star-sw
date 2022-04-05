#ifndef StFgtContainers_h
#define StFgtContainers_h
/*********************************************************************
 * $Id: StFgtContainers.h,v 1.1 2011/04/07 19:31:22 balewski Exp $
 *********************************************************************
 * Description:
 * STAR Forward Gem Tracker : intermediate data definitions
 *********************************************************************
 */
#include <TVector3.h>
class g2t_fgt_hit_st;

//.... utility vector class for g2t hits
class fgt_g2t_auxil {
 public:
  TVector3 Rlab;// hit entrance in LAB
  TVector3 Rloc, Dloc; // entrance & path in local ref frame
  g2t_fgt_hit_st *hitPtr; // the oryginal g2t hit
  int iQuad; // quadrant of the FGT DISK: [0-3]
  void Clear() { Rlab=Rloc=Dloc=TVector3(0,0,0); hitPtr=0; iQuad=-1;}
  fgt_g2t_auxil() { Clear();}
};

//...... single strip response
class fgt_strip{// response of non-zero strips
 public:
  int id; // globalId = localId + strip_number*iqurd (iquad = 0-3) WMZ
  float adc;
};

//...... 1D clusters in one disk
class fgt_cluster1D{
 public:
  double fBin_mean; // centroid
  int nbin; // total width in bin-units
  double totAmpl, peakAmpl; // a.u. - measure of energy spread
  //... secondary quantities
  int iQuad;  
  double position; // (cm or rad ) LAB position after conversion 
  bool matched;
  fgt_cluster1D() { matched=false; nbin=0; totAmpl=position=0;}
};

#endif
