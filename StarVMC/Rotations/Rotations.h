// $Id: Rotations.h,v 1.8 2009/01/21 23:57:42 fisyak Exp $
// $Log: Rotations.h,v $
// Revision 1.8  2009/01/21 23:57:42  fisyak
// Add dead area accounting strip stereo
//
// Revision 1.7  2009/01/16 16:01:14  fisyak
// Adjust SVT geometry topology for alignment
//
// Revision 1.6  2009/01/14 16:31:52  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.5  2008/11/17 14:16:43  fisyak
// add comments
//
// Revision 1.4  2008/09/03 20:44:48  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
// Revision 1.3  2008/08/27 21:48:12  fisyak
//
// Standard Rotations  Master = x + Rotation * Local
#include <vector>
#include <string.h>
#include "Riostream.h"
#include "TMath.h"
#ifndef __NO_TGEO__
#include "TGeoMatrix.h"
#include "TGeoManager.h"
#endif /* ! __NO_TGEO__ */
class rotm_t {
 public:
  rotm_t(const Char_t *Name = "", 
	 Float_t thet1 = 0, Float_t phi1 = 0, 
	 Float_t thet2 = 0, Float_t phi2 = 0, 
	 Float_t thet3 = 0, Float_t phi3 = 0) : 
    Thet1(thet1), Phi1(phi1), Thet2(thet2), Phi2(phi2), Thet3(thet3), Phi3(phi3) {strcpy(name, (Char_t *) Name);}
    //  static vector<rotm_t> fgRotaions;
  void Print() {std::cout 
      << name 
      << "\t" <<  Thet1 
      << "\t" <<   Phi1 
      << "\t" <<  Thet2 
      << "\t" <<   Phi2 
      << "\t" <<  Thet3 
      << "\t" <<   Phi3 
      << std::endl;}
  Char_t name[10];
  Float_t Thet1,    Phi1,   Thet2,    Phi2,   Thet3,    Phi3;
  static void Rotations();
#ifndef __NO_TGEO__
  TGeoRotation *GetRotM() {return new TGeoRotation(name,Thet1,Phi1,Thet2,Phi2,Thet3,Phi3);}
  ClassDef(rotm_t,0)
#endif /* ! __NO_TGEO__ */
};
