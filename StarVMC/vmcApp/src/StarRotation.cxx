/* $Id: StarRotation.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include <TGeoManager.h>
#include <iostream.h>
#include "StarRotation.h"

ClassImp(StarRotation)

;

Int_t StarRotation::_nRotations(0);
TList StarRotation::_rotations;

//_______________________________________________________________________
StarRotation::StarRotation() {}
StarRotation::StarRotation(const StarRotation& g_) {}
StarRotation::StarRotation(const char* name_, const char *title_): TNamed(name_,title_) {}
StarRotation::~StarRotation() {}

//-------------------------------------------------------------------------
StarRotation* StarRotation::FindRotation(const char* name_) {
  return (StarRotation*) StarRotation::_rotations.FindObject(name_);
  }
//-------------------------------------------------------------------------
void StarRotation::Rotation(const char *name_,
			    Double_t theta1_, Double_t phi1_,
			    Double_t theta2_, Double_t phi2_,
			    Double_t theta3_, Double_t phi3_) {


  Float_t* ubuf_ = 0;

  // The number is NOT set in the GeoManager; we make it here
  Int_t serial = GetNextNumber();

  gGeoManager->Matrix(serial,theta1_,phi1_,theta2_,phi2_,theta3_,phi3_);

  StarRotation* r = new StarRotation(name_,"rotation");
  r->SetNumber(serial);
  StarRotation::_rotations.Add(r);

  cout<<"Created rotation: "<<name_<<" with serial: "<<serial<<endl;
 }
