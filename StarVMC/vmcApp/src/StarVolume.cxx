/* $Id: StarVolume.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include <TVirtualMC.h>
#include <iostream.h>
#include "StarVolume.h"
#include "StarMedium.h"

ClassImp(StarVolume)

;

Int_t StarVolume::_nVolumes(0);
TList StarVolume::_volumes;

//_______________________________________________________________________
StarVolume::StarVolume() {}
StarVolume::StarVolume(const StarVolume& v_) {}
StarVolume::StarVolume(const char* name_, const char *title_): TNamed(name_,title_) {}
StarVolume::~StarVolume() {}

//-------------------------------------------------------------------------
StarVolume* StarVolume::FindVolume(const char* name_) {
  return (StarVolume*) StarVolume::_volumes.FindObject(name_);
  }
//-------------------------------------------------------------------------
StarVolume* StarVolume::FindVolume(      Int_t numb_) {
  return (StarVolume*) StarVolume::_volumes.At(numb_-1); // one-off situation
  }
//-------------------------------------------------------------------------
void StarVolume::Volume(const char* name_,   const char* shape_,  const char* medium_,
			Float_t *upar_, Int_t npar_) {


  // Determine the required medium and its properties:
  StarMedium* sm = StarMedium::FindMedium(medium_);
  Int_t imed=sm->GetNumber();
  Int_t sens=sm->IsSensitive();

  Int_t serial  = GetNextNumber();
  StarVolume* v = new StarVolume(name_,"volume");

  // set properties
  v->SetNumber   (serial);
  v->SetSensitive(sens);

  StarVolume::_volumes.Add(v); // add to our local collection
  gMC->Gsvolu(name_, shape_, imed, upar_, npar_); // finally attach it to the TGeo hierarchy

  cout<<"Created volume: "<<name_<<" with serial: "<<serial<<" with medium: "<<imed<<" sensitivity: "<<sens<<endl;
}

//-------------------------------------------------------------------------
void StarVolume::Division(const char* nameDiv_,   const char* nameMother_, Int_t howMany_, Int_t axis_) {
  StarVolume* mother = FindVolume(nameMother_);
  if(!mother) {
    cout<<"Fatal error: volume "<<nameMother_<<" was not located in the list"<<endl;
    exit(-1);
  }

  Int_t sens=mother->IsSensitive();
  Int_t serial  = GetNextNumber();

  StarVolume* v = new StarVolume(nameDiv_,"division");

  // set properties
  v->SetNumber   (serial);
  v->SetSensitive(sens);

  StarVolume::_volumes.Add(v); // add to our local collection
  gMC->Gsdvn(nameDiv_,nameMother_,howMany_,axis_);
  cout<<"Created division: "<<nameDiv_<<" with serial: "<<serial<<" sensitivity: "<<sens<<endl;
}


// void StarVolume::Volume(const char* name_, Double_t a_,  Double_t z_,
// 			    Double_t dens_, Double_t radl_, Double_t absl_) {

//   Float_t* ubuf_ = 0;
//   Int_t serial = GetNextNumber();

//   StarVolume* m = new StarVolume(name_,"material");
//   m->SetNumber(serial);
//   StarVolume::_materials.Add(m);

//   gMC->Volume(serial, name_, a_, z_, dens_, radl_, absl_, ubuf_, 0);  

//   cout<<"Created material: "<<serial<<endl;
//  }
