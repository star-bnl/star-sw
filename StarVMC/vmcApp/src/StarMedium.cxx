/* $Id: StarMedium.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include <TVirtualMC.h>
#include <iostream.h>
#include "StarMedium.h"
#include "StarMaterial.h"

ClassImp(StarMedium)

;


Int_t StarMedium::_nMedia(0);
TList StarMedium::_media;

const Int_t StarMedium::Sensitive(1);
const Int_t StarMedium::Insensitive(0);

//_______________________________________________________________________
StarMedium::StarMedium() {}
StarMedium::StarMedium(const StarMedium& g_) {}
StarMedium::StarMedium(const char* name_, const char *title_, Int_t s_): TNamed(name_,title_), _sensitive(s_) {}
StarMedium::~StarMedium() {}

//-------------------------------------------------------------------------
StarMedium* StarMedium::FindMedium(const char* name_) {
    return (StarMedium*) StarMedium::_media.FindObject(name_);
  }
//_______________________________________________________________________
void StarMedium::Medium(const char* name_, const char* materialName_,
			Int_t isvol_,
			Int_t ifield_,    Double_t fieldm_, Double_t tmaxfd_,
			Double_t stemax_, Double_t deemax_, Double_t epsil_,
			Double_t stmin_) {

  Float_t* ubuf_ = 0;
  // The medium number is set in the GeoManager and returned here
  Int_t serial = 0; // GetNextNumber();

  Int_t imat=StarMaterial::FindMaterial(materialName_)->GetNumber();

  gMC->Medium(serial, name_,imat, isvol_, ifield_, fieldm_, tmaxfd_, stemax_, deemax_, epsil_, stmin_, ubuf_, 0); 

  StarMedium* m = new StarMedium(name_,"medium",isvol_);
  m->SetNumber(serial);
  StarMedium::_media.Add(m);

  cout<<"Created medium: "<<name_<<" serial: "<<serial<<" with material: "<<imat<<" sensitivity: "<<isvol_<<endl;
}
