/* $Id: StarMaterial.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include <TVirtualMC.h>
#include <iostream.h>
#include "StarMaterial.h"

ClassImp(StarMaterial)

;

Int_t StarMaterial::_nMaterials(0);
TList StarMaterial::_materials;

//_______________________________________________________________________
StarMaterial::StarMaterial() {}
StarMaterial::StarMaterial(const StarMaterial& g_) {}
StarMaterial::StarMaterial(const char* name_, const char *title_): TNamed(name_,title_) {}
StarMaterial::~StarMaterial() {}

//-------------------------------------------------------------------------
StarMaterial* StarMaterial::FindMaterial(const char* name_) {
    return (StarMaterial*) StarMaterial::_materials.FindObject(name_);
  }
//-------------------------------------------------------------------------
void StarMaterial::Mixture(const char* name_, Float_t* a_, Float_t* z_, 
			   Double_t dens_, Int_t nlmat_, Float_t* wmat_) {

  // The number is set in the GeoManager and returned here
  Int_t serial = 0; //GetNextNumber();

  gMC->Mixture(serial, name_, a_, z_, dens_, nlmat_, wmat_);

  StarMaterial* m = new StarMaterial(name_,"mixture");
  m->SetNumber(serial);
  StarMaterial::_materials.Add(m);

  cout<<"Created mixture:  "<<name_<<" with serial: "<<serial<<endl;
}
//-------------------------------------------------------------------------
void StarMaterial::Material(const char* name_, Double_t a_,  Double_t z_,
			    Double_t dens_, Double_t radl_, Double_t absl_) {

  Float_t* ubuf_ = 0;

  // The number is set in the GeoManager and returned here
  Int_t serial = 0; //GetNextNumber();

  gMC->Material(serial, name_, a_, z_, dens_, radl_, absl_, ubuf_, 0);  

  StarMaterial* m = new StarMaterial(name_,"material");
  m->SetNumber(serial);
  StarMaterial::_materials.Add(m);

  cout<<"Created material: "<<name_<<" with serial: "<<serial<<endl;
 }
