/* $Id: StarModule.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include "StarModule.h"
#include "StarParams.h"

ClassImp(StarModule)


//_______________________________________________________________________
StarModule::StarModule():
  _idMaterials(0),
  _idMedia(0)
{}
//_______________________________________________________________________
StarModule::StarModule(const StarModule& mod_)
 {}

//_______________________________________________________________________
StarModule::StarModule(const char* name_, const char *title_):
  TNamed(name_,title_),
  _idMaterials(new TArrayI(MAX_MATERIALS)),
  _idMedia(new TArrayI(MAX_MEDIA))
 {}

//_______________________________________________________________________
StarModule::~StarModule()
{
}
//_______________________________________________________________________
void StarModule::StarMaterial(Int_t imat, const char* name, Float_t a, 
			      Float_t z, Float_t dens, Float_t radl,
			      Float_t absl, Float_t *buf, Int_t nwbuf) const {return;}

//_______________________________________________________________________
void StarModule::GetStarMaterial(Int_t imat, char* name, Float_t &a, 
				 Float_t &z, Float_t &dens, Float_t &radl,
				 Float_t &absl) const {return;}
//_______________________________________________________________________
void StarModule::StarMixture(Int_t imat, const char *name, Float_t *a,
			     Float_t *z, Float_t dens, Int_t nlmat,
			     Float_t *wmat) const {}
//_______________________________________________________________________
void StarModule::StarMedium(Int_t numed, const char *name, Int_t nmat,
			    Int_t isvol, Int_t ifield, Float_t fieldm,
			    Float_t tmaxfd, Float_t stemax, Float_t deemax,
			    Float_t epsil, Float_t stmin, Float_t *ubuf,
			    Int_t nbuf) const {return;}
//_______________________________________________________________________
void StarModule::StarMatrix(Int_t &nmat, Float_t theta1, Float_t phi1,
			    Float_t theta2, Float_t phi2, Float_t theta3,
			    Float_t phi3) const {return;}

//_______________________________________________________________________
//_______________________________________________________________________

