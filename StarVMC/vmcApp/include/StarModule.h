#ifndef STARMODULE_H
#define STARMODULE_H

/* $Id: StarModule.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>
#include <TArrayI.h>

// 
class StarModule: public TNamed {

 public:
  StarModule();
  StarModule(const char* name_, const char *title_);
  StarModule(const StarModule &mod);
  virtual ~StarModule();


  virtual void CreateGeometry(void)=0;



  // Module composition
  virtual void StarMaterial(Int_t imat, const char* name, Float_t a, 
			    Float_t z, Float_t dens, Float_t radl,
			    Float_t absl, Float_t *buf=0, Int_t nwbuf=0) const;

  virtual void GetStarMaterial(Int_t imat, char* name, Float_t &a, 
			       Float_t &z, Float_t &dens, Float_t &radl,
			       Float_t &absl) const;

  virtual void StarMixture(Int_t imat, const char *name, Float_t *a,
			   Float_t *z, Float_t dens, Int_t nlmat,
			   Float_t *wmat) const;

  virtual void StarMedium(Int_t numed, const char *name, Int_t nmat,
                          Int_t isvol, Int_t ifield, Float_t fieldm,
                          Float_t tmaxfd, Float_t stemax, Float_t deemax,
                          Float_t epsil, Float_t stmin, Float_t *ubuf=0,
                          Int_t nbuf=0) const;

  virtual void StarMatrix(Int_t &nmat, Float_t theta1, Float_t phi1,
                          Float_t theta2, Float_t phi2, Float_t theta3,
                          Float_t phi3) const;
  
 protected:
  TArrayI*               _idMaterials;
  TArrayI*               _idMedia;

 private:

  ClassDef(StarModule,0)
    };
#endif //STARMODULE_H
