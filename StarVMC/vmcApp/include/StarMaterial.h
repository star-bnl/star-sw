#ifndef STARMATERIAL_H
#define STARMATERIAL_H

/* $Id: StarMaterial.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>

// 
class StarMaterial: public TNamed {

 public:
  StarMaterial();
  StarMaterial(const char* name_, const char *title_);
  StarMaterial(const StarMaterial &mod_);
  virtual ~StarMaterial();

  int  GetNumber(void)    const {return _number;}
  void SetNumber(Int_t n_)      {_number=n_;}

  static StarMaterial*    GetInstance() {return 0;} // dummy

  static void             Material(const char* name_, Double_t a_,  Double_t z_,
				   Double_t dens_, Double_t radl_, Double_t absl_);

  static void             Mixture (const char* name_, Float_t  *a_, Float_t *z_, 
				   Double_t dens_, Int_t nlmat_,   Float_t* wmat_);

  static Int_t            GetNextNumber(void)    {return ++_nMaterials;}

  static StarMaterial*    FindMaterial(const char* name_);

 protected:

 private:
  Int_t         _number;

  static Int_t  _nMaterials;
  static TList  _materials;

  ClassDef(StarMaterial,0)
    };
#endif //STARMATERIAL_H
