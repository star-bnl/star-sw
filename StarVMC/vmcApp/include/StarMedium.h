#ifndef STARMEDIUM_H
#define STARMEDIUM_H

/* $Id: StarMedium.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>



// 
class StarMedium: public TNamed {

 public:

  StarMedium();
  StarMedium(const char* name_, const char *title_, Int_t sensitive_);
  StarMedium(const StarMedium &med_);
  virtual ~StarMedium();

  Int_t GetNumber(void) const    {return _number;}
  void  SetNumber(Int_t n_)      {_number=n_;}

  Int_t IsSensitive(void) const {return  _sensitive;}

  static StarMedium* GetInstance() {return 0;} // dummy

  static void Medium(const char* name_, const char* materialName_,
		     Int_t isvol_,
		     Int_t ifield_,    Double_t fieldm_, Double_t tmaxfd_,
		     Double_t stemax_, Double_t deemax_, Double_t epsil_,
		     Double_t stmin_);

  static Int_t          GetNextNumber(void)    {return ++_nMedia;}

  static StarMedium*    FindMedium(const char* name_);

  static const Int_t    Sensitive;
  static const Int_t    Insensitive;

 protected:

 private:
  Int_t         _number;
  Int_t         _sensitive;  
  static Int_t  _nMedia;
  static TList  _media;

  ClassDef(StarMedium,0)
    };
#endif //STARMEDIUM_H
