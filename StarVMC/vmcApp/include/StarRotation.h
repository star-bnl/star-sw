#ifndef STARROTATION_H
#define STARROTATION_H

/* $Id: StarRotation.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>

// 
class StarRotation: public TNamed {

 public:
  StarRotation();
  StarRotation(const char* name_, const char *title_);
  StarRotation(const StarRotation &med_);
  virtual ~StarRotation();

  Int_t GetNumber(void) const    {return _number;}
  void  SetNumber(Int_t n_)      {_number=n_;}

  static StarRotation* GetInstance() {return 0;} // dummy

  static void Rotation(const char *name_,
		       Double_t theta1_, Double_t phi1_,
		       Double_t theta2_, Double_t phi2_,
		       Double_t theta3_, Double_t phi3_);

  static Int_t    GetNextNumber(void)    {return ++_nRotations;}

  static StarRotation*    FindRotation(const char* name_);

 protected:

 private:
  Int_t         _number;
  static Int_t  _nRotations;
  static TList  _rotations;

  ClassDef(StarRotation,0)
    };
#endif //STARROTATION_H
