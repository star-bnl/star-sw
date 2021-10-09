#ifndef AthenaMagField_H
#define AthenaMagField_H
#include "TH2.h"
#include "TVirtualMagField.h"
class AthenaMagField : public TVirtualMagField {
 public:
  AthenaMagField ();
  virtual ~AthenaMagField () { 
    fgInstance = 0; 
    SafeDelete(fBr);
    SafeDelete(fBz);
  }
  virtual void    Field    ( const Double_t *x, Double_t *B );
  static AthenaMagField *Instance() {return fgInstance;}
 private:
  static AthenaMagField *fgInstance;
  TH2F *fBr, *fBz;
  ClassDef(AthenaMagField,1)    // Base class for all ATHENA MagField
};

#endif
