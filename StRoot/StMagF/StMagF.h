#ifndef StMagF_H
#define StMagF_H

#include "TNamed.h"
#include "TVector.h"

enum Field_t {Undef=1, Const=1, ConMesh=2};

class StMagF : public TNamed {

protected:
  Int_t     fType;   // Type allows for different implementations. 
  Int_t     fMap;    // Field Map identifier
  Float_t   fFactor; // Multiplicative factor (allows sign reversal)

public:
  void agufld_ (float *x, float *b);
  StMagF(){}
  StMagF(const char *name, const char *title, const Field_t map=ConMesh, 
	  const Float_t factor=1);
  virtual ~StMagF() {}
  virtual void Field(Float_t *x, Float_t *b);
  virtual Field_t Type() {return (Field_t) fType;}
  virtual void ReadField() {}
  
  ClassDef(StMagF,1)  //Base class for all STAR MagField
};

class StMagFC  : public StMagF
{
  //STAR Constant Magnetic Field
private:

public:
  StMagFC(){}
  StMagFC(const char *name, const char *title, const Field_t map, 
	   const Float_t factor);
  virtual ~StMagFC() {}
  virtual void Field(Float_t *x, Float_t *b);
  virtual void ReadField() {}
  
  ClassDef(StMagFC,1)  //Class for all STAR Constant MagField 
};

class StMagFCM : public StMagF
{
  //STAR Magnetic Field with constant mesh in z,r,phi
protected:


  Float_t    fZbeg;  // Start of mesh in z
  Float_t    fZdel;  // Mesh step in z
  Int_t      fZn;    // Number of mesh points in z
  Float_t    fRbeg;  // Start of mesh in r
  Float_t    fRdel;  // Mesh step in r
  Int_t      fRn;    // Number of mesh points in r
  Float_t    fPbeg;  // Start of mesh in phi(degrees)
  Float_t    fPdel;  // Mesh step in phi (degrees)
  Int_t      fPn;    // Number of mesh points in phi
  Double_t   fPdeli; // Inverse of Mesh step in phi
  Double_t   fRdeli; // Inverse of Mesh step in r
  Double_t   fZdeli; // Inverse of Mesh step in z

  TVector   *fB;     //! Field map
public:
  StMagFCM(){}
  StMagFCM(const char *name, const char *title, const Field_t map, 
	   const Float_t factor);
  virtual ~StMagFCM() {}
  virtual void Field(Float_t *x, Float_t *b);
  virtual void ReadField();

  inline Float_t Bp(const Int_t ip, const Int_t ir, const Int_t iz) {
    return (*fB)(3*(iz*(fPn*fRn)+ir*fPn+ip));
  }
  inline Float_t Br(const Int_t ip, const Int_t ir, const Int_t iz) {
    return (*fB)(3*(iz*(fPn*fRn)+ir*fPn+ip)+1);
  }
  inline Float_t Bz(const Int_t ip, const Int_t ir, const Int_t iz) {
    return (*fB)(3*(iz*(fPn*fRn)+ir*fPn+ip)+2);
  }
  
  ClassDef(StMagFCM,1)  //Class for all STAR MagField with Constant Mesh
};

#endif
#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class StMagF;
#pragma link C++ class StMagFC;
#pragma link C++ class StMagFCM;

#endif
