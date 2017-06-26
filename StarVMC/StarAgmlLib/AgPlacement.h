#ifndef __AgPlacement_h__
#define __AgPlacement_h__

#include "TNamed.h"
//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>
#include "TString.h"
#include <map>

class TGeoCombiTrans;

class AgPlacement : public TNamed, public AgParameterList<double>
{
 public:

  AgPlacement(const Char_t *block="none", const Char_t *mother="none", const Char_t *group="none");
  ~AgPlacement(){ /* nada */ };

  void AlphaX( Double_t ax );
  void AlphaY( Double_t ay );
  void AlphaZ( Double_t az );

  void Ortho( const Char_t *ort="+X+Y+Z" );
  void Reference( Double_t thetax, Double_t phix,
		  Double_t thetay, Double_t phiy, 
		  Double_t thetaz, Double_t phiz );

  void TranslateX( Double_t x ){ mTranslation.x+=x; }
  void TranslateY( Double_t y ){ mTranslation.y+=y; }
  void TranslateZ( Double_t z ){ mTranslation.z+=z; }

  /// Rotation matrix
  void Matrix( Double_t rotm[9] ){ for (int i=0;i<9;i++ ) mRotationMatrix[i]=rotm[i]; mHasRotm=true; }

  void Print( Option_t *opts="" )const;

  TGeoCombiTrans *matrix();

  //  const Char_t *block();
  //  const Char_t *mother();
  //  const Char_t *group();

  /// Returns a reference to the named parameter.
  //  Double_t &par( const Char_t *name );

  /// Tests whether the named parameter is set for this shape
  //  Bool_t isSet( const Char_t *par ) const;  

  enum { kOnly=0, kMany };

 private:
 protected:

  bool mHasRotm;

  //  std::map< TString, Double_t > mParameters;

  //  TString mBlock;
  //  TString mMother;
  //  TString mGroup;

  struct Rotation3 
  {
    Double_t alpha;
    Int_t    axis;
  };
  struct Rotation6
  {
    Double_t thetax, phix, thetay, phiy, thetaz, phiz;
  };
public:
  union Rotation
  {
    Rotation3 rot3;
    Rotation6 rot6;
    Short_t   ort[3];
  };

  struct Translation
  {
    Double_t x;
    Double_t y;
    Double_t z;
    Translation() : x(0), y(0), z(0){ };
  };
protected:
  enum { kUnknown, kRot3, kRot6, kRotO };
  enum { kAlphaX,  kAlphaY, kAlphaZ };

  Translation             mTranslation;
  std::vector< Rotation > mRotation;
  std::vector< Int_t    > mType;
  double                  mRotationMatrix[9];

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

};

#endif
