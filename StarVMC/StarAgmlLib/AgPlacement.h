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

  enum { kOnly=0, kMany };

 private:
 protected:

  bool mHasRotm;

public:

  struct Rotation3 
  {
    double alpha;
    int    axis;
  };

  struct Rotation6
  {
    double thetax, phix, thetay, phiy, thetaz, phiz;
  };

  union Rotation
  {
    Rotation3 rot3;
    Rotation6 rot6;
    short     ort[3];
  };

  struct Translation
  {
    double x;
    double y;
    double z;
    Translation() : x(0), y(0), z(0){ };
  };

  enum { kUnknown, kRot3, kRot6, kRotO };
  enum { kAlphaX,  kAlphaY, kAlphaZ };

  Translation             mTranslation;
  std::vector< Rotation > mRotation;
  std::vector< int      > mType;
  double                  mRotationMatrix[9];


 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

  ClassDef(AgPlacement,0);
};

#endif
