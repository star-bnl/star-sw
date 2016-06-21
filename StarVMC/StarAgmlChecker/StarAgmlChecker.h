#ifndef __StarAgmlChecker_h__
#define __StarAgmlChecker_h__

#include "TGeoChecker.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"

#include "TObjectSet.h"
#include "TMath.h"
#include "TMD5.h"
#include <map>
// Shapes 
#include "TGeoArb8.h"
#include "TGeoBBox.h"
#include "TGeoCone.h"
#include "TGeoTube.h"
#include "TGeoEltu.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoHype.h"
#include "TGeoPara.h"
#include "TGeoSphere.h"
#include "TGeoTorus.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include <assert.h>

class TStopwatch;

class CheckSum_t 
{ 
public:
  CheckSum_t(){ mCount=0; };
  virtual ~CheckSum_t(){ };

  operator TMD5()   { TMD5 final = mResult; final.Final(); return final; }      /// Cast to TMD5
  operator TString(){ TMD5 final = *((TMD5 *)this); return final.AsString(); }  /// Cast to TString.
  operator const Char_t*(){ TMD5 final = *((TMD5 *)this); return final.AsString(); }  /// Cast to c-string

  /// Cast to 

  TMD5  operator+=( const Int_t      &rhs ){ assert(sizeof(rhs)==4); return add(rhs); } 
  TMD5  operator+=( const Long_t     &rhs ){ assert(sizeof(rhs)==4); return add(rhs); }
  TMD5  operator+=( const Long64_t   &rhs ){ assert(sizeof(rhs)==8); return add(rhs); } 
  TMD5  operator+=( const ULong_t    &rhs ){ assert(sizeof(rhs)==4); return add(rhs); }
  TMD5  operator+=( const ULong64_t  &rhs ){ assert(sizeof(rhs)==8); return add(rhs); } 
  TMD5  operator+=( const Float_t    &rhs ){ assert(sizeof(rhs)==4); return add(rhs); }
  TMD5  operator+=( const Double_t   &rhs ){ assert(sizeof(rhs)==8); return add(rhs); }
  TMD5  operator+=( const Bool_t     &rhs ){ assert(sizeof(rhs)==1); return add(rhs); }
  TMD5  operator+=( const TString    &rhs ){                       ; return add(rhs); }
  TMD5  operator+=( const char*      &rhc ){ TString rhs(rhc);       return add(rhs); }

  /// Total number of values checked.
  Int_t count(){ return mCount; }
  
private:
protected:
  TMD5  mResult; // resulting checksum
  Int_t mCount;

  TMD5  add( Int_t     a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( Long_t    a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( Long64_t  a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( ULong_t   a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( ULong64_t a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( Bool_t    a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( Float_t   a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( Double_t  a ){ mCount++; UChar_t *b = (UChar_t *)&a; mResult.Update(b,sizeof(a)); return *this; }
  TMD5  add( TString   a ){ mCount++; UChar_t *b = (UChar_t *)a.Data(); mResult.Update(b,a.Length()); return *this; }

  ClassDef(CheckSum_t,1);

};


class StarAgmlChecker : public TGeoChecker
{
 public:
  StarAgmlChecker( TGeoManager *manager );
  ~StarAgmlChecker(){ /* nada */ };

  /// Produces plots showing the total amount of material within the top 
  /// volume, and the total amount of material in all of the daughters of
  /// the top volume.  Material is measured in number of radiation lengths.
  /// The plot is a 2D plot phi vs eta, wrapped by a TObjectSet.
  /// @param top Top volume
  /// @param nEta number of bins in eta
  /// @param mnEta minimum eta
  /// @param mxEta maximum eta
  /// @param nPhi number of bins in phi
  /// @param mnPhi minimum phi
  /// @param mxPhi maximum phi
  /// @param rmin  minimum radius
  /// @param rmax  maximum radius
  TObjectSet *MaterialPlot( const Char_t   *top   ="CAVE"  , 
			    const Int_t     nEta  =   100  , 
			    const Double_t  mnEta = -5.00  ,
			    const Double_t  mxEta = +5.00  ,
			    const Int_t     nPhi  =   360  ,
			    const Double_t  mnPhi = -TMath::Pi() ,
			    const Double_t  mxPhi = +TMath::Pi() ,
			    const Double_t  rmin  =    0.0 ,
			    const Double_t  rmax  =    400.0, // ~30cm outside of MAGP
			    const Double_t  zmin  = -4000.0,
			    const Double_t  zmax  = +4000.0,
			    const Option_t *opts = "top" );

  /// Provides a checksum for the volume
  TMD5 CheckSum( const Char_t *volume );

  /// Returns a "checksum data set"... i.e. the geometry tree with each node reduced
  /// to its checksum.
  TDataSet *CheckSet( const Char_t *volume="HALL" );

  void Skip( const Char_t *vol ){ mSkipList.push_back(vol); }
  

 private:
 protected:
  void Fill( TObjectSet *set, Double_t rmin, Double_t rmax, Double_t zmin, Double_t zmax );

  std::vector<TString> mSkipList;

  TStopwatch *mTimer;
  Bool_t     *mFlags;
  Double_t   *mVal1, *mVal2;

  std::map< TGeoVolume *, CheckSum_t *> mCheckSum; // stores volume-wise checksum

  void Update( CheckSum_t *sum, TGeoMaterial *mat ); /// Adds material to checksum
  void Update( CheckSum_t *sum, TGeoNode     *nod ); /// Adds node to checksum
  void Update( CheckSum_t *sum, TGeoMatrix   *mat ); /// Adds matrix to checksum
  void Update( CheckSum_t *sum, TGeoMedium   *med ); /// Adds medium to checksum

  void Update( CheckSum_t *sum, TGeoShape    *sha ); /// Adds shape to checksum
  void Update( CheckSum_t *sum, TGeoBBox * );
  void Update( CheckSum_t *sum, TGeoArb8 * );
  void Update( CheckSum_t *sum, TGeoTrap * );
  void Update( CheckSum_t *sum, TGeoGtra * );
  void Update( CheckSum_t *sum, TGeoCone * );
  void Update( CheckSum_t *sum, TGeoConeSeg * );
  void Update( CheckSum_t *sum, TGeoTube * );
  void Update( CheckSum_t *sum, TGeoTubeSeg * );
  void Update( CheckSum_t *sum, TGeoCtub * );
  void Update( CheckSum_t *sum, TGeoEltu * );
  void Update( CheckSum_t *sum, TGeoHype * );
  void Update( CheckSum_t *sum, TGeoPara * );
  void Update( CheckSum_t *sum, TGeoPcon * );
  void Update( CheckSum_t *sum, TGeoPgon * );
  void Update( CheckSum_t *sum, TGeoSphere * );
  void Update( CheckSum_t *sum, TGeoTorus * );
  void Update( CheckSum_t *sum, TGeoTrd1 * );
  void Update( CheckSum_t *sum, TGeoTrd2 * );

  void Update( CheckSum_t *sum, TGeoVolume *);

  ClassDef(StarAgmlChecker,1);

};

#endif
