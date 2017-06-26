#ifndef __AgShape_h__
#define __AgShape_h__

#include "TNamed.h"
#include <vector>
#include <map>

class _Dummy;
class TGeoShape;

class AgBlock;
class AgModule;

//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>

class AgShape : public TNamed, public AgParameterList<double>
{
 public:

  AgShape(const Char_t *name="unknown");
  AgShape(const AgShape& other );
  ~AgShape(){ /* nada */ };

  enum { kUnknown=0, kBbox=1, kTrd1, kTrd2, kTrap, kTube,  kTubs, kCone, kCons, kSphe,  
	 kPara, kPgon, kPcon,   kEltu,  kHype, kGtra, kCtub, kTorus, 
	 kDivision }; // kDivision should always remain the last in the list


  /// Returns the type of the shape, as specified by the enumerations above
  Int_t  type();

  /// Returns a reference to the named parameter.
  Double_t &par( const Char_t *name );
  //  Double_t GetPar( const Char_t *name );

  /// Returns a reference to the ith slice parameter
  Double_t &Z( UInt_t i );
  /// Returns a reference to the ith slice parameter
  Double_t &Rmin(UInt_t i );
  /// Returns a reference to the ith slice parameter
  Double_t &Rmax( UInt_t i);

  /// Prints the shape parameters.  If an unknown shape was specified,
  /// it prints a list of known shapes and parameters
  void Print(Option_t *opts="") const;

  /// Tests whether this shape is equal to another shape
  Bool_t operator == ( const AgShape &other ) const;

  /// Tests whether the named parameter is set for this shape
  //  Bool_t isSet( const Char_t *par ) const;  
  /// Tests whether the named parameter is valid for this shape
  Bool_t hasPar( const Char_t *par ) const;

  /// Inherit appropriate parameters from anotrher shape
  void Inherit( AgBlock *block );
  
  /// Creates the TGeoShape using the current parameters of this shape
  TGeoShape *Make();

  /// Creates the TGeoShape if the shape parameteers have changed
  TGeoShape *MakeIfUnique();

  /// Returns true if the shape parameters have changed since a previous 
  /// invocation
  Bool_t morphed();

  /// Return a list of the parameters which define this shape
  std::vector< TString > &parList(){ return mParList[mId]; }

  /// Returns true if the shape's parameters are to be set
  /// when positioned
  Bool_t parameterized();

 private:
 protected:

  Int_t                                         mId; // ID of this shape
  //  std::map<TString, Double_t>                   mParameters;
  std::vector<Double_t>                         mZ, mRmin, mRmax;

  static std::map<Int_t, std::vector<TString> > mParList;
  static std::map<TString, Int_t>               mShapeIds;
  static std::map<Int_t, TString>               mShapeNames;
  static std::map<Int_t, TString>               mRootCtors;   // ROOT constructors
  static std::map<Int_t, std::vector<TString> > mRootMembers; // ROOT data members for each parameter

  std::vector<TGeoShape *>                      mStoredShapes;
  std::vector< std::map<std::string,Double_t> > mStoredParams;

  AgBlock                                      *mBlock;   // Pointer to the block which owns this shape
  AgModule                                     *mModule;  // Pointer to the module which owns this shape

  Bool_t                                        mIsRunTime; // Flag indicating that this shape's parameters will be set at position time

  friend class _Dummy; // for automagic initialization

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

};

#endif
