#ifndef __AgMaterial_h__
#define __AgMaterial_h__

#include "TNamed.h"
#include "TString.h"

#include <map>
#include <vector>

//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>
class AgMaterial;
class AgBlock;

class AgMaterial : public TNamed, public AgParameterList<double>
{
 public:

  AgMaterial(const Char_t *name="None"); // default value should not be changed
  ~AgMaterial();

  AgMaterial( const AgMaterial &other );

  enum { kUnknown=0, kMaterial, kMixture, kCompound };


  /// Returns a reference to the named parameter.
  Double_t &par( const Char_t *name );

#if 0 // lifted
  /// Tests whether the named parameter is set for this shape
  Bool_t isSet( const Char_t *par ) const;  
  /// Tests whether the named parameter is valid for this shape
  Bool_t hasPar( const Char_t *par ) const;
#endif

  /// Tests wether the mixture has the named component.  Returns false
  /// if this is a material.
  Bool_t hasComponent( const Char_t *comp ) const;

  /// Inherit properties from specified material
  void Inherit( AgBlock *other );

  /// Prints the details about the material
  void Print( Option_t *opts="" ) const;

  /// Lists all defined materials
  static void List( Option_t *opts="ALL" );

  /// Returns the enumerated type.  
  /// kMaterial is a material specifying a, z, etc... directly
  /// kMixture  is a mixture by fractional weight
  /// kCompound is a mixture by atomic formula
  Int_t type(){ return mType; }

  /// Adds components to a mixture.  If a weight > 1 is specified,
  /// the mixture will be done by atomic formula.  Else by fractional
  /// weight.
  void Component( const Char_t *name, Double_t a, Double_t z, Double_t weight );

  /// Returns a copy of the named material
  static AgMaterial CopyMaterial(const Char_t *name);

  /// Get a reference to the named material.  If the material does not
  /// exist, it is created with unset parameters.
  static AgMaterial &Get( const Char_t *name );

  /// Returns true if the requested material
  /// exists, false otherwise
  static Bool_t IsDefined(const Char_t *name);

  /// Gets the number of components for this material.  Returns 0 if this
  /// is not a mixture
  Int_t numberOfComponents();

  /// Returns (by reference) the name, A, Z and Weight of each component
  void Component( Int_t ic, TString &name, Double_t &a, Double_t &z, Double_t &weight );

  
  Bool_t locked(){ return mLock; }

  /// Locks the material and (silently) prevents any changes from being made.
  /// The locking mechanism is provided to protect the main material database.
  /// Users may change their materials at will.
  void   lock(){ mLock=true; }

  Double_t sumWeights();
 
 private:
 protected:

  // lifted  std::map< TString, Double_t > mParameters;

  /// Find and return a pointer to the specified material
  static AgMaterial *Find( const Char_t *name );

  // Components
  std::vector< TString >  mC;
  std::vector< Double_t > mA, mZ, mW;
public:	
  struct MyComponent {
	  Double_t a;
	  Double_t z;
	  Double_t w;
  };
protected:
  std::map< TString, MyComponent > mComponentTable; // Table storing already defined components

  static std::map< TString, AgMaterial * > mMaterialTable; // List of existing materials  
  static std::vector< TString >            mMaterialList;  // List of defined materials
  // lifted  static std::vector< TString >            mParameterList; // List of legal parameters

  Bool_t mLock;
  Int_t  mType;

  void Average();

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

  friend class _MaterialDummy;

};

// ------------------------------------------------------------------------------------------------------------

#endif
