#ifndef __AgMedium_h__
#define __AgMedium_h__

#include "TNamed.h"
#include "TString.h"

#include <vector>
#include <map>

//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>
class AgBlock;
class AgMedium : public TNamed, public AgParameterList<double>
{
 public:
  AgMedium(const Char_t *name="None");
  AgMedium(const AgMedium &other );
  ~AgMedium(){ /* nada */ };

#if 0 // lifted to AgParameterList
  /// Returns a reference to the named parameter.
  Double_t &par( const Char_t *name );
  /// Tests whether the named parameter is set for this shape
  Bool_t isSet( const Char_t *par ) const;  
  /// Tests whether the named parameter is valid for this shape
  Bool_t hasPar( const Char_t *par ) const;
#endif 

  /// Inherit properties from mother block
  void Inherit( AgBlock *other );
  /// 
  static AgMedium CopyMedium( const Char_t *name );
  static AgMedium &Get( const Char_t *name );

  /// Returns true if the requested medium
  /// exists, false otherwise
  static Bool_t IsDefined(const Char_t *name);

  Bool_t isEqual( const AgMedium &other );
  Bool_t operator==(const AgMedium &other ){ return isEqual(other); }

  /// Prints the details about the medium
  void Print( Option_t *opts="" ) const;

  /// Lists all defined media
  static void List( Option_t *opts="ALL" );

 private:
 protected:

  /// Find and return a pointer to the specified material
  static AgMedium *Find( const Char_t *name );

#if 0 // lifted  
  static std::vector< TString > mParameterList;
  std::map< TString, Double_t > mParameters;
#endif
  static std::map< TString, AgMedium * > mMediumTable;
  static std::vector< TString > mMediumList;
  friend class _MediumDummy;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }


}; 

#endif
