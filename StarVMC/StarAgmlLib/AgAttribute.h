#ifndef __AgAttribute_h__
#define __AgAttribute_h__

#include "TNamed.h"
#include "TString.h"

#include <vector>
#include <map>

#include <StarVMC/StarAgmlUtil/AgParameterList.h>

class AgBlock;

class AgAttribute : public TNamed, public AgParameterList<double>
{
 public:
  AgAttribute(const Char_t *name="none");
  AgAttribute( const AgAttribute &other );
  ~AgAttribute(){ /* nada */ };

#if 0 // lift
  /// Returns a reference to the named parameter.
  Double_t &par( const Char_t *name );
#endif

  Double_t &operator()(const Char_t *name) { return par(name); }

#if 0 // lift
  /// Tests whether the named parameter is set for this shape
  Bool_t isSet( const Char_t *par ) const;  
  /// Tests whether the named parameter is valid for this shape
  Bool_t hasPar( const Char_t *par ) const;
  /// Unset a parameter
  Bool_t unSet( const Char_t *par );
#endif

  void Inherit( AgBlock *prev );
  void Print( const Option_t *otps="" ) const;
  
 private:
 protected:
  //  static std::vector< TString > mParList;
  //  std::map< TString, Double_t > mParameters;
  friend class _AttributeDummy;
  ClassDef(AgAttribute,1);


 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

}; 

#endif
