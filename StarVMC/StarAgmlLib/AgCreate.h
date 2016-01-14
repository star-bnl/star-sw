#ifndef __AgCreate_h__
#define __AgCreate_h__

#include "TNamed.h"
#include <vector>
#include <map>

#include "AgShape.h"
//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>
class AgCreate : public TNamed, public AgParameterList<double>
{
 public:

  /// Class constructor
  AgCreate( const Char_t *block="Unknown" );
  /// Copy constructor
  AgCreate( const AgCreate &other );
  /// Class destructor
  ~AgCreate();

  AgCreate &operator=( const AgCreate& other );

#if 0 // lift 
  /// Returns a reference to the named parameter
  Double_t &par( const Char_t *name );
  /// Tests whether the named parameter is set
  Bool_t isSet( const Char_t *par ) const;  
#endif

  /// Takes a reference to the specified shape, loops over
  /// all parameters defined in the create object and sets
  /// them iff:
  /// (1) The par is valid for the given shape
  /// (2) The par has not been set for the shape
  void SetParameters( AgShape &shape );

  void Print( const Option_t *opts="" ) const;

 private:
 protected:

  /// Map of key=value pairs
  //  std::map<TString, Double_t>                   mParameters;  

  /// Root dictionary interface
  ClassDef(AgCreate,1);

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

};

#endif
