#ifndef __AgStructure_h__
#define __AgStructure_h__

#include "TNamed.h"
#include "TFolder.h"
#include <iostream>
#include "Mortran.h"

/**
 *
 * \class AgStructure
 * \author Jason C. Webb
 *
 * AgStructure is the base class for AgML structures.
 *
 */


class AgStructure : public TNamed
{
 public:

  /// Default class ctor
  AgStructure();
  /// Class Constructor
  AgStructure(const Char_t *name, const Char_t *title);
  /// Class Destructor
  ~AgStructure(){ /* nada */ };

  Int_t  _index; // !$ Instance counter
  Bool_t _inUse; // !$ In use flag

  /// Get the list of member variables for this structure
  /// std::vector<TString> GetListOfMembers();

  /// Returns a reference to the top-level AgML structure folder
  static TFolder &top(){ return _top; }

  /// Get a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t GetMember( const Char_t *member, Int_t    &value );

  /// Set a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t SetMember( const Char_t *member, Int_t    &value );

  /// Get a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t GetMember( const Char_t *member, Float_t  &value );

  /// Set a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t SetMember( const Char_t *member, Float_t  &value );
 
  /// Get a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t GetMember( const Char_t *member, Double_t &value );

  /// Set a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t SetMember( const Char_t *member, Double_t &value );

  /// Get a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t GetMember( const Char_t *member, TString  &value );

  /// Set a member variable defined on the derived class of
  /// this structure.  If successful, it returns true.
  Bool_t SetMember( const Char_t *member, TString  &value );

  Bool_t GetMember( const Char_t *member, Array_t<Int_t> &value );
  Bool_t SetMember( const Char_t *member, Array_t<Int_t> &value ); 

  Bool_t GetMember( const Char_t *member, Array_t<Float_t> &value );
  Bool_t SetMember( const Char_t *member, Array_t<Float_t> &value );
 
  Bool_t GetMember( const Char_t *member, Array_t<Double_t> &value );
  Bool_t SetMember( const Char_t *member, Array_t<Double_t> &value );
   
  void fill();

  /// Default USE operator, selects instance with _index=1
  Bool_t Use(); 
  Bool_t Use( const Char_t *member, Double_t value );
  Bool_t Use( const Char_t *member, TString  value );
  Bool_t Use( const Char_t *member, Int_t    value );
  Bool_t Use( const Char_t *member, Float_t  value );

  /// Finds the last defined version of this structure
  Bool_t Last();

  std::ostream &Out( std::ostream &out );
  std::ostream &operator << (std::ostream &out ) 
  {
    return Out( out );
  }

  /// Registers module for manipulation of data structures
  static Bool_t AgDetpNew( const Char_t *module, const Char_t *title=0 );
  
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Int_t value      );
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Float_t value    );
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Double_t value   );
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, TString  value   );

  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Array_t<Int_t> value   );
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Array_t<Float_t> value );
  static Bool_t AgDetpAdd( const Char_t *structure, const Char_t *member, Array_t<Double_t> value );

 private:
 protected:
  
  static TFolder _top;

  /// Copies the data members defined on the instance of AgStructure
  Bool_t CopyDataMembers( AgStructure *other, const Char_t *only=0, Bool_t verbose=false );

  /// Counts the number of versions of this structure in the folder
  Int_t Count();

  /// Executes AgDetp commands
  Bool_t AgDetpSet();

  Int_t _use;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }



};

#endif
