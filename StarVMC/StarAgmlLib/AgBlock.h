#ifndef __AgBlock_h__
#define __AgBlock_h__

#include "TNamed.h"

#include <vector>
#include <map>

class AgBlock;
#include "AgShape.h"
#include "AgMaterial.h"
class AgMixture;
#include "AgMedium.h"
class AgVolume;
class AgModule;
#include "AgPlacement.h"
#include "AgMath.h"
#include "AgAttribute.h"
#include "AgCreate.h"

#include <assert.h>

//class StarAgmlStacker;
#include "StarAgmlStacker.h"

#include "G3Commons.h"

class AgBlock : public TNamed
{

public:

  /// Info-level printout.  Ignores variadic args.
  virtual void Info    ( const Char_t *name, const Char_t *msg, ... ) const;
  /// Warn-level printout.  Ignores variadic args.
  virtual void Warning ( const Char_t *name, const Char_t *msg, ... ) const;
  /// Error-level printout.  Ignores variadic args.
  virtual void Error   ( const Char_t *name, const Char_t *msg, ... ) const;
  /// Fatal-level printout.  Ignores variadic args.
  virtual void Fatal   ( const Char_t *name, const Char_t *msg, ... ) const;

 public:

  AgBlock(const Char_t *name, const Char_t *title);
  ~AgBlock(){ /* nada */ };

  /// Handles inheritance from the mother volume
  void Inherit();                   

  /// Inherit shape properties
  void Inherit( const AgShape     &shape );
  /// Inherit material properties
  void Inherit( const AgMaterial  &material );
  /// Interhit medium properties
  void Inherit( const AgMedium    &medium );
  /// Inherit attributes
  void Inherit( const AgAttribute &attribute );

  /// The specified shape will inherit unset parameters
  /// from the block
  void InheritFrom( AgShape &shape, Bool_t zeros=false );

  /// Creates the named block
  void Create(const Char_t *name ); 

  /// Virtual function to be overriden by the user's block (volume)
  /// definition.  Will be called when the Create operator is called.
  virtual void Block( AgCreate c ) { } 
  virtual void End() { }   


  /// Return a pointer to the shape of this bloc/volume
  AgShape     *shape()    { return &_shape; }
  /// Return a pointer to the material of this block/volume
  AgMaterial  *material() { return &_material; }
  /// Retun a pointer to the medium of this block/volume
  AgMedium    *medium()   { return &_medium; }
  /// Return a pointer to the attributes of this block/volume
  AgAttribute *attribute(){ return &_attribute; }
  
  /// Find the named block and return a pointer to it.
  static AgBlock *Find(const Char_t *name);  

  /// User-hook to perform initialization... [deprecated]
  virtual void Init(){ 
    /* Initialize data strutctures and variables if needed */ 
  };

  /// Specifies the currently active module
  static void SetModule( AgModule *module ){ _module = module; }
  
  /// Returns a pointer to the currently active module
  static AgModule   *module(){ return _module; }

  /// Prints information about the block
  void Print( Option_t *opts="" )const;

  /// Lists all blocks.  If opts is specified, will search for the named
  /// block and lists details about that block
  static void List( Option_t *opts="ALL" );

  /// Returns the previously active block in the history, with offset
  /// allowing the user to navigate back in the history of the creation
  /// of this branch of blocks/volumes.
  static AgBlock *previous(Int_t offset=0);    

  /// Returns a pointer to the currently active block/volume.
  static AgBlock *active() { return mCurrent; }

  /// Returns a pointer to the stacker.
  static StarAgmlStacker *stacker(){ return _stacker; }

  /// Sets the stacker used to create the concrete geometry.
  static void SetStacker( StarAgmlStacker *s ){ _stacker = s; }

  /// Returns a pointer to the mother of this block/volume
  AgBlock *mother(){ return mMother; }

  /// Sets the mother of this block/volume
  void SetMother( AgBlock *b ){ mMother=b; } 

  /// Sets the shape of this block/volume
  void SetShape( AgShape &shape ){ _shape = shape; }

  /// Add a nickname to the list of nicknames of this block
  void addNickname( const Char_t *name ){ mNicknames.push_back(name); }

  /// Returns the next available nickname
  TString nickname(){ 
    assert(mNicknames.size()>0); // This is a very bad block
    return mNicknames.back(); 
  }

  //  Bool_t isSame( const AgAttribute &attr );
  //Bool_t isSame( const AgShape     &shape );

  Bool_t isValid(){ return _valid; }
  void   setValid( Bool_t v=true ){ _valid = v; }

  /// Methods to examine the block creation stack
  static Int_t   nHistory(){ return (Int_t)mStack.size(); }
  /// Methods to examine the block creation stack
  static AgBlock *History(Int_t i){ return mStack[i]; }

  /// Add a reference system group to this block with the specified name.
  /// A reference group will be a virtual volume which may be used to 
  /// independently align multiple toplevel volumes
  void AddGroup( const Char_t *name );//{ mGroups.push_back( name ); }
  
  /// Get the list of alignment groups
  //  const vector<TString> &groups(){ return mGroups; }

  void   SetAssembly() { mMakeAssembly=true; }
  Bool_t GetAssembly() { return mMakeAssembly; }

 private:
 protected:

  AgBlock                             *mMother;     // Mother of this block
  std::vector< AgBlock *>              mDaughters;  // Daughters of this block

  AgShape     _shape;
  AgMaterial  _material;
  AgMedium    _medium;
  AgAttribute _attribute;
  AgVolume   *_volume;
  static AgModule   *_module;

  AgPlacement _placement;
  Bool_t      _valid;

  // Store the paramters passed on the create command
  AgCreate _create;

  static AgBlock                      *mCurrent;    // Currently executing block
  static std::map<TString, AgBlock *>  mBlockTable; // LUT of all blocks in existence
  static std::vector< AgBlock *>       mStack;      // Stack of blocks... 
  static StarAgmlStacker              *_stacker;

  std::vector< TString >               mNicknames;  // Nicknames
  std::vector< TString > mGroups;

  Bool_t mMakeAssembly;

  friend class _AgBlockDummy;


 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

};


#endif 
