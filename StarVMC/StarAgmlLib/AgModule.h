#ifndef __AgModule_h__
#define __AgModule_h__

#include "AgBlock.h"
#include "Mortran.h"
#include "AgDetp.h"
#include "AgStructure.h"

class AgModule : public AgBlock
{
 public:
  AgModule(const Char_t *name, const Char_t *comment);
  ~AgModule();

  /// ConstructGeometry is the method which is responsible for realizing
  /// the geometry defined by this module.
  virtual void ConstructGeometry( const Char_t *dummy="" ) { }

  /// AddBlock creates an instance of the named block and adds it to the
  /// list of blocks defined in this module.  Such blocks serve as factories
  /// for the production of the subdetectors defined in the module.
  AgBlock *AddBlock( const Char_t *name, AgBlock *block );

  /// Returns a pointer to the requested data structure
  AgStructure *GetStructure( const Char_t *name );

 private:
 protected:

  std::map< TString, AgBlock *>    mBlocks;
  static std::vector< TString, AgDetp * >    mDetectorParameters;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

  ClassDef(AgModule,1);

};

#endif
