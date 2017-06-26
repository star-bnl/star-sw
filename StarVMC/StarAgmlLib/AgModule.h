#ifndef __AgModule_h__
#define __AgModule_h__

#include "AgBlock.h"
#include "Mortran.h"
#include "AgDetp.h"
#include "AgStructure.h"
#include <string>
using namespace std;

class TDataSet;

struct HitSet_t {
  Char_t meas[9]; // measurement
  float  nb; // number of bins or bits
  float  min; // min
  float  max; // max
  Char_t opts[4]; // Options    
};

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

  TDataSet *DataSet(){ return mDataSet; }
  static TDataSet *Geom();

  // Register a hit declaration in this module
  Bool_t AddHit( string _for, string meas, Float_t bits, Float_t mn=0, Float_t mx=0, string opts="" );
  Bool_t AddCut( string block, string cut, Float_t value );
  Bool_t AddPar( string block, string par, Float_t value );


 private:
 protected:

  std::map< TString, AgBlock *>    mBlocks;
  static std::vector< TString, AgDetp * >    mDetectorParameters;

  // Module dataset
  TDataSet *mDataSet;
  TDataSet *mHitsSet;
  TDataSet *mBlocksSet;
  static TDataSet *mGeomSet;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }


};

#endif
