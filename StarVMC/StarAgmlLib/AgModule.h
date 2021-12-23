#ifndef __AgModule_h__
#define __AgModule_h__

#include "AgBlock.h"
#include "Mortran.h"
#include "AgDetp.h"
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


  TDataSet *DataSet(){ return mDataSet; }
  static TDataSet *Geom();

  void SetTrackingFlag( int flag ){ mTrackingFlag = flag; }
  int  GetTrackingFlag(){ return mTrackingFlag; }

 private:
 protected:

  std::map< TString, AgBlock *>    mBlocks;
  static std::vector< TString, AgDetp * >    mDetectorParameters;

  // Module dataset
  TDataSet *mDataSet;
  TDataSet *mHitsSet;
  TDataSet *mBlocksSet;
  static TDataSet *mGeomSet;

  short mTrackingFlag;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }

  ClassDef (AgModule,0);

};

#endif
