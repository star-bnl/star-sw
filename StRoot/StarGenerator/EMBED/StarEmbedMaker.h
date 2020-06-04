#ifndef __StarEmbedMaker_h__
#define __StarEmbedMaker_h__
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include <string>
#include <TTree.h>
#include <TFile.h>
class StarEmbedMaker : public StarPrimaryMaker
{
public:
  StarEmbedMaker();
 ~StarEmbedMaker();

  void SetInputFile( const char* filename );

  int Make();
  void Clear( const Option_t* opts="" );

private:
protected:

  bool mFzdInput;

  std::string mFilename; //
  TFile*      mFile;
  TTree*      mTree;

  long long mCurrentEntry;
  int mRun;
  int mEvent;
  double mVertexX, mVertexY, mVertexZ;
  double mSigmaX, mSigmaY, mSigmaZ;

  
  ClassDef(StarEmbedMaker,1);
};

#endif
