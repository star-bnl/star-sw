#ifndef __StarEmbedMaker_h__
#define __StarEmbedMaker_h__
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "TLorentzVector.h"
#include <string>
#include <TTree.h>
#include <TFile.h>
class StarEmbedMaker : public StarPrimaryMaker
{
public:
  StarEmbedMaker( const char* name="StarEmbed" );
 ~StarEmbedMaker();

  void SetInputFile( const char* filename );

  int Make();
  int Init();

  void Clear( const Option_t* opts="" );

private:
protected:

  bool mFzdInput;

  std::string mFilename; //
  TFile*      mFile;
  TTree*      mTree;

  long long mCurrentEntry;            // TODO: Use the actual tags structure (GlobalTags.idl)
  int mRun;
  int mEvent;
  double mVertexX, mVertexY, mVertexZ;
  double mSigmaX, mSigmaY, mSigmaZ;

  unsigned int mTriggerId[32]; 
  unsigned int mNumberOfPrimaries;
  short mPrimaryVertexFlag;
  


  
  ClassDef(StarEmbedMaker,1);
};

#endif
