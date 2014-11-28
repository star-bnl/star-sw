#ifndef StTofMuDstReader_HH
#define StTofMuDstReader_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StMuTrack;
class StMuEvent;
class StMuTofHit;
class TRandom;
class StMuEvent;
class StMuDst;
class StFileI;
class TChain;
class TClonesArray;
class StMuDstMaker;


#include "TFile.h"
#include "TObject.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"


#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif



class StTofMuDstReader : public StMaker {
  
 protected:
  
 public:

  StMaker* currentChain;
  StTofMuDstReader(const char* name = "StTofMuDstReader",
		   const char* file = "undefined",
		   StMuDstMaker* maker = 0);
  ~StTofMuDstReader();
  void  Clear(const char* opt="");
  Int_t Init();
  
  
  Int_t Make();
  Int_t Finish();
  
 private:

  int eventNumber;
  int oldEventRunId;
  
  TFile* mOutputFile;
  TTree* mTree;

  StMuDst*       mMuDst;
  StMuEvent*     mMuEvent;
  StMuDstMaker*  mMuDstMaker;
  StMuTrack*     mMuTrack;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<Int_t> idVector;
#else
    typedef vector<Int_t,allocator<Int_t>> idVector;
#endif
    typedef idVector::iterator idVectorIter;

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTofMuDstReader.h,v 1.2 2014/08/06 11:43:47 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}	
    
  // the following is a ROOT macro  that is needed in all ROOT accessible code
  ClassDef(StTofMuDstReader, 1)
    
};

#endif
 
