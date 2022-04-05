/*******************************************************************
 *
 * $Id: StBTofNtupleMaker.h,v 1.2 2015/07/28 22:50:03 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: example maker to get the matched TOFr cells and fill
 *              into TOFr tree.
 *
 *****************************************************************
 *
 * $Log: StBTofNtupleMaker.h,v $
 * Revision 1.2  2015/07/28 22:50:03  smirnovd
 * C++11 requires a space between literal and identifier
 *
 * Revision 1.1  2010/04/09 00:28:48  dongx
 * First release
 *
 * Revision 1.1  2010/04/09 00:16:05  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef StBTofNtupleMaker_hh     
#define StBTofNtupleMaker_hh
#include "StMaker.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StBTofCellData.h"

#define __SIGMA_SCALE__ 1000.
#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif

class StEvent;
class StTrack;
class StGlobalTrack;
class StHelix;
#include "StThreeVectorF.hh"
class StTrackGeometry;
class StBTofGeometry; 
class StBTofCollection;
class StBTofHitCollection;   
class StSPtrVecBTofHit;      
class StMuDst;

class TFile;
class TTree;

class StBTofNtupleMaker : public StMaker {
 public:
  StBTofNtupleMaker(const Char_t *name, const Char_t *outname);
  ~StBTofNtupleMaker();

  Int_t  Init();
  Int_t  InitRun(int runnumber);
  Int_t  Make();
  Int_t  FinishRun(int runnumber);
  Int_t  Finish();

  void SetNtupleFileName(Char_t*);
  void setOuterTrackGeometry();
  void setStandardTrackGeometry();
  void setInitGeomFromOther(const Bool_t);
  /// switch to read in StEvent/MuDst
  void setMuDstIn(const Bool_t muDstIn=kTRUE);
  /// switch to turn on the use of event vertex for dca
  void setUseEventVertex(const Bool_t val=kTRUE);
  
private:
  ///
  void processStEvent();
  ///
  void processMuDst();
            
  void bookNtuples();

private:

  StPhysicalHelix* mBeamHelix;

  StEvent*          mEvent;
  StMuDst*          mMuDst;
  Bool_t            mMuDstIn;          //! switch - default is to read in StEvent
  Bool_t            mUseEventVertex;   //! switch - use event vertex for dca
        
  StBTofGeometry *mBTofGeom;  //! tofr geometry
  Bool_t mInitGeomFromOther;  //! geometry initial from other makers
  Bool_t mOuterTrackGeometry; //! select outer track geometry (true)

  // ntuple related data members
  Int_t mAcceptedEvents; //! number of accepted events
  Int_t mPvpdEntries; //! number of pVPD ntuple entries
  Int_t mBTofEvents; //! number of BTof events
  Int_t mBTofEntries; //! number of BTof ntuple entries

  string mTupleFileName; //!
  TFile *mTupleFile; //!
  
  StBTofCellData  mCellData;

  Bool_t  doPrintMemoryInfo; //! control debug memory data
  Bool_t  doPrintCpuInfo; //! control debug timing data

  TTree *mCellTuple; //! BTof calibration ntuple

  virtual const char *GetCVS() const 
    {static const char cvs[]="Tag $Name:  $ $Id: StBTofNtupleMaker.h,v 1.2 2015/07/28 22:50:03 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StBTofNtupleMaker,1)
};

inline void StBTofNtupleMaker::setOuterTrackGeometry(){mOuterTrackGeometry=false;}
inline void StBTofNtupleMaker::setStandardTrackGeometry(){mOuterTrackGeometry=false;}
inline void StBTofNtupleMaker::SetNtupleFileName(Char_t* filename){mTupleFileName=filename;}
inline void StBTofNtupleMaker::setInitGeomFromOther(const Bool_t init) { mInitGeomFromOther = init; }
inline void StBTofNtupleMaker::setMuDstIn(Bool_t val) { mMuDstIn = val; }
inline void StBTofNtupleMaker::setUseEventVertex(const bool val) { mUseEventVertex=val; }

#endif
