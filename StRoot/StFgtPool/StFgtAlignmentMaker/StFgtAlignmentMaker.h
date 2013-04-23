/*!
 * \class  StFgtAlignmentMaker
 * \brief  A FGT Alignment Class
 * \author Akio
 * \date   Dec2012
 *
 * $Id: StFgtAlignmentMaker.h,v 1.4 2013/04/23 16:47:22 akio Exp $
 *
 */
/* -------------------------------------------------------------------------
 * $Log: StFgtAlignmentMaker.h,v $
 * Revision 1.4  2013/04/23 16:47:22  akio
 * Added TPC prompt hit support
 *
 * Revision 1.3  2013/04/04 17:08:30  akio
 * make steps settable from macro
 *
 * Revision 1.2  2013/02/07 00:39:33  akio
 * Adding run# for naming output files
 *
 * Revision 1.1  2013/02/05 21:08:01  akio
 * *** empty log message ***
 *
 * -------------------------------------------------------------------------
 */

//setStep define alignment procedure by with different parameter space search in sequence
//                                                                                                                                         
//  setStep(discmask,quadmask,parmask,hitmask,trackType,minHit,minTpcHit,minPromptHit)                                                 
//    3 masks to specify which parameters are free(1) or fixed(0)
//      discmask = 3f(all),3e(fix d1)    note: discmask=3f & hitmask=3f causes unstable result                                               
//      quadmask = 1(A),2(B),4(C),8(D)                                                                                                       
//      parmask  = 3f(all),38(xyz),19(xy,phi),39(xyz & phi)                                                                                  
//    1 mask to specify wihch hits to be used bit0-5=fgt d1-6, bit6=vtx, bit7=tpc bit8=TPCpromptHit
//      hitmask  = 1ff(FGT+VTX+TPC+PROMPT),ff(FGT+VTX+TPC),c0(TPC+VTX, no FGT), 3f(FGT only), 7f(FGT+VTX)
//
//    Specify track type to be used for refit
//      trackType= 0(straight line), 1(Helix)                                                                                                
//  
//    1st step should have 3 masks all 0 for making "before" histos
//    "after" histos will use last step's paramters except 3 masks are set to 0

#ifndef StFgtAlignmentMaker_hh     
#define StFgtAlignmentMaker_hh

#include "StMaker.h"
#include "TString.h"
#include "StEnumerations.h"

class TMinuit;

struct fgtAlignment_st;

class StFgtAlignmentMaker : public StMaker {
public:
  
  StFgtAlignmentMaker(const Char_t *name="fgtalg");     // constructor
  ~StFgtAlignmentMaker() {}                             // destructor
  
  Int_t  Init();                      // called once for initilization
  Int_t  InitRun(Int_t runnum);       // called once per run
  Int_t  Make();                      // invoked for every event
  Int_t  Finish();                    // called once at the end  

  inline void setDataSource(Int_t v) {mDataSource=v;} // 0=StEvent(Primary) 1=StEvent(Global) 2=AVTrack 3=TTree(alignment.root) 4=Fake [0]
  inline void setWriteTree(char* v="alignment.root") {mOutTreeFile=v;} 
  inline void setReadTree(char* v="alignment.root")  {mInTreeFile=v;}
  inline void setFgtErr(Float_t x, Float_t y, Float_t z) {mFgtXerr=x; mFgtZerr=y; mFgtZerr=z;} // [0.05,0.05,0.2]
  inline void setDcaCut(Float_t v) {mDcaCut=v;}
  inline void setChi2Cut(Float_t v) {mChi2Cut=v;}
  inline void setRunNumber(Int_t v) {mRunNumber=v;}

  void setStep(int discmask,int quadmask, int parmask, int hitmask_disc, 
	       int trackType, int minHit, int minTpcHit, int minPromptHit);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtAlignmentMaker.h,v 1.4 2013/04/23 16:47:22 akio Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  };
  
protected:    
  void fakeData();
  void readFromStEvent();
  void readFromStEventGlobal();
  void readFromStraightTrackMaker();

  void writeTree();
  void readFromTree();

  void setPar(TMinuit* m, fgtAlignment_st* algpar,int discmask,int quadmask, int parmask);
  void getPar(TMinuit* m, fgtAlignment_st* algpar);
  void setHitMask(int hitmask_disc);

  void doAlignment(fgtAlignment_st* input, 
		   int discmask,int quadmask, int parmask, int hitmask_disc, 
		   int trackType, int minHit, int minTpcHit, int minPromptHit,
		   fgtAlignment_st* result);
  
  void readPar(fgtAlignment_st* algpar);
  void writePar(fgtAlignment_st* algpar);
  
  void bookHist();
  void resetHist();
  void saveHist();
  
private:
  Int_t   mEventCounter; //!
  Float_t mFgtXerr;      //!  if>0, overwrite fgt x position error 
  Float_t mFgtYerr;      //!  if>0, overwirte fgt y position error
  Float_t mFgtZerr;      //!  if>0, overwrite fgt z position error 
  Int_t   mDataSource;   //!  0=reading from StEvent(primary) 1=StEvent Global, 2=AVTrack 3=Reading from TTree(alignment.root) 4=Fake
  Char_t* mOutTreeFile;  //!  output Tree file name
  Char_t* mInTreeFile;   //!  input Tree file name
  Float_t mDcaCut;       //!  Dca Cut
  Float_t mChi2Cut;      //!  Track chi2 cut
  Int_t   mRunNumber;    //!  Run# for output file name

  static const int mMaxStep=100;
  int mNStep;
  int mDiscMask[mMaxStep];
  int mQuadMask[mMaxStep];
  int mParMask[mMaxStep];
  int mHitMask[mMaxStep];
  int mTrackType[mMaxStep];
  int mMinHit[mMaxStep];
  int mMinTpcHit[mMaxStep];
  int mMinPromptHit[mMaxStep];
  
  ClassDef(StFgtAlignmentMaker,0)
};
#endif
