/*!
 * \class  StFgtAlignmentMaker
 * \brief  A FGT Alignment Class
 * \author Akio
 * \date   Dec2012
 *
 * $Id: StFgtAlignmentMaker.h,v 1.7 2014/08/06 11:43:10 jeromel Exp $
 *
 */
/* -------------------------------------------------------------------------
 * $Log: StFgtAlignmentMaker.h,v $
 * Revision 1.7  2014/08/06 11:43:10  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/03/05 17:58:01  akio
 * New version
 *
 * Revision 1.5  2013/06/04 23:19:33  akio
 * *** empty log message ***
 *
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
//  setStep(discmask,quadmask,parmask,hitmask,residmask,trackType,minHit,minFgtHit,minVtx,minTpcHit,minPromptHit,minEemcHit)
//    3 masks to specify which parameters are free(1) or fixed(0)
//      discmask = 3f(all),3e(fix d1)    (note: discmask=3f & hitmask=3f causes unstable result)
//      quadmask = 1(A),2(B),4(C),8(D)
//      parmask  = 3f(all),38(xyz),19(xy,phi),39(xyz&phi),1f(xy&3angles) : see StDb/idl/fgtAlignment.idl
//
//    hitmask to specify wihch hits to be used for each track fit
//    residmask to specify wihch hits to be used for FGT alignment (residuals added over tracks)
//      bit0-5=fgt d1-6, bit6=vtx, bit7=tpc, bit8=TPCpromptHit, bit9=EEMCall, bit10=EemcMip, bit11=EEmcElectron
//      hitmask/residmask  = fff(FGT+VTX+TPC+PROMPT+EEMC+MIP+ELE),ff(FGT+VTX+TPC),c0(TPC+VTX, no FGT), 
//                           3f(FGT only), 7f(FGT+VTX), e7f(FGT+VTX+EEMC+MIP+ELE)
//                           f7f(FGT+VTX+Prompt+EEMC+MIP+ELE) 
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

  inline void setDataSource(Int_t v) {mDataSource=v;} 
  // 0=StEvent(Primary) [default]  : didn't work
  // 1=StEvent(Global) : didn't work 
  // 2=AVTrack : internal alignment
  // 3=TTree(alignment.root) : read output from this maker
  // 4=Fake : create fake data
  // 5=AVTrack and StEvent(vertex, tpc & eemc hits) : FGT+vetex+TPC+EEMC tracking
  // 6=AVTrack and MuDST (vertex, eemc hits) : FGT+vetex+EEMC tracking (no TPC hits in MUDST)
  
  inline void setWriteTree(int v=1) {mOutTreeFile=v;} 
  inline void setReadTree(char* v="alignment.root")  {mInTreeFile=v;}
  
  //Set resolution for each detector to normalize residuals (also used for fake data gaussian sigma)
  inline void setError(Float_t fgt, Float_t vtx, Float_t vtxz, Float_t tpci, Float_t tpco, Float_t tpcz, Float_t ppt, Float_t emc) {
    mErrFgt=fgt; mErrVtx=vtx; mErrVtxZ=vtxz;mErrTpcI=tpci; mErrTpcO=tpco; mErrTpcZ=tpcz; mErrPpt=ppt; mErrEmc=emc;
  }      
  //Fake track kinematics
  inline void setFakeKine(Int_t n, Float_t emin, Float_t emax, Float_t etamin, Float_t etamax, Float_t phimin, Float_t phimax, Float_t vsig){
    mFakeNtrk=n; mFakeEmin=emin; mFakeEmax=emax; mFakeEtamin=etamin; mFakeEtamax=etamax; mFakePhimin=phimin; mFakePhimax=phimax; mFakeVtxSig=vsig;
  }
  inline void setRunNumber(Int_t v, Int_t s=0, Int_t d=0) {mRunNumber=v; mSeqNumber=s; mDay=d;}
  inline void setReadParFile(char* v="alignment.dat") {mReadParFile=v;}

  void setStep(int discmask,int quadmask, int parmask, int hitmask_disc, int residmask,
	       int trackType, int minHit, int minFgtHit, int minVtx, int minTpcHit, int minPromptHit, int minEemcHit,
	       float dzcut=0, float dcacut=0, float fgtrcut=0, float fgtpcut=0, float tpcrcut=0, float tpcpcut=0, float emcrcut=0, float emcpcut=0);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtAlignmentMaker.h,v 1.7 2014/08/06 11:43:10 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  };
  
protected:    
  void fakeData();
  void readFromStEvent();
  void readFromStEventGlobal();
  void readFromStraightTrackMaker();
  void readFromStraightTrackAndStEvent();
  void readFromStraightTrackAndMudst();

  void calcETBalanceFromStEvent();
  void fillETBalance(int itrk);

  void writeTree();
  void readFromTree();

  void DispFromStraightTrackMaker();
  void overWriteError();

  void setPar(TMinuit* m, fgtAlignment_st* algpar,int discmask,int quadmask, int parmask);
  void getPar(TMinuit* m, fgtAlignment_st* algpar);
  void setHitMask(int hitmask_disc);

  void doAlignment(fgtAlignment_st* input, 
		   int discmask,int quadmask, int parmask, int hitmask_disc,  int residmask,
		   int trackType, int minHit, int minFgtHit, int minVtx, int minTpcHit, int minPromptHit, int minEemcHit,
		   fgtAlignment_st* result);
  
  void readPar(fgtAlignment_st* algpar);
  void writePar(fgtAlignment_st* algpar);
  
  void bookHist();
  void resetHist();
  void saveHist();
  
private:
  Int_t   mEventCounter; //!
  Float_t mErrFgt,mErrVtx,mErrVtxZ,mErrTpcI,mErrTpcO,mErrTpcZ,mErrPpt,mErrEmc; //hit errors
  Int_t   mFakeNtrk;
  Float_t mFakeEmin, mFakeEmax, mFakeEtamin, mFakeEtamax, mFakePhimin, mFakePhimax, mFakeVtxSig; //fake track kinematics
  Int_t   mDataSource;   //!  0=reading from StEvent(primary) 1=StEvent Global, 2=AVTrack 3=Reading from TTree(alignment.root) 4=Fake
  Int_t  mOutTreeFile;   //!  output Tree file
  Char_t* mInTreeFile;   //!  input Tree file name
  Char_t* mReadParFile;  //!  input parameter file instead of DB
  Int_t   mRunNumber;    //!  Run# for output file name
  Int_t   mSeqNumber;    //!  Seq# for output file name
  Int_t   mDay;          //!  Dat# for output file name

  static const int mMaxStep=100;
  int mNStep;
  int mDiscMask[mMaxStep];
  int mQuadMask[mMaxStep];
  int mParMask[mMaxStep];
  int mHitMask[mMaxStep];
  int mResidMask[mMaxStep];
  int mTrackType[mMaxStep];
  int mMinHit[mMaxStep];
  int mMinFgtHit[mMaxStep];
  int mMinVtx[mMaxStep];
  int mMinTpcHit[mMaxStep];
  int mMinPromptHit[mMaxStep];
  int mMinEemcHit[mMaxStep];
  float mDzCut[mMaxStep];
  float mDcaCut[mMaxStep];
  float mFgtRCut[mMaxStep];
  float mFgtPCut[mMaxStep];
  float mTpcRCut[mMaxStep];
  float mTpcPCut[mMaxStep];
  float mEmcRCut[mMaxStep];
  float mEmcPCut[mMaxStep];

  ClassDef(StFgtAlignmentMaker,0)
};
#endif
