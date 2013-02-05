/*!
 * \class  StFgtAlignmentMaker
 * \brief  A FGT Alignment Class
 * \author Akio
 * \date   Dec2012
 *
 * $Id: StFgtAlignmentMaker.h,v 1.1 2013/02/05 21:08:01 akio Exp $
 *
 */
/* -------------------------------------------------------------------------
 * $Log: StFgtAlignmentMaker.h,v $
 * Revision 1.1  2013/02/05 21:08:01  akio
 * *** empty log message ***
 *
 * -------------------------------------------------------------------------
 */

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

  inline void setDataSource(Int_t v) {mDataSource=v;} // 0=StEvent 1=Fake 2=AVTrack 3=TTree(alignment.root) [0]
  inline void setWriteTree(char* v="alignment.root") {mOutTreeFile=v;} 
  inline void setReadTree(char* v="alignment.root")  {mInTreeFile=v;}
  inline void setTrackType(Int_t v) {mTrackType=v;}  // 0=straight line 1=helix [0]
  inline void setFgtErr(Float_t x, Float_t y, Float_t z) {mFgtXerr=x; mFgtZerr=y; mFgtZerr=z;} // [0.05,0.05,0.2]
  inline void setDcaCut(Float_t v) {mDcaCut=v;}
  inline void setChi2Cut(Float_t v) {mChi2Cut=v;}

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtAlignmentMaker.h,v 1.1 2013/02/05 21:08:01 akio Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  };
  
protected:    
  void fakeData();
  void readFromStEvent();
  void readFromStraightTrackMaker();

  void writeTree();
  void readFromTree();

  void setPar(TMinuit* m, fgtAlignment_st* algpar,int discmask,int quadmask, int parmask);
  void getPar(TMinuit* m, fgtAlignment_st* algpar);
  void setHitMask(int hitmask_disc);

  void doAlignment(fgtAlignment_st* input, 
		   int discmask,int quadmask, int parmask, int hitmask_disc, 
		   int trackType, int minHit, int minTpcHit,
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
  Int_t   mDataSource;   //!  0=reading from StEvent 1=Fake data 2=Reading from TTree(alignment.root)
  Char_t* mOutTreeFile;  //!  output Tree file name
  Char_t* mInTreeFile;   //!  input Tree file name
  Int_t   mTrackType;    //!  0= straight line 1=helix
  Float_t mDcaCut;       //!  Dca Cut
  Float_t mChi2Cut;      //!  Track chi2 cut

  ClassDef(StFgtAlignmentMaker,0)
};
#endif
