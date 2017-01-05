/// \File StvKNSeedSelector.h
/// \author Victor Perev 01/2010
#ifndef StvKNSeedSelector_HH
#define StvKNSeedSelector_HH
#include <assert.h>
#include <vector>
#include <map>

typedef std::multimap <float,int> MyPhiDiv;

typedef std::map <float,MyPhiDiv> MyTheDiv;



#include "TNamed.h"
/// \class StvKNSeedSelector


typedef std::vector<void*> VoidVec;
enum {kKNumber=2};

class StvKNAux {
public:
  StvKNAux() 		{ Reset();}
  void Reset() 		{ for (int i=0;i<kKNumber;i++) {mDist[i]=1e11;mNbor[i]=-1;}}
public:
  void *mHit;			//void pointer to hit
  const void *mDet;		//void pointer to detector plane
  float mLen;			//distance from the 1st hit
  float mDir[3];		//direction from the 1st hit
  float mDist[kKNumber];	//sorted angles to nearest hits
  float mPhi;
  float mThe;
  float mCosThe;
  int   mNbor[kKNumber];	//indices in Aux array to nearest hits
  int   mSel;			//hit selected
};

class StvKNSeedSelector
{
friend class StvKNSeedFinder;
public:

  StvKNSeedSelector();
  virtual ~StvKNSeedSelector(){;}
     void  Reset(const float startPos[3],void *voidHit);
     void  Add  (const float      pos[3],void *voidHit, const void *voidDet);
      int  Select();  
      int  Zelect();  
const VoidVec &Get() const { return mSel;} 
       int GetNHits() const{ return mSel.size();}
      void Show() const;
private:
      void Relink();
      void Update(int ia,int ib);
      void Insert(int ia,int ib,float dis);
      void Pass(int iux,double accuAng);
    double Width();
const float *Eigen() const {return mEigen;}
protected:  
 int  mState;		//Status, &1 =narrow trace
void *mStartHit;
float mStartPos[3];
float mAveDir[3];
float mStartRad;
float mKNNDist;	//minimal KN distance
int   mMinIdx;	//index of aux with minimal KN distance
float mEigen[2];//Eigen numbers of uu,uv,vv matrix in most dense place
float mMaxSel;	//Max angle deviation between hits
float mMaxAccu;	//Max accumulated angle between hit & bestHit in Pass()
float mMaxNear;	//Max angle between hits in Pass()
float mErr;	//Estimated space error
int   mNHits;	//number of selected hits
int   mIx;	//index of X in array,=0 in med eta =2 in forward eta
int   mIy;	//index of Y in array,=1 always 
int   mIz;	//index of Z in array,=2 in med eta =0 in forward eta
VoidVec mSel;
std::vector<StvKNAux> mAux;
std::map<float,int> mMapLen;
MyTheDiv mTheDiv;
};



#endif
