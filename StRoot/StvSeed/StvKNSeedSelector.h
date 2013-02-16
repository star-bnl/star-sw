/// \File StvKNSeedSelector.h
/// \author Victor Perev 01/2010
#ifndef StvKNSeedSelector_HH
#define StvKNSeedSelector_HH
#include <assert.h>
#include <vector>
#include "TNamed.h"
/// \class StvKNSeedSelector

typedef std::vector<void*> VoidVec;
enum {kKNumber=4};

class StvKNAux {
public:
  StvKNAux() 		{ Reset();}
  void Reset() 		{ for (int i=0;i<kKNumber;i++) {mDist[i]=1e11;};}
public:
  void *mHit;			//void pointer to hit
  float mLen;			//distance from the 1st hit
  float mDir[3];		//direction from the 1st hit
  float mDist[kKNumber];	//sorted angles to nearest hits
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
     void  Add  (const float      pos[3],void *voidHit);
      int  Select();  
const VoidVec &Get() const { return mSel; } 
      void Show() const;
private:
      void Relink();
      void Update(int ia,int ib);
      void Insert(int ia,int ib,float dis);
      void Pass(int iux,double accuAng);
    double Width();
private:  
 int  mState;		//Status, &1 =narrow trace
void *mStartHit;
float mStartPos[3];
float mStartDir[3];
float mAveDir[3];
float mSidDir[3];
float mStartRad;
float mKNNDist;	//minimal KN distance
int   mMinIdx;	//index of aux with minimal KN distance
float mEigen[2];//Eigen numbers of uu,uv,vv matrix in most dense place
float mMaxSel;	//Max angle deviation between hits
float mMaxAccu;	//Max accumulated angle between hit & bestHit in Pass()
float mMaxNear;	//Max angle between hits in Pass()
float mErr;	//Estimated space error
int   mNHits;	//number of selected hits
VoidVec mSel;
std::vector<StvKNAux> mAux;
std::map<float,int> mMapLen;

};



#endif
