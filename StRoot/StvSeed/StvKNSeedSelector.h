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
  StvKNAux() { for (int i=0;i<kKNumber;i++) {mDist[i]=1e11;};}
    void Update(StvKNAux *aux);
public:
  void *mHit;
  float mLen;
  float mDir[3];
  float mDist[kKNumber];
  int   mNbor[kKNumber];
  int   mSel;
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
      void Update(int ia,int ib);
      void Insert(int ia,int ib,float dis);
      void Pass(int iux);
    double Width();
private:  
void *mStartHit;
float mStartPos[3];
float mAveDir[3];
float mStartRad;
float mKNNDist;	//minimal KN distance
int   mMinIdx;	//index of aux with minimal KN distance
float mEigen[2];//Eigen numbers of uu,uv,vv matrix in most dense place
float mMaxSel;	//Max angle deviation between hits
float mErr;	//Estimated space error
int   mNHits;	//number of selected hits
VoidVec mSel;
std::vector<StvKNAux> mAux;
std::map<float,int> mMapLen;

};



#endif
