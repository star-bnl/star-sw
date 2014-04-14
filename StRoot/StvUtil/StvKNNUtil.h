
/// \File StvKNNUtil.h
/// \author Victor Perev 01/2013
#ifndef StvKNNUtil_HH
#define StvKNNUtil_HH
#include <assert.h>
#include <vector>
#include "Rtypes.h"
/// \class StvKNNUtil

enum { kKNNgbMax = 5, kKNNvrMax=3};
class StvKNNAux 
{
friend class StvKNNUtil;
public:
class QaIx_t {public: int ix;float qa;}; 
StvKNNAux();
private:
ULong_t mId;
float mVar[kKNNvrMax];
QaIx_t mQa[kKNNgbMax];
};


class StvKNNUtil
{
public:
  StvKNNUtil(int nVr,int nGb);
  virtual ~StvKNNUtil(){;}
  virtual double Dist(const float *a,const float *b) const; 
  void Reset();
   int Add(ULong_t id,const float *vars);
double GetBest(ULong_t *id=0,ULong_t *ngb=0) const;
double GetWost(ULong_t *id=0,ULong_t *ngb=0) const;
double GetBest(int &idx,int *ngb=0) const;
double GetWost(int &idx,int *ngb=0) const;
double GetByIdx(int idx,int *ngb=0)const;

double BestPos(float *var =0) const;
double WostDis(const float *var) const;

private:  
const int mNVars;				//number of variables
const int mKNNgb;				//number of neighbours
mutable int mIdxBestWost[2];
mutable double mBestWost[2];	//[0]=max density;  [1]=min density


std::vector<StvKNNAux> mEnt;		//KNN entries


};

#endif
