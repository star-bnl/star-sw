//StXTrak.h

#ifndef StXTrak_HH
#define StXTrak_HH
#include "TGeoSwim.h"
class TGeoSwim;
class MyMag;
class MyLoss;


class StXTrakAux 
{
public:
double mPos[3];
double mMom[3];
double mCosLam;
double mCurv;
double mP;
double mPt;
double mPti;
double mLen;
double mPLoss;
int    mCharge;
};  
class StXTrak 
{
 public:
    
    StXTrak(MyMag* myMag=0,MyLoss* eLoss=0,TGeoSwimEnd* myEnd=0);
    virtual ~StXTrak(){;}

    void Clear();	     
    void Set1stPoint(int charge1st,double pos1st[3],double mom1st[3]);	     
    void Set2ndPoint(int charge2nd,double pos2nd[3],double mom2nd[3]);	     
    void SetLen2nd  (double length){ m2ndTk.mLen = length + m1stTk.mLen;
                                     mCurTk.mLen = m2ndTk.mLen;} 
  double GetLength  (int idx = 2) const;	     
const char *GetName() const { return (mMyEnd)?mMyEnd->GetName():"" ;}
TGeoSwimMag *GetMag()  	{return mMyMag;}
StXTrakAux  &GetAux() 	{return mCurTk;}     

 int Next();
//=========================

 protected:
 TGeoSwim*     mSwim;		//Swimmer
 TGeoSwimMag*  mMyMag;
 TGeoSwimLoss* mMyLoss;
 TGeoSwimEnd*  mMyEnd;
 private:

char   mBeg[1];
int    mFlag1st; //0=undefined, 1=primary, 2=dca,3=1st point
StXTrakAux m1stTk;
StXTrakAux m2ndTk;
StXTrakAux mCurTk;


char   mEnd[1];

};

class MyMag: public TGeoSwimMag
{
public:
  MyMag(){}
  virtual void operator()(const double* x, double* b);
};

class StvELossTrak;
class MyLoss: public TGeoSwimLoss {
public:
MyLoss();
//		returns momentum loss
virtual double operator()(const TGeoMaterial* mate,double P,double len
                       ,double *theta2=0);
protected:
StvELossTrak *mELoss;
};
#endif
