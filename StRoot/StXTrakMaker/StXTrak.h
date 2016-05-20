//StXTrak.h

#ifndef StXTrak_HH
#define StXTrak_HH

class StvELossTrak;
class MyMag;

class StXTrak 
{
 public:
    
    StXTrak(MyMag *myMag,StvELossTrak* eLoss);
    virtual ~StXTrak(){;}
    double Path(double pos[3],double mom[3], int charge);

 protected:
 StvELossTrak* mELoss;		//ELoss calculator
 MyMag*        mMyMag;		//Mag field calculator

 private:

double mPos[3];
double mMom[3];
double mCurv;
double mP;
int    mCharge;

};

class MyMag 
{
public:
  MyMag(){}
  virtual void operator()(const double* x, double* b) const;
};
#endif
