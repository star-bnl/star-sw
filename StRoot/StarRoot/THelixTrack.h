#ifndef THELIXTRACK_H
#define THELIXTRACK_H
#include "TObject.h"

class TCircle: public TObject
{
public:
TCircle(double *x=0,double *dir=0,double rho=0);

const double* Pos() const 	{return fX;  } 
const double* Dir() const 	{return fD;  } 
      double  Rho() const       {return fRho;}
      void    Nor(double *norVec) const; 

double Path(const double *pnt);
double Move(double step);
void   Rot(double angle);
void   Backward();
double Eval(double step,double *xy,double *dir=0) const;
double Approx(int nPts,const double *Pts  ,int pstep=2);
double Fit   (int nPts,const double *Pts  ,int pstep=2
                      ,const double *Err=0,int estep=3);
double Resid (int nPts,const double *Pts  ,int pstep=2
                      ,const double *Err=0,int estep=3);
double FitZ  (double *Z0TanL,int nPts
                      ,const double *points,int pstep
                      ,const double *zets  ,int zstep=1
                      ,const double *errs=0,int estep=1);
void   Show(int nPts,const double *Pts,int pstep=2);
void   Print(const char* chopt = "") const;
void   SetStrait(int strait=1) 		{SetBit(1,strait) ;}
int    IsStrait()  			{return TestBit(1);}

//	static funs
static void Test(int iTest=1);
static void Test2();
static void Test3();
protected:
double fX[2];
double fD[2];
double fRho;
ClassDef(TCircle,0)
};

class THelixTrack : public TObject 
{
public:

	THelixTrack();
	THelixTrack(const double *xyz,const double *dir,double rho,const double *hxyz=0,double drho=0);
	THelixTrack(const double *pnts,int npnts, int size = 3);
	THelixTrack(const THelixTrack &from);

	void Set   (const double *xyz,const double *dir,double rho,const double *hxyz=0,double drho=0);
	void Set   (double rho,double drho=0);
	void Backward();
	double Fit(const double *pnts,int npnts, int size = 3);
	double Move(double step);
	double Steb(double step, double *xyz, double *dir=0) const;
	double Step(double step, double *xyz, double *dir,double &rho) const;
	double Step(double step, double *xyz, double *dir=0) const;
        double Step(double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(const double *point,double *xyz=0, double *dir=0) const;
        double Step(const double *point,double *xyz=0, double *dir=0) const;
        double GetDCA  () const;
        double GetDCAz () const;
        double GetDCAxy() const;
        double GetDCA  (double xx,double yy) const;
        const double *GetXYZ() const {return fX;}
        const double *GetDir() const {return fP;}
        double GetRho() const {return fRho ;}
        double GetDRho()const {return fDRho ;}
        double GetCos() const {return fCosL;}
        double GetSin() const {return fHP  ;}
        double GetPeriod() const ;

        void Print(Option_t *opt="") const;
 static void Test1();
private:
static  int  SqEqu(double *, double *);
protected:
        double Step(double stmin,double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(double stmin,double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double StepHZ(const double *surf, int nsurf, double *x=0, double *dir=0) const;
	void Build();

	double fX[3];
	double fP[3];
	double fPxy[3];
	double fH[3];
	double fHXP[3];
	double fRho;
	double fDRho;
	double fHP;
	double fCosL;
   	double fMax;
   	mutable double fDCA[2];
        mutable Int_t  fKind;
ClassDef(THelixTrack,0)
};
#endif // THELIXTRACK_H
