#ifndef THELIXTRACK_H
#define THELIXTRACK_H
#include "TObject.h"
#include "TArrayD.h"

class TCircle: public TObject
{
public:
TCircle(double *x=0,double *dir=0,double rho=0);
virtual void  Clear(const char *opt="");
const double* Pos() const 	{return fX;  } 
const double* Dir() const 	{return fD;  } 
      double  Rho() const       {return fRho;}
      double& Rho()             {return fRho;}
      void    Nor(double *norVec) const; 
      void    SetErr(const double *err);
const double* Err() const 	{return fErr;  } 
double Path(const double *pnt) const;
double Move(double step);
void   Rot(double angle);
void   Rot(double cosa,double sina);
void   Backward();
double Eval(double step,double *xy,double *dir=0) const;
double Approx(int nPts,const double *Pts  ,int pstep=2);
double Fit   (int nPts,const double *Pts  ,int pstep=2
                      ,const double *Err=0,int estep=3);
const double* GetErrs() const		{return fErr;}
double Resid (int nPts,const double *Pts  ,int pstep=2
                      ,const double *Err=0,int estep=3);
double FitZ  (double *Z0TanL,int nPts
                      ,const double *points,int pstep
                      ,const double *zets  ,int zstep=1
                      ,const double *errs=0,int estep=1);
void   Show(int nPts,const double *Pts,int pstep=2);
virtual void   Print(const char* chopt = "") const;
void   SetStrait(int strait=1) 		{SetBit(1,strait) ;}
int    IsStrait()  			{return TestBit(1);}

//	static funs
static void Test(int iTest=1);
static void Test2();
static void Test3();

private:
void  MoveErrs(double l);

protected:
double fX[2];
double fD[2];
double fRho;
double fErr[6]; //let h = fX[1]*fD[0], a=atan2(fD[1],fD[0]),c=fRho
                // hh,
		// ah,aa,
		// ch,ca,cc
ClassDef(TCircle,0)
};
class TCircleFitterAux;
class TCircleFitter: public TCircle
{
public:
       TCircleFitter();
int    Size() const 			{return fN;}
void   Add (double x,double y,const double *errs=0); 
void   AddErr(const double *errs); 

void   AddZ(double z,double err2z=0);
double Fit();   
void   MakeErrs();
double FitZ ();   
double GetZ0() const			{return fZ0    ;}
double GetTanL() const			{return fTanL  ;}
void   SetCase(int kase=0) 		{fCase=kase    ;}
int    GetCase() const			{return fKase  ;}
double Chi2() 	const 			{return fChi2XY;}
double Chi2Z () const 			{return fChi2Z ;}
double EvalChi2() const;
void   Clear(const char *opt ="");
void   Print(const char* chopt = "") const;
const double *GetX(int i=0) const;

static void Test();
private:
TCircleFitterAux* GetAux(int i) const;
double Weight(int idx);

private:
TArrayD fArr;
char   fBeg[1];
int    fN;
int    fCase;	//default case 0=automatic,1=small angle,2=Chernov/Ososkov
int    fKase;	//used case
TCircleFitterAux* fAux;
int    fBack;
double fCos,fSin;
double fNor[2];
double fXgravity;
double fYgravity;
double fXx;
double fYy;
double fXy;
double fXrr;
double fYrr;
double fRrrr;
double fRr;
double fWtot;
double fRadius2;
double fXd, fYd, fG1;
double fXCenter,fYCenter;
double fCov[6],fA,fB,fC,fH;
double fCorrR,fCorrB;
double fChi2XY;

double fZ0,fTanL;
double fErrZ[3];
double fChi2Z;
char   fEnd[1];
ClassDef(TCircleFitter,0)
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
