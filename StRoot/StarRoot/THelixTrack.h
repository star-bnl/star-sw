#ifndef THELIXTRACK_H
#define THELIXTRACK_H
#include "TObject.h"
#include "TArrayD.h"
#include "TPolinom.h"
//..............................................................................
class TCEmx_t
{ 
public:
const double *Arr() const 	{ return &mHH;}
      double *Arr()   	{ return &mHH;}
const double &operator[](int idx) const 	{ return (&mHH)[idx];}
      double &operator[](int idx)       	{ return (&mHH)[idx];}
      TCEmx_t &operator*=(double f)       	
              { for (int i=0;i<6;i++) {Arr()[i]*=f;} return *this;}
void Clear()  			{ memset(this,0,sizeof(*this));}
     TCEmx_t()			{ Clear();}
void Set(const double *err);  	
void Move(double const F[3][3]);
double Sign() const;
void Backward();
public:

double
mHH,
mHA, mAA,
mHC, mAC, mCC;
};

//..............................................................................
class THEmx_t
{ 
public:
     THEmx_t()			{ Clear();}
operator const double* ()	{ return &mHH;}		
operator       double* ()	{ return &mHH;}		
const double *Arr() const 	{ return &mHH;}
      double *Arr()   	{ return &mHH;}
const double &operator[](int idx) const 	{ return (&mHH)[idx];}
      double &operator[](int idx)       	{ return (&mHH)[idx];}
      THEmx_t &operator*=(double f)       	
              { for (int i=0;i<15;i++) {Arr()[i]*=f;} return *this;}
void Clear()  			{ memset(this,0,sizeof(*this));}
void Set(const double *err);
void Set(const double *errxy,const double *errz);
void Move(double const F[5][5]);
void Backward();
void Print(const char *tit=0) const;
double Sign() const;
double MaxCorr() const;
public:
//  dA: delta azimuth angle; dH: error along ort to dir and Z axis
//  dC: error of curvature;  dZ == dZ; dL = dLambda
double
mHH,
mHA, mAA,
mHC, mAC, mCC,
mHZ, mAZ, mCZ, mZZ,
mHL, mAL, mCL, mZL, mLL;
};


//..............................................................................
class TCircle: public TObject
{
friend class THelixTrack;
friend class TCircleFitter;
public:
 TCircle();
 TCircle(const double *x,const double *dir,double rho);
 TCircle(const TCircle& fr);
 TCircle(const TCircle* fr);	//special ctr without errors
~TCircle();
TCircle &operator=(const TCircle& fr);
 void Set(const double *x=0,const double *dir=0,const double rho=0);
virtual void  Clear(const char *opt="");
const double* Pos() const 	{return fX;  } 
      double* Pos()      	{return fX;  } 
const double* Dir() const 	{return fD;  } 
      double  Rho() const       {return fRho;}
      double& Rho()             {return fRho;}
      void    Nor(double *norVec) const; 
      void    SetEmx(const double *err=0);
const TCEmx_t *Emx() const 	{return fEmx;} 
      TCEmx_t *Emx()     	{return fEmx;} 
  void GetCenter(double center[2]) const;
double Path(const double pnt[2]) const;
double Path(const double pnt[2], const double exy[3]) const;
double Path(const TCircle &tc,double *s2=0) const;
double Move(double step);
void   Rot(double angle);
void   Rot(double cosa,double sina);
void   Backward();
double Eval(double step,double *xy,double *dir=0) const;
void   Show(int nPts,const double *Pts,int pstep=2) const;
virtual void   Print(const char* chopt = "") const;
void   SetStrait(int strait=1) 		{SetBit(1,strait) ;}
int    IsStrait()  			{return TestBit(1);}

//	static funs
static void Test2();
static void Test3();
static void Test4();
static void TestMtx();

private:
void  MoveErrs(double l);
void  MakeMtx (double l,double F[3][3]);

protected:
double fX[2];
double fD[2];
double fRho;
TCEmx_t *fEmx; //let h = fX[1]*fD[0], a=atan2(fD[1],fD[0]),c=fRho
                // hh,
		// ah,aa,
		// ch,ca,cc
ClassDef(TCircle,0)
};
//..............................................................................
class TCircleFitterAux
{
  public:
  static int dSize() {return sizeof(TCircleFitterAux)/sizeof(double);}
  public:
  double x,y,z;		//x,y,z of measured point
  double exy[3];	//err matrix(xx,xy,yy) of x,y
  double ezz;		//error of z
  double wt;		//calculated weight

};
//..............................................................................
class TCircleFitter: public TCircle
{
public:
       TCircleFitter();
int    Size() const 			{return fN;}
int    Used() const 			{return fNuse;}
void   Add (double x,double y,const double *errs=0); 
void   Add (double x,double y,double z); 
void   AddErr(const double *errs,double errz=0); 
void   AddErr(double errh,double errz=0); 
void   AddZ(double z,double err2z=0);
double Fit();   
void   MakeErrs();
double FixAt(const double vals[5],int flag); 
void   Skip(int idx); 
double GetZ0() const			{return fZ0    ;}
double GetTanL() const			{return fTanL  ;}
void   SetCase(int kase=0) 		{fCase=kase    ;}
int    GetCase() const			{return fKase  ;}
double Chi2() 	const 			{return fChi2  ;}
int    Ndf() 	const 			{return fNdf   ;}
double Chi2Z () const 			{return fChi2Z ;}
void   SetNdf(int ndf);		        
double EvalChi2();
void   Clear(const char *opt ="");
void   Print(const char* chopt = "") const;
const double *GetX(int i=0) const;
      double *GetX(int i=0);
void   Show()  const;
TCircleFitterAux* GetAux(int i) const;

static void Test(int iTest=0);
static void TestCorr(int kode=0);
private:
double f();
double df(int i);
double d2f(int i,int j);
double Rho2();
double dRho2(int i);
double d2Rho2(int i,int j);
double F();
double dF(int i);
double d2F(int i,int j);

private:
TArrayD fArr;
char   fBeg[1];
int    fN;
int    fNuse;
int    fCase;	//default case 0=automatic,1=small angle,2=Chernov/Ososkov
int    fKase;	//used case
int    fBack;
TCircleFitterAux* fAux;
double fCos,fSin;	//Direction of local coordinate
double fNor[2];
double fPol[6];
double fXgravity;
double fYgravity;
double fXx;
double fXy;
double fYy;
double fXrr;
double fYrr;
double fRrrr;
double fRr;
double fWtot;
double fRadius2;
double fXd, fYd, fG1;		//fXd,fYd coordianate of circle center in local sys 
double fNx, fNy;		//Direction from circle center to point on circle in local sys 
double fXCenter,fYCenter;	//coordianate of circle center in globa
double fCov[6],fA,fB,fC,fH;
double fR;			//radius of curvatur
double fRd;			//distance to center in local system
double fCorrR,fCorrB;
double fChi2;
int    fNdf;
double fZ0,fTanL;
double fChi2Z;
char   fEnd[1];
ClassDef(TCircleFitter,0)
};


//..............................................................................
class THelixTrack : public TObject 
{
public:

	THelixTrack();
	THelixTrack(const double *xyz,const double *dir,double rho,double drho=0);
	THelixTrack(const THelixTrack &from);
	THelixTrack(const THelixTrack *from);	//Special ctr without errs
virtual ~THelixTrack();
THelixTrack &operator=(const THelixTrack &from);
	void Set   (const double *xyz,const double *dir,double rho,double drho=0);
	void Set   (double rho,double drho=0);
	void SetEmx(const double*  err2xy,const double*  err2z);
	void SetEmx(const double*  err=0);
    THEmx_t *Emx() const			{return fEmx;}
	void StiEmx(double emx[21]) const;
        void GetSpot(const double axis[3][3],double emx[3]) const;
	void Fill  (TCircle &circ) const;
///		Change direction
	void Backward();
///		Move along helix
	double Move(double step);
	double Move(double step,double F[5][5]);
///		Evaluate params with given step along helix
	double Eval(double step, double *xyz, double *dir,double &rho) const;
	double Step(double step, double *xyz, double *dir,double &rho) const
       {return Eval(       step,         xyz,         dir,        rho);}
///		Get current parameters
        void   Get (double *xyz, double *dir,double &rho) const {Step(0.,xyz,dir,rho);}
	double Eval(double step, double *xyz, double *dir=0) const;
	double Step(double step, double *xyz, double *dir=0) const
       {return Eval(       step,         xyz,         dir  );}
        void   Get (double *xyz, double *dir=0) const {Step(0.,xyz,dir);}
///		Distance to crossing 2nd order surface
///             surf[0]+surf[1]*x+surf[2]*y+surf[3]*z
///            +surf[4]*x*x +surf[5]*y*y+surf[6]*z*z
///            +surf[7]*x*y +surf[8]*y*z+surf[9]*z*x  == 0
///            nearest==0 search alon direction, else the nearest
        double Step(double stmax, const double *surf, int nsurf
	           ,double *x=0, double *dir=0, int nearest=0) const;
        double Path(double stmax, const double *surf, int nsurf
	           ,double *x=0, double *dir=0, int nearest=0) const
                   {return Step(stmax,surf,nsurf,x,dir,nearest);}

///		Distance to nearest point to given space point
        double Step(const double point[3],double *xyz=0, double *dir=0) const;
        double Path(const double point[3],double *xyz=0, double *dir=0) const 
	           {return Step(point,xyz,dir);}
///		DCA to given space point (with error matrix)
        double Dca(const double point[3],double *dcaErr=0) const;

///		Distance to nearest point to given 2dim point
        double Path(double x,double y) const ;
///		DCA to given 2dim point (with error matrix)
        double Dca(double x,double y,double *dcaErr=0) const ;

///		Returns length to nearest point of other helix 
///             along this helix. *s2 the length along the other helix
///             Both lengths are positive

        double Path(const THelixTrack &hlx,double *s2=0) const ;

///		Extention of previous method. 
///             Both lengths could be -ve.
///             *dist - distance between helicies
///		xyz[6] - 1st and 2nd space points on the helicies
        double PathX(const THelixTrack &hlx,double *s2=0
	            ,double *dist=0, double *xyz=0) const;

///		distance and DCAxy and DCAz to given space point (with error matrix)
        double Dca(const double point[3]
                  ,double &dcaXY,double &dcaZ,double dcaEmx[3],int kind=3) const;

        const double *GetXYZ() 	const {return fX;}
        const double *Pos()    	const {return fX;}
              double *Pos()           {return fX;}
        const double *GetDir() 	const {return fP;}
        const double *Dir()    	const {return fP;}
              double *Dir()           {return fP;}
        double GetRho() 	const {return fRho ;}
        double GetDRho()	const {return fDRho ;}
        double GetCos() 	const {return fCosL;}
        double GetSin() 	const {return fP[2];}
        double GetTan() 	const {return fP[2]/fCosL;}
        double GetPeriod() const ;
        void Rot(double angle);
        void Rot(double cosa,double sina);

        void Show(double len, const THelixTrack *other=0) const;
        void Print(Option_t *opt="") const;
//	statics
static  void InvertMtx(double derivs[5][5]);
static  void Test1();
static  void Test2();
static  void Test3();
static  void Test4();
static  void Test5();
static	void TestMtx();
static	void TestDer();
static	void TestTwoHlx();
private:
///     	Make transformatiom matrix to transform errors
///		called only after Eval()
	void MakeMtx(double step,double F[5][5]);
protected:
        double Step(double stmin,double stmax, const double *surf, int nsurf
	           ,double *x=0, double *dir=0,int nearest=0) const;
        double StepHZ(const double *surf, int nsurf
	           ,double *x=0, double *dir=0,int nearest=0) const;
	void Build();
        char   fBeg[1];  
	double fX[3];
	double fP[3];
	double fRho;
	double fDRho;
	double fCosL;
        THEmx_t *fEmx;
        char fEnd[1];
ClassDef(THelixTrack,0)
};
class THelixFitter: public THelixTrack
{
public:
       THelixFitter();
      ~THelixFitter();
int    Size() const 			{return fCircleFitter.Size();}
int    Used() const 			{return fCircleFitter.Used();}
void   Add (double x,double y,double z); 
void   AddErr(const double *err2xy,double err2z); 
void   AddErr(double errhh,double errzz); 
double Fit();   
void   MakeErrs();
double FixAt(const double vals[5],int flag=1); 
void   Skip(int idx); 
void   SetCase(int kase=0) 		{fCircleFitter.SetCase(kase);}
int    GetCase() const			{return fCircleFitter.GetCase();}
double Chi2() const 			{return fChi2;}
int    Ndf()  const			{return fCircleFitter.Ndf()+fPoli1Fitter.Ndf();}
double Chi2XY () const 			{return fCircleFitter.Chi2();}
double Chi2SZ () const 			{return fPoli1Fitter.Chi2() ;}
int    NdfXY ()  const 			{return fCircleFitter.Ndf() ;}
int    NdfSZ ()  const 			{return fPoli1Fitter.Ndf()  ;}
TCircleFitterAux* GetAux(int i) const   {return fCircleFitter.GetAux(i);}
double EvalChi2();
void   Clear(const char *opt ="");
void   Print(const char* chopt = "") const;
void   Show() const;

static void Test(int kase=0);
private:
void Update(int kase);
private:
TCircleFitter fCircleFitter;
TPoliFitter   fPoli1Fitter;
double fChi2;
ClassDef(THelixFitter,0)
};

//..............................................................................
#include <vector>
class THelixKFitterAux { public: double x[3],e[6],wt,xi2; };
typedef std::vector<THelixKFitterAux> THelixKFitterAuxV;

class THelixKFitter: public THelixTrack
{
public:
       THelixKFitter()				{fFitingShow=0;Clear();}
      ~THelixKFitter(){;}
void   Add (const double x[3]); 
void   AddErr(const double err[6]); 
double Fit();   
double Chi2() const 				{return fChi2		;}
int    Ndf()  const			        {return 2*fAux.size()-5	;}
int    Size() const 				{return fAux.size()	;}
const THelixKFitterAux* GetAux(int i) const	{return &fAux[i]	;}
void   Clear(const char * ="")		{fAux.clear();fChi2=0	;}
void   Print(const char* chopt = "") const;
void   Show() const;
void   SetFitingShow() 			{fFitingShow = new std::vector<double>;}
static void Test(int nev=10000);
private:
std::vector<double> *fFitingShow;
THelixKFitterAuxV fAux;
double fChi2;
ClassDef(THelixKFitter,0)
};
#endif // THELIXTRACK_H
