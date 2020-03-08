#ifndef THELIX3D_H
#define THELIX3D_H
#include <assert.h>
#include "TObject.h"
#include "TArrayD.h"

#include "THelixTrack_.h"

enum THelix3dE {kU =0,kV =1, kFita =2 ,kLama =3,kPinv =4};
enum THelix3dE2{kKT=0,kKU=1,kKV=2, kKdDdL=3};

typedef double Mtx33D_t[3][3];
typedef double Mtx43D_t[4][3];

class TkDir_t
{
  public:
  TkDir_t() {Clear();}
  TkDir_t(const TkDir_t  &tkd) 		{*this = tkd;}
  TkDir_t(const Mtx43D_t &tkd) 		{*this = tkd;}
  void Clear() 				{memset(mTkd[0],0,sizeof(mTkd));
               				 for(int i=0;i<3;i++){mTkd[i][i]=1;};}
operator const Mtx43D_t &() const 	{return mTkd;}  
operator       Mtx43D_t &() 		{return mTkd;};  
TkDir_t        &operator=(const TkDir_t  &tkd)
                         {assert(mTkd!=tkd); memcpy(mTkd[0],tkd[0],sizeof(mTkd)); return *this;}      
TkDir_t        &operator=(const Mtx43D_t &tkd)
                         {assert(mTkd!=tkd); memcpy(mTkd[0],tkd[0],sizeof(mTkd)); return *this;}     
const double*  T() const {return mTkd[kKT];}
const double*  U() const {return mTkd[kKU];}
const double*  V() const {return mTkd[kKV];}
  void Backward();
   int Same(const TkDir_t &as) const;
protected:
  Mtx43D_t mTkd;

};


class THDer3d_t
{
public:
friend class THelix3d;
friend class TRungeKutta;
THDer3d_t() 		{ Clear();}
void Clear();		
 operator const  Mtx55D_t &() 	const	{ return mDer	    ;}	//
 operator        Mtx55D_t &()     	{ return mDer	    ;}	//
 const TkDir_t  &TkDir(int idx)	const 	{ return mTkDir[idx];}
          void   SetTkDir(int idx, const TkDir_t& tkd)	{ mTkDir[idx]=tkd;}
          double Len() 		const	{ return mLen 	    ;}
          void Backward();
THDer3d_t &operator*=(const THDer3d_t &by);
 

protected:
double   mLen;
Mtx55D_t mDer;
TkDir_t mTkDir[2];
};

class THEmx3d_t
{ 
public:
     THEmx3d_t()		{ Clear();}
     THEmx3d_t(double const err[15],TkDir_t *tkDir = 0) { Clear();Set(err,tkDir);}
operator const double* () const	{ return &mUU;}		
operator       double* ()	{ return &mUU;}		
     THEmx3d_t &operator*=(double f)       	
              { for (int i=0;i<15;i++) {(*this)[i]*=f;} return *this;}
void  Clear(); 			
const TkDir_t &TkDir()	const 	 { return mTkDir;}
      TkDir_t &TkDir()	 	 { return mTkDir;}
 void Set(double const err[15],TkDir_t *tkDir = 0);  
 void Set(double eUU,double eUV,double eVV);  
 void Set(const THelixTrack_ *ht);  
 void Add(double Theta2,double Orth2,double PinvRr);
 void Move(const THDer3d_t *der);
 void Backward();
 void Update(const TkDir_t &tkDir);
 void Print(const char *tit=0) const;
double Trace() const ;
double Sign() const;
double Len() const 		{ return mLen;}
   int Times(int jk) const 	{ return mTimes[jk];} 
public:
TkDir_t mTkDir;
//  dU:  along Ort to track and Z axis
//  dV:  along Ort to track and dU. When track Ort to Z then it is Zaxis
//  dF == dFita
//  dL == dLama   
//  dPinv Pinv = iQ/Ptot: 
double
mUU,
mUV, mVV,
mUF, mVF, mFF,
mUL, mVL, mFL, mLL,
mUP, mVP, mFP, mLP, mPP;
double   mLen;
int      mTimes[2];
};
class THelix3d : public THelixTrack_ 
{
public:

	THelix3d();
	THelix3d(int charge,const double xyz[3],const double mom[3],const double mag[3]);
	THelix3d(const THelix3d &from);
	THelix3d(const THelix3d *from);	//Special ctr without errs
virtual ~THelix3d();
        void Clear(const char* opt=0);
THelix3d &operator=(const THelix3d &from);
	void Set   (int charge,const double xyz[3],const double mom[3],const double bMag[3]=0);
        THEmx3d_t *SetEmx(THEmx3d_t *emx=0);
        THEmx3d_t *SetEmx(const double G[15]);
  THEmx3d_t *Emx() const	  	{ return fEmx3d;}
        void SetDerOn(int derOn=1)	{ fDerOn =derOn;}
         int IsDerOn() const 	  	{ return fDerOn;}
///		Change direction
	void Backward();
///		Move along helix
	double Move(double step,double F[5][5]=0);
///		Evaluate params with given step along helix
	double Eval(double step, double xyz[3]=0, double mom[3]=0);
///		Get current parameters
///		Distance to crossing 2nd order surface
///             surf[0]+surf[1]*x+surf[2]*y+surf[3]*z
///            +surf[4]*x*x +surf[5]*y*y+surf[6]*z*z
///            +surf[7]*x*y +surf[8]*y*z+surf[9]*z*x  == 0
///            nearest==0 search alon direction, else the nearest
///        double Path(double stmax, const double *surf, int nsurf
///	           ,double *x=0, double mom[3]=0, int nearest=0) const;

///		Distance to nearest point to given space point
        double Path(const double point[3],double xyz[3]=0, double mom[3]=0); 
///		DCA to given space point (with error matrix)
        double Dca(const double point[3],double *dcaErr=0);

///		Distance to nearest point to given 2dim point
        double Path(double x,double y);
///		DCA to given 2dim point (with error matrix)
        double Dca(double x,double y,double *dcaErr=0)  ;

///		Returns length to nearest point of other helix 
///             along this helix. *s2 the length along the other helix
///             Both lengths are positive

//        double Path(const THelix3d &hlx,double *s2=0) const ;

///		Extention of previous method. 
///             Both lengths could be -ve.
///             *dist - distance between helicies
///		xyz[6] - 1st and 2nd space points on the helicies
//        double PathX(const THelix3d &hlx,double *s2=0
//	            ,double *dist=0, double xyz[3]=0) const;

///		distance and DCAxy and DCAz to given space point (with error matrix)
//        double Dca(const double point[3]
//                  ,double &dcaXY,double &dcaZ,double dcaEmx[3],int kind=3) const;

        const   double *Pos()    const {return fX3d;}
                double *Pos()          {return fX3d;}
        const   double *Dir()    const {return fD3d;}
                double *Dir()          {return fD3d;}
        const   double *Mom()    const {return fP3d;}
                double  MomTot() const {return fMom;}
        const   double *Mag()    const {return fH3d;}
                int  Charge()    const {return fCharge;}
                double Pinv()    const {return fPinv;}
        const THDer3d_t *Der()   const {return fDer3d;}
        const TkDir_t  &TkDir(int idx)	const { return fDer3d->mTkDir[idx];}
         void DoTkDir(TkDir_t &tkdir);

//        void Show(double len, const THelix3d *other=0) const;
//        void Print(Option_t *opt="") const;
//	statics
static void MakeTkDir( const double T[3],const double H[3],TkDir_t &tkd);
static void Test();
static void Test2();
static void TestDer();
static void TestDer2();
static void TestErr(int charge=-1);
static void TestErr2(int charge=-1);
static void TestErr3(int charge=-1);
static void ConvertErrs(const THelixTrack_ *he, const double Bz
                       ,double G[15],double D[5][5]=0,double Di[5][5]=0);
static void TestConvertErrs();
static void TestConvertDers();
protected:
static void MakeLocal( const double T[3],const double H[3],double sys[3][3]);


protected:
	void MakeMtx();
        void ToGlobal();
        void ToLocal();
        void ToGlobal(const double locX[3],const double locD[3]
	           ,        double gloX[3],      double gloD[3],double gloP[3]) const;
        void GetdDdL(double dDdL[3]) const;
        
protected:
	void Build();
        char   fBeg3d[1];  
        char   fDerOn;
        char   fCharge;
	double fX3d[3];
	double fP3d[3];		// momentum
	double fD3d[3];		// direction
        double fPinv;
	double fH3d[4];		//[0:2] mag field, [3]=module of the field
        double fLoc[3][3];
        double fMom;		//full momentum
	double fX3dPre[3];	// Previous position
	double fD3dPre[3];	// Previous direction
	double fP3dPre[3];	// Previous momentum
        double fPpre[3];	// Previous direction in local

	mutable double fLen;
        char fMed3d[1];

        THEmx3d_t *fEmx3d;
        THDer3d_t *fDer3d;
        
        char fEnd3d[1];
ClassDef(THelix3d,0)
};


#endif // THELIX3D_H
