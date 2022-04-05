#ifndef TRUNGEKUTTA_H
#define TRUNGEKUTTA_H
#include <assert.h>
#include <list>
#include "THelix3d.h"

class TRKuttaMag 
{
  public:
    TRKuttaMag (){;}
    virtual ~TRKuttaMag(){}    
    // methods
    virtual void operator()(const double X[3], double B[3]) 
    { assert(0 && "Method not defined");};
  protected:
    // data members
};  
#define kMicron 1e-4

class TRungeKutta : public TObject 
{
public:
class TRKutaPars_t { public: double X[3],D[3],P[3],Pinv;
                   void Update(){for (int i=0;i<3;i++){P[i]=D[i]/fabs(Pinv);}}
                   void Print(const char *tit) const;
		   };
class TRKutaPoint {public: double Len; TRKutaPars_t pars; double B[3];};


typedef std::list<TRKutaPoint> TRKutaList;


	TRungeKutta();
	TRungeKutta(int charge,const double xyz[3],const double mom[3],TRKuttaMag* mag=0);
	TRungeKutta(const TRungeKutta &from);
	TRungeKutta(const TRungeKutta *from);	//Special ctr without errs
	TRungeKutta(const THelixTrack_ &from,TRKuttaMag* mag =0);
virtual ~TRungeKutta();
TRungeKutta &operator=(const TRungeKutta &from);
	void SetMag (TRKuttaMag* mag);
	void ZetMag (const double *h);//init FbInp & fBOut by the same field
static 	void SetMAG (TRKuttaMag* mag){ fgMag  = mag ;};
	void Set   (int charge,const double xyz[3],const double mom[3]);
        void SetEmx(const double *err=0);
        void SetEmx(const THEmx3d_t *err);
  THEmx3d_t *Emx() const	  { return fEmx3d;}
        void SetDerOn(int derOn=1){ fDerOn =derOn;}
         int IsDerOn() const 	  { return fDerOn;}
///		Change direction
	void Backward();
///		Move along track
	double Move(double step);
///		Evaluate params with given step along helix
	double Eval(double step, double xyz[3]=0, double mom[3]=0) const;
///		Get current parameters
///		Path to nearest point to given space point
        double Path(const double point[3], int idir=0,int ndca=3) const; 

///		Path  to nearest point to given 2dim point
        double Path(double x,double y, int idir=0) const ;
///		Move to point founded before
        double Move();

///		DCA to given 2dim point (with error matrix)
        double Dca(double x,double y,double *dcaErr=0) const ;
        double Dca(const double point[3],double *dcaErr) const;

        const   double *Pos()    const {return fInp.X;}
        const   double *Dir()    const {return fInp.D;}
        const   double *Mom()    const {return fInp.P;}
        const   double *Mag()    const {return fBOut ;}
                double  MomTot() const {return fabs(1./fInp.Pinv);}
                double  Pinv()   const {return fInp.Pinv;}
                  void  SetPinv(double Pinv) {fInp.Pinv=Pinv;fInp.Update();}
                   int  Charge() const {return fCharge;}
                double  GetCurv()  const; //Full curvature in current point
                   int  GetDir()   const; //0=Outside==>Inside

        const THDer3d_t *Der()   const {return fDer3d;}
        const TkDir_t  &TkDir(int idx)	const { return fDer3d->mTkDir[idx];}

       void Print(Option_t *opt="") const;
//	statics
static void Test(int flag=3);
static void Test2();
static void Test3();
static void TestBak();
static void TestDer();
static void TestDer2();
static void TestErr (int charge=1);
static void TestErr2(int charge=1);
static void TestSign();


protected:
	void MakeMtx();
        void grkuta(double CHARGE,double STEP
	,const TRKutaPars_t &VECT,TRKutaPars_t &VOUT) const;
        
protected:
        char   fBeg[1];  
        char   fDerOn;
        char   fCharge;
        TRKuttaMag *fGUFLD;	
        TRKutaPars_t fInp;
        THEmx3d_t *fEmx3d;
        THDer3d_t *fDer3d;
	mutable int fNStps;
	mutable double fLen;
	mutable TRKutaPars_t fOut;
        mutable double fBInp[3];
        mutable double fBOut[3];

        char fEnd[1];
static TRKuttaMag *fgMag;
ClassDef(TRungeKutta,0)
};


#endif // TRUNGEKUTTA_H
