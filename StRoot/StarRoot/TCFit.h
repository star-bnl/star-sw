// @(#)root/base:$Name:  $:$Id: TCFit.h,v 1.1 2007/12/18 23:12:15 perev Exp $
// Author: Victor Perev   05/07/2007
// Class for Fit with constrains. 
// TCFit   - fitter
// TCFitData - base class to define data
// TCFitV0   - Example of user class, inherited from TCFitData. This class
//             Fit V0's


#ifndef ROOT_TCFit
#define ROOT_TCFit
#include "Rtypes.h"
#include "TNamed.h"
#include "TString.h"
#include "TMatrixD.h"
class TCFitData;


class TCFit : public TNamed {
public:
enum FitResult {
   kFitOK       = 0,
   kBadAprx 	= 0x001,	//Bad approximation, do not continue
   kBadFcn 	= 0x002,	//Fcn too big, do not continue
   kBadCon 	= 0x004,	//Cnr too big, do not continue
   kTooItr 	= 0x008,	//Too many iterations
   kFatal	= 0xfff         //Unknown fatal error
};
enum FitAction {
   kEndFit      = 0,
   kNextStep    = 1,
   kNextCut     = 2,
   kBadFit      = 3,
   kTooIter     = 4
};

public:
             TCFit(const char *name,TCFitData *dat=0);
virtual     ~TCFit();
        void Reset();
       Int_t SetData(TCFitData *dat);  
        void SetMaxIter(int maxiter) { fMaxIter=maxiter;}
        void SetMaxCuts(int maxcuts) { fMaxCuts=maxcuts;}
        void SetDebug(int deb=1) { fDebug=deb;}
         int Fit();  
      double ErMx(int jcol,int jrow) const; 
static void Test0();
private:
       int   PriStep(const char *tit="");

private:

  int        CheckIn();
  int        CheckOut();
  int        FitStep();
  int        CheckStep();
  int        CutStep();
  int        EndStep();


  TCFitData *fDat;
  TMatrixD  *fBigM;
  TMatrixD  *fBigI;
  TMatrixD  *fBigB;
  TMatrixD  *fOldP;
  TMatrixD  *fAddP;
  int        fDebug;
  int        fUPars;
  int        fUMeas;
  int        fUCons;
  int        fIter;
  int        fMaxIter;
  int        fCuts;
  int        fMaxCuts;
  int        fAkt;
  int        fFitRes;
  double     fFcnQA[2];
  double     fConQA[2];
  double     fAddQA[2];
ClassDef(TCFit,0)

};


class Deriv1st;
class Deriv2nd;
class TCFit;

class TCFitData : public TNamed {
friend class Deriv1st;
friend class Deriv2nd;
public:
enum {kMEAS=0,kSLAC=1,kCNSR=2};
enum {kMaxId=100};  
public:
           TCFitData(const char *name, const char *title="");
  virtual ~TCFitData();
  void     Reset();

/// Add measured parameter   
/// tyPar: 0=meas, 1=slack,2=constr
int AddPar(int tyPar,int idPar,double *par,int nPars=1,const char *name="",double tiny=0.);

int GetId(const char *name)    const;	//get par id by name
int GetId(int jd)    const;		//get par id by jd
int GetJd(int id)    const;		//get par jd by id
const char *GetNam(int idx)    const; 	//get par name by id	
int GetType(int id)    const;		//get type(Meas,Slac,Constr) by dd

void FixPar (int ipar,int yes=1);       //(dis/en)able parameter 
int  IsFixed(int ipar) const ;        		//

    
  virtual int      Ready();  			//Ready() could be overloaded 
  virtual int      Approx()=0;  		//Approx() must be overloaded 
  virtual double   Fcn()=0;  			//Fcn,              could be overloaded 
  virtual double  DFcn(int ipar);  		//dFcn/dPar         could be overloaded
  virtual double DDFcn(int ipar,int jpar);    	//d2Fcn/dPar1/dPar2 could be overloaded

  virtual double   Con(int icon);          	//constrain, must be overloaded
  virtual double  DCon(int icon,int ipar); 	//dCon/dPar         could be overloaded 
  						//d2Con/dPar1/dPar2 could be overloaded 
  virtual void   Update()=0; 			//Called when status changed,
                				//must be overloaded 
  virtual void Print(const char *name) const;


//----------------------------------------------------------------------------- 
  void    SetFitter(const TCFit *fitter)       	{fFitter = fitter;}
  void    SetFail(int ifail)       		{fFail   = ifail ;}
  int     GetFail() const 			{return fFail;}
  int     Modified() const         		{return fModi;}
  void    Modify(int m=1)     			{fModi =    m;}
  void    Evaluate();
  double &GetPar (int ipar);  
  double  GetPar (int ipar) const ;  
  double  GetTiny(int ipar) const 		{return fTiny[ipar];}  
  double  GetFcn  () const			{return fFcn[0];} 
  void    SetFcn  (double fcn) 			{fFcn[0] = fcn ;} 
  void    SetFcn  (double tiny,double big) 	{fFcn[1] = tiny; fFcn[2]=big;} 
  double  GetBigFcn()  const 			{return fFcn[2];}
  double  GetTinyFcn() const 			{return fFcn[1];}
    
  int     GetNPars()     const {return fNPars[0]+fNPars[1];}
  int     GetNMeas()     const {return fNPars[0];}
  int     GetNSlac()     const {return fNPars[1];}
  int     GetNCons()     const {return fNPars[2];}
  int     GetUPars()     const {return fNPars[0]-fNFixs[0]+fNPars[1]-fNFixs[1];}
  int     GetUMeas()     const {return fNPars[0]-fNFixs[0];}
  int     GetUSlac()     const {return fNPars[1]-fNFixs[1];}
  int     GetUCons()     const {return fNPars[2]-fNFixs[2];}
  int     GetNDF()	 const {return GetUCons()-GetUSlac();}
  double  ErMx(int icol,int irow) const;

//----------------------------------------------------------------------------- 
private:
//----------------------------------------------------------------------------- 
protected:
const TCFit *fFitter;

  char   fBeg[1];
  int    fFail;		//!fail flag, fit is impossible
  int    fModi;
  int    fFlag;		//!&1=1 fcn calculated from error matrix
  int    fNPars[3];	//!number of "measured" ,slack, constrains
  int    fNFixs[3];		//!number of "slack"    parameters

  double *fPars[kMaxId];
  double  fTiny[kMaxId];	//!Tiny values still modifying fcn 
  short   fTyps[kMaxId];
  short   fFixs[kMaxId];
  char    fIndx[kMaxId];
  char    fJndx[kMaxId];
  double  fFcn[3];		//!Current value of fcn [1]
  char    fEnd[1];
  TString fNams[kMaxId];	//!names of params
private:
  Deriv1st *fD1st;	//!
  Deriv2nd *fD2nd;	//!
ClassDef(TCFitData,0)
};
//end TCFitData


class TLorentzVector;
class THelixTrack;
class TVector3;
class TkErrs;

class TkPars {
// Track parameters in the perigee point (near 2D point xvtx,yvtx)
public:	
     TkPars()			{ Reset(); SetHz();}
void Reset();			
void Update(){curv = hz*ptin;}
void Print(const char *tit) const;
      double *Arr()       	{ return &dca;}
const double *Arr() const 	{ return &dca;}
      double  P()   const 	{ return sqrt(1.+tanl*tanl)/fabs(ptin);}
      double  E()   const 	{ return sqrt((1.+tanl*tanl)/(ptin*ptin)+mass*mass);}
TLorentzVector P4()  const;
TVector3       V3()  const;
      void    Fill(THelixTrack &hlx);
      void    Set(const TVector3 &v3,const TVector3 &d3  ,double  pts  );
      void    Get(      TVector3 *v3,      TVector3 *d3=0,double *pts=0) const;
      void    Rand(const TkErrs &errs);
      void    SetHz(double factor=1.);

static const char*  Name(int mem);
static       double Tiny(int mem);


TkPars &operator+=(const TkPars &a);
  /// point 
  double  dca,z;
  /// angle between track direction and X-axis in xy plane
  double phi;
  /// signed invert pt [sign = sign(-qB)]
  double ptin;  
  /// tangent of the track momentum dip angle
  double tanl;
  /// signed curvature [sign = sign(-qB)]
  double curv;  
  /// Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)
  double hz;  
  /// Mass of track
  double mass;
};

class TkErrs {
public:	

     TkErrs() 		{Reset();} 
void   Reset();		
void   Set(int i,int j,double err);		
double Get(int i,int j) const;		
void   Invert();
double Xi2(const TkPars &pars) const;
void   Mpy(const TkPars &pars,double der[5]) const;
public:	
  double emx[15];

};

class VxPars {
public:	
VxPars() {memset(x,0,sizeof(x));}

TVector3 V3() const;

  double x[3];
public:	
};

class VxErrs {
public:	
  double emx[6];
};

class TCFitV0 : public TCFitData {
public:
  TCFitV0();
 ~TCFitV0(){;}
  virtual double   Fcn();  			
  virtual double  DFcn(int ipar);  		
  virtual double DDFcn(int ipar,int jpar);    

  virtual double   Con(int icon);          //constrain, must be overloaded
  virtual double  DCon(int icon,int ipar); //dCon/dPar         could be overloaded 
  int     Ready();  
  int     Approx();  
  virtual void     Update();  			
  virtual void Print(const char *name) const;
public:
  void Reset();
static void Test();

private:
public:
TkPars mTkBas[2];
TkErrs mTEBas[2];
TkPars mTkFit[2];
TkPars mTkDif[2];

char   mBeg[1];
char   mReady;

VxPars mVx;
VxErrs mVE;
double mLen[3];

double mConr[7];
double mDFcn[2][5];
double mDConDL[3][3];
double mMas;
char   mEnd[1];

ClassDef(TCFitV0,0)
};

#endif //ROOT_TCFit












  
