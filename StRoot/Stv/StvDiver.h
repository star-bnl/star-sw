/// \File StvDiver.h
/// \author Victor Perev 01/2010
#ifndef StvDiver_HH
#define StvDiver_HH
#include "TNamed.h"
#include "StarRoot/TRungeKutta.h"
#include "StvUtil/StvNodePars.h"
#include "StarVMC/GeoTestMaker/GCall.h"
#include "StarVMC/GeoTestMaker/StMCStepping.h"
#include "StvUtil/StvNodePars.h"

class StvMCStepping;
class StvMCPrimaryGenerator; 
class StvELossTrak;
class StarMagField;
class StvMCField;
class TGeoMaterial;
class TRungeKutta;
class StvFitErrs;
class StvNodePars;
// Class StMCInitApp
// ------------------

class StvMCInitApp : public GCall
{
public:
    StvMCInitApp();
   ~StvMCInitApp(){}    
    // methods
   int Fun();
protected:
    ClassDef(StvMCInitApp,0) // 
};

// Class StvMCStepping
// ------------------


class StvMCStepping : public StMCStepping
{
public:
         StvMCStepping(const char *name="",const char *tit="");
virtual ~StvMCStepping();   
    // methods

virtual int  Fun();
void Reset ();
int  GetExit() const 		{return       fExit;}
void Set(StvELossTrak *eLoss)	{fELossTrak = eLoss;}
void Set(TRungeKutta *helx )	{fHelix    = helx  ;}
void Set(StvFitDers  *deriv)	{fDeriv    = deriv ;}
void SetOpt(int opt);
void SetPrev(const char *prev); 
void SetTarget(const double *target,int nTarget=3);


 int EndVolume();
 int TooMany();

void FillHelix();
void Print (const Option_t* opt=0) const;
void Finish(const Option_t* opt=0);

double GetLength() const {return fCurrentLength;}        
   int IsHitted()  const {return fHitted       ;}        

protected:
char fFist[1];
int     fOpt;
int    	fKount;
int    	fExit;
int    	fLastKaze;
int    	fLastNumb;
int    	fHitted;
int    	fNTarget;
float   fNearBeam;
float  	fStartSign;
float  	fCurrentSign;
float  	fPrevSign;
float   fTooManyLength;
double  fTarget[3];
const TGeoMaterial *fPrevMat;
TGeoVolume *fHALLVolu;
TGeoNode   *fPrevNode;
char fMidl[1];
TRungeKutta  *fHelix;
StvFitDers   *fDeriv;		//Derivative matrix in TRungeKutta notation
StvELossTrak *fELossTrak;	//Energy loss calculator
char fLast[1];
TString fCurrPath;		//Path in current  Dive()
TString fPrevPath;		//Path in previous Dive()
private:

ClassDef(StvMCStepping,0) // 
};

class StvMCConstructGeometry : public GCall
{
  public:
    StvMCConstructGeometry(const char *gy);
    virtual ~StvMCConstructGeometry(){}    
    // methods
    int Fun();
  private:

  protected:
    // data members
  
    ClassDef(StvMCConstructGeometry,0) // 
};


class StvMCPrimaryGenerator :  public GCall  {
 public:
  StvMCPrimaryGenerator();
 ~StvMCPrimaryGenerator() {}
  
     void Set(const StvNodePars *pars,int idir)	{mPars=pars; mDir =idir;}
     virtual int Fun();
 protected:
 const StvNodePars *mPars;
 int mDir; // direction of moving 1=along track; 0=opposite
  ClassDef(StvMCPrimaryGenerator,0)  //StvMCPrimaryGenerator
};


class StvMCField :  public GCall  {
 public:
  StvMCField();
 ~StvMCField() {}
  virtual int FunDD(const double *x,double *b);
//  double GetHz(const double X[3]);		
  const double *GetMag(const double *x,double *b=0);
 protected:
 double mH[3],mX[3];
 StarMagField *mFild; // 

  ClassDef(StvMCField,0)    //
};
/// \class StvDiver
class StvDiver : public TNamed
{
protected:
  StvDiver(const char *name="");
  virtual ~StvDiver(){;}
int  Init();
public:
static StvDiver* Inst();

enum StvDiverFlags {kDiveOk =0,kDiveHits=1,kDiveDca=2,kDiveBreak=4,kDiveMany=8};
enum StvDiverOpt   {kTargHit=1,kTarg2D  =2,kTarg3D =4,kDoErrs   =8};
public:
void Reset();
int  Dive();
void Set(const StvNodePars *inpar,const StvFitErrs *inerr,int idir);
void Set(      StvNodePars *otpar,      StvFitErrs *oterr,StvFitDers *deriv);
void SetDebug(int debLev=9) { mDebug = debLev;mSteps->SetDebug(debLev);}
void SetTarget(const double *target,int nTarget=3);
void SetOpt(int opt);
void SetPrev(const char *prev); 
double GetLength() const;
     StvELossTrak *TakeELoss();
void SetRZmax(double rMax,double zMax); 
 int IsDebug() const  { return mDebug;}
 int IsHitted() const { return mSteps->IsHitted();}

static int *G3Debug(int idebug);
static int *G3Debug(int idebug,int idemin,int idemax,int itest);

protected:
static StvDiver *mgInst;

char mBeg[1];
int mDir;
int mDebug;
const   StvNodePars    *mInpPars;
const 	StvFitErrs     *mInpErrs;
	StvNodePars    *mOutPars;
	StvFitErrs     *mOutErrs;
	StvFitDers     *mOutDeri; 	//Out derivatives in StvFitPars notation
	TRungeKutta    *mHelix;
	StvELossTrak   *mELoss;
	StvMCStepping  *mSteps;
	StvMCField     *mFld;
StvMCPrimaryGenerator  *mGen;

char mEnd[1];
ClassDef(StvDiver,0);
};

#endif
