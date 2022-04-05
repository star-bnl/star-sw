/// \File StvDiver.h
/// \author Victor Perev 01/2010
#ifndef StvDiver_HH
#define StvDiver_HH
#include "TNamed.h"
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
class THelixTrack;
class StvFitErrs;
class StvNodePars;
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
void SetTarget(const double *target,int nTarget=3);
void SetOpt(int opt);
double GetLength() const;
     StvELossTrak *TakeELoss();
void SetRZmax(double rMax,double zMax); 

protected:
static StvDiver *mgInst;

char mBeg[1];
int mDir;
const   StvNodePars    *mInpPars;
const 	StvFitErrs     *mInpErrs;
	StvNodePars    *mOutPars;
	StvFitErrs     *mOutErrs;
	StvFitDers     *mOutDeri; 	//Out derivatives in StvFitPars notation
	THelixTrack    *mHelix;
	StvELossTrak   *mELoss;
	StvHlxDers      mHlxDeri; 	//Internal derivatives in StHelixTrack notation
	StvMCStepping  *mSteps;
	StvMCField     *mFld;
StvMCPrimaryGenerator  *mGen;

char mEnd[1];
ClassDef(StvDiver,0);
};
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
void Set(THelixTrack *helx )	{fHelix    = helx  ;}
void Set(StvHlxDers  *deriv)	{fDeriv    = deriv ;}
void Set(StvMCField  *field)	{fField    = field ;}
void SetOpt(int opt);
void SetTarget(const double *target,int nTarget=3);


 int BegVolume();
 int EndVolume();
 int IsDca00(int begEnd);
 int IsOverstep() const;
 int TooMany();

void FillHelix();
void Print (const Option_t* opt=0) const;
void Finish(const Option_t* opt=0);
        

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
float   fTooManyLength;
double  fTarget[3];
char fMidl[1];
THelixTrack  *fHelix;
StvHlxDers   *fDeriv;		//Derivative matrix in THelixTrack notation
StvELossTrak *fELossTrak;	//Energy loss calculator
StvMCField   *fField;		//Mag field calculator
const TGeoMaterial *fPrevMat;
TGeoVolume *fHALLVolu;
TGeoNode   *fPrevNode;
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
  double GetHz(const double X[3]);		
 protected:
 double mH[3],mX[3];
 StarMagField *mFild; // 

  ClassDef(StvMCField,0)    //
};

#endif
