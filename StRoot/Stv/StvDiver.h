/// \File StvDiver.h
/// \author Victor Perev 01/2010
#ifndef StvDiver_HH
#define StvDiver_HH
#include "TNamed.h"
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
typedef double Mtx55D_t[5][5];

/// \class StvDiver
class StvDiver : public TNamed
{
public:
  StvDiver(const char *name="");
  virtual ~StvDiver(){;}
int  Init();
void Reset();
int  Dive();
void Set(const StvNodePars *inpar,const StvFitErrs *inerr,int idir);
void Set(      StvNodePars *otpar,      StvFitErrs *oterr,Mtx55D_t *deriv);
void SetSkip(int skip=1);
double GetLength() const;
const StvELossData &GetELossData() const;

protected:
char mBeg[1];
int mDir;
const StvNodePars    *mInpPars;
const StvFitErrs     *mInpErrs;
StvNodePars    *mOutPars;
StvFitErrs     *mOutErrs;
Mtx55D_t       *mOutDeri; //Out derivatives in StvFitPars notation
THelixTrack    *mHelix;
StvELossTrak   *mELoss;
Mtx55D_t        mHlxDeri; //Internal derivatives in StHeliTrack notation
StvMCStepping  *mSteps;
StvMCField     *mFld;
StvMCPrimaryGenerator *mGen;

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

// Class StMCStepping
// ------------------


class StvMCStepping : public StMCStepping
{
public:
         StvMCStepping(const char *name="",const char *tit="");
virtual ~StvMCStepping();   
    // methods

virtual int  Fun();
void Reset ();
int  GetExit() const 		{return fExit;}
void Set(StvELossTrak *eLoss)	{fELossTrak = eLoss;}
void Set(THelixTrack *helx )	{fHelix    = helx ;}
void Set(Mtx55D_t    *deriv)	{fDeriv    = deriv;}
void Set(StvMCField  *field)	{fField    = field;}
void SetSkip(int skip=1)	{fSkip     = skip ;}
const StvELossData &GetELossData() const { return fELossData;}


 int BegVolume();
 int EndVolume();
 int IsDca00(int begEnd);

void FillHelix();
void Print (const Option_t* opt=0) const;
void Finish(const Option_t* opt=0);
        

protected:
char fFist[1];
int    fKount;
int    fExit;
int    fSkip;
float  fStartSign;
float  fCurrentSign;
char fMidl[1];
THelixTrack *fHelix;
Mtx55D_t    *fDeriv;		//Derivative matrix in THelixTrack notation
StvELossTrak *fELossTrak;	//Energy loss calculator
StvELossData  fELossData;	//Energy loss data
StvMCField  *fField;		//Mag field calculator
const TGeoMaterial *fPrevMat;
char fLast[1];
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
  
     void Set(const THelixTrack *hlx)		{mHelix=hlx ;}
     void Set(const StvNodePars *pars,int idir)	{mPars=pars; mDir =idir;}
     virtual int Fun();
 protected:
 const StvNodePars *mPars;
 const THelixTrack *mHelix;
 int mDir; // direction of moving 1=along track; 0=opposite
  ClassDef(StvMCPrimaryGenerator,0)  //StvMCPrimaryGenerator
};


class StvMCField :  public GCall  {
 public:
  StvMCField();
 ~StvMCField() {}
  virtual int FunDD(const double *x,double *b);
  double GetHz() const 		{return mHz;}
  void   SetHz(double hz) 	{mHz=hz;}
 protected:
 double mHz;
 StarMagField *mFild; // 

  ClassDef(StvMCField,0)    //
};

#endif
