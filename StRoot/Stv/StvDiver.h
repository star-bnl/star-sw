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
enum StvDiverFlags {kDiveOk=0,kDiveHits,kDiveDca,kDiveBreak,kDiveMany};
/// \class StvDiver
class StvDiver : public TNamed
{
public:
  StvDiver(const char *name="");
  virtual ~StvDiver(){;}
int  Init();
void Reset();
int  Dive();
void Set(StvNodePars *inpar,const StvFitErrs *inerr,int idir);
void Set(StvNodePars *otpar,      StvFitErrs *oterr,StvFitDers *deriv);
void SetSkip(int skip=1);
double GetLength() const;
const StvELossData GetELossData() const;
void SetRZmax(double rMax,double zMax); 

protected:
char mBeg[1];
int mDir;
      StvNodePars    *mInpPars;
const StvFitErrs     *mInpErrs;
StvNodePars    *mOutPars;
StvFitErrs     *mOutErrs;
StvFitDers       *mOutDeri; //Out derivatives in StvFitPars notation
THelixTrack    *mHelix;
StvELossTrak   *mELoss;
StvHlxDers        mHlxDeri; //Internal derivatives in StHeliTrack notation
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
void Set(StvHlxDers  *deriv)	{fDeriv    = deriv;}
void Set(StvMCField  *field)	{fField    = field;}
void SetSkip(int skip=1)	{fSkip     = skip ;}


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
int    fLastKaze;
int    fLastNumb;
int    fHitted;
float  fStartSign;
float  fCurrentSign;
char fMidl[1];
THelixTrack  *fHelix;
StvHlxDers     *fDeriv;		//Derivative matrix in THelixTrack notation
StvELossTrak *fELossTrak;	//Energy loss calculator
StvMCField   *fField;		//Mag field calculator
const TGeoMaterial *fPrevMat;
char fLast[1];
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
  double GetHz() const 		{return mHz;}
  void   SetHz(double hz) 	{mHz=hz;}
 protected:
 double mHz;
 StarMagField *mFild; // 

  ClassDef(StvMCField,0)    //
};

#endif
