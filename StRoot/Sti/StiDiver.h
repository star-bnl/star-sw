/// \File StiDiver.h
/// \author Victor Perev 01/2010
#ifndef StiDiver_HH
#define StiDiver_HH
#include "TNamed.h"
#include "StarVMC/GeoTestMaker/GCall.h"
#include "StarVMC/GeoTestMaker/StMCStepping.h"

class StiMCStepping;
class StiMCPrimaryGenerator; 
class StiELossTrak;
class StarMagField;
class StiMCField;
class TGeoMaterial;
class THelixTrack;
class StiFitErrs;
class StiNodePars;
typedef double Mtx55D_t[5][5];

/// \class StiDiver
class StiDiver : public TNamed
{
public:
  StiDiver(const char *name="");
  virtual ~StiDiver(){;}
int  Init();
void Reset();
int  Dive();
void Set(const StiNodePars *inpar,const StiFitErrs *inerr,int idir);
void Set(      StiNodePars *otpar,      StiFitErrs *oterr,Mtx55D_t *deriv);
double GetLength() const;
protected:
char mBeg[1];
int mDir;
const StiNodePars    *mInpPars;
const StiFitErrs     *mInpErrs;
StiNodePars    *mOutPars;
StiFitErrs     *mOutErrs;
Mtx55D_t       *mOutDeri;
THelixTrack    *mHelix;
StiELossTrak    *mELoss;

StiMCStepping  *mSteps;
StiMCField     *mFld;
StiMCPrimaryGenerator *mGen;
char mEnd[1];
ClassDef(StiDiver,0);
};
// Class StMCInitApp
// ------------------

class StiMCInitApp : public GCall
{
public:
    StiMCInitApp();
   ~StiMCInitApp(){}    
    // methods
   int Fun();
protected:
    ClassDef(StiMCInitApp,0) // 
};

// Class StMCStepping
// ------------------


class StiMCStepping : public StMCStepping
{
public:
         StiMCStepping(const char *name="",const char *tit="");
virtual ~StiMCStepping();   
    // methods

virtual int  Fun();
void Reset ();
int  GetExit() const 		{return fExit;}
void Set(StiELossTrak *eLoss)	{fELossTrak = eLoss;}
void Set(THelixTrack *helx )	{fHelix    = helx ;}
void Set(Mtx55D_t    *deriv)	{fDeriv    = deriv;}
void Set(StiMCField  *field)	{fField    = field;}
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
float  fStartSign;
float  fCurrentSign;
char fMidl[1];
THelixTrack *fHelix;
Mtx55D_t    *fDeriv;
StiELossTrak *fELossTrak;
StiMCField  *fField;
const TGeoMaterial *fPrevMat;
char fLast[1];
private:

ClassDef(StiMCStepping,0) // 
};

class StiMCConstructGeometry : public GCall
{
  public:
    StiMCConstructGeometry(const char *gy);
    virtual ~StiMCConstructGeometry(){}    
    // methods
    int Fun();
  private:

  protected:
    // data members
  
    ClassDef(StiMCConstructGeometry,0) // 
};


class StiMCPrimaryGenerator :  public GCall  {
 public:
  StiMCPrimaryGenerator();
 ~StiMCPrimaryGenerator() {}
  
     void Set(const THelixTrack *hlx)		{mHelix=hlx ;}
     void Set(const StiNodePars *pars,int idir)	{mPars=pars; mDir =idir;}
     virtual int Fun();
 protected:
 const StiNodePars *mPars;
 const THelixTrack *mHelix;
 int mDir; // direction of moving 1=along track; 0=opposite
  ClassDef(StiMCPrimaryGenerator,0)  //StiMCPrimaryGenerator
};


class StiMCField :  public GCall  {
 public:
  StiMCField();
 ~StiMCField() {}
  virtual int FunDD(const double *x,double *b);
  double GetHz() const 		{return mHz;}
  void   SetHz(double hz) 	{mHz=hz;}
 protected:
 double mHz;
 StarMagField *mFild; // 

  ClassDef(StiMCField,0)    //
};

#endif
