// $Id: StMCStepping.h,v 1.4 2011/05/04 17:43:32 perev Exp $
//
//
// Class StMCStepping
// ------------------


#ifndef STMC_STEPPING_H
#define STMC_STEPPING_H

#include "TString.h"
#include "TLorentzVector.h"
#include "GCall.h"
class TGeoNode;
class TGeoVolume;
class TGeoMedium;
class TGeoMaterial;
class StMCStepping : public GCall
{
  public:
//   All cases
  enum SteppingCase {
  kNewTrack 		 =   1,
  kRootGeometrySupported =   2,
  kTrackAlive 		 =   4,
  kTrackDisappeared 	 =   8,
  kTrackEntering 	 =  16,
  kTrackExiting 	 =  32,
  kTrackInside           =  64,
  kTrackOut              = 128,
  kTrackStop             = 256};

//   Main cases (KaZes)
  enum SteppingKaze {
  kNEWtrack 		= 1,
  kENTERtrack		= 2,
  kCONTINUEtrack	= 4,
  kEXITtrack		= 8,
  kENDEDtrack		=16,
  kOUTtrack		=32,
  kIgnore    		=64};

  enum MediumPars {
   kIsvol =0,
   kIfield=1,
   kFieldm=2,
   kTmaxfd=3,
   kStemax=4,
   kDeemax=5,
   kEpsil =6,
   kStmin =7,
  };
		  
         StMCStepping(const char *name="",const char *tit="");
virtual ~StMCStepping(){}    
    // methods
virtual int  Fun();
void  SetDir(int dir) 				{fDir=dir; }
const TLorentzVector &CurrentPosition() const 	{return fCurrentPosition;}
const TLorentzVector &CurrentMomentum() const 	{return fCurrentMomentum;}
              double  CurrentLength()   const   {return fCurrentLength  ;}
                 int  Charge()          const   {return (int)fCharge    ;}

virtual void Print(const Option_t* opt=0) const;
static  TString CaseAsString(int kase);
static  TString KazeAsString(int kase);

protected:
void Case();
private:
void RecovEloss();
// 		data members
  protected:
   char   fBeg[1];
   float  fEnterLength;
   float  fCurrentLength;
   float  fLastLength;
   float  fCharge;
   float  fMass;
   float  fEdep;
   float  fEtot;
   float  fLife;
   float  fX0;
   float  fLastVect[7];
   double fTrackNumber;
   int    fPDG;
   int    fPid;
   int    fTrType;
   int    fKaze;
   int    fKazePrev;
   int    fCase;
   int    fDir;
   int    fSteps;
   TGeoNode   *fNode;
   TGeoVolume *fVolume;
   TGeoMedium *fMedium;
   TGeoMaterial *fMaterial;
   char   fEnd[1];
   
   TLorentzVector fStartPosition;
   TLorentzVector fEnterPosition;
   TLorentzVector fCurrentPosition;
   TLorentzVector fEnterMomentum;
   TLorentzVector fCurrentMomentum;
   TLorentzVector fLastMomentum;
   TString fParName;
   TString fCasName;
   TString fKazName;
   ClassDef(StMCStepping,0) // Extended TParticle
};

#endif //STMC_STEPPING_H   
   
