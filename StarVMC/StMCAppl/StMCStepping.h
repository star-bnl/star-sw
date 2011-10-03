// $Id: StMCStepping.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCStepping
// ------------------


#ifndef STMC_STEPPING_H
#define STMC_STEPPING_H

#include "TString.h"
#include "TLorentzVector.h"
#include "GCall.h"

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

class MyMate { public:
char name[100];
   Float_t a;
   Float_t z;
   Float_t dens;
   Float_t radl;
   Float_t absl;   
   Int_t npars;
   Float_t pars[100];
  };
class MyMedi { public:
char name[100];
Int_t nmat;
Int_t isvol;
Int_t ifield;
Float_t fieldm;
Float_t tmaxfd;
 
Float_t stemax;
Float_t deemax;
Float_t epsil;
 
Float_t stmin;
Int_t npars;
Float_t pars[100];
}; 

		  
         StMCStepping(const char *name="",const char *tit="");
virtual ~StMCStepping(){}    
    // methods
virtual int  Fun();
virtual void Print(const Option_t* opt=0) const;
TString GetMateName(int mat);
TString GetMediName(int med);
TString GetVoluName(int vol);
static  TString CaseAsString(int kase);
static  TString KazeAsString(int kase);

  protected:
    void Case();
    // data members
   TLorentzVector fStartPosition;
   TLorentzVector fEnterPosition;
   TLorentzVector fCurrentPosition;
   TLorentzVector fEnterMomentum;
   TLorentzVector fCurrentMomentum;
   float  fEnterLength;
   float  fCurrentLength;
   float  fCharge;
   float  fMass;
   float  fEdep;
   float  fEtot;
   float  fLife;
   double fTrackNumber;
   MyMate fMate;
   int    fMatId;
   MyMedi fMedi;
   int    fMedId;
   int    fVolId;
   int    fVolNb;
   int    fPDG;
   int    fPid;
   int    fTrType;
   int    fKaze;
   int    fKazePrev;
   int    fCase;
   
   TString fVolName;
   TString fMatName;
   TString fMedName;
   TString fParName;
   TString fCasName;
   TString fKazName;
    ClassDef(StMCStepping,0) // Extended TParticle
};

#endif //STMC_STEPPING_H   
   

