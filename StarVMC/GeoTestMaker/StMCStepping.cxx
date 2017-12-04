// $Id: StMCStepping.cxx,v 1.10.6.1 2017/12/04 19:58:59 perev Exp $
//
//
// Class StMCStepping
// ------------------

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "StMCStepping.h"
#include "TPDGCode.h"
#include "TVirtualMC.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TGeant3.h"

static TVirtualMC *myMC=0;

ClassImp(StMCStepping)
int SteppingCasesI[] = {
  StMCStepping::kNewTrack ,
  StMCStepping::kTrackAlive,
  StMCStepping::kTrackDisappeared,
  StMCStepping::kTrackEntering,
  StMCStepping::kTrackExiting,
  StMCStepping::kTrackInside,
  StMCStepping::kTrackOut,
  StMCStepping::kTrackStop,
  0};

const char *SteppingCasesC[] = {
  "NewTrack",
  "TrackAlive",
  "TrackDisappeared",
  "TrackEntering",
  "TrackExiting",
  "TrackInside",
  "TrackOut",
  "TrackStop",
  0};

int SteppingKazesI[] = {
  StMCStepping::kNEWtrack,
  StMCStepping::kENTERtrack,
  StMCStepping::kCONTINUEtrack,
  StMCStepping::kEXITtrack,
  StMCStepping::kENDEDtrack,
  StMCStepping::kOUTtrack,
  StMCStepping::kIgnore,
  0};

const char *SteppingKazesC[] = {
  "NEWtrack",
  "ENTERtrack",
  "CONTINUEtrack",
  "EXITtrack",
  "ENDEDtrack",
  "OUTtrack",
  "Ignore",
  0};

class MyAux 
{
public:
float fLen,fP,fEdep;
};

//_____________________________________________________________________________
StMCStepping::StMCStepping(const char *name,const char *tit)
  : GCall(name,tit)
{
  memset(fBeg,0,fEnd-fBeg+1);	
  fDebug = 0;
  fKazePrev = -1;
  myMC = 0;
  fDir = 1;
}   
//_____________________________________________________________________________
void StMCStepping::Reset()
{
  memset(fBeg,0,fEnd-fBeg+1);	
  fKazePrev = -1;
  fStartPosition*=0.;
  fStartMomentum*=0.;
  fEnterPosition*=0.;
  fEnterMomentum*=0.;
  fCurrentPosition*=0.;
  fCurrentMomentum*=0.;
  fPrevPosition*=0.;
  fPrevMomentum*=0.;
}
//_____________________________________________________________________________
void StMCStepping::Print(const Option_t*) const
{
  double lenTot = 0,eTot=0;
  printf("   totLen=%g totE=%g\n",lenTot,eTot);
}		
//_____________________________________________________________________________
TString StMCStepping::CaseAsString(int kase)
{
  TString ts;
  for (int i=0;SteppingCasesI[i];i++)
  {
     if (!(kase&SteppingCasesI[i])) continue;
     if (ts.Length()) ts +="&";
     ts += SteppingCasesC[i]; 
  }
  return ts;
}
//_____________________________________________________________________________
TString StMCStepping::KazeAsString(int kase)
{
  TString ts;
  for (int i=0;SteppingKazesI[i];i++)
  {
     if (!(kase&SteppingKazesI[i])) continue;
     if (ts.Length()) ts +="&";
     ts += SteppingKazesC[i]; 
  }
  return ts;
}
		
		
//_____________________________________________________________________________
void StMCStepping::Case()
{
static int nCall = 0; nCall++;
  fSteps++;
  fPrevNode = fNode;
  fNode = gGeoManager->GetCurrentNode();
  fPrevVolume = fVolume;
  fVolume = fNode->GetVolume();
  fPrevMedium =fMedium;
  fMedium = fVolume->GetMedium();
  fPrevMaterial = fMaterial;
  fMaterial = fMedium->GetMaterial();
  fX0 = fMaterial->GetRadLen();
  myMC = gMC;
  fCase = 0;
  if(myMC->IsNewTrack 		 ()) fCase |= kNewTrack;
//if(myMC->TrackLength() == 0      ) fCase |= kNewTrack;
//if(myMC->IsTrackAlive 	 ()) fCase |= kTrackAlive;
  if(myMC->IsTrackDisappeared 	 ()) fCase |= kTrackDisappeared;
  if(myMC->IsTrackEntering 	 ()) fCase |= kTrackEntering;
  if(myMC->IsTrackExiting 	 ()) fCase |= kTrackExiting;
  if(myMC->IsTrackInside         ()) fCase |= kTrackInside;
  if(myMC->IsTrackOut            ()) fCase |= kTrackOut;
  if(myMC->IsTrackStop           ()) fCase |= kTrackStop;

  fKaze=0;
  if(!fKaze && fCase&kNewTrack) 	fKaze = kNEWtrack;
  if(!fKaze && fCase&kTrackEntering) 	fKaze = kENTERtrack;
  if(!fKaze && fCase&kTrackInside) 	fKaze = kCONTINUEtrack;
  if(          fCase&kTrackExiting) 	fKaze = kEXITtrack;
  if(          fCase&kTrackStop) 	fKaze = kENDEDtrack;
  if(          fCase&kTrackDisappeared) fKaze = kENDEDtrack;
  if(          fCase&kTrackOut) 	fKaze = kOUTtrack;
  int kaze = fKaze;
//  if(fKazePrev==fKaze && fKaze !=kCONTINUEtrack) fKaze= kIgnore;
  fKazePrev=kaze;
  fPrevPosition = fCurrentPosition;
  myMC->TrackPosition(fCurrentPosition);
  fPrevMomentum = fCurrentMomentum;
  myMC->TrackMomentum(fCurrentMomentum);
  fPrevLength   = fCurrentLength;
  fCurrentLength = myMC->TrackLength();
  assert(fCurrentMomentum[3]>1e-6);
  fCharge = myMC->TrackCharge();
  fMass   = myMC->TrackMass();
  fEtot   = myMC->Etot();
 
  switch (fKaze) {

    case kNEWtrack:
      fSteps=0;
      fTrackNumber++; 
      fStartPosition = fCurrentPosition;
      fStartMomentum = fCurrentMomentum;
    case kENTERtrack:;
      {
      fEnterPosition = fCurrentPosition;
      fEnterMomentum = fCurrentMomentum;
      fEnterLength = fCurrentLength;
      fEdep   = 0;
      break;}

    case kCONTINUEtrack:;
    case kEXITtrack:;
    case kOUTtrack:
      fEdep   = myMC->Edep();
      fEtot   = myMC->Etot();
      RecovEloss();
      myMC->TrackPosition(fCurrentPosition);
      myMC->TrackMomentum(fCurrentMomentum);
    break;

    case kENDEDtrack:
      break;

    case kIgnore:;
    assert(0 && "Ignore case??");
    break;

    default:
     Error("Case","Unexpected case %d == %s",fKaze,fCasName.Data());
     assert(0);
  }
}		
//_____________________________________________________________________________
int StMCStepping::Fun()
{
  Case();
  switch (fCase) {
    case kNewTrack|kTrackEntering:;
    case kNewTrack:;
      fPrevLength =0;
    case kTrackEntering:;
      printf("\n\nStepping %s\n",fCasName.Data());
      printf("Vol= %s Mat=%s Med=%s\n"
             ,fVolume->GetName()
             ,fMaterial->GetName()
             ,fMedium->GetName());
//       printf("Track %s\t Mass(%d) = %g Pos= %g %g %g Mom=%g %g %g\n"
//              ,fParName.Data(),fPDG,fMass
//              ,fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2]
//              ,fCurrentMomentum[0],fCurrentMomentum[1],fCurrentMomentum[2]);

      break;

    case kTrackInside|kTrackDisappeared:
    case kTrackInside|kTrackStop:	
    case kTrackDisappeared: 		
    case kTrackExiting:			
    case kTrackInside:			
    case kTrackOut:			
    case kTrackStop: 			
    case kTrackDisappeared|kTrackOut:   
      printf("Continue %s\n",fCasName.Data());
      printf("Track dLen= %g Pos= %g %g %g Mom=%g %g %g\n"
             ,fCurrentLength-fEnterLength
             ,fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2]
             ,fCurrentMomentum[0],fCurrentMomentum[1],fCurrentMomentum[2]);
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     assert(0);
  }
  return 0;
}		
#if 1
//_____________________________________________________________________________
void StMCStepping::RecovEloss()
{
// 	Update directly Geant3 common when we moving bacward the track
//	and energy loss is negative


static int nCall = 0; nCall++;
  enum {kX=0,kY,kZ,kDx,kDy,kDz,kP};

static Gctrak_t *gGctrak=((TGeant3*)TVirtualMC::GetMC())->Gctrak();
static float *vect = gGctrak->vect;
static Float_t &getot = gGctrak->getot;
static Float_t &gekin = gGctrak->gekin;


  do {
//    if (fEdep<kStMCSMinEabs) 				return;
//    if (fEdep<kStMCSMinEref*fCurrentMomentum.E()) 	return;
    

    double dL = fCurrentLength-fPrevLength;
    assert(dL>=0);
    if (dL<kStMCSMinDist) 				return;

    double dE = (fDir)? -fEdep:fEdep*2;
    if (fDir) 		break; 		// if (Stv direction == Geant direction ) no need to update energy

    getot += dE; fEtot = getot;		// update energy
    gekin += dE;
    vect[kP] = sqrt(gekin*(getot+fMass));

  } while(0);
  myMC->TrackPosition(fCurrentPosition);
  myMC->TrackMomentum(fCurrentMomentum);
}

#endif
