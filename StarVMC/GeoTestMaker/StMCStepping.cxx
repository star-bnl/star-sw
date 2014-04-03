// $Id: StMCStepping.cxx,v 1.8 2014/04/03 21:55:55 perev Exp $
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

static Gctrak_t *gGctrak=0;

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




//_____________________________________________________________________________
StMCStepping::StMCStepping(const char *name,const char *tit)
  : GCall(name,tit)
{
  memset(fBeg,0,fEnd-fBeg+1);	
  fKazePrev = -1;
  myMC = 0;
  fDir = 1;
}   
//_____________________________________________________________________________
void StMCStepping::Print(const Option_t*) const
{
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

  if(!gGctrak) gGctrak = ((TGeant3*)TVirtualMC::GetMC())->Gctrak();
  fNode = gGeoManager->GetCurrentNode();
  fVolume = fNode->GetVolume();
  fMedium = fVolume->GetMedium();
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
//vp  fCasName = CaseAsString(fCase);
//vp  fKazName = KazeAsString(fKaze);

  myMC->TrackPosition(fCurrentPosition);
  myMC->TrackMomentum(fCurrentMomentum);
  fCurrentLength = myMC->TrackLength();
  fCharge = myMC->TrackCharge();
  fMass   = myMC->TrackMass();
  fEtot   = myMC->Etot();
 
  switch (fKaze) {

    case kNEWtrack:
      fTrackNumber++;
      myMC->TrackPosition(fStartPosition);
    case kENTERtrack:;
      {
      fSteps=0;
      fEnterPosition = fCurrentPosition;
      myMC->TrackMomentum(fEnterMomentum);
      fEnterMomentum = fCurrentMomentum;
      assert(fCurrentMomentum[3]>1e-6);
      fEnterLength = fCurrentLength;
      fEdep   = 0;
      fLastVect[6] = fCurrentMomentum.P();
      for (int i=0;i<3;i++) {
        fLastVect[i+0] = fCurrentPosition[i];
        fLastVect[i+3] = fCurrentMomentum[i]/fLastVect[6];}
      }
      break;

    case kCONTINUEtrack:;
    case kEXITtrack:;
    case kENDEDtrack:;
    case kOUTtrack:
      fEdep   = myMC->Edep();
      fEtot   = myMC->Etot();
      RecovEloss();
      myMC->TrackPosition(fCurrentPosition);
      myMC->TrackMomentum(fCurrentMomentum);
    break;

    case kIgnore:;
    break;

    default:
     Error("Case","Unexpected case %x == %s",fKaze,fCasName.Data());
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

//_____________________________________________________________________________
void StMCStepping::RecovEloss()
{
// 	Update directly Geant3 common when we moving bacward the track
//	and energy loss is negative
  enum {kX=0,kY,kZ,kDx,kDy,kDz,kP};
  float *vout = gGctrak->vout;
  do {
    if (fEdep<=0) 	break;
    double dL = fCurrentLength-fLastLength;
    if (dL<1e-5) 	break;
    double cL0 = sqrt((1.-fLastVect[kDz])*(1+fLastVect[kDz]));		//cos(Lambda0)
    double cL1 = sqrt((1.-     vout[kDz])*(1+     vout[kDz]));		//cos(Lambda1)
    double ang = ((vout[kDy]-fLastVect[kDy])*fLastVect[kDx]
               -  (vout[kDx]-fLastVect[kDx])*fLastVect[kDy])/(cL0*cL1);	//sin(dAng)
    ang = (fabs(ang)<0.1)? ang*(1.-ang*ang/6)  :  asin(ang);		//dAng
    double dLxy = dL*cL1;
    double Rho = ang/dLxy;
    double dE = (fDir)? -fEdep:fEdep*2;
    double dP = dE*fEtot/vout[kP];
    double dRho = -Rho*dP/vout[kP];
    double dPhi = dRho*dLxy/2;
    double dOrt = dPhi*dLxy/3;
    vout[kX] += -vout[kDy]/cL1*dOrt;
    vout[kY] +=  vout[kDx]/cL1*dOrt;
    float vDx = vout[kDx];
    vout[kDx] += -vout[kDy]*dPhi;
    vout[kDy] +=       vDx *dPhi;
    if (fDir) 		break;
    fEtot += dE;
    gGctrak->getot  = fEtot;
    gGctrak->gekin += dE;
    vout[kP] = sqrt(gGctrak->gekin*(fEtot+fMass));
    ((TGeant3*)gMC)->Gekbin();
  } while(0);
  fLastLength = fCurrentLength;
  memcpy(fLastVect,vout,sizeof(fLastVect));
}  
