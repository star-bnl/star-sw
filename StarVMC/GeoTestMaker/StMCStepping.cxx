// $Id: StMCStepping.cxx,v 1.2 2009/06/07 02:28:36 perev Exp $
//
//
// Class StMCStepping
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCStepping.h"
#include "TPDGCode.h"
#include "TVirtualMC.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"


TVirtualMC *myMC=0;

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
  int n = (char*)&fCase - (char*)&fEnterLength + sizeof(fCase);
  memset(&fEnterLength,0,n);	
  fKazePrev = -1;
  myMC = 0;
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
  fNode = gGeoManager->GetCurrentNode();
  fVolume = fNode->GetVolume();
  fMedium = fVolume->GetMedium();
  fMaterial = fMedium->GetMaterial();
  fX0 = fMaterial->GetRadLen();
  myMC = gMC;
  fCase = 0;
//if(myMC->IsNewTrack 		 ()) fCase |= kNewTrack;
  if(myMC->TrackLength() == 0      ) fCase |= kNewTrack;
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
  if(fKazePrev==fKaze && fKaze !=kCONTINUEtrack) fKaze= kIgnore;
  fKazePrev=kaze;
  fCasName = CaseAsString(fCase);
  fKazName = KazeAsString(fKaze);

 
  switch (fCase) {

    case kNewTrack:
    case kTrackEntering|kNewTrack:;
      fTrackNumber++;
      myMC->TrackPosition(fStartPosition);
    case kTrackEntering:
      {
      myMC->TrackPosition(fEnterPosition);
      fCurrentPosition = fEnterPosition;
      myMC->TrackMomentum(fEnterMomentum);
      fCurrentMomentum = fEnterMomentum;
      assert(fCurrentMomentum[3]>1e-6);
      fEnterLength     = myMC->TrackLength();
      fCurrentLength   = fEnterLength;
//      fCharge = myMC->TrackCharge();
//      fMass   = myMC->TrackMass();
      fEdep   = 0;
      fEtot   = myMC->Etot();
      }
      break;

    case kTrackInside|kTrackDisappeared:
    case kTrackInside|kTrackStop:
    case kTrackDisappeared:
    case kTrackExiting:
    case kTrackInside:
    case kTrackOut:
    case kTrackStop:
    case kTrackDisappeared|kTrackOut:
      myMC->TrackPosition(fCurrentPosition);
      myMC->TrackMomentum(fCurrentMomentum);
      assert(fCurrentMomentum[3]>1e-6);
      fCurrentLength     = myMC->TrackLength();
      fEdep   = myMC->Edep();
      fEtot   = myMC->Etot();
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
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
