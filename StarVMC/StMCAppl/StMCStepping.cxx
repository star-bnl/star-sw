// $Id: StMCStepping.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCStepping
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCStepping.h"
#include "StTGeant3.h"
#include "TPDGCode.h"
#ifdef  STARMC_H
   StarMC     *myMC=0;
   const int gMCVers = 0;
#endif
#ifndef STARMC_H
   TVirtualMC *myMC=0;
   const int gMCVers = 1;
#endif

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
#ifdef STARMC_H
   myMC = StarMC::GetMC();
#endif
#ifndef STARMC_H
   myMC = TVirtualMC::GetMC();
#endif
  

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
  char cbuf[100];
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

  StTGeant3 *g3 = (StTGeant3*)myMC;
 
  switch (fCase) {

    case kNewTrack:
    case kTrackEntering|kNewTrack:;
      fTrackNumber++;
      myMC->TrackPosition(fStartPosition);
    case kTrackEntering:
      {
      fVolName = myMC->CurrentVolName();
      myMC->TrackPosition(fEnterPosition);
      fCurrentPosition = fEnterPosition;
      myMC->TrackMomentum(fEnterMomentum);
      fCurrentMomentum = fEnterMomentum;
      fEnterLength     = myMC->TrackLength();
      fCurrentLength   = fEnterLength;
//      fCharge = myMC->TrackCharge();
//      fMass   = myMC->TrackMass();
      fPid    = myMC->TrackPid();
      fPDG    = fPid;
      fPid=myMC->IdFromPDG(fPDG);
      memset(cbuf,'@',40);cbuf[40]=0;
      g3->Gfpart(fPDG, cbuf, fTrType, fMass, fCharge, fLife);
      char *e = strchr(cbuf,'@'); if(e) *e=0;
      for(;e&&e[-1]==' ';e--) {e[-1]=0;}
      fParName = cbuf;
      fEdep   = 0;
      fEtot   = myMC->Etot();
      fMedId  = myMC->GetMedium();
      g3->Gftmed(
        fMedId, fMedi.name, fMedi.nmat, fMedi.isvol,  
	fMedi.ifield, fMedi.fieldm, fMedi.tmaxfd, 
	fMedi.stemax, fMedi.deemax, fMedi.epsil, 
	fMedi.stmin , fMedi.pars  ,&fMedi.npars);
      fMedName=fMedi.name;

      fMatId  = fMedi.nmat;
      myMC->Gfmate(fMatId, fMate.name, fMate.a, fMate.z, fMate.dens, fMate.radl, fMate.absl, fMate.pars,fMate.npars);
      fMatName=fMate.name;
    
      fVolId  = myMC->CurrentVolID(fVolNb);}
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
      fCurrentLength     = myMC->TrackLength();
      fEdep   = myMC->Edep();
      fEtot   = myMC->Etot();
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     Assert(0);
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
      printf("Vol= %s.%d.%d Mat=%s.%d Med=%s.%d.%d\n"
             ,fVolName.Data(),fVolId,fVolNb
             ,fMatName.Data(),fMatId
             ,fMedName.Data(),fMedId,fMedi.nmat);
      printf("Track %s\t Mass(%d) = %g Pos= %g %g %g Mom=%g %g %g\n"
             ,fParName.Data(),fPDG,fMass
             ,fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2]
             ,fCurrentMomentum[0],fCurrentMomentum[1],fCurrentMomentum[2]);

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
     Assert(0);
  }
  return 0;
}		
//_____________________________________________________________________________
TString StMCStepping::GetMateName(int mat)	
{
  MyMate m;	
  myMC->Gfmate(mat, m.name, m.a,m.z,m.dens,m.radl,m.absl,m.pars,m.npars);
  return TString(m.name);
} 	
//_____________________________________________________________________________
TString StMCStepping::GetMediName(int med)	
{
  MyMedi m;	
  StTGeant3 *g3 = (StTGeant3*)myMC;
  g3->Gftmed(med     , m.name  , m.nmat, m.isvol,  
	     m.ifield, m.fieldm, m.tmaxfd, 
	     m.stemax, m.deemax, m.epsil, 
	     m.stmin , m.pars  ,&m.npars);
  return TString(m.name);
} 	
//_____________________________________________________________________________
TString StMCStepping::GetVoluName(int vol)	
{
  return TString(myMC->VolName(vol));
} 	
	
	
	
	
	
	
	
	
	
	
	
	
