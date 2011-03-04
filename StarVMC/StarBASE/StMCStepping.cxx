// $Id: StMCStepping.cxx,v 1.4 2011/03/04 19:30:17 jwebb Exp $
//
//
// Class StMCStepping
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "StMCStepping.h"
#include "TVirtualMC.h"

#include "TGeoManager.h"
#include "TGeoNavigator.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"

#include "TFile.h"
#include "TMath.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TList.h"

#include <iostream>


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
  // This is a really sick twisted way to initialize variables
  // in this class to zero... need to make this explicit.
  int n = (char*)&fCase - (char*)&fEnterLength + sizeof(fCase);
  memset(&fEnterLength,0,n);	
  BookHistograms();

}   

StMCStepping::~StMCStepping()
{
  /* nada */
}


void StMCStepping::bookVolume( const Char_t *name )
{
  // books all volumes beneath this top volume


  {

      TGeoVolume *volume = gGeoManager->FindVolumeFast( name );
      assert(volume);
      TIter next( volume->GetNodes() );
      
      TGeoNode *node = 0;
      while ( (node = (TGeoNode*)next()) )
	{
	  
	  TGeoVolume   *volume = node->GetVolume();
	  const Char_t *vname  = volume->GetName();
	  Int_t         vid    = volume->GetNumber();
	  
	  if ( ! hRadlenHist1D[vid] )                                    // check for existing histogram
	    {
	      //	      std::cout << "+ add volume " << vid << " " << name << std::endl;	  

	      hRadlenHist1D[ vid ] = new TH1D( Form("h_radlen_%s_eta",vname), Form("Depth vs eta [%s];#eta;L/#chi_{0}",vname), 500,-5.0,+5.0 );
	      hCountsHist1D[ vid ] = new TH1D( Form("h_counts_%s_eta",vname), Form("Number of geantinos vs eta [%s];#eta",vname), 500,-5.0,+5.0 );

	      hRadlenHist1D_z[ vid ] = new TH1D( Form("h_radlen_%s_z",vname), Form("Depth vs z [%s];z [cm];L/#chi_{0}",vname), 500,-500.0,+500.0);
	      hCountsHist1D_z[ vid ] = new TH1D( Form("h_counts_%s_z",vname), Form("Number of geantinos vs z [%s];z",vname), 500,-500.0,+500.0);

	      hRadlenHist1D_r[ vid ] = new TH1D( Form("h_radlen_%s_r",vname), Form("Depth vs r [%s];r [cm];L/#chi_{0}",vname), 500,0.0,+500.0);
	      hCountsHist1D_r[ vid ] = new TH1D( Form("h_counts_%s_r",vname), Form("Number of geantinos vs r [%s];r",vname), 500,0.0,+500.0);

	      hRadlenHist1D_phi[ vid ] = new TH1D( Form("h_radlen_%s_phi",vname), Form("Depth vs phi [%s];phi [deg];L/#chi_{0}",vname), 360.0,-180.,180.0);
	      hCountsHist1D_phi[ vid ] = new TH1D( Form("h_counts_%s_phi",vname), Form("Number of geantinos vs phi [%s];r",vname), 360.0,-180.,180.0);

	      hRadlenAccu1D[ vid ] = new TH1D( Form("h_acc_radlen_%s_eta",vname), Form("Accumulated depth vs eta [%s];#eta;L/#chi_{0}",vname), 500,-5.0,+5.0 );

	      // This will be fraking huge	      hRadlenHist2D[ vid ] = new TH2F( Form("h_radlen_%s_eta2",vname), Form("Radiation length vs eta [%s];#phi;#eta",vname), 250,0.,30.0,500,-5.0,+5.0 );    // 1GB
	      // This will be fraking huge	      hCountsHist2D[ vid ] = new TH2F( Form("h_counts_%s_eta2",vname), Form("Number of geantinos vs eta [%s];#phi;#eta",vname), 250,0.,30.0,500,-5.0,+5.0 ); // 1GB
	      bookVolume( vname ); // Recurse down the volume tree	  
	    }	  
	}
    }

}
 

void
StMCStepping::BookHistograms()
{


  //$$  TList *materials = gGeoManager -> GetListOfMaterials();
  //$$  Int_t numberOfMaterials = materials -> GetEntries();

  TObjArray *volumes    = gGeoManager->GetListOfVolumes();
  Int_t numberOfVolumes = volumes -> GetEntries();
  mNumberOfVolumes = numberOfVolumes;

  ////////////////////////////////////////////////////////////////////////////////////
  // Count the number of non duplicated volumes
  Int_t nmax=0;
  for ( Int_t i=0;i<numberOfVolumes;i++ )
    {
      TGeoVolume *volume = (TGeoVolume*)volumes->At(i);
      if (!volume)  continue;
      nmax=volume->GetNumber();
    }
  
  ////////////////////////////////////////////////////////////////////////////////////
  // Create space for histograms, accumulators per volume Id  
  mNumberOfVolumes=nmax;
  for ( Int_t i=0;i<mNumberOfVolumes+1;i++ )
    {
      hRadlenHist1D.push_back( NULL ); 
      hCountsHist1D.push_back( NULL );

      hRadlenHist1D_z.push_back( NULL ); 
      hCountsHist1D_z.push_back( NULL );

      hRadlenHist1D_r.push_back( NULL ); 
      hCountsHist1D_r.push_back( NULL );

      hRadlenHist1D_phi.push_back( NULL ); 
      hCountsHist1D_phi.push_back( NULL );

      hRadlenAccu1D.push_back( NULL );
      //
      mRadlenSum   .push_back( 0.   );      
      mRadlenEnter .push_back( 0.   );
      mHasEntered  .push_back( false );
    }


  ////////////////////////////////////////////////////////////////////////////////////
  //
  TList *mediums = gGeoManager->GetListOfMedia();
  Int_t numberOfMediums = mediums->GetEntries();
  std::cout << "Number of mediums = " << numberOfMediums << std::endl;

  h_ncount_rz    = new TH2F("h_ncount_rz",    "Number of geantinos in histo bin; z [cm]; r [cm]", 1600,-800.,800., 800, -400.0, +400.0 );
  h_radlen_rz    = new TH2F("h_radlen_rz",    "radiation length [g/cm^{2}];      z [cm]; r [cm]", 1600,-800.,800., 800, -400.0, +400.0 );
  h_abslen_rz    = new TH2F("h_abslen_rz",    "absorption length [g/cm^{2}];     z [cm]; r [cm]", 1600,-800.,800., 800, -400.0, +400.0 );
  h_pathlen_rz   = new TH2F("h_pathlen_rz",   "path length through volume [cm];  z [cm]; r [cm]", 1600,-800.,800., 800, -400.0, +400.0 );
  h_distance_rz  = new TH2F("h_distance_rz",  "distance from orgin [cm];         z [cm]; r [cm]", 1600,-800.,800., 800, -400.0, +400.0 );

}

void StMCStepping::postTrack()
{

  //
  // This is the eta, phi, x, y, z and r upon exiting the geometry
  //
  Float_t eta = fCurrentPosition.Vect().Eta();
  Float_t phi = fCurrentPosition.Vect().Phi(); // phi returned in degrees
  Float_t zzz = fCurrentPosition[2];
  Float_t xxx = fCurrentPosition[0];
  Float_t yyy = fCurrentPosition[1];
  Float_t rrr = TMath::Sqrt(xxx*xxx+yyy*yyy);

  Float_t cos_theta = TMath::Abs(fCurrentPosition.Vect().CosTheta());
  Float_t sin_theta = TMath::Sqrt(1.0 - cos_theta*cos_theta);

  phi *= 180.0 / TMath::Pi();

  for ( Int_t i=0;i<mNumberOfVolumes+1;i++ )
    {
      if ( hRadlenHist1D[i] ) 
	{
	  hRadlenHist1D[ i ] -> Fill( eta, mRadlenSum[ i ] );
	  hCountsHist1D[ i ] -> Fill( eta, 1.0 );

	  //	  hRadlenHist1D_z[ i ] -> Fill( zzz, mRadlenSum[ i ] * cos_theta );
	  //	  hCountsHist1D_z[ i ] -> Fill( zzz, 1.0 );

	  //	  hRadlenHist1D_r[ i ] -> Fill( rrr, mRadlenSum[ i ] * sin_theta );
	  //	  hCountsHist1D_r[ i ] -> Fill( rrr, 1.0 );

	  //	  hRadlenHist2D[ i ] -> Fill( phi, eta, mRadlenSum[ i ] );
	  //	  hCountsHist2D[ i ] -> Fill( phi, eta );
	}
    }

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

  fNode      = gGeoManager->GetCurrentNode();
  fNavigator = gGeoManager->GetCurrentNavigator();
  fVolume    = fNode->GetVolume();
  fMedium    = fVolume->GetMedium();
  fMaterial  = fMedium->GetMaterial();
  fCase      = 0;

  /*
  TString VOLUME = fVolume->GetName();
  Bool_t  FEEA   = VOLUME=="FEEA";       // in FEE assembly
  Bool_t  TBRW   = VOLUME=="TBRW";       // in big aluminium brick
  */

  if(gMC->TrackLength() == 0      ) fCase |= kNewTrack;
  if(gMC->IsTrackDisappeared 	 ()) fCase |= kTrackDisappeared;
  if(gMC->IsTrackEntering 	 ()) fCase |= kTrackEntering;
  if(gMC->IsTrackExiting 	 ()) fCase |= kTrackExiting;
  if(gMC->IsTrackInside         ()) fCase |= kTrackInside;
  if(gMC->IsTrackOut            ()) fCase |= kTrackOut;
  if(gMC->IsTrackStop           ()) fCase |= kTrackStop;

#define TRACK_NEW      fCase&(kNewTrack)
#define TRACK_ENTER    fCase&(kTrackEntering)
#define TRACK_CONTINUE fCase&(kTrackInside)
#define TRACK_EXIT     fCase&(kTrackExiting)
#define TRACK_ENDED    fCase&(kTrackStop|kTrackDisappeared)
#define TRACK_OUT      fCase&(kTrackOut)
 
  switch (fCase) 
    {
    case kNewTrack:
    case kTrackEntering|kNewTrack:;
      fTrackNumber++;
      gMC->TrackPosition(fStartPosition);
    case kTrackEntering:
      {
	gMC->TrackPosition(fEnterPosition);
	fCurrentPosition = fEnterPosition;
	gMC->TrackMomentum(fEnterMomentum);
	fCurrentMomentum = fEnterMomentum;
	assert(fCurrentMomentum[3]>1e-6);
	fEnterLength     = gMC->TrackLength();
	fCurrentLength   = fEnterLength;
	fEdep   = 0;
	fEtot   = gMC->Etot();
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
      gMC->TrackPosition(fCurrentPosition);
      gMC->TrackMomentum(fCurrentMomentum);
      assert(fCurrentMomentum[3]>1e-6);
      fCurrentLength     = gMC->TrackLength();
      fEdep   = gMC->Edep();
      fEtot   = gMC->Etot();
      break;
    default:
      assert(0);// We were passed an undefined case
    }
}		
//_____________________________________________________________________________
int StMCStepping::Fun()
{

  Case();  
  Double_t radlen  = fMaterial->GetRadLen();
  Double_t abslen  = fMaterial->GetIntLen();
  Double_t density = fMaterial->GetDensity();
  radlen*=density;  // convert cm --> g/cm^2
  abslen*=density;  // convert cm --> g/cm^2
  
  // Histograms always store entries at the position where the
  // track enters the volume
  Float_t x = fEnterPosition[0];
  Float_t y = fEnterPosition[1];
  Float_t z = fEnterPosition[2];

  Float_t xx = fCurrentPosition[0];
  Float_t yy = fCurrentPosition[1];
  Float_t zz = fCurrentPosition[2];


  TVector3 direction(xx,yy,zz);
  Float_t eta = -999.0;
  Float_t phi = -999.0;
  if ( direction.Mag() > 0.0 )
    {
      eta = direction.Eta();
      phi = direction.Phi();
    }
 
  Float_t r = TMath::Sqrt( x*x + y*y );
  Float_t rr = TMath::Sqrt( xx*xx + yy*yy );

  Float_t dist = 0.5 * ( TMath::Sqrt(r*r + z*z) + TMath::Sqrt(rr*rr + zz*zz) );

  // Path length only makes sense when track exits a volume.
  Float_t pathlen     = fCurrentLength - fEnterLength;  
  TString volume      = gMC->CurrentVolName();

  
  if ( TRACK_NEW )
    {
      /// Clear from previous event
      for ( Int_t i=0;i<mNumberOfVolumes+1;i++ )
	{
	  mRadlenSum[ i ] = 0.0;
	  mRadlenEnter[ i ] = 0.0;
	  mHasEntered[ i ] = false;
	}

      mRadlenAcc = 0.0;
	
    }

  if ( TRACK_EXIT ) 
    // Track is exiting this volume
    {

      Int_t level = fNavigator->GetLevel();
      Int_t volu_numbers[ level+1 ];
      Int_t copy_numbers[ level+1 ];
      fNavigator->GetBranchNumbers( copy_numbers, volu_numbers );      
      
      // On exit, accumulate radiation length sums
      if ( radlen > 0. ) 
	{
	  Double_t nradlen = density * pathlen / radlen;

	  /////////////////////// <<<<<<<<<<<<<<<<<<< level or level+1 ??
	  //$$$	  for ( Int_t i=0;i<level+1;i++ )     // Loop over volume numbers in this branch/path

	  // Number of radlen in front of current volume
	  mRadlenAcc += nradlen;

	  for ( Int_t i=0;i<level+1;i++ )
	    {
	      Int_t id = volu_numbers[i];
	      mRadlenSum[ id ] += nradlen;

	      Float_t total = mRadlenAcc - mRadlenEnter[id];
	      hRadlenHist1D_z[ id ] -> Fill( z, total );
	      hRadlenHist1D_r[ id ] -> Fill( r, total );
	      hRadlenHist1D_phi[ id ] -> Fill( phi*TMath::RadToDeg(), total );
	      
	      hCountsHist1D_z[ id ] -> Fill( z, 1.0   );
	      hCountsHist1D_r[ id ] -> Fill( r, 1.0   );
	      hCountsHist1D_phi[ id ] -> Fill( phi*TMath::RadToDeg(), 1.0 );

	    }

	  
	}

    }

  if ( TRACK_ENTER )
    {

      // Track is entering this volume.
      Int_t level = fNavigator->GetLevel();
      Int_t volu_numbers[ level+1 ];
      Int_t copy_numbers[ level+1 ];
      fNavigator->GetBranchNumbers( copy_numbers, volu_numbers );      

      // Radiation length on entrance
      mRadlenEnter[ volu_numbers[ level ] ] = mRadlenAcc;

      for ( Int_t i=0;i<level; i++ )
	{
	  Int_t id = volu_numbers[i];	  
	  if ( ! mHasEntered[id] ) 
	    {
	      hRadlenAccu1D[ id ] -> Fill( eta, mRadlenAcc );
	      mHasEntered[ id ] = true;
	    }
	}
      
    }









  

  switch (fCase) 
    {
    case kNewTrack:
    case kNewTrack|kTrackEntering:;
    case kTrackEntering:;
      if ( y<0 ) r=-r;
      h_ncount_rz -> Fill( z, r, 1.0 );
      h_radlen_rz -> Fill( z, r, radlen );
      h_abslen_rz -> Fill( z, r, abslen );
      break;      
    case kTrackInside|kTrackDisappeared:
    case kTrackInside|kTrackStop:	
    case kTrackDisappeared: 		
    case kTrackExiting:			
    case kTrackInside:			
    case kTrackOut:			
    case kTrackStop: 			
    case kTrackDisappeared|kTrackOut:   
      // On exit, fill the path length in this volume
      if ( y<0 ) r=-r;
      h_pathlen_rz  -> Fill(z, r, pathlen );
      h_distance_rz -> Fill(z, r, dist    );
      break;      
    default:
      Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
      assert(0);
    }
  return 0;

}		
