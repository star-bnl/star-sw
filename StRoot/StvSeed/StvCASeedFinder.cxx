#if 0	//Temporary OFF
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <vector>
#include <complex>
#include "TCernLib.h"
#include "StvCASeedFinder.h"
#include "StMultiKeyMap.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

#include "StEvent/StEnumerations.h"
#include "StEvent/StTpcHit.h"

#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"

#include "Stv/StvHit.h"
#include "THelixTrack.h"
//#define APPROX_DEBUG
#include "StarMagField/StarMagField.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvDraw.h"
#include "Stv/StvHit.h"

#define ENABLE_VECTORIZATION
#include "TPCCATracker/code/AliHLTTPCCAGBHit.h"
#include "TPCCATracker/code/AliHLTTPCCAGBTrack.h"
#include "TPCCATracker/code/AliHLTTPCCAParam.h"
#include "TPCCATracker/code/AliHLTTPCCAGBTracker.h"

// 
// static double innerR[13] = {60.000,  64.800,  69.600,  74.400,  79.200, //  5
// 			       84.000,  88.800,  93.600,  98.800, 104.000, // 10
// 			      109.200, 114.400, 119.600};
// static double outerR[32] = {127.195, 129.195, // 15
// 			    131.195, 133.195, 135.195, 137.195, 139.195, // 20
// 			    141.195, 143.195, 145.195, 147.195, 149.195, // 25
// 			    151.195, 153.195, 155.195, 157.195, 159.195, // 30
// 			    161.195, 163.195, 165.195, 167.195, 169.195, // 35
// 			    171.195, 173.195, 175.195, 177.195, 179.195, // 40
// 			    181.195, 183.195, 185.195, 187.195, 189.195};// 45

static const int NumberOfPadsAtRow[45] = {
    88, 96,104,112,118,126,134,142,150,158, // Inner
   166,174,182,
                98,100,102,104,106,106,108, // Outer
   110,112,112,114,116,118,120,122,122,124,
   126,128,128,130,132,134,136,138,138,140,
   142,144,144,144,144
};
static  const int kNSlices = 24; //TODO initialize from StRoot
static  const int kNRows = 45;

static  float rRows[kNRows] = 
                         {60.000,  64.800,  69.600,  74.400,  79.200, //  5  //TODO initialize from StRoot
			  84.000,  88.800,  93.600,  98.800, 104.000, // 10
			  109.200, 114.400, 119.600, 
			  127.195, 129.195, // 15
			  131.195, 133.195, 135.195, 137.195, 139.195, // 20
			  141.195, 143.195, 145.195, 147.195, 149.195, // 25
			  151.195, 153.195, 155.195, 157.195, 159.195, // 30
			  161.195, 163.195, 165.195, 167.195, 169.195, // 35
			  171.195, 173.195, 175.195, 177.195, 179.195, // 40
			  181.195, 183.195, 185.195, 187.195, 189.195};// 45

typedef std::complex<double> myComplex;
static const double kToRad = M_PI/180;
static const myComplex kIm = myComplex(0,1);

static const myComplex sectAng[24] = {
exp(-kIm*kToRad* 60.),exp(-kIm*kToRad* 30.),exp(-kIm*kToRad*  0.),
exp( kIm*kToRad* 30.),exp( kIm*kToRad* 60.),exp( kIm*kToRad* 90.),
exp( kIm*kToRad*120.),exp( kIm*kToRad*150.),exp(-kIm*kToRad*180.),
exp(-kIm*kToRad*150.),exp(-kIm*kToRad*120.),exp(-kIm*kToRad* 90.),

exp(-kIm*kToRad*120.),exp(-kIm*kToRad*150.),exp(-kIm*kToRad*180.),
exp( kIm*kToRad*150.),exp( kIm*kToRad*120.),exp( kIm*kToRad* 90.),
exp( kIm*kToRad* 60.),exp( kIm*kToRad* 30.),exp( kIm*kToRad*  0.),
exp(-kIm*kToRad* 30.),exp(-kIm*kToRad* 60.),exp(-kIm*kToRad* 90.)};



class AliHLTTPCCAParamVector: public std::vector<AliHLTTPCCAParam>{};
class AliHLTTPCCAGBHitVector :public std::vector<AliHLTTPCCAGBHit>{};
class IdTruthVector          :public std::vector<int>             {};


ClassImp(StvCASeedFinder)
//_____________________________________________________________________________
StvCASeedFinder::StvCASeedFinder(const char *name):StvSeedFinder(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
  fTracker  = new AliHLTTPCCAGBTracker;
  fCaParam  = new AliHLTTPCCAParamVector;
  fCaHits   = new AliHLTTPCCAGBHitVector;
  fIdTruth  = new IdTruthVector;	
  Clear();
}  
//_____________________________________________________________________________
void StvCASeedFinder::Clear(const char*)
{
  memset(mBeg,0,mMed-mBeg+1);
  fTracker->StartEvent();
  fCaParam->clear();	// settings for all sectors to give CATracker
  fCaHits->clear(); 	// hits to give CATracker
  fIdTruth->clear(); 	// id of the Track, which has created CaHit
}
//_____________________________________________________________________________
void StvCASeedFinder::Reset()
{
  memset(mBeg,0,mMed-mBeg+1);
}    
//_____________________________________________________________________________
void StvCASeedFinder::MakeSettings()
{
  for ( int iSlice = 0; iSlice < kNSlices; iSlice++ ) {
    AliHLTTPCCAParam SlicePar;
    memset(&SlicePar, 0, sizeof(AliHLTTPCCAParam));

    int sector = iSlice+1;
      // int sector = iSlice;
    SlicePar.SetISlice( iSlice );
    SlicePar.SetNRows ( kNRows ); 
    double beta = 0;
    if (sector > 12) beta = (24-sector)*2.*TMath::Pi()/12.;
    else             beta =     sector *2.*TMath::Pi()/12.;
    SlicePar.SetAlpha  ( beta );
    SlicePar.SetDAlpha  ( 30*TMath::DegToRad() );			//TODO initialize from StRoot
    SlicePar.SetCosAlpha ( TMath::Cos(SlicePar.Alpha()) );
    SlicePar.SetSinAlpha ( TMath::Sin(SlicePar.Alpha()) );
    SlicePar.SetAngleMin ( SlicePar.Alpha() - 0.5*SlicePar.DAlpha() );
    SlicePar.SetAngleMax ( SlicePar.Alpha() + 0.5*SlicePar.DAlpha() );
    SlicePar.SetRMin     (  51. );					//TODO initialize from StRoot
    SlicePar.SetRMax     ( 194. );					//TODO initialize from StRoot
    SlicePar.SetErrX     (   0. );					//TODO initialize from StRoot
    SlicePar.SetErrY     (   0.12 ); // 0.06  for Inner			//TODO initialize from StRoot
    SlicePar.SetErrZ     (   0.16 ); // 0.12  for Inner		NodePar->fitPars()	//TODO initialize from StRoot
      //   SlicePar.SetPadPitch (   0.675 );// 0.335 -"-
    float x[3]={0,0,0},b[3];
    StarMagField::Instance()->BField(x,b);
    SlicePar.SetBz       ( - b[2] );   // change sign because change z
    if (sector <= 12) {
      SlicePar.SetZMin     (   0. );					//TODO initialize from StRoot
      SlicePar.SetZMax     ( 210. );					//TODO initialize from StRoot
    } else {
      SlicePar.SetZMin     (-210. );					//TODO initialize from StRoot
      SlicePar.SetZMax     (   0. );					//TODO initialize from StRoot
    }
    for( int iR = 0; iR < kNRows; iR++){
      SlicePar.SetRowX(iR, rRows[iR]);
    }
#if 1
    SlicePar.SetRecoType(1); //Stv hiterr parametrisation
    const double *coeffInner = StvHitErrCalculator::Inst("StvTpcInnerHitErrs")->GetPars();
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 0, iCoef, (float)coeffInner[iCoef] );
    }  
    SlicePar.SetParamS0Par(0, 0, 6, 0.0f );
    const double *coeffOuter = StvHitErrCalculator::Inst("StvTpcOuterHitErrs")->GetPars();
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 1, iCoef, (float)coeffOuter[iCoef] );
    }
#endif  
#if 0	//Hack
    SlicePar.SetRecoType(0); //Stv hiterr parametrisation
    float inn[6]= {0.000944592291, 0.00096804701, 0.0307030007
                  ,0.005380766   , 0.00276213209, 0.0185125507};

    float out[6]= {0.00119955395, 0.000499619695, 0.0558479801
                  ,0.0100383796 , 0.000534858496, 0.0479304604};
  
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 0, iCoef, inn[iCoef] );
      SlicePar.SetParamS0Par(0, 1, iCoef, out[iCoef] );
    }  
#endif  

    SlicePar.SetParamS0Par(0, 1, 6, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 0, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 1, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 2, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 3, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 4, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 5, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 6, 0.0f );
    
    fCaParam->push_back(SlicePar);
  } // for iSlice
} // void StiTPCCATrackerInterface::MakeSettings()

//_____________________________________________________________________________
void StvCASeedFinder::MakeHits()
{
const static double TAN15 = tan(M_PI*20/180); //18 != mistype

  fStvHits = StTGeoProxy::Inst()->GetAllHits(); 
  if (!fStvHits) return;
  int nHits = fStvHits->size();
  if (!nHits) 	return;
  for (int ihit = 0;ihit<nHits;ihit++)
  {
    StvHit *hit = (StvHit*)(*fStvHits)[ihit];
    if (!hit) 	continue;
    const StHitPlane* hp =hit->detector(); 
    if (!hp) 	continue;
    if (hp->GetDetId()!=kTpcId)	continue;
    const StTpcHit *tpcHit = (StTpcHit*)(hit->stHit());
    if ( ! tpcHit)      continue;
    int sector = tpcHit->sector();
    int padrow = tpcHit->padrow();


// 		Make  CA Hit
    AliHLTTPCCAGBHit caHit;
    myComplex myXY(hit->x()[0],hit->x()[1]);
    myXY*=sectAng[sector-1];
    assert(fabs(myXY.imag()/myXY.real())<=TAN15);
    assert(myXY.real()>0);
    caHit.SetX(   myXY.real()); // take position of the row
    caHit.SetY( - myXY.imag());
    caHit.SetZ( - hit->x()[2]);
      // caHit.SetErrX(   );
    caHit.SetErrY( 0.12 );// TODO: read parameters from somewhere 
    caHit.SetErrZ( 0.16 );
    caHit.SetISlice( sector - 1 );
    caHit.SetIRow(padrow-1 );
    caHit.SetID(ihit);
    fIdTruth->push_back( tpcHit->idTruth());
    fCaHits->push_back(caHit);
  }

} 
//________________________________________________________________________________
void StvCASeedFinder::Run()
{

//   TStopwatch timer;
//   timer.Start();
  
  MakeSettings();
  MakeHits();
  
    // run tracker
  fTracker->SetSettings(*fCaParam);
  fTracker->SetHits(*fCaHits);
  


//   timer.Stop();
//   fPreparationTime_real = timer.RealTime();
//   fPreparationTime_cpu = timer.CpuTime();  
  
  fTracker->FindTracks();
  fNTracks = fTracker->NTracks();
  fITrack = -1;
// 		copy hits
//   timer.Start();
  
//   timer.Stop();
//   fPreparationTime_real += timer.RealTime();
//   fPreparationTime_cpu += timer.CpuTime();    

} 
//________________________________________________________________________________
const THelixTrack* StvCASeedFinder::NextSeed()
{
  if (!fNTracks) Run();
  if (!fNTracks) 		return 0;;

  while (++fITrack<fNTracks) { 
    fSeedHits.clear();
// 		get seed
    const AliHLTTPCCAGBTrack tr = fTracker->Track( fITrack );
    const int NHits = tr.NHits();
    if (NHits <3) continue;
    for ( int iHit = NHits-1; iHit >= 0; iHit-- ){ 
      const int index = fTracker->TrackHit( tr.FirstHitRef() + iHit );
      const int hId   = fTracker->Hit( index ).ID();
      fSeedHits.push_back((StvHit*)(*fStvHits)[hId]);
    }
    const THelixTrack *ht = Approx();
    if (ht) return ht;
  }
  return 0;

} 
//________________________________________________________________________________
int StvCASeedFinder::padp(int pad, int row) {
  int p = 0;
  p = (int)(pad - NumberOfPadsAtRow[row]/2);
  if(row<13)
    p = (int)p/2;
  return p;
}
#endif
