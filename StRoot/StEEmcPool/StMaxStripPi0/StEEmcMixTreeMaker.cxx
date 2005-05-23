#include "StEEmcMixTreeMaker.h"
#include "StEEmcA2EMaker.h" 
#include "StEEmcPointMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

ClassImp(StEEmcMixTreeMaker);

// ----------------------------------------------------------------------------
StEEmcMixTreeMaker::StEEmcMixTreeMaker(const Char_t *n):StEEmcMixMaker(n)
{
  mFilename="NONE";
  mMixEvent = new StEEmcMixEvent();
  mFile=0;
}

// ----------------------------------------------------------------------------
Int_t StEEmcMixTreeMaker::Init()
{

  if ( !mFilename.Contains("NONE") ) 
    mFile = new TFile(mFilename,"recreate");

  mTree=new TTree("mTree","Real events");
  mTree->Branch("mPi0event","StEEmcMixEvent",&mMixEvent);

  return StEEmcMixMaker::Init();
}


// ----------------------------------------------------------------------------
Int_t StEEmcMixTreeMaker::Make()
{

  /// Form all pairs of points, combinatoric background
  Int_t stat = StEEmcMixMaker::Make();
  if ( stat != kStOK ) return stat;

  /// Set the event, run, trigger info
  mMixEvent -> setEvent( mMuDstMaker -> muDst() -> event() );

  /// loop over points, add to event
  for ( UInt_t i = 0; i < mPoints.size(); i++ ) 
    mMixEvent -> addPoint ( mPoints[i] );
 
  /// loop over pi0 candidate pairs, add to event
  for ( UInt_t i = 0; i < mCandidates.size(); i++ )
    mMixEvent -> addPair ( mCandidates[i] );

  /// loop over mixed events, add to event
  for ( UInt_t i = 0; i < mBackground.size(); i++ ) 
    mMixEvent -> addMixed( mBackground[i] );

  /// Get list of hit towers and store in event   
  StEEmcTowerVec_t towers=mEEanalysis->towers(); 
  Float_t etotal=0.;
  Float_t emax=0.;
  Int_t imax=-1;
  for ( UInt_t i=0; i < towers.size(); i++ ) 
    {
      // need to add copy constructor before this line will work
      //      mMixEvent -> addTower( towers[i] ); 
      etotal+= towers[i].energy();
      if ( towers[i].energy() > emax ) {
	emax=towers[i].energy();
	imax=(UInt_t)i;
      }
    }
  mMixEvent->setTotalEnergy(etotal);
  mMixEvent->setEnergySeen(mEEpoints->energySeen());
  if ( imax>=0 ) 
    {
      mMixEvent->setHighTower( towers[imax] );
    }
  else 
    {
      StEEmcTower tmp;
      mMixEvent -> setHighTower( tmp );
    }
 

  /// Fill the ttree
  mTree -> Fill();

  return kStOK;

}

// ----------------------------------------------------------------------------
void StEEmcMixTreeMaker::Clear(Option_t *o)
{

  mMixEvent -> Clear();

  StEEmcMixMaker::Clear();

}

// ----------------------------------------------------------------------------
Int_t StEEmcMixTreeMaker::Finish()
{

  if ( mFile ) {
    mFile->Write();
  }

  return kStOK;

}
