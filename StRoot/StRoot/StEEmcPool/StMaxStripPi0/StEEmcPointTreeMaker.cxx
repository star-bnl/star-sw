#include "StEEmcPointTreeMaker.h"
#include "StEEmcA2EMaker.h"
#include "StEEmcClusterMaker.h"
#include "StEEmcMixEvent.h"

ClassImp(StEEmcPointTreeMaker);

// ----------------------------------------------------------------------------
StEEmcPointTreeMaker::StEEmcPointTreeMaker( const Char_t *n):StEEmcPointMaker(n)
{
  mFilename="NONE";
  mMixEvent=new StEEmcMixEvent();
  mFile=0;
}

// ----------------------------------------------------------------------------
Int_t StEEmcPointTreeMaker::Init()
{

  if ( !mFilename.Contains("NONE") )
    {
      mFile = new TFile(mFilename,"recreate");
    }

  mTree=new TTree("mTree","Point maker tree");
  mTree->Branch("mEEmcPoints","StEEmcMixEvent",&mMixEvent);

  return StEEmcPointMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPointTreeMaker::Make()
{
  Int_t stat=StEEmcPointMaker::Make();
  if ( stat != kStOK ) return stat;

  /// loop over points, add to event
  for ( UInt_t i = 0; i < mPoints.size(); i++ ) 
    mMixEvent -> addPoint ( mPoints[i] );

  /// loop over all smd clusters and add to event
  for ( Int_t sec=0; sec<12; sec++ )
    for ( Int_t pln=0; pln<2; pln++ )
      for ( Int_t cl=0;cl<mEEclusters->numberOfSmdClusters(sec,pln); cl++ )
	{
	  mMixEvent->addCluster( mEEclusters->smdcluster(sec,pln,cl));
	}

  /// Get list of hit towers and store in event   
  StEEmcTowerVec_t towers=mEEanalysis->towers(); 
  Float_t etotal=0.;
  for ( UInt_t i=0; i < towers.size(); i++ ) 
    {
    //     mMixEvent -> addTower( towers[i] ); 
      etotal+= towers[i].energy();
    }
  mMixEvent->setTotalEnergy(etotal);
  mMixEvent->setEnergySeen(energySeen());

  for ( Int_t i=0;i<12; i++ ) {
    mMixEvent->setTotalEnergyU( i,mEEanalysis->energy(i,4) );
    mMixEvent->setTotalEnergyV( i,mEEanalysis->energy(i,5) );
  }

  /// Fill the ttree
  mTree -> Fill();

  return kStOK;

}

// ----------------------------------------------------------------------------
void StEEmcPointTreeMaker::Clear(Option_t *o)
{
  mMixEvent->Clear();
  StEEmcPointMaker::Clear();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPointTreeMaker::Finish()
{
  if ( mFile )
    {
      std::cout << "StEEmcPointTreeMaker::Finish()" << std::endl;
      mFile->Write();
    }

  return kStOK;
}
