#include "StGammaCandidateMaker.h"

#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaRawMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"

ClassImp(StGammaCandidate);

StGammaCandidateMaker::StGammaCandidateMaker( const Char_t *name ):StMaker(name)
{
  Clear();
  SetMinimumET(5.0);
  SetRadius(0.7);
  SetSmdRange(20.0);
}

// ----------------------------------------------------------------------------
Int_t StGammaCandidateMaker::Init()
{
  return StMaker::Init();
}
// ----------------------------------------------------------------------------
Int_t StGammaCandidateMaker::Make()
{ 
  MakeEndcap();
  MakeBarrel();
  return kStOK;
}
// ----------------------------------------------------------------------------
void StGammaCandidateMaker::Clear( Option_t *opts )
{
  mId=0;
}
// ----------------------------------------------------------------------------

TVector3 momentum( StEEmcCluster cluster, TVector3 vertex )
{
  TVector3 d=cluster.momentum().Unit();
  d.SetMag( kEEmcZSMD / d.CosTheta() );
  d=d-vertex;
  d=d.Unit();
  d*=cluster.energy();
  return d;
};

Int_t StGammaCandidateMaker::MakeEndcap()
{

  StEEmcGenericClusterMaker *mEEclusters = (StEEmcGenericClusterMaker*)GetMaker("mEEclusters");
  if ( !mEEclusters )
    {
      LOG_DEBUG << "no EEmc Clusters"<<endm;
      return kStWarn;
    }

  StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMaker("gemaker");
  if ( !gemaker )
    {
      LOG_DEBUG << "no gamma event maker" << endm;
      return kStWarn;
    }
  StGammaEvent *gevent = gemaker->event();
  assert(gevent);

  StEEmcA2EMaker *mEEanalysis=(StEEmcA2EMaker*)GetMaker("mEEanalysis");
  if ( !mEEanalysis )
    {
      LOG_DEBUG<<"no EEmc adc to energy"<<endm;
      return kStWarn;
    }

  StGammaRawMaker *grawmaker = (StGammaRawMaker*)GetMaker("grawmaker");
  if ( !grawmaker )
    {
      LOG_DEBUG<<"no gamma raw maker" <<endm;
      return kStWarn;
    }


  // loop over all sectors, then all clusters in each sector
  for ( Int_t sector=0;sector<kEEmcNumSectors;sector++ )
    {

      StEEmcClusterVec_t clusters = mEEclusters->clusters(sector, 0);
      for ( UInt_t i=0;i<clusters.size();i++ )
	{

	  StEEmcCluster cluster = clusters[i];
	  TVector3 pcluster = momentum( cluster, gevent->vertex() );
	  Float_t ET = pcluster.Perp();
	  if ( ET < mMinimumET ) continue; // min ET threshold

	  StGammaCandidate *can = gevent->newCandidate();

	  can -> SetId( nextId() );
	  can -> SetTowerClusterId( cluster.key() ); // identity of the cluster
	  can -> SetDetectorId( StGammaCandidate::kEEmc );
	  
	  can->SetMomentum( pcluster );
	  can->SetPosition( cluster.position() );
	  can->SetEnergy( cluster.energy() );

	  StEEmcTower seed = cluster.tower(0);
	  can->SetSeedEnergy( seed.energy() );
	  can->SetTowerId( seed.index() );
	  

	  // loop over all towers in the cluster and add 
	  // each tower and associated pre/post element
	  // to the gamma candiate
	  Float_t epre1 = 0.;
	  Float_t epre2 = 0.;
	  Float_t epost = 0.;

	  for ( Int_t j=0;j<cluster.numberOfTowers();j++ )
	    {
	      StEEmcTower tower = cluster.tower(j);
	      Int_t index = tower.index();
	      StEEmcTower pre1 = mEEanalysis->tower(index,1);
	      StEEmcTower pre2 = mEEanalysis->tower(index,2);
	      StEEmcTower post = mEEanalysis->tower(index,3);

	      if ( !pre1.fail() ) epre1 += pre1.energy();
	      if ( !pre2.fail() ) epre2 += pre2.energy();
	      if ( !post.fail() ) epost += post.energy();

	      // note that the raw maker does not guarentee that these
	      // exist... if they were not hit, the pointer will be null
	      
	      StGammaTower *gtower = grawmaker -> tower( index, kEEmcTower );
	      StGammaTower *gpre1  = grawmaker -> tower( index, kEEmcPre1  );
	      StGammaTower *gpre2  = grawmaker -> tower( index, kEEmcPre2  );
	      StGammaTower *gpost  = grawmaker -> tower( index, kEEmcPost  );

	      // add towers to the list of "my" towers, i.e. towers which 
	      // belong to the gamma candidate cluster

	      if ( gtower )
		can -> addMyTower(gtower);

	      if ( gpre1 && !pre1.fail() && pre1.energy() > 0. )
		can -> addMyPreshower1( gpre1 );

	      if ( gpre2 && !pre2.fail() && pre2.energy() > 0. )
		can -> addMyPreshower2( gpre2 );
	      
	      if ( gpost && !post.fail() && post.energy() > 0. )
		can -> addMyPostshower( gpost );
	      
	      
	    }


	  // set pre and postshwoer energy sums
	  
	  can -> SetPre1Energy( epre1 );
	  can -> SetPre2Energy( epre2 );
	  can -> SetPostEnergy( epost );


	  /*
	   * Next we set the references in the gamma candidate which will point
	   * to the towers, tracks, etc... which will be stored in the ttree.
	   *
	   */


	  Float_t eta_candidate = pcluster.Eta();
	  Float_t phi_candidate = pcluster.Phi();


	  for ( Int_t i=0;i<gevent->numberOfTracks();i++ )
	    {
	      StGammaTrack *track = gevent->track(i);
	      if (!track) continue;

	      Float_t eta_track = track -> eta();
	      Float_t phi_track = track -> phi();
	      Float_t deta = eta_track - eta_candidate;
	      
	      Float_t phi1 = TMath::Max(phi_track, phi_candidate);
	      Float_t phi2 = TMath::Min(phi_track, phi_candidate);

	      Float_t dphi = TMath::Min( phi1 - phi2, phi1 - phi2 + (Float_t)TMath::TwoPi() );
	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addTrack( track );
		}
	      
	    }


	  for ( Int_t i=0;i<gevent->numberOfTowers();i++ )
	    {
	      StGammaTower *tower = gevent->tower(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      
	      Float_t phi1 = TMath::Max(phi_tower, phi_candidate);
	      Float_t phi2 = TMath::Min(phi_tower, phi_candidate);

	      Float_t dphi = TMath::Min( phi1 - phi2, phi1 - phi2 + (Float_t)TMath::TwoPi() );
	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addTower( tower );
		}
	      
	    }

	  for ( Int_t i=0;i<gevent->numberOfPreshower1();i++ )
	    {
	      StGammaTower *tower = gevent->preshower1(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      
	      Float_t phi1 = TMath::Max(phi_tower, phi_candidate);
	      Float_t phi2 = TMath::Min(phi_tower, phi_candidate);

	      Float_t dphi = TMath::Min( phi1 - phi2, phi1 - phi2 + (Float_t)TMath::TwoPi() );
	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPreshower1( tower );
		}
	      
	    }


	  for ( Int_t i=0;i<gevent->numberOfPreshower2();i++ )
	    {
	      StGammaTower *tower = gevent->preshower2(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      
	      Float_t phi1 = TMath::Max(phi_tower, phi_candidate);
	      Float_t phi2 = TMath::Min(phi_tower, phi_candidate);

	      Float_t dphi = TMath::Min( phi1 - phi2, phi1 - phi2 + (Float_t)TMath::TwoPi() );
	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPreshower2( tower );
		}
	      
	    }

	  for ( Int_t i=0;i<gevent->numberOfPostshower();i++ )
	    {
	      StGammaTower *tower = gevent->postshower(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      
	      Float_t phi1 = TMath::Max(phi_tower, phi_candidate);
	      Float_t phi2 = TMath::Min(phi_tower, phi_candidate);

	      Float_t dphi = TMath::Min( phi1 - phi2, phi1 - phi2 + (Float_t)TMath::TwoPi() );
	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPostshower( tower );
		}
	      
	    }

	  /*
	   * Next we add in the SMD strips.  Will need to figure out how we want
	   * to choose the center of the range first.
	   */




	}

    }


  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StGammaCandidateMaker::MakeBarrel()
{
  return kStOK;
}
