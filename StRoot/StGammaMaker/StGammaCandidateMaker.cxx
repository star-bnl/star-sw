#include "StGammaCandidateMaker.h"

#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaRawMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"

#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StBarrelEmcCluster.h"
#include "StBarrelEmcClusterMaker.h"
#include "StGammaFitter.h"

ClassImp(StGammaCandidate);

// ----------------------------------------------------------------------------
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

// ----------------------------------------------------------------------------
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

	  Float_t umin[kEEmcNumSectors], vmin[kEEmcNumSectors];
	  Float_t umax[kEEmcNumSectors], vmax[kEEmcNumSectors];
	  Float_t umid[kEEmcNumSectors], vmid[kEEmcNumSectors];
	  Int_t ntow[kEEmcNumSectors];

	  // 1) find geometric center of tower cluster in each sector
	  for ( Int_t ii=0;ii<12;ii++ )
	    { umin[ii]=0.; vmin[ii]=0.; umax[ii]=0.; vmax[ii]=0.; umid[ii]=-1.; vmid[ii]=-1.; ntow[ii]=0; }

	  EEmcSmdMap *eemap = EEmcSmdMap::instance();
	  for ( Int_t itow = 0; itow < cluster.numberOfTowers(); itow++ )
	    {
	      StEEmcTower tower = cluster.tower(itow);
	      Int_t U, V;
	      eemap->getMiddleU( tower.sector(), tower.subsector(), tower.etabin(), U );
	      eemap->getMiddleV( tower.sector(), tower.subsector(), tower.etabin(), V );
	      ntow[ tower.sector() ]++;
	      umid[ tower.sector() ]+=0.5+(Float_t)U;// from middle of strip
	      vmid[ tower.sector() ]+=0.5+(Float_t)V;
	    }


	  for ( Int_t isec=0;isec<12;isec++ )
	    {
	      if ( ntow[isec] ) { 
		umid[isec]/=ntow[isec]; 
		vmid[isec]/=ntow[isec]; 
		umin[isec]=TMath::Max(umid[isec] - mSmdRange * 2.0, 0. );
		vmin[isec]=TMath::Max(vmid[isec] - mSmdRange * 2.0, 0. );
		umax[isec]=TMath::Min(umid[isec] + mSmdRange * 2.0, 287. );
		vmax[isec]=TMath::Min(vmid[isec] + mSmdRange * 2.0, 287. );
	      }
	    }

	  // now associate SMD strips with the candidate
	  for ( Int_t isec=0;isec<12;isec++ )
	    {
	      if ( !ntow[isec] ) continue;
	      for ( Int_t i=(Int_t)umin[isec];i<(Int_t)umax[isec];i++ )
		{
		  StGammaStrip *strip = grawmaker->strip(isec,0,i);
		  if ( strip ) {
		    can->addSmdu(strip);
		  }
		}
	      for ( Int_t i=(Int_t)vmin[isec];i<(Int_t)vmax[isec];i++ )
		{
		  StGammaStrip *strip = grawmaker->strip(isec,1,i);
		  if ( strip ) {
		    can->addSmdv(strip);
		  }
		}
	    }



	}// loop over clusters

    }// loop over sectors

  //
  // Run gamma fitter on Endcap candidates
  //
  for (int i = 0; i < gevent->numberOfCandidates(); ++i) {
    StGammaCandidate* candidate = gevent->candidate(i);
    if (candidate->detectorId() == StGammaCandidate::kEEmc) {
      StGammaFitterResult u, v;
      if (StGammaFitter::instance()->fitSector(candidate, &u, &v)) {
	candidate->SetSmduFit(u);
	candidate->SetSmdvFit(v);
      }
    }
  }

  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StGammaCandidateMaker::MakeBarrel()
{
  // Get gamma event maker
  StGammaEventMaker* gemaker = (StGammaEventMaker*)GetMaker("gemaker");
  if (!gemaker) {
    LOG_WARN << "MakeBarrel - No gamma event maker" << endm;
    return kStWarn;
  }

  // Get gamma event
  StGammaEvent* gevent = gemaker->event();
  if (!gevent) {
    LOG_WARN << "MakeBarrel - No gamma event" << endm;
    return kStWarn;
  }

  // Get gamma raw maker
  StGammaRawMaker* grawmaker = (StGammaRawMaker*)GetMaker("grawmaker");
  if (!grawmaker) {
    LOG_WARN << "MakeBarrel - No gamma raw maker" << endm;
    return kStWarn;
  }

  // Get BEMC cluster maker
  StBarrelEmcClusterMaker* ecl = (StBarrelEmcClusterMaker*)GetMaker("bemc_cluster");
  if (!ecl) {
    LOG_WARN << "MakeBarrel - No BEMC clusters" << endm;
    return kStWarn;
  }

  // Loop over BEMC clusters
  for (unsigned i = 0; i < ecl->clusters().size(); ++i) {
    StBarrelEmcCluster* cluster = ecl->clusters()[i];

    // Cut on transverse energy of the cluster
    if (cluster->momentum().Pt() < mMinimumET) continue;

    // Create gamma candidate
    StGammaCandidate* candidate = gevent->newCandidate();
    candidate->SetTowerClusterId(i);
    candidate->SetId(nextId());
    candidate->SetDetectorId(StGammaCandidate::kBEmc);
    candidate->SetEnergy(cluster->energy());
    candidate->SetPosition(cluster->position());
    candidate->SetMomentum(cluster->momentum());

    // Set seed energy
    StGammaTower* seed = cluster->seed();
    candidate->SetSeedEnergy(seed->energy);
    candidate->SetTowerId(seed->id);

    float energy = 0;
    for (int deta = -1; deta <= 1; ++deta) {
      for (int dphi = -1; dphi <= 1; ++dphi) {
	if (StGammaTower* tower = cluster->tower(deta, dphi)) {
	  // Add BTOW hit to candidate list of "my" towers
	  candidate->addMyTower(tower);

	  // Add BPRS hit to candidate list of "my" towers
	  if (StGammaTower* preshower = grawmaker->tower(tower->id, kBEmcPres)) {
	    candidate->addMyPreshower1(preshower);
	    energy += preshower->energy;
	  }

	  // Add tracks to candidate list of "my" towers
	  for (int k = 0; k < gevent->numberOfTracks(); ++k) {
	    StGammaTrack* track = gevent->track(k);
	    if (!track) continue;
	    TVector3 position;
	    TVector3 momentum;
	    double magneticField = gevent->magneticField() * kilogauss;
	    if (!getPositionMomentumAtBarrel(track, magneticField, position, momentum)) continue;
	    int id;
	    if (StEmcGeom::instance("bemc")->getId(position.Phi(), position.Eta(), id) != 0) continue;
	    if (id != static_cast<int>(tower->id)) continue;
	    candidate->addMyTrack(track);
	  }
	}
      }
    }

    // Set candidate preshower energy sum
    candidate->SetBPrsEnergy(energy);

    // Add tracks that fall within mRadius of candidate
    for (int k = 0; k < gevent->numberOfTracks(); ++k) {
      if (StGammaTrack* track = gevent->track(k)) {
	float deta = cluster->momentum().Eta() - track->eta();
	float dphi = cluster->momentum().Phi() - track->phi();
	dphi = acos(cos(dphi));
	float r = hypot(deta, dphi);
	if (r <= mRadius) candidate->addTrack(track);
      }
    }

    // Add BTOW hits that fall within mRadius of candidate
    for (int k = 0; k < gevent->numberOfTowers(); ++k) {
      StGammaTower* tower = gevent->tower(k);
      if (tower && tower->layer == kBEmcTower) {
	float deta = cluster->momentum().Eta() - tower->eta;
	float dphi = cluster->momentum().Phi() - tower->phi;
	dphi = acos(cos(dphi));
	float r = hypot(deta, dphi);
	if (r <= mRadius) candidate->addTower(tower);
      }
    }

    // Add BPRS hits that fall within mRadius of candidate
    for (int k = 0; k < gevent->numberOfTowers(); ++k) {
      StGammaTower* preshower = gevent->preshower1(k);
      if (preshower && preshower->layer == kBEmcPres) {
	float deta = cluster->momentum().Eta() - preshower->eta;
	float dphi = cluster->momentum().Phi() - preshower->phi;
	dphi = acos(cos(dphi));
	float r = hypot(deta, dphi);
	if (r <= mRadius) candidate->addPreshower1(preshower);
      }
    }

    // Add BSMDE and BSMDP strips that fall within mSmdRange of candidate
    float smdEtaEnergy = 0;
    float smdPhiEnergy = 0;

    for (int id = 1; id <= 18000; ++id) {
      int sector = 0;		// Not used
      if (StGammaStrip* strip = grawmaker->strip(sector, kBEmcSmdEta, id)) {
	float x, y, z;
	StEmcGeom::instance("bsmde")->getXYZ(id, x, y, z);
	TVector3 d(x, y, z);
	d -= cluster->position();
	if (d.Mag() <= mSmdRange) {
	  candidate->addSmdEta(strip);
	  smdEtaEnergy += strip->energy;
	}
      }
      if (StGammaStrip* strip = grawmaker->strip(sector, kBEmcSmdPhi, id)) {
	float x, y, z;
	StEmcGeom::instance("bsmdp")->getXYZ(id, x, y, z);
	TVector3 d(x, y, z);
	d -= cluster->position();
	if (d.Mag() <= mSmdRange) {
	  candidate->addSmdPhi(strip);
	  smdPhiEnergy += strip->energy;
	}
      }
    }

    candidate->SetSmdEtaEnergy(smdEtaEnergy);
    candidate->SetSmdPhiEnergy(smdPhiEnergy);
  }

  return kStOK;
}

//
// See $STAR/StRoot/StEmcUtil/projection/StEmcPosition.h
//
bool StGammaCandidateMaker::getPositionMomentumAtBarrel(StGammaTrack* track, double magneticField, TVector3& position, TVector3& momentum)
{
  const double radius = StEmcGeom::instance("bemc")->Radius();
  const pair<double, double> VALUE(999999999., 999999999.); // No solution
  const StPhysicalHelix& helix = track->outerHelix();

  if (helix.origin().perp() > radius) return false;
  pair<double, double> ss = helix.pathLength(radius);
  if (!finite(ss.first) || !finite(ss.second)) return false;
  if (ss == VALUE) return false;

  double s = 0;
  if (ss.first > 0 && ss.second > 0)
    s = ss.first;
  else if (ss.first >= 0 && ss.second < 0)
    s = ss.first;
  else if (ss.first < 0 && ss.second >= 0)
    s = ss.second;
  else
    return false;

  position = TVector3(helix.at(s).xyz());
  momentum = TVector3(helix.momentumAt(s, magneticField).xyz());

  return true;
}
