#include "TVector2.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"

#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StBarrelEmcCluster.h"
#include "StBarrelEmcClusterMaker.h"
#include "StGammaFitter.h"
#include "StGammaEvent.h"
#include "StGammaEventMaker.h"
#include "StGammaRawMaker.h"
#include "StGammaCandidateMaker.h"

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
  Compress();
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

	      if ( gtower ) {
		can -> addMyTower(gtower);
		gtower -> candidates.Add( can );
	      }

	      if ( gpre1 && !pre1.fail() && pre1.energy() > 0. ) {
		can -> addMyPreshower1( gpre1 );
		gpre1 -> candidates.Add( can );
	      }

	      if ( gpre2 && !pre2.fail() && pre2.energy() > 0. ) {
		can -> addMyPreshower2( gpre2 );
		gpre2 -> candidates.Add( can );
	      }
	      
	      if ( gpost && !post.fail() && post.energy() > 0. ) {
		can -> addMyPostshower( gpost );
		gpost -> candidates.Add( can );
	      }

	      // add tracks to the list of "my" tracks, i.e. tracks which 
	      // extrapolate to the gamma candidate tower

	      if ( gtower ) {
		for (int k = 0; k < gevent->numberOfTracks(); ++k) {
		  StGammaTrack* track = gevent->track(k);
		  if (!track || track->pz() < 0) continue;
		  try
		    {
		      EEmcGeomSimple& geom = EEmcGeomSimple::Instance();
		      TVector3 position = track->positionAtZ(geom.getZ1());
		      int sector, subsector, etabin;
		      if (geom.getTower(position, sector, subsector, etabin))
			{
			  if (gtower->sector   () == sector    &&
			      gtower->subsector() == subsector &&
			      gtower->etabin   () == etabin)
			    {
			      can -> addMyTrack( track );
			      track -> candidates.Add( can );
			    }
			}
		    }
		  catch (StGammaTrack::Exception& e) {}
		}
	      }
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
	      Float_t dphi = TVector2::Phi_mpi_pi(phi_track - phi_candidate);

	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addTrack( track );
		  if (!track -> candidates.FindObject( can )) track -> candidates.Add( can );
		}
	      
	    }


	  for ( Int_t i=0;i<gevent->numberOfTowers();i++ )
	    {
	      StGammaTower *tower = gevent->tower(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      Float_t dphi = TVector2::Phi_mpi_pi(phi_tower - phi_candidate);

	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addTower( tower );
		  if (!tower -> candidates.FindObject( can )) tower -> candidates.Add( can );
		}
	      
	    }

	  for ( Int_t i=0;i<gevent->numberOfPreshower1();i++ )
	    {
	      StGammaTower *tower = gevent->preshower1(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      Float_t dphi = TVector2::Phi_mpi_pi(phi_tower - phi_candidate);

	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPreshower1( tower );
		  if (!tower -> candidates.FindObject( can )) tower -> candidates.Add( can );
		}
	      
	    }


	  for ( Int_t i=0;i<gevent->numberOfPreshower2();i++ )
	    {
	      StGammaTower *tower = gevent->preshower2(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      Float_t dphi = TVector2::Phi_mpi_pi(phi_tower - phi_candidate);

	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPreshower2( tower );
		  if (!tower -> candidates.FindObject( can )) tower -> candidates.Add( can );
		}
	      
	    }

	  for ( Int_t i=0;i<gevent->numberOfPostshower();i++ )
	    {
	      StGammaTower *tower = gevent->postshower(i);
	      if (!tower) continue;

	      Float_t eta_tower = tower -> eta;
	      Float_t phi_tower = tower -> phi;
	      Float_t deta = eta_tower - eta_candidate;
	      Float_t dphi = TVector2::Phi_mpi_pi(phi_tower - phi_candidate);

	      Float_t r = TMath::Sqrt( dphi*dphi + deta*deta );

	      if ( r <= mRadius ) 
		{
		  can -> addPostshower( tower );
		  if (!tower -> candidates.FindObject( can )) tower -> candidates.Add( can );
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
                    strip->position = i;
		    can->addSmdu(strip);
		    strip->candidates.Add(can);
		  }
		}
	      for ( Int_t i=(Int_t)vmin[isec];i<(Int_t)vmax[isec];i++ )
		{
		  StGammaStrip *strip = grawmaker->strip(isec,1,i);
		  if ( strip ) {
                    strip->position = i;
		    can->addSmdv(strip);
		    strip->candidates.Add(can);
		  }
		}
	    }

	  // Sum SMDU energy
	  float smduEnergy = 0;
	  for (int i = 0; i < can->numberOfSmdu(); ++i)
	    {
	      StGammaStrip* strip = can->smdu(i);
	      smduEnergy += strip->energy;
	    }
	  can->SetSmduEnergy(smduEnergy);

	  // Sum SMDV energy
	  float smdvEnergy = 0;
	  for (int i = 0; i < can->numberOfSmdv(); ++i)
	    {
	      StGammaStrip* strip = can->smdv(i);
	      smdvEnergy += strip->energy;
	    }
	  can->SetSmdvEnergy(smdvEnergy);

	}// loop over clusters

    }// loop over sectors

  //
  // Run gamma fitter on Endcap candidates
  //
  for (int i = 0; i < gevent->numberOfCandidates(); ++i) {
    StGammaCandidate* candidate = gevent->candidate(i);
    if (candidate->detectorId() == StGammaCandidate::kEEmc) {
      StGammaFitterResult fit;
      int status = StGammaFitter::instance()->fitSector(candidate, &fit);
      if (status == 0) candidate->SetSmdFit(fit);
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
	  tower->candidates.Add(candidate);

	  // Add BPRS hit to candidate list of "my" towers
	  if (StGammaTower* preshower = grawmaker->tower(tower->id, kBEmcPres)) {
	    candidate->addMyPreshower1(preshower);
	    preshower->candidates.Add(candidate);
	    energy += preshower->energy;
	  }

	  // Add tracks to candidate list of "my" towers
	  for (int k = 0; k < gevent->numberOfTracks(); ++k) {
	    StGammaTrack* track = gevent->track(k);
	    if (!track) continue;
	    try {
	      StEmcGeom* geom = StEmcGeom::instance("bemc");
	      TVector3 position = track->positionAtRadius(geom->Radius());
	      int id;
	      if (geom->getId(position.Phi(), position.Eta(), id) == 0 &&
		  id == static_cast<int>(tower->id)) {
		candidate->addMyTrack(track);
		track->candidates.Add(candidate);
	      }
	    }
	    catch (StGammaTrack::Exception& e) {}
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
	dphi = TVector2::Phi_mpi_pi(dphi);
	float r = hypot(deta, dphi);
	if (r <= mRadius) {
	  candidate->addTrack(track);
	  if (!track->candidates.FindObject(candidate)) track->candidates.Add(candidate);
	}
      }
    }

    // Add BTOW hits that fall within mRadius of candidate
    for (int k = 0; k < gevent->numberOfTowers(); ++k) {
      StGammaTower* tower = gevent->tower(k);
      if (tower && tower->layer == kBEmcTower) {
	float deta = cluster->momentum().Eta() - tower->eta;
	float dphi = cluster->momentum().Phi() - tower->phi;
	dphi = TVector2::Phi_mpi_pi(dphi);
	float r = hypot(deta, dphi);
	if (r <= mRadius) {
	  candidate->addTower(tower);
	  if (!tower->candidates.FindObject(candidate)) tower->candidates.Add(candidate);
	}
      }
    }

    // Add BPRS hits that fall within mRadius of candidate
    for (int k = 0; k < gevent->numberOfTowers(); ++k) {
      StGammaTower* preshower = gevent->preshower1(k);
      if (preshower && preshower->layer == kBEmcPres) {
	float deta = cluster->momentum().Eta() - preshower->eta;
	float dphi = cluster->momentum().Phi() - preshower->phi;
	dphi = TVector2::Phi_mpi_pi(dphi);
	float r = hypot(deta, dphi);
	if (r <= mRadius) {
	  candidate->addPreshower1(preshower);
	  if (!preshower->candidates.FindObject(candidate)) preshower->candidates.Add(candidate);
	}
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
	  strip->candidates.Add(candidate);
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
	  strip->candidates.Add(candidate);
	  smdPhiEnergy += strip->energy;
	}
      }
    }

    candidate->SetSmdEtaEnergy(smdEtaEnergy);
    candidate->SetSmdPhiEnergy(smdPhiEnergy);
  }

  return kStOK;
}

Int_t StGammaCandidateMaker::Compress()
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

  // Drop strips, tracks, towers without candidates
  Compress<StGammaStrip>(gevent->mStrips);
  Compress<StGammaTrack>(gevent->mTracks);
  Compress<StGammaTower>(gevent->mTowers);
  Compress<StGammaTower>(gevent->mPreshower1);
  Compress<StGammaTower>(gevent->mPreshower2);
  Compress<StGammaTower>(gevent->mPostshower);

  return kStOk;
}

template<class T>
void StGammaCandidateMaker::Compress(TClonesArray* clones)
{
  TIter next(clones);
  while (T* x = (T*)next()) if (x->candidates.IsEmpty()) clones->Remove(x);
  clones->Compress();
}
