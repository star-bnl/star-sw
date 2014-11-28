#include "TVector2.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"

#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

#include "StBarrelEmcCluster.h"
#include "StBarrelEmcClusterMaker.h"
#include "StGammaFitter.h"
#include "StGammaEvent.h"
#include "StGammaEventMaker.h"
#include "StGammaRawMaker.h"

#include "StGammaCandidateMaker.h"

ClassImp(StGammaCandidate);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaCandidateMaker::StGammaCandidateMaker(const char *name): StMaker(name)
{

    mUseBemc = false;
    mUseEemc = false;
    
    mStrictBemcStatus = true;
    
    mCompressLevel = 2;

    mMinimumEt = 0.0;
    mRadius = 0.7; 
    mBsmdRange = 0.05237;
    mEsmdRange = 20.0;   
    
}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaCandidateMaker::~StGammaCandidateMaker()
{}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaCandidateMaker::Init()
{
    return StMaker::Init();
}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StGammaCandidateMaker::Clear( Option_t *opts )
{
    mId = 0;
}

//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaCandidateMaker::Make()
{ 

    if(mUseBemc) MakeBarrel();
    if(mUseEemc) MakeEndcap();
    
    Compress();
    
    return kStOK;
  
}


//////////////////////////////////////////////////
//     Return the vertex corrected momentum     //
//         for the given EEMC cluster           //
//////////////////////////////////////////////////
TVector3 momentum(StEEmcCluster cluster, TVector3 vertex)
{

    TVector3 d = cluster.momentum().Unit();
    
    d.SetMag( kEEmcZSMD / d.CosTheta() );
    d = d - vertex;
    d = d.Unit();
    d *= cluster.energy();
    
    return d;
  
}

//////////////////////////////////////////////////
//            Compile BEMC Clusters             //
//////////////////////////////////////////////////
Int_t StGammaCandidateMaker::MakeBarrel()
{

    // Retrieve all those gamma makers from the chain
    StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    if(!mGammaEventMaker)
    {
        LOG_WARN << "MakeBarrel() - No StGammaEventMaker found!" << endm;
        return kStWarn;
    }

    StGammaEvent *mGammaEvent = mGammaEventMaker->event();
    if(!mGammaEvent) 
    {
        LOG_WARN << "MakeBarrel() - No StGammaEvent found!" << endm;
        return kStWarn;
    }

    StGammaRawMaker *mGammaRawMaker = dynamic_cast<StGammaRawMaker*>(GetMakerInheritsFrom("StGammaRawMaker"));
    if(!mGammaRawMaker)
    {
        LOG_WARN << "MakeBarrel() - No StGammaRawMaker found!" << endm;
        return kStWarn;
    }
    
    StBarrelEmcClusterMaker *mBemcClusterMaker = dynamic_cast<StBarrelEmcClusterMaker*>(GetMakerInheritsFrom("StBarrelEmcClusterMaker"));
    if(!mBemcClusterMaker)
    {
        LOG_WARN << "MakeBarrel() - No StBarrelEmcClusterMaker found!" << endm;
        return kStWarn;
    }

    // Create a StEmcGeom instance for track projections
    StEmcGeom* geom = StEmcGeom::instance("bemc");

    StEmcGeom *smdEtaGeom = new StEmcGeom("bsmde");
    StEmcGeom *smdPhiGeom = new StEmcGeom("bsmdp");

    StEmcPosition emcPosition;

    // Loop over BEMC clusters
    for(unsigned int i = 0; i < mBemcClusterMaker->clusters().size(); ++i) 
    {
    
        StBarrelEmcCluster* cluster = mBemcClusterMaker->clusters()[i];
        
        // Require minimum Et
        if(cluster->momentum().Pt() < mMinimumEt) continue;
        
        // Require that seed towers have non fail status
        if(cluster->seed()->fail) continue;
        
        // If strict status is observed, require that all
        // neighboring towers also have a non fail status
        if(mStrictBemcStatus)
        {
        
            bool fail = false;
        
            for(int deta = -1; deta <= 1; ++deta)
            {
                for(int dphi = -1; dphi <= 1; ++dphi)
                {
                    if(StGammaTower *tower = cluster->tower(deta, dphi))
                    {
                        if(tower->fail) fail = true;
                    }
                }
            }
            
            if(fail) continue;
        
        }
        
        // Create gamma candidate
        StGammaCandidate *candidate = mGammaEvent->newCandidate();
        candidate->SetTowerClusterId(i);
        candidate->SetId(nextId());
        candidate->SetDetectorId(StGammaCandidate::kBEmc);
        candidate->SetEnergy(cluster->energy());
        candidate->SetPosition(cluster->position());
        candidate->SetMomentum(cluster->momentum());
        
        // Start with the seed tower
        StGammaTower *seed = cluster->seed();
        candidate->SetSeedEnergy(seed->energy);
        candidate->SetTowerId(seed->id);

        // Loop over neighboring towers in the cluster
        float energy = 0;
        
        for(int deta = -1; deta <= 1; ++deta) 
        {
        
            for(int dphi = -1; dphi <= 1; ++dphi) 
            {
            
                if(StGammaTower *tower = cluster->tower(deta, dphi)) 
                {
                
                    // Skip any failing towers
                    if(tower->fail) continue;
                    
                    // Associate tower with the candidate
                    candidate->addMyTower(tower);
                    tower->candidates.Add(candidate);
                    
                    // Associate preshower with the candidate
                    if(StGammaTower *preshower = mGammaRawMaker->tower(tower->id, kBEmcPres)) 
                    {
                        candidate->addMyPreshower1(preshower);
                        preshower->candidates.Add(candidate);
                        energy += preshower->energy;
                    }
                    
                    // Associate tracks with the candidate
                    for(int k = 0; k < mGammaEvent->numberOfTracks(); ++k) 
                    {
                    
                        StGammaTrack* track = mGammaEvent->track(k);
                        if(!track) continue;
                    
                        TVector3 position = track->positionAtRadius(geom->Radius());
                        if(position != TVector3(0, 0, 0))
                        {
                            
                            int id;
                            if(geom->getId(position.Phi(), position.Eta(), id) == 0 && id == static_cast<int>(tower->id)) 
                            {
                                candidate->addMyTrack(track);
                                track->candidates.Add(candidate);
                            }
                    
                        }
                    
                    }

                } // if(tower)
            
            } // dphi

        } // deta


        // Set candidate preshower sum
        candidate->SetBPrsEnergy(energy);
        
        // Associate tracks falling within a cone of mRadius around the candidate
        for (int k = 0; k < mGammaEvent->numberOfTracks(); ++k) 
        {
    
            if (StGammaTrack* track = mGammaEvent->track(k)) 
            {
            
                float deta = cluster->momentum().Eta() - track->eta();
                float dphi = cluster->momentum().Phi() - track->phi();
                dphi = TVector2::Phi_mpi_pi(dphi);
                float r = hypot(deta, dphi);
                
                if(r <= mRadius) 
                {
                    candidate->addTrack(track);
                    if(!track->candidates.FindObject(candidate)) track->candidates.Add(candidate);
                }
                
            }
            
        }

        // Associate towers falling within a cone of radius mRadius around the candidate
        for(int k = 0; k < mGammaEvent->numberOfTowers(); ++k) 
        {
        
            StGammaTower* tower = mGammaEvent->tower(k);
            if(tower && tower->layer == kBEmcTower) 
            {
            
                float deta = cluster->momentum().Eta() - tower->eta;
                float dphi = cluster->momentum().Phi() - tower->phi;
                dphi = TVector2::Phi_mpi_pi(dphi);
                float r = hypot(deta, dphi);
                
                if(r <= mRadius) 
                {
                    candidate->addTower(tower);
                    if(!tower->candidates.FindObject(candidate)) tower->candidates.Add(candidate);
                    
                    // Associate tracks projecting to the tower with the tower
                    for(int t = 0; t < mGammaEvent->numberOfTracks(); ++t) 
                    {
                    
                        StGammaTrack* track = mGammaEvent->track(t);
                        if(!track) continue;
                        
                        TVector3 position = track->positionAtRadius(geom->Radius());
                        if(position != TVector3(0, 0, 0))
                        {
                            
                            int id;
                            if(geom->getId(position.Phi(), position.Eta(), id) == 0 && id == static_cast<int>(tower->id)) 
                            {
                                if(!tower->tracks.FindObject(track)) tower->tracks.Add(track);
                            }
                    
                        }
                    
                    }
                    
                }
                
            }
            
        }

        // Associate preshower hits falling within a cone of radius mRadius around the candidate
        for(int k = 0; k < mGammaEvent->numberOfPreshower1(); ++k) 
        {
        
            StGammaTower* preshower = mGammaEvent->preshower1(k);
            if(preshower && preshower->layer == kBEmcPres) 
            {
            
                float deta = cluster->momentum().Eta() - preshower->eta;
                float dphi = cluster->momentum().Phi() - preshower->phi;
                dphi = TVector2::Phi_mpi_pi(dphi);
                float r = hypot(deta, dphi);
                
                if (r <= mRadius) 
                {
                    candidate->addPreshower1(preshower);
                    if(!preshower->candidates.FindObject(candidate)) preshower->candidates.Add(candidate);
                }
                
            }
            
        }

        // Associate BSMDE and BSMDP strips falling within mSmdRange of the candidate
        float smdEtaEnergy = 0;
        float smdPhiEnergy = 0;
        
        int centralId = 0;
        int module = 0;
        int eta = 0;
        int sub = 0;

        smdEtaGeom->getId(cluster->position().Phi(), cluster->position().Eta(), centralId);         

        int sector = 0; // Dummy variable in the BEMC

        for(int dPhiStrip = -1; dPhiStrip <= 1; ++dPhiStrip)
        {

            for(int dEtaStrip = -20; dEtaStrip <= 20; ++dEtaStrip)
            {

                int id = emcPosition.getNextId(3, centralId, dEtaStrip, dPhiStrip);
                if(id == 0) continue;        

                smdEtaGeom->getBin(id, module, eta, sub);

                // Find vertex corrected position of the strip
                float x, y, z;
                smdEtaGeom->getXYZ(id, x, y, z);
                TVector3 vEta(x, y, z);

                float deta = cluster->position().Eta() - vEta.Eta();
                float dphi = cluster->position().Phi() - vEta.Phi();
                dphi = TVector2::Phi_mpi_pi(dphi);
                float r = hypot(deta, dphi);
            
                // If the strip is within range then associate the strip
                if(r <= mBsmdRange)
                {                      
                
                    // Use strip is already created in StGammaRawMaker
                    if(StGammaStrip* strip = mGammaRawMaker->strip(sector, kBEmcSmdEta, id)) 
                    {
                
                        candidate->addSmdEta(strip);
                        strip->candidates.Add(candidate);
                        smdEtaEnergy += strip->energy;
                
                    }
                    // Otherwise create a new, empty strip
                    else
                    {

                        float eta;

                        smdEtaGeom->getEta(id, eta);
             
                        strip = mGammaEvent->newStrip();
         
                        strip->index = id;
                        strip->sector = module;
                        strip->plane  = kBEmcSmdEta;
                        // strip->stat Filled in StGammaRawMaker::AddEtaStrip()
                        // strip->fail Filled in StGammaRawMaker::AddEtaStrip()
                        strip->energy = 0;
                        strip->position = 230.705 * sinh(eta);
         
                        double offset = fabs(eta) < 0.5 ? 0.73 : 0.94;
              
                        strip->left = strip->position - offset;
                        strip->right = strip->position + offset;

                        mGammaRawMaker->AddEtaStrip(strip);
            
                        candidate->addSmdEta(strip);
                        strip->candidates.Add(candidate);

                    } // if exist

                } // if range

            } // dEtaStrips

        } // dPhiStrips

        centralId = 0;
        smdPhiGeom->getId(cluster->position().Phi(), cluster->position().Eta(), centralId);
                        
        for(int dEtaStrip = -1; dEtaStrip <= 1; ++dEtaStrip)
        {
       	                
            for(int dPhiStrip = -20; dPhiStrip <= 20; ++dPhiStrip)
            {
            
                int id = emcPosition.getNextId(4, centralId, dEtaStrip, dPhiStrip);
                if(id == 0) continue;

                smdEtaGeom->getBin(id, module, eta, sub);

                // Find vertex corrected position of the strip
                float x, y, z;
                smdPhiGeom->getXYZ(id, x, y, z);
                TVector3 vPhi(x, y, z);

                float deta = cluster->position().Eta() - vPhi.Eta();
                float dphi = cluster->position().Phi() - vPhi.Phi();
                dphi = TVector2::Phi_mpi_pi(dphi);
                float r = hypot(deta, dphi);

                // If the strip is within range then associate the strip
                if(r < mBsmdRange)
                {

                    // Use strip is already created in StGammaRawMaker
                    if(StGammaStrip* strip = mGammaRawMaker->strip(sector, kBEmcSmdPhi, id)) 
                    {
            
                        candidate->addSmdPhi(strip);
                        strip->candidates.Add(candidate);
                        smdPhiEnergy += strip->energy;
                  
                    }
                    // Otherwise create a new, empty strip
                    else
                    {

                        float phi;

                        smdPhiGeom->getPhi(id, phi);

                        strip = mGammaEvent->newStrip();

                        strip->index = id;
                        strip->sector = module;
                        strip->plane  = kBEmcSmdPhi;
                        //strip->stat Filled in StGammaRawMaker::AddPhiStrip()
                        //strip->fail Filled in StGammaRawMaker::AddPhiStrip()
                        strip->energy = 0;
                        strip->position = phi;

                        double offset = 0.00293;

                        strip->left = phi - offset;
                        strip->right = phi + offset;

                        mGammaRawMaker->AddPhiStrip(strip);

                        candidate->addSmdPhi(strip);
                        strip->candidates.Add(candidate);

                    } // if exist
                  
                } // if range

            } // dEtaStrips

        } // dPhiStrips

        candidate->SetSmdEtaEnergy(smdEtaEnergy);
        candidate->SetSmdPhiEnergy(smdPhiEnergy);
        
    } // Clusters
    
    return kStOK;
    
}

//////////////////////////////////////////////////
//            Compile EEMC Clusters             //
//////////////////////////////////////////////////
Int_t StGammaCandidateMaker::MakeEndcap()
{

    // Retrieve all those gamma makers from the chain
    StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    if(!mGammaEventMaker)
    {
        LOG_WARN << "MakeEndcap() - No StGammaEventMaker found!" << endm;
        return kStWarn;
    }

    StGammaEvent *mGammaEvent = mGammaEventMaker->event();
    if(!mGammaEvent) 
    {
        LOG_WARN << "MakeEndcap() - No StGammaEvent found!" << endm;
        return kStWarn;
    }

    StGammaRawMaker *mGammaRawMaker = dynamic_cast<StGammaRawMaker*>(GetMakerInheritsFrom("StGammaRawMaker"));
    if(!mGammaRawMaker)
    {
        LOG_WARN << "MakeEndcap() - No StGammaRawMaker found!" << endm;
        return kStWarn;
    }
    
    StEEmcGenericClusterMaker *mEEclusters = dynamic_cast<StEEmcGenericClusterMaker*>(GetMakerInheritsFrom("StEEmcGenericClusterMaker"));
    if(!mEEclusters)
    {
        LOG_DEBUG << "MakeEndcap() - No StEEmcGenericClusterMaker found!" << endm;
        return kStWarn;
    }

    StEEmcA2EMaker *mEEanalysis = dynamic_cast<StEEmcA2EMaker*>(GetMakerInheritsFrom("StEEmcA2EMaker"));
    if(!mEEanalysis)
    {
        LOG_DEBUG << "MakeEndcap() - No StEEmcA2EMaker found!" << endm;
        return kStWarn;
    }


    // Loop over each sector, 
    for(Int_t sector = 0; sector < kEEmcNumSectors; sector++)
    {

        // then the clusters in each sector
        StEEmcClusterVec_t clusters = mEEclusters->clusters(sector, 0);
        for(UInt_t i = 0; i < clusters.size(); i++)
        {

            // Correct the cluster momentum for vertex
            StEEmcCluster cluster = clusters[i];
            TVector3 position = getEEmcClusterPosition(cluster);
            if(position == TVector3(0,0,0)) position = cluster.position();
            TVector3 pcluster = position - mGammaEvent->vertex();
            pcluster.SetMag(cluster.energy());
            
            // Require minimum transverse energy
            Float_t ET = pcluster.Perp();
            if(ET < mMinimumEt) continue;
            
            // Creat a new candidate
            StGammaCandidate *mCandidate = mGammaEvent->newCandidate();
            
            mCandidate->SetId( nextId() );
            mCandidate->SetTowerClusterId( cluster.key() );
            mCandidate->SetDetectorId( StGammaCandidate::kEEmc );
            
            // Set candidate kinematics
            mCandidate->SetMomentum( pcluster );
            mCandidate->SetPosition( cluster.position() );
            mCandidate->SetEnergy( cluster.energy() );
            
            // Set seed information
            StEEmcTower seed = cluster.tower(0);
            mCandidate->SetSeedEnergy( seed.energy() );
            mCandidate->SetTowerId( seed.index() );
	  

            // Associate towers, preshowers, and postshowers with the candidate
            // while summing up the total energy response in each
            Float_t epre1 = 0.;
            Float_t epre2 = 0.;
            Float_t epost = 0.;
            
            // Loop over neighboring towers
            for(Int_t j = 0; j < cluster.numberOfTowers(); j++)
            {
            
                // Grab the jth neighboring tower
                StEEmcTower tower = cluster.tower(j);
                Int_t index = tower.index();
                
                // and its pre/postshowers
                StEEmcTower pre1 = mEEanalysis->tower(index, 1);
                StEEmcTower pre2 = mEEanalysis->tower(index, 2);
                StEEmcTower post = mEEanalysis->tower(index, 3);
                
                // Increment the energy sums
                if( !pre1.fail() ) epre1 += pre1.energy();
                if( !pre2.fail() ) epre2 += pre2.energy();
                if( !post.fail() ) epost += post.energy();

                // Remember, if the respective detector element did not have
                // sufficient energy deposited in it then these pointers
                // will be null
                StGammaTower *gtower = mGammaRawMaker->tower(index, kEEmcTower);
                StGammaTower *gpre1  = mGammaRawMaker->tower(index, kEEmcPre1);
                StGammaTower *gpre2  = mGammaRawMaker->tower(index, kEEmcPre2);
                StGammaTower *gpost  = mGammaRawMaker->tower(index, kEEmcPost);
                
                
                // Associate clustered towers with the candidate
                if(gtower)
                {
                    mCandidate->addMyTower(gtower);
                    gtower->candidates.Add(mCandidate);
                }
            
                // Associate clustered preshower1 tiles with the candidate
                if(gpre1 && !pre1.fail() && pre1.energy() > 0.0) 
                {
                    mCandidate->addMyPreshower1(gpre1);
                    gpre1->candidates.Add(mCandidate);
                }
                
                // Associate clustered preshower2 tiles with the candidate
                if(gpre2 && !pre2.fail() && pre2.energy() > 0.0) 
                {
                    mCandidate->addMyPreshower2(gpre2);
                    gpre2->candidates.Add(mCandidate);
                }
                
                // Associate clustered postshower tiles with the candidate
                if(gpost && !post.fail() && post.energy() > 0.0) 
                {
                    mCandidate->addMyPostshower(gpost);
                    gpost->candidates.Add(mCandidate);
                }

                // Associate tracks extrapolating to the tower to the cluster
                if(gtower) 
                {
                
                    // Loop over all tracks in the event
                    for(int k = 0; k < mGammaEvent->numberOfTracks(); ++k) 
                    {
                    
                        StGammaTrack* track = mGammaEvent->track(k);
                        
                        // Skip misbehaving or null tracks
                        if (!track || track->pz() < 0) continue;
                        
                        // Extrapolate track to the SMD distance
                        EEmcGeomSimple& geom = EEmcGeomSimple::Instance();
                        TVector3 position = track->positionAtZ(geom.getZ1());
                    
                        if(position != TVector3(0, 0, 0))
                        {
                    
                            // If the tower to which the track extrapolates is equal
                            // to the current tower then associate track with cluster
                            int sector, subsector, etabin;
                            
                            if (geom.getTower(position, sector, subsector, etabin))
                            {
                            
                                bool match = gtower->sector() == sector;
                                match &= gtower->subsector() == subsector;
                                match &= gtower->etabin() == etabin;
                                
                                if(match)
                                {
                                    mCandidate->addMyTrack(track);
                                    track->candidates.Add(mCandidate);
                                }
                    
                            }
                            
                        }
                    
                    
                    } // Tracks
                    
                } // if(gtower)
                
            } // Cluster towers


            // Set preshower and postshower energy sums
            mCandidate->SetPre1Energy(epre1);
            mCandidate->SetPre2Energy(epre2);
            mCandidate->SetPostEnergy(epost);

            // Now associate tracks, towers, etc, lying within a cone
            // of mRadium around the candidate to the candidate
            // Only these objects will be stored in the StGammaEvent

            Float_t candidateEta = pcluster.Eta();
            Float_t candidatePhi = pcluster.Phi();
            
            // Check all tracks
            for(Int_t i = 0; i < mGammaEvent->numberOfTracks(); i++)
            {
            
                StGammaTrack *track = mGammaEvent->track(i);
                if (!track) continue;
                
                Float_t trackEta = track->eta();
                Float_t trackPhi = track->phi();
                Float_t dEta = trackEta - candidateEta;
                Float_t dPhi = TVector2::Phi_mpi_pi(trackPhi - candidatePhi);
                
                Float_t r = TMath::Sqrt(dPhi * dPhi + dEta * dEta);
                
                if(r <= mRadius) 
                {
                    mCandidate->addTrack(track);
                    if(!track->candidates.FindObject(mCandidate)) track->candidates.Add(mCandidate);
                }
            
            }

            // Check all towers
            for(Int_t i = 0; i < mGammaEvent->numberOfTowers(); i++)
            {
            
                StGammaTower *tower = mGammaEvent->tower(i);
                if(!tower) continue;
                
                Float_t towerEta = tower->eta;
                Float_t towerPhi = tower->phi;
                Float_t dEta = towerEta - candidateEta;
                Float_t dPhi = TVector2::Phi_mpi_pi(towerPhi - candidatePhi);
                
                Float_t r = TMath::Sqrt(dPhi * dPhi + dEta * dEta);
                
                if(r <= mRadius) 
                {
                    mCandidate->addTower(tower);
                    if(!tower->candidates.FindObject(mCandidate)) tower->candidates.Add(mCandidate);
                }
            
            }

            // Check all preshowers
            for(Int_t i = 0; i < mGammaEvent->numberOfPreshower1(); i++)
            {
            
                StGammaTower *tower = mGammaEvent->preshower1(i);
                if(!tower) continue;
                
                Float_t towerEta = tower->eta;
                Float_t towerPhi = tower->phi;
                Float_t dEta = towerEta - candidateEta;
                Float_t dPhi = TVector2::Phi_mpi_pi(towerPhi - candidatePhi);
                
                Float_t r = TMath::Sqrt(dPhi * dPhi + dEta * dEta);
                
                if(r <= mRadius) 
                {
                    mCandidate->addPreshower1(tower);
                    if(!tower->candidates.FindObject(mCandidate)) tower->candidates.Add(mCandidate);
                }
            
            }


            for(Int_t i = 0; i < mGammaEvent->numberOfPreshower2(); i++)
            {
            
                StGammaTower *tower = mGammaEvent->preshower2(i);
                if(!tower) continue;
            
                Float_t towerEta = tower->eta;
                Float_t towerPhi = tower->phi;
                Float_t dEta = towerEta - candidateEta;
                Float_t dPhi = TVector2::Phi_mpi_pi(towerPhi - candidatePhi);
                
                Float_t r = TMath::Sqrt(dPhi * dPhi + dEta * dEta);
            
                if(r <= mRadius) 
                {
                    mCandidate->addPreshower2(tower);
                    if(!tower->candidates.FindObject(mCandidate)) tower->candidates.Add(mCandidate);
                }
            
            }
    
            // Check all postshowers
            for(Int_t i = 0; i < mGammaEvent->numberOfPostshower(); i++)
            {
            
                StGammaTower *tower = mGammaEvent->postshower(i);
                if (!tower) continue;
                
                Float_t towerEta = tower->eta;
                Float_t towerPhi = tower->phi;
                Float_t dEta = towerEta - candidateEta;
                Float_t dPhi = TVector2::Phi_mpi_pi(towerPhi - candidatePhi);
                
                Float_t r = TMath::Sqrt(dPhi * dPhi + dEta * dEta);
                
                if(r <= mRadius) 
                    {
                    mCandidate->addPostshower(tower);
                    if(!tower->candidates.FindObject(mCandidate)) tower->candidates.Add(mCandidate);
                }
            
            }

            // Now associate SMD strips within mSMDRange of the cluster, being
            // care in how one defines the center from which mSMDRange is defined

            Float_t umin[kEEmcNumSectors], vmin[kEEmcNumSectors];
            Float_t umax[kEEmcNumSectors], vmax[kEEmcNumSectors];
            Float_t umid[kEEmcNumSectors], vmid[kEEmcNumSectors];
            Int_t ntow[kEEmcNumSectors];

            // First, find the geometric center of the tower cluster in each sector
            for(Int_t ii = 0; ii < 12; ii++)
            { 
                umin[ii] = 0.0; 
                vmin[ii] = 0.0; 
                umax[ii] = 0.0; 
                vmax[ii] = 0.0; 
                umid[ii] = -1.0; 
                vmid[ii] = -1.0; 
                ntow[ii] = 0; 
            }

            EEmcSmdMap *eemap = EEmcSmdMap::instance();
            for(Int_t itow = 0; itow < cluster.numberOfTowers(); itow++)
            {
            
                StEEmcTower tower = cluster.tower(itow);
                Int_t U, V;
                eemap->getMiddleU( tower.sector(), tower.subsector(), tower.etabin(), U );
                eemap->getMiddleV( tower.sector(), tower.subsector(), tower.etabin(), V );
                ntow[ tower.sector() ]++;
                umid[ tower.sector() ]+=0.5+(Float_t)U;// from middle of strip
                vmid[ tower.sector() ]+=0.5+(Float_t)V;
            
            }


            for(Int_t isec = 0; isec < 12; isec++)
            {
            
                if(ntow[isec]) 
                { 
                    umid[isec] /= ntow[isec]; 
                    vmid[isec] /= ntow[isec]; 
                    umin[isec] = TMath::Max(umid[isec] - mEsmdRange * 2.0, 0. );
                    vmin[isec] = TMath::Max(vmid[isec] - mEsmdRange * 2.0, 0. );
                    umax[isec] = TMath::Min(umid[isec] + mEsmdRange * 2.0, 287. );
                    vmax[isec] = TMath::Min(vmid[isec] + mEsmdRange * 2.0, 287. );
                }
                
            }

            // Now associate SMD strips within mSMDRange of the center
            for(Int_t isec = 0; isec < 12; isec++)
            {
            
                if(!ntow[isec]) continue;
                
                // Start with the U plane
                for(Int_t i = (Int_t)umin[isec]; i < (Int_t)umax[isec]; i++)
                {
                
                    StGammaStrip *strip = mGammaRawMaker->strip(isec, 0, i);
                    if(strip) 
                    {
                        strip->position = i;
                        mCandidate->addSmdu(strip);
                        strip->candidates.Add(mCandidate);
                    }
                    
                }
                
                // Finish with the V plane
                for(Int_t i = (Int_t)vmin[isec]; i < (Int_t)vmax[isec]; i++)
                {
                
                    StGammaStrip *strip = mGammaRawMaker->strip(isec,1,i);
                    if(strip) 
                    {
                    strip->position = i;
                    mCandidate->addSmdv(strip);
                    strip->candidates.Add(mCandidate);
                    }
                
                }
                
            } // Sectors (SMD)

            // Sum SMDU energy
            float smduEnergy = 0;
            
            for(int i = 0; i < mCandidate->numberOfSmdu(); ++i)
            {
                StGammaStrip* strip = mCandidate->smdu(i);
                smduEnergy += strip->energy;
            }
            
            mCandidate->SetSmduEnergy(smduEnergy);
            
            // Sum SMDV energy
            float smdvEnergy = 0;
            for (int i = 0; i < mCandidate->numberOfSmdv(); ++i)
            {
                StGammaStrip* strip = mCandidate->smdv(i);
                smdvEnergy += strip->energy;
            }
            
            mCandidate->SetSmdvEnergy(smdvEnergy);

        } // Clusters

    } // Sectors (Clusters)

    // Run the gamma fitter
    for(int i = 0; i < mGammaEvent->numberOfCandidates(); ++i) 
    {
    
        StGammaCandidate* candidate = mGammaEvent->candidate(i);
        if (candidate->detectorId() == StGammaCandidate::kEEmc) 
        {
            StGammaFitterResult ufit;
	    StGammaFitterResult vfit;
            int ustatus = StGammaFitter::instance()->fit(candidate, &ufit, 0);
            if(ustatus == 0) candidate->SetSmdFit(ufit,0);
	    int vstatus = StGammaFitter::instance()->fit(candidate, &vfit, 1);
	    if(vstatus == 0) candidate->SetSmdFit(vfit,1);
        }
    
    }
    
    return kStOK;
    
}

//////////////////////////////////////////////////
//   Return the position of an EEMC Cluster     //
//////////////////////////////////////////////////
TVector3 StGammaCandidateMaker::getEEmcClusterPosition(const StEEmcCluster& cluster)
{

    // Get gamma raw maker
    StGammaRawMaker *mGammaRawMaker = dynamic_cast<StGammaRawMaker*>(GetMakerInheritsFrom("StGammaRawMaker"));
    if(!mGammaRawMaker)
    {
        LOG_WARN << "MakeEndcap() - No StGammaRawMaker found!" << endm;
        return TVector3(-999,-999,-999);
    }
    
    // Get cluster seed tower
    StEEmcTower seed = cluster.tower(0);
    
    // Get ranges of ESMD strips under seed tower
    int sector    = seed.sector();
    int subsector = seed.subsector();
    int etabin    = seed.etabin();
    int xmin[2], xmax[2];
    
    EEmcSmdMap::instance()->getRangeU(sector, subsector, etabin, xmin[0], xmax[0]);
    EEmcSmdMap::instance()->getRangeV(sector, subsector, etabin, xmin[1], xmax[1]);
    
    // Find max strips within ranges
    StGammaStrip* maxStrips[2] = {0, 0};
    for(int plane = 0; plane < 2; ++plane) 
    {
    
        for(int i = xmin[plane]; i <= xmax[plane]; ++i) 
        {
        
            StGammaStrip* strip = mGammaRawMaker->strip(sector, plane, i);
            if(strip) 
            {
                if(!maxStrips[plane] || strip->energy > maxStrips[plane]->energy) 
                {
                    maxStrips[plane] = strip;
                }
            }
            
        }
        
    }

    // Get intersection of max strips and check that it lies within
    // the fiducial volume of the seed tower.
    if(maxStrips[0] && maxStrips[1]) 
    {
    
        TVector3 position = EEmcSmdGeom::instance()->getIntersection(sector, maxStrips[0]->index, maxStrips[1]->index);
        int sec, sub, eta;
        
        bool success = position.z() != -999;
        success &= EEmcGeomSimple::Instance().getTower(position, sec, sub, eta);
        success &= sector == sec;
        success &= subsector == sub;
        success &= etabin == eta;
        
        if(success) return position;
        
    }
    
    // Failed to calculate cluster position using ESMD strips
    return TVector3(0, 0, 0);
  
}

//////////////////////////////////////////////////
// Clean up the StGammaEvent, dropping detector //
//    objects not associated with a candidate   //
//////////////////////////////////////////////////
Int_t StGammaCandidateMaker::Compress()
{

    // No compression
    if(mCompressLevel == 0) return kStOk;

    // Retrieve the StGammaEvent via StGammaEventMaker
     StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    if(!mGammaEventMaker)
    {
        LOG_WARN << "Compress() - No StGammaEventMaker found!" << endm;
        return kStWarn;
    }

    StGammaEvent *mGammaEvent = mGammaEventMaker->event();
    if(!mGammaEvent) 
    {
        LOG_WARN << "Compress() - No StGammaEvent found!" << endm;
        return kStWarn;
    }

    Compress<StGammaStrip>(mGammaEvent->mStrips);
    
    // Compress SMD strips only
    if(mCompressLevel == 1) return kStOk;  
    
    // Drop strips, tracks, towers without candidates
    Compress<StGammaTrack>(mGammaEvent->mTracks);
    Compress<StGammaTower>(mGammaEvent->mTowers);
    Compress<StGammaTower>(mGammaEvent->mPreshower1);
    Compress<StGammaTower>(mGammaEvent->mPreshower2);
    Compress<StGammaTower>(mGammaEvent->mPostshower);
    
    // Compress all
    if(mCompressLevel == 2) return kStOk;
    
    LOG_WARN << Form("Unknown compression level (%d)", mCompressLevel) << endm;
    
    return kStWarn;
    
}

//////////////////////////////////////////////////
//  Template function for dropping objects in   //
//   the TClonesArray without an association    //
//             to a StGammaCandidate            //
//////////////////////////////////////////////////
template<class T>
void StGammaCandidateMaker::Compress(TClonesArray* clones)
{
    TIter next(clones);
    while (T* x = (T*)next()) if (x->candidates.IsEmpty()) clones->Remove(x);
    clones->Compress();
}
