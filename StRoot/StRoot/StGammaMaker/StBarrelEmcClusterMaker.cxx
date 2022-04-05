#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaRawMaker.h"
#include "StBarrelEmcCluster.h"

#include "StBarrelEmcClusterMaker.h"

using namespace std;

ClassImp(StBarrelEmcClusterMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StBarrelEmcClusterMaker::StBarrelEmcClusterMaker(const char *name): StMaker(name)
{
    mHighTowerThreshold = 3.6;
    mClusterThreshold = 5.1;
}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
int StBarrelEmcClusterMaker::Init()
{

    // Retrieve StGammaEventMaker
    mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    assert(mGammaEventMaker);
    
    // Retrieve StGammaRawMaker
    mGammaRawMaker = dynamic_cast<StGammaRawMaker*>(GetMakerInheritsFrom("StGammaRawMaker"));
    assert(mGammaRawMaker);
    
    return StMaker::Init();
  
}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StBarrelEmcClusterMaker::Clear(Option_t* option)
{

    mVertex.SetXYZ(0,0,0);
    
    for(unsigned i = 0; i < mClusters.size(); ++i) 
    {
        delete mClusters[i];
        mClusters[i] = 0;
    }
    
    mClusters.clear();
    
    StMaker::Clear(option);
  
}

//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
int StBarrelEmcClusterMaker::Make()
{

    // Retrieve event vertex
    if (mGammaEventMaker->event()) mVertex = mGammaEventMaker->event()->vertex();
    
    // Search through BEMC for seed towers
    for(int id = 1; id <= 4800; ++id) 
    {
    
        if(StGammaTower* tower = mGammaRawMaker->tower(id, kBEmcTower)) 
        {
        
            // If a tower passes the seed energy threshold
            // then construct a candidate cluster
            if (tower->energy > mHighTowerThreshold) 
            {
            
                StBarrelEmcCluster* cluster = makeCluster(tower);
                
                // If the clustering was successful and the
                // cluster passes the energy threshold 
                // then store the cluster
                if(cluster && cluster->energy() > mClusterThreshold) 
                {
                    mClusters.push_back(cluster);
                }
                
            } // if(seedThreshold)
          
        } // if(tower)
        
    } // Towers


    LOG_DEBUG << "Make() - Number of BEMC clusters: " << mClusters.size() << endm;

    for(unsigned int i = 0; i < mClusters.size(); ++i) 
    {
        LOG_DEBUG << "---------- BEMC CLUSTER #" << i << " ----------" << endm;
        LOG_DEBUG << *mClusters[i] << endm;
    }

    return kStOk;
}

//////////////////////////////////////////////////
//    Construct a cluster consisting of the     //
//     3x3 patch of towers surrounding the      // 
//              input seed tower                //
//                                              //
//     The cluster energy is the sum of the     //
//  energies of all towers in the cluster, and  //
// the clsuter position is the energy weighted  //
//         centroid of all the towers.          //
//////////////////////////////////////////////////
StBarrelEmcCluster* StBarrelEmcClusterMaker::makeCluster(StGammaTower* tower) const
{

    // Instantiate a copy of StEmcPosition to find
    // the towers neighboring the seed tower
    StEmcPosition emcPosition;

    // Store useful tower info
    int id = tower->id;
    float energy = tower->energy; 

    // Calculate the momentum of the tower energy depostion
    TVector3 position;
    getTowerPosition(id, position);
    position *= energy;

    // Create a candidate cluster
    StBarrelEmcCluster* cluster = new StBarrelEmcCluster;
    cluster->setSeed(tower);

    // Loop over eta neighbors
    for (int deta = -1; deta <= 1; ++deta) 
    {
    
        // Loop over phi neighbors
        for (int dphi = -1; dphi <= 1; ++dphi) 
        {
        
            // Skip the seed tower
            if(deta == 0 && dphi == 0) continue;
        
            // Determine the neighbor softId
            int neighborId = emcPosition.getNextTowerId(id, deta, dphi);
            
            // Grab the StGammaTower corresponding to the neighboring tower
            if(StGammaTower* neighbor = mGammaRawMaker->tower(neighborId, kBEmcTower)) 
            {
            
                // If the neighbor energy is less than the seed energy
                // then add the neighboring tower to the cluster, otherwise
                // the seed is no longer valid so stop the clustering
                // and delete the instantiated cluster
                if (tower->energy > neighbor->energy) 
                {
                
                    cluster->setTower(deta, dphi, neighbor);
                    TVector3 neighborPosition;
                    getTowerPosition(neighborId, neighborPosition);
                    position += neighborPosition * neighbor->energy;
                    energy += neighbor->energy;
                }
                else 
                {
                    delete cluster;
                    return 0;
                }
                
            } // if(neighbor)
            
        } // dPhi
        
    } // dEta

    // Normalize the cluster centroid
    position *= 1.0 / energy;
    
    // Set energy and position
    cluster->setEnergy(energy);
    cluster->setPosition(position);
    
    // Calculate momentum, correcting for the vertex
    TVector3 momentum = position - mVertex;
    momentum.SetMag(energy);
    cluster->setMomentum(momentum);
    
    return cluster;
    
}

//////////////////////////////////////////////////
//      Retrieve the position of the tower      //
//            with the given softId             // 
//////////////////////////////////////////////////
void StBarrelEmcClusterMaker::getTowerPosition(int id, TVector3& position) const
{
    float x, y, z;
    StEmcGeom::instance("bemc")->getXYZ(id, x, y, z);
    position.SetXYZ(x, y, z);
}
