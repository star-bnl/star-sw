// STL
#include <vector>

// ROOT Libraries
#include "TVector3.h"
#include "TFile.h"
#include "TTree.h"

// StEvent Libraries
#include "StEvent.h"

// StEvent Libraries
#include "StEventTypes.h"

// StMcEvent Libraries
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"

// StEmc Libraries
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/others/emcDetectorName.h"

// StMuDst Libraries
#include "StBFChain/StBFChain.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

// St_g2t Libraries
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"
#include "StSpinPool/StJetSkimEvent/StPythiaEvent.h"

// Class Header
#include "StGammaFilterMaker.h"

ClassImp(StGammaFilterMaker)

////////////////////////////////////////////////////////////
//                      Constructor                       //
////////////////////////////////////////////////////////////
StGammaFilterMaker::StGammaFilterMaker(const char *name): 
StMaker(name), mPythiaFile(0), mPythiaTree(0), mPythiaEvent(0)
{

    LOG_DEBUG << "StGammaFilterMaker()" << endm;

    mFirst = true;
    mPythiaName = "pythia.root";

    // 2009 Defaults
    mSeedEnergyThreshold = 5.0;
    mClusterEtThreshold = 6.55;

    // Zero counters
    mTotal = 0;
    mAccepted = 0;
    
    // Calorimeter Geometries
    
    mMomentum = new TVector3();
        
    // BEMC Geometry   
    mBemcGeom = StEmcGeom::instance("bemc");
    assert(mBemcGeom);
    
    mPosition = new StEmcPosition();
    assert(mPosition);

    for ( int ii=0;ii< nBemcTowers+1;ii++ )
      {
	mBemcTowerHits[ii] = 0;
      }

    
}

////////////////////////////////////////////////////////////
//                       Destructor                       //
////////////////////////////////////////////////////////////
StGammaFilterMaker::~StGammaFilterMaker()
{
    LOG_DEBUG << "~StGammaFilterMaker()" << endm;
    
    // Clean up
    mMomentum->Delete();
    delete mPosition;
    
}

////////////////////////////////////////////////////////////
//                      Maker Init                        //
////////////////////////////////////////////////////////////
Int_t StGammaFilterMaker::Init()
{

    LOG_DEBUG << "Init()" << endm;

    // Force the BFC to listen to the filter
    SetAttr(".Privilege", 1);
    
    LOG_INFO  << "Init() - Using gamma filter on the BEMC" << endm;
    
    LOG_INFO << "Init() : Seed energy threshold = " << mSeedEnergyThreshold << " GeV " << endm;
    LOG_INFO << "Init() : Cluster eT threshold = " << mClusterEtThreshold << " GeV " << endm;
    
    // Create pythia output file
    StBFChain *bfChain = dynamic_cast<StBFChain*>(GetMakerInheritsFrom("StBFChain"));
    if(bfChain)
    {
        TString name(bfChain->GetFileOut());
        name.ReplaceAll(".root",".pythia.root");
        mPythiaName = name;
    }    

    LOG_INFO << "Init() : Storing pythia event information in " << mPythiaName << endm;
    mPythiaEvent = new StPythiaEvent;
    mPythiaFile = new TFile(mPythiaName, "recreate");
    mPythiaTree = new TTree("pythiaTree", "Pythia Tree");
    mPythiaTree->Branch("PythiaEvents", "StPythiaEvent", &mPythiaEvent);

    return kStOK;
    
}

////////////////////////////////////////////////////////////
//                     Maker Clear                        //
////////////////////////////////////////////////////////////    
void StGammaFilterMaker::Clear(const Option_t*)
{
    LOG_DEBUG << "Clear()" << endm;

    // Clear the StEmcRawHit arrays
    StMcCalorimeterHit **point1 = mBemcTowerHits;
    for(unsigned int i = 0; i < nBemcTowers + 1; ++i)
    {
        *point1 = NULL;
        ++point1;
    }

    mPythiaEvent->Clear();
    
}

////////////////////////////////////////////////////////////
//                 Set filter thresholds                  //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::setThresholds(double seed, double cluster)
{

    LOG_DEBUG << "setThresholds()" << endm;
    
    if(seed > 0) mSeedEnergyThreshold = seed;
    if(cluster > 0) mClusterEtThreshold = cluster;
    
}

////////////////////////////////////////////////////////////
//  Calculate multiplier between energy deposited in the  //
//    BEMC sampling calorimeter and the true particle     //
//   energy, i.e. the inverse of the sampling fraction,   //  
//          as a function of tower pseudorapidity         //
//                                                        //
//        LOW_EM compatible coeffiecients taken from      //
//   StEmcSimulatorMaker/StEmcSimpleSimulator.cxx v1.16   //
////////////////////////////////////////////////////////////

double StGammaFilterMaker::BEMCSamplingMultiplier(double eta)
{

    double x = fabs(eta);
    double c0 = 14.365;
    double c1 = -0.512;
    double c2 = 0.668;
    
    return c0 + c1 * x + c2 * x * x;

}
////////////////////////////////////////////////////////////
//                      Apply filter                      //
////////////////////////////////////////////////////////////
Int_t StGammaFilterMaker::Make()
{

    LOG_DEBUG << "Make()" << endm;
    
    // Acquire StEvent from the chain
    StEvent *mEvent = dynamic_cast<StEvent*>(GetDataSet("StEvent"));
    if(!mEvent)
    {
     	LOG_WARN << "Make() - StEvent not found!" << endm;
        return kStWarn;
    }
    
    // Acquire StMcEvent from the chain
    StMcEvent *mMcEvent = dynamic_cast<StMcEvent*>(GetDataSet("StMcEvent"));
    
    ++mTotal;

    // Store the pythia event record
    fStorePythia();

    // Store the first event to ensure that a MuDst is created
    // in the case of no events passing the filter
    if(mFirst)
    {

        LOG_INFO << "Make() : Storing first event without applying filter" << endm;   
        
        ++mAccepted;
        mFirst = false;

        return kStOK;
    }    

    // Check for functional StMcEvent
    if(!mMcEvent)
    {
     	LOG_WARN << "Make() - Bad StMcEvent!" << endm;
        return kStWarn;
    }

    // Acquire vertex information from the geant record
    // and abort the event if the vertex is too extreme
    double zVertex = mMcEvent->primaryVertex()->position().z();

    // Look for clusters in the chosen calorimeter
    Int_t status = 0;
    
    // Pick up geant calorimeter depositions from StMcEvent
    StMcEmcHitCollection *mcEmcCollection = mMcEvent->bemcHitCollection();
    assert(mcEmcCollection);
    status = makeBEMC(mcEmcCollection, zVertex);
    
    if(status == kStSKIP)
    {
        LOG_INFO << "Make() : Aborting Event " << mEvent->id() << " due to gamma filter rejection" << endm;
    }
    
    if(status == kStOK)
    {
        ++mAccepted;
        LOG_INFO << "Make() : Event " << mEvent->id() << " accepted!" << endm;
    }
    
    return status;
 
}

////////////////////////////////////////////////////////////
//       Construct crude BEMC clusters of 3x3 towers,     // 
//        returning kStOK if a satisifactory cluster      // 
//             is found and kStSKIP otherwise             //
////////////////////////////////////////////////////////////

Int_t StGammaFilterMaker::makeBEMC(StMcEmcHitCollection *mcEmcCollection, double zVertex)
{

    LOG_DEBUG << "makeBEMC()" << endm;

    //////////////////////////////
    //   Instantiate variables  //
    //////////////////////////////
    
    // Tower identification
    int bemcId = 0;
    
    unsigned int m = 0;
    unsigned int e = 0;
    unsigned int s = 0;
    
    float eta = 0;

    // Tower position
    float x = 0;
    float y = 0;
    float z = 0;
    double r = 0;
    
    //////////////////////////////
    //    Fill mBemcTowerHits   //
    //       with hit data      //
    //////////////////////////////
    for(unsigned int m = 1; m < mcEmcCollection->numberOfModules() + 1; ++m)
    {
        
        const StMcEmcModuleHitCollection *module = mcEmcCollection->module(m);
        const vector<StMcCalorimeterHit*> hits = module->detectorHits();
        
        for(unsigned int l = 0; l < hits.size(); ++l)
        {
            mBemcGeom->getId(hits[l]->module(), hits[l]->eta(), hits[l]->sub(), bemcId);
            mBemcTowerHits[bemcId] = hits[l];
        }
        
    }
    
    //////////////////////////////
    //    Search for clusters   //
    //////////////////////////////
    double seedEnergy = 0;
    double clusterEnergy = 0;
    double clusterEt = 0;
    unsigned int neighborId = 0;
    StMcCalorimeterHit *neighborHit = NULL;
    
    for(unsigned int n = 1; n < nBemcTowers + 1; ++n)
    {
        
        StMcCalorimeterHit *hit = mBemcTowerHits[n];
        if(!hit) continue;
        
        mBemcGeom->getEta(n, eta);
        
        seedEnergy = hit->dE() * BEMCSamplingMultiplier(eta);
        
        // Start with a seed tower...
        if(seedEnergy > mSeedEnergyThreshold)
        {
        
            clusterEnergy = seedEnergy;
        
            m = hit->module();
            e = hit->eta();
            s = hit->sub();
        
            // and accumulate energy from the neighboring towers
            for(int i = -1; i < 2; ++i)
            {
                for(int j = -1; j < 2; ++j)
                {
                    
                    if( !i && !j ) 
                    {
                        LOG_DEBUG << "\t\tTower " << n << ", E = " << seedEnergy << " (Seed)" << endm;
                        continue;
                    }
                    
                    neighborId = mPosition->getNextId(1, m, (int)e, s, i, j);
                    if(neighborId < 1) continue;
                    if(neighborId > 4800) continue;
                    
                    neighborHit = mBemcTowerHits[neighborId];
                    
                    if(!neighborHit) continue;
                    mBemcGeom->getEta(neighborId, eta);
                    LOG_DEBUG << "\t\tTower " << neighborId << ", E = " << neighborHit->dE() * BEMCSamplingMultiplier(eta) << endm;
                    clusterEnergy += neighborHit->dE() * BEMCSamplingMultiplier(eta);
                    
                }
                
            }
            
            // Project cluster energy into transverse plane
            mBemcGeom->getXYZ((int)n, x, y, z);
            z -= zVertex;
            r = sqrt(x * x + y * y + z * z);
            x *= clusterEnergy / r;
            y *= clusterEnergy / r;
            z *= clusterEnergy / r;
            mMomentum->SetXYZ(x, y, z);
            clusterEt = mMomentum->Perp();

            LOG_DEBUG << "\tCluster Energy = " << clusterEnergy << " GeV" << endm;
            LOG_DEBUG << "\tCluster eT  (corrected for vertex) = " << clusterEt << " GeV" << endm;
            LOG_DEBUG << "\tCluster eta (corrected for vertex) = " << mMomentum->Eta() << endm;
            LOG_DEBUG << "\tCluster phi = " << mMomentum->Phi() << endm;
            LOG_DEBUG << "" << endm;
            
            // Apply filter
            if(clusterEt > mClusterEtThreshold)
            {
                return kStOk;
            }
            
        } // Seed tower check
        
    } // Seed tower loop
    
    return kStSKIP;
    
}
 
////////////////////////////////////////////////////////////
//                 Display filter stats                   //
////////////////////////////////////////////////////////////
Int_t StGammaFilterMaker::Finish()
{

    LOG_DEBUG << "::Finish()" << endm;

    mPythiaFile->Write();
    mPythiaFile->Close();
    
    LOG_INFO << "Finish() : " << GetName() << " finishing with " << endm;
    LOG_INFO << "Finish() : \t" << mAccepted << " of " << mTotal << " events passing the filter" << endm;
    
    return kStOK;

}
  
////////////////////////////////////////////////////////////
//                Store Pythia Event Record               //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::fStorePythia()
{

    TDataSet* geant = GetDataSet("geant");
    
    if(geant) 
    {
    
        TDataSetIter iter(geant);
    
        // Global event information
        St_g2t_event* g2tEvent = (St_g2t_event*)iter("g2t_event");
        if(g2tEvent)
        {

            g2t_event_st* eventTable = (g2t_event_st*)g2tEvent->GetTable();
            if(eventTable) 
            {
                mPythiaEvent->setRunId(eventTable->n_run);
                mPythiaEvent->setEventId(eventTable->n_event);
            }
    
        }

        // Pythia event information
        St_g2t_pythia* pythiaEvent = (St_g2t_pythia*)iter("g2t_pythia");
        if(pythiaEvent) 
        {
        
            g2t_pythia_st* pythiaTable = (g2t_pythia_st*)pythiaEvent->GetTable();
            if(pythiaTable) 
            {

                mPythiaEvent->setProcessId(pythiaTable->subprocess_id);
                mPythiaEvent->setS(pythiaTable->mand_s);
                mPythiaEvent->setT(pythiaTable->mand_t);
 	        mPythiaEvent->setU(pythiaTable->mand_u);
                mPythiaEvent->setPt(pythiaTable->hard_p);
                mPythiaEvent->setCosTheta(pythiaTable->cos_th);
        	mPythiaEvent->setX1(pythiaTable->bjor_1);
                mPythiaEvent->setX2(pythiaTable->bjor_2);
           
            }
    
        }

        // Vertex information
        St_g2t_vertex* vertexEvent = (St_g2t_vertex*)iter("g2t_vertex");
        if(vertexEvent) 
        {
     
            g2t_vertex_st* vertexTable = (g2t_vertex_st*)vertexEvent->GetTable();
            if(vertexTable) mPythiaEvent->setVertex(TVector3(vertexTable[0].ge_x));
        }

        // Pythia particle record
        St_particle* particleEvent = (St_particle*)iter("particle");
        if(particleEvent) 
        {

            particle_st* particleTable = (particle_st*)particleEvent->GetTable();
            if(particleTable) 
            {

                for(int i = 0; i < particleEvent->GetNRows(); ++i) 
                {
                    mPythiaEvent->addParticle(TParticle(particleTable[i].idhep,                  // pdg
                                                        particleTable[i].isthep,                 // status
                                                        particleTable[i].jmohep[0],              // mother1
                                                        particleTable[i].jmohep[1],              // mother2
                                                        particleTable[i].jdahep[0],              // daughter1
                                                        particleTable[i].jdahep[1],              // daughter2
                                                        TLorentzVector(particleTable[i].phep),   // momentum and energy
                                                        TLorentzVector(particleTable[i].vhep))); // production vertex and time
                }

           }

       }

    }

    mPythiaTree->Fill();

    return;

}
