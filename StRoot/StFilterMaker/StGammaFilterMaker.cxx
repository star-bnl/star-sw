// STL
#include <vector>

// ROOT Libraries
#include "TVector3.h"

// StEvent Libraries
#include "StEvent.h"

// StEvent Libraries
#include "StEventTypes.h"

// StMcEvent Libraries
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcEvent.hh"

// StEmc Libraries
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/others/emcDetectorName.h"

// Endcap Libraries
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

// Geant Libraries
#include "tables/St_g2t_vertex_Table.h"

// Class Header
#include "StGammaFilterMaker.h"

ClassImp(StGammaFilterMaker)

////////////////////////////////////////////////////////////
//                      Constructor                       //
////////////////////////////////////////////////////////////
StGammaFilterMaker::StGammaFilterMaker(const char *name):
StMaker(name)
{

    LOG_DEBUG << "StGammaFilterMaker()" << endm;

    // Set reasonable defaults
    mUseBemc = 1;
    
    mMaxVertex = 80;
    mSeedEnergyThreshold = 3.06;
    mClusterEtThreshold = 4.335;
    
    mMomentum = new TVector3();
    
    ////////////////////////////////////////
    //       Calorimeter Geometries       //
    ////////////////////////////////////////
    
    // BEMC Geometry   
    mBemcGeom = StEmcGeom::instance("bemc");
    assert(mBemcGeom);
    
    mPosition = new StEmcPosition();
    assert(mPosition);
    
    // Zero counters
    mTotal = 0;
    mAccepted = 0;
    mVertexRejected = 0;
    mFilterRejected = 0;
    
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
    
    // If using the EEMC ensure that a sampling 
    // fraction has been entered by the user 
    if(!mUseBemc) assert(mEemcSamplingFraction > 0);
    
    // Display maker information
    if(mUseBemc)
    {
        LOG_INFO  << "Init() - Using gamma filter on the BEMC" << endm;
    }
    else
    {
        LOG_INFO << "Init() : Using gamma filter on the EEMC" << endm;
        LOG_INFO << "Init() : EEMC Sampling Fraction = " << mEemcSamplingFraction << endm;
    }
    
    LOG_INFO << "Init() : Seed energy threshold = " << mSeedEnergyThreshold << " GeV " << endm;
    LOG_INFO << "Init() : Cluster eT threshold = " << mClusterEtThreshold << " GeV " << endm;
    LOG_INFO << "Init() : Maximum vertex = +/- " << mMaxVertex << " cm" << endm;
    
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
    
}

////////////////////////////////////////////////////////////
//               Apply filter to the BEMC                 //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::useBEMC()
{
    LOG_DEBUG << "useBEMC()" << endm;
    mUseBemc = 1;
}

////////////////////////////////////////////////////////////
//               Apply filter to the EEMC                 //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::useEEMC()
{
    LOG_DEBUG << "useEEMC()" << endm;
    mUseBemc = 0;
}

////////////////////////////////////////////////////////////
//       Set maximum vertex allowed by the filter         //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::setMaxVertex(double maxVertex)
{
    LOG_DEBUG << "setMaxVertex()" << endm;
    mMaxVertex = maxVertex;
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
//               Set EEMC sampling fraction               //
////////////////////////////////////////////////////////////
void StGammaFilterMaker::setEEMCSamplingFraction(double f)
{
    mEemcSamplingFraction = f;
}

////////////////////////////////////////////////////////////
//  Calculate multiplier between energy deposited in the  //
//    BEMC sampling calorimeter and the true particle     //
//   energy, i.e. the inverse of the sampling fraction,   //  
//          as a function of tower pseudorapidity         //
//                                                        //
//                Coeffiecients taken from                //
//   StEmcSimulatorMaker/StEmcSimpleSimulator.cxx v1.14   //
////////////////////////////////////////////////////////////
double StGammaFilterMaker::BEMCSamplingMultiplier(double eta)
{

    double x = fabs(eta);
    double c0 = 14.69;
    double c1 = -0.1022;
    double c2 = 0.7484;
    
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
    assert(mEvent);
    
    // Acquire StMcEvent from the chain
    StMcEvent *mMcEvent = dynamic_cast<StMcEvent*>(GetDataSet("StMcEvent"));
    assert(mMcEvent);
    
    ++mTotal;
    
    // Store the first event to ensure that a MuDst is created
    // in the case of no events passing the filter
    if(mEvent->id() == 1)
    {
        LOG_INFO << "Make() : Storing first event without applying filter" << endm;   
        
        ++mAccepted;
        return kStOK;
    }
    
    // Acquire vertex information from the geant record
    // and abort the event if the vertex is too extreme
    double zVertex = 0;
    
    St_DataSet *geant = StMaker::GetChain()->GetDataSet("geant");
    assert(geant);
    St_g2t_vertex *vertex = (St_g2t_vertex*)geant->Find("g2t_vertex");
    if(vertex) 
    {
        g2t_vertex_st *vertexTable= vertex->GetTable();
        zVertex = vertexTable->ge_x[2];
    }
    
    if(fabs(zVertex) > mMaxVertex)
    {
        LOG_INFO << "Make() : Aborting Event " << mEvent->id()
                 << " due to extreme geant vertex of " << zVertex << " cm " << endm;
        
        ++mVertexRejected;
        return kStSKIP;
    }     
    
    // Look for clusters in the chosen calorimeter
    Int_t status = 0;
    
    // Pick up geant calorimeter depositions from StMcEvent
    StMcEmcHitCollection *mcEmcCollection = mMcEvent->bemcHitCollection();
    assert(mcEmcCollection);
    status = makeBEMC(mcEmcCollection, zVertex);
    
    if(status == kStSKIP)
    {
        ++mFilterRejected;
        LOG_INFO << "Make() : Aborting Event " << mEvent->id() << " due to gamma filter rejection" << endm;
        LOG_DEBUG << "Make() : Vertex = " << zVertex << " (cm)" << endm;
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
    
    LOG_INFO << "Finish() : " << GetName() << " finishing with " << endm;
    LOG_INFO << "Finish() : \t" << mAccepted << " of " << mTotal << " events passing the filter" << endm;
    LOG_INFO << "Finish() : \t" << mVertexRejected << " rejected for bad vertex" << endm;
    LOG_INFO << "Finish() : \t" << mFilterRejected << " rejected for no clusters" << endm;
    
    return kStOK;
}
  
