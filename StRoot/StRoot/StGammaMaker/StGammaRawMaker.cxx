#include "StMessMgr.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StEventTypes.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"

#include "StGammaRawMaker.h"

ClassImp(StGammaRawMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaRawMaker::StGammaRawMaker(const char *name): StMaker(name)
{

    SetTowerCutoff(0.0);
    SetTrackCutoff(0.0);
    
    mUseBemc = false;
    mUseEemc = false;

    mBemcGainShift = 0.0;
    
}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaRawMaker::~StGammaRawMaker()
{}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaRawMaker::Init()
{

    mEEmcGeometry = new EEmcGeomSimple();
    mTables = new StBemcTables();
    mCorrupt = false;
    
    return StMaker::Init();
    
}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StGammaRawMaker::Clear(Option_t *opts)
{

    // Clear detector object vectors
    mTracks.clear();
    mTowers.clear();
    mStrips.clear();
    mPreshower1.clear();
    mPreshower2.clear();
    mPostshower.clear();

    // for efficiency, we could clear only those elements which have been "hit"
    for (Int_t index = 0; index < kEEmcNumSectors * kEEmcNumSubSectors * kEEmcNumEtas; index++)
    {
    
        for(Int_t layer = 0; layer < 4; layer++)
        {
        
            mEEtowers[index][layer] = 0;

            for(Int_t sec = 0; sec < kEEmcNumSectors; sec++)
            {
    
                for(Int_t plane = 0; plane < kEEmcNumSmdUVs; plane++)
                {
      
                    for(Int_t strip = 0; strip < kEEmcNumStrips; strip++)
                    {
                    
                        mEEstrips[sec][plane][strip] = 0;
	                   
                    }
	               
                } // Planes
            
            } // Sectors
        
        } // Layers
    
    } // Index

    // Clear BTOW and BPRS arrays of pointers
    memset(mBarrelEmcTower, 0, sizeof(mBarrelEmcTower));
    memset(mBarrelEmcPreshower, 0, sizeof(mBarrelEmcPreshower));
    
    // Clear BSMDE and BSMDP maps
    mBarrelSmdEtaStrip.clear();
    mBarrelSmdPhiStrip.clear();
    
}

//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaRawMaker::Make()
{

    mMuDstMaker = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
    if(!mMuDstMaker)
    {
        LOG_WARN << "Make - No MuDst found!" << endm;
        return kStWarn;
    }

    // Retrieve StGammaEventMaker from the chain
    mGammaEvent = 0;
    
    if(!GetMaker("mGammaEventMaker")) 
    {
        LOG_WARN << "StGammaEventMaker not in chain, no tree for you" << endm;
        return kStWarn;
    }
    else 
    {
        StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
        mGammaEvent = mGammaEventMaker->event();
    }
    
    // Retrieve MuDst
    if(!GetDataSet("MuDst")) 
    {
        LOG_WARN << "No MuDst" << endm;
        return kStWarn;
    }
    
    // Skip events without a reconstructed vertex
    if(!StMuDst::numberOfPrimaryVertices() ) return kStOK;

    GetTracks();
    if(mUseBemc) GetBarrel();
    if(mUseEemc) GetEndcap();

    return kStOk;
    
}

//////////////////////////////////////////////////
//    Retrieve (primary) track information      //
//////////////////////////////////////////////////
void StGammaRawMaker::GetTracks()
{ 

    // Retrieve MuDst
    if(!GetDataSet("MuDst")) 
    {
        LOG_WARN << "No MuDst" << endm;
        return;
    }

    // Loop over primary tracks and store in StGammaEvent
    
    //TIter next(StMuDst::globalTracks()); // do we want global tracks here?????
    TIter next(StMuDst::primaryTracks());
    
    while(StMuTrack* track = (StMuTrack*)next()) 
    {
    
        if(track->momentum().perp() < mTrackCutoff) continue;
    
        if(Accept(track)) 
        {
            StGammaTrack *mGammaTrack = mGammaEvent->newTrack(track);
            mTracks.push_back(*mGammaTrack);
        }
        
    }

}

//////////////////////////////////////////////////
//       Retrieve BEMC tower, preshower,        //
//             and SMD information              //
//////////////////////////////////////////////////
void StGammaRawMaker::GetBarrel()
{

    // Load status tables
    mTables->loadTables(this);
    
    // Retrieve BEMC detector collection 
    // Retrieve MuDst
    if(!GetDataSet("MuDst")) 
    {
        LOG_WARN << "No MuDst" << endm;
        return;
    }
    
    StEmcCollection *emc = 0;
    
    StEvent *event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
    emc = event ? event->emcCollection() : StMuDst::emcCollection();
    if(!emc) return;
    
    StEmcDetector* btow = emc->detector(kBarrelEmcTowerId);     // BTOW
    StEmcDetector* bprs = emc->detector(kBarrelEmcPreShowerId); // BPRS
    StEmcDetector* smde = emc->detector(kBarrelSmdEtaStripId);  // SMDE
    StEmcDetector* smdp = emc->detector(kBarrelSmdPhiStripId);  // SMDP
    
    // Check for corruption in the detectors
    mCorrupt = false;

    // StEmcDetector == NULL implies corruption for pre-October 2004 BEMC code
    if(!btow) 
    {
        mCorrupt = true;
        return;
    }
  
    // Check for corruption in post-October 2004 BEMC code
    // Crate corruption key: 
    //     crateUnknown = 0
    //     crateNotPresent = 1
    //     crateOK = 2
    //     crateHeaderCorrupt = 3
    for(int crate = 1; crate <= MAXCRATES; crate++) 
    {
        StEmcCrateStatus crateStatus = btow->crateStatus(crate);
        if(crateStatus == crateHeaderCorrupt) 
        {
            mCorrupt = true;
            return;
        }
    }
  
    /*
    //And now we can implement Alex's new StEmcAdc2EMaker test
    StEmcADCtoEMaker *adc2e = dynamic_cast<StGammaEventMaker*>GetMakerInheritsFrom("StEmcADCtoEMaker");
    if(adc2e)
    {
        mCorrupt = adc2e->isCorrupted();
        if(mCorrupt == true) return;
    }
    else
    {
        LOG_DEBUG << "GetBarrel() - StEmcADCtoEMaker not found in chain" << endl;
    }
    */
        

    // Instantiate Variables
    float pedestal;
    float rms;
    float bEta;
    float bPhi;
    int CAP=0; //this argument matters only for SMD


    // Loop over BEMC modules to access tower and preshower information
    for(int m = 1; m <= 120; m++) 
    { 
    
        // Retrieve tower information if the collection exists
        if(btow)
        {
            
            StEmcGeom *geom = StEmcGeom::instance("bemc"); 
            StEmcModule *module = btow->module(m);
            
            StSPtrVecEmcRawHit& rawHits = module->hits();
            
            //loop on hits in modules
            for(UInt_t k = 0; k < rawHits.size(); k++) 
            {
            
                StEmcRawHit* tempRawHit = rawHits[k];
                
                int id = tempRawHit->softId(BTOW);
 
                // Ignore excluded towers
                bool excludedTower = false;

                for(UInt_t i = 0; i < mExcludedBemcTowers.size(); ++i)
                {
                    if(id == mExcludedBemcTowers.at(i)) excludedTower = true;
                }

                if(excludedTower)
                {
                    LOG_DEBUG << " Excluding tower "<< id << endm;
                    continue;
                }

                int status;
                int ADC = tempRawHit->adc(); //not pedestal subtracted!
                double energy = tempRawHit->energy();
                float x, y, z;
                geom->getXYZ(id, x, y, z);
                TVector3 position(x, y, z);
                position -= TVector3(StMuDst::event()->primaryVertexPosition().xyz());
                bEta = position.Eta();
                bPhi = position.Phi();
                mTables->getStatus(BTOW, id, status);
                mTables->getPedestal(BTOW, id, CAP, pedestal, rms);

                // Require signal significantly above pedestal
                if(ADC < 7) continue;
                double pADC = ADC - pedestal;
                if(pADC < 5.0 * rms) continue;	
 
                // Vary energy per gain shift
                energy = (1.0 + mBemcGainShift) * energy;
                
                // min pT cut next
                if ( energy / TMath::CosH(bEta) < mTowerCutoff ) continue;
	
                StGammaTower *btower = mGammaEvent->newTower();
                
                btower->id     = id;
                btower->layer  = kBEmcTower;
                btower->stat   = status;
                btower->fail   = (int)(status != 1);
                btower->energy = energy;
                btower->eta    = bEta;
                btower->phi    = bPhi;

                LOG_DEBUG << " TowerID =" << id << ", Status = " << status << ", Energy = " << energy << endm;
                LOG_DEBUG << " Eta = " << bEta << ", Phi = " << bPhi << ", ADC = " << ADC << endm;


                mTowers.push_back(*btower);
                mBarrelEmcTower[btower->id] = btower;
                
            }
            
        }
    
        // Retrieve preshower information if the collection exists
        if(bprs)
        {
        
            StEmcGeom* geom = StEmcGeom::instance("bprs"); 
            StEmcModule* module = bprs->module(m);
            StSPtrVecEmcRawHit& rawHits=module->hits();
            
            // Loop over hits
            for(UInt_t k = 0; k < rawHits.size(); k++) 
            {
            
                StEmcRawHit* tempRawHit = rawHits[k];
                int id, bprs_status;
                int ADC = tempRawHit->adc();
                double energy = tempRawHit->energy();
                id = tempRawHit->softId(BPRS);
                float x, y, z;
                geom->getXYZ(id,x,y,z);
                TVector3 position(x, y, z);
                position -= TVector3(StMuDst::event()->primaryVertexPosition().xyz());
                bEta = position.Eta();
                bPhi = position.Phi();
                mTables->getStatus(BPRS, id, bprs_status);
                mTables->getPedestal(BPRS, id, CAP, pedestal, rms);

                // Require signal significantly above pedestal
                if(ADC < 15) continue;
                double pADC = ADC-pedestal;
                if ( pADC < 5.0 * rms ) continue;
                
                StGammaTower *mPreShower = mGammaEvent->newPreshower1();
            
                mPreShower->id     = id;
                mPreShower->layer  = kBEmcPres;
                mPreShower->stat   = bprs_status;
                mPreShower->fail   = (int)(bprs_status != 1);
                mPreShower->energy = energy;
                mPreShower->eta    = bEta;
                mPreShower->phi    = bPhi;

	            LOG_DEBUG << " BPRS ID =" << id << ", Status = " << bprs_status << ", Energy = " << energy << endm;
                LOG_DEBUG << " Eta = " << bEta << ", Phi = " << bPhi << ", ADC = " << ADC << endm;

                mPreshower1.push_back(*mPreShower);

            }
            
        }  

    } // Modules
  
    // Retrieve SMDE information if the collection exists
    if(smde) 
    {
    
        StEmcGeom* geom = StEmcGeom::instance("bsmde");
        
        for (UInt_t i = 1; i <= smde->numberOfModules(); i++) 
        {
        
            StEmcModule* module = smde->module(i);
            if (module) 
            {
            
                StSPtrVecEmcRawHit& hits = module->hits();
                
                for (size_t k = 0; k < hits.size(); ++k) 
                {
                
                    StEmcRawHit* hit = hits[k];
                    Int_t smde_id = hit->softId(BSMDE);

                    Int_t smde_status = 0;
                    mTables->getStatus(BSMDE,smde_id,smde_status);
                    mTables->getPedestal(BSMDE, smde_id, hit->calibrationType(), pedestal, rms); 

                    Float_t eta = -999;
                    geom->getEta(smde_id,eta);
                    
                    // Remove strips too close to pedestal 
                    if( (hit->adc() - pedestal) < 3.0 * rms) continue;

                    StGammaStrip *bstrip = mGammaEvent->newStrip();
                    
                    bstrip->index = smde_id;
                    bstrip->sector = hit->module();
                    bstrip->plane  = kBEmcSmdEta;
                    bstrip->stat   = smde_status;
                    bstrip->fail   = (int)(smde_status != 1);
                    bstrip->energy = hit->energy();
                    bstrip->adc    = hit->adc();
                    bstrip->position = 230.705 * sinh(eta);

                    double offset = fabs(eta) < 0.5 ? 0.73 : 0.94;
                
                    bstrip->right = bstrip->position + offset;
                    bstrip->left = bstrip->position - offset;

                    LOG_DEBUG << " eStrip ID =" << smde_id << ", Status = " << smde_status << endm;
                    LOG_DEBUG << " Energy = " << hit->energy() << ", Module = " << hit->module() << endm;

                    mStrips.push_back(*bstrip);
                    mBarrelSmdEtaStrip[bstrip->index] = bstrip;
                    
	           } // Hits

            } // if(module)
            
        } // Modules
    
    } // if(smde)

    // Retrieve SMDP information if the collection exists
    if(smdp) 
    {
    
        StEmcGeom* geom = StEmcGeom::instance("bsmdp");
        
        for (UInt_t i = 1; i <= smdp->numberOfModules(); i++) 
        {
    
            StEmcModule* module = smdp->module(i);
            if(module) 
            {
            
                StSPtrVecEmcRawHit& hits = module->hits();
                for (size_t k = 0; k < hits.size(); ++k) 
                {
                
                    StEmcRawHit* hit = hits[k];
                    Int_t smdp_id = hit->softId(BSMDP);

                    Int_t smdp_status = 0;
                    mTables->getStatus(BSMDP,smdp_id,smdp_status);
                    mTables->getPedestal(BSMDP, smdp_id, hit->calibrationType(), pedestal, rms);

                    Float_t phi = -999;
                    geom->getPhi(smdp_id,phi);
                    
                    // Remove strips too close to pedestal
                    if( (hit->adc() - pedestal) < 3.0 * rms) continue;

                    StGammaStrip *bstrip = mGammaEvent->newStrip();
                    
                    bstrip->index = smdp_id;
                    bstrip->sector = hit->module();
                    bstrip->plane  = kBEmcSmdPhi;
                    bstrip->stat   = smdp_status;
                    bstrip->fail   = (int)(smdp_status != 1);
                    bstrip->energy = hit->energy(); 
                    bstrip->adc    = hit->adc();
                    bstrip->position = phi;

                    double offset = 0.00293;

                    bstrip->left = phi - offset;
                    bstrip->right = phi + offset;
    
                    LOG_DEBUG << " pStrip ID =" << smdp_id << ", Status = " << smdp_status << endm;
                    LOG_DEBUG << " Energy = " << hit->energy() << ", Module = " << hit->module() << endm;

                    mStrips.push_back(*bstrip);
                    mBarrelSmdPhiStrip[bstrip->index] = bstrip;
                    
                } // Hits
 	          
            } // if(module)
        
        } // Modules
  
    } // if(smdp)
    
    return;
     	
}

//////////////////////////////////////////////////
//       Retrieve BEMC tower, preshower,        //
//       postshower, and SMD information        //
//////////////////////////////////////////////////
void StGammaRawMaker::GetEndcap()
{


    // Retrieve vertex information
    StMuPrimaryVertex *primaryVertex = mMuDstMaker->muDst()->primaryVertex();
    if(!primaryVertex) return; // we're going to need tracking, therefore vertex
    
    Float_t zvertex = primaryVertex->position().z();
    
    // Retrieve StEEmcA2eMaker
    StEEmcA2EMaker *adc2e = dynamic_cast<StEEmcA2EMaker*>(GetMakerInheritsFrom("StEEmcA2EMaker"));
    if (!adc2e)
    {
      LOG_WARN << "GetEndcap() - StEEmcA2EMaker not found!" << endm;
      return;
    }

    // Define depths for determining the momentum of each detector element
    const Float_t depths[] = 
    {
        kEEmcZSMD,   // Towers go to SMD depth
        kEEmcZPRE1,  // Pre/postshowers go to their own depth
        kEEmcZPRE2,
        kEEmcZPOST
    };

    Int_t enumerations[] = {kEEmcTower, kEEmcPre1, kEEmcPre2, kEEmcPost };

    // Loop over endcap layers
    for(Int_t layer = 0; layer < 4; layer++)
    {
    
        // Loop over hits in the current layer
        for (Int_t ihit = 0; ihit < adc2e->numberOfHitTowers(layer); ihit++)
        {
        
            StEEmcTower tower = adc2e->hittower(ihit, layer);	  
            
            UInt_t sec = (UInt_t)tower.sector();
            UInt_t sub = (UInt_t)tower.subsector();
            UInt_t eta = (UInt_t)tower.etabin();
            
            TVector3 center = mEEmcGeometry->getTowerCenter(sec,sub,eta);
            Float_t  depth = depths[layer];
            center.SetMag( depth/center.CosTheta() );

            Float_t R        = center.Perp();
            Float_t Z        = depth-zvertex;
            Float_t TanTheta = R/Z;
            Float_t Phi      = center.Phi();

            TVector3 Momentum;
            Momentum.SetMagThetaPhi(tower.energy(), TMath::ATan(TanTheta), Phi );
            
            StGammaTower  *etower = 0;
            
            if(layer == 0)       etower = mGammaEvent->newTower();
            else if (layer == 1) etower = mGammaEvent->newPreshower1();
            else if (layer == 2) etower = mGammaEvent->newPreshower2();
            else if (layer == 3) etower = mGammaEvent->newPostshower();
            
            etower->id     = tower.index();
            etower->layer  = enumerations[ tower.layer() ];
            etower->stat   = tower.stat();
            etower->fail   = tower.fail();
            etower->energy = tower.energy();
            etower->eta    = Momentum.Eta();
            etower->phi    = Momentum.Phi();

            if( Accept(*etower) ) 
            {
            
                if(layer == 0) 
                {
                
                    if( etower->energy / TMath::CosH(etower->eta) > mTowerCutoff )
                    {
                        mTowers.push_back(*etower);
                        mEEtowers[ etower->id ][ etower->layer ] = etower;
                    }
                    
                }
                else if(layer == 1) 
                {
                    mPreshower1.push_back(*etower);
                    mEEtowers[ etower->id ][ etower->layer ] = etower;
                }
                else if(layer == 2)
                {
                    mPreshower2.push_back(*etower);
                    mEEtowers[ etower->id ][ etower->layer ] = etower;
                }
                else if(layer == 3)
                {
                    mPostshower.push_back(*etower);
                    mEEtowers[ etower->id ][ etower->layer ] = etower;
                }
                else continue; 
                
            } // if(Accept)

    	} // Hits
    
    } // Layers

    // Loop over SMD strips
    Int_t smdenum[] = {kEEmcSmdu, kEEmcSmdv};
    
    for (Int_t sector = 0; sector < 12; sector++)
    {
    
        for(Int_t plane = 0; plane < 2; plane++)
        {
        
            for (Int_t ihit = 0; ihit < adc2e->numberOfHitStrips(sector, plane); ihit++)
            {
            
                StEEmcStrip strip = adc2e->hitstrip(sector, plane, ihit);
                
                StGammaStrip *gstrip = mGammaEvent->newStrip();
                gstrip->index = strip.index();
                gstrip->sector = strip.sector();
                gstrip->plane  = smdenum[ strip.plane() ];
                gstrip->stat   = strip.stat();
                gstrip->fail   = strip.fail();
                gstrip->energy = strip.energy();
                
                mEEstrips[ gstrip->sector ][ gstrip->plane ][ gstrip->index ]= gstrip;
                
                if( Accept(*gstrip) )
                {
                    mStrips.push_back(*gstrip);
                }
        
            } // Hits

        } // Planes
        
    } // Sectors

}

//////////////////////////////////////////////////
//         Check StGammaTrack behavior          //
//////////////////////////////////////////////////
Bool_t StGammaRawMaker::Accept(StGammaTrack &track)
{
    return true;
}

//////////////////////////////////////////////////
//         Check StGammaTower behavior          //
//////////////////////////////////////////////////
Bool_t StGammaRawMaker::Accept( StGammaTower &tower )
{

  if(tower.fail) return false;

  return true;
  
}

//////////////////////////////////////////////////
//         Check StGammaStrip behavior          //
//////////////////////////////////////////////////
Bool_t StGammaRawMaker::Accept( StGammaStrip &strip )
{

  if(strip.fail) return false;

  return true;
    
}

//////////////////////////////////////////////////
//           Check StMuTrack behavior           //
//////////////////////////////////////////////////
Bool_t StGammaRawMaker::Accept( StMuTrack* track )
{

    return (track->flag() > 0        && // Good fit
            track->flag() / 100 != 7 && // FTPC only
            track->flag() / 100 != 8 && // FTPC+primary vertex
            track->flag() / 100 != 9 && // Beam background
            (double)track->nHitsFit() / (double)track->nHitsPoss() > 0.51 );
          
}


//////////////////////////////////////////////////
//         Return pointer to tower with         //
//             given id and layer               //
//                                              //
//   0 - Endcap Tower                           //
//   1 - Endcap Preshower 1                     //
//   2 - Endcap Preshower 2                     //
//   3 - Endcap Postshower                      //
//                                              //
//  10 - Barrel Tower                           //
//  11 - Barrel Preshower                       //
//                                              //
//////////////////////////////////////////////////
StGammaTower *StGammaRawMaker::tower(Int_t id, Int_t layer)
{

    if (layer >= kEEmcTower && layer <= kEEmcPost)
    {
        return mEEtowers[id][layer];
    }
    else if(layer == kBEmcTower)
    {
        return mBarrelEmcTower[id];
    }
    else if(layer == kBEmcPres)
    {
        return mBarrelEmcPreshower[id];
    }
    
    return 0;
  
}

//////////////////////////////////////////////////
//         Return pointer to strip with         //
//        given sector, plane, and index        //
//                                              //
//   Plane:                                     //
//   1 - ESMD U Plane                           //
//   2 - ESMD V Plane                           //
//                                              //
//  10 - BSMD Eta Plane                         //
//  11 - BSMD Phi Plane                         //
//                                              //
//////////////////////////////////////////////////
StGammaStrip *StGammaRawMaker::strip(Int_t sec, Int_t plane, Int_t index)
{

    if(plane == kEEmcSmdu || plane == kEEmcSmdv)
    {
        return mEEstrips[sec][plane][index];
    }
    else if(plane == kBEmcSmdEta)
    {
        return mBarrelSmdEtaStrip[index];
    }
    else if(plane == kBEmcSmdPhi)
    {
        return mBarrelSmdPhiStrip[index];
    }
    
    return 0;
  
}

//////////////////////////////////////////////////
//      Add eta strip from external source      //
//////////////////////////////////////////////////
void StGammaRawMaker::AddEtaStrip(StGammaStrip *strip)
{

    int smdStatus = 0;

    mTables->getStatus(BSMDE, strip->index, smdStatus);

    strip->stat   = smdStatus;
    strip->fail   = (int)(smdStatus != 1);

    mStrips.push_back(*strip);
    mBarrelSmdEtaStrip[strip->index] = strip;

    return;

}

//////////////////////////////////////////////////
//      Add eta strip from external source      //
//////////////////////////////////////////////////
void StGammaRawMaker::AddPhiStrip(StGammaStrip *strip)
{

    int smdStatus = 0;
    
    mTables->getStatus(BSMDP, strip->index, smdStatus);
    
    strip->stat   = smdStatus;
    strip->fail   = (int)(smdStatus != 1);
 
    mStrips.push_back(*strip);
    mBarrelSmdPhiStrip[strip->index] = strip;

    return;

}
