//******************************************************************************
//
// StEmcEnergy.cxx
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/12/11
//
//******************************************************************************
//
//
//
//******************************************************************************
#include "StEmcEnergy.h"
#include "StEmcFilter.h"
#include "StEmcHadDE.h"
#include "StMcEvent.hh"
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StEventTypes.h"
#include "StEventUtilities/StuRefMult.hh"
#include "SystemOfUnits.h"
#include "StarClassLibrary/StParticleTable.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StEmcGeom.h"
#include "StEmcPosition.h"
#include <math.h>
#include <TFile.h>

ClassImp(StEmcEnergy)

//------------------------------------------------------------------------------
StEmcEnergy::StEmcEnergy():TObject()
{
  mInternalFilter=kTRUE;
	mEmcFilter = new StEmcFilter();
	setBfield(0.5);
  setQ0Factor(0.163);
  mBemcGeom = StEmcGeom::getEmcGeom("bemc");    
  setEval(evalOff);
}
//------------------------------------------------------------------------------
StEmcEnergy::~StEmcEnergy()
{
}
//------------------------------------------------------------------------------
void StEmcEnergy::processEvent()
{ 
 
  if ( mEvalMetd == evalOff)
  {
    energyInBtow();
    chHadEnergyInBtow();
    q0HadEnergyInBtow();
  }
 
  if ( mEvalMetd == metd1 ) 
  {
    mcEnergyInBtow();
  }
 
  if ( mEvalMetd == metd2 )
  {
    energyInBtow();
    hadDepFromMcTrackInBtow();
  }
    
  emEnergyInBtow();    
  processEnergy();
 
}
//------------------------------------------------------------------------------
void StEmcEnergy::energyInBtow()
{
  // Getting the energy in all towers

  for (UInt_t i=1; i<4800; i++) mEnergyInBtow[i]=0;
  
  StEmcCollection* emcCollection = mEvent->emcCollection();
  StEmcDetector* detector = emcCollection->detector(kBarrelEmcTowerId);

  Int_t m = 0, e = 0, s = 0;
  Int_t towerId = 0, towerNdx = 0;

  for (UInt_t i = 1; i < 121; i++) // The EMC modules
  {
    StSPtrVecEmcRawHit& emcTowerHits = detector->module(i)->hits();
    for (UInt_t j = 0; j < emcTowerHits.size(); j++)
    {
      m = (Int_t)emcTowerHits[j]->module();
      e = (Int_t)emcTowerHits[j]->eta();
      s = emcTowerHits[j]->sub();
			if(abs(m)<=120)
			{
      	mBemcGeom->getId(m, e, s, towerId);
      	towerNdx = towerId - 1;
      	if (mEmcFilter->getEmcStatus(1,towerId)==kGOOD) 
        	mEnergyInBtow[towerNdx] = emcTowerHits[j]->energy();
			}
    }
  }  
}
//------------------------------------------------------------------------------
void StEmcEnergy::chHadEnergyInBtow()
{  
  mNTracks=0;
	for (UInt_t i=1; i<4800; i++) mChHadEnergyInBtow[i]=0;

  StSPtrVecTrackNode& trackNodes = mEvent->trackNodes();

  mEmcFilter->setMustProjectEmc(kTRUE);
  
  StThreeVectorD  finalPosition, finalMomentum;
  Float_t         trackFinalEta, trackFinalPhi;

  UInt_t Nhadminus = uncorrectedNumberOfNegativePrimaries(*mEvent);

  StEmcPosition* emcPosition = new StEmcPosition();
  StEmcHadDE* emcHadDE = new StEmcHadDE();
  for (UInt_t i = 0; i < trackNodes.size(); i++)
  {    
    StTrack* track = trackNodes[i]->track(0);

    if (mEmcFilter->accept(track))
    {
      emcPosition->trackOnEmc(&finalPosition, &finalMomentum, track, mBfield);
      Int_t m = 0, e = 0, s = 0, id = 0;
      trackFinalEta = finalPosition.pseudoRapidity();
      trackFinalPhi = finalPosition.phi();
      mBemcGeom->getBin(trackFinalPhi, trackFinalEta, m, e, s);
      if(s==-1) s=1;
      if ( m != 0 && s > 0)
      {
        mBemcGeom->getId(m, e, s, id);
        if (mEmcFilter->getEmcStatus(1,id)==kGOOD)
        {  
          mNTracks++;
					Float_t dist;
          Int_t nTowersdEta = 0, nTowersdPhi = 0;
          Int_t towerId=0, towerNdx = -1;

          // Storing the hadronic energy to be subtracted from each tower
          // surrounding the track tower and track tower itself
          for (UInt_t j = 0; j < 5; j++)
          {
            nTowersdEta = j - 2;
            for (UInt_t k = 0; k < 5; k++)
            {
              nTowersdPhi = k - 2;
              dist = emcPosition->getDistTowerToTrack(trackFinalEta, trackFinalPhi, 
                                                   nTowersdEta, nTowersdPhi);
              if (dist <= 0.1 )
              {
                towerId = emcPosition->getNextTowerId(trackFinalEta, trackFinalPhi, 
                                                   nTowersdEta, nTowersdPhi);                                            
                towerNdx = towerId - 1;
                if (towerNdx != -1 && mEmcFilter->getEmcStatus(1,towerId)==kGOOD)
                {
                  Float_t tempDepEnergy = 0;
                  tempDepEnergy = emcHadDE->getDepEnergy(track, mBfield, nTowersdEta,
                                                         nTowersdPhi);
                  //tempDepEnergy*=0.88; // Checking if Et_em smaller is due to wrong had dep
                  if (tempDepEnergy >= 0)
                  {  
                    Float_t eff = trackEff(track, Nhadminus);
                    if(eff!=0) mChHadEnergyInBtow[towerNdx] += tempDepEnergy/eff;                    
                  }                    
                }
              }
            }
          }        
        }
      }
    }
  }
  delete emcPosition;
  delete emcHadDE;
}
//------------------------------------------------------------------------------
void StEmcEnergy::q0HadEnergyInBtow()
{ 
  for(Int_t i=0;i<4800;i++) mQ0HadEnergyInBtow[i]=mQ0Factor*mChHadEnergyInBtow[i];
}
//------------------------------------------------------------------------------
void StEmcEnergy::emEnergyInBtow()
{  
  for(Int_t i=0;i<4800;i++) mEmEnergyInBtow[i] = mEnergyInBtow[i] -
                                                 mChHadEnergyInBtow[i] -
                                                 mQ0HadEnergyInBtow[i];
}
//------------------------------------------------------------------------------
void StEmcEnergy::mcEnergyInBtow()
{ 
  for (UInt_t i=0; i<4800; i++) 
  {
    mEnergyInBtow[i]=0;
    mChHadEnergyInBtow[i]=0;
    mQ0HadEnergyInBtow[i]=0;
    mEmEnergyInBtow[i]=0;
  }

  StPtrVecMcTrack& tracks = mMcEvent->tracks();
  for (UInt_t i = 0; i < tracks.size(); i++)
  { 
    StPtrVecMcCalorimeterHit& bemcHits = tracks[i]->bemcHits(); 
    Float_t geantId = tracks[i]->geantId();
    StParticleDefinition* partDef = tracks[i]->particleDefinition();
    if (partDef) 
    {
      Double_t charge = partDef->charge();
      Int_t module = 0, eta = 0, sub = 0, towerId = 0, towerNdx = -1;
      for ( UInt_t j = 0; j < bemcHits.size(); j++ )
      { 
        module = bemcHits[j]->module();
        eta = bemcHits[j]->eta();
        sub = bemcHits[j]->sub();
        Float_t hitEnergy = dEToTotaldE( bemcHits[j], "bemc");
        mBemcGeom->getId( module, eta, sub, towerId );
        towerNdx = towerId - 1;
        if ( mEmcFilter->getEmcStatus( 1,towerId )==kGOOD )
        {
          mEnergyInBtow[towerNdx] += hitEnergy;
          if ( geantId < 7 ) mEmEnergyInBtow[towerNdx] += hitEnergy;
          if ( geantId > 7 && geantId < 33 )
          {
            if ( charge!= 0 ) mChHadEnergyInBtow[towerNdx] += hitEnergy;
            else mQ0HadEnergyInBtow[towerNdx] += hitEnergy;
          }
        }
        //else cout << " mc hits in non valid tower" << endl;
      }
    }
  }

}
//------------------------------------------------------------------------------
void StEmcEnergy::hadDepFromMcTrackInBtow()
{  
  for (UInt_t i=1; i<4800; i++) 
  {
    mChHadEnergyInBtow[i]=0;
    mQ0HadEnergyInBtow[i]=0;
  }
  
  StSPtrVecMcTrack& tracks = mMcEvent->tracks();
          
  mEmcFilter->setOnlyHadrons(kTRUE);
  //mEmcFilter->setMcChargeType(kCHARGED);
  mEmcFilter->setMcMustProjectEmc(kTRUE);
  
  StEmcPosition* emcPosition = new StEmcPosition();
  StEmcHadDE* emcHadDE = new StEmcHadDE();
  
  for (UInt_t i = 0; i < tracks.size(); i++)
  {                   
    if (mEmcFilter->accept(tracks[i]))
    {      
      Float_t charge = tracks[i]->particleDefinition()->charge();
      
      StThreeVectorD  finalPosition, finalMomentum;
      Float_t         trackFinalEta, trackFinalPhi;
      emcPosition->trackOnEmc(&finalPosition, &finalMomentum, tracks[i], mBfield);
      Int_t m = 0, e = 0, s = 0, id = 0;
      trackFinalEta = finalPosition.pseudoRapidity();
      trackFinalPhi = finalPosition.phi();
      mBemcGeom->getBin(trackFinalPhi, trackFinalEta, m, e, s);
      if(s==-1) s=1;
      if ( m != 0 && s > 0 )
      { 
        mBemcGeom->getId(m, e, s, id);
        if ( mEmcFilter->getEmcStatus(1,id)==kGOOD )
        {        
          Float_t dist;
          Int_t nTowersdEta = 0, nTowersdPhi = 0;
          Int_t towerId=0, towerNdx = -1;
          // Storing the hadronic energy do be subtracted from each tower
          // surrounding the track tower and track tower itself 
          for (UInt_t j = 0; j < 5; j++)
          {
            nTowersdEta = j - 2;
            for (UInt_t k = 0; k < 5; k++)
            {
              nTowersdPhi = k - 2;
              dist = emcPosition->getDistTowerToTrack(trackFinalEta, trackFinalPhi, 
                                                   nTowersdEta, nTowersdPhi);
              if (dist <= 0.1 )
              {
                towerId = emcPosition->getNextTowerId(trackFinalEta, trackFinalPhi, 
                                                   nTowersdEta, nTowersdPhi);                                            
                towerNdx = towerId - 1;
                if (towerNdx != -1 && mEmcFilter->getEmcStatus(1,towerId)==kGOOD)
                {
                  Float_t tempDepEnergy = 0;
                  tempDepEnergy = emcHadDE->getDepEnergy(tracks[i], mBfield, 
                                                         nTowersdEta, nTowersdPhi);
                  if (tempDepEnergy >= 0) 
                  {
                    if ( charge == 0 ) mQ0HadEnergyInBtow[towerNdx] += tempDepEnergy;
                    else mChHadEnergyInBtow[towerNdx] += tempDepEnergy;
                  }
                }
              }
            } 
          } 
        }
      }
    }
  } 
  delete emcPosition;
  delete emcHadDE;
}
//------------------------------------------------------------------------------
void StEmcEnergy::processEnergy()
{ 
  mBemcEnergy = 0;
  mChHadEnergy = 0;
  mQ0HadEnergy = 0;
  mEmEnergy = 0;
  mBemcEt = 0;
  mChHadEt = 0;
  mQ0HadEt = 0;
  mEmEt = 0;  
    
  StEmcPosition* emcPosition = new StEmcPosition();

  for(Int_t i=0;i<4800;i++)
  {
    mBemcEnergy+=mEnergyInBtow[i];
    mChHadEnergy+=mChHadEnergyInBtow[i];
    mQ0HadEnergy+=mQ0HadEnergyInBtow[i];
    mEmEnergy+=mEmEnergyInBtow[i];
    Float_t theta;
    Int_t TowerId = i+1;
    
    if ( mEvalMetd != evalOff ) 
    {
      StMcVertex *vertex = mMcEvent->primaryVertex();
      theta = emcPosition->getThetaFromVertex(vertex,TowerId);
    }
    else
    {
      StVertex *vertex = mEvent->primaryVertex();   
      theta = emcPosition->getThetaFromVertex(vertex,TowerId); 
    }
    
    Float_t sint=sin(theta);
     
    mEtInBtow[i]=sint*mEnergyInBtow[i];
    mBemcEt+=mEtInBtow[i];
    
    mChHadEtInBtow[i]=sint*mChHadEnergyInBtow[i];
    mChHadEt+=mChHadEtInBtow[i];
    
    mQ0EtInBtow[i]=sint*mQ0HadEnergyInBtow[i];
    mQ0HadEt+=mQ0EtInBtow[i];
    
    mEmEtInBtow[i]=sint*mEmEnergyInBtow[i];
    mEmEt+=mEmEtInBtow[i];    
  }
  delete emcPosition; 
}
//------------------------------------------------------------------------------
Float_t StEmcEnergy::trackEff( StTrack* track, UInt_t Nhadminus)
{ 
  Float_t parm0, parm1, parm2;
  
  if(!mTPCEff) return 1;
  
  Float_t trackEff;
  parm0 = 0.9715 - 67.44e-5 * Nhadminus;
  parm1 = 0.02805 - 3.731e-5 * Nhadminus;
  parm2 = 0.05309 + 3.387e-5 * Nhadminus;
  
  Float_t trackMomentum=track->geometry()->momentum().perp();
  
  trackEff = parm0 * ( 1 - exp(-(trackMomentum-parm1)/parm2) );
  return trackEff;

}
//------------------------------------------------------------------------------
Float_t StEmcEnergy::dEToTotaldE(StMcCalorimeterHit* hit, const char* detname)
{
  
  char* det[]={"bemc","bprs","bsmde","bsmdp"};
  
  /* Get hit deposited energy and convert 
     to energy for each EMC detector. */

  Float_t P0[]={14.69,559.7,0.1185e6,0.1260e6};
  Float_t P1[]={-0.1022,-109.9,-0.3292e5,-0.1395e5};
  Float_t P2[]={0.7484,-97.81,0.3113e5,0.1971e5};

  Int_t m=hit->module();
  Int_t e=hit->eta();
  //UInt_t s=abs(hit->sub());
  Float_t dE=hit->dE();
  Float_t hitEnergy;

  StEmcGeom* localBemcGeom = StEmcGeom::getEmcGeom(detname);
  Float_t Eta;
  localBemcGeom->getEta(m,e,Eta);

  for (Int_t i=0; i<4; i++)
  {
    if (!strcmp (detname, det[i])) // For i<4, only Barrel EMC
    {
      Float_t x=abs(Eta);
      Float_t sf=P0[i]+P1[i]*x+P2[i]*x*x;
      hitEnergy=dE*sf;
      if (hitEnergy<=0)hitEnergy=0;
    }
  }
  return hitEnergy;
}
//------------------------------------------------------------------------------
void StEmcEnergy::setEmcFilter(StEmcFilter* filter)
{ 
	if(mInternalFilter && mEmcFilter) delete mEmcFilter;
	mEmcFilter = filter; 
	mInternalFilter=kFALSE;
}
//------------------------------------------------------------------------------
void StEmcEnergy::setBfield(Float_t Bfield)  
{ 
	mBfield = Bfield; 
	if(mEmcFilter) mEmcFilter->setBField(Bfield);
}


