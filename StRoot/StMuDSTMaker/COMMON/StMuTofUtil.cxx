#include "StEvent.h"
#include "StTrack.h"
#include "StTofCollection.h"
#include "StMuTofUtil.h"
#include "StMuTofHitCollection.h"

ClassImp(StMuTofUtil)

StMuTofUtil::StMuTofUtil()
{ }

StMuTofUtil::~StMuTofUtil()
{ }

StMuTofHitCollection* StMuTofUtil::getMuTofHit(StTofCollection *tofcol)
{
  if(!tofcol) return NULL;
  StMuTofHitCollection* muTofHit = new StMuTofHitCollection();
  fillMuTofHit(muTofHit, tofcol);
  return muTofHit;
}

void StMuTofUtil::fillMuTofHit(StMuTofHitCollection* muTofHit, StTofCollection* tofcol)
{
  if(!tofcol || !tofcol->dataPresent()) return;
  if(!muTofHit) return;

  StMuTofHit* tofMuHit = new StMuTofHit(); 

  if( ( tofcol->cellsPresent() || tofcol->slatsPresent() ) &&
      tofcol->hitsPresent() ) {
    StSPtrVecTofHit &tofHits = tofcol->tofHits();
    StSPtrVecTofCell &tofCell = tofcol->tofCells();
    StSPtrVecTofSlat &tofSlat = tofcol->tofSlats();
    
    for(size_t i=0; i < tofHits.size(); i++) {
      tofMuHit->setIconf(1);
      tofMuHit->setTrayIndex(tofHits[i]->trayIndex());
      tofMuHit->setModuleIndex(tofHits[i]->moduleIndex());
      tofMuHit->setCellIndex(tofHits[i]->cellIndex());
      tofMuHit->setDaqIndex(tofHits[i]->daqIndex());
      tofMuHit->setTimeOfFlight(tofHits[i]->timeOfFlight());
      tofMuHit->setPathLength(tofHits[i]->pathLength());
      tofMuHit->setBeta(tofHits[i]->beta());
      tofMuHit->setAssociatedTrackId(tofHits[i]->associatedTrack()->key());
      if(tofHits[i]->trayIndex()!=0&&tofHits[i]->moduleIndex()!=0) { // tofr
	tofMuHit->setADC((int)tofCell[tofHits[i]->cellCollIndex()]->adc());
	tofMuHit->setProjectedPoint((StThreeVectorF&)tofCell[tofHits[i]->cellCollIndex()]->position());
      } else { // tofp
	tofMuHit->setADC((int)tofSlat[tofHits[i]->cellCollIndex()]->adc());
	tofMuHit->setProjectedPoint((StThreeVectorF&)tofSlat[tofHits[i]->cellCollIndex()]->position());
      }
      tofMuHit->settofExpectedAsElectron(tofHits[i]->tofExpectedAsElectron());
      tofMuHit->settofExpectedAsPion(tofHits[i]->tofExpectedAsPion());
      tofMuHit->settofExpectedAsKaon(tofHits[i]->tofExpectedAsKaon());
      tofMuHit->settofExpectedAsProton(tofHits[i]->tofExpectedAsProton());
      tofMuHit->setsigmaElectron(tofHits[i]->sigmaElectron());
      tofMuHit->setsigmaPion(tofHits[i]->sigmaPion());
      tofMuHit->setsigmaKaon(tofHits[i]->sigmaKaon());
      tofMuHit->setsigmaProton(tofHits[i]->sigmaProton());
      //tofMuHit->setparticleHypothesis(int);

      muTofHit->push_back(tofMuHit);
    }
  
  }
  return;
}
