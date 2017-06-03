#include "StPreEclMaker.h"
#include "StEvent.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "Stiostream.h"
#include "TStopwatch.h"
#include "StEmcUtil/others/emcDetectorName.h"
ClassImp(StPreEclMaker)

//_____________________________________________________________________________
StPreEclMaker::StPreEclMaker(const char *name, const char *title):StMaker(name,title)
{
    mFinder = NULL;
    mAlg =  kEmcClDefault;
}
//_____________________________________________________________________________
StPreEclMaker::~StPreEclMaker()
{
    if(mFinder)
        delete mFinder;
}
//_____________________________________________________________________________
Int_t StPreEclMaker::Init()
{
    if(!mFinder)
    {
        if(mAlg == kEmcClOld)
            mFinder = new StEmcOldFinder();
    }

    if(!mFinder)
    {
        mFinder = new StEmcOldFinder(); // this is the default
        mAlg = kEmcClOld;
    }
    return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StPreEclMaker::Make()
{
    TStopwatch clock;
    clock.Start();
    LOG_DEBUG << "StPreEclMaker::Make()***********************************************************"<<endm;

    StEvent *ev = getEvent();
    if(!ev)
        return kStOk;

    if(mFinder)
    {
        mFinder->clear(); // clear last event information
        mFinder->clear(ev); // clear clusters and points in StEvent
        mFinder->findClusters(ev); // find new clusters
        mFinder->fillStEvent(ev); // fill StEvent with new clusters
        mFinder->fillHistograms(ev); // fill QA histograms
    }

    clock.Stop();
    LOG_DEBUG <<"Time to run StPreEclMaker::Make() real = "<<clock.RealTime()<<"  cpu= "<<clock.CpuTime()<<endm;
    LOG_DEBUG << "*******************************************************************************"<<endm;
    return kStOK;
}
//_____________________________________________________________________________
Int_t StPreEclMaker::Finish()
{
    return kStOK;
}
StEvent* StPreEclMaker::getEvent()
{
    // First of all, try to get StEvent Pointer
    StEvent *ev = (StEvent*)GetInputDS("StEvent");
    if(!ev)
        return NULL;

    // check if there is a collection
    StEmcCollection *emc = ev->emcCollection();
    if(emc)
        return ev;

    // no emcCollection. Will try from other sources.

    // first try from simulation
    StEmcSimulatorMaker* sim = (StEmcSimulatorMaker*)GetMaker("EmcSimulator");
    if(sim)
    {
        emc = (StEmcCollection*)sim->getEmcCollection();
        //sim->clearStEventStaf();
        if(emc)
        {
            ev->setEmcCollection(emc);
            return ev;
        }
    }

    // no emc collection yet. Try ADCtoE
    StEmcADCtoEMaker *adc =(StEmcADCtoEMaker*)GetMaker("Eread");
    if(adc)
    {
        emc = (StEmcCollection*)adc->getEmcCollection();
        // StEmcADCtoEMaker::clearStEventStaf() has been a NO-OP since 2004
        // and is no longer supported as of 2017-06-02
        //adc->clearStEventStaf();
        if(emc)
        {
            ev->setEmcCollection(emc);
            return ev;
        }
    }
    return NULL;
}
void StPreEclMaker::SetClusterConditions(char *cdet,Int_t sizeMax,
        Float_t energySeed,
        Float_t energyAdd,
        Float_t energyThresholdAll,
        Bool_t  kCheckClustersOk)
{
    if(mAlg!=kEmcClOld)
        return;

    for(Int_t i=0;i<4;i++)
    {
        if(!strcmp(cdet,detname[i].Data()))
        {
            ((StEmcOldFinder*)mFinder)->setEnergySeed(i+1,energySeed);
            ((StEmcOldFinder*)mFinder)->setEnergyAdd(i+1,energyAdd);
            ((StEmcOldFinder*)mFinder)->setEnergyThresholdAll(i+1,energyThresholdAll);
            ((StEmcOldFinder*)mFinder)->setSizeMax(i+1,sizeMax);
            break;
        }
    }
}
