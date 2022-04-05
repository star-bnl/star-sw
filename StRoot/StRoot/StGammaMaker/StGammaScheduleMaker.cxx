#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "St_db_Maker/St_db_Maker.h"

#include "TList.h"
#include "TTree.h"

#include "StGammaScheduleMaker.h"

ClassImp(StGammaScheduleMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaScheduleMaker::StGammaScheduleMaker(const char *name): StMaker(name) 
{}

//////////////////////////////////////////////////
//               Add a timestamp                //
//////////////////////////////////////////////////
void StGammaScheduleMaker::addTimestamp(int date, int time, double weight)
{
    
    stamp currentStamp;
    currentStamp.date = date;
    currentStamp.time = time;
    currentStamp.weight = weight;
    
    mStamps.push_back(currentStamp);

}

//////////////////////////////////////////////////
//       Sneak the maker right before the       //
//          slow simulator in the chain         //
//////////////////////////////////////////////////
void StGammaScheduleMaker::rearrange()
{

    StMaker *simulator = GetMakerInheritsFrom("StEmcSimulatorMaker");
    if(!simulator)
    {
        LOG_WARN << "rearrange() - No StEmcSimulatorMaker found in the chain!" << endm;
        return;
    }
    
    TList *makerList = this->GetParentChain()->GetMakeList();

    makerList->Remove(dynamic_cast<StMaker*>(this));
    makerList->AddBefore(simulator, dynamic_cast<StMaker*>(this));

}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaScheduleMaker::Init()
{
    
    // Fetch the total number of events in the MuDst
    StMuDstMaker *muDstMaker = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
    if(!muDstMaker)
    {
        LOG_WARN << "Init() - No StMuDstMaker found in the chain!" << endm;
        return kStWarn;
    }
    
    mTotalEvents = muDstMaker->tree()->GetEntries();
    mCurrentEvent = 0;
    mStampIndex = 0;
    
    if(mStamps.size())
    {

        LOG_INFO << "Init() - Distributing status tables across " << mTotalEvents << " events" << endm;

     	// Calculate integrated weight to ensure proper normalization
        double totalWeight = 0;

        vector<stamp>::iterator it;

        for(it = mStamps.begin(); it != mStamps.end(); ++it)
        {
            totalWeight += (*it).weight;
        }

        // Calculate the events at which the status tables will switch
        double usedEvents = 0;
        for(it = mStamps.begin(); it != mStamps.end(); ++it)
        {
            double newEvents = ((*it).weight / totalWeight) * mTotalEvents;
            (*it).event = usedEvents;
            usedEvents += newEvents;
        }

 	LOG_INFO << "Init() - Using the timestamps" << endm;
        LOG_INFO << "Init() - \tDate\t\tTime\tWeight\tInitial Event" << endm;
        for(it = mStamps.begin(); it != mStamps.end(); ++it)
        {
            LOG_INFO << "Init() - \t" << it->date << "\t" << it->time << "\t" << it->weight << "\t" << it->event << endm;
        }
 
    }
    else
    {
        LOG_INFO << "Init() - Using the default timestamp" << endm;
    }

    return StMaker::Init();

}


//////////////////////////////////////////////////
//                 Maker Make                   //
//////////////////////////////////////////////////
Int_t StGammaScheduleMaker::Make()
{   
        
    // Grab a pointer to the database
    St_db_Maker *database = dynamic_cast<St_db_Maker*>(GetMakerInheritsFrom("St_db_Maker"));
    if(!database)
    {
        LOG_WARN << "Make() - No St_db_Maker found in the chain!" << endm;
        return kStWarn;
    }
        
    // Loop over timestamps
    vector<stamp>::iterator it;
    
    for(it = mStamps.begin(); it != mStamps.end(); ++it)
    {
        
        // Reset the database date and time to align with
        // the new time stamp if necessary
        double diff = mCurrentEvent - it->event;
        if(!mCurrentEvent) diff = 0.5;
        
        if(diff > 0 && diff < 1)
        {
            LOG_INFO << "Make() - Setting time stamp to Date = " << it->date << ", Time = " << it->time
                     << " (Index " << mStampIndex + 1 << ") at event " << mCurrentEvent + 1 << endm;
            database->SetDateTime(it->date, it->time);
            ++mStampIndex;
            break;
        }
    
    }
    
    ++mCurrentEvent;
        
    return kStOk;
    
}
