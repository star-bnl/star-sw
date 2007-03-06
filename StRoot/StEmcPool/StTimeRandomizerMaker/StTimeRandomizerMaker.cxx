#include "StTimeRandomizerMaker.h"

#include <fstream>

#include <St_base/StMessMgr.h>
#include <StEvent/StEvent.h>

ClassImp(StTimeRandomizerMaker);

//_____________________________________________________________________________
StTimeRandomizerMaker::StTimeRandomizerMaker(const char *name)
    : inherited(name)
    , runTimesFilename("")
    , normalizeEventsTotal(0)
    , datasetNameStEvent("StEvent")
    , mEventsTotal(0)
    , mEventNum(0)
    , mNormalizedEventsCounter(0)
{
}

//_____________________________________________________________________________
StTimeRandomizerMaker::~StTimeRandomizerMaker() {
}

//_____________________________________________________________________________
Int_t StTimeRandomizerMaker::Init() {
  {LOG_DEBUG << "Starting Init()" << endm;}
  Int_t result = this->inherited::Init();
  this->mEventNum = 0;
  this->mRunsUsed.clear();
  this->mRuns.clear();
  this->mDates.clear();
  this->mTimes.clear();
  this->mEvents.clear();
  this->mEventsTotal = 0;
  if (this->runTimesFilename) {
	TString filename = this->runTimesFilename;
	if (filename != "") {
	    {LOG_INFO << "Reading timestamps from " << filename << "..." << endm;}
	    ifstream str(filename);
	    Int_t numRuns = 0;
	    while (str.good()) {
		Int_t run = -1, date = -1, time = -1;
	        Float_t events = -1;
		str >> run >> date >> time >> events;
	        if (str.good() && (run != -1) && (date != -1) && (time != -1) && (events > 0)) {
		    numRuns++;
		    this->mRunsUsed.push_back(0);
		    this->mRuns.push_back(run);
		    this->mDates.push_back(date);
		    this->mTimes.push_back(time);
		    this->mEvents.push_back(events);
		    this->mEventsTotal += events;
		}
	    }
	    str.close();
	    {LOG_INFO << "Read " << numRuns << " runs, " << this->mEventsTotal << " events" << endm;}
	} else {
	    {LOG_ERROR << "File " << this->runTimesFilename << " not found!" << endm;}
	    result = kStWarn;
	}
  }
  this->mRandom.SetSeed(0);
  this->mNormalizedEventsCounter = 0;
  if (this->normalizeEventsTotal != 0) {
	this->mNormalizedEventsCounter = this->mRandom.Uniform(0, this->mEventsTotal / this->normalizeEventsTotal);	
  }
  {LOG_DEBUG << "Finished Init()" << endm;}
  return result;
}

//_____________________________________________________________________________
Int_t StTimeRandomizerMaker::Make() {
  {LOG_DEBUG << "Starting Make()" << endm;}
  Int_t result = this->inherited::Make();
  if ((this->mEventsTotal > 0) && (this->mDates.size() == this->mTimes.size()) && (this->mDates.size() == this->mEvents.size()) && (this->mDates.size() > 0)) {
    Float_t random = 0;
    if (this->normalizeEventsTotal != 0) {
	this->mNormalizedEventsCounter += this->mEventsTotal / this->normalizeEventsTotal;
	while (this->mNormalizedEventsCounter >= this->mEventsTotal) this->mNormalizedEventsCounter -= this->mEventsTotal;
	random = this->mNormalizedEventsCounter;
    } else {
	random = this->mRandom.Uniform(0, this->mEventsTotal);
    }
    list<Int_t>::iterator runUsedIter = this->mRunsUsed.begin();
    list<Int_t>::const_iterator runIter = this->mRuns.begin();
    list<Int_t>::const_iterator dateIter = this->mDates.begin();
    list<Int_t>::const_iterator timeIter = this->mTimes.begin();
    list<Float_t>::const_iterator eventsIter = this->mEvents.begin();
    Int_t run = -1, date = -1, time = -1;
    do {
	run = *runIter;
	date = *dateIter;
	time = *timeIter;
	random -= *eventsIter;
	++runUsedIter;
	++runIter;
	++dateIter;
	++timeIter;
	++eventsIter;
    } while((random > (*eventsIter)) && (runUsedIter != this->mRunsUsed.end()) && (runIter != this->mRuns.end()) && (dateIter != this->mDates.end()) && (timeIter != this->mTimes.end()) && (eventsIter != this->mEvents.end()));
    if ((run != -1) && (date != -1) && (time != -1)) {
	{LOG_INFO << "Setting run " << run << ", date " << date << ", time " << time << endm;}
	this->SetDateTime(date, time);
	StEvent *event = (StEvent*)GetInputDS(this->datasetNameStEvent);
	if (event) {
	    event->setRunId(run);
	    event->setId(this->mEventNum);
	    this->mEventNum++;
	} else {LOG_ERROR << "Cannot find StEvent at " << this->datasetNameStEvent << endm;}
   	--runUsedIter;
   	*runUsedIter = (*runUsedIter) + 1;
    }
  } else {LOG_DEBUG << "Nothing to do" << endm;}
  {LOG_DEBUG << "Finished Make()" << endm;}
  return result;
}

//______________________________________________________________________________
Int_t StTimeRandomizerMaker::Finish() {
  {LOG_DEBUG << "Starting Finish()" << endm;}
  Int_t result = this->inherited::Finish();
  {
    list<Int_t>::const_iterator runIter = this->mRuns.begin();
    for (list<Int_t>::iterator runUsedIter = this->mRunsUsed.begin();runUsedIter != this->mRunsUsed.end();++runUsedIter) {
	{LOG_DEBUG << "Run " << (*runIter) << " used " << (*runUsedIter) << " times" << endm;}
	++runIter;
    }
  }
  {LOG_DEBUG << "Finished Finish()" << endm;}
  return result;
}
