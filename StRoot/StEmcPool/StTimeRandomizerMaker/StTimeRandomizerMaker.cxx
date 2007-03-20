#include "StTimeRandomizerMaker.h"

#include <fstream>

#include <St_base/StMessMgr.h>
#include <StEvent/StEvent.h>

ClassImp(StTimeRandomizerMaker);

//_____________________________________________________________________________
StTimeRandomizerMaker::StTimeRandomizerMaker(const char *name)
    : inherited(name)
    , mRunTimesFilename("")
    , mNormalizeEventsTotal(0)
    , mDatasetNameStEvent("StEvent")
    , mSeed(0)
    , mBaseEventId(0)
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
    const TString &filename = this->getRunTimesFilename();
    if (filename != "") {
	    {LOG_INFO << "Reading timestamps from " << filename << "..." << endm;}
	    ifstream str(filename);
	    if (str.good()) {
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
		{LOG_INFO << "Read " << numRuns << " runs, " << this->mEventsTotal << " events" << endm;}
	    } else {
		{LOG_ERROR << "Cannot read file " << filename << endm;}
		result = kStWarn;
	    }
	    str.close();
    } else {
	{LOG_ERROR << "Timestamps file not specified: setRunTimesFilename(\"file.txt\")" << endm;}
        result = kStWarn;
    }
    this->mRandom.SetSeed(this->getSeed());
    this->mNormalizedEventsCounter = this->mRandom.Uniform(0, this->mEventsTotal); // init at random position in the list
    {LOG_DEBUG << "Finished Init()" << endm;}
    return result;
}

//_____________________________________________________________________________
Int_t StTimeRandomizerMaker::Make() {
  {LOG_DEBUG << "Starting Make()" << endm;}
  Int_t result = this->inherited::Make();
  if ((this->mEventsTotal > 0) && (this->mDates.size() == this->mTimes.size()) && (this->mDates.size() == this->mEvents.size()) && (this->mDates.size() > 0)) {
    Float_t random = 0;
    if (this->getNormalizeEventsTotal() != 0) {
	this->mNormalizedEventsCounter += this->mEventsTotal / this->getNormalizeEventsTotal();
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
	Int_t eventId = this->getBaseEventId() + this->mEventNum;
	{LOG_INFO << "Setting run " << run << ", date " << date << ", time " << time << ", event ID " << eventId << endm;}
	this->inherited::SetDateTime(date, time);
	StEvent *event = (StEvent*)this->inherited::GetInputDS(this->getDatasetNameStEvent());
	if (event) {
	    event->setRunId(run);
	    event->setId(eventId);
	} else {LOG_ERROR << "Cannot find StEvent at " << this->getDatasetNameStEvent() << endm;}
   	--runUsedIter;
   	*runUsedIter = (*runUsedIter) + 1;
    } else {LOG_ERROR << "Internal error when selecting timestamp" << endm;}
  } else {LOG_DEBUG << "Nothing to do" << endm;}
  this->mEventNum++;
  {LOG_DEBUG << "Finished Make()" << endm;}
  return result;
}

//______________________________________________________________________________
Int_t StTimeRandomizerMaker::Finish() {
  {LOG_DEBUG << "Starting Finish()" << endm;}
  Int_t result = this->inherited::Finish();
  {LOG_DEBUG << "Processed " << this->mEventNum << " events" << endm;}
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

//______________________________________________________________________________
const TString &StTimeRandomizerMaker::getRunTimesFilename() const {
    return this->mRunTimesFilename;
}

//______________________________________________________________________________
void StTimeRandomizerMaker::setRunTimesFilename(const TString &filename) {
    this->mRunTimesFilename = filename;
}

//______________________________________________________________________________
Float_t StTimeRandomizerMaker::getNormalizeEventsTotal() const {
    return this->mNormalizeEventsTotal;
}

//______________________________________________________________________________
void StTimeRandomizerMaker::setNormalizeEventsTotal(Float_t evNum) {
    this->mNormalizeEventsTotal = evNum;
}

//______________________________________________________________________________
const TString &StTimeRandomizerMaker::getDatasetNameStEvent() const {
    return this->mDatasetNameStEvent;
}

//______________________________________________________________________________
void StTimeRandomizerMaker::setDatasetNameStEvent(const TString &name) {
    this->mDatasetNameStEvent = name;
}

//______________________________________________________________________________
UInt_t StTimeRandomizerMaker::getSeed() const {
    return this->mSeed;
}

//______________________________________________________________________________
void StTimeRandomizerMaker::setSeed(UInt_t seed) {
    this->mSeed = seed;
}

//______________________________________________________________________________
Int_t StTimeRandomizerMaker::getBaseEventId() const {
    return this->mBaseEventId;
}

//______________________________________________________________________________
void StTimeRandomizerMaker::setBaseEventId(Int_t baseId) {
    this->mBaseEventId = baseId;
}
