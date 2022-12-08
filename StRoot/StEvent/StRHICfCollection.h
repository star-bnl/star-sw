#ifndef StRHICfCollection_hh
#define StRHICfCollection_hh

#include <vector>

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"

#include "StRHICfRawHit.h"
#include "StRHICfHit.h"
#include "StRHICfPoint.h"

class StRHICfCollection : public StObject 
{
	public:
		StRHICfCollection();
		~StRHICfCollection();

		void clear();

		// main RHICf data structure
		StRHICfRawHit* rawHitCollection();
		StRHICfHit* hitCollection();

		void addPoint(StRHICfPoint* pointColl); 
		std::vector<StRHICfPoint*>& pointCollection();    // Return the point list
		const std::vector<StRHICfPoint*>& pointCollection() const;

		// run header 
		void isAllSave();
		void setRunNumber(unsigned int run);
		void setEventNumber(unsigned int event);
		void setBunchNumber(unsigned int bunch);
		void setRunType(unsigned int type);
		void setTriggerNumber(unsigned int trigger);
		void setRunTime(Int_t idx, unsigned int time);
		void setRunTRGM(unsigned int trgm);

		unsigned int numberOfPoints() const;
		unsigned int getRunNumber();
		unsigned int getEventNumber();
		unsigned int getBunchNumber();
		unsigned int getRunType();
		unsigned int getTriggerNumber();
		unsigned int getRunTime(Int_t idx);
		unsigned int getRunTRGM();

	private:
		StRHICfRawHit* mRHICfRawHitColl; 
		StRHICfHit* mRHICfHitColl; 
		std::vector<StRHICfPoint*> mRHICfPointColl; 

		unsigned int mRunNumber;
		unsigned int mEventNumber;
		unsigned int mBunchNumber;
		unsigned int mRunType;
		unsigned int mRHICfTrigger;
		unsigned int mRunTime[kRHICfNorder];
		unsigned int mRunTRGM;

	ClassDef(StRHICfCollection,1)
};

#endif
