#include "StDetectorDbBeamInfo.h"
#include "tables/St_spaceChargeCor_Table.h"

struct beamInfo_st{ 

   unsigned int  runNumber;   /*   */
   int  entryTag;   /*   0=startrun, 1=endrun, 2=runave, 3=std  */
   char  blueSpecies[32];   /*   species  */
  unsigned int blueMassNumber;
   float  blueEnergy;   /*   energy  */
   float  blueIntensity;   /*   Ions  */
   float  blueLifeTime;   /*   Ions per minute  */
   float  blueBunchIntensity;   /*   bunch intensity  */
   char  yellowSpecies[32];   /*   species  */
  unsigned int yellowMassNumber;
   float  yellowEnergy;   /*   energy  */
   float  yellowIntensity;   /*   Ions  */
   float  yellowLifeTime;   /*   Ions per minute  */
   float  yellowBunchIntensity;   /*   bunch intensity  */
   float  blueFillNumber;   /*   */
   float  yellowFillNumber;   /*   */

 };  

/*!

 This class provides and interface to get beamInformation.

*/

/// Initialize Instance
StDetectorDbBeamInfo* StDetectorDbBeamInfo::sInstance = 0;

/// Returns previous instance if exits, if not makes new one
StDetectorDbBeamInfo* StDetectorDbBeamInfo::instance()
{
    
    if(!sInstance){
	sInstance = new StDetectorDbBeamInfo();
    }
    // Need to reinitilize array for some unkown reason
    // the array memeory changed althoug the TTable stays the same
    if(sInstance->mTable)
	sInstance->mBeamInfo = (beamInfo_st*)(sInstance->mTable->GetArray());
    
    return sInstance;
};

/// Updates data in instance from database
void StDetectorDbBeamInfo::update(StMaker* maker){

    mMaker = maker;
    
    if(maker){
		
	TDataSet* dataSet = maker->GetDataBase("RunLog/onl");

	if(dataSet){
	    mTable = dynamic_cast<TTable*>(dataSet->Find("beamInfo"));
	    
	    if(mTable){
		mBeamInfo = (beamInfo_st*)(mTable->GetArray());
	    }
	}
    }
};



/// Default constructor
StDetectorDbBeamInfo::StDetectorDbBeamInfo(){
    cout << "StDetectorDbBeamInfo::StDetectorDbBeamInfo" << endl;
    mBeamInfo = 0;
    mTable = 0;
};

/// Default destructor, does nothing
StDetectorDbBeamInfo::~StDetectorDbBeamInfo(){};

unsigned int StDetectorDbBeamInfo::getRunNumber(){
    unsigned int value = 0;
    if(mBeamInfo)
	value = mBeamInfo->runNumber;

    return value;
};

/// Gets the Entry Tag
int StDetectorDbBeamInfo::getEntryTag(){
    int value = 0;
    if(mBeamInfo)
	value = mBeamInfo->entryTag;

    return value;
};

/// Gets the Blue Species (ie Au, Proton)
char* StDetectorDbBeamInfo::getBlueSpecies(){
    char* value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueSpecies;

    return value;

};

/// gets the Blue Mass Number (ie 197, 1)
unsigned int StDetectorDbBeamInfo::getBlueMassNumber(){
    unsigned int value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueMassNumber;

    return value;

};

/// Gets the Blue Energy
float StDetectorDbBeamInfo::getBlueEnergy(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueEnergy;

    return value;

};

/// Gets the blue intensity at begining of run
float StDetectorDbBeamInfo::getBlueIntensity(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueIntensity;

    return value;

};

/// Gets the blue lifetime at begining of run
float StDetectorDbBeamInfo::getBlueLifeTime(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueLifeTime;

    return value;

};

/// Gets the blue bunch intensity at begining of run
float StDetectorDbBeamInfo::getBlueBunchIntensity(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueBunchIntensity;

    return value;

};

/// Gets the blue fill number
float StDetectorDbBeamInfo::getBlueFillNumber(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->blueFillNumber;

    return value;

};

/// Gets the yellow species (ie Au, Proton)
char* StDetectorDbBeamInfo::getYellowSpecies(){
    char* value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowSpecies;

    return value;

};

/// Gets the yellow mass number (ie 197, 1)
unsigned int StDetectorDbBeamInfo::getYellowMassNumber(){
    unsigned int value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowMassNumber;

    return value;

};

/// Gets the yellow energy
float StDetectorDbBeamInfo::getYellowEnergy(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowEnergy;

    return value;

};

/// Gets the yellow intensity at begining of run
float StDetectorDbBeamInfo::getYellowIntensity(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowIntensity;

    return value;

};

/// Gets the yellow lifetime at begining of run
float StDetectorDbBeamInfo::getYellowLifeTime(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowLifeTime;

    return value;

};

/// gets the yellow bunch intensity at begining of run
float StDetectorDbBeamInfo::getYellowBunchIntensity(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowBunchIntensity;

    return value;

};

/// gets the yellow fill number
float StDetectorDbBeamInfo::getYellowFillNumber(){
    float value = 0;
    if(mBeamInfo)
	value = mBeamInfo->yellowFillNumber;

    return value;

};

/// outputs to ostream the entire class
ostream& operator<<(ostream& os, StDetectorDbBeamInfo& v){

    os << "Beam Info For Run: " << v.getRunNumber() << endl;
    os << "               " << "\tBlue" << "\tYellow" << endl;
    os << "Species        " << "\t" << v.getBlueSpecies() << "\t" << v.getYellowSpecies() << endl;
    os << "Mass Number    " << "\t" << v.getBlueMassNumber() << "\t" << v.getYellowMassNumber() << endl;
    os << "Energy         " << "\t" << v.getBlueEnergy() << "\t" << v.getYellowEnergy() << endl;
    os << "Intensity      " << "\t" << v.getBlueIntensity() << "\t" << v.getYellowIntensity() << endl;
    os << "Lifetime       " << "\t" << v.getBlueLifeTime() << "\t" << v.getYellowLifeTime() << endl;
    os << "Bunch Intensity" << "\t" << v.getBlueBunchIntensity() << "\t" << v.getYellowBunchIntensity() << endl;
    os << "Fill Number    " << "\t" << v.getBlueFillNumber() << "\t" << v.getYellowFillNumber() << endl;
    os << endl;
    
    return os;
};
