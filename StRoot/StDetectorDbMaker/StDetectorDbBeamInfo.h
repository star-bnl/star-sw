#ifndef StDetectorDbBeamInfo_h
#define StDetectorDbBeamInfo_h
class StMaker;
class  TTable;
struct beamInfo_st;

class StDetectorDbBeamInfo{
public:
    static StDetectorDbBeamInfo*  instance();
    unsigned int  getRunNumber();
    int           getEntryTag();
    char*         getBlueSpecies();
    unsigned int  getBlueMassNumber();
    float         getBlueEnergy();
    float         getBlueIntensity();
    float         getBlueLifeTime();
    float         getBlueBunchIntensity();
    float         getBlueFillNumber();
    char*         getYellowSpecies();
    unsigned int  getYellowMassNumber();
    float         getYellowEnergy();
    float         getYellowIntensity();
    float         getYellowLifeTime();
    float         getYellowBunchIntensity();
    float         getYellowFillNumber();
    
    friend ostream& operator<<(ostream& os, StDetectorDbBeamInfo& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbBeamInfo();
    StDetectorDbBeamInfo();
    beamInfo_st * mBeamInfo; // points to beamInfo structure
    TTable* mTable; // points to table, need to re-intilize mStarClockOnl every event
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbBeamInfo* sInstance;
};

#endif
