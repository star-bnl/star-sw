#ifndef StDetectorDbRichScalers_h
#define StDetectorDbRichScalers_h

#include <iostream.h>
#include "StMaker.h"
#include "tables/St_trigDetSums_Table.h"
#include "tables/St_richvoltages_Table.h"
#include "tables/St_y1Mult_Table.h"

class StDetectorDbRichScalers{
public:
    
    
    static StDetectorDbRichScalers* instance();
    
    double getCTBWest();
    double getCTBEast();
    double getCTBOr();
    double getTOFp();
    double getZDCWest();
    double getZDCEast();
    double getZDCX();
    double getMult();
    double getL0();
    double getRichHVStatus();
    void update(StMaker*);
    
    friend class nodbody; // For virtual ~
    friend ostream& operator<<(ostream& os, StDetectorDbRichScalers& v);

protected:
    trigDetSums_st* mScalers;
    richvoltages_st* mVolts;
    y1Mult_st* mY1Mults;
    
    StDetectorDbRichScalers();
    virtual ~StDetectorDbRichScalers();
private:
    static StDetectorDbRichScalers* sInstance;
};

#endif
