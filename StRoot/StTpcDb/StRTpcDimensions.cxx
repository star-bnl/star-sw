#include <StRTpcDimensions.h>

ClassImp(StRTpcDimensions)

int StRTpcDimensions::numberOfSectors() const {
return mTpc->numberOfSectors;
}

float StRTpcDimensions::ifcRadius() const {
return mTpc->tpcInnerRadius;
}
    
float StRTpcDimensions::ofcRadius() const {
return mTpc->tpcOuterRadius;
}
    
float StRTpcDimensions::tpcTotalLength() const {
return mTpc->tpcTotalLength;
}

float StRTpcDimensions::wheelInnerRadius() const {
return mTpc->wheelInnerRadius;
}

float StRTpcDimensions::wheelOuterRadius() const {
return mTpc->wheelOuterRadius;
}

float StRTpcDimensions::wheelThickness() const {
return mTpc->wheelThickness;
}

float StRTpcDimensions::senseGasOuterRadius() const {
return mTpc->senseGasOuterRadius;
}
    
float StRTpcDimensions::tpeaThickness() const {
return mTpc->tpeaThickness; 
}

float StRTpcDimensions::cathodeInnerRadius() const {
return mTpc->cathodeInnerRadius;
}
    
float StRTpcDimensions::cathodeOuterRadius() const {
return mTpc->cathodeOuterRadius;
}
    
float StRTpcDimensions::cathodeThickness() const {
return mTpc->cathodeThickness;
} 

