//:>------------------------------------------------------------------
//: FILE:       gl3Algorithm.cc
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "Stl3Util/gl3/gl3Algorithm.h"

#include "Stl3Util/base/FtfLog.h"

//####################################################################
//
//####################################################################
gl3Algorithm::gl3Algorithm (  ) 
{
    GI1 = 0; GI2 = 0; GI3 = 0; GI4 = 0; GI5 = 0;
    GF1 = 0; GF2 = 0; GF3 = 0; GF4 = 0; GF5 = 0;

    preScale = 1;
    postScale = 1;

    priority = 2;

    init();
}

//####################################################################
//
//####################################################################
gl3Algorithm::~gl3Algorithm ( ) 
{
}


//####################################################################
//
//####################################################################
int gl3Algorithm::process (gl3Event* event_in)
{
    event = event_in;

    on = 0; accept = 0; build = 0;

    // PreScaling
    preScale_cnt++;

    if ( (preScale > 0) && (preScale_cnt != preScale) ) {
	return 0;
    }

    preScale_cnt = 0;
    on = 1;

    int decision = this->decide();

    //ftfLog("decision: %d\n", decision);

    if (!decision) {
	return 0;
    }
    accept = 1;
  
    // PostScaling
    if ( accept_cnt%postScale != 0 ) {
	return 0;
    }

    build = 1;

    event = NULL;

    return decision ? priority : 0;
}


//####################################################################
//
//####################################################################
void gl3Algorithm::incrementCounters () 
{
    if (on)      call_cnt++;
    if (accept)  accept_cnt++;
    if (build)   build_cnt++;
}


//####################################################################
//
//####################################################################
int gl3Algorithm::init()
{
    resetCounters();
    on = 0; accept = 0; build = 0;

    for (int i=0; i<10; i++) {
	SummaryData[i] = 0;
    }

    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Algorithm::end ( ) 
{
    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Algorithm::setParameters(int GI1_in, int GI2_in, int GI3_in, 
				int GI4_in, int GI5_in, 
				float GF1_in, float GF2_in, float GF3_in, 
				float GF4_in, float GF5_in)
{
    GI1 = GI1_in;
    GI2 = GI2_in;
    GI3 = GI3_in;
    GI4 = GI4_in;
    GI5 = GI5_in;

    GF1 = GF1_in;
    GF2 = GF2_in;
    GF3 = GF3_in;
    GF4 = GF4_in;
    GF5 = GF5_in;

    return 0;

}

//####################################################################
// Set the scalers: pre- and postScale must be >=1!
//####################################################################
void gl3Algorithm::setScaling(int preScale_in, int postScale_in) 
{
    preScale  = (preScale_in  > 0) ? preScale_in  : 1;
    postScale = (postScale_in > 0) ? postScale_in : 1;
}

//####################################################################
//
//####################################################################
void gl3Algorithm::fillSummary(struct algorithm_data *dest)
{
    dest->algId      = getAlgorithmID();
    dest->on         = on;
    dest->accept     = accept;
    dest->build      = build;
    dest->nProcessed = call_cnt;
    dest->nAccept    = accept_cnt;
    dest->nBuild     = build_cnt;;

    for (int i=0; i<10; i++) {
	dest->data[i] = SummaryData[i];
    }
}


void gl3Algorithm::resetCounters() 
{
    preScale_cnt = 0;
    call_cnt =0;
    accept_cnt = 0;
    build_cnt = 0;
}

void gl3Algorithm::showConfiguration() 
{
    ftfLog("%s: Pre-PostScale: %d %d\n", 
	   getAlgorithmName(), (int)preScale, (int)postScale);
    
    ftfLog("%s: Int Parameters: %d %d %d %d %d\n", 
	   getAlgorithmName(), GI1, GI2, GI3, GI4, GI5);
    
    ftfLog("%s: Float Parameters: %f %f %f %f %f\n", 
	   getAlgorithmName(), GF1, GF2, GF3, GF4, GF5);
};

