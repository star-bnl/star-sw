//StiStTrackFilter.h
//M.L. Miller (Yale Software)
//03/01

//This file contains functors that are used to filter StTracks

struct StiStTrackFilter
{
    virtual bool operator()(const StTrack* mytrack) = 0;
};

//Here's an examle of how to write a filter
struct StiTpcHitStTrackFilter : public StiStTrackFilter
{
    virtual bool operator()(const StTrack* mytrack) {
	return (mytrack->detectorInfo()->numberOfPoints(kTpcId) > 0);
    }
};













