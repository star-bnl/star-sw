//Projection.h
//M.L. Miller (MIT Software)


#ifndef Projection_HH
#define Projection_HH

class StMuTrack;
class EmcHit;

class Projection
{
public:
    Projection() : mTrack(0), mHit(0), mDeltaR(0) {};
    virtual ~Projection() {};
    
    //sets
    void setTrack(StMuTrack* t) {mTrack=t;}
    void setHit(EmcHit* h) {mHit = h;}
    void setDeltaR(double v) {mDeltaR=v;}
    
    //gets
    StMuTrack* track() {return mTrack;}
    EmcHit* hit() {return mHit;}
    double deltaR() const {return mDeltaR;}
    
protected:
    StMuTrack* mTrack;
    EmcHit* mHit;
    double mDeltaR;
};

struct ProjectionLessThan
{
    bool operator()(const Projection& lhs, const Projection& rhs) const
    {
	return lhs.deltaR() < rhs.deltaR();
    }
};

#endif
