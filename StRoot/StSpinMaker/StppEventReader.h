//StppEventReader.h
//M.L. Miller (Yale Software)
//07/02

#ifndef StppEventReader_HH
#define StppEventReader_HH

class StMuDst;
class StMuTrack;
class StppuDstMaker;
class StppEvent;
class MuEventReader;

class StppEventReader
{
public:
    StppEventReader(MuEventReader* m, StppuDstMaker* p)
	: mMuEventReader(m), mStppuDstMaker(p) {};
    virtual ~StppEventReader() {};

    //Event access
    int numberOfEvents() const;
    StppEvent* getEvent();
    StppEvent* getNextEvent();
    StMuTrack* lcp() {return mLcp;}

protected:
    StppEventReader(); // not implemented
    MuEventReader* mMuEventReader; //!
    StppuDstMaker* mStppuDstMaker; //!
    StMuTrack* mLcp; //!
    ClassDef(StppEventReader,1)
};

#endif
