//MuEventReader.h
//M.L. Miller (Yale Software)
//07/02

#ifndef MuEventReader_HH
#define MuEventReader_HH

class StMuDstMaker;
class StMuDst;
class StMuTrack;

class MuEventReader
{
public:
    MuEventReader(StMuDstMaker* m) : mMuDstMaker(m) {};
    virtual ~MuEventReader() {};

    //Event access
    int numberOfEvents() const;
    StMuDst* getEvent();
    StMuDst* getNextEvent();
    StMuTrack* lcp() {return mLcp;}

protected:
    MuEventReader(); // not implemented
    StMuDstMaker* mMuDstMaker; //!
    StMuTrack* mLcp; //!
    ClassDef(MuEventReader,1)
};

#endif
