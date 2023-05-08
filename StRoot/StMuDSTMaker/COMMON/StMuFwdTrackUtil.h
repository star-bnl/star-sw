#ifndef StMuFwdTrackUtil_h
#define StMuFwdTrackUtil_h

#include <TObject.h>
#include <map>

class StMuFwdTrackCollection;
class StFwdTrackCollection;
class StMuDst;
class StFwdTrack;
class StMuFcsUtil;

class StMuFwdTrackUtil : public TObject
{
public:
    StMuFwdTrackCollection* getMuFwdTrack(StFwdTrackCollection*);
    StFwdTrackCollection*   getFwdTrack(StMuFwdTrackCollection*);
    void               fillMuFwdTrack(StMuFwdTrackCollection*,StFwdTrackCollection*, StMuFcsUtil*);
    void               fillFwdTrack(StFwdTrackCollection*,StMuFwdTrackCollection*);


private:

    ClassDef(StMuFwdTrackUtil,1)
};

#endif
