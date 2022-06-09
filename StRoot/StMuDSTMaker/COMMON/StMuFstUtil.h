#ifndef StMuFstUtil_h
#define StMuFstUtil_h

#include <TObject.h>
#include <map>

class StMuFstCollection;
class StFstHitCollection;
class StMuDst;
class StFstHit;


class StMuFstUtil : public TObject
{
public:
    StMuFstUtil();
    ~StMuFstUtil();
    StMuFstCollection* getMuFst(StFstHitCollection*);
    StFstHitCollection*   getFst(StMuFstCollection*);
    void               fillMuFst(StMuFstCollection*,StFstHitCollection*);
    void               fillFst(StFstHitCollection*,StMuFstCollection*);


private:

    /** Create StMuFstHits from StFstHits and fill StMuFstCollection */
    void fillMuFstHits(StMuFstCollection*, StFstHitCollection*);

    /** fill the StFstHits from MuDst */
    void fillFstHits(StFstHitCollection*, StMuFstCollection*);

    ClassDef(StMuFstUtil,1)
};

#endif
