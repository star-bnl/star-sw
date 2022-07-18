#ifndef StMuFstUtil_h
#define StMuFstUtil_h

#include <TObject.h>
#include <map>

class StMuFstCollection;
class StFstHitCollection;
class StFstCollection;
class StMuDst;
class StFstHit;
class StFstRawHit;


class StMuFstUtil : public TObject
{
public:
    StMuFstUtil();
    ~StMuFstUtil();
    StMuFstCollection* getMuFst(StFstHitCollection*,StFstCollection*);
    StFstHitCollection*   getFst(StMuFstCollection*);
    void               fillMuFst(StMuFstCollection*,StFstHitCollection*,StFstCollection*);
    void               fillFst(StFstHitCollection*,StMuFstCollection*);


private:

    /** Create StMuFstHits from StFstHits and fill StMuFstCollection */
    void fillMuFstHits(StMuFstCollection*, StFstHitCollection*, StFstCollection*);

    /** fill the StFstHits from MuDst */
    void fillFstHits(StFstHitCollection*, StMuFstCollection*);

    ClassDef(StMuFstUtil,1)
};

#endif
