/*
  Generic device row.  For TPC, this would be one padrow.
  It just holds hits.
*/

#ifndef StDevRow_HH
#define StDevRow_HH

#include <vector>

class StMcTpcHit;
class StTpcHit;
class StLocalHit;

class StDevRow{
public:
    StDevRow();
    ~StDevRow();
    
    void addHit(const StMcTpcHit* hit, float xLocal, float zGlobal);
    void addHit(const StTpcHit* hit, float xLocal, float zGlobal);
    
    StLocalHit* hit(const int ihit){return localHits[ihit];};

    unsigned int nHits() { return localHits.size(); };
private:
    vector<StLocalHit*> localHits;


};


#endif
