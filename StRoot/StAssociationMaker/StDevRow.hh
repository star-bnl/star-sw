/***************************************************************************
 *
 * $Id: StDevRow.hh,v 1.3 1999/09/23 21:25:20 calderon Exp $
 * Generic device row.  For TPC, this would be one padrow.
 * It just holds hits.
 *
 * $Log: StDevRow.hh,v $
 * Revision 1.3  1999/09/23 21:25:20  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 *
 **************************************************************************/

#ifndef StDevRow_HH
#define StDevRow_HH

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

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
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StLocalHit*> localHits;
#else
    vector<StLocalHit*, allocator<StLocalHit*> > localHits;
#endif


};


#endif
