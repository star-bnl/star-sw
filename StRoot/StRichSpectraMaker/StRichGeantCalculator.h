/***************************************************************************
 *
 * $Id: StRichGeantCalculator.h,v 1.1 2001/11/21 21:06:04 lasiuk Exp $
 *
 * Author:  bl Sept 10, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              
 ***************************************************************************
 *
 * $Log: StRichGeantCalculator.h,v $
 * Revision 1.1  2001/11/21 21:06:04  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichGeantCalculator_h
#define StRichGeantCalculator_h

#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StRichCerenkovPhoton.h"

struct gInfo {
    int parentId;        // unique tag
    int numberOfPhotons;
    int parentGeantPid;
};

#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_track_Table.h"

class StRichGeantCalculator {

public:
    StRichGeantCalculator();
    ~StRichGeantCalculator();

    //StRichGeantCalculator(const StRichGeantCalculator&){/* nopt */}    
    //operator =(StRichGeantCalculator(const StRichCerenkovHistoram&) {/* nopt */}

    void process(g2t_rch_hit_st *, g2t_track_st *);

    void clearData();
    void status() const;
    
protected:
    bool addData(int, int);
  
private:
#ifndef __CINT__
    vector<gInfo> mTracks;//!
#endif

};
#endif
