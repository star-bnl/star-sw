/***************************************************************************
 *
 * $Id: StTrsReader.hh,v 1.1 1998/11/10 17:12:12 fisyak Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: Reads g2t structures for simulator
 *
 ***************************************************************************
 *
 * $Log: StTrsReader.hh,v $
 * Revision 1.1  1998/11/10 17:12:12  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.4  1998/11/08 17:29:16  lasiuk
 * bool built-in data type and allocators
 *
 * Revision 1.3  1998/06/04 23:19:55  lasiuk
 * *** empty log message ***
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 **************************************************************************/
#ifndef ST_TRS_READER_HH
#define ST_TRS_READER_HH

#include <fstream.h>
#include <vector>
#include <string>
#include "StGlobals.hh"
#include "StThreeVector.hh"

#include "StTrsChargeSegment.hh"

class StTrsReader {

public:
    StTrsReader(const string& file);
    ~StTrsReader();
    //StTrsReader(const StTrsReader&);
    //StTrsReader& operator=(cont StTrsReader&);

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    bool getData(vector<StTrsChargeSegment>&);
#else
    bool getData(vector<StTrsChargeSegment, allocator<StTrsChargeSegment> >&);
#endif
private:
    StTrsReader();

private:
    ifstream               mIfs;
    
};

#endif
