/***************************************************************************
 *
 * $Id: StTrsWriter.hh,v 1.1 1998/11/10 17:12:13 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: write out the digitized information from the sector
 *
 ***************************************************************************
 *
 * $Log: StTrsWriter.hh,v $
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/06/30 22:54:09  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_WRITER_HH
#define ST_TRS_WRITER_HH

#include <iostream.h>

class StTrsWriter {
public:
    StTrsWriter();
    ~StTrsWriter();
    
    //StTrsWriter(const StTrsWriter&);
    //StTrsWriter& operator=(const StTrsWriter&);

    void writeTable() const;
    void writeAscii() const;
    
protected:

private:

};

#endif
