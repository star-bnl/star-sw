/***************************************************************************
 *
 * $Id: StTrsWriter.cc,v 1.1 1998/11/10 17:12:28 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsWriter.cc,v $
 * Revision 1.1  1998/11/10 17:12:28  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/06/30 22:46:50  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsWriter.hh"

StTrsWriter::StTrsWriter() {/* missing */}

StTrsWriter::~StTrsWriter() {/* missing */}

void StTrsWriter::writeTable() const
{
    cout << "write tables (STAF)" << endl;
}

void StTrsWriter::writeAscii() const
{
    cout << "write Ascii file" << endl;
}
