/***************************************************************************
 *
 * $Id: StTrsIstream.hh,v 1.1 1999/10/11 23:55:13 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Istream class for
 *              reading TRS data.
 ***************************************************************************
 *
 * $Log: StTrsIstream.hh,v $
 * Revision 1.1  1999/10/11 23:55:13  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 *
 **************************************************************************/
#ifndef ST_TRS_ISTREAM_HH
#define ST_TRS_ISTREAM_HH

#include "StTrsIos.hh"

class StTrsRawDataEvent;

class StTrsIstream : public StTrsIos {

public:
    StTrsIstream(string, StTpcGeometry*);
    ~StTrsIstream();

    void fillTrsEvent(StTrsRawDataEvent*);
    
private:
    ifstream ifs;
};
#endif
