/*******************************************************************
 * $Id: StRichASCIIReader.h,v 1.1 2000/01/27 17:06:01 lasiuk Exp $
 *
 * Description:
 *   Reads ASCII files for processing in examples
 *   Used for stand-alone
 ********************************************************************
 * $Log: StRichASCIIReader.h,v $
 * Revision 1.1  2000/01/27 17:06:01  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/01/27 17:06:01  lasiuk
 * Initial Revision
 *
 ********************************************************************/

#ifndef ST_RICH_ASCII_READER_H
#define ST_RICH_ASCII_READER_H

#include <fstream.h>
#include <string>
#include "StThreeVector.hh"

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichGHit.h"
    
class StRichASCIIReader
{
public:
    StRichASCIIReader();
    StRichASCIIReader(string&);
    ~StRichASCIIReader() { }
    
    int operator()(StRichGHit& );

    int  check(char*) const;
    int whichVolume(int);
    int whichQuadrant(StThreeVector<double>&);
private:

    void switchQuads(int);            // changes quadrant system
    bool inVolume(char*) const;        // predicates used internally
    int  read();                        // just to make code readable

    int  inError;                             // no loss of time - inlined

private:
    ifstream              mIfs;

    int mHitID, mTrackID, mNvl;               // for storing purposes
    float mCols[12];                          // temporaries

    StThreeVector<double> mX;
    StThreeVector<double> mXX;
    StThreeVector<double> mP;
    float                 mTof;
    float                 mdE;
    float                 mdS;
    float                 mVolume;
    float                 mVID;
    string                mGV;
    // write(21,*) x(1),x(2),x(3),p(1),p(2),p(3),tof,Elos,Step,
    //    (volume+Isys+1000)
    int                   mQuadrant;
    
};

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif




