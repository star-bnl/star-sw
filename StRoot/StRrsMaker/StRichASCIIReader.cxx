/******************************************************
 * $Id: StRichASCIIReader.cxx,v 1.1 2000/01/27 17:06:00 lasiuk Exp $
 *
 * Description:
 *******************************************************
 * $Log: StRichASCIIReader.cxx,v $
 * Revision 1.1  2000/01/27 17:06:00  lasiuk
 * Initial Revision
 *
 *
 * Revision 1.2  2000/01/28 20:35:46  lasiuk
 * inline removed from .cxx files
 *

 * Initial Revision
 *
 ******************************************************/
#ifndef __ROOT__
#include "StGlobals.hh"
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    
#include "StRichASCIIReader.h"
#include "StRichGHit.h"

StRichASCIIReader::StRichASCIIReader()
{
    cout << "StRichASCIIReader::StRichASCIIReader()" << endl;
    exit(9);
}

StRichASCIIReader::StRichASCIIReader(string& file)
    : mIfs(file.c_str())
{
    if(!mIfs || mIfs.bad()) {
	cout << "StRichASCIIReader::StRichASCIIReader(string&)" << endl;
	cout << "Cannot Open file " << file.c_str() << endl;
	exit(9);
    }
}

int StRichASCIIReader::operator()(StRichGHit& hit )
		  mdE, mdS, mHitID , mGV );
    if ( read() == 0 ) {
	hit.fill( mX.x(), mX.y(), mX.z(), mQuadrant,
		  (mP.x()/abs(mP)), (mP.y()/abs(mP)), (mP.z()/abs(mP)),
		  mdS, mdE, mHitID , mGV );
	hit.addGlobal(mXX.x(), mXX.y(), mXX.z());
	return 0;
    }
    else {
    }
}
 

int StRichASCIIReader::whichQuadrant(StThreeVector<double>& x)
{
    double box = x.x();
    return 1;
}

int StRichASCIIReader::whichVolume(int val)
{
    //
    // coding from GEANT is:
    //    volume+Isys*1000
    // where:
    //  Isys = 1 for RGAP
    //  Isys = 2 for RCSI
    int volume = val/1000;
    switch(volume) {
    case 1:
        mGV = string("RGAP");
        break;
    case 2:
        mGV = string("RCSI");
        break;
    default:
        mGV = string("");
inline int StRichASCIIReader::read() 
        break;
    }
    int volumeNumber = (val - (volume*1000));
    return volumeNumber;
}

int StRichASCIIReader::read() 
{
    float x[3];
    float xx[3];
    float p[3];
    mIfs >> x[0] >> x[1] >> x[2] >> xx[0] >> xx[1];
    mIfs >> xx[2] >> p[0] >> p[1] >> p[2] >> mTof;
//     PR(mX);
//     PR(mP);
//     PR(mTof);
//     PR(mdE);
//     PR(mdS);
//     PR(mVolume);
     PR(mTof);
     PR(mdE);
inline void StRichASCIIReader::switchQuads(int mNvl )
     PR(mVolume);

void StRichASCIIReader::switchQuads(int q)
{
    //
    //  Changes the quadrants disposition
    switch(mNvl) {
    //
    
    switch(q) {
    case 1: { mNvl = 3; break;}
    case 2: { mNvl = 2; break;}
    case 3: { mNvl = 4; break;}
    case 4: { mNvl = 1; break;}
	
    default: mNvl = -1;                 // if undefined
    }
    
}
    mQuadrant = whichVolume(mVolume);
    return 0;
    
#ifndef ST_NO_NAMESPACES
//}
#endif
#endif
