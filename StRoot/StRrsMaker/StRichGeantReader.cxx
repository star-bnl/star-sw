/******************************************************
 * $Id: StRichGeantReader.cxx,v 1.2 2000/02/08 16:24:55 lasiuk Exp $
 *
 * Description:
 *  Implementation of the StRichGeantReader input module.
 *
 *  Operator() is the function that reads hits one after
 *  another. It uses private data members to temporarly 
 *  store the results. 
 *  It's structure is the following: reads a hit if
 *  possible, or changes to 2nd volume. If neither of the
 *  volumes returns hits, exits. The initialization to 
 *  "RGAP" is done in the constructor. 
 *
 *
 *  Invokes Geant function with the names of the
 *  detector and volumes. The two 4's are the length
 *  of the c-strings, which are needed for Fortran
 *  compatibility. 
 *
 *
 *******************************************************
 * $Log: StRichGeantReader.cxx,v $
 * Revision 1.2  2000/02/08 16:24:55  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.2  2000/02/08 16:24:55  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.1  2000/01/18 21:32:01  lasiuk
 * Initial Revision
 *
 ******************************************************/
#ifdef NEVER
extern "C" int agfhit0_ (char*, char*, int, int);
extern "C" int agfhit1_ (int*, int*, int*, float*);

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    
#include "StRichGeantReader.h"
#include "StRichGHit.h"

  StRichGeantReader::StRichGeantReader()
  {
      //
      //  Initialize to volume "RGAP" first 
      //

      mCset = "RICH";
      mCdet = "RGAP";
      
      inError = check(mCdet) ? 1 : 0; 
  }


    int StRichGeantReader::operator()(StRichGHit& hit )
    {
	if ( !inError ) {
	    if ( read() == 0 ) {
		switchQuads(mNvl);
		hit.fill( mCols[0], mCols[1], mCols[2], mNvl, mCols[3], 
			  mCols[4], mCols[5], mCols[9], mCols[10], mHitID , mCdet );
		return 0;
	    }
	    else if ( inVolume("RGAP") ) {
		mCdet = "RCSI";
		inError = check(mCdet) ? true : false;
		return ( (*this)(hit) );
	    }
	}
      return 1;
    }
    

    inline int StRichGeantReader::check( char* det ) const
    {
	return ( ::agfhit0_(mCset,det,4,4) );
    }
    
    
    
    inline bool StRichGeantReader::inVolume( char* det ) const
    {
	//
	//  Checks the third letters of 2 C-strings
	//
	
	return ( mCdet[2] == det[2] );
    }
    
    
    inline int StRichGeantReader::read() 
    {
	//
	//  Invokes Geant read function, stores the results 
	//  in temporaries.
	//
	
	return ( ::agfhit1_(&mHitID, &mTrackID, &mNvl, mCols) );
    }
    

    inline void StRichGeantReader::switchQuads(int mNvl )
    {
	//
	//  Changes the quadrants disposition
	//  It also changes any undefined quad value to 0
	//
	
	switch(mNvl) {
	case 1: { mNvl = 3; break;}
	case 2: { mNvl = 2; break;}
	case 3: { mNvl = 4; break;}
	case 4: { mNvl = 1; break;}
	    
	default: mNvl = -1;                 // if undefined
	}
	
    }
#ifndef ST_NO_NAMESPACES
//}
#endif
#endif
