/****************************************************************
 * $Id: StRichGeantReader.h,v 1.2 2000/02/08 16:24:56 lasiuk Exp $
 *
 * Description:
 *   StRichGeantReader is the input module for  
 *   StRichRawData project.
 * 
 *   StRichGeantReader is derived from Reader and has:
 *     - operator(GHit&) : get a new hit
 *     - check(char*)   : test input source
 *
 *   Its implementation relies directly on calls to Fortran 
 *   Geant functions. StRichGeantReader actually reads 2 tables.
 *   The reason is that Geant stores hit in Rich for two volumes:
 *   "RGAP" and "RCSI". That is why several stats variables had to
 *   be put into place.
 *
 * $Log: StRichGeantReader.h,v $
 * Revision 1.2  2000/02/08 16:24:56  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.2  2000/02/08 16:24:56  lasiuk
 * Remove from package next revision.  I/O handled by maker
 *
 * Revision 1.1  2000/01/18 21:32:01  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/29/1999 created the class,               Alexandre Nevski.
 *     - 7/30/1999 re-implementation of operator(), Alexandre Nevski.
 *     - 8/5/1999  changed bool return type to int
 *                 added filter()                 , Alexandre Nevski.
 * 
 ********************************************************************/
#ifdef NEVER
#ifndef ST_RICH_GEANTREADER_H
#define ST_RICH_GEANTREADER_H


#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichGHit.h"
#include "StRichReader.h"
    
    class StRichGeantReader : public StRichReader<StRichGHit,char*> 
    {
    public:
	StRichGeantReader();
	~StRichGeantReader() { }
	
	int operator()(StRichGHit& );

	int  check(char*) const;
 
    private:

	inline void switchQuads(int);            // changes quadrant system
	inline bool inVolume(char*) const;        // predicates used internally
	inline int read();                        // just to make code readable
	int  inError;                             // no loss of time - inlined

	char* mCset;                              // Geant ID of the detector
	char* mCdet;                              // ID of the volume
	
	int mHitID, mTrackID, mNvl;               // for storing purposes
	float mCols[12];                          // temporaries
	   
    };

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif




#endif
