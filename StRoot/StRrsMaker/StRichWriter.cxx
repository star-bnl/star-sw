/*****************************************************
 * $Id: StRichWriter.cxx,v 1.8 2000/04/05 16:07:42 lasiuk Exp $
 *
 * Description:
 *  Implementation of the StRichWriter output object.
 ******************************************************
 * $Log: StRichWriter.cxx,v $
 * Revision 1.8  2000/04/05 16:07:42  lasiuk
 * add to coded list of info
 *
 * Revision 1.7  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.6  2000/02/14 01:10:40  lasiuk
 * interface for StRichID is changed
 *
 * Revision 1.5  2000/02/08 19:53:43  lasiuk
 * add to the pads rather than reassign each time!
 *
 * Revision 1.4  2000/02/08 16:36:47  lasiuk
 * Bring into line with HP
 *
 * Revision 1.3  2000/01/27 17:09:59  lasiuk
 * modify to work stand-alone from ROOT
 *
 * Revision 1.2  2000/01/25 22:02:23  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:05  lasiuk
 * Initial Revision
 *
 ******************************************************/

// STL
#include <iostream.h>

// ROOT
#ifdef __ROOT__
#include "St_Table.h"                 
#endif
//#include "St_rd_rd_Table.h"           // STAR_Table #1
//#include "St_rd_rd2_Table.h"          // STAR_Table #2
//#include "rd_rd.h"
//#include "rd_rd2.h"

#include "StRichWriter.h"
#include "StRichOtherAlgorithms.h"

StRichWriter* StRichWriter::p2Instance = 0;
  
StRichWriter::StRichWriter()
    : mStorage(0), mPhysicsDb(0)
{ /* NEVER CAN CALL */
    cerr << "StRichWriter::StRichWriter()--> Never called" << endl;
    exit(-9);
}

StRichWriter::StRichWriter(StRichPadPlane* aPadPlane)
    : mStorage(aPadPlane)
{
    mPhysicsDb = StRichPhysicsDb::getDb();
    mAdcConversion = mPhysicsDb->adcConversion();
}

StRichWriter* StRichWriter::getInstance()
{
    //cerr << "WARNING::StRichWriter::getInstance()" << endl;
    if(!p2Instance)
	cerr << "An Instance of StRichPadPlane must exist!" << endl;
    return p2Instance;
}

StRichWriter* StRichWriter::getInstance(StRichPadPlane* aPadPlane)
{
    if(!p2Instance)
	p2Instance = new StRichWriter(aPadPlane);
    return p2Instance;
}


StRichWriter::~StRichWriter()
{ /* nopt */ }

void StRichWriter::putSignal(int row, int col, double s, int id, int gid, int track_p, StRichSignalType signalType)
{
    (*mStorage)[row][col].signal += s;
    (*mStorage)[row][col].IDs.push_back(StRichID(id,
						 gid,
 						 track_p,
 						 floor(s/mAdcConversion),
						 signalType));
} 

void StRichWriter::cleanUpMCInfo()
{
    anIDList aNewList;
    id_iter theNewIter;
    id_iter theTmpIter;

    for(unsigned int iR=0; iR<mStorage->row_size(); iR++) {
	for(unsigned int iC=0; iC<mStorage->col_size(); iC++) {
	    //
	    // Loop over existing structure;
	    anIDList theTmpList = (*mStorage)[iR][iC].IDs;

	    if(theTmpList.size() < 2 ) continue;
	    //cout << "c/r=> " << iC << " " << iR << endl;

	    aNewList.clear();
	    for(theTmpIter  = theTmpList.begin();
		theTmpIter != theTmpList.end();
		theTmpIter++) {
		
		// is there a matching segment?
		bool necessaryToAdd = true;
		for(theNewIter  = aNewList.begin();
		    theNewIter != aNewList.end();
		    theNewIter++) {
		    if(*theNewIter == *theTmpIter) {
			if(theTmpIter->mAmount>0) {
			    theNewIter->mAmount += theTmpIter->mAmount;
			}
			necessaryToAdd = false;
			break;
			// otherwise, check the next
		    }
		}
		if(necessaryToAdd && theTmpIter->mAmount != 0) {
		    aNewList.push_back(*theTmpIter);
		}
	    } // Tmp iterator

	    //
	    // swap the new and old list here
	    //
	    (*mStorage)[iR][iC].IDs = aNewList;
	}
    }
}
