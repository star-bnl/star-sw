/*****************************************************
 * $Id: StRichWriter.cxx,v 1.2 2000/01/25 22:02:23 lasiuk Exp $
 *
 * Description:
 *  Implementation of the StRichWriter output object.
 ******************************************************
 * $Log: StRichWriter.cxx,v $
 * Revision 1.2  2000/01/25 22:02:23  lasiuk
 * Second Revision
 *
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
 * Revision 1.1  2000/01/18 21:32:05  lasiuk
#include "St_DataSet.h"

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

#ifndef ST_NO_NAMESPACES
#include "StRichGeometryDb.h"
#endif
    
    StRichWriter* StRichWriter::p2Instance = 0;
#include "StRichWriter.h"
    StRichWriter::StRichWriter()
	: mStorage(0)
    { /* NEVER CAN CALL */
	cerr << "StRichWriter::StRichWriter()--> Never called" << endl;
	exit(-9);
    }

    StRichWriter::StRichWriter(StRichPadPlane* aPadPlane)
	: mStorage(aPadPlane)
    { /* nopt */ }
	cerr << "WARNING::StRichWriter::getInstance()" << endl;
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

    void StRichWriter::putSignal(int row, int col, double s, int id)
    {
	(*mStorage)[row][col].signal = s;
	(*mStorage)[row][col].IDs.push_back(id_type(id) );
    } 


  /*
   *  Creates, fills and returns an St_rd_rd STAR_Table from
   *  internal storage (PadPlane).
   *  Loops through the PadPlane and for each pad, it creates
   *  a new entry with the signal, row and pad, and ads it
   *  to main table.
   */

//   St_DataSet* StRichWriter::getPadsTable()
//   {   
//     int n = 0;
//     rd_rd_st pad;
//     St_rd_rd  *tableSet  = new St_rd_rd("rd_rd",
// 		       	   mStorage->row_size() * mStorage->col_size() );
    
//     for ( unsigned int i = 0; i < mStorage->row_size(); i++ )
//       for ( unsigned int j = 0; j < mStorage->col_size(); j++ ) 
//       {
// 	pad.row = i;
//         pad.pad = j;
// 	pad.signal = (int)(*mStorage)[i][j].signal;
// 	tableSet->AddAt(&pad,n++);
//       }
//     return (St_DataSet*)tableSet;
    
//   }



//   /*
//    *  Creates, fills and returns an St_rd_rd2 STAR_Table from
//    *  internal storage (PadPlane).
//    *  A PadPlane can store an infinite number of partial hits
//    *  for each pad. This method creates a new entry with 
//    *  signal, id, amount, row and pad for each such hit, for
//    *  each pad in the plane.
//    *  Note: the ID here is the geant ID that can be found in the
//    *  "tracks" g2t table.
//    */ 

//   St_DataSet*  StRichWriter::getIDTable()
//   {
//     int n = 0;
//     rd_rd2_st g_id;
//     St_rd_rd2 *tableSet2 = new St_rd_rd2("rd_rd2",
// 			   mStorage->row_size() * mStorage->col_size() );

//     for ( unsigned int i = 0; i < mStorage->row_size(); i++ )
//       for ( unsigned int j = 0; j < mStorage->col_size(); j++ ) 
// 	for ( PadPlane::const_id_iter k = (*mStorage)[i][j].IDs.begin();
// 	                              k != (*mStorage)[i][j].IDs.end(); ++k )
// 	{
// 	  g_id.row = i;
// 	  g_id.pad = j;
// 	  g_id.id  = (*k).G_ID;
// 	  if ( (*mStorage)[i][j].signal )
// 	    g_id.q = (*k).amount / (*mStorage)[i][j].signal;
// 	  else
// 	    g_id.q = 1;
	  
// 	  tableSet2->AddAt(&g_id,n++);
// 	}   

//     return (St_DataSet *)tableSet2;
   
//   }



  /*
   *  Static member returns pointer to sole instance
   */
    (*mStorage)[row][col].IDs.push_back(StRichID(id,
						 track_p,
						 nearestInteger(s/mAdcConversion)));
} 


#ifndef ST_NO_NAMESPACES
//}
#endif
