/*******************************************************
 * $Id: StRichMonitor.cxx,v 1.2 2000/01/27 17:09:56 lasiuk Exp $
 *
 * Description:
 *  Implementation of the Viewer displaying module
 *
 ********************************************************
 * $Log: StRichMonitor.cxx,v $
 * Revision 1.2  2000/01/27 17:09:56  lasiuk
 * modify to work stand-alone from ROOT
 *
 * Revision 1.1  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 ********************************************************/
#ifdef __ROOT__
#ifdef RICH_WITH_MONITOR
#include "StRichMonitor.h"

#include <iostream.h>

#include "TLine.h"

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

StRichMonitor* StRichMonitor::mInstance = 0;


StRichMonitor::StRichMonitor() 
{
    StRichGeometryDb* mGeometryDb = StRichGeometryDb::getDb();
    //
    //  creates a Canvas, histograms.
    //  Sets attributes. Graphical stuff.
    //
    mCanvas    = new TCanvas("Pad Monitor","Rich Digitalization",200,10, 1120,720);

    //gStyle->SetPalette(1);
  
    // Make Grid -- For some reason, root doesn't like for loops for this
    TLine *line;
    int i = 1;
    while( i < mGeometryDb->numberOfPadsInColumn()) //NUM_PADS_Y )
	{
	    line = new TLine(-.5, i-.5,mGeometryDb->numberOfPadsInRow()-.5 , i-.5);
	    line->Draw();
	    i++;
	}
  i = 0;
  while( i < mGeometryDb->numberOfPadsInRow()) // _X )
      {
	  line = new TLine(i-.5, -.5,i-.5,mGeometryDb->numberOfPadsInColumn()-.5);
	  line->Draw();
	  i++;
      }
  // End Make Grid. 

  //richCanvas->Update();   
}


StRichMonitor::~StRichMonitor()
{
    delete mCanvas;
    
}


StRichMonitor* StRichMonitor::getInstance()
{
    if(!mInstance)
	mInstance = new StRichMonitor();
    return mInstance;
}

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif

#endif // __ROOT__
