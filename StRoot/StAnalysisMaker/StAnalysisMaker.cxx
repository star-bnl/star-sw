//
//  This is a STAR typical comment header. You should modify
//  it to reflect your changes.
//  As a minimum it should contain the name of the author, the
//  date it was written/modified, and a short description of what
//  the class is meant to do. The cvs strings $X$ (where X=Id, Log)
//  are not needed when you do not intend to put the file under
//  cvs control. Remove them.
//  
/***************************************************************************
 *
 * $Id: StAnalysisMaker.cxx,v 2.2 2000/07/12 05:23:28 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a maker to perform analysis
 *               using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: StAnalysisMaker.cxx,v $
 * Revision 2.2  2000/07/12 05:23:28  ullrich
 * Updated for better use as template for actual analysis.
 *
 * Revision 2.1  1999/12/30 01:54:57  ogilvie
 * added countPrimaryPions as example how to use PID
 *
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/

//
//  Include header files. What has to be included strongly depends
//  on your implementation. StEventTypes.h contains all includes
//  you need to use StEvent.
//
#include "StAnalysisMaker.h"
#include "StEventTypes.h"
#include "TNtuple.h"
#include "TFile.h"
#include "StMessMgr.h"

//
//  The following line defines a static string. Currently it contains
//  the cvs Id. The compiler will put the string (literally) in the
//  object file. It thus ends up in the shared library.
//  The UNIX command 'strings' allowsto print all printable character in
//  a non-text file. This way one can check the version of the file
//  contained in a given shared library. If you do not intend to put
//  the file under cvs control (likely) you can remove the line.

//
//  Proptotypes of little functions which perform
//  specific analysis tasks. You'll find them
//  in the same directory as StAnalysisMaker.cxx.
//  You most likely will not need them but they can serve
//  as an example for your own functions.
//
void summarizeEvent(StEvent&, const int&);
long countPrimaryTracks(StEvent&);
long countPrimaryPions(StEvent&);


//
//  This is needed to make your maker work in root4star.
//  It can be place anywhere in the file. Note that this
//  is a macro, that's why the ';' is missing.
//
ClassImp(StAnalysisMaker)

//    
//  The constructor. Initialize you data members here.
//    
StAnalysisMaker::StAnalysisMaker(const Char_t *name) : StMaker(name)
{
    mEventCounter = 0;
    mFile = 0;
    mTuple = 0;
}

//
//  Usually ok to leave this as it is.
//
StAnalysisMaker::~StAnalysisMaker() { /* noop */ }

//
//  Called once at the beginning.
//  This is a good place to book histos and tuples.
//
Int_t
StAnalysisMaker::Init()
{
    //
    //  Output file.
    //  mFileName should contain a valid filename. Here
    //  we dump the file into the null device.
    //
    mFileName = "/dev/null";    
    mFile =  new TFile(mFileName.c_str(), "RECREATE");
    cout << "StAnalysisMaker::Init():\n";
    cout << "\tHistograms will be stored in file '"
	 <<  mFileName.c_str() << "'" << endl;
    
    //
    //  Define Ntuple.
    //  Keep the order TFile -> TNtuple
    //  
    string varList = "evt:xvt:yvtx:zvtx:ctbsum:zdcsum:nprimary:npions:goodfrac";
    mTuple = new TNtuple("example","example",varList.c_str());
    
    //
    //  Call Init() of the base class.
    //  Always leave this in.
    //
    return StMaker::Init();
}

//
//  Called every event after Make(). Usually you do not
//  need to do anything here. Leave it as it is.
//
void
StAnalysisMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

//
//  Called once at the end.
//
Int_t
StAnalysisMaker::Finish()
{
    //
    //  A good place for printout and to summarize
    //  the run.
    //
    cout << "StAnalysisMaker::Finish()\n";
    cout << "\tProcessed " << mEventCounter << " events." << endl;
    
    //
    //  Write Ntuple/histos to file and close it.
    //  
    mFile->Write();  
    mFile->Close();
    
    return kStOK;
}

//
//  This method is called every event. That's the
//  right place to plug in your analysis. 
//
Int_t
StAnalysisMaker::Make()
{
    mEventCounter++;  // increase counter
	
    //
    //	Get pointer to StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event) return kStOK;        // if no event, we're done

    //
    //  The following is only needed since the
    //  QA folks use this maker for their QA runs.
    //  You do not need this.
    //  
    summarizeEvent(*event, mEventCounter); 

    //
    //  See if this event survives the event filter.
    //  If not we stop here right away.
    //
    if (!accept(event)) return kStOK;   

    //
    //  Ok we survived the filter. Now it is time
    //  to do something with the event.
    //  In the following we simply fill the Ntuple.
    //
    int k = 0;
    float tuple[10];
    tuple[k++] = event->id();       // the event number
    
    tuple[k++] = event->primaryVertex()->position().x();    // x-vertex
    tuple[k++] = event->primaryVertex()->position().y();    // y-vertex
    tuple[k++] = event->primaryVertex()->position().z();    // z-vertex

    //
    //  Get the ZDC and CTB data.
    //  
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if (!theTriggers) return kStOK;      // good idea to check if the data is available at all
    StCtbTriggerDetector &theCtb = theTriggers->ctb();
    StZdcTriggerDetector &theZdc = theTriggers->zdc();

    //
    //  Sum all CTB counter
    float ctbsum = 0;
    for (unsigned int islat=0; islat<theCtb.numberOfSlats(); islat++) 
	for (unsigned int itray=0; itray<theCtb.numberOfTrays(); itray++)
	    ctbsum += theCtb.mips(itray, islat, 0);

    tuple[k++] = ctbsum;            // CTB
    tuple[k++] = theZdc.adcSum();   // ZDC
    
    //
    //  Count tracks
    //  This is just an example on how to use little
    //  helper functions which makes the code more readable.
    //
    tuple[k++] = countPrimaryTracks(*event);
    tuple[k++] = countPrimaryPions(*event);

    //
    //  Last but not least we count the number of good global
    //  tracks. Good or bad is determined by the track filter.
    //  The fraction of good globals gets stored in the tuple. 
    //
    int allGlobals = 0;
    int goodGlobals = 0;

    StTrack *track;
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (unsigned int j=0; j<nodes.size(); j++) {
	track = nodes[j]->track(global);
	if (track) allGlobals++;
	if (accept(track)) goodGlobals++;
    }
    tuple[k++] = static_cast<float>(goodGlobals)/allGlobals;

    //
    //  That's it.
    //  Store the current tuple. See you next event.
    //
    mTuple->Fill(tuple);

    return kStOK;
}

bool StAnalysisMaker::accept(StEvent* event)
{
    //
    //  This is a kind of very simple event filter.
    //  We select only events with a valid event vertex,
    //  i.e. event->primaryVertex() returns a non-zero pointer.
    // 
    return event->primaryVertex();
}

bool StAnalysisMaker::accept(StTrack* track)
{
    //
    //  This is a kind of very simple track filter.
    //  We only check for positive flags.
    //  Note that this method works for global and
    //  primary tracks since we deal with the base
    //  class only (StTrack).
    //
    return track && track->flag() >= 0;
}
