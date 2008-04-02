//
//  This is a STAR typical comment header. You should modify
//  it to reflect your changes.
//  As a minimum it should contain the name of the author, the
//  date it was written/modified, and a short description of what
//  the class is meant to do. The cvs strings $X$ (where X=Id, Log)
//  are not needed when you do not intend to put the file under
//  cvs control. Remove them.
//  
/*!
 * \class  StAnalysisMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 *
 * This is an example of a maker to perform analysis using StEvent.
 * Use this as a template and customize it for your studies.
 *
 * $Id: StAnalysisMaker.cxx,v 2.9 2008/04/02 23:15:35 fisyak Exp $
 *
 */


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


/// The constructor. Initialize you data members here.
StAnalysisMaker::StAnalysisMaker(const Char_t *name) : StMaker(name)
{
    mEventCounter = 0;
    mFile = 0;
    mTuple = 0;
}



/*!
 *
 *  Usually ok to leave this as it is, the destructor should
 *  however free/delete private data allocated in other part
 *  of the code.
 *
 */
StAnalysisMaker::~StAnalysisMaker() { /* noop */ }



/*!
 *
 * Called once at the beginning.
 * This is a good place to book histos and tuples.
 *
 */
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
    gMessMgr->Info() << "StAnalysisMaker::Init(): "
		     << "Histograms will be stored in file '"
		     <<  mFileName.c_str() << "'" << endm;
    
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

/*!
 *
 *  Called every event after Make(). Usually you do not
 *  need to do anything here. Leave it as it is.
 *
 */
void
StAnalysisMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

/*!
 *
 * Called once at the end.
 *
 */
Int_t
StAnalysisMaker::Finish()
{
    //
    //  A good place for printout and to summarize
    //  the run.
    //
    gMessMgr->Info() << "StAnalysisMaker::Finish() "
		     << "Processed " << mEventCounter << " events." << endm;
    
    //
    //  Write Ntuple/histos to file and close it.
    //  
    if( mFile){
      mFile->Write();  
      mFile->Close();
    }
    
    return kStOK;
}

/*!
 *  This method is called every event. That's the
 *  right place to plug in your analysis. 
 */
Int_t
StAnalysisMaker::Make()
{
    mEventCounter++;  // increase counter
	
    //
    //	Get pointer to StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event){
      gMessMgr->Warning() << "StAnalysisMaker::Make : No StEvent" << endm;
      return kStOK;        // if no event, we're done
    }

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
    if (!accept(event)){
      gMessMgr->Warning() << "StAnalysisMaker::Make : Event was not accepted" << endm;
      return kStOK;
    }

    //
    //  Ok we survived the filter. Now it is time
    //  to do something with the event.
    //  In the following we simply fill the Ntuple.
    //
    int k = 0;
    float tuple[10];
    tuple[k++] = event->id();                               // the event number
    
    tuple[k++] = event->primaryVertex()->position().x();    // x-vertex
    tuple[k++] = event->primaryVertex()->position().y();    // y-vertex
    tuple[k++] = event->primaryVertex()->position().z();    // z-vertex


    // Y3 trigger Id dump
    StTriggerIdCollection *trgcol = event->triggerIdCollection();
    if ( ! trgcol ){
      gMessMgr->Warning() << "StAnalysisMaker::Make : No triggerIdCollection" << endm;
    } else {
      const StTriggerId *l1 = trgcol->l1();
      const StTriggerId *l2 = trgcol->l2();
      const StTriggerId *l3 = trgcol->l3();
      const StTriggerId *nominal = trgcol->nominal();

      if(l1) {
	vector<unsigned int> l1Vec = l1->triggerIds();
	cout << "L1: Mask " <<l1->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l1Vec.begin();
	     viter != l1Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      if(l2) {
	vector<unsigned int> l2Vec = l2->triggerIds();
	cout << "L2: Mask " <<l2->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l2Vec.begin();
	     viter != l2Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      if(l3) {
	vector<unsigned int> l3Vec = l3->triggerIds();
	cout << "L3: Mask " <<l3->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l3Vec.begin();
	     viter != l3Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      
      if(nominal) {
	vector<unsigned int> nominalVec = nominal->triggerIds();
	cout << "NOMINAL: Mask " <<nominal->mask() << " " ;
	for (vector<unsigned int>::iterator viter = nominalVec.begin();
	     viter != nominalVec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
    }


    //
    //  Get the ZDC and CTB data.
    //  
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if (!theTriggers){
      // good idea to check if the data is available at all
      gMessMgr->Warning() << "StAnalysisMaker::Make : no triggerDetectorCollection" << endm;
      return kStOK;           
    }
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
    if (allGlobals) tuple[k++] = static_cast<float>(goodGlobals)/allGlobals;
    else            tuple[k++] = -1;
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




/* -------------------------------------------------------------------------
 * $Log: StAnalysisMaker.cxx,v $
 * Revision 2.9  2008/04/02 23:15:35  fisyak
 * Add protection against allGlobals == 0
 *
 * Revision 2.8  2004/02/04 01:36:40  jeromel
 * Minor change for user's education. Use of gMessMgr
 *
 * Revision 2.7  2004/02/01 18:01:53  jeromel
 * A few message addition
 *
 * Revision 2.6  2003/03/20 00:29:19  jeromel
 * Calling Wite() on 0x0 pointer
 *
 * Revision 2.5  2003/02/27 15:25:36  jeromel
 * Missing check on triggerIdCollection() now added
 *
 * Revision 2.4  2003/02/18 22:19:09  jeromel
 * Added dump of Y3 triggers
 *
 * Revision 2.3  2002/04/28 00:10:27  jeromel
 * doxygen basic dox added. GetCVS() had wrong signature : corrected to avoid
 * propagation of this typo in new makers.
 *
 * Revision 2.2  2000/07/12 05:23:28  ullrich
 * Updated for better use as template for actual analysis.
 *
 * Revision 2.1  1999/12/30 01:54:57  ogilvie
 * added countPrimaryPions as example how to use PID
 *
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 * -------------------------------------------------------------------------
 */

