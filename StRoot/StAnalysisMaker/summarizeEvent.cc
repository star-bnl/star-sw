/***************************************************************************
 *
 * $Id: summarizeEvent.cc,v 2.1 1999/11/16 12:28:44 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a function which performs
 *               some simple analysis using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: summarizeEvent.cc,v $
 * Revision 2.1  1999/11/16 12:28:44  ullrich
 * Corrected typo and added print-out of number of primary tracks.
 *
 * Revision 2.1  1999/11/16 12:28:44  ullrich
 * Corrected typo and added print-out of number of primary tracks.
 *
 * Revision 2.0  1999/11/04 16:10:11  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "StEventTypes.h"
#include "StMessMgr.h"

static const char rcsid[] = "$Id: summarizeEvent.cc,v 2.1 1999/11/16 12:28:44 ullrich Exp $";

void
summarizeEvent(StEvent& event, Int_t &nevents)
{
    nevents++;
    gMessMgr->QAInfo() << "StAnalysisMaker,  Reading Event: " << nevents
		       << "  Type: " << event.type()
		       << "  Run: " << event.runId() << endm;
    
    gMessMgr->QAInfo() << "# track nodes:         "
		       << event.trackNodes().size() << endm;

    int nprimary = 0;
    if (event.primaryVertex())
	nprimary = event.primaryVertex()->numberOfDaughters();
    gMessMgr->QAInfo() << "# primary tracks:         "
		       << nprimary << endm;
    
    gMessMgr->QAInfo() << "# V0 vertices:       "
		       << event.v0Vertices().size() << endm;

    gMessMgr->QAInfo() << "# Xi vertices:       "
		       << event.xiVertices().size() << endm;
    
    gMessMgr->QAInfo() << "# Kink vertices:       "
		       << event.kinkVertices().size() << endm;
    
    gMessMgr->QAInfo() << "# TPC hits:       "
		       << (event.tpcHitCollection() ? event.tpcHitCollection()->numberOfHits() : 0) << endm;
    
    gMessMgr->QAInfo() << "# SVT hits:       "
		       << (event.svtHitCollection() ? event.svtHitCollection()->numberOfHits() : 0) << endm;
    
    gMessMgr->QAInfo() << "# FTPC hits:      "
		       << (event.ftpcHitCollection() ? event.ftpcHitCollection()->numberOfHits() : 0) << endm;
    
    if (event.primaryVertex()) {
	gMessMgr->QAInfo() << "primary vertex:   "
			   << event.primaryVertex()->position() << endm;
    }
}
