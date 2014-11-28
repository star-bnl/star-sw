/**********************************************************************
 *
 * $Id: StEStructAnalysis.cxx,v 1.2 2006/04/04 22:05:03 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract analysis class
 *
 ***********************************************************************/
#include "StEStructAnalysis.h"

ClassImp(StEStructAnalysis)

StEStructAnalysis::StEStructAnalysis(){ manalysisIndex=-1;};

/***********************************************************************
 *
 * $Log: StEStructAnalysis.cxx,v $
 * Revision 1.2  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
