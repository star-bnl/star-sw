/***************************************************************************
 *
 * $Id: StJetOutputMaker.h,v 1.1 2003/05/15 18:11:19 thenry Exp $
 * $Log: StJetOutputMaker.h,v $
 * Revision 1.1  2003/05/15 18:11:19  thenry
 * Creation of StJetOutputMaker: this maker takes jets from the StJetMaker, with
 * general event information, and fills the JetEvent structure, and then
 * saves the JetEvent structure to disk.
 *
 * Revision 1.0  2003/05/09 21:01:34  thenry
 * created
 *
 * Author: Thomas Henry May 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a binary output file of the jets and
 * some other basic information like trigger and event ids.  It utilizes
 * the JetEvent class.
 *
 ***************************************************************************/
#ifndef StJetOutputMaker_h
#define StJetOutputMaker_h
#include <map>
#include <fstream>
#include <string>
#include "StEmcTpcFourPMaker.h"
#include "StJetMaker.h"
#include "JetEvent.h"

class StJetOutputMaker : public StMaker {
public:
    StJetOutputMaker(const char *name, StMuDstMaker *uDstMaker, 
		     StJetMaker *stjetmaker, const char *outputName,
		     StEmcTpcFourPMaker* emcTpcFourPMaker);
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

protected:
    StMuDstMaker *muDstMaker;
    StJetMaker *jetMaker;
    string ofilename;
    StEmcTpcFourPMaker* fourPMaker;
    JetEvent oJetEvent;
    ofstream* ofile;
    StProjectedTrack tempTrack;

    ClassDef(StJetOutputMaker,1)
};
#endif
