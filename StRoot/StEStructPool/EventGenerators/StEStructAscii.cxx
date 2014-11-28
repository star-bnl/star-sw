/**********************************************************************
 *
 * $Id: StEStructAscii.cxx
 *
 * Author: msd
 *
 **********************************************************************
 *
 * Description:  
 *
 **********************************************************************/
#include "StEStructAscii.h"
#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "TLorentzVector.h"

StEStructAscii::StEStructAscii(): meventCount(0), meventsToDo(0), mAmDone(false) {};

StEStructAscii::StEStructAscii(int nevents, char* infile, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

  meventsToDo=nevents;
  mECuts=ecuts;
  mTCuts=tcuts;

  cout << "Opening ASCII file " << infile << endl;
  in.open(infile); 
  mlineNumber = 0;

  if (!in) cout << " *** WARNING: Error reading file " << infile << endl;

};

bool StEStructAscii::hasInputFile() { return (in.good()) ? true : false ; };

//-------------------------------------------------------------------------
StEStructEvent* StEStructAscii::next() {

  if( (!in.good()&&meventCount>0) || ((meventsToDo>0)&&(meventCount==meventsToDo)) || in.eof() ){  // quit on read error, event limit, or eof   
    mAmDone=true;
    in.close();  // Close the ifstream when finished
    cout << "Done.  Quitting after " << meventCount << " events." << endl; 
    return (StEStructEvent*)NULL;
  }
  return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructAscii::generateEvent(){

  StEStructEvent* retVal=NULL;

  retVal = new StEStructEvent();

  fillTracks(retVal);

  /*
  bool useEvent=mECuts->goodCentrality((float)mnumTracks);
  if(!useEvent){
    cout << "Cutting event with " << mnumTracks << " tracks." << endl; 
    delete retVal;
    retVal=NULL;
  } else {
    retVal->FillChargeCollections();
    meventCount++;
  }
  mECuts->fillHistogram(mECuts->centralityName(),(float)mnumTracks,useEvent);
  */

  retVal->FillChargeCollections();                                                                                                                                              
  retVal->SetEventID(meventCount);
  retVal->SetVertex(0,0,0);
  retVal->SetBField(0);

  meventCount++;                      

  return retVal;
}   

//--------------------------------------------------------------------------
void StEStructAscii::fillTracks(StEStructEvent* estructEvent){

  mnumTracks=0;

  float pt, eta, phi;
  

  int numParticles, charge;
  in >> numParticles;  // Read # of particles in this event
  mlineNumber++;               // Increment line number
  if (in.eof())  {             // Do some error checking
    cout << "Found EOF" << endl;
    return;
  }
  if (!in.good())  {
    cout << "WARNING: Read error at line" << mlineNumber << endl;
    return;
  }
  //cout << "Reading " << numParticles << " particles" << endl;  // ***TEST***

  estructEvent->SetCentrality(numParticles);

  StEStructTrack* eTrack= new StEStructTrack();

  for(int i=0;i<numParticles;i++){   // Loop over particles
    eTrack->SetInComplete();

    // Making new format: pt, eta, phi, charge
    in >> pt >> eta >> phi >> charge;
    mlineNumber+=1;
    if (!in.good()) {   // Check for error
      cout << "WARNING: Read error near line" << mlineNumber << endl;
      return;
    }

    TLorentzVector track;  // for finding components
    track.SetPtEtaPhiM(pt, eta, phi, 0.14);

    eTrack->SetPx(track.Px());
    eTrack->SetPy(track.Py());
    eTrack->SetPz(track.Pz());
    eTrack->SetEta(eta);
    eTrack->SetPhi(phi);
    eTrack->SetCharge(charge);

    // Not doing any track or event cuts, but lets fill some cut hists
    bool useTrack=true;
    useTrack = (mTCuts->goodCharge(charge) && useTrack);
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    mTCuts->fillHistograms(true);

    //cout << "Adding track:\t" << pt<<"\t"<< eta << "\t" << phi << "\t" << charge << endl;
    estructEvent->AddTrack(eTrack);
  } // for

  delete eTrack;
  return;

}    




/**********************************************************************
 *
 * $Log: StEStructAscii.cxx,v $
 * Revision 1.5  2012/11/16 21:23:18  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.4  2007/01/26 17:19:33  msd
 * Total rewrite.  Now uses simple input text file format of pt,eta,phi for each particle.
 *
 * Revision 1.3  2006/02/22 22:05:33  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.2  2005/09/14 17:18:38  msd
 * Setting numEvents to 0 now does all events in file
 *
 * Revision 1.1  2004/06/28 19:54:54  msd
 * Initial check-in
 *
 * Revision 1.1 
 * 
 *
 *
 *********************************************************************/
