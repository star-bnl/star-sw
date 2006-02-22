/**********************************************************************
 *
 *  StEStructAscii.cxx
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

StEStructAscii::StEStructAscii(): meventCount(0), meventsToDo(0), mAmDone(false), mECuts(0), mTCuts(0){};

StEStructAscii::StEStructAscii(int nevents, char* infile, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

  meventsToDo=nevents;
  mECuts=ecuts;
  mTCuts=tcuts;

  cout << "Opening file " << infile << endl;
  in.open(infile); 
  mlineNumber = 0;

  if (!in) cout << " *** WARNING: Error reading file " << infile << endl;

};

bool StEStructAscii::hasInputFile() { return (in.good()) ? true : false ; };
bool StEStructAscii::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructAscii::hasTrackCuts() { return (mTCuts) ? true : false ; }


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

  return retVal;
}   

//--------------------------------------------------------------------------
void StEStructAscii::fillTracks(StEStructEvent* estructEvent){

  // See protected/estruct/msd/humanic for this file format
  // All we really read is momentum px,py,pz

  mnumTracks=0;

  // Quantities read from input file; most of them are ignored
  float x,y,z,t,mass,px,py,pz,tchem; 
  int res;

  int numParticles, temp;
  in >> numParticles >> temp;  // Read # of particles in this event
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

  StEStructTrack* eTrack= new StEStructTrack();

  for(int i=0;i<numParticles;i++){   // Loop over particles
    eTrack->SetInComplete();

    in >> x >> y >> z >> t >> mass >> res >> px >> py >> pz;   // Read track data
    in >> tchem; 
    mlineNumber+=2;
    //cout << "X: " << x << " pz: " << pz << endl;  // ***TEST***
    if (!in.good()) {   // Check for error
      cout << "WARNING: Read error near line" << mlineNumber << endl;
      return;
    }

    float p[3];   // Set momentum, convert MeV to GeV
    p[0] = px/1000;  p[1] = py/1000;  p[2] = pz/1000;  
    mass /= 1000;

    float pt=sqrt(p[0]*p[0]+p[1]*p[1]);
    //if(pt<0.15)continue;  // Letting the cut file handle this 

    bool useTrack=true;
    
    float energy = sqrt( (p[0]*p[0]+p[1]*p[1]+p[2]*p[2]) + (mass*mass) );  // E^2 = p^2 + m^2
    float num=energy+p[2];  // Calculate Eta
    float den=energy-p[2];
    float eta=-999.;  
    if(den!=0.) { 
       float arg=num/den;
       if(arg>0.) eta=0.5*log(arg);
    }
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    float phi=atan2((double)p[1], (double)p[0]);  // Calculate Phi
    useTrack=(mTCuts->goodPhi(phi) && useTrack);

    useTrack=(mTCuts->goodPt(pt) && useTrack);
 
    float _r=pt/0.139;  // Calculate Yt
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);

    if(useTrack)mnumTracks++;  

    mTCuts->fillHistograms(useTrack);
    
    eTrack->SetBx(0);  // Unsure what to do with these...
    eTrack->SetBy(0);
    eTrack->SetBz(0);
    eTrack->SetBxGlobal(0);
    eTrack->SetByGlobal(0);
    eTrack->SetBzGlobal(0);

    //if (!useTrack) cout << "Tracked failed pt, eta, or phi.  Pt: " << pt << " Eta: " << eta << " Phi: " << phi << endl;  
    if(!useTrack) continue;  
  
    eTrack->SetPx(p[0]);
    eTrack->SetPy(p[1]);
    eTrack->SetPz(p[2]);
    eTrack->SetEta(eta);
    eTrack->SetPhi(phi);

    // We must assign charge randomly for this generator
    temp = mnumTracks%2;  // alternate between + and -
    if (temp==0) temp=-1;
    //cout << "Setting Charge " << temp << endl;  // ***TEST***
    eTrack->SetCharge(temp);
    //cout << "Adding good track" << endl;  // ***TEST***
    estructEvent->AddTrack(eTrack);
  } // for

  delete eTrack;
  return;

}    


//--------------------------------------------------------------------------
void StEStructAscii::setEventCuts(StEStructEventCuts* cuts){

  if(mECuts) delete mECuts;
  mECuts=cuts;

};

//---------------------------------------------------------------
void StEStructAscii::setTrackCuts(StEStructTrackCuts* cuts){
  if(mTCuts) delete mTCuts;
  mTCuts=cuts;
}




/**********************************************************************
 *
 * $Log: StEStructAscii.cxx,v $
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
