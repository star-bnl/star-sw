/***************************************************************************
 *
 * $Id: StJetOutputMaker.cxx,v 1.2 2003/05/28 23:46:06 thenry Exp $
 * 
 * Author: Thomas Henry May 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a binary output file of the jets and
 * some other basic information like trigger and event ids.  It contains
 * the jetevent class.  If __ROOT__ is not defined, this file simplifies
 * to only the jetevent class.
 *
 ***************************************************************************
 */

#ifdef __ROOT__
#include <iostream>
#include <fstream>
#include "TLorentzVector.h"
#include "StSpinMaker/StJets.h"
#include "StJetMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
//#include "StSpinMaker/StJet.h"
#endif //__ROOT__
#include "StJetOutputMaker.h"
const string eofException::Msg = "End of File has been reached." ; 

#ifdef __ROOT__
ClassImp(StJetOutputMaker)

StJetOutputMaker::StJetOutputMaker(const char* name, StMuDstMaker* uDstMaker,
				   StJetMaker *stjetmaker, 
				   const char *outputName, 
				   StEmcTpcFourPMaker* emcTpcFourPMaker = NULL)
  : StMaker(name), muDstMaker(uDstMaker), jetMaker(stjetmaker), 
    ofilename(outputName), fourPMaker(emcTpcFourPMaker), ofile(NULL){
}

Int_t StJetOutputMaker::Init() 
{
  if(ofile == NULL)
    ofile = new ofstream(ofilename.c_str(), ofstream::out | ofstream::trunc);
  eventsProcessed = 0;
  return StMaker::Init();
}

Int_t StJetOutputMaker::Make() {
  cout <<" Start StJetOutputMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  oJetEvent.clear();
  // First basic event information:
  StMuDst* muDst = muDstMaker->muDst();
  StMuEvent* muEvent = muDst->event();
  oJetEvent.event.subEvent.eventId = muEvent->eventId();
  const vector<UInt_t> &triggers = 
    muEvent->triggerIdCollection().nominal().triggerIds();
  oJetEvent.triggers.clear();
  for(vector<UInt_t>::const_iterator it = triggers.begin(); 
      it != triggers.end(); ++it)
    oJetEvent.triggers.push_back(static_cast<int>(*it));
  oJetEvent.event.subEvent.xVertex = muEvent->primaryVertexPosition().x();
  oJetEvent.event.subEvent.yVertex = muEvent->primaryVertexPosition().y();
  oJetEvent.event.subEvent.zVertex = muEvent->primaryVertexPosition().z();
  int numberPrimaryTracks = muDst->numberOfPrimaryTracks();
  oJetEvent.event.subEvent.npTracks = numberPrimaryTracks;
  StMuEmcCollection* muEmc = NULL;
  if(fourPMaker != NULL)
    muEmc = fourPMaker->getMuEmcCollection();

  // Then the jets (if any)
  StJetMaker::jetBranchesMap &jetAnalyzers = jetMaker->getJets();
  for(StJetMaker::jetBranchesMap::iterator it = jetAnalyzers.begin(); 
      it != jetAnalyzers.end(); ++it)
    {
      oJetEvent.jet.jetname = (*it).first;
      StppJetAnalyzer* ana = (*it).second;
      StJets* muDstJets = ana->getmuDstJets();
      int nJets = muDstJets->nJets();
      cout << "Number Jets " << nJets << endl;
      for(int i = 0; i < nJets; ++i)
	{
	  vector<int> trackIndices = muDstJets->jetTrackIndices(i);
	  TLorentzVector* jet = dynamic_cast<TLorentzVector*> 
	    ((muDstJets->jets())->UncheckedAt(i));
	  oJetEvent.jet.jet.energy = muDstJets->e(i);
	  oJetEvent.jet.jet.px = jet->Px();
	  oJetEvent.jet.jet.py = jet->Py();
	  oJetEvent.jet.jet.pz = jet->Pz();
	  oJetEvent.jet.jet.eta = jet->Eta();
	  oJetEvent.jet.jet.phi = jet->Phi();
	  oJetEvent.jet.jet.numCharges = muDstJets->nCell(i);
	  oJetEvent.jet.jet.charge = muDstJets->charge(i);
	  for(vector<int>::iterator trackit = trackIndices.begin();
	      trackit != trackIndices.end(); ++trackit)
	    {
	      if(*trackit >= numberPrimaryTracks)
		if(muEmc)
		  {
		    StMuEmcPoint* muPoint = muEmc->getPoint(*trackit);
		    StCorrectedEmcPoint &point = fourPMaker->binmap.
		      moddPoints[muPoint];
		    oJetEvent.track.trackE = point.E();
		    oJetEvent.track.trackPhi = point.Phi();
		    oJetEvent.track.trackEta = point.Eta();
		    oJetEvent.track.isTpcTrack = false;
		    oJetEvent.push_track();
		    continue;
		  }
	      StMuTrack* muTrack = muDst->primaryTracks(*trackit);
	      tempTrack.initProbabilities(muTrack);
              oJetEvent.track.trackE = 
		sqrt(tempTrack.masssqr() + muTrack->p().mag2());
	      oJetEvent.track.trackPhi = muTrack->phi();
	      oJetEvent.track.trackEta = muTrack->eta();
	      oJetEvent.track.isTpcTrack = true;
	      oJetEvent.push_track();
	    }
	  oJetEvent.push_jet();
	}
    }
  ofstream &jetFile = *ofile;
  cout << oJetEvent;
  write(jetFile, oJetEvent);
  cout << "Event number " << ++eventsProcessed << " Processed." << endl;

  /*
  // check for debugging
  string outname("out.txt");
  ofstream outfile(outname.c_str(), ofstream::ate);
  outfile << oJetEvent;
  outfile.close();
  
  string outputtxt("output.txt");
  ofstream loopback(outputtxt.c_str(), ofstream::trunc);
  loopback.seekp(0, ios::beg);
  write(loopback, oJetEvent);
  oJetEvent.clear();
  loopback.close();
  fstream loopin(outputtxt.c_str(), fstream::in);
  loopin.seekg(0, ios::beg);
  read(loopin, oJetEvent);
  loopin.close();

  string oioname("oio.txt");
  ofstream oio(oioname.c_str(), ofstream::ate);
  oio << oJetEvent;
  oio.close();
  */

  return kStOk;
}

Int_t StJetOutputMaker::Finish()
{
  ofile->close();
  delete ofile;
  return StMaker::Finish();
}
#endif // __ROOT__

#ifdef __ROOT__
ClassImp(PJetEvent)
#endif // __ROOT__

ostream& write(ostream &os, PJetEvent& jets)
{
  jets.event.setTriggers(jets.triggers);
  write(os,jets.event);
  write(os,jets.jets);
  return os;
}

istream& read(istream &is, PJetEvent& jets)
{
  read(is,jets.event);
  read(is,jets.jets);
  jets.setTriggers(jets.event.triggers);
  return is;
}

ostream& write(ostream &os, EventStruct& event)
{
  write(os, reinterpret_cast<char*>(&event.subEvent), sizeof(event.subEvent));
  return write(os,event.triggers);
}

istream& read(istream &is, EventStruct& event)
{
  read(is, reinterpret_cast<char*>(&event.subEvent), sizeof(event.subEvent));
  return read(is,event.triggers);
}

ostream& write(ostream &os, TrackStruct& track)
{
  return write(os, reinterpret_cast<char*>(&track), sizeof(track));
}

istream& read(istream &is, TrackStruct& track)
{
  return read(is, reinterpret_cast<char*>(&track), sizeof(track));
}

ostream& write(ostream &os, JetStruct &toWrite)
{
  short size = toWrite.jetname.size();
  write(os, reinterpret_cast<char*>(&size), sizeof(size));
  write(os, toWrite.jetname.c_str(), size);
  write(os, reinterpret_cast<char*>(&toWrite.jet), sizeof(toWrite.jet));
  return write(os,toWrite.tracks);
}

istream& read(istream &is, JetStruct &toRead)
{
  toRead.tracks.clear();
  short size = 0;
  read(is, reinterpret_cast<char*>(&size), sizeof(size));
  if(toRead.jetname.size() < static_cast<unsigned short>(size) ) 
    toRead.jetname.resize(size);
  for(int i = 0; i < size; i++)
    {
      char input;
      read(is, &input, sizeof(input));
      toRead.jetname[i] = input;
    }
  read(is, reinterpret_cast<char*>(&toRead.jet), sizeof(toRead.jet));
  return read(is,toRead.tracks);
}

ostream& write(ostream &os, short &toWrite)
{
  return write(os, reinterpret_cast<char*>(&toWrite), sizeof(toWrite));
}

istream& read(istream &is, short &toRead)
{
  return read(is, reinterpret_cast<char*>(&toRead), sizeof(toRead));
}

ostream& write(ostream &os, const char *toWrite, int size)
{
  int pos = os.tellp();
  os.write(toWrite, size);
  pos += size;
  if(os.tellp() != pos)
    os.seekp(pos, ios::beg);
  return os;
}

istream& read(istream &is, char *toRead, int size)
{
  int pos = is.tellg();
  is.read(toRead, size);
  if(is.eof()) throw eofException();
  pos += size;
  if(is.tellg() != pos)
    is.seekg(pos, ios::beg);
  return is;
}

ostream& operator << (ostream &os, PJetEvent& jets)
{
  os << "Printing Jet Info." << endl;
  jets.event.setTriggers(jets.triggers);
  return os << jets.event << jets.jets << endl;
}

ostream& operator << (ostream &os, EventStruct& event)
{
  return os << event.subEvent << event.triggers << endl;
}

ostream& operator << (ostream &os, JetStruct& jet)
{
  os << jet.jetname.c_str() << endl;
  os << jet.jet << endl;
  return os << jet.tracks << endl;
}

ostream& operator << (ostream &os, TrackStruct& track)
{
  return track.out(os);
}

ostream& operator << (ostream &os, EventSubStruct& subEvent)
{
  return subEvent.out(os);
}

ostream& operator << (ostream &os, JetSubStruct& jet)
{
  return jet.out(os);
}


