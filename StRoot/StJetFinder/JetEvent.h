/***************************************************************************
 *
 * $Id: JetEvent.h,v 1.1 2003/05/15 18:18:16 thenry Exp $
 * $Log: JetEvent.h,v $
 * Revision 1.1  2003/05/15 18:18:16  thenry
 * Creation of JetEvent:  This class is little more than a bare Jet structure
 * with the ability to do unformatted stream i/o, and formatted stream output.
 * Its greatest redeeming quality is that it has no dependencies on Star
 * or Root, so it can be freely used to access its own data on machines with
 * only a standard C++ distribution.
 *
 * Revision 1.0  2003/05/09 21:01:34  thenry
 * created
 *
 * Author: Thomas Henry May 2003
 ***************************************************************************
 *
 * Description: JetEvent handles file io for a simple jet data structure
 *
 ***************************************************************************/
#ifndef _JetEvent_h
#define _JetEvent_h
#include <iostream>
#include <string>
#include <vector>
#include <typeinfo>
using namespace std;

class EventSubStruct {
 public:
  EventSubStruct() : eventId(0), 
    xVertex(0), yVertex(0), zVertex(0), 
    npTracks(0) {};
  EventSubStruct(const EventSubStruct &copy) : eventId(copy.eventId),
    xVertex(copy.xVertex), yVertex(copy.yVertex), zVertex(copy.zVertex),
    npTracks(copy.npTracks) {};
  void clear(void) {};
  ostream& out(ostream &os)
    {
      os << "EventSubStruct" << endl;
      os << "eventId: " << eventId << endl;
      os << "xVertex: " << xVertex << endl;
      os << "yVertex: " << yVertex << endl;
      os << "zVertex: " << zVertex << endl;
      os << "npTracks: " << npTracks << endl;
      return os;
    }

  short eventId;
  double xVertex;
  double yVertex;
  double zVertex;
  short npTracks;
};

class EventStruct {
 public:
  EventStruct() {};
  EventStruct(const EventStruct &copy) : subEvent(copy.subEvent), 
    triggers(copy.triggers) {};
  void clear(void) { triggers.clear(); };
  void setTriggers(vector<int> trigs) {
    for(vector<int>::iterator it = trigs.begin(); it != trigs.end(); ++it)
      triggers.push_back(*it);
  };

  EventSubStruct subEvent;

  vector<short> triggers;
};

class TrackStruct {
 public:
  TrackStruct() {};
  TrackStruct(const TrackStruct &copy) : trackE(copy.trackE), 
    trackPhi(copy.trackPhi), trackEta(copy.trackEta), 
    isTpcTrack(copy.isTpcTrack) {};
  void clear(void) {};  
  ostream& out(ostream &os)
    {
      os << "TrackStruct:" << endl;
      os << "TrackE: " << trackE << endl;
      os << "TrackPhi: " << trackPhi << endl;
      os << "TrackEta: " << trackEta << endl;
      if(isTpcTrack) os << "Track is from TPC" << endl;
      return os;
    }
  
  double trackE;
  double trackPhi;
  double trackEta;
  bool isTpcTrack;
};

typedef vector<TrackStruct> TrackStructVec;

class JetSubStruct {
 public:
  JetSubStruct() {};
  JetSubStruct(const JetSubStruct &copy) : energy(copy.energy), px(copy.px),
    py(copy.py), pz(copy.pz), eta(copy.eta), phi(copy.phi), 
    numCharges(copy.numCharges), charge(copy.charge) {};
  void clear(void) {};
  ostream& out(ostream &os)
    {
      os << "JetSubStruct:" << endl;
      os << "energy: " << energy << endl;
      os << "px: " << px << endl;
      os << "py: " << py << endl;
      os << "pz: " << pz << endl;
      os << "eta: " << eta << endl;
      os << "phi: " << phi << endl;
      os << "numCharges: " << numCharges << endl;
      os << "charge: " << charge << endl;
      return os;
    }

  double energy;
  double px;
  double py;
  double pz;
  double eta;
  double phi;
  short numCharges;
  short charge;
};

class JetStruct {
 public:
  JetStruct() {};
  JetStruct(const JetStruct &copy) : jetname(copy.jetname), jet(copy.jet), 
    tracks(copy.tracks) {};
  void clear(void) { jetname.erase(); tracks.clear(); };

  string jetname;
  JetSubStruct jet;
  TrackStructVec tracks;
};

typedef vector<JetStruct> JetStructVec;

class JetEvent {
public:
    JetEvent() : 
      eventId(event.subEvent.eventId),
      xVertex(event.subEvent.xVertex),
      yVertex(event.subEvent.yVertex),
      zVertex(event.subEvent.zVertex),
      npTracks(event.subEvent.npTracks),
      
      jetname(jet.jetname),
      energy(jet.jet.energy),
      px(jet.jet.px),
      py(jet.jet.py),
      pz(jet.jet.pz),
      eta(jet.jet.eta),
      phi(jet.jet.phi),
      numCharges(jet.jet.numCharges),
      charge(jet.jet.charge),
 
      trackE(track.trackE),
      trackPhi(track.trackPhi),
      trackEta(track.trackEta),
      isTpcTrack(track.isTpcTrack) {};

    ~JetEvent();

    void clear(void) { triggers.clear(); event.clear(); 
      jets.clear(); tracks.clear(); };

public:
    EventStruct event;
    vector<int> triggers;

    JetStructVec jets;
    JetStruct jet;    

    TrackStructVec tracks;
    TrackStruct track;

    short &eventId;
    double &xVertex;
    double &yVertex;
    double &zVertex;
    short &npTracks;

    string &jetname;
    double &energy;
    double &px;
    double &py;
    double &pz;
    double &eta;
    double &phi;
    short &numCharges;
    short &charge;

    double &trackE;
    double &trackPhi;
    double &trackEta;
    bool &isTpcTrack;

    void push_track(void){
      tracks.push_back(track);
    }; 
    void push_jet(void){
      jet.tracks = tracks;
      jets.push_back(jet);
      tracks.clear();
    }; 
    void setTriggers(vector<short> newtriggers) {
      for(vector<short>::iterator it = newtriggers.begin(); 
	  it != newtriggers.end(); ++it)
	triggers.push_back(static_cast<short>(*it));
    };
};

ostream& write(ostream &os, JetEvent& jets);
istream& read (istream &is, JetEvent& jets);
ostream& write(ostream &os, EventStruct& event);
istream& read (istream &is, EventStruct& event);
ostream& write(ostream &os, JetStruct& jet);
istream& read (istream &is, JetStruct& jet);
ostream& write(ostream &os, TrackStruct& track);
istream& read (istream &is, TrackStruct& track);
ostream& write(ostream &os, short &toWrite);
istream& read(istream &is, short &toWrite);

template <class T>
ostream& write(ostream &os, vector<T> &toWrite)
{
  short size = toWrite.size();
  os.write(&size, sizeof(size));
  for(vector<T>::iterator it = toWrite.begin(); 
      it != toWrite.end(); ++it)
    {
      write(os, *it);
    }
  return os;
}

template <class T>
istream& read (istream &is, vector<T> &toRead)
{
  short size;
  is.read(&size, sizeof(size));
  for(int i = 0; i < size; i++)
    {
      static T input;
      read(is, input);
      toRead.push_back(input);
    }
  return is;
}

ostream& operator << (ostream &os, JetEvent& jets);
ostream& operator << (ostream &os, EventStruct& event);
ostream& operator << (ostream &os, JetStruct& jet);
ostream& operator << (ostream &os, TrackStruct& track);
ostream& operator << (ostream &os, EventSubStruct& subEvent);
ostream& operator << (ostream &os, JetSubStruct& jet);

template <class T>
ostream& operator << (ostream &os, vector<T> &toWrite)
{
  short size = toWrite.size();
  os << typeid(T).name() << " (Size: " << size << ")" << endl;
  for(vector<T>::iterator it = toWrite.begin(); 
      it != toWrite.end(); ++it)
    {
      os << (*it);
    }
  return os;
}

#endif









