/***************************************************************************
 *
 * $Id: JetEvent.cxx,v 1.1 2003/05/15 18:18:16 thenry Exp $
 * $Log: JetEvent.cxx,v $
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
#include "JetEvent.h"

ostream& write(ostream &os, JetEvent& jets)
{
  jets.event.setTriggers(jets.triggers);
  write(os,jets.event);
  write(os,jets.jets);
  return os;
}

istream& read(istream &is, JetEvent& jets)
{
  read(is,jets.event);
  read(is,jets.jets);
  jets.setTriggers(jets.event.triggers);
  return is;
}

ostream& write(ostream &os, EventStruct& event)
{
  os.write(&event.subEvent, sizeof(event.subEvent));
  return write(os,event.triggers);
}

istream& read(istream &is, EventStruct& event)
{
  is.read(&event.subEvent, sizeof(event.subEvent));
  return read(is,event.triggers);
}

ostream& write(ostream &os, TrackStruct& track)
{
  return os.write(&track, sizeof(track));
}

istream& read(istream &is, TrackStruct& track)
{
  return is.read(&track, sizeof(track));
}

ostream& write(ostream &os, JetStruct &toWrite)
{
  short size = toWrite.jetname.size();
  os.write(&size, sizeof(size));
  os.write(toWrite.jetname.c_str(), size);
  os.write(&toWrite.jet, sizeof(toWrite.jet));
  return write(os,toWrite.tracks);
}

istream& read(istream &is, JetStruct &toRead)
{
  short size;
  is.read(&size, sizeof(size));
  if(toRead.jetname.size() < static_cast<unsigned short>(size) ) 
    toRead.jetname.resize(size);
  for(int i = 0; i < size; i++)
    {
      char input;
      is.read(&input, sizeof(input));
      toRead.jetname[i] = input;
    }
  is.read(&toRead.jet, sizeof(toRead.jet));
  return read(is,toRead.tracks);
}

ostream& write(ostream &os, short &toWrite)
{
  return os.write(&toWrite, sizeof(toWrite));
}

istream& read(istream &is, short &toWrite)
{
  return is.read(&toWrite, sizeof(toWrite));
}

/*
ostream& write(ostream &os, JetStructVec &toWrite)
{
  short size = toWrite.size();
  os.write(&size, sizeof(size));
  for(JetStructVec::iterator it = toWrite.begin(); it != toWrite.end(); ++it)
    {
      write(os,*it);
    }
  return os;
}

istream& read(istream &is, JetStructVec &toRead)
{
  short size;
  is.read(&size, sizeof(size));
  for(int i = 0; i < size; i++)
    {
      JetStruct input;
      read(is, input);
      toRead.push_back(input);
    }
  return is;
}
*/

ostream& operator << (ostream &os, JetEvent& jets)
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






