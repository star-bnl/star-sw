/*!
  \class  Messenger Provides organized informational message routing.
  \author Ben Norman (Kent State)
*/

#ifndef MESSENGER_H
#define MESSENGER_H

#include <iostream.h>
#include <map>
#include <fstream.h>
#include <string>

#include "MessengerBuf.h"

/// These message types can be ORed into routing codes.
/// When you add a new routing code, make sure it is 2 times
/// the next highest code.  Also increment g_nMessageTypes
/// and add the address of the default stream to g_apMessageOstreams.
static const unsigned int kHitMessage   = 1;
static const unsigned int kTrackMessage = 2;
static const unsigned int kNodeMessage  = 4;
static const unsigned int kDetectorMessage = 8;
static const unsigned int kGeometryMessage = 16;
static const unsigned int kSeedFinderMessage = 32;

/// Number of message types
static const int g_nMessageTypes = 6;
/// Output streams corresponding to the message types
static ostream *g_apMessageOstreams[g_nMessageTypes] = 
{ &cout, &cout, &cout, &cout, &cout, &cout};
/// Human readable names for message types
static const char *g_apMessageNames[g_nMessageTypes] =
{"Hit", "Track", "Node", "Detector", "Geometry", "SeedFinder"};

/// Typedefs for containers
typedef map<unsigned int, Messenger*> messengerMap;
typedef messengerMap::const_iterator messengerMapIterator;
typedef messengerMap::value_type messengerMapValueType;

class Messenger: public ostream{

public:

    /// Return a Messenger instance corresponding to the given routing code,
    /// or all routes allowed by the global mask if no routing is specified.
    static Messenger *instance(unsigned int routing=0);

    /// Set the global routing mask which tells which messages are actually
    /// delivered to a given stream.  This mask is AND-ed with the routing
    /// code of a given Messenger to determine if a message should be
    /// printed.  Returns the original routing mask.
    static unsigned int setRoutingMask(unsigned int routing){
      return MessengerBuf::setRoutingMask(routing);
    }
    /// Returns the global routing mask
    static unsigned int getRoutingMask(){
      return MessengerBuf::getRoutingMask();
    }
    /// Sets only the given bits in the global routing mask.
    /// Returns the original state of the bits.
    static unsigned int setRoutingBits(unsigned int messages){
      unsigned int oldRouting = MessengerBuf::getRoutingMask();
      MessengerBuf::setRoutingMask(oldRouting | messages);
      return oldRouting & messages;
    }
    /// Clears only the given bits in the global routing mask.
    /// Returns the original state of the bits.
    static unsigned int clearRoutingBits(unsigned int messages){
      unsigned int oldRouting = MessengerBuf::getRoutingMask();
      MessengerBuf::setRoutingMask(oldRouting & (~messages));
      return oldRouting & messages;
    }
    /// Returns the given bits in the global routing mask.
    static unsigned int getRoutingBits(unsigned int messages){
      unsigned int oldRouting = MessengerBuf::getRoutingMask();
      return oldRouting & messages;
    }
    /// Returns the number of message types
    static int nMessageTypes(){ return g_nMessageTypes; }

    /// Returns the human-readable name for the message type
    static string getMessageName(unsigned int message){ 
      int iMessageType = 0;
      while( (message /= 2) > 0 ){ iMessageType++; }
      if(iMessageType<g_nMessageTypes){
        return string(g_apMessageNames[iMessageType]);
      }
      return string();
    }

    /// Initialize the output streams for the message maps.  This must be
    /// called before using a Messenger.  If a routing code is specified,
    /// it is used as the global routing mask.
    static void init(unsigned int routing=0);

    /// Delete any created Messenger & ofstream objects.  
    /// This must be called after messenging is finished.
    static void kill();

    /// Change the message stream for the given routing bit,
    /// deleting the old one if appropriate.  If routing is not
    /// a power of 2, uses the highest bit.
    static void setMessageOstream(unsigned int routing, ostream *pNewStream);

    /// Destructor;
    virtual ~Messenger(){}

protected:

    /// Construct a Messenger with a MessengerMap using the given routing.
    Messenger(unsigned int routing=0):
            ostream(new MessengerBuf(
                routing==0 ? MessengerBuf::getRoutingMask() : routing)){}
    /// static map of Messenger instances indexed by routing code.
    static messengerMap s_messengerMap;

};


#endif  
