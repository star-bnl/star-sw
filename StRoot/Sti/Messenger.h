/*!
  \class  Messenger Provides organized informational message routing.
  \author Ben Norman (Kent State)
*/

#ifndef MESSENGER_H
#define MESSENGER_H

#include <ostream.h>
#include <fstream.h>
#include <strstream.h>
#include <map>
#include <vector>
#include <typeinfo>
#include <stdio.h>

/// These message types can be ORed into routing codes.
/// When you add a new routing code, make sure it is 2 times
/// the next highest code.  Also increment g_nMessageTypes
/// and add the address of the default stream to g_apMessageOstreams.
static const unsigned int kHitMessage   = 1;
static const unsigned int kTrackMessage = 2; 
static const unsigned int kNodeMessage  = 4;
static const unsigned int kDetectorMessage = 8;

/// Number of message types
static const int g_nMessageTypes = 4;
/// Output streams corresponding to the message types
static ostream *g_apMessageOstreams[g_nMessageTypes] = 
{ &cout, &cout, &cout, &cout };

/// Typedefs for containers
class Messenger;
typedef map<unsigned int, Messenger*> messengerMap;
typedef messengerMap::const_iterator messengerMapIterator;
typedef messengerMap::value_type messengerMapValueType;

typedef vector<Messenger*> messengerVector;
typedef messengerVector::const_iterator messengerVectorIterator;

/*
typedef void (*__sender)(ostream &stream, void*);
*/
class Messenger;
class MessengerBuf: public streambuf{
public:
    MessengerBuf(Messenger *pMessenger):m_pMessenger(pMessenger){}
    virtual ~MessengerBuf(){}

    int sync ();
    int overflow (int ch);
    // Defining xsputn is an optional optimization.
    // (streamsize was recently added to ANSI C++, not portable yet.)
    streamsize xsputn (char* text, streamsize n);

protected:
    Messenger *m_pMessenger;
};

class Messenger: public ostream{

public:

    /// we override this to capture messages for dispatching to 
    /// the various output streams
    //virtual ostream& flush();

/*
    /// Allows you to route message strings using ostream syntax
    Messenger& operator<<(char *szMessage);
    /// Allows you to route iostream manipulators using ostream syntax
    Messenger& operator<<(__omanip manip);
    /// Allows you to route numbers using ostream syntax
    Messenger& operator<<(double dVal); 
*/
    /// Return a Messenger instance corresponding to the given routing code,
    /// or all routes allowed by the global mask if no routing is specified.
    static Messenger *instance(unsigned int routing=0);

    /// Set the global routing mask which tells which messages are actually
    /// delivered to a given stream.  This mask is AND-ed with the routing
    /// code of a given Messenger to determine if a message should be
    /// printed.
    static void setRoutingMask(unsigned int routing){ s_routing = routing; }
    /// Returns the global routing mask
    static unsigned int getRoutingMask(){ return s_routing; }

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

    /// This is a necessary kludge because of a bug in g++ 2.x which SEGVs
    /// when you try to access RTTI (dynamic_cast or typeid, for example)
    /// from another shared library (we need RTTI for ostream in libstdc++)
    //static bool isMessenger(ostream *pOstream);

    /// sends the current message to all appropriate output streams.
    void dispatchMessage(char *szMessage, streamsize iLen);

protected:

    /// Construct a Messenger with the given routing.
    Messenger(unsigned int routing=0):
            ostream(new MessengerBuf(this)),
            m_routing(routing==0 ? s_routing : routing){}

/*
    /// sends the given message to all appropriate output streams.
    /// The actual writeMessageXXX function called determines the
    /// type of pMessage.
    void dispatchMessage(void *pMessage, __sender sender);
*/

    
/*
    /// print a string to the output stream
    static void writeMessageSz(ostream &stream, char *szMessage){
      stream << szMessage;
    }
    /// print a manipulator to the output stream
    static void writeMessageManip(ostream &stream, __omanip *pManip){
      stream << *pManip;
    }
    /// print a number to the output stream
    static void writeMessageD(ostream &stream, double *pdVal){
      stream << *pdVal;
    }
*/
    /// buffer between the ostream implementation (ostrstream) and the
    /// actual output streams selected by the routing code & mask.
    //char m_szBuf[1024];

    /// routing code for this Messenger tells which streams should be output
    unsigned int m_routing;

    /// static routing mask, AND-ed with the instance routing code.
    static unsigned int s_routing;

    /// static map of Messenger instances indexed by routing code.
    static messengerMap s_messengerMap;

};


#endif  
