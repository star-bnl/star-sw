/*!
  \class  Messenger Provides organized informational message routing.
  \author Ben Norman (Kent State)
*/
 
#ifndef MESSENGER_H
#define MESSENGER_H

#include <iostream>
#include <map>
#include "MessengerBuf.h"
#include "MessageType.h"

class Messenger;

/// Typedefs for containers
typedef map<unsigned int, Messenger*> messengerMap;
typedef messengerMap::const_iterator messengerMapIterator;
typedef messengerMap::value_type messengerMapValueType;

class Messenger: public ostream{

public:

    /// Return a Messenger instance corresponding to the given routing code,
    /// or all routes allowed by the global mask if no routing is specified.
    static Messenger *instance(unsigned int routing=0);

    /// return the instance routing code
    inline unsigned int getRoutingCode(){ 
      MessengerBuf *pBuf = dynamic_cast<MessengerBuf *>(rdbuf());
      return pBuf->getRoutingCode(); 
    }

    /// Set the global routing mask which tells which messages are actually
    /// delivered to a given stream.  This mask is AND-ed with the routing
    /// code of a given Messenger to determine if a message should be
    /// printed.  Returns the original routing mask.
    static inline unsigned int setRoutingMask(unsigned int routing){
      unsigned int oldRouting = s_routing;
      s_routing = routing;
      return oldRouting;
    }
    /// Returns the global routing mask
    static inline  unsigned int getRoutingMask(){
      return s_routing;
    }
    /// Sets only the given bits in the global routing mask.
    /// Returns the original state of the bits.
    static inline unsigned int setRoutingBits(unsigned int messages){
      unsigned int oldRouting = s_routing;
      s_routing |= messages;
      return oldRouting & messages;
    }
    /// Clears only the given bits in the global routing mask.
    /// Returns the original state of the bits.
    static inline unsigned int clearRoutingBits(unsigned int messages){
      unsigned int oldRouting = s_routing;
      s_routing &= (~messages);
      return oldRouting & messages;
    }
    /// Returns the given bits in the global routing mask.
    static inline unsigned int getRoutingBits(unsigned int messages){
      return s_routing & messages;
    }
    
    /// Initialize the output streams for the message maps.  This must be
    /// called before using a Messenger.  If a routing code is specified,
    /// it is used as the global routing mask.
    static void init(unsigned int routing=0);

    /// Delete any created Messenger & ofstream objects.  
    /// This must be called after messenging is finished.
    static void kill();

    /// Destructor;
    virtual ~Messenger(){      
      MessengerBuf *pBuf = dynamic_cast<MessengerBuf *>(rdbuf());
      delete pBuf;
    }

protected:

    /// Construct a Messenger with a MessengerMap using the given routing.
    Messenger(unsigned int routing=0):
            ostream(new MessengerBuf(routing==0 ? s_routing : routing)){}

    /// static map of Messenger instances indexed by routing code.
    static messengerMap s_messengerMap;

    /// static routing mask, ANDed with the instance routing code.
    static unsigned int s_routing;

};

#endif  
