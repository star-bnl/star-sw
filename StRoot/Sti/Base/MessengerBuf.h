/*!
  \class  MessengerBuf is a streambuf class used in Messenger
  \author Ben Norman (Kent State)
*/

#ifndef MESSENGER_BUF_H
#define MESSENGER_BUF_H

#include "Stsstream.h"
using namespace std;
#ifdef __CINT__
class streambuf;
class MessengerBuf;
#else
class MessengerBuf: public std::streambuf {
public:
    MessengerBuf(unsigned int routing);
    virtual ~MessengerBuf();

    // this delivers buffered data to the ostreams allowed by the 
    // global & static routing codes.  Returns 0 (ok) or EOF.
    //    virtual int sync();
    
    /// this delivers the given character to all output streams
    virtual int overflow (int ch);
    // Defining xsputn is an optional optimization.
    // (streamsize was recently added to ANSI C++, not portable yet.)
    virtual streamsize xsputn (const char* text, streamsize n);
    
    /// return the instance routing code
    inline unsigned int getRoutingCode(){ return m_routing; }

protected:
    
    /// sends the current message to all appropriate output streams.
    /// returns 0 (ok) or EOF.
    int dispatchMessage(const char *szMessage, streamsize iLen);
    
    /// routing code for this MessengerBuf tells which streams should be output
    unsigned int m_routing;
    
};

#endif // on cint

#endif // on MessengerBuf_H

