/*!
  \class  MessengerBuf is a streambuf class used in Messenger
  \author Ben Norman (Kent State)
*/

#ifndef MESSENGER_BUF_H
#define MESSENGER_BUF_H

class Messenger;

class MessengerBuf: public streambuf{
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
    //virutal streamsize xsputn (char* text, streamsize n);

    /// Set the global routing mask
    static void setRoutingMask(unsigned int routing){ s_routing = routing; }
    /// Returns the global routing mask
    static unsigned int getRoutingMask(){ return s_routing; }

protected:

    /// sends the current message to all appropriate output streams.
    /// returns 0 (ok) or EOF.
    int dispatchMessage(char *szMessage, streamsize iLen);

    /// routing code for this MessengerBuf tells which streams should be output
    unsigned int m_routing;
    
    /// static routing mask, ANDed with the instance routing code.
    static unsigned int s_routing;

};

#endif
