#include "Messenger.h"

unsigned int Messenger::s_routing = 0;
messengerMap Messenger::s_messengerMap;
 
void Messenger::setMessageOstream(unsigned int routing, ostream *pNewStream){

  // find index of routing code
  if(routing==0){ return; }
  int iMessageType = 0;
  while( (routing /= 2) > 0 ){ iMessageType++; }

  ostream *pOstream = g_apMessageOstreams[iMessageType];
  if( pOstream!=&cout && pOstream!=&cerr ){
    pOstream->flush();
    ofstream *pOfstream = dynamic_cast<ofstream *>(pOstream);
    if(pOfstream != NULL){ pOfstream->close(); }
    delete pOstream;
  }
  g_apMessageOstreams[iMessageType] = pNewStream;

} // setMessageOstream

void Messenger::init(unsigned int routing){

  // default is that all routes are valid
  if(routing==0){
    for(int iMessageType = 0; iMessageType<g_nMessageTypes; iMessageType++){
      routing |= (1 << iMessageType);
    }
  }

  s_routing = routing;
} // init

void Messenger::kill(){

  // delete the Messengers we created
  for(messengerMapIterator iterator = s_messengerMap.begin();
        iterator!=s_messengerMap.end(); iterator++){
    delete iterator->second;
  }

  // note: we should delete here any ofstreams we may have created on the heap
  for(int iMessageType = 0; iMessageType<g_nMessageTypes; iMessageType++){
    setMessageOstream(1 << iMessageType, NULL);
  }
} // kill

Messenger* Messenger::instance(unsigned int routing){ 

  // check for existing messenger & return if found
  messengerMapIterator where = s_messengerMap.find(routing);
  if(where!=s_messengerMap.end()){ return where->second; }

  // create new messenger & add to map before returning
  Messenger *pMessenger = new Messenger(routing);
  s_messengerMap.insert( messengerMapValueType(routing, pMessenger) );
  return pMessenger;

} // instance

/*
Messenger& Messenger::operator<<(char *szMessage){ 
  dispatchMessage(szMessage, (__sender)writeMessageSz);
  return *this;
} // operator<<(const char *)

Messenger& Messenger::operator<<(__omanip manip){ 
  dispatchMessage(&manip, (__sender)writeMessageManip);
  return *this;
} // operator<<(__omanip)

Messenger& Messenger::operator<<(double dVal){ 
  dispatchMessage(&dVal, (__sender)writeMessageD);
  return *this;
} // operator<<(double)

void Messenger::dispatchMessage(void *pMessage, __sender sender){
*/
void Messenger::dispatchMessage(char *szMessage, streamsize iLen){

  // keep track of which unique streams (not routing bits) we have
  // written to
  bool bStreamUsed[g_nMessageTypes] = {false};

  // test our routing bits agains the global mask and output
  // to all relevant streams

  unsigned int routing = m_routing & s_routing;
  for(int iMessageType = 0; iMessageType<g_nMessageTypes; iMessageType++){
    
    unsigned int message = 1 << iMessageType;
    if((routing&message) == message && !bStreamUsed[iMessageType]){

      ostream *pStream = g_apMessageOstreams[iMessageType];
      pStream->write(szMessage, iLen);
      pStream->flush();
//      (*sender)(*pStream, pMessage);

      // mark that we have written to this stream for all message routes
      // using exactly the same stream
      for(int iStream = 0; iStream<g_nMessageTypes; iStream++){
        if(pStream == g_apMessageOstreams[iStream]){
          bStreamUsed[iStream] = true;
        }
      }

    }
  }

} // dispatchMessage
/*
ostream& Messenger::flush(){

  //printf("[[Messenger::flush()]]");

  // print message to all streams
  dispatchMessage();

  // clear buffer for next message
  //rdbuf()->seekpos(0, ios::out);

} // flush

/// kludge necessary because ostream::flush() isn't virtual.
/// We need a hook into ostream to know when to dispatch our messages,
/// and this is it.
ostream& ostream::flush()
{
  //printf("[[ostream::flush()]]");

  // dynamic_cast and type_info don't work here for ostream 
  // because of a bug in gcc <3.0, see
  // http://faqchest.dynhost.com/prgm/gcc-l/gcc-01/gcc-0108/gcc-010802/gcc01081306_08660.html

  if(Messenger::isMessenger(this)){
    Messenger *pMessenger = dynamic_cast<Messenger *>(this);
    pMessenger->flush();
  }

  //----------------------
  // real ostream::flush
  if (_strbuf->sync())
    set(ios::badbit);

  return *this;
  //----------------------
}

bool Messenger::isMessenger(ostream *pOstream){

  for(messengerMapIterator iterator = s_messengerMap.begin();
        iterator!=s_messengerMap.end(); iterator++){
    if(pOstream == iterator->second){ return true; }
  }

  return false;
} // isMessenger
*/

int MessengerBuf::sync ()
{ streamsize n = pptr () - pbase ();
//  return (n && m_pMessenger->dispatchMessage(pbase(), n) != n) ? EOF : 0;
 m_pMessenger->dispatchMessage(pbase(), n);
 return n;
}

int MessengerBuf::overflow (int ch)
{ streamsize n = pptr () - pbase ();
  if (n && sync ())
    return EOF;
  if (ch != EOF)
    {
      char cbuf[1];
      cbuf[0] = ch;
//      if (m_pMessenger->dispatchMessage(cbuf, 1) != 1)
//        return EOF;
      m_pMessenger->dispatchMessage(cbuf, 1);
    }
  pbump (-n);  // Reset pptr().
  return 0;
}

streamsize MessengerBuf::xsputn (char* text, streamsize n)
{// return sync () == EOF ? 0 : m_pMessenger->dispatchMessage(text, n); }
 m_pMessenger->dispatchMessage(text, n); 
return n;
}
