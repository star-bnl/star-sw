#include "Messenger.h"
#include "MessengerBuf.h"

unsigned int MessengerBuf::s_routing = 0;

MessengerBuf::MessengerBuf(unsigned int routing):m_routing(routing){
} // MessengerBuf
MessengerBuf::~MessengerBuf(){
} // ~MessengerBuf
/*
int MessengerBuf::sync (){
  // determine how many bytes have been used in the output buffer.
  streamsize n = pptr () - pbase ();

  // try to write them out.
  int ret = 0;
  if(n){
    ret = dispatchMessage(pbase(), n);
    if(ret==0){ pbump(-n); } // reset pptr()
  }
  return ret;
} // sync
*/
int MessengerBuf::overflow (int ch){ 
  // first flush our buffer
//  if (sync()==EOF){ return EOF; }

  // output the given character.
  if (ch != EOF){
    char cbuf[1] = {ch};
    if( dispatchMessage(cbuf, 1)==EOF ){ return EOF; }
  }
  return 0;
} // overflow

/*
streamsize MessengerBuf::xsputn (char* text, streamsize n){
  // first flush our buffer
  if (sync()==EOF){ return EOF; }

  return dispatchMessage(text, n)==EOF ? 0 : n;
} // xsputn
*/
int MessengerBuf::dispatchMessage(char *szMessage, streamsize iLen){

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

      // mark that we have written to this stream for all message routes
      // using exactly the same stream
      for(int iStream = 0; iStream<g_nMessageTypes; iStream++){
        if(pStream == g_apMessageOstreams[iStream]){
          bStreamUsed[iStream] = true;
        }
      }

    }
  }

  return 0;
} // dispatchMessage
