#include "Sti/Base/MessengerBuf.h"
#include "Sti/Base/MessageType.h"
#include "Sti/Base/Messenger.h"

MessengerBuf::MessengerBuf(unsigned int routing):m_routing(routing){
} // MessengerBuf
MessengerBuf::~MessengerBuf(){
} // ~MessengerBuf

int MessengerBuf::overflow (int ch){ 
  // output the given character.
  if (ch != EOF){
    char cbuf[2] = {ch, 0};
    if( dispatchMessage(cbuf, 1)==EOF ){ return EOF; }
  }
  return 0;
} // overflow

streamsize MessengerBuf::xsputn (const char* text, streamsize n){
  return dispatchMessage(text, n)==EOF ? 0 : n;
} // xsputn

int MessengerBuf::dispatchMessage(const char *szMessage, streamsize iLen){

  // bail immediately if we aren't printing.
  unsigned int routing = m_routing & Messenger::getRoutingMask();
  if(routing == 0){ return 0; }

  // keep track of which unique streams (not routing bits) we have
  // written to
  unsigned int nMessageTypes = MessageType::getNtypes();
  bool *abStreamUsed = new bool[nMessageTypes];
  for(unsigned int iMessageType = 0; iMessageType<nMessageTypes; 
      iMessageType++){ abStreamUsed[iMessageType] = false; }

  // test our routing bits agains the global mask and output
  // to all relevant streams

  for(unsigned int iMessageType = 0; iMessageType<nMessageTypes; 
      iMessageType++){
    
    MessageType *pType = MessageType::getTypeByIndex(iMessageType);
    unsigned int message = pType->getCode();
    if((routing&message)==message && !abStreamUsed[iMessageType]){

      ostream *pStream = pType->getOstream();
      pStream->write(szMessage, iLen);
      pStream->flush();

      // mark that we have written to this stream for all message routes
      // using exactly the same stream
      for(unsigned int iStream = 0; iStream<nMessageTypes; iStream++){
        if(pStream == MessageType::getTypeByIndex(iStream)->getOstream()){
          abStreamUsed[iStream] = true;
        }
      }
      
    }
  }

  delete [] abStreamUsed;
  return 0;
} // dispatchMessage
