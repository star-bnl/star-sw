#include <fstream.h>

#include "Messenger.h"

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

  MessengerBuf::setRoutingMask(routing);
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
