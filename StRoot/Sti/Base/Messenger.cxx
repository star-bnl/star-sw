#include "Sti/Base/Messenger.h"

// statics
messengerMap Messenger::s_messengerMap;
unsigned int Messenger::s_routing = 0;

void Messenger::init(unsigned int routing){

  // default is that all routes are valid
  if(routing==0){
    s_routing = (1 << MessageType::getNtypes()) - 1;
  }else{
    s_routing = routing;
  }

} // init

void Messenger::kill(){

  // delete the Messengers we created
  for(messengerMapIterator iterator = s_messengerMap.begin();
        iterator!=s_messengerMap.end(); iterator++){
    delete iterator->second;
  }

  // note: we should delete here any ofstreams we may have created on the heap
  for(unsigned int iMessageType = 0; iMessageType<MessageType::getNtypes(); 
      iMessageType++){
    delete MessageType::getTypeByIndex(iMessageType);
  }
} // kill

void Messenger::updateStates(){
  for(messengerMapIterator iterator = s_messengerMap.begin();
      iterator!=s_messengerMap.end(); iterator++){
    Messenger *pMessenger = iterator->second;
// #ifndef DEBUG
//    pMessenger->clear(ios::badbit);
// #else
    if(pMessenger->canWrite()){ pMessenger->clear(ios::goodbit); }
    else{                       pMessenger->clear(ios::badbit);}
// #endif
  }
} // updateStates

Messenger* Messenger::instance(unsigned int routing){ 

  // check for existing messenger & return if found
  messengerMapIterator where = s_messengerMap.find(routing);
  if(where!=s_messengerMap.end()){ return where->second; }

  // create new messenger & add to map
  Messenger *pMessenger = new Messenger(routing);
  s_messengerMap.insert( messengerMapValueType(routing, pMessenger) );
  // set its badbit if it hasn't been instantiated
  if(!pMessenger->canWrite()){ pMessenger->setstate(ios::badbit); }

  return pMessenger;
} // instance
