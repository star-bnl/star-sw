#include "MessageType.h"

// statics
messageTypeMap MessageType::s_typeByIndex;
messageTypeMap MessageType::s_typeByCode;
unsigned int MessageType::s_nTypes = 0;

//-----------------------------------------------------------------------------
// here is where you should add new message types, as well as MessageType.h
CREATE_MESSAGE(Hit);
CREATE_MESSAGE(Track);
CREATE_MESSAGE(Node);
CREATE_MESSAGE(Detector);
CREATE_MESSAGE(Geometry);
CREATE_MESSAGE(SeedFinder);
//-----------------------------------------------------------------------------

MessageType::MessageType(string name): m_name(name), m_iIndex(s_nTypes++), 
    m_iCode(1 << m_iIndex), m_pOstream(&cout){
  s_typeByIndex[m_iIndex] = this;
  s_typeByCode[m_iCode] = this;
} // MessageType

MessageType::~MessageType(){
  setOstream(NULL);
  s_typeByIndex.erase(m_iIndex);
  s_typeByCode.erase( m_iCode);
} // ~MessageType

MessageType *MessageType::getTypeByIndex(unsigned int iIndex){
  // check for existing messenger & return if found
  messageTypeMapIterator where = s_typeByIndex.find(iIndex);
  if(where!=s_typeByIndex.end()){ return where->second; }
  return NULL;
} // getTypeByIndex

MessageType *MessageType::getTypeByCode(unsigned int iCode){
  // check for existing messenger & return if found
  messageTypeMapIterator where = s_typeByCode.find(iCode);
  if(where!=s_typeByCode.end()){ return where->second; }
  return NULL;
} // getTypeByCode
