#include "MessageType.h"

// statics
//messageTypeMap MessageType::s_typeByIndex;
//messageTypeMap MessageType::s_typeByCode;
MessageType* MessageType::s_apTypes[32] = {0};
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
  s_apTypes[m_iIndex] = this;
} // MessageType

MessageType::~MessageType(){
  setOstream(NULL);
  s_apTypes[m_iIndex] = NULL;
} // ~MessageType

MessageType *MessageType::getTypeByIndex(unsigned int iIndex){
  // check for existing messenger & return if found
  if(iIndex>=s_nTypes){ return NULL; }
  return s_apTypes[iIndex];
} // getTypeByIndex

MessageType *MessageType::getTypeByCode(unsigned int iCode){
  // check for existing messenger & return if found
  if(iCode==0){ return NULL; }
  int iIndex = 0;
  while( (iCode /= 2) > 0){ iIndex++; }
  return getTypeByIndex(iIndex);
} // getTypeByCode









