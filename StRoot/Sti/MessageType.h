/*!
  \class  MessageType Represents one type of informational message
  \author Ben Norman (Kent State)
*/

#ifndef MESSAGE_TYPE_H
#define MESSAGE_TYPE_H

#include <string>
#include <iostream>

using std::string;
using std::ostream;
using std::cout;
using std::cerr;

// stuff to make adding new messages easy
#define ADD_MESSAGE(MSG)    \
    static unsigned int k##MSG##Message
#define CREATE_MESSAGE(MSG) \
unsigned int MessageType::k##MSG##Message = (new MessageType(#MSG))->getCode();

class MessageType{
public:

//-----------------------------------------------------------------------------
// Here is where you should add new message types, as well as MessageType.cxx .
ADD_MESSAGE(Hit);
ADD_MESSAGE(Track);
ADD_MESSAGE(Node);
ADD_MESSAGE(Detector);
ADD_MESSAGE(Geometry);
ADD_MESSAGE(SeedFinder);
//-----------------------------------------------------------------------------

    MessageType(string name);
    /// deletes our ostream if it is not cout or cerr
    virtual ~MessageType();

    /// returns the name
    inline string getName(){ return m_name; }
    /// returns the index (0, 1, 2, 3, etc)
    inline unsigned int getIndex(){ return m_iIndex; }
    /// returns the code  (0x01, 0x02, 0x04, etc)
    inline unsigned int getCode(){ return m_iCode; }
    /// returns the ostream
    inline ostream *getOstream(){ return m_pOstream; }
    /// sets the ostream
    inline void setOstream(ostream *pOstream){
      ostream *pOldOstream = m_pOstream;
      m_pOstream = pOstream; // set new ostream
      if(pOldOstream!=NULL && pOldOstream!=&cout && pOldOstream!=&cerr){
        delete pOldOstream; 
      }
    }

    /// returns the number of message types
    static inline unsigned int getNtypes(){ return s_nTypes; }

    /// returns the MessageType with the given index, or NULL if none exists
    static MessageType *getTypeByIndex(unsigned int iIndex);

    /// returns the MessageType with the given code, or NULL if none exists
    static MessageType *getTypeByCode(unsigned int iCode);

protected:

    /// name (HitMessage, TrackMessage, etc)
    string m_name;
    /// index ( log_2(code) )
    unsigned int m_iIndex;
    /// bit code ( 0x01, 0x02, 0x04, etc )
    unsigned int m_iCode;
    /// the ostream where messages of this type should go
    ostream *m_pOstream;

    /// number of message types
    static unsigned int s_nTypes;

    /// look up type by index
    static MessageType* s_apTypes[32];

};

#endif
