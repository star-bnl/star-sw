/***************************************************************************
 *
 * $Id: StTrigger.hh,v 1.2 1999/01/15 22:54:12 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.hh,v $
 * Revision 1.2  1999/01/15 22:54:12  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:12  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTrigger_hh
#define StTrigger_hh

class StTrigger {
public:
    StTrigger();
    StTrigger(unsigned short aw, unsigned short w);
    virtual ~StTrigger();
    // StTrigger(const StTrigger&);  use default
    // const StTrigger & operator=(const StTrigger&);
    int operator==(const StTrigger&) const;
    int operator!=(const StTrigger&) const;

    unsigned short triggerActionWord() const;
    unsigned short triggerWord() const;

    void setTriggerActionWord(unsigned short);
    void setTriggerWord(unsigned short);
    
protected:
    unsigned short mTriggerActionWord;
    unsigned short mTriggerWord;
};

inline unsigned short StTrigger::triggerActionWord() const { return mTriggerActionWord; }

inline unsigned short StTrigger::triggerWord() const { return mTriggerWord; }

#endif
