/***************************************************************************
 *
 * $Id: StCtbCounter.hh,v 1.2 1999/01/15 22:53:29 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.hh,v $
 * Revision 1.2  1999/01/15 22:53:29  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StCtbCounter_hh
#define StCtbCounter_hh

class StCtbCounter {
public:
    StCtbCounter();
    StCtbCounter(short id, float m, float t);
    ~StCtbCounter();
    // StCtbCounter(const StCtbCounter &right);
    // const StCtbCounter & operator=(const StCtbCounter &right);

    short id() const;
    float mips() const;
    float time() const;

    void setId(short);
    void setMips(float);
    void setTime(float);
    
protected:
    short mId;
    float mMips;
    float mTime;
};

inline short StCtbCounter::id() const { return mId; }

inline float StCtbCounter::mips() const { return mMips; }

inline float StCtbCounter::time() const { return mTime; }

#endif
