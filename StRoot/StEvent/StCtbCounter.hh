/***************************************************************************
 *
 * $Id: StCtbCounter.hh,v 1.1 1999/01/15 20:39:38 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.hh,v $
 * Revision 1.1  1999/01/15 20:39:38  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StCtbCounter_hh
#define StCtbCounter_hh
#include <vector>

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
