/***************************************************************************
 *
 * $Id: StMwcSector.hh,v 1.2 1999/01/15 22:53:48 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcSector.hh,v $
 * Revision 1.2  1999/01/15 22:53:48  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StMwcSector_hh
#define StMwcSector_hh

class StMwcSector {
public:
    StMwcSector();
    StMwcSector(short id, float m);
    ~StMwcSector();
    // StMwcSector(const StMwcSector &right);
    // const StMwcSector & operator=(const StMwcSector &right);

    short id() const;
    float mips() const;

    void setId(short);
    void setMips(float);
    
protected:
    short mId;
    float mMips;
};

inline short StMwcSector::id() const { return mId; }

inline float StMwcSector::mips() const { return mMips; }

#endif
