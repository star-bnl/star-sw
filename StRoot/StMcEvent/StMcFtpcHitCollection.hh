/***************************************************************************
 *
 * $Id: StMcFtpcHitCollection.hh,v 2.1 1999/11/19 19:06:32 calderon Exp $
 * $Log: StMcFtpcHitCollection.hh,v $
 * Revision 2.1  1999/11/19 19:06:32  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcFtpcHitCollection_hh
#define StMcFtpcHitCollection_hh

#include "StMcFtpcPlaneHitCollection.hh"

class StMcFtpcHit;

class StMcFtpcHitCollection {
public:

    StMcFtpcHitCollection();
    ~StMcFtpcHitCollection();
    
    bool addHit(StMcFtpcHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfPlanes() const;
    
    StMcFtpcPlaneHitCollection*       plane(unsigned int);
    const StMcFtpcPlaneHitCollection* plane(unsigned int) const;
private:
    enum { mNumberOfPlanes = 20 };
    StMcFtpcPlaneHitCollection mPlanes[mNumberOfPlanes];
};
#endif
