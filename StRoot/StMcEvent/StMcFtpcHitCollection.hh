/***************************************************************************
 *
 * $Id: StMcFtpcHitCollection.hh,v 2.5 2012/03/22 00:41:15 perev Exp $
 * $Log: StMcFtpcHitCollection.hh,v $
 * Revision 2.5  2012/03/22 00:41:15  perev
 * private => protected
 *
 * Revision 2.4  2009/07/24 19:08:07  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.3  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
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

class StMcFtpcHitCollection : public StObject {
public:

    StMcFtpcHitCollection();
    virtual ~StMcFtpcHitCollection();
    
    bool addHit(StMcFtpcHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfPlanes() const;
    
    StMcFtpcPlaneHitCollection*       plane(unsigned int);
    const StMcFtpcPlaneHitCollection* plane(unsigned int) const;
protected:
    enum { mNumberOfPlanes = 20 };
    StMcFtpcPlaneHitCollection mPlanes[mNumberOfPlanes];
    ClassDef(StMcFtpcHitCollection,1)
};
#endif
