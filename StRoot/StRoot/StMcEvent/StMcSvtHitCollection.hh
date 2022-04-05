/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.hh,v 2.6 2012/03/22 00:48:45 perev Exp $
 * $Log: StMcSvtHitCollection.hh,v $
 * Revision 2.6  2012/03/22 00:48:45  perev
 * private => protected
 *
 * Revision 2.5  2009/07/24 19:08:08  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.4  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2000/04/18 23:46:12  calderon
 * Fix bug in reurning barrel number
 * Enumerations for the Max barrels, ladders & wafers modified for
 * SSD inclusion in current scheme.
 *
 * Revision 2.2  2000/03/06 18:05:22  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcSvtHitCollection_hh
#define StMcSvtHitCollection_hh

#include "StMcSvtBarrelHitCollection.hh"
class StMcSvtHit;

class StMcSvtHitCollection : public StObject {
public:
    StMcSvtHitCollection();
    virtual ~StMcSvtHitCollection();
    // StMcSvtHitCollection(const StMcSvtHitCollection&);            use default
    // StMcSvtHitCollection& operator=(const StMcSvtHitCollection&); use default
    
    bool          addHit(StMcSvtHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfBarrels() const;
    
    StMcSvtBarrelHitCollection*       barrel(unsigned int);
    const StMcSvtBarrelHitCollection* barrel(unsigned int) const;

protected:
    enum { mNumberOfBarrels = 4 }; // Keeping the SSD along with SVT
    StMcSvtBarrelHitCollection mBarrels[mNumberOfBarrels];
    ClassDef(StMcSvtHitCollection,1)
};

#endif
