/***************************************************************************
 *
 * $Id: StMcIstHitCollection.hh,v 2.8 2015/03/13 18:44:58 perev Exp $
 * $Log: StMcIstHitCollection.hh,v $
 * Revision 2.8  2015/03/13 18:44:58  perev
 * Roll back
 *
 * Revision 2.6  2012/12/18 21:02:06  perev
 * Ist development (Jonathan)
 *
 * Revision 2.5  2012/03/22 00:43:52  perev
 * private => protected
 *
 * Revision 2.4  2009/07/24 19:08:07  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.3  2009/02/06 15:38:12  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.2  2005/05/11 20:54:29  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcIstHitCollection_hh
#define StMcIstHitCollection_hh

#include "StMcIstLayerHitCollection.hh"
class StMcIstHit;

class StMcIstHitCollection : public StObject {
public:

    StMcIstHitCollection();
    virtual ~StMcIstHitCollection();
    
    bool addHit(StMcIstHit*);
    unsigned long numberOfHits() const;
    unsigned int  numberOfLayers() const;
    
    StMcIstLayerHitCollection*       layer(unsigned int);
    const StMcIstLayerHitCollection* layer(unsigned int) const;
protected:
    enum { mNumberOfLayers = 24 };
    StMcIstLayerHitCollection mLayers[mNumberOfLayers];
    ClassDef(StMcIstHitCollection,1)
};
#endif
