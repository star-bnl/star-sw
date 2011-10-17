/***************************************************************************
 *
 * $Id: StMcIstHit.cc,v 2.9 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcIstHit.cc,v $
 * Revision 2.9  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.8  2009/02/06 15:36:45  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.7  2006/10/23 21:13:46  calderon
 * Updates to layer(), wafer() and side() methods from Willie L.
 *
 * Revision 2.6  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.5  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.4  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.2  2005/05/11 20:54:28  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.2  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"


#include "StMcIstHit.hh"

static const char rcsid[] = "$Id: StMcIstHit.cc,v 2.9 2011/10/17 00:24:00 fisyak Exp $";
#ifdef POOL
StMemoryPool StMcIstHit::mPool(sizeof(StMcIstHit));
#endif
ClassImp(StMcIstHit);
ostream&  operator<<(ostream& os, const StMcIstHit& h)
{
    os << "IstHit" << endl;
    os << *((StMcHit *) &h);
    os << "Layer           : " << h.layer() << endl;
    return os;
}

ULong_t
StMcIstHit::layer() const
{
  //Only one ist layer now (WL, 03/13/08)
  /*
  unsigned long iLayer = mVolumeId/1000000;
  unsigned long layer;
  if(iLayer<4) layer=iLayer-1;
  else 
    {
      cout << "StMcIstHit::layer() -E- volumeId not known!" << endl;
      layer = 10000; 
    }
  
  return layer;
  */
  return 1;
}

ULong_t
StMcIstHit::ladder() const
{
  //unsigned long iModule = (mVolumeId%1000000)/10000;
  unsigned long iLadder=mVolumeId/1000000-1;
  /*
  unsigned long iLadder = 0;
  if(iModule<=24) iLadder=iModule;
  else 
    {
      cout << "StMcIstHit::ladder() -E- volumeId not known!" << endl;
      iLadder = 10000; 
    }
  */
  return iLadder;
}

unsigned long StMcIstHit::wafer() const
{
  unsigned long iWafer=(mVolumeId%1000000)/10000;
  return iWafer;
}
//________________________________________________________________________________
void StMcIstHit::Print(Option_t *option) const {
  cout << "IstHit\t"; 
  StMcHit::Print();
  cout  << "\tLayer: " << layer();  
}
