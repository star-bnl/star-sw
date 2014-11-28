/***************************************************************************
 *
 * $Id: StMcContainers.hh,v 2.17 2013/03/25 23:25:35 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Containers for StMcEvent objects
 *
 ***************************************************************************
 *
 * $Log: StMcContainers.hh,v $
 * Revision 2.17  2013/03/25 23:25:35  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.16  2012/03/22 00:32:17  perev
 * Etr hit added
 *
 * Revision 2.15  2011/10/11 01:09:39  perev
 * Mtd added
 *
 * Revision 2.14  2009/07/24 19:08:06  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.13  2007/10/16 19:49:13  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.12  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.11  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.10  2005/07/07 18:20:48  calderon
 * Added support for IGT detector.
 *
 * Revision 2.9  2005/04/18 20:11:32  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.8  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.7  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.6  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.6  2003/02/18 00:00:00  gans
 * Introduction of the CTB classes.  Modified several classes
 * accordingly.
 *
 * $Log: StMcContainers.hh,v $
 * Revision 2.17  2013/03/25 23:25:35  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.16  2012/03/22 00:32:17  perev
 * Etr hit added
 *
 * Revision 2.15  2011/10/11 01:09:39  perev
 * Mtd added
 *
 * Revision 2.14  2009/07/24 19:08:06  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.13  2007/10/16 19:49:13  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.12  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.11  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.10  2005/07/07 18:20:48  calderon
 * Added support for IGT detector.
 *
 * Revision 2.9  2005/04/18 20:11:32  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.8  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.7  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.6  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.5  2000/06/06 02:58:40  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.4  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:00:58  calderon
 * Completely revised for new StEvent
 *
 **************************************************************************/
#ifndef StMcContainers_hh
#define StMcContainers_hh

#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif


class StMcHit;
class StMcCalorimeterHit;
class StMcVertex;
class StMcSvtHit;
class StMcSsdHit;
class StMcTpcHit;
class StMcFtpcHit;
class StMcRichHit;
class StMcCtbHit;
class StMcPxlHit;
class StMcIstHit;
class StMcFgtHit;
class StMcEtrHit;
class StMcTofHit;
class StMcBTofHit;
class StMcMtdHit;
class StMcTrack;
   
// Owners
typedef vector<StMcHit*>     StSPtrVecMcHit;     
typedef vector<StMcVertex*>  StSPtrVecMcVertex; 
typedef vector<StMcCalorimeterHit*>     StSPtrVecMcCalorimeterHit;   
typedef vector<StMcSvtHit*>  StSPtrVecMcSvtHit; 
typedef vector<StMcSsdHit*>  StSPtrVecMcSsdHit; 
typedef vector<StMcTpcHit*>  StSPtrVecMcTpcHit; 
typedef vector<StMcFtpcHit*> StSPtrVecMcFtpcHit; 
typedef vector<StMcRichHit*> StSPtrVecMcRichHit; 
typedef vector<StMcCtbHit*>  StSPtrVecMcCtbHit; 
typedef vector<StMcPxlHit*> StSPtrVecMcPxlHit; 
typedef vector<StMcIstHit*>  StSPtrVecMcIstHit; 
typedef vector<StMcFgtHit*>  StSPtrVecMcFgtHit; 
typedef vector<StMcEtrHit*>  StSPtrVecMcEtrHit; 
typedef vector<StMcTofHit*>  StSPtrVecMcTofHit; 
typedef vector<StMcBTofHit*>  StSPtrVecMcBTofHit; 
typedef vector<StMcMtdHit*>  StSPtrVecMcMtdHit; 
typedef vector<StMcTrack*>   StSPtrVecMcTrack;  
// Not owners
typedef vector<StMcVertex*>  StPtrVecMcVertex; 
typedef vector<StMcCalorimeterHit*>     StPtrVecMcCalorimeterHit;   
typedef vector<StMcHit*>     StPtrVecMcHit;     
typedef vector<StMcSvtHit*>  StPtrVecMcSvtHit; 
typedef vector<StMcSsdHit*>  StPtrVecMcSsdHit; 
typedef vector<StMcTpcHit*>  StPtrVecMcTpcHit; 
typedef vector<StMcFtpcHit*> StPtrVecMcFtpcHit; 
typedef vector<StMcRichHit*> StPtrVecMcRichHit; 
typedef vector<StMcCtbHit*>  StPtrVecMcCtbHit; 
typedef vector<StMcPxlHit*> StPtrVecMcPxlHit; 
typedef vector<StMcIstHit*>  StPtrVecMcIstHit; 
typedef vector<StMcFgtHit*>  StPtrVecMcFgtHit; 
typedef vector<StMcEtrHit*>  StPtrVecMcEtrHit; 
typedef vector<StMcTofHit*>  StPtrVecMcTofHit; 
typedef vector<StMcBTofHit*>  StPtrVecMcBTofHit; 
typedef vector<StMcMtdHit*>  StPtrVecMcMtdHit; 
typedef vector<StMcTrack*>   StPtrVecMcTrack; 
//Iterators
typedef StPtrVecMcVertex::iterator  StMcVertexIterator; 
typedef StPtrVecMcCalorimeterHit::iterator  StMcCalorimeterHitIterator; 
typedef StPtrVecMcHit::iterator     StMcHitIterator; 
typedef StPtrVecMcSvtHit::iterator  StMcSvtHitIterator; 
typedef StPtrVecMcSsdHit::iterator  StMcSsdHitIterator; 
typedef StPtrVecMcTpcHit::iterator  StMcTpcHitIterator; 
typedef StPtrVecMcFtpcHit::iterator StMcFtpcHitIterator; 
typedef StPtrVecMcRichHit::iterator StMcRichHitIterator; 
typedef StPtrVecMcCtbHit::iterator  StMcCtbHitIterator; 
typedef StPtrVecMcPxlHit::iterator StMcPxlHitIterator; 
typedef StPtrVecMcIstHit::iterator  StMcIstHitIterator; 
typedef StPtrVecMcFgtHit::iterator  StMcFgtHitIterator; 
typedef StPtrVecMcEtrHit::iterator  StMcEtrHitIterator; 
typedef StPtrVecMcTofHit::iterator  StMcTofHitIterator; 
typedef StPtrVecMcBTofHit::iterator  StMcBTofHitIterator; 
typedef StPtrVecMcMtdHit::iterator  StMcMtdHitIterator; 
typedef StPtrVecMcTrack::iterator   StMcTrackIterator; 
//Const Iterators
typedef StPtrVecMcVertex::const_iterator  StMcVertexConstIterator; 
typedef StPtrVecMcCalorimeterHit::const_iterator  StMcCalorimeterHitConstIterator;
typedef StPtrVecMcHit::const_iterator     StMcSvtConstIterator; 
typedef StPtrVecMcSvtHit::const_iterator  StMcSvtHitConstIterator; 
typedef StPtrVecMcSsdHit::const_iterator  StMcSsdHitConstIterator; 
typedef StPtrVecMcTpcHit::const_iterator  StMcTpcHitConstIterator; 
typedef StPtrVecMcFtpcHit::const_iterator StMcFtpcHitConstIterator; 
typedef StPtrVecMcRichHit::const_iterator StMcRichHitConstIterator; 
typedef StPtrVecMcCtbHit::const_iterator  StMcCtbHitConstIterator; 
typedef StPtrVecMcPxlHit::const_iterator StMcPxlHitConstIterator; 
typedef StPtrVecMcIstHit::const_iterator  StMcIstHitConstIterator;
typedef StPtrVecMcFgtHit::const_iterator  StMcFgtHitConstIterator; 
typedef StPtrVecMcEtrHit::const_iterator  StMcEtrHitConstIterator; 
typedef StPtrVecMcTofHit::const_iterator  StMcTofHitConstIterator; 
typedef StPtrVecMcBTofHit::const_iterator  StMcBTofHitConstIterator; 
typedef StPtrVecMcMtdHit::const_iterator  StMcMtdHitConstIterator; 
typedef StPtrVecMcTrack::const_iterator   StMcTrackConstIterator; 
//     ClassDef(StMcContainers,0)
#endif //StMcContainers
