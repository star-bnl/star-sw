/***************************************************************************
 * $Id: StuFtpcRefMult.hh,v 1.2 2003/04/29 13:20:18 putschke Exp $
 ***************************************************************************
 *
 * Description:
 * Function to give a standard reference multiplicity (uncorrected) for 
 * an dAu mibias event [isTrigger(2001) || isTrigger(2003)] (preliminary !!!).
 *
 * The cuts are :
 * ---------------
 * minbias 
 * primary vertex found
 * |vertex-z| < 50 cm
 * isTrigger(2001) || isTrigger(2003)
 *
 * charged primary tracks only
 * fit points >= 5 (>=6 included Vertex)
 * 2.8 <= |eta| < 3.8
 * pt < 3 GeV
 * dca < 3 cm 
 *
 * With the multiplicity in FTPC east 3 "centrality bins" are defined :
 *
 * centrality bin    multiplicity FTPC east    percentOfEvents
 *        1                   <=11                 100-40
 *        2                   <=17                 40-20
 *        3                   >=18                 20-0
 *
 * This definition seems to be stable up to day 40 !!!
 * 
 * return 9999 if event cuts failed and centrality bin 99 !!!
 *
 **************************************************************************
 * $Log: StuFtpcRefMult.hh,v $
 * Revision 1.2  2003/04/29 13:20:18  putschke
 * use now dca of global track
 *
 * Revision 1.1  2003/03/27 21:37:46  calderon
 * Joern's code for reference multiplicity using the FTPC.
 *
 **************************************************************************/

#ifndef StuRefFtpcMult_hh
#define StuRefFtpcMult_hh

#include "StEventTypes.h"

inline bool
multiplicityEventCutFtpc(const StEvent& evt)
{

  // check dAu minbias
  if (evt.triggerIdCollection() &&
      evt.triggerIdCollection()->nominal() )
    if (evt.triggerIdCollection()->nominal()->isTrigger(2001) 
   ||
   evt.triggerIdCollection()->nominal()->isTrigger(2003))
      {
   const StPrimaryVertex* primVtx = evt.primaryVertex();

   if (!primVtx) return false;
   else if (fabs(primVtx->position().x())<10e-3 && fabs(primVtx->position().y())<10e-3 && fabs(primVtx->position().z())<10e-3) return false;
   else if (fabs(primVtx->position().z())>=50) return false;
   else return true;
      }

  return false;
}

inline unsigned int
uncorrectedNumberOfFtpcEastPrimaries(const StEvent& evt)
{
    if (!multiplicityEventCutFtpc(evt)) return 9999;
    
    const StPrimaryVertex* primVtx = evt.primaryVertex();
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();

    size_t countedTracks = 0;
    for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++) {
   StTrack* track = (*iter);
   // check if possible FTPC tracks
   if (track->fitTraits().numberOfFitPoints()<6 || (track->fitTraits().numberOfFitPoints()>11)) continue; 
   // check eta range and FTPC east
   if (track->geometry()->momentum().pseudoRapidity()>-2.8 || track->geometry()->momentum().pseudoRapidity()<=-3.8) continue;
   // check pt
   if (track->geometry()->momentum().perp()>=3) continue; 
   // finally, check dca, if a track satisfies gets inside the if, count it.
   if (track->node()->track(global)->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfFtpcWestPrimaries(const StEvent& evt)
{
    if (!multiplicityEventCutFtpc(evt)) return 9999;

    const StPrimaryVertex* primVtx = evt.primaryVertex();
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();

    size_t countedTracks = 0;
    for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++) {
   StTrack* track = (*iter);
   // check if possible FTPC tracks
   if (track->fitTraits().numberOfFitPoints()<6 || (track->fitTraits().numberOfFitPoints()>11)) continue; 
   // check eta range and FTPC west
   if (track->geometry()->momentum().pseudoRapidity()<=2.8 || track->geometry()->momentum().pseudoRapidity()>3.8) continue;
   // check pt
   if (track->geometry()->momentum().perp()>=3) continue; 
   // finally, check dca, if a track satisfies gets inside the if, count it.
   if (track->node()->track(global)->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfFtpcPrimaries(const StEvent& evt)
{   
  if (!multiplicityEventCutFtpc(evt)) return 9999;
  
  return uncorrectedNumberOfFtpcWestPrimaries(evt) + uncorrectedNumberOfFtpcEastPrimaries(evt);
}

inline unsigned int
uncorrectedBinOfFtpcEastCentrality(StEvent& evt)
{

  size_t countedTracks = uncorrectedNumberOfFtpcEastPrimaries(evt);

  unsigned int cent[] = {0,12,18};
  if (countedTracks!=9999)
    {
      if (countedTracks < cent[1])       { return 1; }
      else if (countedTracks < cent[2])  { return 2; }
      else { return 3; }
    }
  else {return 99;}

}
#endif
