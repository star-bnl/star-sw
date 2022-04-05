/***************************************************************************
 * $Id: StuFtpcRefMult.hh,v 1.8 2018/06/29 17:21:24 perev Exp $
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
 *        1                   <10                 100-40
 *        2                   <17                 40-20
 *        3                   >=17                 20-0
 *
 * This definition seems to be stable up to day 40 !!!
 * 
 * return 9999 if event cuts failed and centrality bin 99 !!!
 *
 **************************************************************************
 * $Log: StuFtpcRefMult.hh,v $
 * Revision 1.8  2018/06/29 17:21:24  perev
 * Irakli_Jun29
 *
 * Revision 1.7  2005/08/19 03:44:28  perev
 * Marco updates
 *
 * Revision 1.6  2004/06/30 15:58:36  putschke
 * Fix comments !
 *
 * Revision 1.5  2004/04/11 11:19:33  putschke
 * Remove dAu minbias trigger cut
 *
 * Revision 1.4  2004/02/06 15:04:25  putschke
 * Update and fix multiplicity cuts
 *
 * Revision 1.3  2003/12/04 03:55:08  perev
 * Simplify code for compiler
 *
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
multiplicityEventCutFtpc(const StPrimaryVertex*& primVtx)
{

  if (!primVtx) return false;
  else if (fabs(primVtx->position().x())<10e-3 && fabs(primVtx->position().y())<10e-3 && fabs(primVtx->position().z())<10e-3) return false;
  else if (fabs(primVtx->position().z())>=50) return false;
  else return true;
  
  return false;

}


inline unsigned int
uncorrectedNumberOfFtpcEastPrimaries(const StPrimaryVertex*& primVtx)
{
    if (!multiplicityEventCutFtpc(primVtx)) return 9999;
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
   StTrack *glt = (StTrack *) track->node()->track(global);
   if (glt->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfFtpcEastPrimaries(const StEvent& evt, int vtx_id = 0)
{
    const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);
    return uncorrectedNumberOfFtpcEastPrimaries(primVtx);
}

inline unsigned int
uncorrectedNumberOfFtpcWestPrimaries(const StPrimaryVertex*& primVtx)
{

    if (!multiplicityEventCutFtpc(primVtx)) return 9999;
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
   if (((StTrack *)track->node()->track(global))->geometry()->helix().distance(primVtx->position())<3) ++countedTracks;
    }
    return countedTracks;
}

inline unsigned int
uncorrectedNumberOfFtpcWestPrimaries(const StEvent& evt, int vtx_id = 0)
{
    const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);
    return uncorrectedNumberOfFtpcWestPrimaries(primVtx);
}


inline unsigned int
uncorrectedNumberOfFtpcPrimaries(const StEvent& evt, int vtx_id)
{   
  const StPrimaryVertex* primVtx = evt.primaryVertex(vtx_id);
  if (!multiplicityEventCutFtpc(primVtx)) return 9999;
  
  return uncorrectedNumberOfFtpcWestPrimaries(primVtx) + uncorrectedNumberOfFtpcEastPrimaries(primVtx);
}

inline unsigned int
uncorrectedBinOfFtpcEastCentrality(StEvent& evt)
{

  size_t countedTracks = uncorrectedNumberOfFtpcEastPrimaries(evt);

  unsigned int cent[] = {0,10,17};
  if (countedTracks!=9999)
    {
      if (countedTracks < cent[1])       { return 1; }
      else if (countedTracks < cent[2])  { return 2; }
      else { return 3; }
    }
  else {return 99;}

}
#endif
