/***************************************************************************
 *
 * $Id: StuRefCentrality.hh,v 1.1 2001/12/18 03:11:38 ychen Exp $
 *
 * Author: Yu Chen / UCLA   Feb 2001
 ***************************************************************************
 *
 * Description:
 * Function to give a standard uncorrected reference multiplicity for 
 * an event according to the definition in STAR flow paper. The purpose is
 * to give a reference centrality bin.
 *    The agreed upon track cuts are: 
 * a) primary tracks only
 * b) flag > 0
 * c) abs(eta) < 0.75
 * d) no cuts on either number of hits or number of fit points
 *    The event cut:
 * PrimaryVertex pVertex->numberOfDaughters() > 10
 * should be used for giving percentages below.
 *
 *    The functions are:
 * 1) uncorrectedMultiplicity
 *      return uncorrected multiplicity as a measure of centrality; 
 * 2) meanScaledMultiplicity
 *      return mean Nch/Nch(max) for 8 centrality bins;
 * 3) binOfCentrality
 *      return the number of centrality bins:
 *      1-- most peripheral; ... ; 8-- most central  
 * 4) percentOfCentrality
 *      return the higher edge of percentages that centrality bin covers:
 *      1-- 85% (to 58%); ... ; 8-- 6% (to 0%)
 ***************************************************************************
 *
 * $Log: StuRefCentrality.hh,v $
 * Revision 1.1  2001/12/18 03:11:38  ychen
 * Added two files to get Nch defined in flow paper. Yu Chen
 *
 *
 **************************************************************************/
#ifndef StuRefCentrality_hh
#define StuRefCentrality_hh

#include "StEventTypes.h"
inline unsigned int
uncorrectedMultiplicity(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return 0;
    
    const StSPtrVecPrimaryTrack& tracks = primVtx->daughters();
    size_t countedTracks = 0;
    for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++) {
	StTrack* track = (*iter);
	if (track->flag()<=0 ) continue;
	if (fabs(track->geometry()->momentum().pseudoRapidity())<0.75)  ++countedTracks;
    }
    return countedTracks;
}
inline float
meanScaledMultiplicity(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return -1.;
    
    size_t countedTracks = uncorrectedMultiplicity(evt);

    unsigned int cent[] = {20,100,180,270,360,460,560,660,870};
    float meanNch[] = {0.067,0.175,0.282,0.391,0.502,0.615,0.730,0.877,1.00};
//    float percent[] = {0.85,0.58,0.45,0.34,0.26,0.18,0.11,0.06,0.00};
    
    if (countedTracks < cent[0])       { return 0.00; }
    else if (countedTracks < cent[1])  { return meanNch[0]; }
    else if (countedTracks < cent[2])  { return meanNch[1]; }
    else if (countedTracks < cent[3])  { return meanNch[2]; }
    else if (countedTracks < cent[4])  { return meanNch[3]; }
    else if (countedTracks < cent[5])  { return meanNch[4]; }
    else if (countedTracks < cent[6])  { return meanNch[5]; }
    else if (countedTracks < cent[7])  { return meanNch[6]; }
    else if (countedTracks < cent[8])  { return meanNch[7]; }
    else                               { return meanNch[8]; }
}
inline unsigned int
binOfCentrality(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return 10;

    size_t countedTracks = uncorrectedMultiplicity(evt);

    unsigned int cent[] = {20,100,180,270,360,460,560,660,870};
//    float percent[] = {0.85,0.58,0.45,0.34,0.26,0.18,0.11,0.06,0.00};
    
    if (countedTracks < cent[0])       { return 0; }
    else if (countedTracks < cent[1])  { return 1; }
    else if (countedTracks < cent[2])  { return 2; }
    else if (countedTracks < cent[3])  { return 3; }
    else if (countedTracks < cent[4])  { return 4; }
    else if (countedTracks < cent[5])  { return 5; }
    else if (countedTracks < cent[6])  { return 6; }
    else if (countedTracks < cent[7])  { return 7; }
    else if (countedTracks < cent[8])  { return 8; }
    else                               { return 9; }
}
inline float
percentOfCentrality(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return -1.;
    
    size_t countedTracks = uncorrectedMultiplicity(evt);

    unsigned int cent[] = {20,100,180,270,360,460,560,660,870};
    float percent[] = {0.85,0.58,0.45,0.34,0.26,0.18,0.11,0.06,0.00};
    
    if (countedTracks < cent[0])       { return 1.00; }
    else if (countedTracks < cent[1])  { return percent[0]; }
    else if (countedTracks < cent[2])  { return percent[1]; }
    else if (countedTracks < cent[3])  { return percent[2]; }
    else if (countedTracks < cent[4])  { return percent[3]; }
    else if (countedTracks < cent[5])  { return percent[4]; }
    else if (countedTracks < cent[6])  { return percent[5]; }
    else if (countedTracks < cent[7])  { return percent[6]; }
    else if (countedTracks < cent[8])  { return percent[7]; }
    else                               { return percent[8]; }
}

#endif



