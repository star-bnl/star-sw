/***************************************************************************
 *
 * $Id: StuRefCentrality2.hh,v 1.1 2001/12/18 03:11:39 ychen Exp $
 *
 * Author: Yu Chen / UCLA   Sep 2001
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
 * is suggested to be used.
 *
 *    The functions are:
 * 1) uncorrectedMultiplicity
 *      return uncorrected multiplicity as a measure of centrality; 
 * 2) binOfCentrality
 *      return the number of centrality bins:
 *      1-- most peripheral; ... ; 10-- most central  
 * 3) percentOfCentrality
 *      return the higher edge of percentages that centrality bin covers
 *    (based on published STAR h- paper, normalized):
 *      1-- 100% (to 80%); ... ; 10-- 5% (to 0%)
 *
 *   binOfCentrality    percentOfCentrality   uncorrectedMultiplicity  
 *         1                 1.00                    0
 *         2                 0.80                   14
 *         3                 0.70                   30
 *         4                 0.60                   60
 *         5                 0.50                  107
 *         6                 0.40                  176
 *         7                 0.30                  271
 *         8                 0.20                  400
 *         9                 0.10                  569
 *        10                 0.05                  675
 ***************************************************************************
 *
 * $Log: StuRefCentrality2.hh,v $
 * Revision 1.1  2001/12/18 03:11:39  ychen
 * Added two files to get Nch defined in flow paper. Yu Chen
 *
 *
 **************************************************************************/
#ifndef StuRefCentrality2_hh
#define StuRefCentrality2_hh

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
inline unsigned int
binOfCentrality(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return 0;

    size_t countedTracks = uncorrectedMultiplicity(evt);

    unsigned int cent[] = {0,14,30,60,107,176,271,400,569,675};
//    float percent[] = {0.80,0.70,0.60,0.50,0.40,0.30,0.20,0.10,0.05,0.00};
    
    if (countedTracks < cent[1])       { return 1; }
    else if (countedTracks < cent[2])  { return 2; }
    else if (countedTracks < cent[3])  { return 3; }
    else if (countedTracks < cent[4])  { return 4; }
    else if (countedTracks < cent[5])  { return 5; }
    else if (countedTracks < cent[6])  { return 6; }
    else if (countedTracks < cent[7])  { return 7; }
    else if (countedTracks < cent[8])  { return 8; }
    else if (countedTracks < cent[9])  { return 9; }
    else                               { return 10; }
}
inline float
percentOfCentrality(StEvent& evt)
{
    StPrimaryVertex* primVtx = evt.primaryVertex();

    if (!primVtx) return -1.;
    
    size_t countedTracks = uncorrectedMultiplicity(evt);

    unsigned int cent[] = {0,14,30,60,107,176,271,400,569,675};
    float percent[] ={1.00,0.80,0.70,0.60,0.50,0.40,0.30,0.20,0.10,0.05,0.00};
    
    if (countedTracks < cent[1])       { return percent[0]; }
    else if (countedTracks < cent[2])  { return percent[1]; }
    else if (countedTracks < cent[3])  { return percent[2]; }
    else if (countedTracks < cent[4])  { return percent[3]; }
    else if (countedTracks < cent[5])  { return percent[4]; }
    else if (countedTracks < cent[6])  { return percent[5]; }
    else if (countedTracks < cent[7])  { return percent[6]; }
    else if (countedTracks < cent[8])  { return percent[7]; }
    else if (countedTracks < cent[9])  { return percent[8]; }
    else                               { return percent[9]; }
}

#endif



