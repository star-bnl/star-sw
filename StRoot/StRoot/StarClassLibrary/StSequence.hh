/***************************************************************************
 *
 * $Id: StSequence.hh,v 1.1 1999/02/19 16:27:48 fisyak Exp $
 *
 * Author: bl/Christof Struck, Jan 20, 1999
 *         based on design outlined in:
 *         DAQ interface to Offline 12/30/98 (M. Levine, M. Shultz, B. Lasiuk)
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSequence.hh,v $
 * Revision 1.1  1999/02/19 16:27:48  fisyak
 * Add from StarClassLibary
 *
 * Revision 1.1  1999/02/01 22:33:37  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_SEQUENCE_HH
#define ST_SEQUENCE_HH

struct StSequence {
    unsigned short startTimeBin;    // the time of the sequence start
    unsigned short length;          // in units of time bins
    unsigned char* firstAdc;        // ptr to the first hit in the sequence
};

#endif
