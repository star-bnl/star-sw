/***************************************************************************
 * $Id: swaps.hh,v 1.2 1999/07/02 04:37:42 levine Exp $
 * Author: M.W. Schulz
 ***************************************************************************
 * Description: swap method function prototypes for use on DAQ banks
 *
 ***************************************************************************
 * $Log: swaps.hh,v $
 * Revision 1.2  1999/07/02 04:37:42  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/
#ifndef SWAPS_HH
#define SWAPS_HH


int swap_raw(int data_byte_ordering,int* data, int size);
int swap_short(int data_byte_ordering,int* data, int size);

#endif
