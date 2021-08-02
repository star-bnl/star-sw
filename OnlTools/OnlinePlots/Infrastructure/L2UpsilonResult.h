// -*- C++ -*-
//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// 13 March 2007
//
// L2Result must not exceed 6 words
//


// 13/03/2007 Frank Laue : copied this structure from Pibero's offline code


#ifndef L2UpsilonResult_h
#define L2UpsilonResult_h

#include "rts.h"

#include <iostream>
using namespace std;


class L2UpsilonResult {
public:
  unsigned short tag;
  unsigned short time;		    // Microseconds

  unsigned short eventsSeen;
  unsigned short eventsAccepted;

  unsigned short triggerTowerL0;    // softId
  unsigned short triggerTowerL2;    // softId

  unsigned char  numberOfTowersL0;
  unsigned char  numberOfTowersL2;
  unsigned short energyOfClusterL0; // MeV

  unsigned short energyOfClusterL2; // MeV
  unsigned short invMass;	    // MeV

  float cosTheta;
  float vertexZ;                    // cm
  int   reserved;
  void swap() {
    unsigned int* tail = (unsigned int*)&reserved; 
    unsigned int* pos =  (unsigned int*)&tag;
    tail++;
    while ( pos  < tail ) {
      *pos = swap32( *pos );
      pos++;
    }
  }
  

};

#endif


/*************************************************************************************
 $Id: L2UpsilonResult.h,v 1.1 2009/01/23 16:10:56 jeromel Exp $
 *************************************************************************************
 $Log: L2UpsilonResult.h,v $
 Revision 1.1  2009/01/23 16:10:56  jeromel
 Import from online/RTS/src/

 Revision 1.1  2007/05/07 19:14:29  laue
 Initial version

*************************************************************************************/
