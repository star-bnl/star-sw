/***************************************************************************
 *
 * $Id: StHiMicroTrack.cxx,v 1.1 2002/04/02 19:36:15 jklay Exp $                                                         
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This is a uDST for highpt Analysis: Track Information               
 *               
 ***************************************************************************
 *
 * $Log: StHiMicroTrack.cxx,v $
 * Revision 1.1  2002/04/02 19:36:15  jklay
 * Bums highpt uDST format
 *
 *
 **************************************************************************/
#include "StHiMicroTrack.h"
#include <iostream>
#define DEBUG 0

ClassImp(StHiMicroTrack)


StHiMicroTrack::StHiMicroTrack()
{ 
  if(DEBUG)
    cout << "StHiMicroTrack::StHiMicroTrack" << endl;
}

StHiMicroTrack::~StHiMicroTrack()
{ 
  if(DEBUG)
    cout << "StHiMicroTrack::~StHiMicroTrack" << endl;

}
//______________________


