/**************************************************************************
 * Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.       *
 *                                                                        *
 * Author: STAR Integrated Track Task Force                               *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 *                                                                        *
 **************************************************************************/

/**************************************************************************
 *                                                                        *
 * StiHit					                  *
 *                                                                        *
 * Author:  Claude Pruneau, Wayne State University                        *
 * Created: April 2001                                                    *
 *                                                                        *
 * Description: A concrete class used to hold information about hits      *
 * used in the reconstruction of tracks in Star detector                  *
 *                                                                        *
 **************************************************************************/
#include "StiHit.h"

ClassImp(StiHit) 

StiHit::StiHit()
{
  x=0;
  y=0;
  z=0;
}

StiHit::~StiHit()
{  
}

