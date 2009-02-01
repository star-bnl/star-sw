/**************************************************************************
 * Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 *                                                                        *
 * Author: The ALICE Off-line Project.                                    *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

/*
$Log: gcadd.cxx,v $
Revision 1.1.1.2  2009/02/01 17:10:07  fisyak
Merge with .DEV2

Revision 1.1.1.1  2008/12/10 20:46:04  fisyak
Merge with macos version

Revision 1.1.1.1  2005/05/25 22:36:40  fisyak
Alice version of geant3 (-minicern)

Revision 1.2  2003/11/28 11:23:55  brun
New version of geant321 with all geant3 routines renamed from G to G3

Revision 1.1.1.1  2002/07/24 15:56:24  rdm
initial import into CVS

Revision 1.1.1.1  2002/06/16 15:17:54  hristov
Separate distribution  of Geant3

Revision 1.5  2000/12/20 09:46:49  alibrary
dlsym not supported on HP, reverting to gcomad

Revision 1.3  1999/09/29 09:24:14  fca
Introduction of the Copyright and cvs Log

*/

#if defined(CERNLIB_WINNT)
  #define gcaddb GCADDB
  #define gcaddc GCADDC
  #define gcaddf GCADDF
  #define gcaddd GCADDD
  #define gcaddi GCADDI
  #define gcaddl GCADDL
  #define type_of_call _stdcall
#else
  #define gcaddb gcaddb_
  #define gcaddc gcaddc_
  #define gcaddf gcaddf_
  #define gcaddd gcaddd_
  #define gcaddi gcaddi_
  #define gcaddl gcaddl_
  #define type_of_call
#endif

extern "C" bool* type_of_call gcaddb(bool *arg)
{
  return arg;
}
extern "C" char* type_of_call gcaddc(char *arg)
{
  return arg;
}
extern "C" double* type_of_call gcaddd(double *arg)
{
  return arg;
}
extern "C" int*  type_of_call gcaddi(int  *arg)
{
  return arg;
}
extern "C" float* type_of_call gcaddf(float *arg)
{
  return arg;
}
extern "C" int* type_of_call gcaddl(int *arg)
{
  return arg;
}
