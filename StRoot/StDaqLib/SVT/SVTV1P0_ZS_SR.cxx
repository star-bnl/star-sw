/***************************************************************************
 *      
 * $Id: SVTV1P0_ZS_SR.cxx,v 1.1 2000/06/06 18:08:31 jml Exp $
 *      
 * Author: M.J. LeVine and Marcelo Munhoz(for the SVT group)
 *      
 ***************************************************************************
 *      
 * Description: SVT Zero Suppressed Reader
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0_ZS_SR.cxx,v $
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *      
 **************************************************************************/
#include <iostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "SVTV1P0.hh"
// 

SVTV1P0_ZS_SR::SVTV1P0_ZS_SR(int w, SVTV1P0_Reader *det)
{}

SVTV1P0_ZS_SR::SVTV1P0_ZS_SR(int b, int l, int w, SVTV1P0_Reader *det)
{}

int SVTV1P0_ZS_SR::initialize()
{
  return TRUE;
}

SVTV1P0_ZS_SR::~SVTV1P0_ZS_SR()
{}

int SVTV1P0_ZS_SR::getPadList(int Hybrid, u_char **anodeList)
{
return 0;
}

int SVTV1P0_ZS_SR::getSequences(int Hybrid, int Anode, int *nSeq, 
				Sequence **SeqData)
{
  return 0;
}

int SVTV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				Sequence **SeqData)
{
  return 0;
}


// Read the clusters (space points) found in the mezzanine cluster-finder
int SVTV1P0_ZS_SR::getSpacePts(int Hybrid, int *nSpacePts, SpacePt **SpacePts)
{
  return TRUE;
}


int SVTV1P0_ZS_SR::MemUsed()
{
  return 0;
}

