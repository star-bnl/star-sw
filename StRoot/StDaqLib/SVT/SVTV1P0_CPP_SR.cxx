/***************************************************************************
 *      
 * $Id: SVTV1P0_CPP_SR.cxx,v 1.3 2007/12/24 06:04:27 fine Exp $
 *      
 * Author: M.J. LeVine and Marcelo Munhoz(for the SVT group)
 *      
 ***************************************************************************
 *      
 * Description: SVT reader for raw cluster pointer banks (SVTCPPR)
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0_CPP_SR.cxx,v $
 * Revision 1.3  2007/12/24 06:04:27  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.2  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *      
 **************************************************************************/

#include <Stiostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "SVTV1P0.hh"

using namespace OLDEVP;
SVTV1P0_CPP_SR::SVTV1P0_CPP_SR(int w, SVTV1P0_Reader *det)
{}

SVTV1P0_CPP_SR::SVTV1P0_CPP_SR(int b, int l, int w, SVTV1P0_Reader *det)
{}

int SVTV1P0_CPP_SR::initialize()
{
  return TRUE;
}

int SVTV1P0_CPP_SR::getAsicParams(ASIC_params *params)
{
  return FALSE;
}

SVTV1P0_CPP_SR::~SVTV1P0_CPP_SR()
{
  //  cout << "Deleting SVTV1P0_CPP_SR" << endl;
}

int SVTV1P0_CPP_SR::getClusters(int Hybrid, int Anode, 
		  int *nClusters, struct ASIC_Cluster **clusters)
{
  return 0;
}


int SVTV1P0_CPP_SR::MemUsed()
{
  return 0;
}



