#ifndef BEMCHISTOGRAMS
#define BEMCHISTOGRAMS

#define BEMCNOSWAP 1

#define BEMCOK 1
#define BEMCNOTINSTALLED 2
#define BEMCCORRUPTED 3

#define BEMCNJET 12

class TFile;

int bemcSave(TFile*);
int bemcReset();
int bemcMakeHisto();
int bemcInit();
int bemcFillHisto(char* rdr, const unsigned char *dsmL0WestInput = 0, const unsigned char *dsmL0EastInput = 0);

#endif





/***************************************************************************
 *
 * $Id: bemc.h,v 1.3 2009/01/24 01:13:50 ogrebeny Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: bemc.h,v $
 * Revision 1.3  2009/01/24 01:13:50  ogrebeny
 * Now uses the new DAQ reader
 *
 * Revision 1.2  2009/01/21 03:22:38  ogrebeny
 * Made it compilable with the old EVP_READER
 *
 * Revision 1.1  2009/01/18 00:58:31  ogrebeny
 * Better separate EMC histogramming from OnlinePlots infrastructure
 *
 * Revision 1.3  2009/01/08 20:10:51  fine
 * fix the bemc interfaces
 *
 * Revision 1.2  2009/01/08 19:39:28  fine
 * fix the bemcFillHisto function signature  HistoHandler.cxx
 *
 * Revision 1.1  2007/02/27 15:23:40  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/

