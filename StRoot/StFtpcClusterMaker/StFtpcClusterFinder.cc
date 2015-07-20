// $Id: StFtpcClusterFinder.cc,v 1.83 2015/07/20 17:37:01 jeromel Exp $
//
// $Log: StFtpcClusterFinder.cc,v $
// Revision 1.83  2015/07/20 17:37:01  jeromel
// Use std::isnan to satisfy C++11
//
// Revision 1.82  2011/09/09 21:32:42  jcs
// Undo rev 1.81 since different cluster finding parameters are used for laser and t0 calibration
// Comment out the different parameters when studying cluster finding and/or tracking
//
// Revision 1.81  2011/09/09 12:53:19  jcs
// use the same cluster finding parameters for debug (fdbg) that are used for production
//
// Revision 1.80  2011/05/21 15:33:30  jcs
// make corrections to save correct ChargeSum for clusters (one line of code
// was deleted inadvertently with update 1.79 and the wrong conversion between
// integer and float was performed when splitting merged clusters)
//
// Revision 1.79  2010/04/08 16:46:16  jcs
// swap data for RDO6,RDO7 FTPC East when Calibrations_ftpc/ftpcElectronics->swapRDO6RDO7East=1
//
// Revision 1.78  2009/12/11 15:41:06  jcs
// For laser run use laserTZero and no inner cathode correction
//
// Revision 1.77  2009/11/25 19:50:15  jcs
// remove all references to StFtpcSoftwareMonitor
//
// Revision 1.76  2009/11/14 12:51:08  jcs
// added suggested parentheses to avoid warnings which appeared with system upgrade
//
// Revision 1.75  2009/10/14 15:52:43  jcs
// write out all gas temperature, air pressure info to Run branch of FTPC debug root file
//
// Revision 1.74  2008/07/14 19:47:02  jcs
// The inner cathode correction is an Ftpc geometry correction; it should be applied to both data and laser runs
//
// Revision 1.73  2007/02/01 11:57:00  jcs
// move unessential output from INFO to DEBUG
//
// Revision 1.72  2007/01/15 07:49:21  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.71  2006/05/04 06:18:48  jcs
// remove old debug statement
//
// Revision 1.70  2006/03/19 19:29:44  jcs
// Move cluster struct definitions to StFtpcClustersStructures.hh
// Create DEBUGFILE with bfc option 'fdbg'
//
// Revision 1.69  2005/12/02 09:03:10  jcs
// Delete pradius,pdeflection before error exits to avoid memory leaks
//
// Revision 1.68  2005/07/12 15:18:01  jcs
// save raw (not corrected) timeposition
//
// Revision 1.67  2005/07/11 13:33:03  jcs
// remove duplicate code
//
// Revision 1.66  2005/06/30 09:19:06  jcs
// remove commented out values which were moved to Geometry_ftpc/ftpcClusterGeom a long time ago
//
// Revision 1.65  2004/12/10 23:07:46  jcs
// Only fill FTPC software monitor if it exists
//
// Revision 1.64  2004/09/07 14:08:17  jcs
// use the IAttr(".histos") to control histogramming
// remove obsolete clfradius histogram
//
// Revision 1.63  2004/08/09 12:50:14  jcs
// move initialization or westHits and eastHits outside of loop over iftpc
//
// Revision 1.62  2004/08/03 00:48:32  oldi
// Printout of found clusters in FTPC east and west fixed.
//
// Revision 1.61  2004/06/18 12:04:57  jcs
// replace #ifdef...#elif...#endif conditional compiler directives with #ifdef...#endif #ifdef...#endif
//
// Revision 1.60  2004/06/18 09:04:39  jcs
// replace obsolete DEBUGFILE code with code to write out a root file for cluster/laser analysis
//
// Revision 1.59  2004/05/24 13:38:47  jcs
// save number of clusters found in StFtpcSoftwareMonitor
//
// Revision 1.58  2004/05/19 09:53:38  jcs
// print out number of clusters found for both Ftpcsy
//
// Revision 1.57  2004/04/06 18:36:51  oldi
// New data mebers for pad and time position and pad and time sigma filled.
//
// Revision 1.56  2004/01/28 02:04:43  jcs
// replace all instances of StFtpcReducedPoint and StFtpcPoint with StFtpcConfMapPoint
//
// Revision 1.55  2004/01/28 01:41:13  jeromel
// Change OST to OS everywhere since defaultoption is now not to print
// the date.
//
// Revision 1.54  2004/01/02 10:27:38  jcs
// remove incorrect correction to lower limit in fastlog filling loop
//
// Revision 1.53  2003/12/09 10:48:57  jcs
// correct lower limit in fastlog filling loop
//
// Revision 1.52  2003/11/27 03:13:13  perev
// protection sqrt(-1) added
//
// Revision 1.51  2003/10/08 13:49:53  jcs
// initialize pointers and arrays
//
// Revision 1.50  2003/09/02 17:58:14  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.49  2003/06/11 12:06:03  jcs
// get inner cathode and cluster geometry parameters from database
//
// Revision 1.48  2003/05/15 20:29:50  putschke
// bug fixed
//
// Revision 1.47  2003/05/07 15:09:22  putschke
// improvements for cathode offset corretions
//
// Revision 1.46  2003/04/23 15:13:13  putschke
// minor change in padtrans
//
// Revision 1.45  2003/04/15 11:35:39  putschke
// Include corrections for inner cathode offset and move some parameter to database
//
// Revision 1.44  2003/01/21 09:34:31  jcs
// initialize variables to eliminate compiler warnings for NODEBUG=yes
//
// Revision 1.43  2003/01/16 18:03:52  oldi
// Bug eliminated. Now it compiles on Solaris again.
//
// Revision 1.42  2003/01/14 12:58:00  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.41  2002/11/15 11:52:05  jcs
// correct error in merging CUCs for statement condition
// (CurrentCUC->Sequence[159] was not filled for the case of >= MAXNUMSEQUENCES)
//
// Revision 1.40  2002/11/07 13:27:30  oldi
// Eliminated a very dumb mistake.
//
// Revision 1.39  2002/11/06 13:43:59  oldi
// Flag for clusters not to be used for tracking introduced.
//
// Revision 1.38  2002/08/02 13:03:55  oldi
// Wrong pad ordering was corrected in previous version already
// (see code changes there).
//
// Revision 1.37  2002/08/02 11:26:33  oldi
// Chargestep corrected (it was looping over the sequences twice).
// Pad ordering corrected in this version already.
//
// Revision 1.36  2002/07/15 13:30:35  jcs
// incorporate charge step histos into cluster finder and remove StFtpcChargeStep
//
// Revision 1.35  2002/06/04 12:33:09  putschke
// new 2-dimenisional hitfinding algorithm
// correct error in padposition numbering
//
// Revision 1.34  2002/04/22 09:53:45  jcs
// correct errors in calculation of cluster phi angle for ftpc east (Frank Simon)
//
// Revision 1.33  2002/04/05 16:45:54  oldi
// Small code clean ups, to be sure that this part is recompiled. It relies
// on StFtpcTracker/StFtpcPoint.* which were changed.
//
// Revision 1.32  2002/03/22 08:58:46  jcs
// invert cluster pad number in FTPC East
// convert cluster cartestian coordinates for FTPC west into STAR global coordinate system
//
// Revision 1.31  2002/03/22 08:52:52  jcs
// correct memory leaks found by Insure
//
// Revision 1.30  2002/03/08 16:32:34  jcs
// initialize bSlope to prevent nan's
//
// Revision 1.29  2002/03/05 16:51:35  jcs
// force data type definitions to avoid compiler warnings (this is a correct
// but inelegant fix which must be changed)
//
// Revision 1.28  2002/03/01 14:22:20  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.26  2002/02/10 21:10:44  jcs
// allow for individual west/east Ftpc temperature/pressure corrections
//
// Revision 1.25  2002/01/21 22:09:29  jcs
// use average FTPC gas temperature adjusted air pressure
//
// Revision 1.24  2001/12/12 16:01:17  jcs
// Remove old PhiDeflect definitions
// PhiDeflect has correct sign, add instead of subtracting in calculation of phi angle
//
// Revision 1.23  2001/11/21 12:51:27  jcs
// replace malloc/free with new/delete; prevent overstepping table boundaries
//
// Revision 1.22  2001/08/20 00:35:17  jcs
// check index (j) before using it
//
// Revision 1.21  2001/07/12 10:42:05  jcs
// reject clusters outside FTPC sensitive volume
// change to linear interpolation method in padtrans
// create and fill new histogram (currently inactive)
//
// Revision 1.20  2001/07/05 13:47:03  jcs
// move debug statement to correct location
//
// Revision 1.19  2001/06/25 00:45:35  jcs
// change DEBUG print statement to print out both soft and hard row/sector info
//
// Revision 1.18  2001/06/24 21:08:22  jcs
// Change Hit rejected message since this also occurs for FTPC daq data
//
// Revision 1.17  2001/04/23 19:37:40  oldi
// Output will be sent to StMessMgr.
//
// Revision 1.16  2001/04/19 11:32:39  oldi
// Reject clusters with x = 0. and y = 0.
// This is just to avoid a crash in StFtpcTrackMaker but the origin of the problem
// is in the FTPC slow simulator.
//
// Revision 1.15  2001/04/02 12:10:11  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.14  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
// Revision 1.13  2001/03/06 23:33:38  jcs
// use database instead of params
//
// Revision 1.12  2001/01/25 15:25:21  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.11  2001/01/15 18:18:00  jcs
// replace hidden constants with parameters
//
// Revision 1.10  2000/11/27 14:09:07  hummler
// implement tzero and lorentz angle correction factor
//
// Revision 1.9  2000/11/14 13:08:04  hummler
// add charge step calculation, minor cleanup
//
// Revision 1.8  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
// Revision 1.6  2000/04/13 18:08:21  fine
// Adjusted for ROOT 2.24
//
// Revision 1.5  2000/01/27 09:47:16  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
// Revision 1.4  2000/01/03 12:48:44  jcs
// Add CVS Id strings
//

#include <Stiostream.h>
#include <stdlib.h>
#include "StMessMgr.h"
#include "StFtpcClusterFinder.hh"
#include "StFtpcTrackMaker/StFtpcConfMapPoint.hh"
#include "math_constants.h"
#include <math.h>
#include "TH1.h"

#include "PhysicalConstants.h"

#include "asic_map_correction.h"


// Constructor used for production

StFtpcClusterFinder::StFtpcClusterFinder(StFTPCReader *reader,  
					 StFtpcParamReader *paramReader,
                                         StFtpcDbReader *dbReader,
					 TObjArray *pointarray,
					 TH2F *hpad,
					 TH2F *htime,
                                         TH2F *histo,
                                         TH1F *histoW,
                                         TH1F *histoE)
{
  LOG_DEBUG << "StFtpcClusterFinder constructed for production" << endm;  
  mReader = reader;
  mParam = paramReader; 
  mDb    = dbReader;
  mPoint = pointarray;
  mHisto=histo;
  mHistoW=histoW;
  mHistoE=histoE;

  mcldebug = NULL;

  MAXSEQPEAKS = mParam->maxNumSeqPeaks();
  MAXPEAKS = mParam->maxNumPeaks();
  MAXLOOPS = mParam->maxLoops();
  MAXFASTLOOPS = mParam->maxFastLoops();
  UNFOLDLIMIT = mParam->unfoldLimit();
  UNFOLDFAILEDLIMIT = mParam->unfoldFailedLimit();

  mMinTimeBin = mDb->minTimeBin();
  mMinTimeBinMed = mDb->minTimeBinMed();
  mMinTimeBinOut = mDb->minTimeBinOut();

  mMaxPadlength = mDb->maxPadLength();
  mMaxTimelength = mDb->maxTimeLength();
  mMaxPadlengthMed = mDb->maxPadLengthMed();
  mMaxTimelengthMed = mDb->maxTimeLengthMed();
  mMaxPadlengthOut = mDb->maxPadLengthOut();
  mMaxTimelengthOut = mDb->maxTimeLengthOut();

  DeltaTime = mDb->deltaTime();
  DeltaPad = mDb->deltaPad();

  mMinChargeWindow = mDb->minChargeWindow();

  mOffsetCathodeWest = mDb->offsetCathodeWest();
  mOffsetCathodeEast = mDb->offsetCathodeEast();
  mAngleOffsetWest = mDb->angleOffsetWest();
  mAngleOffsetEast = mDb->angleOffsetEast();

  mhpad = hpad;
  mhtime = htime;
}

// Constructor used to produce the FTPC special root file used for FTPC
// laser, t0 and cluster calibration

StFtpcClusterFinder::StFtpcClusterFinder(StFTPCReader *reader,  
					 StFtpcParamReader *paramReader,
                                         StFtpcDbReader *dbReader,
					 TObjArray *pointarray,
					 TH2F *hpad,
					 TH2F *htime,
                                         TH2F *histo,
                                         TH1F *histoW,
                                         TH1F *histoE,
					 StFtpcClusterDebug *cldebug)
{
  LOG_DEBUG << "StFtpcClusterFinder constructed for calibration" << endm;  
  mReader = reader;
  mParam = paramReader; 
  mDb    = dbReader;
  mPoint = pointarray;
  mHisto=histo;
  mHistoW=histoW;
  mHistoE=histoE;

  mcldebug = cldebug;

  MAXSEQPEAKS = mParam->maxNumSeqPeaks();
  MAXPEAKS = mParam->maxNumPeaks();
  MAXLOOPS = mParam->maxLoops();
  MAXFASTLOOPS = mParam->maxFastLoops();
  UNFOLDLIMIT = mParam->unfoldLimit();
  UNFOLDFAILEDLIMIT = mParam->unfoldFailedLimit();

  mMinTimeBin = mDb->minTimeBin();
  mMinTimeBinMed = mDb->minTimeBinMed();
  mMinTimeBinOut = mDb->minTimeBinOut();

  mMaxPadlength = mDb->maxPadLength();
  mMaxTimelength = mDb->maxTimeLength();
  mMaxPadlengthMed = mDb->maxPadLengthMed();
  mMaxTimelengthMed = mDb->maxTimeLengthMed();
  mMaxPadlengthOut = mDb->maxPadLengthOut();
  mMaxTimelengthOut = mDb->maxTimeLengthOut();

  DeltaTime = mDb->deltaTime();
  DeltaPad = mDb->deltaPad();

  mMinChargeWindow = mDb->minChargeWindow();

// For studying cluster finding and tracking comment out the following lines of code
// from here ->
// Set FTPC inner cathode offsets = 0
  mOffsetCathodeWest = 0.0;
  mOffsetCathodeEast = 0.0;
  mAngleOffsetWest =   0.0;
  mAngleOffsetEast =   0.0;


// Set ftpcClusterPars
  MAXPEAKS = 160;

// Set FTPC Cluster Geometry values for cluster/laser analysis
  mMinChargeWindow = 30;
  mMaxPadlengthOut = 30;
  mMaxTimelengthOut = 30;
// <- to here

  mhpad = hpad;
  mhtime = htime;

}

StFtpcClusterFinder::~StFtpcClusterFinder()
{
//   LOG_DEBUG << "StFtpcClusterFinder destructed" << endm;
}

int StFtpcClusterFinder::search()
{

  Double_t  *pradius = 0;
  Double_t  *pdeflection = 0;
  int iRow, iSec, iPad, iPadBuf;
  int iRowBuf, iSecBuf;
  int firstPadrowToSearch;
  int bNewSec;
  int clusters;
  int iNowSeqIndex, iNewSeqIndex, iOldSeqNumber, iOldSeqIndex;
  int iCUCSequence, iMoveSequence, iOldSeqBuf;
  int bOldSequenceUsed, bLastSequence;
  TClusterUC *FirstCUC, *CurrentCUC, *LastCUC, *DeleteCUC;
  TClusterUC *SequenceInCUC;
  TPCSequence *OldSequences, *NewSequences, *(SequencePointer[3]);
  int iNewSeqNumber;

  int iIndex;
  float fastlog[256];

  double deltaAirPressure;

  /* variables for dynamic CUC memory handling */
  TClusterUC CUCMemory[MAXNUMCUC];
  int CUCMemoryArray[MAXNUMCUC];
  int CUCMemoryPtr;

  /* allocate memory for padtrans table */
  pradius = new Double_t[mParam->numberOfDriftSteps()
                           *mDb->numberOfPadrowsPerSide()];
  memset(pradius, 0, (mParam->numberOfDriftSteps()*mDb->numberOfPadrowsPerSide())*sizeof(Double_t));
  pdeflection = new Double_t[mParam->numberOfDriftSteps()
                               *mDb->numberOfPadrowsPerSide()];
  memset(pdeflection, 0, (mParam->numberOfDriftSteps()*mDb->numberOfPadrowsPerSide())*sizeof(Double_t));

  if(pradius == 0 || pdeflection == 0)
    {
      LOG_ERROR << "Padtrans memory allocation failed, exiting!" << endm;
      if (pradius != 0) delete[] pradius;        // release the pradius array
      if (pdeflection != 0) delete[] pdeflection;   // release the pdeflection array
      return kStERR;
    }

// Loop over FTPC West and East individually
  // initialize to elimate compiler warning when setenv NODEBUG yes 
  firstPadrowToSearch = 0;
  deltaAirPressure = 0;  

Int_t westHits = 0;
Int_t eastHits = 0;
for ( int iftpc=0; iftpc<2; iftpc++) {
   if ( iftpc == 0 ) {
      deltaAirPressure = mParam->adjustedAirPressureWest() - mParam->standardPressure();
      firstPadrowToSearch = mDb->firstPadrowToSearch() - 1;
      LOG_DEBUG << "Ftpc West: deltaAirPressure = adjustedAirPressureWest ("<<mParam->adjustedAirPressureWest()<<") - standardPressure ("<<mParam->standardPressure()<<") = "<<deltaAirPressure<<endm;
   }
   if ( iftpc == 1 ) {
      deltaAirPressure = mParam->adjustedAirPressureEast() - mParam->standardPressure();
      firstPadrowToSearch = mDb->firstPadrowToSearch() - 1 + mDb->numberOfPadrowsPerSide();
      LOG_DEBUG <<"Ftpc East: deltaAirPressure = adjustedAirPressureEast ("<<mParam->adjustedAirPressureEast()<<") - standardPressure ("<<mParam->standardPressure()<<") = "<<deltaAirPressure<<endm;
   }

  /* integrate padtrans table from magboltz database */
  if(!calcpadtrans(pradius, pdeflection,deltaAirPressure))
    {
      LOG_ERROR << "Couldn't calculate padtrans table, exiting!" << endm;
      delete[] pradius;        // release the pradius array
      delete[] pdeflection;   // release the pdeflection array
      return kStERR;
    }

  /* initialize CUC memory handling */
  if(!cucInit(CUCMemory, CUCMemoryArray, &CUCMemoryPtr))
    {
      LOG_ERROR <<  "Couldn't initialize CUC memory, exiting!" << endm;
      delete[] pradius;        // release the pradius array
      delete[] pdeflection;   // release the pdeflection array
      return kStERR;
    }

  /* calculate fastlog lookup */
  for(iIndex=1; iIndex<256; iIndex++)
    {
      fastlog[iIndex] = ::log((double) iIndex);
    }

  /* reset counter for found clusters */
  clusters = 0;

  /* initialize sequence and cluster lists */ 
  NewSequences = 0;
  FirstCUC = 0;
  LastCUC = 0;
  iOldSeqNumber = 0;
  iNewSeqIndex = 0;

  /* loop over raw data sequences */
  iNowSeqIndex = 0;
  iPad=0;
  iSec=0;
  iRow=0;
  bLastSequence=0;

  for(iRow=firstPadrowToSearch,iRowBuf=firstPadrowToSearch;iRow<firstPadrowToSearch+mDb->numberOfPadrowsPerSide(); iRow++)
    {
      for(iSec=mDb->firstSectorToSearch()-1,
	    iSecBuf=mDb->firstSectorToSearch()-1; 
	  iSec<mDb->lastSectorToSearch(); iSec++)
	{
	  // new sector, set pad buffer so there can be no matches
	  iPadBuf=-2;
	  bNewSec=TRUE;

	  // calculate hardware (daq) sectors from software position
	  iHardSec = mDb->numberOfSectors()*(int)(iRow/2) + iSec + 1;
	  iHardRow = iRow%2 + 1;

          if (DebugOn) {
	     LOG_DEBUG<<"Cluster Finder: Now on Sector "<<iSec<<", Row "<<iRow<<" (iHardSec "<<iHardSec<<", iHardRow "<<iHardRow<<")"<<endm;
          }

	  // get list of occupied pads in sector
	  unsigned char *(padlist[2]);
	  int iOccPads=mReader->getPadList(iHardSec, iHardRow, 
					   padlist[iHardRow-1]);

	  // loop over occupied pads
	  int iThPad;
//===============================================================
      int newpadlist[160];

      for (iThPad=0; iThPad<160; iThPad++) 
               newpadlist[iThPad]=0;
  
      for(iThPad=0; iThPad<iOccPads; iThPad++)
      {
         iPad=padlist[iHardRow-1][iThPad];
         if ( mDb->Asic2EastNotInverted() && iRow>=10 && (iPad>=65 && iPad<=96))
             newpadlist[padkey[iPad-1]-1] = iPad; 
         else
             newpadlist[iPad-1] = iPad;
      }
      
//===============================================================
	  for(iThPad=0; iThPad<160; iThPad++)
	    {
              if (newpadlist[iThPad] == 0 ) continue;
	      iPad=iThPad+1;

	      // search, fit and remove finished CUCs  
	      for(CurrentCUC = FirstCUC; CurrentCUC!=0; 
		  CurrentCUC = CurrentCUC->NextClusterUC)
		{
		  // check if CurrentCUC is adjacent to this pad
		  if(iPad > CurrentCUC->EndPad + 1 || bNewSec)
		    {
		      // CurrentCUC is finished 
		      // if CUC has not been lost by merging, process cluster
		      if(CurrentCUC->EndPad > CurrentCUC->StartPad)
			{
			  // cluster processing: increment cluster counter 
			  clusters ++;

			  // cluster processing: call hitfinder 
			  if (geometryCut(CurrentCUC))
			    if(!findHits(CurrentCUC, iRowBuf, iSecBuf, 
					 pradius, pdeflection, 
					 fastlog)
			       )
			      {
                                if (DebugOn) {
				   LOG_DEBUG<<"Hitfinder failed! Cluster is lost"<<endm;
                                }
			      }
			}
		      DeleteCUC=CurrentCUC;
		      // bypass CurrentCUC in CUC list 
		      if(CurrentCUC==FirstCUC)
			{
			  FirstCUC=CurrentCUC->NextClusterUC;
			}
		      else
			{
			  LastCUC->NextClusterUC=CurrentCUC->NextClusterUC;
			  CurrentCUC=LastCUC;
			}
		      // free CurrentCUC memory 
		      if(!cucFree(CUCMemory, CUCMemoryArray, 
				  &CUCMemoryPtr, DeleteCUC))
			{
			  LOG_ERROR << "Fatal memory management error."  << endm;
                          delete[] pradius;        // release the pradius array
                          delete[] pdeflection;   // release the pdeflection array
			  return kStERR;
			}
		    }
		  LastCUC=CurrentCUC;
		}
	      iRowBuf=iRow;
	      iSecBuf=iSec;

	      // initialize sequence lists: 
	      // new-array is moved to old 
	      // and first element initialized 
	      OldSequences=NewSequences;
	      iOldSeqNumber=iNewSeqIndex;
	      iNewSeqIndex=0;
      
	      if(iPad!=iPadBuf+1)
		{
		  iOldSeqNumber=0;
		}
	      iPadBuf=iPad;

	      // reset beginning of sequence comparison
	      iOldSeqBuf=0;

	      // get sequences on this pad
	      mReader->getSequences(iHardSec, iHardRow, newpadlist[iThPad], &iNewSeqNumber,
				   SequencePointer[iHardRow]);
	      NewSequences=SequencePointer[iHardRow];
	      
	      // loop over sequences
	      for(iNewSeqIndex=0; iNewSeqIndex < iNewSeqNumber; 
		  iNewSeqIndex++)
		{
if (mcldebug){
		  if (mcldebug->drawclhisto!=0)
		  {
		     mcldebug->drawhisto(iHardSec,iHardRow,iPad,NewSequences[iNewSeqIndex]);
	             mcldebug->drawgainhisto(iHardSec,iHardRow,iPad,((float)(mDb->amplitudeSlope((iSec*mDb->numberOfPads()+iPad),iRow))),NewSequences[iNewSeqIndex]);
                  }
}
//+++++++++++++++++++ fill charge step histograms +++++++++++++++
		  // This loop is running already, but the running variable is called iNewSeqIndex instead of iSeqIndex .
		  //int iSeqIndex;
		  //for(iSeqIndex=0; iSeqIndex < iNewSeqNumber; iSeqIndex++)
		  //{
                  int entry;
                  for(entry=0; entry<NewSequences[iNewSeqIndex].Length; entry++)
                    {
                      if (mHisto) mHisto->Fill(iHardSec-1, // sector
                                   entry+NewSequences[iNewSeqIndex].startTimeBin, //bin
                                   NewSequences[iNewSeqIndex].FirstAdc[entry]); // weight
                      if (iHardSec >= 1 && iHardSec <= 30 ) {
			mHistoW->Fill( entry+NewSequences[iNewSeqIndex].startTimeBin, //bin
				       NewSequences[iNewSeqIndex].FirstAdc[entry]); // weight
                      }
                      if (iHardSec >= 31 && iHardSec <= 60 ) {
			mHistoE->Fill( entry+NewSequences[iNewSeqIndex].startTimeBin, //bin
				       
				       NewSequences[iNewSeqIndex].FirstAdc[entry]); // weight
                      }
                    }
		  //}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

		  // mark sequence as unused 
		  SequenceInCUC = 0;

		  // compare this sequence to old sequences
		  for(iOldSeqIndex=iOldSeqBuf; iOldSeqIndex < iOldSeqNumber; 
		      iOldSeqIndex++)
		    {
		      // are beginning or end of new sequence between
		      // beginning and end of old sequence?
		      if(((NewSequences[iNewSeqIndex].startTimeBin >= 
			   OldSequences[iOldSeqIndex].startTimeBin) && 
			  (NewSequences[iNewSeqIndex].startTimeBin <= 
			   OldSequences[iOldSeqIndex].startTimeBin + 
			   OldSequences[iOldSeqIndex].Length-1)) || 
			 ((NewSequences[iNewSeqIndex].startTimeBin + 
			   NewSequences[iNewSeqIndex].Length-1 >= 
			   OldSequences[iOldSeqIndex].startTimeBin) && 
			  (NewSequences[iNewSeqIndex].startTimeBin + 
			   NewSequences[iNewSeqIndex].Length-1 <= 
			   OldSequences[iOldSeqIndex].startTimeBin + 
			   OldSequences[iOldSeqIndex].Length-1)) ||
			 ((OldSequences[iOldSeqIndex].startTimeBin >= 
			   NewSequences[iNewSeqIndex].startTimeBin) && 
			  (OldSequences[iOldSeqIndex].startTimeBin <= 
			   NewSequences[iNewSeqIndex].startTimeBin + 
			   NewSequences[iNewSeqIndex].Length-1)))
			{
			  // yes, matching sequence found 
			  // set beginning of search for next sequence 
			  iOldSeqBuf=iOldSeqIndex;
			  bOldSequenceUsed=0;
 
			  // compare matching sequences to old CUCs  
			  // loop over all active CUCs 
			  for(CurrentCUC = FirstCUC; CurrentCUC != 0; 
			      CurrentCUC = CurrentCUC->NextClusterUC)
			    {
			      LastCUC=CurrentCUC;
			      // loop over all sequences in CUC
			      for(iCUCSequence=1; 
				  iCUCSequence<CurrentCUC->NumSequences; 
				  iCUCSequence++)
				{
				  // is cuc sequence identical to 
				  // matching old sequence?
				  if((OldSequences[iOldSeqIndex].startTimeBin == 
				      CurrentCUC->Sequence[iCUCSequence].startTimeBin)
				     && (CurrentCUC->SequencePad[iCUCSequence] == 
					 iPad-1))
				    {
				      bOldSequenceUsed=1;
				      // matching old sequence is in CUC 
				      // check if new sequence is already used 
				      if(SequenceInCUC!=CurrentCUC)
					{
					  if(SequenceInCUC != 0 && SequenceInCUC!=CurrentCUC)
					    {
					      // yes, already used, merge CUCs 
					      // mark old CUC for removal
					      SequenceInCUC->EndPad = 
						SequenceInCUC->StartPad;
					      // set StartPad to the smaller StartPad
					      if(SequenceInCUC->StartPad < 
						 CurrentCUC->StartPad)
						{
						  CurrentCUC->StartPad = 
						    SequenceInCUC->StartPad;
						}
					      CurrentCUC->EndPad=iPad;
					      // append SequenceInCUC to CurrentCUC
					      // copy all sequences to CurrentCUC 
					      for(iMoveSequence=0; 
						  (iMoveSequence <SequenceInCUC->NumSequences) &&
						    ( (CurrentCUC->NumSequences+iMoveSequence) < MAXNUMSEQUENCES); 
						  iMoveSequence++)
						{
						  CurrentCUC->
						    Sequence[iMoveSequence + 
							    CurrentCUC->NumSequences] = 
						    SequenceInCUC->
						    Sequence[iMoveSequence];
						  CurrentCUC->
						    SequencePad[iMoveSequence + 
							       CurrentCUC->NumSequences] = 
						    SequenceInCUC->
						    SequencePad[iMoveSequence]; 
						}
					      // add up number of sequences
					      CurrentCUC->NumSequences += 
						SequenceInCUC->NumSequences;
					      if(CurrentCUC->NumSequences > MAXNUMSEQUENCES)
						{
						  CurrentCUC->NumSequences = MAXNUMSEQUENCES;
						}
					      
					      // sequence is now in CurrentCUC
					      SequenceInCUC=CurrentCUC;
					    }
					  else // to: if(SequenceInCUC!=0)
					    {
					      // sequence did not belong to any CUC before
					      // add sequence to CurrentCUC
					      if(CurrentCUC->NumSequences<MAXNUMSEQUENCES)
						{
						  CurrentCUC->Sequence[CurrentCUC->NumSequences]
						    = NewSequences[iNewSeqIndex];
						  CurrentCUC->SequencePad[CurrentCUC->NumSequences]
						    =iPad;
						  CurrentCUC->NumSequences++;
						}
					      CurrentCUC->EndPad=iPad;
					      SequenceInCUC=CurrentCUC;
				      
					      // check if new sequence touches sector limit
					      if(NewSequences[iNewSeqIndex].startTimeBin==0 ||
						 NewSequences[iNewSeqIndex].startTimeBin
						 +NewSequences[iNewSeqIndex].Length
						 ==mDb->numberOfTimebins()-1 || 
						 iPad==mDb->numberOfPads())
						{
						  CurrentCUC->CutOff=1;
						}
					    } // end of: if(SequenceInCUC!=0) ... else 
					} // end of: if(SequenceInCUC...) 
				    } // end of: if((OldSequences...)) 
				} // end of: for(ICUCSequence...) 
			    } // end of: for(CurrentCUC...)
			  if(SequenceInCUC==0 && NewSequences[iNewSeqIndex].Length>1)
			    {
			      // no matching CUC was found: create new CUC
			      // allocate memory
			      CurrentCUC=cucAlloc(CUCMemory, CUCMemoryArray, 
						  &CUCMemoryPtr);
			      if(CurrentCUC == 0)
				{
				  // no free memory, overwrite last CUC
                                  if (DebugOn) {
				     LOG_DEBUG<<"Previous cluster is now lost"<<endm;
                                  }
				  CurrentCUC=LastCUC;
                                  delete[] pradius;        // release the pradius array
                                  delete[] pdeflection;   // release the pdeflection array
                                  return kStWarn;
				}
			      else
				{
				  // set pointers to this CUC
				  if(FirstCUC == 0)
				    {
				      FirstCUC = CurrentCUC;
				    }
				  else
				    {
				      LastCUC->NextClusterUC=CurrentCUC;
				    }
				  
				  // this is the newest CUC 
				  CurrentCUC->NextClusterUC=0;
				}
		      
			      // fill new CUC structure
			      CurrentCUC->StartPad=iPad-1;
			      CurrentCUC->EndPad=iPad;
			      CurrentCUC->NumSequences=2;
		      
			      // copy sequences to CUC 
			      CurrentCUC->Sequence[0] =
				OldSequences[iOldSeqIndex];
			      // struct assignment operator used
			      // created copy of sequence in CUC
			      // pointer operation avoided because data in CUC
			      // will be calibrated
			      CurrentCUC->SequencePad[0] = iPad-1;
		      
			      CurrentCUC->Sequence[1] =
				NewSequences[iNewSeqIndex];
			      CurrentCUC->SequencePad[1]=iPad;
			      SequenceInCUC=CurrentCUC;
			      
			      // check if new CUC touches sector limits
			      if(iPad==1 || iPad==mDb->numberOfPads() || 
				 CurrentCUC->Sequence[0].startTimeBin==0 || 
				 CurrentCUC->Sequence[0].startTimeBin
				 +CurrentCUC->Sequence[0].Length
				 ==mDb->numberOfTimebins()-1 || 
				 CurrentCUC->Sequence[1].startTimeBin==0 || 
				 CurrentCUC->Sequence[1].startTimeBin
				 +CurrentCUC->Sequence[1].Length
				 ==mDb->numberOfTimebins()-1)
				{
				  CurrentCUC->CutOff=1;
				}
			      else
				{
				  CurrentCUC->CutOff=0;
				}
			    }   // end of: if(SequenceInCUC==0)
			  else
			    {
			      if(bOldSequenceUsed==0 && SequenceInCUC!=0)
				{  
				  // new sequence has been used but old one hasn't 
				  // append that to cluster, too
				  if(SequenceInCUC->NumSequences<MAXNUMSEQUENCES)
				    {
				      SequenceInCUC->Sequence[SequenceInCUC->NumSequences]
					= OldSequences[iOldSeqIndex];
				      SequenceInCUC->SequencePad[SequenceInCUC->NumSequences]
					=iPad-1;
				      SequenceInCUC->NumSequences++;
				    }
			  
				  // check if Old sequence touches sector limit 
				  if(OldSequences[iOldSeqIndex].startTimeBin==0 ||
				     OldSequences[iOldSeqIndex].startTimeBin
				     +OldSequences[iOldSeqIndex].Length
				     ==mDb->numberOfTimebins()-1 || 
				     iPad>=mDb->numberOfPads()-1)    //change == to >=
				    {
				      SequenceInCUC->CutOff=1;
				    }
				}
			    }
			} // end of: if(sequence matching) 
		    } // end of: for(iOldSeqIndex...) 
		} // end of: for(iNewSeqIndex...)
	      bNewSec=FALSE;
	    } // end of: for(iThPad...)

	  // sector done, fit and remove all remaining CUCs  
	  for(CurrentCUC = FirstCUC; CurrentCUC!=0; 
	      CurrentCUC = CurrentCUC->NextClusterUC)
	    {
	      // CurrentCUC is finished 
	      // if CUC has not been lost by merging, process cluster
	      if(CurrentCUC->EndPad > CurrentCUC->StartPad)
		{
		  // cluster processing: increment cluster counter 
		  clusters ++;
		  
		  // cluster processing: call hitfinder 
		  if (geometryCut(CurrentCUC))
		    if(!findHits(CurrentCUC, iRowBuf, iSecBuf, 
			       pradius, pdeflection, 
			       fastlog)
		       )
		      {
                        if (DebugOn) {
			   LOG_DEBUG<<"Hitfinder failed! Cluster is lost"<<endm;
                        }
		      }
		}
	      DeleteCUC=CurrentCUC;
	      // bypass CurrentCUC in CUC list 
	      if(CurrentCUC==FirstCUC)
		{
		  FirstCUC=CurrentCUC->NextClusterUC;
		}
	      else
		{
		  LastCUC->NextClusterUC=CurrentCUC->NextClusterUC;
		  CurrentCUC=LastCUC;
		}
	      // free CurrentCUC memory 
	      if(!cucFree(CUCMemory, CUCMemoryArray, 
			  &CUCMemoryPtr, DeleteCUC))
		{
		  LOG_ERROR << "Fatal memory management error." << endm;
                  delete[] pradius;        // release the pradius array
                  delete[] pdeflection;   // release the pdeflection array
		  return kStERR;
		}
	      LastCUC=CurrentCUC;
	    }
	  
	} // end of: for(iSec...)
    } // end of: for(iRow...)

  if (iftpc == 0 ) {
	  westHits = mPoint->GetEntriesFast();
	  LOG_INFO << "StFtpcClusterFinder found "  << clusters << " clusters and processed to " << westHits << " hits in Ftpc West." << endm;
  }	  
  if (iftpc == 1 ) {
	  eastHits = mPoint->GetEntriesFast() - westHits;
	  LOG_INFO << "StFtpcClusterFinder found "  << clusters << " clusters and processed to " <<  eastHits << " hits in Ftpc East." << endm;
  }
}  // end of: for(iftpc
  
  delete[] pradius;        // release the pradius array
  delete[] pdeflection;   // release the pdeflection array
  
  return kStOK;
}

bool StFtpcClusterFinder::geometryCut(TClusterUC *Cluster)
{
 
  int seqlength=0;
  int minTimebin=256;
  int maxTimebin=0;

  for(int iPad=Cluster->StartPad; iPad<=Cluster->EndPad; iPad++)
    {
      for(int iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	{
	  if(Cluster->SequencePad[iSequence] == iPad)
	    {
	      if (Cluster->Sequence[iSequence].Length > seqlength)
		seqlength=Cluster->Sequence[iSequence].Length;
	      if (Cluster->Sequence[iSequence].startTimeBin<minTimebin)
		minTimebin=Cluster->Sequence[iSequence].startTimeBin;
	      if ((Cluster->Sequence[iSequence].startTimeBin+Cluster->Sequence[iSequence].Length) > maxTimebin)
		maxTimebin=(Cluster->Sequence[iSequence].startTimeBin+Cluster->Sequence[iSequence].Length);
	    }
	}
    }

if (mcldebug)  {
   if (abs(Cluster->EndPad-Cluster->StartPad)<mMaxPadlengthOut && seqlength<mMaxTimelengthOut)
      return true;
   else
      return false;
}
  if (minTimebin>mMinTimeBin) return true;
  else if ((minTimebin>mMinTimeBinMed && minTimebin<=mMinTimeBin) && abs(Cluster->EndPad-Cluster->StartPad)<mMaxPadlengthMed && seqlength<mMaxTimelengthMed)
    return true;
  else if (minTimebin>mMinTimeBinOut && minTimebin<=mMinTimeBinMed && abs(Cluster->EndPad-Cluster->StartPad)<mMaxPadlengthOut && seqlength<mMaxTimelengthOut)
    return true;
  else
    return false;

}

int StFtpcClusterFinder::findHits(TClusterUC *Cluster, 
				 int iRow, 
				 int iSec, 
				 double *pRadius, 
				 double *pDeflection, 
				 float fastlog[256])
{

  int iNumPeaks; 
  int i,k;

  bool PeakFound;

  TPeak *Peaks = 0;
  Peaks = new TPeak[MAXPEAKS];
  float TClSearch [162][258]; 

  for (int i=0;i<162;i++)
    for (int j=0;j<258;j++)
      TClSearch[i][j]=0; 

  float cTemp;

  iNumPeaks=0;

  // read out cluster and fill window with gain correction
  // =====================================================

   for(int iPad=Cluster->StartPad; iPad<=Cluster->EndPad; iPad++)
    {
      for(int iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	{
	  if(Cluster->SequencePad[iSequence] == iPad)
	    {
	      for(int iIndex=0; iIndex< Cluster->Sequence[iSequence].Length; iIndex++)
		{
		  {
		    cTemp=((float)(unsigned int)(Cluster->Sequence[iSequence].FirstAdc[iIndex])
			    * mDb->amplitudeSlope(iSec*mDb->numberOfPads()+iPad,iRow) 
			    + mDb->amplitudeOffset(iSec*mDb->numberOfPads()+iPad,iRow));
		    Cluster->Sequence[iSequence].FirstAdc[iIndex]=(unsigned char) cTemp;
		    TClSearch[iPad][Cluster->Sequence[iSequence].startTimeBin+iIndex]=cTemp;
		  }
		}
	    }
	}
    }

   // check each cluster pixel for local maximum
   // ==========================================

   float cl_charge;

  for(int iPad=Cluster->StartPad; iPad<=Cluster->EndPad; iPad++)
    {
      for(int iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	{
	  if(Cluster->SequencePad[iSequence] == iPad)
	    {
	      for(int iTime=Cluster->Sequence[iSequence].startTimeBin; iTime <=Cluster->Sequence[iSequence].startTimeBin+Cluster->Sequence[iSequence].Length; iTime++)
		{

		  // ------------------------------------------------------------------

		  PeakFound=true;
		  cl_charge=0;

		  // search window
		  // =============

		  // check if > MaximumPad/Time
		   for (i=iPad-DeltaPad;i<=(iPad+DeltaPad) && (iPad+DeltaPad)<=160 && (iPad-DeltaPad)>=0;i++)
		     {
		      for (k=iTime-DeltaTime;k<=(iTime+DeltaTime) && (iTime+DeltaTime)<=256 && (iTime-DeltaTime)>=0;k++)
			{
			  if (i<iPad+DeltaPad && i>iPad-DeltaPad && k<iTime+DeltaTime && k>iTime-DeltaTime)
			    cl_charge=cl_charge+TClSearch[i][k];

			  if (TClSearch[iPad][iTime]>=TClSearch[i][k]+1.25 && TClSearch[iPad][iTime]>0 
			      && TClSearch[i][k]>0)
			    {
			    }
			  else if (TClSearch[iPad][iTime]<TClSearch[i][k] && TClSearch[i][k]>0 && TClSearch[i][k]>0)
			    {
			      PeakFound=false;
			    }
			}
		    }
		  
		  // if local maximum Found make Peak
		  // ================================

		   if (PeakFound && cl_charge>mMinChargeWindow && iNumPeaks<MAXPEAKS)
		    {
		      
		      if (iNumPeaks>0)
			{
			  if ((Peaks[iNumPeaks-1].Timebin!=iTime && Peaks[iNumPeaks-1].Timebin!=iTime-1 && Peaks[iNumPeaks-1].Timebin!=iTime+1) || Peaks[iNumPeaks-1].pad!=iPad )
			    {
			      Peaks[iNumPeaks].pad=iPad;
			      Peaks[iNumPeaks].Timebin=iTime;
			      Peaks[iNumPeaks].Sequence=Cluster->Sequence[iSequence];
			      iNumPeaks++;
			    }
			}
		      else
			{
			  Peaks[iNumPeaks].pad=iPad;
			  Peaks[iNumPeaks].Timebin=iTime;
			  Peaks[iNumPeaks].Sequence=Cluster->Sequence[iSequence];
			  iNumPeaks++;
			}
		    }
		  
		  // ------------------------------------------------------------------ 

		}
	    }
	  
	}
    }

  if(!fitPoints(Cluster, iRow, iSec, pRadius, pDeflection, Peaks, 
		iNumPeaks, fastlog))
    {
      if (DebugOn) {
         LOG_DEBUG<<"Point fitting failed! Cluster is lost."<<endm;
      }
      delete[] Peaks;
      return FALSE;
    }
  
  delete[] Peaks;
  return TRUE;
}

int StFtpcClusterFinder::fitPoints(TClusterUC* Cluster, 
				  int iRow, 
				  int iSec, 
				  double *pRadius, 
				  double *pDeflection, 
				  TPeak *Peak, 
				  int iNumPeaks, 
				  float fastlog[256])
{
  int iRowSAVE, iSecSAVE;
  int iADCValue, iADCPlus, iADCMinus;
  int iADCTimePlus, iADCTimeMinus;
  int iSequence, iBin;
  int ChargeSum, PadSum;
  float TimeSum;
  int iUseGauss;
  int iPeakIndex, iInnerIndex;
  int iNumUnfoldLoops;
  int BadFit, PeakShifted, FailedFit;
  int PadtransPerTimebin, PadtransBin;
  float SumDeltaPos, SumDeltaAmp;
  float NewTimePosition, NewPadPosition;
  float NewPeakHeight, PeakHeightSum;
  float fDeltaADC, fDeltaADCPlus, fDeltaADCMinus;
  float fDeltaADCTimePlus, fDeltaADCTimeMinus;
  float fDriftLength, fRadError, fPhiError;

  PadtransPerTimebin=(int) mParam->numberOfDriftSteps()
    / mDb->numberOfTimebins();

  if(iNumPeaks == 0)
    {
      if (DebugOn) {
         LOG_DEBUG<<"Cluster starting "<<Cluster->StartPad<<", "<<Cluster->Sequence->startTimeBin<<" has no peak!"<<endm;
      }
      return FALSE;
    }

  /* sum up cluster charge */
  ChargeSum=0;
  PadSum=0;
  TimeSum=0;
  iADCValue=0;
  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
    {
      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
	{
	  iADCValue = Cluster->Sequence[iSequence].FirstAdc[iBin];
	  ChargeSum += iADCValue;
	  PadSum += Cluster->SequencePad[iSequence] * iADCValue;
	  TimeSum += (Cluster->Sequence[iSequence].startTimeBin + iBin + 
		      mDb->timeOffset(iSec*mDb->numberOfPads()
				      +Cluster->SequencePad[iSequence],iRow)) 
	    * iADCValue;
	}
    }

  if(iNumPeaks == 1)
    {  
      Peak->PeakHeight = 
	Peak->Sequence.FirstAdc[Peak->Timebin - Peak->Sequence.startTimeBin];  

      /* one-peak cluster, fit according to preference from det */
      iUseGauss=mParam->gaussFittingFlags();


      /* calculate pad position first */
      if((iUseGauss & 1) == 1)
	{
	  /* get values for gaussfit */
	  iADCValue = (int) Peak->PeakHeight;
	  iADCPlus=0;
	  iADCMinus=0;
	  /* find sequences with neighbor pixels */ 
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      if((Cluster->SequencePad[iSequence] == Peak->pad - 1) && 
		 (Cluster->Sequence[iSequence].startTimeBin <= Peak->Timebin) && 
		 (Cluster->Sequence[iSequence].startTimeBin + 
		  Cluster->Sequence[iSequence].Length > Peak->Timebin))
		{
		  iADCMinus = Cluster->Sequence[iSequence]
		    .FirstAdc[Peak->Timebin 
			     - Cluster->Sequence[iSequence].startTimeBin];
		}
	      if((Cluster->SequencePad[iSequence] == Peak->pad + 1) && 
		 (Cluster->Sequence[iSequence].startTimeBin < Peak->Timebin) && 
		 (Cluster->Sequence[iSequence].startTimeBin + 
		  Cluster->Sequence[iSequence].Length > Peak->Timebin))
		{
		  iADCPlus = Cluster->Sequence[iSequence]
		    .FirstAdc[Peak->Timebin 
			     - Cluster->Sequence[iSequence].startTimeBin];
		}
	    }
	  
	  /* check if Gaussfit is possible */
	  if((iADCValue == 0) || (iADCPlus == 0) || (iADCMinus == 0) || 
	     ((iADCValue <= iADCPlus) && (iADCValue <= iADCMinus)))
	    {
	      /* use weighted mean instead */
	      iUseGauss = iUseGauss & 2;
	    }
	  else
	    {
	      /* do gaussfit */
	      Peak->PadSigma = sqrt (1 / 
			       ((2 * fastlog[iADCValue]) -
				    (fastlog[iADCPlus] + fastlog[iADCMinus])));
	      Peak->PadPosition = (float) Peak->pad + 
		sqr(Peak->PadSigma) * (fastlog[iADCPlus] - fastlog[iADCMinus]);
	    }
	} 
      
      if((iUseGauss & 1) == 0)
	{
	  /* calculate pad position by weighted mean */ 
	  Peak->PadPosition = (float) PadSum / (float) ChargeSum;
	  
	  /* calculate pad direction cluster width by weighted mean */
	  Peak->PadSigma=0;
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
		{
		  Peak->PadSigma+=(sqr((float) Cluster->SequencePad[iSequence] 
				       - Peak->PadPosition)
				   *Cluster->Sequence[iSequence].FirstAdc[iBin]);
		}
	    }
	  Peak->PadSigma /= (float) ChargeSum;
	}

      /* now calculate the time position */
      if((iUseGauss & 2) > 0)
	{
	  /* get values for gaussfit */
	  iADCValue = (int) Peak->PeakHeight;
	  iADCPlus=0;
	  iADCMinus=0;

	  if(Peak->Timebin > Peak->Sequence.startTimeBin)
	    {
	      iADCMinus = Peak->Sequence.FirstAdc[Peak->Timebin - 
						 Peak->Sequence.startTimeBin - 1];
	    }

	  if(Peak->Timebin + 1 < 
	     Peak->Sequence.startTimeBin + Peak->Sequence.Length)
	    {
	      iADCPlus = Peak->Sequence.FirstAdc[Peak->Timebin - 
						 Peak->Sequence.startTimeBin + 1];
	    }
	  
	  /* check if Gaussfit will fail */
	  if((iADCValue == 0) || (iADCPlus == 0) || (iADCMinus == 0) || 
	     ((iADCValue <= iADCPlus) && (iADCValue <= iADCMinus)))
	    {
	      /* use weighted mean instead */
	      iUseGauss = 0;
	    }
	  else
	    {
	      /* do gaussfit */
	      Peak->TimeSigma = sqrt (1 / 
				((2 * fastlog[iADCValue]) -
				 (fastlog[iADCPlus] + fastlog[iADCMinus])));
	      Peak->TimePosition = (float) Peak->Timebin + 
		mDb->timeOffset(iSec*mDb->numberOfPads()+Peak->pad,iRow) +
		sqr(Peak->TimeSigma) * (fastlog[iADCPlus] - fastlog[iADCMinus]);
	    }
	}

      if((iUseGauss & 2) == 0)
	{
	  /* calculate time position by weighted mean */ 
	  Peak->TimePosition = (float) TimeSum / (float) ChargeSum;
	  
	  /* calculate pad direction cluster width by weighted mean */
	  Peak->TimeSigma=0;
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
		{
		  Peak->TimeSigma+=(sqr((float) 
					Cluster->Sequence[iSequence].startTimeBin 
					+ (float) iBin - Peak->TimePosition)
				    *Cluster->Sequence[iSequence].FirstAdc[iBin]);
		}
	    }
	  Peak->TimeSigma /= (float) ChargeSum;
	}

      /* transform from pad/time to x/y/z */
      if(padtrans(Peak, iRow, iSec, 
		   pRadius, pDeflection))
      {
      if (Peak->x == 0. && Peak->y == 0.) {
	// This if-statement can be deleted as soon as the slow simulator is fixed. This also occurs for FTPC DAQ data.
	LOG_WARN << "Hit rejected because of an error in the FTPC data. (x, y, z) = (0. ,0., z)" << endm;
      }

/*
      if(!std::isnan(Peak->x) && !std::isnan(Peak->y) && !std::isnan(Peak->PadSigma) &&
!std::isnan(Peak->TimeSigma) // && Peak->PeakHeight>=mParam->minimumClusterMaxADC())
	 && Peak->Rad <= mDb->sensitiveVolumeOuterRadius() && Peak->Rad >= mDb->sensitiveVolumeInnerRadius() )
*/
      if(!std::isnan(Peak->x) && !std::isnan(Peak->y) && !std::isnan(Peak->PadSigma) && !std::isnan(Peak->TimeSigma))
	//&& (Cluster->EndPad +1 - Cluster->StartPad)<=MAXPADLENGTH 
	//&& Peak->Sequence.Length<=MAXTIMELENGTH )

	{
	  // create new point

	  // fill QA histograms - unless BFC option "noHists" selected
	  if (mhpad)  mhpad->Fill(Cluster->EndPad +1 - Cluster->StartPad,1);
	  if (mhtime) mhtime->Fill(Peak->Sequence.Length,1);

	  Int_t numPoint = mPoint->GetEntriesFast();
	  if (numPoint >= mPoint->GetSize()) mPoint->Expand(mPoint->GetSize()+5000);

	  mPoint->AddAt(new StFtpcConfMapPoint(), numPoint);
	  StFtpcConfMapPoint *thispoint = (StFtpcConfMapPoint *) mPoint->At(numPoint);


          if (iRow>=10 && mDb->SwapRDO6RDO7East()) {
             iSecSAVE = iSec;
             iRowSAVE = iRow;
             if(iSec==3&&iRow==10){
                iRow = 16;
             }  
             else if(iSec==3&&iRow==11){
                iRow = 17;
             }  
             else if(iSec==3&&iRow==12){
                iRow = 18;
             } 
             else if(iSec==3&&iRow==13){
                iRow = 19;
             }
             else if(iSec==3&&iRow==14){
                iRow = 18;
                iSec = 4;
             }    
             else if(iSec==3&&iRow==15){
                iRow = 19;
                iSec = 4;
             }
             else if(iSec==3&&iRow==16){
                iRow = 10;
             }
             else if(iSec==3&&iRow==17) {
                iRow = 11;
             }
             else if(iSec==3&&iRow==18){
                iRow = 12;
             }
             else if(iSec==3&&iRow==19) {
                iRow = 13;
             }
             else if(iSec==4&&iRow==18){
                iRow = 14;
                iSec = 3;
             }
             else if(iSec==4&&iRow==19){
                iRow = 15;
                iSec = 3;
             }
             thispoint->SetPadRow(iRow+1);
	     thispoint->SetSector(iSec+1);
             iSec = iSecSAVE;
             iRow = iRowSAVE;
          }
          else {
             thispoint->SetPadRow(iRow+1);
	     thispoint->SetSector(iSec+1);
          }

	  thispoint->SetNumberPads(Cluster->EndPad +1 - Cluster->StartPad);
	  thispoint->SetNumberBins(Peak->Sequence.Length);
	  thispoint->SetMaxADC((long)Peak->PeakHeight);
          thispoint->SetCharge(ChargeSum);
	  thispoint->SetPadPos(Peak->PadPosition);
	  thispoint->SetTimePos(Peak->TimePosition);
	  thispoint->SetPadPosSigma(Peak->PadSigma);
	  thispoint->SetTimePosSigma(Peak->TimeSigma);
	  thispoint->SetX(Peak->x);
	  thispoint->SetY(Peak->y);
	  thispoint->SetZ(Peak->z);
	  thispoint->SetSigmaPhi(Peak->PadSigma*mDb->radiansPerPad());
	  thispoint->SetSigmaR(Peak->TimeSigma*Peak->Rad/Peak->TimePosition);
	  fDriftLength = mDb->sensitiveVolumeOuterRadius() - Peak->Rad;
          fPhiError = mParam->padDiffusionErrors(0) 
	    + fDriftLength*mParam->padDiffusionErrors(1) 
	    + fDriftLength*fDriftLength*mParam->padDiffusionErrors(2);
          fRadError = mParam->timeDiffusionErrors(0)
	      + fDriftLength*mParam->timeDiffusionErrors(1) 
	      + fDriftLength*fDriftLength*mParam->timeDiffusionErrors(2);
	  if(thispoint->GetNumberPads()==2)
	    {
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->twoPadWeightedError()));
	    }
	  if((thispoint->GetNumberPads()==3) && ((iUseGauss & 1) == 1))
	    {
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->threePadGaussError()));
	    }
	  if((thispoint->GetNumberPads()==3) && ((iUseGauss & 1) == 0))
	    {
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->threePadWeightedError()));
	    }

	  thispoint->SetFlags(0);

	  if(iADCValue>254)
	    {
	      thispoint->SetFlags(4);
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->padSaturatedClusterError()));
	      fRadError = ::sqrt(fRadError * fRadError
			       + sqr(mParam->timeSaturatedClusterError()));
	    }	  
	  if(Cluster->CutOff==1)
	    {
	      thispoint->SetFlags(thispoint->GetFlags() | 16);
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->padCutoffClusterError()));
	      fRadError = ::sqrt(fRadError * fRadError
			       + sqr(mParam->timeCutoffClusterError()));
	    }
	  
	  if (Peak->Rad > mDb->sensitiveVolumeOuterRadius() ||
	      Peak->Rad < mDb->sensitiveVolumeInnerRadius()
	      /* please add additional criteria here*/) {
	    // don't use this point for tracking
	    
	    thispoint->SetFlags(thispoint->GetFlags() | 128);
	  }

	  /* transform errors to actual hit position */ 
	  PadtransBin=(int) ((Peak->TimePosition+0.5)*PadtransPerTimebin);
	  fPhiError *= Peak->Rad / mDb->sensitiveVolumeOuterRadius();
	  fRadError *= (pRadius[10*PadtransBin]-pRadius[10*PadtransBin+10])
	    / (pRadius[10]-pRadius[20]);

if (mcldebug){
          mcldebug->fillclustertree(Peak,Cluster,ChargeSum,iHardSec,iHardRow,fRadError,fPhiError,0,0,iNumPeaks);
}

	  thispoint->SetXerr(::sqrt(fRadError*cos(Peak->Phi)
				  *fRadError*cos(Peak->Phi) 
				  + fPhiError*sin(Peak->Phi)
				  *fPhiError*sin(Peak->Phi)));
	  thispoint->SetYerr(::sqrt(fRadError*sin(Peak->Phi)
				  *fRadError*sin(Peak->Phi) 
				  + fPhiError*cos(Peak->Phi)
				  *fPhiError*cos(Peak->Phi)));
	  thispoint->SetZerr(mParam->zDirectionError());

	}
      else
	{
          if (DebugOn) {
	     LOG_DEBUG<<"Cluster fitting error. Point not stored"<<endm;
          }
	}
     }
	else{
          if (DebugOn) {
	     LOG_DEBUG<<"Peak position can't be transformed"<<endm;
          } 
	}
    } /* end of: if(iNumPeaks == 1) */
  else
    {
      /* multi-peak cluster, unfold */
      /* initialize all peaks for unfolding */
      BadFit=0;
      FailedFit=0;
      for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	{
	  Peak[iPeakIndex].TimePosition = (float) Peak[iPeakIndex].Timebin;
	  Peak[iPeakIndex].Timebin_saved = Peak[iPeakIndex].Timebin;
	  Peak[iPeakIndex].PadPosition = (float) Peak[iPeakIndex].pad;
	  Peak[iPeakIndex].pad_saved = Peak[iPeakIndex].pad;
	  Peak[iPeakIndex].OldTimePosition = 0;
	  Peak[iPeakIndex].OldPadPosition = 0;
	  Peak[iPeakIndex].OldPeakHeight = 0;
	  Peak[iPeakIndex].TimeSigma = sigmat((float) Peak[iPeakIndex].Timebin);
	  Peak[iPeakIndex].PadSigma = sigmax((float) Peak[iPeakIndex].Timebin);
	  Peak[iPeakIndex].PeakHeight = 
	    (float)Peak[iPeakIndex].Sequence
	    .FirstAdc[Peak[iPeakIndex].Timebin 
		     - Peak[iPeakIndex].Sequence.startTimeBin];  
	}

      /* approximation loop */
      iNumUnfoldLoops=0;
      do 
      { /* beginning of: while((SumDeltaPos > 0.01... */
	SumDeltaPos=0;
	SumDeltaAmp=0;
	PeakHeightSum=0;

	/* loop over all peaks in cluster */ 
	for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	  {

	    do
	      {

		PeakShifted=0;
		/* get values for pad direction gaussfit */
		iADCValue=0;
		iADCPlus=0;
		iADCMinus=0;
		iADCTimePlus=0;
		iADCTimeMinus=0;
		/* find sequences on the right pads */ 
		for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		  {
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad) && 
		       (Cluster->Sequence[iSequence].startTimeBin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].startTimeBin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCValue = Cluster->Sequence[iSequence]
			  .FirstAdc[Peak[iPeakIndex].Timebin
				   - Cluster->Sequence[iSequence].startTimeBin];
			/* get values for time direction gaussfit */ 
			if(Peak[iPeakIndex].Timebin > 
			   Cluster->Sequence[iSequence].startTimeBin)
			  {
			    iADCTimeMinus = Cluster->Sequence[iSequence]
			      .FirstAdc[Peak[iPeakIndex].Timebin 
				       - Cluster->Sequence[iSequence].startTimeBin - 1];
			  }
			
			if(Peak[iPeakIndex].Timebin + 1 < 
			   Cluster->Sequence[iSequence].startTimeBin 
			   + Cluster->Sequence[iSequence].Length)
			  {
			    iADCTimePlus = Cluster->Sequence[iSequence]
			      .FirstAdc[Peak[iPeakIndex].Timebin 
				       - Cluster->Sequence[iSequence].startTimeBin + 1];
			  }
			
		      }
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad -1) && 
		       (Cluster->Sequence[iSequence].startTimeBin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].startTimeBin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCMinus = Cluster->Sequence[iSequence]
			  .FirstAdc[Peak[iPeakIndex].Timebin
				   - Cluster->Sequence[iSequence].startTimeBin];
		      }
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad + 1) && 
		       (Cluster->Sequence[iSequence].startTimeBin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].startTimeBin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCPlus = Cluster->Sequence[iSequence]
			  .FirstAdc[Peak[iPeakIndex].Timebin
				   - Cluster->Sequence[iSequence].startTimeBin];
		      }
		  }
		
		/* calculate influence of other peaks */ 
		fDeltaADC=0;

		fDeltaADCPlus=0;
		fDeltaADCMinus=0;
		fDeltaADCTimePlus=0; 
		fDeltaADCTimeMinus=0;
		for(iInnerIndex=0; iInnerIndex < iNumPeaks; iInnerIndex++)
		  {
		    if(iInnerIndex != iPeakIndex)
		      {
			/* check if peaks are overlaid */
			if((Peak[iPeakIndex].pad == Peak[iInnerIndex].pad)
			   && (Peak[iPeakIndex].Timebin == Peak[iInnerIndex].Timebin))
			  {
			    /* one peak has been shifted on top of the other */
			    /* that kills the unfold procedure */ 
			    FailedFit=1;
			    break;
			  }
			
			fDeltaADC+=gauss_2d(Peak[iPeakIndex].pad, 
					    Peak[iPeakIndex].Timebin, 
					    Peak[iInnerIndex].PadPosition, 
					    Peak[iInnerIndex].PadSigma, 
					    Peak[iInnerIndex].TimePosition, 
					    Peak[iInnerIndex].TimeSigma, 
					    Peak[iInnerIndex].PeakHeight);
			fDeltaADCPlus+=gauss_2d(Peak[iPeakIndex].pad+1, 
						Peak[iPeakIndex].Timebin, 
						Peak[iInnerIndex].PadPosition, 
						Peak[iInnerIndex].PadSigma, 
						Peak[iInnerIndex].TimePosition, 
						Peak[iInnerIndex].TimeSigma, 
						Peak[iInnerIndex].PeakHeight);
			fDeltaADCMinus+=gauss_2d(Peak[iPeakIndex].pad-1, 
						 Peak[iPeakIndex].Timebin, 
						 Peak[iInnerIndex].PadPosition, 
						 Peak[iInnerIndex].PadSigma, 
						 Peak[iInnerIndex].TimePosition, 
						 Peak[iInnerIndex].TimeSigma, 
						 Peak[iInnerIndex].PeakHeight);
			fDeltaADCTimePlus+=gauss_2d(Peak[iPeakIndex].pad, 
						    Peak[iPeakIndex].Timebin+1, 
						    Peak[iInnerIndex].PadPosition, 
						    Peak[iInnerIndex].PadSigma, 
						    Peak[iInnerIndex].TimePosition, 
						    Peak[iInnerIndex].TimeSigma, 
						    Peak[iInnerIndex].PeakHeight);
			fDeltaADCTimeMinus+=gauss_2d(Peak[iPeakIndex].pad, 
						     Peak[iPeakIndex].Timebin-1, 
						     Peak[iInnerIndex].PadPosition, 
						     Peak[iInnerIndex].PadSigma, 
						     Peak[iInnerIndex].TimePosition, 
						     Peak[iInnerIndex].TimeSigma, 
						     Peak[iInnerIndex].PeakHeight);
		      }
		  }
		
		/* subtract influence of other peaks */
		iADCValue-=(int) (fDeltaADC+0.5);
		iADCPlus-=(int) (fDeltaADCPlus+0.5);
		iADCMinus-=(int) (fDeltaADCMinus+0.5);
		iADCTimePlus-=(int) (fDeltaADCTimePlus+0.5);
		iADCTimeMinus-=(int) (fDeltaADCTimeMinus+0.5);
		
		/* check if Gaussfit is possible */
		
		/* introduce minimal gradient if none is there */ 
		if(iADCValue == iADCPlus)
		  {
		    iADCPlus--;
		  }
		if(iADCValue == iADCMinus)
		  {
		    iADCMinus--;
		  }
		if(iADCValue == iADCTimePlus)
		  {
		    iADCTimePlus--;
		  }
		if(iADCValue == iADCTimeMinus)
		  {
		    iADCTimeMinus--;
		  }
		
		
		/* set all 0s to 1 to keep gauss from crashing */
                /* and iADCValue=2 if iADCValue <= 1 (09/28/98) */
		if(iADCValue <= 1)
		  {
		    iADCValue=2;
		    BadFit=1;
		  }
		if(iADCPlus <= 0)
		  {
		    iADCPlus=1;
		    BadFit=1;
		  }
		if(iADCMinus <= 0)
		  {
		    iADCMinus=1;
		    BadFit=1;
		  }
		if(iADCTimePlus <= 0)
		  {
		    iADCTimePlus=1;
		    BadFit=1;
		  }
		if(iADCTimeMinus <= 0)
		  {
		    iADCTimeMinus=1;
		    BadFit=1;
		  }
		
/*    move here to protect against iADCValue=iADCPlus=iADCMinus which
      causes Peak[..].PadSigma=Inf            09.30.98    HH,JCS   */
		if(FailedFit == 1)
		  {
		    break;
		  }
		
		/* shift if peak has moved after correction */ 
		if(iADCValue < iADCPlus)
		  {
		    Peak[iPeakIndex].pad++;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCMinus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].pad--;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCTimePlus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].Timebin++;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCTimeMinus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].Timebin--;
		    PeakShifted=1;
		  }
	      }
	    while(PeakShifted>0 && PeakShifted<5);
	    
	    /* recalculate peak position */
	    NewPeakHeight=iADCValue;
            double qwe;
            qwe = (2 * fastlog[iADCValue]) -(fastlog[iADCPlus] + fastlog[iADCMinus]);
            if (qwe<=0.) continue; 	//skip case for non physical value (VP)
	    Peak[iPeakIndex].PadSigma = sqrt (1 / qwe);
	    NewPadPosition = (float) Peak[iPeakIndex].pad + 
	      sqr(Peak[iPeakIndex].PadSigma) * 
	      (fastlog[iADCPlus] - fastlog[iADCMinus]);
            qwe = (2 * fastlog[iADCValue]) -(fastlog[iADCTimePlus] + fastlog[iADCTimeMinus]);
            if (qwe<=0.) continue;	//skip case for non physical value (VP)
	    Peak[iPeakIndex].TimeSigma = sqrt (1 / qwe);

	    NewTimePosition = (float) Peak[iPeakIndex].Timebin + 
	      mDb->timeOffset(iSec*mDb->numberOfPads()
			      +Peak[iPeakIndex].pad,iRow) +
	      sqr(Peak[iPeakIndex].TimeSigma) * 
	      (fastlog[iADCTimePlus] - fastlog[iADCTimeMinus]);
	    
	    /* introduce inertia to break infinite loops */ 
 	    if(iNumUnfoldLoops > MAXFASTLOOPS)
 	      { 
		NewPeakHeight=(NewPeakHeight+Peak[iPeakIndex].PeakHeight)/2;
		NewPadPosition=(NewPadPosition+Peak[iPeakIndex].PadPosition)/2;
		NewTimePosition=(NewTimePosition+Peak[iPeakIndex].TimePosition)/2;
 	      } 

	    SumDeltaAmp+=fabs(NewPeakHeight - Peak[iPeakIndex].PeakHeight);
	    Peak[iPeakIndex].OldPeakHeight=Peak[iPeakIndex].PeakHeight;
	    Peak[iPeakIndex].PeakHeight=NewPeakHeight;
	    SumDeltaPos+=fabs(NewPadPosition - Peak[iPeakIndex].PadPosition);
	    Peak[iPeakIndex].OldPadPosition=Peak[iPeakIndex].PadPosition;
	    Peak[iPeakIndex].PadPosition=NewPadPosition;
	    SumDeltaPos+=fabs(NewTimePosition - Peak[iPeakIndex].TimePosition);
	    Peak[iPeakIndex].OldTimePosition=Peak[iPeakIndex].TimePosition;
	    Peak[iPeakIndex].TimePosition=NewTimePosition;
	    
	    PeakHeightSum+=NewPeakHeight;

	  } /* end of: for(iPeakIndex=0;... */ 
	iNumUnfoldLoops++;
      }
      while((SumDeltaPos > UNFOLDLIMIT || SumDeltaAmp > 1) 
	    && (iNumUnfoldLoops < MAXLOOPS || SumDeltaPos > UNFOLDFAILEDLIMIT)
	    && iNumUnfoldLoops < MAXLOOPS+1 && FailedFit == 0);
      /* try unfold loop once more if error > failedlimit */
      
      if(SumDeltaPos > UNFOLDFAILEDLIMIT || FailedFit == 1)
	{
	  /* unfolding has failed, use original peak positions */ 

	  for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	    {
	      Peak[iPeakIndex].TimePosition = (float) Peak[iPeakIndex].Timebin_saved;
	      Peak[iPeakIndex].Timebin = Peak[iPeakIndex].Timebin_saved;
	      Peak[iPeakIndex].PadPosition = (float) Peak[iPeakIndex].pad_saved;
	      Peak[iPeakIndex].pad = Peak[iPeakIndex].pad_saved;
		for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		  {
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad) && 
		       (Cluster->Sequence[iSequence].startTimeBin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].startTimeBin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			Peak[iPeakIndex].PeakHeight = Cluster->Sequence[iSequence]
			  .FirstAdc[Peak[iPeakIndex].Timebin
				   - Cluster->Sequence[iSequence].startTimeBin];
		      }
		  }
	    }
          if (DebugOn) {
	     LOG_DEBUG<<"unfold failed!"<<endm;
          }
	} /* end of: if(SumDeltaPos > UNFOLDFAILEDLIMIT ... */

      /* write unfolded peaks to point table */
      for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	{
	  /* transform from pad/time to x/y/z */
	  if(padtrans(&(Peak[iPeakIndex]), iRow, iSec, 
		       pRadius, pDeflection))
          {
	  if (Peak[iPeakIndex].x == 0. && Peak[iPeakIndex].y == 0.) {
	    // This if-statement can be deleted as soon as the slow simulator is fixed. This also occurs for FTPC DAQ data.
	    LOG_WARN << "Hit rejected because of an error in the FTPC data. (x, y, z) = (0. ,0., z)" << endm;
	  }
	  
	  /* in very complicated clusters some hits may have been unfolded
	     with errors while the rest of the cluster is okay, don't fill 
	     these hits into array: */
/*
	  if(!std::isnan(Peak[iPeakIndex].x) && !std::isnan(Peak[iPeakIndex].y) &&
!std::isnan(Peak[iPeakIndex].PadSigma) && !std::isnan(Peak[iPeakIndex].TimeSigma) // && Peak[iPeakIndex].PeakHeight>=mParam->minimumClusterMaxADC())
	     && Peak[iPeakIndex].Rad <= mDb->sensitiveVolumeOuterRadius() 
             && Peak[iPeakIndex].Rad >= mDb->sensitiveVolumeInnerRadius() )
*/
          if(!std::isnan(Peak[iPeakIndex].x) && !std::isnan(Peak[iPeakIndex].y) 
	     &&	!std::isnan(Peak[iPeakIndex].PadSigma) && !std::isnan(Peak[iPeakIndex].TimeSigma))
	    //&& (Cluster->EndPad +1 - Cluster->StartPad)<=MAXPADLENGTH 
	    //&& Peak[iPeakIndex].Sequence.Length<=MAXTIMELENGTH)
	    {

	      // fill QA histograms - unless BFC option "noHists" selected
	      if (mhpad)  mhpad->Fill(Cluster->EndPad +1 - Cluster->StartPad,iNumPeaks);
	      if (mhtime) mhtime->Fill(Peak[iPeakIndex].Sequence.Length,iNumPeaks);

	      // create new point
	      Int_t numPoint = mPoint->GetEntriesFast();
	      if (numPoint >= mPoint->GetSize()) mPoint->Expand(mPoint->GetSize()+5000);
	      
	      mPoint->AddAt(new StFtpcConfMapPoint(), numPoint);
	      StFtpcConfMapPoint *thispoint = (StFtpcConfMapPoint *) mPoint->At(numPoint);

              if (iRow>=10 && mDb->SwapRDO6RDO7East()) {
                 iSecSAVE = iSec;
                 iRowSAVE = iRow;
                 if(iSec==3&&iRow==10){
                    iRow = 16;
                 } 
                 else if(iSec==3&&iRow==11){
                    iRow = 17;
                 }
                 else if(iSec==3&&iRow==12){
                    iRow = 18;
                 }
                 else if(iSec==3&&iRow==13){
                    iRow = 19;
                 }
                 else if(iSec==3&&iRow==14){
                    iRow = 18;
                    iSec = 4;
                 }
                 else if(iSec==3&&iRow==15){
                    iRow = 19;
                    iSec = 4;
                 }
                 else if(iSec==3&&iRow==16){
                    iRow = 10;
                 }
                 else if(iSec==3&&iRow==17){
                    iRow = 11;
                 }
                 else if(iSec==3&&iRow==18){
                    iRow = 12;
                 }
                 else if(iSec==3&&iRow==19){
                    iRow = 13;
                }
                else if(iSec==4&&iRow==18){
                    iRow = 14;
                    iSec = 3;
                }
                else if(iSec==4&&iRow==19){
                    iRow = 15;
                    iSec = 3;
                }
                thispoint->SetPadRow(iRow+1);
	        thispoint->SetSector(iSec+1);
                iSec = iSecSAVE;
                iRow = iRowSAVE;
              }
              else {
                 thispoint->SetPadRow(iRow+1);
         	 thispoint->SetSector(iSec+1);
              }

	      thispoint->SetNumberPads(Cluster->EndPad +1 - Cluster->StartPad);
	      thispoint->SetNumberBins(Peak[iPeakIndex].Sequence.Length);
	      thispoint->SetMaxADC((long)Peak[iPeakIndex].PeakHeight);
	      thispoint->SetCharge((Long_t)(ChargeSum*(Peak[iPeakIndex].PeakHeight
				   /PeakHeightSum)));
	      thispoint->SetPadPos(Peak[iPeakIndex].PadPosition);
	      thispoint->SetTimePos(Peak[iPeakIndex].TimePosition);
	      thispoint->SetPadPosSigma(Peak[iPeakIndex].PadSigma);
	      thispoint->SetTimePosSigma(Peak[iPeakIndex].TimeSigma);
	      thispoint->SetX(Peak[iPeakIndex].x);
	      thispoint->SetY(Peak[iPeakIndex].y);
	      thispoint->SetZ(Peak[iPeakIndex].z);
	      thispoint->SetSigmaPhi(Peak[iPeakIndex].PadSigma
				     *mDb->radiansPerPad());
	      thispoint->SetSigmaR(Peak[iPeakIndex].TimeSigma
				   * Peak[iPeakIndex].Rad
				   /Peak[iPeakIndex].TimePosition);

	      fDriftLength = mDb->sensitiveVolumeOuterRadius() 
		- Peak[iPeakIndex].Rad;
	      fPhiError = mParam->padDiffusionErrors(0) 
		  + fDriftLength*mParam->padDiffusionErrors(1) 
		  + fDriftLength*fDriftLength*mParam->padDiffusionErrors(2);
	      fRadError = mParam->timeDiffusionErrors(0)
		  + fDriftLength*mParam->timeDiffusionErrors(1)
		  + fDriftLength*fDriftLength*mParam->timeDiffusionErrors(2);
	      
	      /* clusters are unfolded */ 
	      thispoint->SetFlags(1);
	      fPhiError = ::sqrt(fPhiError * fPhiError
			       + sqr(mParam->padUnfoldError()));
	      fRadError = ::sqrt(fRadError * fRadError
			       + sqr(mParam->timeUnfoldError()));
	      
	      if(iADCValue>254)
		{
		  thispoint->SetFlags(5);
		  fPhiError = ::sqrt(fPhiError * fPhiError
				   + sqr(mParam->padSaturatedClusterError()));
		  fRadError = ::sqrt(fRadError * fRadError
				   + sqr(mParam->timeSaturatedClusterError()));
		}	  
	      if(BadFit==1)
		{
		  thispoint->SetFlags(thispoint->GetFlags() | 8);
		  fPhiError = ::sqrt(fPhiError * fPhiError
				   + sqr(mParam->padBadFitError()));
		  fRadError = ::sqrt(fRadError * fRadError
				   + sqr(mParam->timeBadFitError()));
		}	  
	      if(iNumUnfoldLoops == MAXLOOPS)
		{
		  thispoint->SetFlags(thispoint->GetFlags() | 10);
		  fPhiError = ::sqrt(fPhiError * fPhiError
				   + sqr(mParam->padFailedFitError()));
		  fRadError = ::sqrt(fRadError * fRadError
				   + sqr(mParam->timeFailedFitError()));
		}	  
	      if(Cluster->CutOff==1)
		{
		  thispoint->SetFlags(thispoint->GetFlags() | 16);
		  fPhiError = ::sqrt(fPhiError * fPhiError
				   + sqr(mParam->padCutoffClusterError()));
		  fRadError = ::sqrt(fRadError * fRadError
				   + sqr(mParam->timeCutoffClusterError()));
		}

	      if (Peak[iPeakIndex].Rad > mDb->sensitiveVolumeOuterRadius() || 
		  Peak[iPeakIndex].Rad < mDb->sensitiveVolumeInnerRadius()
		  /* please add additional criteria here*/) {
		// don't use this point for tracking
		
		thispoint->SetFlags(thispoint->GetFlags() | 128);
	      }
	      
	      /* transform errors to actual hit position */ 
	      PadtransBin=(int) ((Peak->TimePosition+0.5)*PadtransPerTimebin);
	      fPhiError *= Peak->Rad / mDb->sensitiveVolumeOuterRadius();
	      fRadError *= (pRadius[10*PadtransBin]-pRadius[10*PadtransBin+10])
		/ (pRadius[10]-pRadius[20]);

if (mcldebug){
              mcldebug->fillclustertree(Peak[iPeakIndex],Cluster,ChargeSum*Peak[iPeakIndex].PeakHeight/PeakHeightSum,iHardSec,iHardRow,fRadError,fPhiError,10,0,iNumPeaks);
}	      

	      thispoint->SetXerr(::sqrt(fRadError*cos(Peak[iPeakIndex].Phi)
				      *fRadError*cos(Peak[iPeakIndex].Phi) 
				      + fPhiError*sin(Peak[iPeakIndex].Phi)
				      *fPhiError*sin(Peak[iPeakIndex].Phi)));
	      thispoint->SetYerr(::sqrt(fRadError*sin(Peak[iPeakIndex].Phi)
				      *fRadError*sin(Peak[iPeakIndex].Phi) 
				      + fPhiError*cos(Peak[iPeakIndex].Phi)
				      *fPhiError*cos(Peak[iPeakIndex].Phi)));
	      thispoint->SetZerr(mParam->zDirectionError());

	    }
         }
            else
	    {
              if (DebugOn) {
	         LOG_DEBUG<<"Peak position can't be transformed!"<<endm;
              }
	    }
	} /* end of: for(iPeakIndex=0;... */
    } /*end of: if(iNumPeaks==1) ... else { */
  return TRUE;
}

int StFtpcClusterFinder::padtrans(TPeak *Peak, 
				  int iRow, 
				  int iSec, 
				  double *pRadius, 
				  double *pDeflection)
{  
 int PadtransPerTimebin;
  int PadtransLower;
  float PhiDeflect, TimeCoordinate;

  if (iRow>=10 && mDb->SwapRDO6RDO7East()) {
    if(iSec==3&&iRow==10) { 
       iRow=16;
    }
    else if(iSec==3&&iRow==11) {
       iRow=17;
    }
    else if(iSec==3&&iRow==12) {
       iRow=18;
    }
    else if(iSec==3&&iRow==13) {
       iRow=19;
    }
    else if(iSec==3&&iRow==14) {
       iSec=4;
       iRow=18;
    }
    else if(iSec==3&&iRow==15) {
       iSec=4;
       iRow=19;
    }
    else if(iSec==3&&iRow==16) {
       iRow=10;
    }
    else if(iSec==3&&iRow==17) {
       iRow=11;
    }
    else if(iSec==3&&iRow==18) {
       iRow=12;
    }
    else if(iSec==3&&iRow==19) {
       iRow=13;
    }
    else if(iSec==4&&iRow==18) {
       iSec=3;
       iRow=14;
    } 
    else if(iSec==4&&iRow==19) {
       iSec=3;
       iRow=15;
    }
  }

  /* preparatory calculations */
  TimeCoordinate = Peak->TimePosition + 0.5; /*time start at beginning of bin 0*/
  // include tZero = time from collision to beginning of bin 0
  TimeCoordinate += mDb->tZero()/mDb->microsecondsPerTimebin();
  PadtransPerTimebin = (int) mParam->numberOfDriftSteps() / mDb->numberOfTimebins();
  PadtransLower= (int) (TimeCoordinate*PadtransPerTimebin);

  if ( TimeCoordinate > mDb->numberOfTimebins() || TimeCoordinate <= 0 ) 
    {
      // exceeds table dimensions
      return FALSE;
    }

  /* linear interpolation in radius table */
  Peak->Rad=pRadius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
    -(pRadius[iRow + mDb->numberOfPadrowsPerSide()*(PadtransLower)]
      -pRadius[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)])/2;

  if ( pRadius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower] == 0 )
    {
      // outside FTPC sensitive region
      return FALSE;
    }
  
   /* linear interpolation in deflection table */
  PhiDeflect=pDeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
    +(pDeflection[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)]
      -pDeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower])/2;

    
  /* calculate phi angle from pad position */
    // for FTPC West
  if (iRow <10) {
    Peak->Phi = mDb->radiansPerBoundary() / 2 
      + ((Peak->PadPosition-1) + 0.5) * mDb->radiansPerPad()
      + PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
       	    + mDb->radiansPerBoundary())+halfpi;
  }
    // for FTPC East
  /* Invert pad number (== Peak->PadPosition) for FTPC East  */
  /* (not yet understood where and why pad numbers were inverted) */
  if (iRow >= 10) {
    Peak->Phi = mDb->radiansPerBoundary() / 2 
      + (159.5 - (Peak->PadPosition-1))* mDb->radiansPerPad()
      - PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
	    + mDb->radiansPerBoundary())+halfpi;
  } 

  // ===================================================================
  
  // shift time => radius to correct for the offset of the inner cathode :

  if (fabs(mOffsetCathodeWest)>0 || fabs(mOffsetCathodeEast)>0)
    {

      if (iRow<10) // correct for west chamber
	TimeCoordinate=(0.999997-0.09739494018294076*mOffsetCathodeWest*cos(Peak->Phi-mAngleOffsetWest))*TimeCoordinate;
	//TimeCoordinate=(1.0-0.09739494018294076*mOffsetCathodeWest*cos(Peak->Phi))*TimeCoordinate;
      else // correct for east chamber
	TimeCoordinate=(0.999997-0.09739494018294076*mOffsetCathodeEast*sin(Peak->Phi+mAngleOffsetEast))*TimeCoordinate;
	//TimeCoordinate=(1.0-0.09739494018294076*mOffsetCathodeEast*sin(Peak->Phi))*TimeCoordinate;
      
      // calculate new corrected radius and deflection angle :
      
      //Peak->TimePosition=TimeCoordinate; // DEBUG only !
      
      PadtransLower= (int) (TimeCoordinate*PadtransPerTimebin);
      
      if ( TimeCoordinate > mDb->numberOfTimebins() || TimeCoordinate <= 0 ) 
	{
	  // exceeds table dimensions
	  return FALSE;
	}
      
      /* linear interpolation in radius table */
      
      Peak->Rad=pRadius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
	-(pRadius[iRow + mDb->numberOfPadrowsPerSide()*(PadtransLower)]
	  -pRadius[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)])/2;
      
      if ( pRadius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower] == 0 )
	{
	  // outside FTPC sensitive region
	  return FALSE;
	}
      
      // calculate new corrected deflection angle and phi 
      
      /* linear interpolation in deflection table */
      PhiDeflect=pDeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
	+(pDeflection[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)]
	  -pDeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower])/2;
      
      
      /* calculate phi angle from pad position */
      // for FTPC West
      if (iRow < 10) {
        Peak->Phi = mDb->radiansPerBoundary() / 2 
	  + ((Peak->PadPosition-1) + 0.5) * mDb->radiansPerPad()
	  + PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
		+ mDb->radiansPerBoundary())+halfpi;
      }
      // for FTPC East
      /* Invert pad number (== Peak->PadPosition) for FTPC East  */
      /* (not yet understood where and why pad numbers were inverted) */
      if (iRow >= 10) {
        Peak->Phi = mDb->radiansPerBoundary() / 2 
          + (159.5 - (Peak->PadPosition-1))* mDb->radiansPerPad()
          - PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
		+ mDb->radiansPerBoundary())+halfpi;
      }
    }

  // ===================================================================
    
  /* transform to cartesian */
  Peak->x = Peak->Rad*cos(Peak->Phi);
  /* Peak->x = -Peak->x to transform FTPC West into STAR global coordinate system */
  if (iRow <10) {
    Peak->x = -Peak->x;
  }
  Peak->y = Peak->Rad*sin(Peak->Phi);
  Peak->z = mDb->padrowZPosition(iRow);
  return TRUE;
}

float StFtpcClusterFinder::gauss_2d(int x, int y, float x1, float sigma_x1, float y1, float sigma_y1, float amp1)
{
  float result;
  float g1, g2;
  
  if(sigma_x1==0 || sigma_y1==0)
    {
      return 0.0;
    }

  g1=exp(-sqr((float)x-x1)/(2*sqr(sigma_x1)));
  g2=exp(-sqr((float)y-y1)/(2*sqr(sigma_y1)));
  
  result=amp1*g1*g2;
  
  return result;
}

float StFtpcClusterFinder::sigmax(float timebin)
{
  return 0.9;
}

float StFtpcClusterFinder::sigmat(float timebin)
{
  return 0.6;
}

int StFtpcClusterFinder::calcpadtrans(double *pradius, 
				      double *pdeflection, double deltap)
{
  int i, j, v_buf, padrow;
  double t_last, t_next, r_last, r_next, e_now, v_now, psi_now;
  double step_size;
  
  step_size=((float) mDb->numberOfTimebins()
	     / (float) mParam->numberOfDriftSteps());

  LOG_DEBUG <<"StFtpcClusterFinder::calcpadtrans deltap = "<<deltap<<endm;
  
  for (padrow=0; padrow<mDb->numberOfPadrowsPerSide(); padrow++)
    {
      /* determine starting values */
      t_last=0;
      v_buf=0;
      r_last=mDb->sensitiveVolumeOuterRadius();
      pradius[padrow]=mDb->sensitiveVolumeOuterRadius();
      pdeflection[padrow]=0;
      e_now = mDb->radiusTimesField() / (0.5*r_last);
      for(j=v_buf; j<mDb->numberOfMagboltzBins()-1
	    && mDb->magboltzEField(j) < e_now; j++);
      if(j<1 || j>mDb->numberOfMagboltzBins())
	{
	  LOG_ERROR << "calcpadtrans error 1: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	  return FALSE;
	}
      v_buf=j-1;
      v_now=((mDb->magboltzVDrift(v_buf, padrow)
	      +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
	     *(mDb->magboltzEField(j)-e_now)
	     +(mDb->magboltzVDrift(j, padrow)
	       +deltap*mDb->magboltzdVDriftdP(j, padrow))
	     *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
      psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		+deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
	       *mParam->lorentzAngleFactor()
	       *(mDb->magboltzEField(j)-e_now)
	       +(mDb->magboltzDeflection(j,padrow)
		 +deltap*mDb->magboltzdDeflectiondP(j,padrow))
	       *mParam->lorentzAngleFactor()
	       *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
      for (i=0; i<mParam->numberOfDriftSteps()-1 
	     && e_now < mDb->magboltzEField(mDb->numberOfMagboltzBins()-2)
	     ; i++) 
	{
	  t_next = t_last + step_size;
	  /* first guess for r_next: */
	  r_next = r_last - v_now * step_size * mDb->microsecondsPerTimebin();
	  e_now = mDb->radiusTimesField() / (0.5*(r_last+r_next));
	  
	  for(j=v_buf; j<mDb->numberOfMagboltzBins()-1
		       && mDb->magboltzEField(j) < e_now; j++);
	  
	  if(j<1 || j>mDb->numberOfMagboltzBins())
	    {
	      LOG_ERROR << "calcpadtrans error 2: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	      return FALSE;
	    }
	  
	  v_buf=j-1;
	  v_now=((mDb->magboltzVDrift(v_buf, padrow)
		  +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
		 *(mDb->magboltzEField(j)-e_now)
		 +(mDb->magboltzVDrift(j, padrow)
		   +deltap*mDb->magboltzdVDriftdP(j, padrow))
		 *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	  psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		    +deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
		   *mParam->lorentzAngleFactor()
		   *(mDb->magboltzEField(j)-e_now)
		   +(mDb->magboltzDeflection(j,padrow)
		     +deltap*mDb->magboltzdDeflectiondP(j,padrow))
		   *mParam->lorentzAngleFactor()
		   *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	  
	  /* correct r_next: */
	  r_next = r_last - v_now * step_size *mDb->microsecondsPerTimebin();
	  pradius[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]=r_next;
	  pdeflection[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]
	    =pdeflection[padrow+mDb->numberOfPadrowsPerSide()*i]
	    +((r_last-r_next)*tan(degree * psi_now)/r_last);
	  t_last=t_next;
	  r_last=r_next;
	}
      if (DebugOn) {
         LOG_DEBUG<<i<<" steps calculated, padrow "<<padrow<<endm;
      }
    }
  return TRUE;
}

int StFtpcClusterFinder::cucInit(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer)
{
  int i;
  
  for(i=0; i<MAXNUMCUC; i++)
    {
      RealMemory[i]=i;
    }
  *pointer = -1;
  return TRUE;
}

TClusterUC *StFtpcClusterFinder::cucAlloc(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer)
{

  if((*pointer)+1 < MAXNUMCUC)
    {
      (*pointer)++;
      memory[RealMemory[*pointer]].MemoryPtr = RealMemory[*pointer];
      return &memory[RealMemory[*pointer]];
    }
  else
    {
      if (DebugOn) {
         LOG_DEBUG<<"CUC memory exhausted! requested "<<*pointer<<" CUCs"<<endm;
      }
      return 0;
    }
}

int StFtpcClusterFinder::cucFree(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer, TClusterUC *OldCUC)
{
  if(*pointer >= 0)
    {
      RealMemory[*pointer] = OldCUC->MemoryPtr;
      (*pointer)--;
      return TRUE;
    }
  else
    {
      if (DebugOn) {
         LOG_DEBUG<<"CUC memory management confused!"<<endm;
      }
      return FALSE;
    }
}
