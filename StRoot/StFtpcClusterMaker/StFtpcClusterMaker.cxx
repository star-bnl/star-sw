/***************************************************************************
 *
 * $Id: StFtpcClusterMaker.cxx,v 1.1 1999/11/02 09:41:28 jcs Exp $
 *
 * Author:   Holm Huemmler  (hummler@mppmu.mpg.de)
 ***************************************************************************
 *
 * Description:
 * 
 *
 ***************************************************************************
 *
 * $Log: StFtpcClusterMaker.cxx,v $
 * Revision 1.1  1999/11/02 09:41:28  jcs
 * add source files to empty StFtpcClusterMaker
 *
 **************************************************************************/
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcClusterMaker class for Makers                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFtpcClusterMaker.h"
#include "StFtpcClusterFinder.hh"
#include "StFtpcFastSimu.hh"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

#include "tables/St_fcl_fppoint_Table.h"
#include "tables/St_fcl_ftpcndx_Table.h"
#include "tables/St_fcl_ftpcsqndx_Table.h"
#include "tables/St_fcl_ftpcadc_Table.h"

#include "tables/St_fcl_padtrans_Table.h"
#include "tables/St_fcl_ampslope_Table.h"
#include "tables/St_fcl_ampoff_Table.h"
#include "tables/St_fcl_timeoff_Table.h"
#include "tables/St_fcl_det_Table.h"
#include "tables/St_fcl_zrow_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_ffs_gaspar_Table.h"
#include "tables/St_ffs_fspar_Table.h"
#include "tables/St_ffs_gepoint_Table.h"

ClassImp(StFtpcClusterMaker)

  //_____________________________________________________________________________
StFtpcClusterMaker::StFtpcClusterMaker(const char *name):
StMaker(name),
    m_ampoff(0),
    m_ampslope(0),
    m_timeoff(0),
    m_padtrans(0),
    m_det(0),
    m_zrow(0),
    m_fspar(0),
    m_gaspar(0)
{
  drawinit=kFALSE;
}
//_____________________________________________________________________________
StFtpcClusterMaker::~StFtpcClusterMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Init(){

  St_DataSet *ftpc = GetDataBase("params/ftpc");
  assert(ftpc);
  St_DataSetIter       local(ftpc);

  m_ampoff     = (St_fcl_ampoff   *)local("fclpars/ampoff"  );
  m_ampslope   = (St_fcl_ampslope *)local("fclpars/ampslope");
  m_timeoff    = (St_fcl_timeoff  *)local("fclpars/timeoff" );
  m_padtrans   = (St_fcl_padtrans *)local("fclpars/padtrans");
  m_det        = (St_fcl_det      *)local("fclpars/det"     );
  m_zrow       = (St_fcl_zrow     *)local("fclpars/zrow"    );
  m_fspar      = (St_ffs_fspar    *)local("ffspars/fspar"   );
  m_gaspar     = (St_ffs_gaspar   *)local("ffspars/gaspar"  );

  ffs_gaspar_st *ffs_gaspar = m_gaspar->GetTable();
  ffs_gaspar->sig_rad[0] = 800.00;
  ffs_gaspar->sig_rad[1] = 0.00;
  ffs_gaspar->sig_rad[2] = 0.00;
  ffs_gaspar->sig_rad[3] = 0.00;
  ffs_gaspar->sig_azi[0] = 2000.00;
  ffs_gaspar->sig_azi[1] = 0.00;
  ffs_gaspar->sig_azi[2] = 0.00;
  ffs_gaspar->sig_azi[3] = 0.00;

// 		Create Histograms
  m_flags      = new TH1F("fcl_flags"	,"FTPC cluster finder flags"	,7,0.,8.);
  m_row        = new TH1F("fcl_row"	,"FTPC rows"			,20,1.,21.);
  m_sector     = new TH1F("fcl_sector"	,"FTPC sectors"			,6,1.,7.);
  m_pads       = new TH1F("fcl_pads"	,"FTPC pads"			,80,1.,161.);
  m_timebins   = new TH1F("fcl_timebins","FTPC timebins"		,100,1.,257.);
  m_row_sector = new TH2F("fcl_row_sector","FTPC(fcl) row vs. sector"	,20,1.,21.,6,1.,7.);
  m_npad_nbin  = new TH2F("fcl_pad_bin"	,"FTPC(fcl) pad vs. timebin"	,80,1.,161.,100,1.,257.);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Make()
{
  int iMake=kStOK;
  St_DataSet *raw = GetDataSet("ftpc_raw");
    if (raw) {
//			FCL
    St_DataSetIter get(raw);

    St_fcl_ftpcndx   *fcl_ftpcndx   = (St_fcl_ftpcndx*  )get("fcl_ftpcndx");
    St_fcl_ftpcsqndx *fcl_ftpcsqndx = (St_fcl_ftpcsqndx*)get("fcl_ftpcsqndx");
    St_fcl_ftpcadc   *fcl_ftpcadc   = (St_fcl_ftpcadc*  )get("fcl_ftpcadc");
    if (fcl_ftpcndx&&fcl_ftpcsqndx&&fcl_ftpcadc) { 

      St_fcl_fppoint *fcl_fppoint = new St_fcl_fppoint("fcl_fppoint",150000);
      m_DataSet->Add(fcl_fppoint);
      if(Debug()) cout<<"start running fcl"<<endl;







  StFtpcClusterFinder *fcl = new StFtpcClusterFinder();
  // get all tables
  fcl_det_st *det = m_det->GetTable();
  fcl_padtrans_st *padtrans = m_padtrans->GetTable();
  fcl_zrow_st *zrow = m_zrow->GetTable();
  fcl_ampoff_st *ampoff = m_ampoff->GetTable();
  fcl_ampslope_st *ampslope = m_ampslope->GetTable();
  fcl_timeoff_st *timeoff = m_timeoff->GetTable();
  //  fcl_ftpcndx_st *ftpcndx = fcl_ftpcndx->GetTable(); // not used
  fcl_ftpcsqndx_st *ftpcsqndx = fcl_ftpcsqndx->GetTable();
  fcl_ftpcadc_st *ftpcadc = fcl_ftpcadc->GetTable();
  fcl_fppoint_st *fppoint = fcl_fppoint->GetTable();
    
  double *pradius, *pdeflection;
  int iRow, iSec, iPad, iPadBuf;
  int iSecBuf, iRowBuf;
  int clusters;
  int iNowSeqIndex, iNewSeqIndex, iOldSeqNumber, iOldSeqIndex;
  int iADCCounter;
  int iNowPad;
  int iNowSequence, iOldSeqBuf;
  int iCUCSequence, iMoveSequence;
  int bOldSequenceUsed, bLastSeqOnPad, bLastSequence;
  TSequence OldSequencesMemory[MAXNUMSEQUENCES];
  TSequence NewSequencesMemory[MAXNUMSEQUENCES];
  TSequence *SequenceBuffer, *OldSequences, *NewSequences;
  TClusterUC *FirstCUC, *CurrentCUC, *LastCUC, *DeleteCUC;
  TClusterUC *SequenceInCUC;


  int iIndex;
  float fastlog[256];


  /* variables for dynamic CUC memory handling */
  TClusterUC CUCMemory[MAXNUMCUC];
  int CUCMemoryArray[MAXNUMCUC];
  int CUCMemoryPtr;

#ifdef DEBUGFILE
  FILE *fin, *fpoints;
  char finname[100], fpointsname[100];
  TPeak testpeak;
  int iBin, iHeight;
  int xLower, xUpper, yLower, yUpper, xNow, yNow;
  int *maparray;
  char getstring[100];
  float xwrite, ywrite;

  maparray= (int *)malloc(4000000*sizeof(int));
#endif

  /* is magboltz database loaded, if not exit */
  if(m_padtrans->GetNRows()<1)
    {
      printf("Couldn't find magboltz data table, exiting!\n");
      return kStWarn;
    }

  /* is calibration amplitude slope database loaded, if not load */
  if(m_ampslope->GetNRows()<1)
    {
      printf("Couldn't find calibration amplitude slope table, exiting!\n");
      return kStWarn;
    }

  /* is calibration amplitude offset database loaded, if not load */
  if(m_ampoff->GetNRows()<1)
    {
      printf("Couldn't find calibration amplitude offset data table, exiting!\n");
      return kStWarn;
    }

  /* is calibration time offset database loaded, if not load */
  if(m_timeoff->GetNRows()<1)
    {
	  printf("Couldn't find calibration time offset data table, exiting!\n");
	  return kStWarn;
    }

  /* allocate memory for padtrans table */
  pradius = (double *)malloc(det->n_int_steps*10*sizeof(double));
  pdeflection = (double *)malloc(det->n_int_steps*10*sizeof(double));

  if(pradius == NULL || pdeflection == 0)
    {
      printf("Padtrans memory allocation failed, exiting!\n");
      return kStWarn;
    }

  /* integrate padtrans table from magboltz database */
  if(!fcl->calcpadtrans(det, padtrans, pradius, pdeflection))
    {
      printf("Couldn't calculate padtrans table, exiting!\n");
      return kStWarn;
    }

  /* initialize CUC memory handling */
  if(!fcl->CUCInit(CUCMemory, CUCMemoryArray, &CUCMemoryPtr))
    {
      printf("Couldn't initialize CUC memory, exiting!\n");
      return kStWarn;
    }

  /* calculate fastlog lookup */
  for(iIndex=1; iIndex<256; iIndex++)
    {
      fastlog[iIndex] = log(iIndex);
    }

  /* reset counter for found clusters */
  clusters = 0;

  /* initialize sequence and cluster lists */ 
  OldSequences=OldSequencesMemory;
  NewSequences=NewSequencesMemory;
  NewSequences[0].Length = 0;
  FirstCUC = NULL;
  iOldSeqNumber = 0;
  iNewSeqIndex=0;


  /* check if sequence table begins with a pad header */
  if((ftpcsqndx[0].index & 32768) == 0)
    {
      printf("Raw data start entry damaged! Sequence[0] = %d\n", 
	     ftpcsqndx[0].index);
      return kStWarn;
    }

#ifdef DEBUGFILE
  fin=fopen("test00", "w");
  fpoints=fopen("points00", "w");
#endif

  /* loop over raw data sequences */
  iNowSeqIndex = 0;
  iADCCounter=0;
  iPad=0;
  iSec=0;
  iRow=0;
  iNowSequence=ftpcsqndx[0].index;
  bLastSequence=0;

  while(iNowSeqIndex < fcl_ftpcsqndx->GetNRows() || bLastSequence==0)
    {
      if(iNowSeqIndex==fcl_ftpcsqndx->GetNRows() && bLastSequence==0)
	  {
	      bLastSequence=1;
	      iPad++;
	  }
       /* use next pad if iNowSequence is not a new pad header: */
      iPad++;
      iSecBuf = iSec;
      iRowBuf = iRow;

      if((iNowSequence & 32768) == 32768)
	{
	  iNowPad=ftpcsqndx[iNowSeqIndex].index;

	  /* calculate current pad/sector/row */
	  iPad = iNowPad & 255;
	  iSec = ((iNowPad >> 8) & 127);
	  iRow = iSec / 6; /* integer division! */
	  iSec -= 6 * iRow;
	  
	  /* move to next entry in sequence table */
	  iNowSequence=ftpcsqndx[++iNowSeqIndex].index;
	}
      if(iSec!=iSecBuf || iRow!=iRowBuf)
	{
	  /* set last analyzed pad out of reach, so there will be no matches */
	  iPadBuf=-2;
#ifdef DEBUG
	  printf("Now on Sector %d, Row %d\n", iSec, iRow);
#endif
	}

      /* search, fit and remove finished CUCs */ 
      for(CurrentCUC = FirstCUC; CurrentCUC!=NULL; 
	  CurrentCUC = CurrentCUC->NextClusterUC)
	{
	  if(iPad > CurrentCUC->EndPad + 1
	     || iSecBuf !=iSec || iRowBuf != iRow)
	    {
	      /* CurrentCUC is finished */
	      /* if CUC has not been lost by merging, process cluster */
	      if(CurrentCUC->EndPad > CurrentCUC->StartPad)
		{
		  /* cluster processing: increment cluster counter */
		  clusters ++;

		  int numbuf=fcl_fppoint->GetNRows();
		  int maxbuf=fcl_fppoint->GetHeader()->maxlen;
		  /* cluster processing: call hitfinder */
		  if(!fcl->FindHits(CurrentCUC, iRowBuf, iSecBuf, pradius, pdeflection, 
			       ftpcadc, det, zrow,
			       fastlog, &numbuf, 
			       &maxbuf, fppoint,
			       ampslope, ampoff, timeoff)
		     )
		    {
#ifdef DEBUG
		      printf("Hitfinder failed! Cluster is lost.\n");
#endif
		    }
		  fcl_fppoint->SetNRows(numbuf);
		}
	      DeleteCUC=CurrentCUC;
	      /* bypass CurrentCUC in CUC list */
	      if(CurrentCUC==FirstCUC)
		{
		  FirstCUC=CurrentCUC->NextClusterUC;
		}
	      else
		{
		  LastCUC->NextClusterUC=CurrentCUC->NextClusterUC;
		  CurrentCUC=LastCUC;
		}
	      /* free CurrentCUC memory */
	      if(!fcl->CUCFree(CUCMemory, CUCMemoryArray, &CUCMemoryPtr, DeleteCUC))
		{
		  printf("Fatal memory management error.\n");
		  return kStWarn;
		}
	    }
	  LastCUC=CurrentCUC;
	}

#ifdef DEBUGFILE
      if(iSec!=iSecBuf || iRow!=iRowBuf)
	{
	  fclose(fin);
	  fclose(fpoints);
	  sprintf(finname,"test%d%d", iRow,iSec); 
	  sprintf(fpointsname,"points%d%d", iRow,iSec); 
	  fin=fopen(finname, "w");
	  fpoints=fopen(fpointsname, "w");
	}
#endif

      /* initialize sequence lists: */
      /* new-array is moved to old */
      /* memory of old-array is used for new and first element initialized */
      SequenceBuffer=OldSequences;
      OldSequences=NewSequences;
      NewSequences=SequenceBuffer;
      iOldSeqNumber=iNewSeqIndex;
      iNewSeqIndex=0;
      
      if(iPad!=iPadBuf+1)
	{
	  iOldSeqNumber=0;
	}
      iPadBuf=iPad;
      
      /* reset beginning of sequence comparison */
      iOldSeqBuf=0;
      bLastSeqOnPad=0;

      /* loop over sequences while not a new pad header */
      while(bLastSeqOnPad == 0 && bLastSequence == 0)
	{
	  /* unpack to Seq structure from STAF table */ 
	  NewSequences[iNewSeqIndex].Length = (iNowSequence & 31)+1;
	  NewSequences[iNewSeqIndex].StartTimebin = (iNowSequence >> 6) & 511; 
	  NewSequences[iNewSeqIndex].StartADCEntry = iADCCounter;
	  bLastSeqOnPad=iNowSequence & 32;
	  iADCCounter += NewSequences[iNewSeqIndex].Length;
	  /* concatenate split sequences (of over 32 bins) */
	  while(((iNowSequence & 31) == 31) && 
		((ftpcsqndx[iNowSeqIndex+1].index & 32768) == 0) && 
		(((ftpcsqndx[iNowSeqIndex+1].index>> 6) & 511) == 
		 NewSequences[iNewSeqIndex].StartTimebin + 
		 NewSequences[iNewSeqIndex].Length)) 
	    {
	      iNowSequence=ftpcsqndx[++iNowSeqIndex].index;
	      bLastSeqOnPad=iNowSequence & 32;
	      NewSequences[iNewSeqIndex].Length += (iNowSequence & 31)+1;
	      iADCCounter += (iNowSequence & 31)+1; 

	    }
	  /* mark sequence as unused */
	  SequenceInCUC=NULL;

	  /* compare this sequence to old sequences */
	  for(iOldSeqIndex=iOldSeqBuf; iOldSeqIndex < iOldSeqNumber; 
	      iOldSeqIndex++)
	    {
	      /* are beginning or end of new sequence between */
	      /* beginning and end of old sequence? */
	      if(((NewSequences[iNewSeqIndex].StartTimebin >= 
		   OldSequences[iOldSeqIndex].StartTimebin) && 
		  (NewSequences[iNewSeqIndex].StartTimebin <= 
		   OldSequences[iOldSeqIndex].StartTimebin + 
		   OldSequences[iOldSeqIndex].Length-1)) || 
		 ((NewSequences[iNewSeqIndex].StartTimebin + 
		   NewSequences[iNewSeqIndex].Length-1 >= 
		   OldSequences[iOldSeqIndex].StartTimebin) && 
		  (NewSequences[iNewSeqIndex].StartTimebin + 
		   NewSequences[iNewSeqIndex].Length-1 <= 
		   OldSequences[iOldSeqIndex].StartTimebin + 
		   OldSequences[iOldSeqIndex].Length-1)) ||
		 ((OldSequences[iOldSeqIndex].StartTimebin >= 
		   NewSequences[iNewSeqIndex].StartTimebin) && 
		  (OldSequences[iOldSeqIndex].StartTimebin <= 
		   NewSequences[iNewSeqIndex].StartTimebin + 
		   NewSequences[iNewSeqIndex].Length-1)))
		{
		  /* yes, matching sequence found */
		  /* set beginning of search for next sequence */
		  iOldSeqBuf=iOldSeqIndex;
		  bOldSequenceUsed=0;
 
		  /* compare matching sequences to old CUCs */ 
		  /* loop over all active CUCs */
		  for(CurrentCUC = FirstCUC; CurrentCUC!=NULL; 
		      CurrentCUC = CurrentCUC->NextClusterUC)
		    {
		      LastCUC=CurrentCUC;
		      /* loop over CUC Sequences on last pad */
		      for(iCUCSequence=1; 
			  iCUCSequence<CurrentCUC->NumSequences; 
			  iCUCSequence++)
			{
			  if((OldSequences[iOldSeqIndex].StartTimebin == 
			      CurrentCUC->Sequence[iCUCSequence].StartTimebin)
			     && (CurrentCUC->SequencePad[iCUCSequence] == 
				 iPad-1))
			    {
			      bOldSequenceUsed=1;
			      /* matching old sequence is in CUC */
			      /* check if new sequence is already used */
			      if(SequenceInCUC!=CurrentCUC)
				{
				  if(SequenceInCUC!=NULL && SequenceInCUC!=CurrentCUC)
				    {
				      /* yes, already used, merge CUCs */
				      /* mark old CUC for removal */ 
				      SequenceInCUC->EndPad = 
					SequenceInCUC->StartPad;
				      /* set StartPad to the smaller StartPad */
				      if(SequenceInCUC->StartPad < 
					 CurrentCUC->StartPad)
					{
					  CurrentCUC->StartPad = 
					    SequenceInCUC->StartPad;
					}
				      CurrentCUC->EndPad=iPad;
				      /* copy all sequences to CurrentCUC */
				      for(iMoveSequence=0; 
					  (iMoveSequence
					   <SequenceInCUC->NumSequences) &&
					    (CurrentCUC->NumSequences+iMoveSequence+1
					     <MAXNUMSEQUENCES); 
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
				      CurrentCUC->NumSequences += 
					SequenceInCUC->NumSequences;

				      if(CurrentCUC->NumSequences > MAXNUMSEQUENCES)
					{
					  CurrentCUC->NumSequences = MAXNUMSEQUENCES;
					}

				      SequenceInCUC=CurrentCUC;
				    }
				  else /* to: if(SequenceInCUC!=NULL) */
				    {
				      /* add sequence to CUC */
				      if(CurrentCUC->NumSequences<MAXNUMSEQUENCES)
					{
					  CurrentCUC->Sequence[CurrentCUC->NumSequences]
					    .Length = NewSequences[iNewSeqIndex].Length;
					  CurrentCUC->Sequence[CurrentCUC->NumSequences]
					    .StartTimebin
					    =NewSequences[iNewSeqIndex].StartTimebin;
					  CurrentCUC->Sequence[CurrentCUC->NumSequences]
					    .StartADCEntry
					    = NewSequences[iNewSeqIndex].StartADCEntry;
					  CurrentCUC->SequencePad[CurrentCUC->NumSequences]
					    =iPad;
					  CurrentCUC->NumSequences++;
				       
					}
				      CurrentCUC->EndPad=iPad;
				      SequenceInCUC=CurrentCUC;
				      
				      /* check if new sequence touches sector limit */
				      if(NewSequences[iNewSeqIndex].StartTimebin==0 ||
					 NewSequences[iNewSeqIndex].StartTimebin
					 +NewSequences[iNewSeqIndex].Length
					 ==det[0].n_bins-1 || 
					 iPad==det[0].n_pads-1)
					{
					  CurrentCUC->CutOff=1;
					}
				    } /* end of: if(SequenceInCUC!=NULL) ... else */
				} /* end of: if(SequenceInCUC...) */
			    } /* end of: if((OldSequences...)) */ 
			}    /* end of: for(ICUCSequence...) */

		    }    /* end of: for(CurrentCUC...) */
		  if(SequenceInCUC==NULL && NewSequences[iNewSeqIndex].Length>1)
		    {
		      /* no matching CUC was found: create new CUC */
		      /* allocate memory */
		      CurrentCUC=fcl->CUCAlloc(CUCMemory, CUCMemoryArray, 
					  &CUCMemoryPtr);
		      if(CurrentCUC == NULL)
			{
			  /* no free memory, overwrite last CUC */
#ifdef DEBUG
			  printf("Previous cluster is now lost.\n");
#endif
			  CurrentCUC=LastCUC;
			  return kStWarn;
			}
		      else
			{
			  /* set pointers to this CUC */
			  if(FirstCUC == NULL)
			    {
			      FirstCUC = CurrentCUC;
			    }
			  else
			    {
			      LastCUC->NextClusterUC=CurrentCUC;
			    }
			  
			  /* this is the newest CUC */
			  CurrentCUC->NextClusterUC=NULL;
			}
		      
		      /* fill new CUC structure */
		      CurrentCUC->StartPad=iPad-1;
		      CurrentCUC->EndPad=iPad;
		      CurrentCUC->NumSequences=2;
		      
		      /* copy sequences to CUC */
		      CurrentCUC->Sequence[0].Length =
			OldSequences[iOldSeqIndex].Length;
		      CurrentCUC->Sequence[0].StartTimebin =
			OldSequences[iOldSeqIndex].StartTimebin;
		      CurrentCUC->Sequence[0].StartADCEntry = 
			OldSequences[iOldSeqIndex].StartADCEntry;
		      CurrentCUC->SequencePad[0] = iPad-1;
		      
		      CurrentCUC->Sequence[1].Length =
			NewSequences[iNewSeqIndex].Length;
		      CurrentCUC->Sequence[1].StartTimebin =
			NewSequences[iNewSeqIndex].StartTimebin;
		      CurrentCUC->Sequence[1].StartADCEntry =
			NewSequences[iNewSeqIndex].StartADCEntry;
		      CurrentCUC->SequencePad[1]=iPad;
		      SequenceInCUC=CurrentCUC;
		      
		      /* check if new CUC touches sector limits */
		      if(iPad==1 || iPad==det[0].n_pads-1 || 
			 CurrentCUC->Sequence[0].StartTimebin==0 || 
			 CurrentCUC->Sequence[0].StartTimebin
			 +CurrentCUC->Sequence[0].Length
			 ==det[0].n_bins-1 || 
			 CurrentCUC->Sequence[1].StartTimebin==0 || 
			 CurrentCUC->Sequence[1].StartTimebin
			 +CurrentCUC->Sequence[1].Length
			 ==det[0].n_bins-1)
			{
			  CurrentCUC->CutOff=1;
			}
		      else
			{
			  CurrentCUC->CutOff=0;
			}
		    }   /* end of: if(SequenceInCUC==NULL) */
		  else
		    {
		      if(bOldSequenceUsed==0 && SequenceInCUC!=0)
			{  
			  /* new sequence has been used but old one hasn't */
			  /* append to cluster */
			  if(SequenceInCUC->NumSequences<MAXNUMSEQUENCES)
			    {
			      SequenceInCUC->Sequence[SequenceInCUC->NumSequences]
				.Length = OldSequences[iOldSeqIndex].Length;
			      SequenceInCUC->Sequence[SequenceInCUC->NumSequences]
				.StartTimebin
				=OldSequences[iOldSeqIndex].StartTimebin;
			      SequenceInCUC->Sequence[SequenceInCUC->NumSequences]
				.StartADCEntry
				= OldSequences[iOldSeqIndex].StartADCEntry;
			      SequenceInCUC->SequencePad[SequenceInCUC->NumSequences]
				=iPad-1;
			      SequenceInCUC->NumSequences++;
			    }
			  
			  /* check if Old sequence touches sector limit */
			  if(OldSequences[iOldSeqIndex].StartTimebin==0 ||
			     OldSequences[iOldSeqIndex].StartTimebin
			     +OldSequences[iOldSeqIndex].Length
			     ==det[0].n_bins-1 || 
			     iPad==det[0].n_pads-1)
			    {
			      SequenceInCUC->CutOff=1;
			    }
			}
		    }
		}     /* end of: if(sequence matching) */
	      
	    }     /* end of: for(iOldSeqIndex...) */
	  /* increment counter for ftpcsqndx table */
	  iNowSeqIndex++;
	  /* increment counter for NewSequence array */
	  iNewSeqIndex++;
	  /* load next sequence */
	  iNowSequence=ftpcsqndx[iNowSeqIndex].index;
	}     /* end of: while((iNowSequence & 32768) == 0) */
    }     /* end of: while(iNowSeqIndex < fcl_ftpcsqndx->GetNRows()) */
  printf("Found %d clusters and processed to %d hits.\n", clusters,(int) fcl_fppoint->GetNRows() );

#ifdef DEBUGFILE
  fclose(fin);
  fclose(fpoints);

  for(iRow=0; iRow<20; iRow++)
    {
      for(iSec=0; iSec<6; iSec++)
	{
	  sprintf(finname, "test%d%d", iRow, iSec);
	  sprintf(fpointsname, "charge%d%d", iRow, iSec);
	  fin=fopen(finname, "r");
	  fpoints=fopen(fpointsname, "w");
	  for(xNow=0; xNow<2000; xNow++)
	    {
	      for(yNow=0; yNow<2000; yNow++)
		{
		  maparray[xNow+2000*yNow]=0;
		}
	    }

	  sprintf(getstring, "\0");
	  fgets(getstring, sizeof(getstring), fin);
	  while(strlen(getstring)>3)
	    {
	      sscanf(getstring, "%d %d %d", &iPad, &iBin, &iHeight);
	      
	      testpeak.PadPosition=iPad-0.5;
	      testpeak.TimePosition=iBin-0.5;
	      fcl->Padtrans(&testpeak, iRow, iSec, det, zrow, pradius, pdeflection);
	      xLower=(int) ((1000 / 31) * testpeak.x + 1000);
	      yLower=(int) ((1000 / 31) * testpeak.y + 1000);

	      testpeak.PadPosition=iPad+0.5;
	      testpeak.TimePosition=iBin+0.5;
	      fcl->Padtrans(&testpeak, iRow, iSec, det, zrow, pradius, pdeflection);
	      xUpper=(int) ((1000 / 31) * testpeak.x + 1000);
	      yUpper=(int) ((1000 / 31) * testpeak.y + 1000);

	      for(xNow=0; xNow<abs(xUpper-xLower); xNow++)
		{
		  for(yNow=0; yNow<abs(yUpper-yLower); yNow++)
		    {
		      if(xUpper<xLower)
			maparray[(xUpper+xNow)+2000*(yUpper+yNow)]=iHeight;
		      else
			maparray[(xLower+xNow)+2000*(yLower+yNow)]=iHeight;
		    }
		}

	      sprintf(getstring, "\0");
	      fgets(getstring, sizeof(getstring), fin);

	    }
	  
	  for(xNow=0; xNow<2000; xNow++)
	    {
	      for(yNow=0; yNow<2000; yNow++)
		{
		  if(maparray[xNow+2000*yNow]!=0)
		    {
		      xwrite= ((float) xNow-1000)*31/1000;
		      ywrite= ((float) yNow-1000)*31/1000;

		      fprintf(fpoints, "%f %f %d\n", xwrite, ywrite, maparray[xNow+2000*yNow]);
		    }
		}
	    }
	  fclose (fin);
	  fclose (fpoints);
	}
    }

  free(maparray);

#endif

  free(pradius);
  free(pdeflection);

  delete fcl;




      if(Debug())  cout<<"finished running fcl"<<endl;

      } else {

      cout <<"StFtpcClusterMaker: Tables are not found:" 
	   << " fcl_ftpcndx   = " << fcl_ftpcndx 
	   << " fcl_ftpcsqndx = " << fcl_ftpcsqndx 
	   << " fcl_ftpcadc   = " << fcl_ftpcadc << endl;
      }

    } 

else { 

//                      FFS
      St_DataSet *gea = GetDataSet("geant");
      St_DataSetIter geant(gea);
      St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex *) geant("g2t_vertex");
      St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
      St_g2t_ftp_hit *g2t_ftp_hit = (St_g2t_ftp_hit *) geant("g2t_ftp_hit");
      if (g2t_track && g2t_ftp_hit){
        St_ffs_gepoint *ffs_gepoint = new St_ffs_gepoint("ffs_gepoint",150000);
                                           m_DataSet->Add(ffs_gepoint);
        St_fcl_fppoint *fcl_fppoint = new St_fcl_fppoint("fcl_fppoint",150000);
                                           m_DataSet->Add(fcl_fppoint);

        if(Debug()) cout<<"NO RAW DATA AVAILABLE - start running StFtpcFastSimu"<<endl;

	Int_t numHit=g2t_ftp_hit->GetNRows();
	Int_t numTrack=g2t_track->GetNRows();
	Int_t numGepoint=ffs_gepoint->GetNRows();
	Int_t maxGepoint=ffs_gepoint->GetHeader()->maxlen;
	Int_t numFppoint=fcl_fppoint->GetNRows();
	Int_t maxFppoint=fcl_fppoint->GetHeader()->maxlen;
	StFtpcFastSimu *ffs = new StFtpcFastSimu(g2t_ftp_hit->GetTable(), 
						 &numHit,
						 g2t_track->GetTable(), 
						 &numTrack,
						 g2t_vertex->GetTable(),
						 m_fspar->GetTable(),
						 m_gaspar->GetTable(),
						 ffs_gepoint->GetTable(),
						 &numGepoint, maxGepoint,
						 fcl_fppoint->GetTable(),
						 &numFppoint, maxFppoint);
	ffs_gepoint->SetNRows(numGepoint);				      
	fcl_fppoint->SetNRows(numFppoint);				      
        if(Debug())cout<<"finished running StFtpcFastSimu"<<endl;
	delete ffs;
      }
    }

  MakeHistograms(); // FTPC cluster finder histograms
  return iMake;
}
//_____________________________________________________________________________
void StFtpcClusterMaker::MakeHistograms() 
{

 // cout<<"*** NOW MAKING HISTOGRAMS FOR fcl ***"<<endl;

  // Create an iterator
  St_DataSetIter fcl_points(m_DataSet);

  //Get the table
  St_fcl_fppoint *ppointh;
  ppointh = (St_fcl_fppoint *) fcl_points.Find("fcl_fppoint");
  if (! ppointh) 	return;
  fcl_fppoint_st *r = ppointh->GetTable();
  for (Int_t i=0; i<ppointh->GetNRows();i++,r++) {
    Int_t flag = r->flags;
    if (flag > 0) {
      Int_t bin = 6;
      for (Int_t twofac=32; twofac>0; twofac=twofac/2,bin--) {
        Int_t nbit = flag/twofac;
        if (nbit != 1) 	continue;
        m_flags->Fill((float)bin);
        flag = flag - nbit*twofac;        
      }//end loop twofac
    }//endif flag

    Float_t nrow = r->row;
    m_row->Fill(nrow);
    Float_t nsec = r->sector;
    m_sector->Fill(nsec);
    m_row_sector->Fill(nrow,nsec);
    Float_t npad = r->n_pads;
    m_pads->Fill(npad);
    Float_t nbin = r->n_bins;
    m_timebins->Fill(nbin);
    m_npad_nbin->Fill(npad,nbin);
  }//end rows loop 
}
                                   

