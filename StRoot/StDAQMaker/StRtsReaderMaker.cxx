/***************************************************************************
 *
 * $Id: StRtsReaderMaker.cxx,v 1.18 2008/12/29 23:57:40 fine Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Create the DAQ table from the RTS_READER
 *               
 * Input:  RTS_Reader
 * Output: daq tables
 *
 ***************************************************************************
 *
 * $Log: StRtsReaderMaker.cxx,v $
 * Revision 1.18  2008/12/29 23:57:40  fine
 * restore the economic mode
 *
 * Revision 1.17  2008/12/29 21:16:47  fine
 * Preserve / accumulate the copy the DAQ table to avoid the dead data access
 *
 * Revision 1.16  2008/12/22 19:39:35  fine
 * improve the diagnostic
 *
 * Revision 1.15  2008/12/22 16:58:23  fine
 * eliminate the redundant  messages
 *
 * Revision 1.14  2008/12/17 02:04:59  fine
 * improve the diagnostic messages
 *
 * Revision 1.13  2008/12/17 00:55:15  fine
 * improve the diagnostic messages
 *
 * Revision 1.12  2008/12/16 22:33:00  fine
 * Improve the diagnostic message
 *
 * Revision 1.11  2008/12/16 19:39:19  fine
 * replace daq_dta_dict with get_size_t()
 *
 * Revision 1.10  2008/11/26 18:01:30  fine
 * prepare StRtsReaderMaker for DAQ_READER transition
 *
 * Revision 1.9  2008/11/25 21:33:22  fine
 * preparing  DAQ maker for DAQ_READER
 *
 * Revision 1.8  2008/11/25 21:28:03  fine
 * preprae DAQ maker for DAQ_READER
 *
 * Revision 1.7  2008/04/28 18:44:53  fine
 * Add the third numeric parameter for get method
 *
 * Revision 1.6  2008/04/28 15:12:36  fine
 * Fix the messages
 *
 * Revision 1.5  2008/04/10 16:23:34  fine
 * Activate the generic RTS_READER interface
 *
 * Revision 1.4  2008/04/09 21:05:03  fine
 * Add  the generic way to treat the DAQ RTS structures
 *
 * Revision 1.3  2008/04/08 22:39:47  fine
 * typo
 *
 * Revision 1.2  2008/04/08 22:19:56  fine
 * Synch old and new EVP and RTS DAQ readers
 *
 * Revision 1.1  2008/04/08 20:03:52  fine
 * add the maker to communicate with the new RTS_READER from RTS
 *
 * Revision 1.14  2008/02/01 01:20:14  fine
 * Add the Slave mode
 *
 * Revision 1.13  2008/02/01 00:44:13  fine
 * Add the TROOT header file
 *
 * Revision 1.12  2008/01/31 23:53:22  fine
 * Do not call fRtsReader->Make() in Slave mode
 *
 * Revision 1.11  2008/01/30 20:05:22  fine
 * add the simple datastruct
 *
 * Revision 1.10  2008/01/30 04:39:22  fine
 * Add map for tof/raw query
 *
 * Revision 1.9  2008/01/29 18:29:54  fine
 * Introdcue the slave mode for the rts_reader
 *
 * Revision 1.8  2008/01/12 00:37:53  fine
 * Clean up
 *
 * Revision 1.7  2008/01/11 23:41:31  fine
 * The first correctly working version
 *
 * Revision 1.6  2008/01/11 21:09:44  fine
 * Fix the table size
 *
 * Revision 1.5  2008/01/11 19:08:33  fine
 * Couple of bugs fixed. Move on
 *
 * Revision 1.4  2008/01/11 18:28:05  fine
 * Remove the redundant data-member
 *
 * Revision 1.3  2008/01/11 18:09:35  fine
 * Fix some typos
 *
 * Revision 1.2  2008/01/11 14:57:05  fine
 * add DAQ system C-struct dictionary
 *
 * Revision 1.1  2008/01/11 01:16:54  fine
 * The generic maker to steer the online RTS system
 *
 *
 * StRtsReaderMaker - class to fill the STAR table DAQ event model
 *
 **************************************************************************/
#include "StRtsReaderMaker.h"
#include "StRtsTable.h"
#include "TROOT.h"
#include "StDAQMaker/StDAQReader.h"

#include "TDataSetIter.h"

#if !defined(OLD_EVP_READER) && !defined(NEW_DAQ_READER)
#  include "RTS/src/RTS_READER/daq_det.h"
#  include "RTS/src/RTS_READER/daq_dta.h"
#  include "RTS/src/RTS_READER/daq_dta_structs.h"
    typedef unsigned int UINT32;
#  include "RTS/include/evp.h"
#  include "RTS/src/EVP_READER/cfgutil.h"
#  include "RTS/src/EVP_READER/evpReaderClass.h"
#  include "RTS/src/RTS_READER/rts_reader.h"
#elif defined(NEW_DAQ_READER)
#  include "RTS/src/DAQ_READER/daq_det.h"
#  include "RTS/src/DAQ_READER/daq_dta.h"
#  include "RTS/src/DAQ_READER/daq_dta_structs.h"
    typedef unsigned int UINT32;
#  include "RTS/include/evp.h"
#  include "RTS/src/DAQ_READER/cfgutil.h"
#  include "RTS/src/DAQ_READER/daqReader.h"
#endif



ClassImp(StRtsReaderMaker);

//_____________________________________________________________
StRtsReaderMaker::StRtsReaderMaker(const char *name):StMaker(name)
      ,fRtsReader(0),fRtsTable(0),fBank(0),fSlaveMode(true)
{
  LOG_DEBUG << "StRtsReaderMaker::ctor"  << endm;
}

//_____________________________________________________________
StRtsReaderMaker::~StRtsReaderMaker() 
{
   if (!fSlaveMode) delete fRtsReader; 
   fRtsReader = 0;
   delete fRtsTable;  fRtsTable  = 0;
}
//_____________________________________________________________
Int_t StRtsReaderMaker::Init() {
#if 0
   if (!fFileName.IsNull() ) {
     fRtsReader = new rts_reader("R") ;  // call myself "R"; used for debugging prints...
     fRtsReader->enable("*") ;           // enable the selected det or dets
     fRtsReader->add_input(fFileName);   // add all files to the list...
     fSlaveMode = false;
   }
#endif
   return 0;
}

//_____________________________________________________________
rts_reader *StRtsReaderMaker::InitReader()
{
   // Init EVP_READER 
   if (!fRtsReader) {
      StDAQReader *daqReader = 0;
      LOG_INFO << "StRtsReaderMaker::InitReader"  << endm;
      TDataSet *dr = GetDataSet("StDAQReader");
      if(dr) daqReader = (StDAQReader *)(dr->GetObject());

      if(daqReader == NULL) {
         LOG_INFO << "StRtsReaderMaker::InitReader No daqReader available..." << endm;
      } else {
         evpReader *daqEvp = daqReader->getFileReader();
         if(daqEvp == NULL) {
            LOG_INFO << "StRtsReaderMaker::InitReader No evpReader available..." << endm;
         } else {
            LOG_INFO << "StRtsReaderMaker::InitReader: evpReader was found: "  << daqEvp << endm;
            fRtsReader = daqEvp->rts();
            if (!fRtsReader) {
              LOG_ERROR << "StRtsReaderMaker::InitReader: no rts_reader was found!" << endm;
            }
         }
      }
   }
   return fRtsReader;
}

//_____________________________________________________________
void StRtsReaderMaker::Clear(Option_t *option)
{
   fRtsTable = 0;
   fBank = 0;
   fLastQuery = "";
   // StMaker::Clear will delete fRtsTable
   StMaker::Clear(option);
}

#if 0
//_____________________________________________________________
static const char*RtsDataTypeByBankName(const char *bankName) 
{
   //______________________________________________________
   //
   // Here we can do something more clever of course
   // The idea l solution:
   // the mapping should be provided by the class rts_reader
   //______________________________________________________
   //
   // rts_reader::RtsDataTypeByBankName(const char *bankName)
   //______________________________________________________

   TString bn = bankName;
   if (bn == "cld" ) return "daq_cld";
   if (bn == "raw" ) {
      static const char *str = 0;
      if (!str) {
         gROOT->ProcessLine("struct intstruct { int a;}");
      }
      return "intstruct";
   }
   return 0;
}
#endif

//_____________________________________________________________
StRtsTable *StRtsReaderMaker::InitTable(const char *detName,const char *bankName)
{
   // Create the new instance of the StRtsTable
   if (fRtsTable) {
       // make sure there was no data anymore
       if (fBank && !fBank->is_empty()) 
       {
          LOG_INFO << " You are going to use \"" << detName << "/" << bankName << "\" RTS bank" << endm;
          LOG_INFO << " even though you did not use all information from the previous RTS  bank: \""
                << fLastQuery << "\" yet" << endm;
       }
       delete fRtsTable; 
       fRtsTable = 0; // forget this table. It will be deleted by Clear method anyway
   }
#ifndef NEW_DAQ_READER
   size_t dtBankSize = daq_dta_dict(detName,bankName);
#else
   size_t dtBankSize = fBank->get_size_t();
#endif
   if ( dtBankSize )  {
       // we will reallocate it within FillTable() method
      fRtsTable = new StRtsTable(dtBankSize,2);
      fRtsTable->Print();
      AddData(fRtsTable);
   }
   return fRtsTable;
}

//_____________________________________________________________
TDataSet *StRtsReaderMaker::FillTable() 
{
   assert(fRtsTable);
   if (fBank->iterate()) {
      fRtsTable->SetAll(  fBank->sec
                        , fBank->pad
                        , fBank->rdo
                        , fBank->row);

      fRtsTable->SetNRows(0);
      LOG_DEBUG <<" StRtsReaderMaker::FillTable(): the bank size is  " 
            << fBank->ncontent << " row" << ((fBank->ncontent>1)?"s ":" ")
            << fRtsTable->GetRowSize() << " bytes each" << endm;
      fRtsTable->AppendRows(fBank->Byte,fBank->ncontent);
      fRtsTable->SetNRows(fBank->ncontent);
   } else {
      if (!fLastQuery.IsNull()) {
         LOG_INFO <<" StRtsReaderMaker::FillTable(): No data has been found for \"" 
                  << fLastQuery << "\" to fill the table"
                  << endm;
      }
      if (fRtsTable) fRtsTable->Print(0,5);
      delete fRtsTable; 
      fRtsTable = 0; // forget this table. It will be deleted by Clear method anyway
      fLastQuery = ""; 

   }
   return fRtsTable;
}

//_____________________________________________________________________________
TDataSet  *StRtsReaderMaker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const 
{
   //--------------------------------------------------------------
   // Input: Query: RTS/[detector[/rts_bank[ [bank_parameters] ]]]
   // for example:  RTS/tpx/cld[16] - to get the clusters for the
   //                                 16th sector of tpx
   // Return the RTS bank  decorated as TGenericTable
   //--------------------------------------------------------------
   
  // Do not process the empty request:

  if ( !(logInput && logInput[0]) )
     return StMaker::FindDataSet(logInput,uppMk,dowMk);

  TString rtsRequest = logInput;
  Bool_t rtsSystem   = false;
  TDataSet *ds       = 0;
  StRtsReaderMaker *thisMaker = (StRtsReaderMaker *)this;

  if (fLastQuery == rtsRequest) {
     rtsSystem = true;
  } else if (fRtsReader) {
     TObjArray *tokens = rtsRequest.Tokenize("/[],");
     Int_t nItems = tokens->GetEntries();  
     if ( !strcmp(tokens->At(0)->GetName(),"RTS") ) {
        // Pick the detector name
        TString detName = tokens->At(1)->GetName();
           // we found the detector 
        daq_det *rts_det = fRtsReader->det((const char*)detName);
        if (rts_det) {
           // Pick the rts bank
           StRtsReaderMaker *thisMaker = (StRtsReaderMaker *)this;
           switch (nItems-2) {
              case 1:
                 thisMaker->fBank = rts_det->get( tokens->At(2)->GetName() );
                 break;
               case 2:
                 thisMaker->fBank = rts_det->get( tokens->At(2)->GetName()
                                 ,atoi(tokens->At(3)->GetName()));
                 LOG_INFO <<" StRtsReaderMaker::FindDataSet det(\"" <<
                       detName << "\")->get(\""<<tokens->At(2)->GetName()
                       << "\"," << atoi(tokens->At(3)->GetName())
                       << ")  "
                       << "fBank = "<< thisMaker->fBank << endm;
                 break;
               case 3:
                 thisMaker->fBank = rts_det->get( tokens->At(2)->GetName()
                                 ,atoi(tokens->At(3)->GetName())
                                 ,atoi(tokens->At(4)->GetName()));
                 break;
               case 4:
                 thisMaker->fBank = rts_det->get( tokens->At(2)->GetName()
                                 ,atoi(tokens->At(3)->GetName())
                                 ,atoi(tokens->At(4)->GetName())
                                 ,atoi(tokens->At(5)->GetName()));
                 break;
              default:  thisMaker->fBank = 0; break;
           };
           if (fBank) {
              thisMaker->InitTable(detName,tokens->At(2)->GetName());
              rtsSystem = true;
              thisMaker->fLastQuery = rtsRequest;
           } else {
              thisMaker->fLastQuery = "";
           }
        }
        delete tokens;
     }
  }

  if (rtsSystem) {
     ds = thisMaker->FillTable();
  } else {
     ds = StMaker::FindDataSet(logInput,uppMk,dowMk); 
  }
  return ds;
}

//_____________________________________________________________
Int_t StRtsReaderMaker::InitRun(int run)
{
  Int_t res = kStOK;
#ifndef NEW_EVP_READER
  if (!fSlaveMode && fRtsReader) {
     LOG_INFO << "StRtsReaderMaker::InitRun"  << endm;
     fRtsReader->InitRun(run);               // some dummy for now
     fRtsReader->det("tpx")->SetMode(3);     // make TPX do all!  
  }
#endif
  return res;
}

//_____________________________________________________________
Int_t StRtsReaderMaker::Make()
{
   // Force  RTS_READER to get the next event if it is not slave
   int res = kStOk;
   InitReader();
#ifndef NEW_EVP_READER
   if (fSlaveMode)  return kStOk;
   //-------------------------------------
   int rtsReturn = fRtsReader->Make();  //
   //-------------------------------------
   
   // Check the outcome:
   if (rtsReturn == EOF)  {
      res = kStEOF;
   } else {
      if(fRtsReader->l_errno) {
         rts_reader *r = fRtsReader;
         LOG_ERROR << Form("At file \"%s\", event %d: error [%s]"
               , r->select_files[r->cur_file_ix]
               , r->cur_event_in_file
               , strerror(r->l_errno))
         << endm ;
         res = kStErr;
       }
       LOG_INFO << " ----- > StRtsReaderMaker::Make() was Ok  < ----- ! " 
                << kStOk << endm;
   }
   return res;
#else
   // with NEW_EVP_READER we are always in "slave" mode
   return res;
#endif
}
