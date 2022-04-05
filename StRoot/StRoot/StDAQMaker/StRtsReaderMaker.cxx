/***************************************************************************
 *
 * $Id: StRtsReaderMaker.cxx,v 1.34 2013/02/19 20:10:19 fisyak Exp $
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
 * Revision 1.34  2013/02/19 20:10:19  fisyak
 * Akio & Valery modification for meta data, bug #2452
 *
 * Revision 1.33  2012/09/13 20:01:49  fisyak
 * Clean up, use Jeff's skip_then_get
 *
 * Revision 1.32  2010/12/14 15:27:04  genevb
 * Use LOG_DEBUG only in maker's Debug modes
 *
 * Revision 1.31  2010/03/17 19:52:44  fine
 * Comment out the redundant debug message
 *
 * Revision 1.30  2009/11/23 15:56:15  fisyak
 * reduce print out
 *
 * Revision 1.29  2009/11/19 22:42:22  fine
 * remove the token leak #1712
 *
 * Revision 1.28  2009/10/14 14:46:29  fine
 * Initialize fDatReader to zero at class ctor. Issue #1665
 *
 * Revision 1.27  2009/10/13 19:42:28  fine
 * remove the redundant assert
 *
 * Revision 1.26  2009/10/13 19:32:44  fine
 * Re-Activate DAQ reader
 *
 * Revision 1.25  2009/10/13 15:51:48  fine
 * Activate the new DAT file format
 *
 * Revision 1.24  2009/10/09 22:36:34  fine
 * remove the redundant components
 *
 * Revision 1.23  2009/10/07 00:52:32  fine
 * Move daqReader instantiation from StDAQMaker to StDAQReader to switch between input files properly
 *
 * Revision 1.22  2009/07/22 21:42:52  fine
 * Add DAQ event header to pass
 *
 * Revision 1.21  2009/04/28 16:35:48  fine
 * downgrade the message level from INFO to DEBUG
 *
 * Revision 1.20  2009/03/23 15:38:23  fine
 * remove redundant messafes , reduce the level of others from INFO to DEBUG
 *
 * Revision 1.19  2009/02/20 23:32:43  fine
 * avoid the fRtsTable double destruction. It causes the chain crash. Thanks Fisyak
 *
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

#  include "RTS/src/DAQ_READER/daq_det.h"
#  include "RTS/src/DAQ_READER/daq_dta.h"
#  include "RTS/src/DAQ_READER/daq_dta_structs.h"
    typedef unsigned int UINT32;
#  include "RTS/include/evp.h"
#  include "RTS/src/DAQ_READER/cfgutil.h"
#  include "RTS/src/DAQ_READER/daqReader.h"

#include "StStreamFile.h"


ClassImp(StRtsReaderMaker);

//_____________________________________________________________
StRtsReaderMaker::StRtsReaderMaker(const char *name):StMaker(name)
      ,fRtsReader(0),fDatReader(0),fRtsTable(0),fBank(0)
{
  // LOG_DEBUG << "StRtsReaderMaker::ctor"  << endm;
}

//_____________________________________________________________
StRtsReaderMaker::~StRtsReaderMaker() 
{
   delete fRtsReader; 
   fRtsReader = 0;
   // fRtsTable will be deleted by the base StMaker dtor.
   fRtsTable = 0;
}
//_____________________________________________________________
Int_t StRtsReaderMaker::Init() {
   return StMaker::Init();
}

//_____________________________________________________________
daqReader *StRtsReaderMaker::InitReader()
{
   // Init EVP_READER 
   if (!fRtsReader && !fDatReader) {
      StDAQReader *daqReader = 0;
      // LOG_DEBUG << "StRtsReaderMaker::InitReader"  << endm;
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
              LOG_ERROR << "StRtsReaderMaker::InitReader: no daqReader was found!" << endm;
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
//_____________________________________________________________
void StRtsReaderMaker::SetDaqReader(daqReader *reader)
{
   if (reader) {
      assert(reader && !fDatReader && "Can not use two readers simultaneously");
   }
   fRtsReader = reader;
}
//_____________________________________________________________
void StRtsReaderMaker::SetDatReader(StStreamFile *reader)
{
   if (reader) {
      assert(reader && !fRtsReader && "Can not use two readers simultaneously");
   }
   fDatReader = reader;
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
          if (Debug()) {
             LOG_DEBUG << " You are going to use \"" << detName << "/" << bankName << "\" RTS bank" << endm;
             LOG_DEBUG << " even though you did not use all information from the previous RTS  bank: \""
                   << fLastQuery << "\" yet" << endm;
          }
       }
       delete fRtsTable; 
       fRtsTable = 0; // forget this table. It will be deleted by Clear method anyway
   }
   size_t dtBankSize = 0;
   if (fBank) { dtBankSize = fBank->get_size_t(); }
   else if (fDatReader) { dtBankSize = fDatReader->Length() ; }
   if ( dtBankSize )  {
       // we will reallocate it within FillTable() method
      fRtsTable = new StRtsTable(dtBankSize,2);
      if (Debug() > 3 ) fRtsTable->Print();
      AddData(fRtsTable);
   }
   return fRtsTable;
}
//_____________________________________________________________
void StRtsReaderMaker::FillDaqHeader() 
{
   if (fRtsReader) {
      fRtsTable->SetToken   (fRtsReader->token  );
      fRtsTable->SetTrgcmd  (fRtsReader->trgcmd );
      fRtsTable->SetDaqcmd  (fRtsReader->daqcmd );
      fRtsTable->SetTrgword (fRtsReader->trgword);
      fRtsTable->SetPhyword (fRtsReader->phyword);
      fRtsTable->SetDaqbits (fRtsReader->daqbits);
      fRtsTable->SetDaqbits_l1 (fRtsReader->daqbits_l1);
      fRtsTable->SetDaqbits_l2 (fRtsReader->daqbits_l2);
      fRtsTable->SetEvpgroups (fRtsReader->evpgroups);
      fRtsTable->SetDetectors (fRtsReader->detectors);
   }
}

//_____________________________________________________________
TDataSet *StRtsReaderMaker::FillTable() 
{
   assert(fRtsTable);
   if (fBank && fBank->iterate()) {
      fRtsTable->SetAll(  fBank->sec
                        , fBank->pad
                        , fBank->rdo
			, fBank->row);
      fRtsTable->SetMeta(fBank->meta);

      fRtsTable->SetNRows(0);
      if (Debug()) {
	LOG_DEBUG <<" StRtsReaderMaker::FillTable(): the bank size is  " 
		  << fBank->ncontent << " row" << ((fBank->ncontent>1)?"s ":" ")
		  << fRtsTable->GetRowSize() << " bytes each" << endm;
      }
      fRtsTable->AppendRows(fBank->Byte,fBank->ncontent);
      fRtsTable->SetNRows(fBank->ncontent);
   } else if (fDatReader) {
      fRtsTable->SetAll(0,0,0,0);
      fRtsTable->SetNRows(0);
      fRtsTable->AppendRows(fDatReader->Record(),1);
      fRtsTable->SetNRows(1);
   } else {
#ifdef HARD_DEBUG         
      if (!fLastQuery.IsNull() && Debug()) {
         LOG_DEBUG <<" StRtsReaderMaker::FillTable(): No data has been found for \"" 
                  << fLastQuery << "\" to fill the table"
                  << endm;
      }
#endif
      if (fRtsTable && Debug() > 3 ) fRtsTable->Print(0,5);
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
   // Special case: /RTS/trg/raw - the tge data can be provided by fDatReader too
   // Return the RTS bank  decorated as TGenericTable
   //--------------------------------------------------------------
   
  // Do not process the empty request:
  //LOG_INFO << " StRtsReaderMaker::FindDataSet: " 
  //       << logInput << endm;
  if ( !(logInput && logInput[0]) )
     return StMaker::FindDataSet(logInput,uppMk,dowMk);

  TString rtsRequest = logInput;
  Bool_t rtsSystem   = false;
  TDataSet *ds       = 0;
  StRtsReaderMaker *thisMaker = (StRtsReaderMaker *)this;

  if (fLastQuery == rtsRequest) {
     rtsSystem = true;
  } else if (fRtsReader || fDatReader) {
     TObjArray *tokens = rtsRequest.Tokenize("/[],");
     Int_t nItems = tokens->GetEntries();  
     if ( !strcmp(tokens->At(0)->GetName(),"RTS") ) {
        // Pick the detector name
        TString detName = tokens->At(1)->GetName();
           // we found the detector 
        char *detNameBuf =  new char[detName.Length()+1];
        strncpy(detNameBuf,detName.Data(),detName.Length());
        detNameBuf[detName.Length()]=0;
        daq_det *rts_det = fRtsReader ? fRtsReader->det(detNameBuf) : 0;
        delete [] detNameBuf;
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
                 if (Debug()) {
                    LOG_DEBUG <<" StRtsReaderMaker::FindDataSet det(\"" <<
                          detName << "\")->get(\""<<tokens->At(2)->GetName()
                          << "\"," << atoi(tokens->At(3)->GetName())
                          << ")  "
                          << "fBank = "<< thisMaker->fBank << endm;
                 }
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
        } else if (fDatReader && detName=="trg" && TString("raw") == tokens->At(2)->GetName()) {
           thisMaker->InitTable(detName,tokens->At(2)->GetName());
           thisMaker->fLastQuery = rtsRequest;
           rtsSystem = fDatReader->Record();
           if (Debug()) {
              LOG_DEBUG << " StRtsReaderMaker::FindDataSet: DAT request was found: " 
                       << fDatReader->Length() << (void *)fDatReader->Record() << endm;
           }
        }
     }
     delete tokens;
  }

  if (rtsSystem) {
     thisMaker->FillDaqHeader();
     ds = thisMaker->FillTable();
  } else {
     ds = StMaker::FindDataSet(logInput,uppMk,dowMk); 
  }
  return ds;
}

//_____________________________________________________________
Int_t StRtsReaderMaker::InitRun(int run)
{
  return StMaker::InitRun(run);
}

//_____________________________________________________________
Int_t StRtsReaderMaker::Make()
{
   // Force  RTS_READER to get the next event if it is not slave
   int res = kStOk;
   InitReader();
   return res;
}
