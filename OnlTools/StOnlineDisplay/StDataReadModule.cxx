//*-- Author : Valeri Fine
// 
// $Id: StDataReadModule.cxx,v 1.10 2009/12/12 01:02:03 fine Exp $

#include "StDataReadModule.h"
#include "StTpcDb/StTpcDb.h"
#include "TApplication.h"
#include "TObjectTable.h"
#include "StEvtHddr.h"
#include "TDataSetIter.h"
#include "TPolyLine3D.h"
#include "StHelix3DPoints.h"
#include "TH1F.h"
#include "TRegexp.h"
#include "StHelixD.hh"
#include "TEmcSizeProvider.h"
#include "TEmcColorProvider.h"
#include "StReadBEmcStatus.h"
#include "St_emcOnlineStatus_Table.h"
#include "StEvpReader.h"
#include "StDraw3D.h"

#include "Riostream.h"
#include "TCanvas.h"
#ifdef nPhi
#  undef nPhi
#endif
#include "RTS/EventTracker/eventTrackerLib.hh"
#include "TPolyMarker3D.h"
#include "RTS/EventTracker/gl3Event.h"
#include "RTS/EventTracker/gl3Hit.h"
#include "gl3Data.h"

#include "St_db_Maker/St_db_Maker.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"

#include "StEventUtilities/StuDraw3DEvent.h"

#include "DAQ_TPC/daq_tpc.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_BTOW/daq_btow.h"

#include <vector>
#include <QMessageBox>

#define tpc  (*fTpc)


//_________________________________________________________________________________
int StDataReadModule::tpcReader(daqReader*m, int sector ) 
{
   // optimized version the tpcReader
  if(!m) return -1;
  fTpc = 0;
  daqReader *rrr = m;
  daq_dta *dd= rrr->det("tpx")->get("legacy",sector);
  int size = 0;
  if (!dd) dd= rrr->det("tpc")->get("legacy",sector);
  if (dd && (size = dd->iterate())) {
    fTpc = (tpc_t *)dd->Void;
  }
  return dd ? dd->ncontent : 0;
}


// #define DEBUGDATA
//#include <qthread.h>

static inline int  tpc_hit_postion(int sector, int row, const tpc_cl &cluster, float&x,float &y,float &z)
{
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcPadCoordinate padcoord;
  padcoord.setSector(sector); padcoord.setRow(row); padcoord.setPad(cluster.p); padcoord.setTimeBucket(cluster.t);
#ifndef  No_StTpcMaker_h
  static StGlobalCoordinate global;
  transform(padcoord,global);
#else
  static StTpcLocalSectorCoordinate local;
  static StTpcLocalCoordinate global;
  transform(padcoord,local,kFALSE);
  transform(local,global);
#endif
  StThreeVector<double> p = global.position();
  x = p.x();
  y = p.y();
  z = p.z();
  return 1;
}

//_________________________________________________________________________________
// const int GL3_TRACK_MEM_SIZE=0x800000/sizeof(L3_P);
const int GL3_TRACK_MEM_SIZE=szL3_max/sizeof(L3_P);
// ClassImp(StDataReadModule)

//_____________________________________________________________________________
/// StDataReadModule constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
StDataReadModule::StDataReadModule(const char *name):TModule(name)
, fEventPoolReader(),fNeedRecreate(kTRUE), fMountPoint("/evp"), fMountDirectory("a"),fEventType(EVP_TYPE_ANY),fEventNumber(0)
, fBadEventCounter(),fGoodEventCounter()
, fDataP(),fDataBuffer(),fBField(0.05),fL3p(),fEventTracker(),fL3DataProvider()
, fSizeProvider(),fColorProvider()
, fL3TracksOn(0),fEmcHitsOn(1),fL3HitsOn(1),fBemcOnlineStatus(),fRecordReady(kFALSE)
, fEmcDataLength(),fTracks(),fHits(),fGuiObject(),fTpc(),fBtow(),fEmc_in(),fLengthBtow()
, fEventDisplay(), fDemo(kFALSE)

{
  //
   Int_t lookUpSize = sizeof(fDeLookUp);
   fNbins = lookUpSize/sizeof(fDeLookUp[0]);
   ReadEmcStatusTable a("/evp/a/bemcStatus.txt");
//   ReadEmcStatusTable a("bemcStatus.txt");
   a.StateLoop();  
      fBemcOnlineStatus = (St_emcOnlineStatus*) a.Table();
      if (!fBemcOnlineStatus || fBemcOnlineStatus->GetNRows() != 4800) {
         Error("StDataReadModule"," The bemc status table is corrupted");
         delete fBemcOnlineStatus; fBemcOnlineStatus = 0;
         int reply = QMessageBox::critical(0,"Corrupted information"
                       , "Check the \"bemcStatus.txt\" file. The bemc status table is corrupted"
                       , QMessageBox::Ignore, QMessageBox::Abort);
         if (reply == QMessageBox::Abort ) gApplication->Terminate(0);
         fBemcOnlineStatus  = 0;
      }
}

//_____________________________________________________________________________
/// This is TLA destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StDataReadModule::~StDataReadModule()
{
   //
   delete fL3DataProvider;   fL3DataProvider = 0; 
   delete fEventTracker;     fEventTracker = 0;
   delete fL3p;              fL3p = 0;
   delete fBemcOnlineStatus; fBemcOnlineStatus = 0;
   delete fTracks;           fTracks = 0;
   delete fHits;             fHits = 0;
   if (fEventPoolReader) {
      DeletePool(); 
      fEventPoolReader = 0;
   }
   delete fEventDisplay; fEventDisplay = 0;
}
//_____________________________________________________________________________
StuDraw3DEvent  *StDataReadModule::Display()
{
   if (!fEventDisplay)  fEventDisplay = new StuDraw3DEvent("TPC,StarLogo,StarFloor,StarBeam");
   return fEventDisplay;
}

//_____________________________________________________________________________
void  StDataReadModule::Clear(Option_t * option)
{
   ClearTracks(option);
   ClearHits(option);
   if (fSizeProvider)  { fSizeProvider ->ResetAvailable(); fSizeProvider->ResetCounter();  }
   if (fColorProvider) { fColorProvider->ResetAvailable(); fColorProvider->ResetCounter(); }
   TModule::Clear();
   if (fEventDisplay) fEventDisplay->Clear();
 }
//_____________________________________________________________________________
void  StDataReadModule::ClearHits(Option_t * /*option*/)
{
   
   if (fHits) {
      //  Clean it
      delete fHits; fHits = 0;
   }
}
//_____________________________________________________________________________
void  StDataReadModule::ClearTracks(Option_t * /*option*/)
{
     //  Clean it
    delete fTracks; fTracks = 0;
}
//_____________________________________________________________________________
void  StDataReadModule::DeletePool()
{
   // Delete the current event pool reader
   if (fEventPoolReader) {
      StopEvents();
      fNeedRecreate = kTRUE;
   }
}
//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StDataReadModule::Init(){

    // Allocate the l3 buffer
    fL3p  = new L3_P[GL3_TRACK_MEM_SIZE];

    fL3DataProvider = new Gl3Data(0,0); fL3DataProvider->Init();
    fSizeProvider   = new TEmcSizeProvider(&fBtow,(void **)&fEmc_in, &fLengthBtow);
    fColorProvider  = new TEmcColorProvider (&fBtow,(void **)&fEmc_in, &fLengthBtow);
    ((TEmcSizeProvider *)fSizeProvider)->SetOnlineStatus(fBemcOnlineStatus);
    ((TEmcColorProvider *)fColorProvider)->SetOnlineStatus(fBemcOnlineStatus);

  // Create tables  

  // Create Histograms    

   return TModule::Init();

}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StDataReadModule::Make()
{
   int retStatus = kStOK;

   if (!fEventPoolReader )   retStatus = NextFile();

   if ( retStatus == kStOk ) retStatus = NextEvent();

   if ( fDemo && retStatus == kStEOF ) retStatus = NextFile();

   if ( retStatus == kStOk )  retStatus = MakeEvent();

   return retStatus;
}
//_____________________________________________________________________________
/// MakeEvent - Create a emc hits
Int_t StDataReadModule::MakeEmcHits()
{
  if (fEmcHitsOn && fEmcDataLength > 0) {
     for ( int i=1; i<=4800; i++ ) { 
        Int_t colorAttribute = (fColorProvider) ? fColorProvider->NextAttribute() : -1;
        Float_t dr = (fSizeProvider) ?  fSizeProvider->NextAttribute() : 0;
#ifndef CONTROLROOM    
       dr /= 4;
#endif 
       if (dr > 0.1) {
          Display()->EmcHit(i,colorAttribute,4070,dr);
          Display()->SetComment(fSizeProvider->GetObjectInfo(i,0));
       }
     }
  }
  return kStOk;
 
}

//_____________________________________________________________________________
/// MakeEvent - Create a next good event if any
Int_t StDataReadModule::MakeEvent()
{
   fEmcDataLength = 0;
   const char *readerName = "L3";
   int counter = 0;
   //
   //   Read emc hits
   //
    if (fEmcHitsOn)
    {
       readerName = "Emc";
       fEmcDataLength = EmcReader(); 
       counter +=  fEmcDataLength >0 ? fEmcDataLength : 0;
  	    fprintf(stderr,"%s:  %d bytes...\n", readerName, counter);
       MakeEmcHits();
    }

   //
   //   Read l3 tracks 
   //
   if (fL3TracksOn) {
      readerName = "L3";
      int l3Counter = 0;
      if (L3Reader() == kStOk) {
          l3Counter = MakeTracks();
      }

      // Check status
      if (l3Counter == 0) 
          fprintf(stderr,"%s: not present...\n", readerName);
      else if (l3Counter   < 0) 
          fprintf(stderr,"%s: problems in data (%d)...\n", readerName,l3Counter);
      else {
         fprintf(stderr,"%s:  %d bytes...\n", readerName, l3Counter);
         counter += l3Counter;
      }
    }
    if (fL3HitsOn) {
      readerName = "tpc";
      int l3HitsCount = MakeTpcHits();
      if (l3HitsCount == 0 ) 
                 fprintf(stderr,"%s: hits were not present...\n", readerName);
      else if (l3HitsCount > 0 ) 
                 fprintf(stderr,"%s: %d hits were rendered...\n", readerName,l3HitsCount );
  
      counter += l3HitsCount;
    }
    return  ( counter > 0) ? kStOK : kStErr;
}
//_____________________________________________________________________________
/// NextEvent - this method is called open the next daq file if any
Int_t StDataReadModule::NextEvent()
{
   // Create the next event from evp data
   int retStatus=kStErr;
   fEventPoolReader->NextEvent(); 
   retStatus = fRecordReady = fEventPoolReader->EventStatus();
   if ( retStatus  == kStOk ) {
      daqReader *currentData =  fDataP = fEventPoolReader->GetReader(); 

//     fprintf(stderr," StDataReadModule::NextEvent - evpReader = %x, event # = %d event type %d; mem %p\n",fDataP, fEventNumber,fEventType
//           ,((evpReader*)fDataP)->mem);
       if(currentData && fRecordReady) {
          retStatus=kStOk;    	// event  valid
          StEvtHddr *eventHeader =  GetEvtHddr();
          daqReader *reader = fEventPoolReader->GetReader();
          eventHeader->SetEventNumber(reader->event_number);
          eventHeader->SetIventNumber(reader->seq         );
          eventHeader->SetEventSize  (reader->bytes       );
          eventHeader->SetTriggerMask(reader->daqbits     );
          eventHeader->SetRunNumber  (reader->run         );
          eventHeader->SetGMTime     (reader->evt_time    );   
          if (fSizeProvider) 
              ((TEmcSizeProvider *)fSizeProvider)->ResetEmcDecoder(eventHeader->GetDate(),eventHeader->GetTime());
          if (fColorProvider) 
              ((TEmcColorProvider *)fColorProvider)->ResetEmcDecoder(eventHeader->GetDate(),eventHeader->GetTime());
       }
    }
    return retStatus;
}

      
//_____________________________________________________________________________
/// NextFile - this method is called open the next daq file if any
Int_t StDataReadModule::NextFile()
{
   int retStatus = kStOK;
   if (! fEventPoolReader) {
      fEventPoolReader = fDaqFileName.IsNull() ?
                 new StEvpReader()
            :
                 new StEvpReader(fDaqFileName,fMountPoint);
      LOG_DEBUG << " new StEvpReaderThread to be started with "
                << fDaqFileName << endm;
      LOG_DEBUG << " new StEvpReaderThread has been started" << endm;
   } else if (fNeedRecreate || true ) {
      fEventPoolReader->RestartReader(fDaqFileName,fMountPoint);
      LOG_DEBUG << " existent StEvpReaderThread recreated"  << fDaqFileName << fMountPoint << endm;
      fNeedRecreate = kFALSE;
   }
   if(!fEventPoolReader->GetReader()) {
      LOG_DEBUG  << "Error initializing reader for file " << fDaqFileName << endm;
      retStatus = kStErr;
   }
   return retStatus;
}

//_____________________________________________________________________________
void StDataReadModule::SetDaqFileName(const char *fileName)
{
   fDaqFileName = fileName;
   if (!fDaqFileName.IsNull()) fLastGoodDaqFileName = fDaqFileName;
   DeletePool();
}
//_____________________________________________________________________________
void StDataReadModule::SetMountPoint(const char *mountPoint)
{
  fMountPoint = mountPoint;
}

//______________________________________________________________________________
//
//    The RTS/src/EventDisplay event builder 
//_____________________________________________________________________________
int StDataReadModule::L3Reader()
{
   int retStatus = -1;
   if (fDataP) {
      fL3DataProvider->ReadNextEvent(fDataP, (char *)fDataP);
      retStatus =  kStOk;
 //     retStatus += MakeTpcHits();
  }
  return retStatus;
}

//_____________________________________________________________________________
int StDataReadModule::MakeTpcHits()
{ 
   UInt_t nHits = 0; 
   
   vector<float> &hittxyz = fHittxyz;
   hittxyz.clear();
   hittxyz.reserve(TPC_READER_MAX_CLUSTERS);
   daqReader *reader = fDataP; // *fEventPoolReader;

   for(int sector=0;sector<24;sector++) {
      if ( tpcReader(reader,  sector) && tpc.has_clusters ) {
      for(int row=0;row<45;row++) {
        for(int j=0;j<tpc.cl_counts[row];j++) {
           float x,y,z;
           tpc_cl *c = &tpc.cl[row][j];
           int time = int(c->t);
           tpc_hit_postion(sector+1,row+1,*c, x, y,z);
           if (time) {
              // skip next hit
              hittxyz.push_back(x); hittxyz.push_back(y); hittxyz.push_back(z);
           }
       }
     }}
  }
  nHits = hittxyz.size()/3;
  if (nHits) {
     Display()->Points(fHittxyz, 17,8,0.12);
     TString info = Form("<p><b>Total TPC hits: </b>%d",nHits );
     Display()->SetComment(info.Data()) ;
  }

  return nHits;
}

//_____________________________________________________________________________
int StDataReadModule::MakeTracks()
{    
   int retStatus = kStOK;
    UInt_t nTrack =  fL3DataProvider->GetNTracks();
   if ( (nTrack >0) &&  fL3TracksOn ) {
       retStatus = nTrack;
       
       Distribution(fL3DataProvider,nTrack);

       gl3Track *track =  fL3DataProvider->getFirstTrack();

       for (UInt_t i = 0; i < nTrack; i++, track =  fL3DataProvider->getNextTrack() ){
          fTrackXYZ.clear();
          Color_t trackColor = kRed;
            if (trackColor > 0) {
                 
//see: StEvent/StHelixModel 
              const float pi2 = 3.1415926/2.;
// Global tracks:

// see: L3/L3Formants.h 

// You need this stuff for calculating the helix
  
               Float_t angle  = track->phi0;
               Float_t ro     = track->r0;
               double x0      = ro*cos(angle);
               double y0      = ro*sin(angle);
               double z0      = track->z0;
               StThreeVectorD vector(x0,y0,z0);
   
   
               const double c = 0.3*track->q*fBField;
               double kapa    = TMath::Abs((c )/track->pt);
               double lambda  = atan(track->tanl);
               int  h         = c > 0 ? -1 : 1;
               double psi     = track->psi;

               StHelix helix (kapa, lambda, psi-h*pi2, vector, h);
               Float_t      len = track->length;
               
               if (fEmcHitsOn && (fEmcDataLength > 0) &&  (track->outerMostRow >= 44) && (len > 50) ) 
               {       len += 70;                                                                  }
               
               Int_t nSteps = Int_t(28*len*kapa + 1); 
               Float_t step = len / nSteps;

               StHelix3DPoints tracksPoints(&helix,step,nSteps,kFALSE);
               LOG_INFO << i << ": " <<  helix << ": Len="<< len << ": nSteps="<< nSteps  << endm; 
               for (Int_t j=0; j<= nSteps; j++ ) {
                  fTrackXYZ.push_back(tracksPoints.GetX(j));
                  fTrackXYZ.push_back(tracksPoints.GetY(j));
                  fTrackXYZ.push_back(tracksPoints.GetZ(j));
                        
               }
               LOG_INFO <<" Color" << MakeColor(track->dedx) << ": "<<  fTrackXYZ.size() 
                         << " de/dx=" << track->dedx << " pT="  << track->pt << endm; 
               Display()->Line(fTrackXYZ,MakeColor(track->dedx));
               Display()->SetComment(Form("<p><b>De/Dx=</b>%f<br><b>pT=   </b>%f",track->dedx,track->pt));
           }
     }
  }
  return retStatus;
}

//_____________________________________________________________________________
int StDataReadModule::EmcReader()
{    
   int retStatus = -1;
   if (fDataP) {
//      retStatus = emcReader((char *)fDataP) ;
      fEmc_in = fDataP->det("btow")->get("adc");
      if (fEmc_in) { 
         // btow_t*
         fBtow = (fLengthBtow = fEmc_in->iterate()) ? (btow_t*)fEmc_in->Void : 0;     
         fSizeProvider->ComputerScale(); 
         retStatus = 1;
      }
   }
   return retStatus;
}

//________________________________________________________________________________
Int_t StDataReadModule::MakeColor(Double_t energy)
{
   int  STFLowColor  = 51; 
   int STFHighColor = 100;
   double STFLowEnergy  = 0;
   double STFHighEnergy = 0.000005;
   int STFColor = 0;
   if ( energy>STFHighEnergy )  
      STFColor = STFHighColor;
   else if ( energy<STFLowEnergy ) 
      STFColor = STFLowColor;
   else  {
       STFColor = STFLowColor
                  + int((STFHighColor-STFLowColor)
                   *fDeLookUp[int((energy - STFLowEnergy)*flookFactor)]);
 }
 return STFColor;
}
//_____________________________________________________________________________
void StDataReadModule::Distribution(global_track *tracks,UInt_t nTracks) 
{
 // Calculate De/Dx distribution

   Float_t low, high;
   low  = 0;
   high = 0.000005;

   TH1F de("__de__","dedx",fNbins,low,high);
   de.SetDirectory(0);
   if (nTracks > 0) {
      for (UInt_t i = 0; i < nTracks; i++ ){
          de.Fill(tracks[i].dedx);
      }
   }
   Double_t  s = de.ComputeIntegral();
   flookFactor = 1./de.GetBinWidth(1);
   Int_t hSize = fNbins;
   if (s > 0) {
      Double_t *f = de.GetIntegral();
      for (hSize = 0; hSize < fNbins; hSize++) {
        fDeLookUp[hSize] = f[hSize]/s;
        if ( fDeLookUp[hSize] > 0.995) break; // Look for the rightmost non-zero bin
      }
   }
   de.SetBit(kMustCleanup,0);   
}
//_____________________________________________________________________________
void StDataReadModule::Distribution(Gl3Data *tracks,UInt_t nTracks) 
{
 // Calculate De/Dx distribution

   Float_t low, high;
   low  = 0;
   high = 0.000005;

   TH1F de("__de__","dedx",fNbins,low,high);
   de.SetDirectory(0);
   if (nTracks > 0) {
      gl3Track *track= tracks->getFirstTrack();
      for (UInt_t i = 0; i < nTracks; i++, track= tracks->getNextTrack())
      {
          de.Fill(track->dedx);
      }
   }
   Double_t  s = de.ComputeIntegral();
   flookFactor = 1./de.GetBinWidth(1);
   Int_t hSize = fNbins;
   if (s > 0) {
      Double_t *f = de.GetIntegral();
      for (hSize = 0; hSize < fNbins; hSize++) {
        fDeLookUp[hSize] = f[hSize]/s;
        if ( fDeLookUp[hSize] > 0.995) break; // Look for the rightmost non-zero bin
      }
   }
   de.SetBit(kMustCleanup,0);   
}
//_____________________________________________________________________________
Int_t       StDataReadModule::Remake()
{
   // Remake the current event (for example due new magnetic field re-setting)
   Clear();
   return MakeEvent(); // L3Reader();
}

//_____________________________________________________________________________
void StDataReadModule::SetGuiObject(QObject *gui)
{
   fGuiObject = gui;
}
//_____________________________________________________________________________
void  StDataReadModule::NextEventsSlot(int interval)
{
  NextFile();
  LOG_DEBUG << " StDataReadModule::NextEventsSlot (" << interval << ")" << endm;
  if (fEventPoolReader) fEventPoolReader->NextEvent(); // (interval);
}

//_____________________________________________________________________________
void  StDataReadModule::StopEvents()
{
  if (fEventPoolReader) { 
     assert (0); // fEventPoolReader->StopEventsSlot();
  }
}

//_____________________________________________________________________________
void  StDataReadModule::SetEventNumber(int eventNumber2beRead)
{
   fEventNumber = eventNumber2beRead;
   if (fEventPoolReader) fEventPoolReader->SetEventNumber(fEventNumber);
}

