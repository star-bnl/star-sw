/***************************************************************************
 *
 * $Id: StFgtQaAdcVsChannel.cxx,v 1.2 2012/01/31 16:48:34 wwitzke Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaAdcVsChannel.cxx,v $
 * Revision 1.2  2012/01/31 16:48:34  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.13  2012/01/30 10:42:23  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.12  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.11  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.10  2011/10/04 18:45:22  sgliske
 * made cut on short events optional
 *
 * Revision 1.9  2011/09/30 19:05:31  sgliske
 * general update
 *
 * Revision 1.8  2011/09/29 18:39:21  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.7  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 * Revision 1.6  2011/09/27 15:28:17  sgliske
 * added common StFgtQaMaker parent
 *
 * Revision 1.5  2011/09/27 00:49:01  sgliske
 * cosmic QA update
 *
 * Revision 1.4  2011/09/26 16:55:53  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.3  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.2  2011/09/22 21:21:35  sgliske
 * working on adding in ped. subtr.
 *
 * Revision 1.1  2011/09/21 20:26:46  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#include "StFgtQaAdcVsChannel.h"

#include <string>
#include <TH2F.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "StMaker.h"

#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"

#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"


// constructors
StFgtQaAdcVsChannel::StFgtQaAdcVsChannel( const Char_t* name,
                                          Short_t discId,
                                          Short_t quadId,
                                          const Char_t* quadName ) :
   StFgtQaMaker( name, discId, quadId, quadName ),
   mPath(""), mFileNameBase(""), mFileNameKey( quadName ), mHist(0) {

   // few defaults
   mXbins = 1280;
   mXmin = 0;
   mXmax = mXbins;

   mYbins = 4096;
   mYmin = 0;
   mYmax = mYbins;
};

StFgtQaAdcVsChannel::StFgtQaAdcVsChannel(const StFgtQaAdcVsChannel&){
   std::cerr << "TODO" << endl;
   throw 0;
};

// deconstructor
StFgtQaAdcVsChannel::~StFgtQaAdcVsChannel(){
   if( mHist )
      delete mHist;
};

// equals operator
StFgtQaAdcVsChannel& StFgtQaAdcVsChannel::operator=(const StFgtQaAdcVsChannel&){
   std::cerr << "TODO" << endl;
   throw 0;
};

Int_t StFgtQaAdcVsChannel::Init(){
   Int_t ierr = StFgtQaMaker::Init();

   if( !ierr ){
      // Take care of the histogram

      if( mHist )
         delete mHist;

      std::string name, title;
      {
         std::stringstream ss;
         ss << GetName() << "_" << mDiscId << "_" << mQuadId << "_" << mDoVsStrip << mDoSubtrPeds;
         ss >> name;
      };

      // easiest way to ensure stream is cleared is construct a new one
      std::stringstream ss;

      if( mDoVsStrip == 'r' )
         ss << "r-strips, cham. 1: ";
      else if( mDoVsStrip == 'R' )
         ss << "r-strips, cham. 2: ";
      else if( mDoVsStrip == 'P' )
         ss << "#phi-strips: ";

      ss << "ADC ";
      if( mDoSubtrPeds )
         ss << "(ped sub) ";
      if( mDoVsStrip == 'R' || mDoVsStrip == 'r' || mDoVsStrip == 'P' )
         ss << "vs. Position";
      else
         ss << "vs. Channel";

      ss << ", Quad " << mQuadName;

      if( mDoVsStrip == 'R' || mDoVsStrip == 'r' )
         ss << "; r [cm]";
      else if( mDoVsStrip == 'P' )
         ss << "; #phi [rad]";
      else
         ss << "; 128x(APV Num) + Channel Id.";

      ss << "; ADC Value";
      title = ss.str();

      //cout << "title = " << title << endl;

      mXmin = 0;
      mXmax = 1280;
      mXbins = 1280;
      if( mDoVsStrip == 'r' || mDoVsStrip == 'R' ){
         mXmin = 10;
         mXmax = 40;
         mXbins = 280;
      } else if ( mDoVsStrip == 'P' ){
         mXmin = 0;
         mXmax = 6.28/4;
         mXbins = 720;
      };

      mHist = new TH2F( name.data(), title.data(), mXbins/mBinFactorX,  mXmin, mXmax, mYbins/mBinFactorY, mYmin, mYmax );
   };

   return ierr;
};

Int_t StFgtQaAdcVsChannel::Make(){
   Int_t ierr = StFgtQaMaker::Make();

   StFgtStripCollection *stripCollectionPtr = 0;
   if( !ierr ){
      stripCollectionPtr = mFgtCollectionPtr->getStripCollection( mDiscId );
   };

   if( stripCollectionPtr ){

      const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
      StSPtrVecFgtStripConstIterator stripIter;

      for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
         Int_t geoId = (*stripIter)->getGeoId();
         Short_t adc = (*stripIter)->getAdc( mTimeBin );

         // to store position data
         Short_t disc, quad; //, strip;
         Char_t layer;

         Double_t pos, high, low;

         //StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );
         StFgtGeom::getPhysicalCoordinate( geoId, disc, quad, layer, pos, high, low );

         if( disc == mDiscId && quad == mQuadId ){
            Float_t bin = 0;

            Bool_t pass = 0;
            if( mDoVsStrip == 'R' || mDoVsStrip == 'r' ){
               pass = ( layer == 'R' );
               pass &= ( mDoVsStrip == 'R' ? (low > 0.785) : (low < 0.785) );
               bin = pos;
            } else if( mDoVsStrip == 'P' ){
               pass = ( layer == 'P' );
               bin = pos;
            } else {
               pass = 1;

               Int_t rdo, arm, apv, channel;
               StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId(geoId,rdo,arm,apv,channel);
               bin = 128*(apv%12) + channel;
            };

            //cerr << "QA mDoSubtrPeds = " << mDoSubtrPeds << endl;
            //cout << geoId << " adc = " << adc << endl;
            if( mDoSubtrPeds && pass ){
               pass = 0;
               Float_t ped = 0, err = 0;

               mPedReader->getPed( geoId, mTimeBin, ped, err );
               adc -= ped;
               pass = (adc > mPedThres*err );
            };

            if( pass )
               mHist->Fill( bin, adc );
         };
      };
   };

   return ierr;
};

// make the histogram pretty and save it to a file, if given a filename base
Int_t StFgtQaAdcVsChannel::Finish(){
   //cout << "StFgtQaAdcVsChannel::Finish()" << endl;

   if( !mFileNameBase.empty() ){

      // filename
      std::string filename;
      if( !mPath.empty() )
         filename = mPath + "/";
      filename += mFileNameBase;
      if( !mFileNameKey.empty() )
         filename += std::string( "." ) + mFileNameKey;
      filename += ".eps";

      // draw
      gROOT->SetStyle("Plain");
      TCanvas *can = new TCanvas( "fgtQAcan", "fgtQAcan", 900, 400 );

      gStyle->SetOptStat(0);
      gStyle->SetPalette(1);
      mHist->Draw("COLZ");

      can->Print( filename.data() );

      delete can;
   };

   return kStOk;
};

ClassImp(StFgtQaAdcVsChannel);
