/*!
 * \class StFgtQaMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaMaker.cxx,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaMaker.cxx,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.4  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.3  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.2  2011/09/28 17:48:50  sgliske
 * minor updates
 *
 * Revision 1.1  2011/09/27 15:28:17  sgliske
 * added common StFgtQaMaker parent
 *
 *
 **************************************************************************/

#include "StFgtQaMaker.h"

#include <string>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "StMaker.h"

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"

// constructors
StFgtQaMaker::StFgtQaMaker( const Char_t* name,
                            Short_t discId,
                            Short_t quadId,
                            const Char_t* quadName ) :
   StMaker( name ), mFgtCollectionPtr(0), mDiscId( discId ), mQuadId( quadId ), mTimeBin( 4 ),
   mDoVsStrip( 'c' ), mDoSubtrPeds( 0 ),
   mXbins( 1280 ), mYbins( 4096 ), mXmin( 0 ), mXmax( 1280 ), mYmin( 0 ), mYmax( mYbins ),
   mBinFactorX( 1 ), mBinFactorY( 1 ), mQuadName( quadName ), mPedReader(0), mPedThres(-1e100) {
   // set the style to plain
   gROOT->SetStyle("Plain");
};

StFgtQaMaker::StFgtQaMaker(const StFgtQaMaker&){
   std::cerr << "TODO" << endl;
   throw 0;
};

// deconstructor
StFgtQaMaker::~StFgtQaMaker(){
   if( mPedReader )
      delete mPedReader;
};

// equals operator
StFgtQaMaker& StFgtQaMaker::operator=(const StFgtQaMaker&){
   std::cerr << "TODO" << endl;
   throw 0;
};

Int_t StFgtQaMaker::Init(){
   Int_t ierr = kStOk;

   if( mDoSubtrPeds && mPedFile.empty() ){
      std::cout << "Cannot subtract peds--database not yet implemented" << endl;
      ierr = kStFatal;
   };

   // now the ped reader, if needed
   if( !ierr && mDoSubtrPeds ){
      //std::cout << "Constructing ped reader" << std::endl;
      mPedReader = new StFgtPedReader( mPedFile.data() );
      ierr = mPedReader->Init();
   };

   return ierr;
};

Int_t StFgtQaMaker::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   mFgtCollectionPtr = 0;

   if( eventPtr ) {
      mFgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !mFgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   return ierr;
};

ClassImp(StFgtQaMaker);
