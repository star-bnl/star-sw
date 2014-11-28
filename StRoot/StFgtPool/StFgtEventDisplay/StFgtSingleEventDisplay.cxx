/***************************************************************************
 *
 * $Id: StFgtSingleEventDisplay.cxx
 * Author: P. Nord, Sept 2012
 *
 ***************************************************************************
 *
 * Description: Interface to display strips from the FGT within a standard 
 *              STAR event display.  No tracks or clusters are being drawn
 *              at this time.  The Make() function stores ADC values for each
 *              time bin.  Geometry and pedistal values from the database 
 *              will be loaded when needed by Make()
 *              Graphics are drawn by a COIN library.
 *
 ***************************************************************************
 *
 *
 *
 **************************************************************************/

#include "StFgtSingleEventDisplay.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

#include <string>
#include <TFile.h>
#include <TH2F.h>

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStripCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"

// constructors
StFgtSingleEventDisplay::StFgtSingleEventDisplay( const Char_t* name, const Char_t* fname ) : StMaker( name ) {
};


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void StFgtSingleEventDisplay::readFgtStripDb(){

  double pi=2.*acos(0.);
  double rad2deg=pi/180.;

   FgtStripDbItem *S;

   // set db pointer, if needed
   mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
   if( !mFgtDbMkr ){
     LOG_FATAL << "Error finding StFgtDbMaker" << endm;
     assert(!mFgtDbMkr);
   };

   //cout << "Got DB"<< endl;
   mDb = mFgtDbMkr->getDbTables();
   //cout << "Got DB"<< endl;


    int nTry=0, nMap=0;
    Int_t geoId;
    for (int rdo=1;rdo<=2;rdo++){// 2 RDOs numbered 1,2
      for (int arm=0;arm<6;arm++){//6 arms numbered from 0
        for (int apv=0;apv<=21;apv++){//24 APVs numbered 0-23 but in real life APV# 10,11,22,23 are unused so 0-19 in determining electronic Id
          if ((apv==10)||(apv==11)) continue;
          for (int channel=0;channel<128;channel++){//128 channels numbered from 0
          nTry++;
          geoId=mDb->getGeoIdFromElecCoord(rdo, arm, apv, channel);
          if (geoId<0) continue;
          std::string geoName=mDb->getGeoNameFromElecCoord(rdo,arm,apv,channel);
          //cout << numFgtElect << " ";
          S = &(stripDb[numFgtElect]);
          S->geoId = geoId;
          entryForGeoId[geoId] = numFgtElect++;  // local lookup table
          nMap++;
          Double_t ordinate,lowerSpan,upperSpan;
          StFgtGeom::decodeGeoId(S->geoId,S->disc,S->quad,S->layer,S->strip);
          StFgtGeom::getGlobalPhysicalCoordinate(geoName,S->disc,S->quad,S->layer,ordinate,  lowerSpan,  upperSpan);

          if (S->layer=='P') {
            S->phi1=S->phi2=ordinate;
            S->r1=lowerSpan;
            S->r2=upperSpan;
            S->x1=S->r1*cos(S->phi1);
            S->x2=S->r2*cos(S->phi2);
            S->y1=S->r1*sin(S->phi1);
            S->y2=S->r2*sin(S->phi2);
          } else {
            S->r1=S->r2=ordinate;
            S->phi1=lowerSpan;
            S->phi2=upperSpan;
            S->phi1Deg=S->phi1/rad2deg;
            S->phi2Deg=S->phi2/rad2deg;
          }
    //cout << " geoId=" << geoId << " " << geoName << " ";
    //cout << "d=" << S->disc << " lr=" << S->layer << " ";
    //cout << "q=" << S->quad << " sp=" << S->strip << " ";


          S->ped   = mDb->getPedestalFromElecCoord(rdo,arm,apv,channel);
          S->sigPed= mDb->getPedestalSigmaFromElecCoord(rdo,arm,apv,channel);
          S->stat  = mDb->getStatusFromElecCoord(rdo,arm,apv,channel);
          S->electId = StFgtGeom::getElectIdFromElecCoord(rdo,arm,apv,channel);
          S->Z = StFgtGeom::getDiscZ(S->disc);
    //cout << " r1=" << S->r1 ;
    //cout << " r2=" << S->r2 ;
    //cout << " x1=" << S->x1 ;
    //cout << " x2=" << S->x2 ;
    //cout << " y1=" << S->y1 ;
    //cout << " y2=" << S->y2 ;
    //cout << " p1=" << S->phi1 ;
    //cout << " p2=" << S->phi2 ;
    //cout << " Z=" << S->Z <<endl;

        }
      }
    }
  }
}


// deconstructor
StFgtSingleEventDisplay::~StFgtSingleEventDisplay(){
};

Int_t StFgtSingleEventDisplay::Init(){
   Int_t ierr = kStOk;

   mEventNum = 0;

   return ierr;
};

Int_t StFgtSingleEventDisplay::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   mFgtCollectionPtr = 0;

   if (mDb == NULL) readFgtStripDb(); // Lazy instantiation of the Db

   for(int j = 0; j < numFgtElect; j++) {
     stripDb[j].hit = 0;  // clear any previous hits
   }

   eventPtr = (StEvent*)GetInputDS("StEvent");
   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent in '" << ClassName() << "'" << endm;
      ierr = kStErr;
   } else {
      mFgtCollectionPtr=eventPtr->fgtCollection();

      if( !mFgtCollectionPtr) {
         LOG_ERROR << "Error getting pointer to StFgtCollection in '" << ClassName() << "'" << endm;
         ierr = kStErr;
      };
   };
   //printf("analysis\n");
   
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      //printf("disc = %d\n",disc);
      StFgtStripCollection *stripCollectionPtr = 0;
      if( !ierr ){
         stripCollectionPtr = mFgtCollectionPtr->getStripCollection( disc );
      };
      //printf("stripCollectionPtr = %d\n",stripCollectionPtr);

      if( stripCollectionPtr ){
         const StSPtrVecFgtStrip &stripVec = stripCollectionPtr->getStripVec();
         StSPtrVecFgtStripConstIterator stripIter;
         stripIter = stripVec.begin();
         //printf("stripVec = %d\n",stripVec);

         for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
           //printf("stripIter = %d\n",stripIter);
            Int_t geoId = (*stripIter)->getGeoId();
            //printf("geoId = %d\n",geoId);
            if(geoId < 0) continue; // strip is flagged for removal


            // to store position data
            Short_t disc, quad, strip;
            Char_t layer;
            StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );
            Int_t entry = getEntryForGeoId(geoId); 
            Short_t ped = stripDb[entry].ped;
	    Double_t pedDb=mDb->getPedestalFromGeoId(geoId);
	    Short_t statDb=mDb->getStatusFromGeoId(geoId);
	    //cout<<" Peds = "<<pedDb<<" Stats = "<<statDb<<endl;
            Short_t sigPed = stripDb[entry].sigPed;

            //if (stripDb[entry].stat != 0) {
              //printf("geoid %d stat %d\n", geoId, stripDb[entry].stat);
            //}
            //stripDb[entry].stat = stat; 

            for( Int_t tb = 0; tb < kFgtNumTimeBins; ++tb ){
               Short_t adc = (*stripIter)->getAdc(tb);

               // Note: r strips 0-279 and phi strips 0-359 are for
               // the half of the quadrant with local coordinate phi
               // in the range of pi/4 and pi/2.  Note: phi increases
               // for decreasing phi strip number.  r strips in
               // 400-679 and phi strips in 360-719 are for the local
               // coordinate phi in the range 0 to pi/4.  Short side
               // is for phi strips near 720, i.e. short side has
               // smaller local phi values than long side.
               Bool_t octIsShort = ( layer == 'R' ? (strip > 279) : (strip > 359) );

               //Int_t bin = ( quad*2 + octIsShort )*kFgtNumTimeBins + tb;

               stripDb[entry].adc[tb] = adc; 
               //printf("geoId=%d entry=%d timebin=%d ped=%d  adc=%d\n",geoId,entry,tb,ped,adc);

               if(adc > ped + 6 * sigPed) {
                 //printf("timebin = %d ped = %d  adc = %d\n",tb,ped,adc);
                 stripDb[entry].hit += 1<<tb; // encode the timebin into hit 
               }


            };
         };

      };
   };

   ++mEventNum;
   return ierr;
};

void StFgtSingleEventDisplay::drawStrip(Int_t geoId, Color_t lineColor) {
    Short_t disc;
    Short_t quad; 
    Char_t layer; 
    Int_t strip;
    Double_t x1; Double_t y1;
    Double_t x2; Double_t y2;
    Double_t z;


    Int_t entry = getEntryForGeoId(geoId);

    assert(entry >= 0); // error contition, bad geoId

    x1 = stripDb[entry].x1;
    y1 = stripDb[entry].y1;
    x2 = stripDb[entry].x2;
    y2 = stripDb[entry].y2;
    z = stripDb[entry].Z;

    disc = stripDb[entry].disc;
    quad = stripDb[entry].quad;
    layer = stripDb[entry].layer;
    strip = stripDb[entry].strip;
    Short_t ped = stripDb[entry].ped;
    Short_t sigPed = stripDb[entry].sigPed;
    Short_t maxAdc;
    maxAdc = 0;
    for (int l = 0; l < kFgtNumTimeBins-1; l++) {
      if(stripDb[entry].adc[l] > maxAdc) {
	maxAdc = stripDb[entry].adc[l];
      }
      //printf("tb %d adc %d maxAdc %d\n",l,stripDb[entry].adc[l], maxAdc);
    }

    Char_t stripName[256];
    sprintf(stripName,"FGT%d %d%c%c%d %d%c%d %d",geoId,disc,quad,layer,strip,ped,177/*ascii +-*/,sigPed,maxAdc);

    //lineColor = 3;

#define NUM_SEGMENTS 10

    float xyz[NUM_SEGMENTS][3];

    //cout << " geo=" << geoId ;
    //cout << " #=" << entry ;
    //cout << " l=" << disc ;
    //cout << " q=" << quad ;
    //cout << " x1=" << x1 ;
    //cout << " x2=" << x2 ;
    //cout << " y1=" << y1 ;
    //cout << " y2=" << y2 ;
    //cout << " Z=" << z <<endl;

    if( layer=='P') { //ppppppppppppppppppppp
      xyz[0][0] = x1;
      xyz[0][1] = y1;
      xyz[0][2] = z;
      xyz[1][0] = x2;
      xyz[1][1] = y2;
      xyz[1][2] = z ;
      gEventDisplay->Line(2, (float *) xyz, lineColor);
      gEventDisplay->SetComment(stripName);
      //printf("in drawStrip\n");
      ////numDrawn++;
    } // end of P-plane 


    if(  layer=='R'  ) { //RRRRRRRRRRRRRRRRRRRRRRR

      Double_t phiDiff;  
      Double_t mx1, my1, phi1, phi2, r1;
      r1   = stripDb[entry].r1;
      phi1 = stripDb[entry].phi1;
      phi2 = stripDb[entry].phi2;
      phiDiff = (phi2 - phi1)/ (NUM_SEGMENTS - 1.0);
      for (int j = 0; j < NUM_SEGMENTS; j++) {

        mx1=r1*cos(phi1 + phiDiff*j);
        my1=r1*sin(phi1 + phiDiff*j);

        xyz[j][0] = mx1;
        xyz[j][1] = my1;
        xyz[j][2] = z;
        //printf("%d %f %f %f %f %f\n", j, x1, y1, myz, mx1, mx2);
       }

      gEventDisplay->Line(NUM_SEGMENTS, (float*) xyz, lineColor );
      //gEventDisplay->Line(NUM_SEGMENTS, (float*) xyz, kPrimaryTrack);
      gEventDisplay->SetComment(stripName);
      //printf("in drawStrip\n");
      //gEventDisplay->SetComment(S->name);
      //numDrawn++;
      //printf("%d\n",numDrawn);

     } // end of R-plane 

}

Int_t StFgtSingleEventDisplay::Finish(){
   return kStOk;
};

ClassImp(StFgtSingleEventDisplay);
