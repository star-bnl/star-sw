// $Id: estimateVertexZ.cc,v 1.1 2000/06/15 19:02:28 wdeng Exp $
// $Log: estimateVertexZ.cc,v $
// Revision 1.1  2000/06/15 19:02:28  wdeng
// Estimate the z position of primary vertex by using the straight line model in x-y and r-z planes.
//
#include <stdlib.h>

#include "TString.h"
#include "TH1.h"

#include "TTable.h"
#include "TTableSorter.h"
#include "TTableIter.h"

#include "tables/St_tcl_tphit_Table.h"

void estimateVertexZ(St_tcl_tphit *tphit, Float_t& vertexZ, 
                     Float_t& relativeHeight, Bool_t drawHistogram = kFALSE) 
{
  TH1F*  vertexZHistogram = new TH1F("vertex_z","estimated_z_distribution",4000,-200,200);

  TString sortBy("row"); 
  TTableSorter *tphitSorter = new TTableSorter(tphit,sortBy,0,tphit->GetNRows());
  
  tcl_tphit_st* tphitStart = tphit->GetTable();
  tcl_tphit_st* tphitPtr1;
  tcl_tphit_st* tphitPtr2;
  
  Int_t rowNumOuterHit=-1;
  Int_t rowNumInnerHit=-1;

  for(Int_t sector=1; sector<=24; sector++) {
    
    for(Int_t padrow=45; padrow>17; padrow-=2) {
      Int_t sectorPadrow = sector*100 + padrow;
      TTableIter nextOuterHit(tphitSorter,sectorPadrow);

      while( (rowNumOuterHit=nextOuterHit())>=0 ) {
        tphitPtr1 = tphitStart + rowNumOuterHit;
	Float_t outerX = tphitPtr1->x;
	Float_t outerY = tphitPtr1->y;
        Float_t outerZ = tphitPtr1->z;  
	Float_t outerR = sqrt( outerX * outerX + outerY * outerY );
        
        for(Int_t nextPadrow=padrow-3; nextPadrow>=14; nextPadrow-=2) {
          Int_t sectorNextPadrow = sector*100 + nextPadrow;
          TTableIter nextInnerHit(tphitSorter,sectorNextPadrow);
          
          while( (rowNumInnerHit=nextInnerHit())>=0)
            {
              tphitPtr2 = tphitStart + rowNumInnerHit;
              Float_t innerX = tphitPtr2->x;
              Float_t innerY = tphitPtr2->y;
              Float_t innerZ = tphitPtr2->z;

              Float_t deltaX = outerX - innerX;
              Float_t deltaY = outerY - innerY;

              Float_t distanceSquare = ( innerY * deltaX - innerX * deltaY )
                                     * ( innerY * deltaX - innerX * deltaY )
                                     / ( deltaY * deltaY + deltaX * deltaX ); 
              if( distanceSquare > 1225 ) continue;

              Float_t innerR = sqrt( innerX * innerX + innerY * innerY );
              Float_t deltaR = innerR - outerR;
              if( fabs(deltaR) < 0.000001 ) continue;

              Float_t z0 = (innerR*outerZ - outerR*innerZ)/ deltaR;
              
              vertexZHistogram->Fill(z0);
          }
          
        } // end of nextPadrow iteration
    }
  }
}

  Int_t   maximumBin = vertexZHistogram->GetMaximumBin();
  Float_t peakValue = vertexZHistogram->GetBinContent(maximumBin);

  Float_t backgroundSamples = 0;
  Int_t counter = 0;
  for(Int_t i=1; i<8; i++) {
    Int_t sampleBin = i*4000/8;

    if( abs(sampleBin-maximumBin) > 100 ) {    
      backgroundSamples += vertexZHistogram->GetBinContent(sampleBin);
      counter++;  
    }
  }

  Float_t background = backgroundSamples/counter;

  // output
  vertexZ =  vertexZHistogram->GetBinCenter(maximumBin);
  relativeHeight = peakValue/background;

  if( drawHistogram ) vertexZHistogram->Draw();
}
