//$Id: estimateVertexZ.cc,v 1.8 2000/08/24 19:59:08 wdeng Exp $
//$Log: estimateVertexZ.cc,v $
//Revision 1.8  2000/08/24 19:59:08  wdeng
//Initialize both index variables to zero.
//
//Revision 1.7  2000/08/24 00:56:18  wdeng
//Add protection.
//
//Revision 1.6  2000/08/23 22:06:50  wdeng
//Rewrite. Improve the speed by a factor of 3. This introduces a dependence: tphit table must be pre-sorted.
//
#include <stdlib.h>
#include "TH1.h"
#include "TTable.h"
#include "tables/St_tcl_tphit_Table.h"

void estimateVertexZ(St_tcl_tphit *tphit, Float_t& vertexZ, Float_t& relativeHeight) 
{  
  TH1F*  vertexZHistogram = new TH1F("vertex_z","estimated_z_distribution",4000,-200,200); 

  Int_t hitNumber = tphit->GetNRows();
  tcl_tphit_st* tphitT = tphit->GetTable();
  tcl_tphit_st* tphitPtr = tphitT;

  Float_t* radius2D = new Float_t[hitNumber];
  for( Int_t i=0; i<hitNumber; i++,tphitPtr++) {
    Float_t hitX = tphitPtr->x;     
    Float_t hitY = tphitPtr->y;     
    radius2D[i] = sqrt( hitX*hitX + hitY*hitY );
  }
  
  tphitPtr = tphitT;
  Int_t indexTableI=0; // index for the inner hit
  Int_t indexTableO=0; // index for the outer hit
  
  for(Int_t i=0; i<hitNumber; i++, tphitPtr++) {
    if( tphitPtr->row==114 ) { indexTableI=i; break; }
  }
  
  for(Int_t sector=1; sector<=24; sector++) {
    
    for(Int_t padrow=14; padrow<=40; padrow+=2) {
      Short_t sectorPadrow = sector*100 + padrow;
      
      while(1) {
	if( indexTableI==hitNumber ) break;
	Short_t rowI = tphitT[indexTableI++].row;

	if( rowI>sectorPadrow ) break; 
	if( sectorPadrow == rowI ) {
	  Float_t innerX = tphitT[indexTableI].x;
	  Float_t innerY = tphitT[indexTableI].y;
	  Float_t innerZ = tphitT[indexTableI].z;
	  Float_t innerR = radius2D[indexTableI];
	  
	  indexTableO = indexTableI;
	  for(Int_t nextPadrow=padrow+3; nextPadrow<=43; nextPadrow+=2) {
	    Short_t sectorNextPadrow = sector*100 + nextPadrow;
	    
	    while(1) {
	      if( indexTableO==hitNumber ) break;
	      Short_t rowO = tphitT[indexTableO++].row;
	      
	      if( rowO>sectorNextPadrow ) break;
	      if( sectorNextPadrow == rowO ) {
		Float_t outerX = tphitT[indexTableO].x;
		Float_t outerY = tphitT[indexTableO].y;
		Float_t outerZ = tphitT[indexTableO].z;
		
		Float_t deltaX = outerX - innerX;
		Float_t deltaY = outerY - innerY;
		
		Float_t distanceSquare = ( innerY * deltaX - innerX * deltaY )
		                       * ( innerY * deltaX - innerX * deltaY )
		                       / ( deltaY * deltaY + deltaX * deltaX ); 
		if( distanceSquare > 1225 ) continue;
		
		Float_t outerR = radius2D[indexTableO];
		Float_t deltaR = outerR - innerR;
		if( fabs(deltaR) < 0.000001 ) continue;
		
		Float_t z0 = (outerR*innerZ - innerR*outerZ)/ deltaR;
		//  if( fabs(z0)>200 ) continue;
		
		vertexZHistogram->Fill(z0);
	      }
	    }
	  }
	}
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
  //  vertexZHistogram->Draw();
  
  delete vertexZHistogram;
  delete radius2D;
}
