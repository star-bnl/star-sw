#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_itpcPadPlanesC.h"


tpcPadConfig_st *St_tpcPadConfigC::Struct(Int_t i)                        {return ((St_tpcPadConfig*) Table())->GetTable(i);}
UInt_t           St_tpcPadConfigC::getNumRows()                	          {return GetNRows();}
UChar_t          St_tpcPadConfigC::iTpc(Int_t sector)                     {UChar_t iTPC = Struct()->itpc[sector-1];  return iTPC;}
Int_t 	         St_tpcPadConfigC::padRows(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->padRows()               :St_itpcPadPlanesC::instance()->padRows();}
Int_t 	         St_tpcPadConfigC::innerPadRows(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerPadRows() 	   :St_itpcPadPlanesC::instance()->innerPadRows();}
Int_t 	         St_tpcPadConfigC::innerPadRows48(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerPadRows48()	   :St_itpcPadPlanesC::instance()->innerPadRows48();}
Int_t 	         St_tpcPadConfigC::innerPadRows52(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerPadRows52()	   :St_itpcPadPlanesC::instance()->innerPadRows52();}
Int_t 	         St_tpcPadConfigC::outerPadRows(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerPadRows() 	   :St_itpcPadPlanesC::instance()->outerPadRows();}
Int_t 	         St_tpcPadConfigC::superInnerPadRows(Int_t sector)        {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->superInnerPadRows()     :St_itpcPadPlanesC::instance()->superInnerPadRows();}
Int_t 	         St_tpcPadConfigC::superOuterPadRows(Int_t sector)        {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->superOuterPadRows()     :St_itpcPadPlanesC::instance()->superOuterPadRows();}
Double_t 	 St_tpcPadConfigC::innerSectorPadWidth(Int_t sector)      {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorPadWidth()   :St_itpcPadPlanesC::instance()->innerSectorPadWidth();}
Double_t 	 St_tpcPadConfigC::innerSectorPadLength(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorPadLength()  :St_itpcPadPlanesC::instance()->innerSectorPadLength();}
Double_t 	 St_tpcPadConfigC::innerSectorPadPitch(Int_t sector)      {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorPadPitch()   :St_itpcPadPlanesC::instance()->innerSectorPadPitch();}
Double_t 	 St_tpcPadConfigC::innerSectorRowPitch1(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorRowPitch1()  :St_itpcPadPlanesC::instance()->innerSectorRowPitch1();}
Double_t 	 St_tpcPadConfigC::innerSectorRowPitch2(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorRowPitch2()  :St_itpcPadPlanesC::instance()->innerSectorRowPitch2();}
Double_t 	 St_tpcPadConfigC::firstPadRow(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->firstPadRow() 	   :St_itpcPadPlanesC::instance()->firstPadRow();}
Double_t 	 St_tpcPadConfigC::firstOuterSectorPadRow(Int_t sector)   {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->firstOuterSectorPadRow():St_itpcPadPlanesC::instance()->firstOuterSectorPadRow();}
Double_t 	 St_tpcPadConfigC::lastOuterSectorPadRow(Int_t sector)    {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->lastOuterSectorPadRow() :St_itpcPadPlanesC::instance()->lastOuterSectorPadRow();}
Double_t 	 St_tpcPadConfigC::firstRowWidth(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->firstRowWidth()         :St_itpcPadPlanesC::instance()->firstRowWidth();}
Double_t 	 St_tpcPadConfigC::lastRowWidth(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->lastRowWidth()          :St_itpcPadPlanesC::instance()->lastRowWidth();}
Double_t 	 St_tpcPadConfigC::outerSectorPadWidth(Int_t sector)      {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorPadWidth()   :St_itpcPadPlanesC::instance()->outerSectorPadWidth();}
Double_t 	 St_tpcPadConfigC::outerSectorPadLength(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorPadLength()  :St_itpcPadPlanesC::instance()->outerSectorPadLength();}
Double_t 	 St_tpcPadConfigC::outerSectorPadPitch(Int_t sector)      {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorPadPitch()   :St_itpcPadPlanesC::instance()->outerSectorPadPitch();}
Double_t 	 St_tpcPadConfigC::outerSectorRowPitch(Int_t sector)      {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorRowPitch()   :St_itpcPadPlanesC::instance()->outerSectorRowPitch();}
Double_t 	 St_tpcPadConfigC::outerSectorLength(Int_t sector)        {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorLength()     :St_itpcPadPlanesC::instance()->outerSectorLength();}
Double_t 	 St_tpcPadConfigC::ioSectorSeparation(Int_t sector)       {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->ioSectorSeparation()    :St_itpcPadPlanesC::instance()->ioSectorSeparation();}
Double_t 	 St_tpcPadConfigC::innerSectorEdge(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorEdge()       :St_itpcPadPlanesC::instance()->innerSectorEdge();}
Double_t 	 St_tpcPadConfigC::outerSectorEdge(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorEdge() 	   :St_itpcPadPlanesC::instance()->outerSectorEdge();}
Double_t 	 St_tpcPadConfigC::innerSectorPadPlaneZ(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerSectorPadPlaneZ()  :St_itpcPadPlanesC::instance()->innerSectorPadPlaneZ();}
Double_t 	 St_tpcPadConfigC::outerSectorPadPlaneZ(Int_t sector)     {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerSectorPadPlaneZ()  :St_itpcPadPlanesC::instance()->outerSectorPadPlaneZ();}
Int_t* 	         St_tpcPadConfigC::innerPadsPerRow(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerPadsPerRow()       :St_itpcPadPlanesC::instance()->innerPadsPerRow();}
Int_t* 	         St_tpcPadConfigC::outerPadsPerRow(Int_t sector) 	  {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerPadsPerRow()       :St_tpcPadPlanesC::instance()->outerPadsPerRow();}
Int_t            St_tpcPadConfigC::padsPerRow(Int_t sector, Int_t row)    {
  Int_t Ninner = innerPadRows(sector);
  return (row <= Ninner) ?
    innerPadsPerRow(sector)[row-1] :
    outerPadsPerRow(sector)[row-1-Ninner];
}
Double_t* 	 St_tpcPadConfigC::innerRowRadii(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->innerRowRadii()         :St_itpcPadPlanesC::instance()->innerRowRadii();}
Double_t* 	 St_tpcPadConfigC::outerRowRadii(Int_t sector) 	          {return (! iTpc(sector)) ? St_tpcPadPlanesC::instance()->outerRowRadii() 	   :St_itpcPadPlanesC::instance()->outerRowRadii();}
// taken from StRItpcPadPlane
Int_t            St_tpcPadConfigC::numberOfRows(Int_t sector)             {return padRows(sector);}
Int_t            St_tpcPadConfigC::numberOfInnerRows(Int_t sector)        {return innerPadRows(sector);}
Int_t            St_tpcPadConfigC::numberOfInnerRows48(Int_t sector)      {return innerPadRows48(sector);}
Int_t            St_tpcPadConfigC::numberOfInnerRows52(Int_t sector)      {return innerPadRows52(sector);}
Int_t            St_tpcPadConfigC::numberOfOuterRows(Int_t sector)        {return outerPadRows(sector);}
Bool_t           St_tpcPadConfigC::isRowInRange(Int_t sector, Int_t row)  {return (row >= 1 && row<=numberOfRows(sector)) ? kTRUE: kFALSE;}
Double_t         St_tpcPadConfigC::radialDistanceAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector,row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=Ninner ) return innerRowRadii(sector)[row-1];
  else               return outerRowRadii(sector)[row-1-Ninner];
}
Int_t            St_tpcPadConfigC::numberOfPadsAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector, row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=Ninner ) return innerPadsPerRow(sector)[row-1];
  return outerPadsPerRow(sector)[row-1-Ninner];
}
Double_t         St_tpcPadConfigC::PadWidthAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector,row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=Ninner) return innerSectorPadWidth(sector);
  return outerSectorPadWidth(sector);
}
Double_t         St_tpcPadConfigC::PadLengthAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector,row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=Ninner) return innerSectorPadLength(sector);
  return outerSectorPadLength(sector);
}
Double_t         St_tpcPadConfigC::PadPitchAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector,row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=Ninner) return innerSectorPadPitch(sector);
  return outerSectorPadPitch(sector);
}
Double_t         St_tpcPadConfigC::RowPitchAtRow(Int_t sector, Int_t row)       {
  if (! isRowInRange(sector,row)) return 0;
  Int_t Ninner = innerPadRows(sector);
  if ( row<=numberOfInnerRows48(sector) ) return innerSectorRowPitch1(sector);
  else if (row>numberOfInnerRows48(sector)&&row<=Ninner) return innerSectorRowPitch2(sector);
  return outerSectorRowPitch(sector);
}
Int_t            St_tpcPadConfigC::indexForRowPad(Int_t sector, Int_t row, Int_t pad)       {
  if (pad >numberOfPadsAtRow(sector,row)) return -1;
  Int_t index = 0;
  Int_t Ninner = innerPadRows(sector);
  if (row>0 && row<=Ninner )             for (Int_t i=1;i<row;i++) index += numberOfPadsAtRow(sector,i);
  else
    if (row>Ninner&&row<=numberOfRows(sector)) for (Int_t i=Ninner+1;i<row;i++)  index += numberOfPadsAtRow(sector,i);
  index+=pad-1;
  return index;
}
