/***************************************************************************
 *
 * $Id: StSvtBarrel.hh,v 1.1 2004/02/06 02:30:33 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtBarrel.hh,v $
 * Revision 1.1  2004/02/06 02:30:33  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#ifndef STSVTBARREL_H
#define STSVTBARREL_H

class TList; 

class StSvtBarrel : public TObject
{
 
 public:
  
  StSvtBarrel(Int_t id);
  ~StSvtBarrel();
 
  void DrawLadder(Int_t LadderNumber);
  void UpdateLadderButtons(Int_t LadderNumber, Int_t WaferNumber, Int_t HybridNumber); 

 private:
  
  Int_t       mBarrelNumber ;     //!
  Char_t*     mBarrelName ;       //!
  
  TList*      mLadderList ;       //! 
  Float_t*    mLadderPositions;   //!

  ClassDef(StSvtBarrel,1)
    
};
    
#endif
