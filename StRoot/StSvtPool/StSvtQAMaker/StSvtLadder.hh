/***************************************************************************
 *
 * $Id: StSvtLadder.hh,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Ladder
 *
 ***************************************************************************
 *
 * $Log: StSvtLadder.hh,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#ifndef STSVTLADDER_H
#define STSVTLADDER_H

class   TObjArray;

class StSvtLadder : public TObject
{
 
 public:

  StSvtLadder(Int_t BarrelNumber=0, Int_t ladderNumber=0);
  ~StSvtLadder();
  
  void DrawVertical();
  void DrawHorizontal(char* config);
  void UpdateButtons(Int_t WaferNumber, Int_t HybridNumber); 

 private:
  
  Int_t         mBarrelNumber ;     //!
  Int_t         mLadderNumber ;     //!
  TObjArray*    mSddArray ;         //! 
 
  ClassDef(StSvtLadder,1)
    
};
    
#endif
