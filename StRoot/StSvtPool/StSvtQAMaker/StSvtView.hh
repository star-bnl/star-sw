/***************************************************************************
 *
 * $Id: StSvtView.hh,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtView.hh,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#ifndef STSVTVIEW_H
#define STSVTVIEW_H

class TDialogCanvas ;
class TPad;
class TButton;
class TOrdCollection;
class TPaveText;
class StSvtBarrel;
class StSvtLadder;

class StSvtView : public TObject {
  
 public:
  
  StSvtView(char *config, TDialogCanvas* GUIFrame=0);
  ~StSvtView();

  void DrawBarrels();  
  void DrawLadder(Int_t BarrelNumber=0, Int_t LadderNumber=0); 
  void ReadGeometry();
  void UpdateBarrelButtons(Int_t BarrelNumber, Int_t LadderNumber); 
  void UpdateLadderButtons(Int_t BarrelNumber, Int_t LadderNumber, Int_t WaferNumber, Int_t HybridNumber); 
  void SetInfoEvent(int event=-1, int buffer=-1);
  void SetInfoFile(const char* fileName);
  void SetInfoPedFile(const char* fileName);

 private:

  char* fConfig;     //!
  
  Float_t            mLadderPositionX[3][16] ;   //!     
  Float_t            mLadderPositionY[3][16] ;   //!     
  Float_t            mLadderPositionZ[3][16] ;   //!     

  StSvtBarrel*  mBarrel1 ;                  //!
  StSvtBarrel*  mBarrel2 ;                  //! 
  StSvtBarrel*  mBarrel3 ;                  //! 

  StSvtLadder*  mLadder ;                  //! 

  TDialogCanvas*     mDialogCanvas ;             //! 
  TPad*              mBarrelPad       ;          //!
  TPad*              mLadderPad ;                //!
  TPad*              mMonitorPad ;               //!
  TPad*              mInfoPad;                   //!

  TPaveText*         mFilePave;                  //!
  TPaveText*         mFilePedPave;               //!
  TPaveText*         mEventPave;                 //!
  TPaveText*         mEventStatPave;             //!

  Float_t*           mButtonSize ;               //! 
  Float_t            mWHRatio ;                  //!
 
  TOrdCollection*  buttons;              //!
  
  ClassDef(StSvtView,1)    
    
};
    
#endif
