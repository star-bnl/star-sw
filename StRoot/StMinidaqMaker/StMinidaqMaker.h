#ifndef STAR_StMinidaqMaker
#define STAR_StMinidaqMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMinidaqMaker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
//class St_stk_stkpar;
class St_type_structtbl;
class StMinidaqMaker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t  m_VersionCVS = "$Id: StMinidaqMaker.h,v 1.1 1999/02/11 23:40:22 sakrejda Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
// St_stk_stkpar *m_stk_stkpar;  //! pointer to stk parameters
St_type_structtbl *m_tpc_gain;   //! pointer to the gain table

 
 protected:
 public: 
   St_DataSet     *m_Params;     //! Params
                  StMinidaqMaker(const char *name="tpc_raw", const char *title="event/raw_data/tpc");
   virtual       ~StMinidaqMaker();
   virtual Int_t Init();
   virtual Int_t  Finish();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(StMinidaqMaker, 1)   //StAF chain virtual base class for Makers
};

#endif




