// 
//  
//
#ifndef STAR_St_db_Maker
#define STAR_St_db_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_db_Maker virtual base class for Maker                          //
//                                                                      //
// This class is C++ implementation of the Begin_html <a href="http://www.rhic.bnl.gov/afs/rhic/star/doc/www/packages_l/pro/pams/db/sdb/doc/index.html">Simple Database Manager</a> End_html    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif 

#include "TDatime.h"

class St_FileSet;
class St_DataSet;
class TList;
class TBrowser;
class St_db_Maker : public StMaker {
 private:
               Bool_t drawinit;
               TObjArray  *m_DBList;        // List of the nodes containing the active validation tables
               TDatime     m_ValidTime;     // the start point (Date and time) of this validation period
               Int_t       m_ValidDate;     //
               Int_t       m_ValidHours;    //
               TString     m_RootDbDirectory; // The root directory for the calibrarion data
//  static Char_t m_VersionCVS = "$Id: St_db_Maker.h,v 1.1 1999/01/02 19:08:15 fisyak Exp $";
 protected:
 public: 
                   St_db_Maker(const char *name="db", const char *title="db", 
                                  const TString &rootdir="/afs/rhic/star/packages/dev/StDb");
   virtual        ~St_db_Maker();
   virtual void    Browse(TBrowser *b);
   virtual void    Clear(Option_t *option="");
   virtual TString GetDbDir(){ return m_RootDbDirectory;}
   virtual Int_t Init();
   virtual Int_t   Make();                                                               // *MENU*
   virtual void    PrintInfo();
   virtual void    SetDbDir(const TString &db="/afs/rhic/star/packages/dev/StDb"){ m_RootDbDirectory = db;}     // *MENU*
//   virtual void   SetValidTime(Int_t date, Int_t time){ m_ValidTime.Set(date-19000000, time); }
   virtual void    SetValidTime(Int_t date=19950101, Int_t time=194500) { m_ValidTime.Set(date-19000000, time); m_ValidDate = date; m_ValidHours = time; } // *MENU*

   ClassDef(St_db_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
