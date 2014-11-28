// $Id: St_TLA_Maker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $

#ifndef STAR_St_TLA_Maker
#define STAR_St_TLA_Maker

/*!
 *                                                                     
 * \class  St_TLA_Maker
 * \author fisyak
 * \date   1998/07/20
 * \brief  virtual base class for Maker
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * St_TLA_Maker virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/St_TLA_Maker
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).

class St_TLA_Maker : public StMaker {
 private:
  // Private method declaration if any
 
 protected:
  // Protected method if any

 public: 
  St_TLA_Maker(const char *name="TLA");
  virtual       ~St_TLA_Maker();
  virtual Int_t Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St_TLA_Maker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St_TLA_Maker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St_TLA_Maker.h,v $
// Revision 1.16  2014/08/06 11:43:53  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.15  2003/09/10 19:47:43  perev
// ansi corrs
//
// Revision 1.14  2002/11/26 23:49:40  jeromel
// Small modif after Art's note ... doxygen issue + cleanup
//
// Revision 1.13  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
// Revision 1.11  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.10  1999/07/10 22:59:17  fine
// Some comments have been introduced to show html docs
//
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
