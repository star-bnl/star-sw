// $Id: St_l3Clufi_Maker.h,v 1.5 2000/04/12 18:39:21 flierl Exp $
// $Log: St_l3Clufi_Maker.h,v $
// Revision 1.5  2000/04/12 18:39:21  flierl
// check whether enough memory is allocated for the clusters ( for the tables and the buffers )
//
// Revision 1.4  2000/03/28 19:53:59  fine
// Adjuested to ROOT 2.24
//
// Revision 1.3  2000/02/24 01:55:25  flierl
// i960 timing built in.
// output just with debug option.
// pixelarray has now space for 512 timebuckets.
//
// Revision 1.2  1999/12/07 23:13:53  flierl
// histogramms created and filled
//
// Revision 1.1.1.1  1999/11/19 18:31:48  flierl
// test
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
#ifndef STAR_St_l3Clufi_Maker
#define STAR_St_l3Clufi_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_l3Clufi_Maker virtual base class for Maker                        //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "tables/St_pixelarray_Table.h"
#include "tables/St_hitarray_Table.h"

#include "St_DataSet.h"

//class St_stk_stkpar;
class St_l3Clufi_Maker : public StMaker {
 private:
    // static Char_t  m_VersionCVS = "$Id: St_l3Clufi_Maker.h,v 1.5 2000/04/12 18:39:21 flierl Exp $";
     
    //
    // l3 clusterfinding variables
    //
    St_DataSet *raw_data_tpc;  //! Raw data from tpc packed according to SN325
    St_DataSet *sector; //! Raw data of sector xx
    Int_t Max_number_of_rows;    //!  45
    Int_t Max_number_of_pads;    //! 184
    Int_t Max_number_of_buckets; //! 512
    St_pixelarray* Stpixel; //!
    pixelarray_st* pixelst; //!
    St_hitarray* St_hit_bank_this; //!
    hitarray_st* hit_bank_this_st; //!
    St_hitarray*   St_hit_bank[12];//!
    hitarray_st* hit_bank_array[12];//!

    // histos
    TH1F* x_dis;
    TH1F* y_dis; 
    TH1F* z_dis; 
    TH1F* charge_dis;
    TH1D* i960_time;
    
    // some constants
    Int_t Buffer_size ; // define max buffer_size

 protected:
 public: 
    //
    // l3 clusterfinding functions
    //
    Int_t Fill_pixel_of_inner_rows();
    Int_t Fill_pixel_of_outer_rows();
   

                  St_l3Clufi_Maker(const char *name="l3Clufi");
   virtual       ~St_l3Clufi_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_l3Clufi_Maker.h,v 1.5 2000/04/12 18:39:21 flierl Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_l3Clufi_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
