// $Id: StClusterDisplayMaker.h,v 1.1 2000/07/07 02:31:31 flierl Exp $
#ifndef STAR_StClusterDisplayMaker
#define STAR_StClusterDisplayMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                    StClusterDisplayMaker                             // 
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#include "TH2.h"
#endif
#include "TH2.h"
#include "TTree.h"
#include "TGraphErrors.h"

class StClusterDisplayMaker : public StMaker {

 private:
    // static Char_t  m_VersionCVS = "$Id: StClusterDisplayMaker.h,v 1.1 2000/07/07 02:31:31 flierl Exp $";
    TTree* mytree ;
    TH2S*  myhist ;
        
    Int_t sec ;
    Int_t row ;
    
    // l3 off
    TGraphErrors* graph_l3_off_points ;
    Float_t* l3offpoints_pad;
    Float_t* l3offpoints_time;
    

    // l3 on
    TGraphErrors* graph_l3_on_points ;
    Float_t* l3onpoints_pad;
    Float_t* l3onpoints_time;

    // off
    TGraphErrors* graph_off_points ;
    Float_t* offpoints_pad ;
    Float_t* offpoints_pad_err ;
    Float_t* offpoints_time ;
    Float_t* offpoints_time_err;

    // matcher
    TGraphErrors* matched_points ;
    Float_t* matched_points_pad ;
    Float_t* matched_points_pad_err ;
    Float_t* matched_points_time ;
    Float_t* matched_points_time_err;
    // not machted
    TGraphErrors* not_matched_points ;
    Float_t* not_matched_points_pad ;
    Float_t* not_matched_points_time ;

 protected:
 public: 
                  StClusterDisplayMaker(const char *name="ClusterDisplay");
   virtual       ~StClusterDisplayMaker();
   virtual Int_t  Init();
   virtual Int_t  Make(Int_t sec = 1, Int_t row = 1, Char_t* opt =""  ,Int_t pad_min = 1, Int_t pad_max =184, Int_t time_min = 0, Int_t time_max = 511);
   virtual Int_t Get_l3off_points();
   virtual Int_t Get_off_points();
   virtual Int_t Get_l3on_points();
   virtual Int_t Get_matched_points(Int_t&);

// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   // my member functions
   void Fill_Pixel_into_histo_outer() ;
   void Fill_Pixel_into_histo_inner() ;
   void Fill_Pixel_into_histo_artificial(TH2S* histo, Int_t row, Int_t sec) ;
   TTree* GetTree() {return mytree;};
   TH2S* GetHist() {return myhist;};
   TGraphErrors* Get_graph_l3_off_points() { return graph_l3_off_points;} ;
   TGraphErrors* Get_graph_off_points() { return graph_off_points;} ;
   TGraphErrors* Get_graph_l3_on_points() { return graph_l3_on_points;} ;
   TGraphErrors* Get_graph_matched_points() { return matched_points;} ;

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StClusterDisplayMaker.h,v 1.1 2000/07/07 02:31:31 flierl Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StClusterDisplayMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
