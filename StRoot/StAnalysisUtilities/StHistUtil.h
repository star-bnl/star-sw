// $Id: StHistUtil.h,v 2.20 2016/06/13 20:31:11 genevb Exp $
// $Log: StHistUtil.h,v $
// Revision 2.20  2016/06/13 20:31:11  genevb
// Resolve Coverity BUFFER_SIZE_WARNING with careful copy function
//
// Revision 2.19  2016/06/10 02:55:54  genevb
// Coverity: memory leaks, possible null pointer dereferences, over-write character buffers
//
// Revision 2.18  2014/08/06 11:42:52  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.17  2014/02/20 20:16:19  genevb
// Adjust dE/dx slope hist range, handle ROOT change for 2D polar plots
//
// Revision 2.16  2012/01/31 22:14:54  genevb
// QA Shift Mode, optimized for AutoQA Browser
//
// Revision 2.15  2011/05/24 20:50:43  genevb
// Allow limited graphics file printing
//
// Revision 2.14  2011/02/19 02:43:39  genevb
// Fix those missing consts
//
// Revision 2.13  2011/02/19 02:22:18  genevb
// Allow for specification of histogram usage by the required detector sets
//
// Revision 2.12  2011/02/07 20:25:26  genevb
// Allow for limiting detectors
//
// Revision 2.11  2011/01/19 02:05:22  genevb
// Allow plain ROOT files with hists, and individual plot generation from 1 file
//
// Revision 2.10  2010/03/12 07:29:05  genevb
// Additional capability for saving images of each pad
//
// Revision 2.9  2010/01/14 19:29:53  genevb
// Fix ROOT quirk with 1 page print, fix string/char conversions, protect LOG calls
//
// Revision 2.8  2009/01/08 23:40:14  genevb
// Introduce analyses with reference histograms
//
// Revision 2.7  2008/05/28 05:16:06  genevb
// Allow summing over (ignoring) histogram prefixes
//
// Revision 2.6  2007/03/13 18:42:28  genevb
// Add Svt list, simplified hlist include files, handle StMultiH2F, store dirName
//
// Revision 2.5  2006/05/18 16:38:03  genevb
// Introduce StHistUtil::GetRunYear()
//
// Revision 2.4  2006/03/28 21:35:32  genevb
// Single page output capability for eps,jpg,png,gif,tiff,etc. [see TPad::Print()]
//
// Revision 2.3  2006/03/28 01:58:39  genevb
// Allow PDF (and other) output formats (was only PostScript)
//
// Revision 2.2  2002/09/06 02:51:34  genevb
// Remove limit on maximum number of histograms that can be copied
//
// Revision 2.1  2000/08/25 22:06:50  genevb
// Added histo descriptor in top right
//
// Revision 2.0  2000/08/25 15:47:38  genevb
// New revision: cleaned up, multiple PS files
//
//
///////////////////////////////////////////////////////////////////////////////
// Histogram Utility methods for use with star makers and bfc output
///////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHistUtil
#define STAR_StHistUtil


#ifndef ROOT_TH1
#include "TH1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#endif

#include "TList.h"
#include "TString.h"


//  - if not using the methods of the class, then can just put class TCanvas;
//   -  however, if we are using the methods of TCanvas, then put include "TCanvas.h"
class TCanvas;
class StMaker;
class TPaveLabel;
class TPaveText;
class TDatime;

const int maxPathLen = 1024;

class StHistUtil {

 private:
  // Data-members to make up the output Canvases and Postscript files
  TCanvas* m_HistCanvas; //!
  TCanvas* m_HistCanvasR; //!
  Int_t   m_PadColumns;  // Number of the columns (TPad's) on the single Canvas
  Int_t   m_PadRows;     // Number of the columns (TPad's) on the single Canvas
  Int_t   m_PaperWidth;  // Paper size in cm
  Int_t   m_PaperHeight; // Paper size in cm
  TString m_FirstHistName;
  TString m_LastHistName;
  TString m_OutFileName; // Base name of the output file to plot hists out
  TString m_CurFileName; // Name of the current output file
  TString m_CurFileNameR; // Name of the current ref output file
  TString m_OutType;    // Output file type
  Bool_t  m_OutMultiPage; // Output file is multipage
  TString m_OutIndividuals; // Additional output of each pad
  Bool_t  m_QAShiftMode; // For the Offline QA Browser
  Int_t   m_CurPrefix;
  Int_t   m_CurPage;
  TString m_GlobalTitle; // Title at top of each page of output
  TPaveLabel* Ltitle;   //! PaveLabel with title for pages
  TPaveText*  Ldesc;    //! PaveLabel with descriptor for hists
  TList*  m_ListOfLogY; //! list of histogram names that will be drawn with logY scale
  TList*  m_ListOfLogX; //! list of histogram names that will be drawn with logX scale
  TList*  m_ListOfPrint;//! list of histogram names that will be drawn,printed
  StMaker* m_PntrToMaker;//! pointer to an St_Maker, so can find histograms
  TFile*  m_PntrToPlainFile; //! pointer to a plain root file if not using makers
  Int_t   maxHistCopy;  //! size of array of new histograms
  TH1**   newHist;      //! array of new histograms that other will be copied into
  Bool_t  debug;
  Int_t   m_RunYear;    // Run year
  Char_t  m_dirName[maxPathLen];//! Directory name for histograms in StIO tree
  Bool_t  ignorePrefixes;// whether or not to ignore prefixes when combining histograms
  Int_t numOfPosPrefixes; // number of possible prefixes
  const Char_t** possiblePrefixes; //!
  const Char_t** possibleSuffixes; //!
  TString m_Detectors;   // List of detectors
  UInt_t  m_PrintMode;   // Which output files to print

  // For reference analyses:
  Bool_t m_analMode;
  Char_t m_refResultsFile[maxPathLen];
  Char_t m_refOutFile[maxPathLen];
  TList* m_refCuts;
  TFile* m_refInFile;

 protected:
  virtual void    CloseOutFile();
  virtual TString StripPrefixes(const Char_t* histName, Int_t& prenum, Int_t mode=1);
  virtual Bool_t  CheckOutFile(const Char_t* histName);
  virtual TList*  TrimListByPrefix(TList* dList, const Char_t* withPrefix);
  virtual TH1*    FlipAxes(TH1* hist);
  virtual void    PathCopy(char *destination, const char* source);


 public: 
  StHistUtil();
  virtual        ~StHistUtil();
  virtual void    SetDebug(Bool_t dbg=kTRUE) { debug=dbg; }
  virtual Bool_t  Debug() { return debug; }
  virtual Int_t   DrawHists(const Char_t *dirName="EventQA");
  virtual Int_t   ListHists(const Char_t *dirName="EventQA");
  virtual TList*  FindHists(const Char_t *dirName="EventQA",
                            const Char_t *withPrefix=0);
  virtual TList*  FindHists(TFile* histFile, const Char_t* withPrefix=0);
  virtual Int_t   CopyHists(TList  *dirList);
  virtual Int_t   AddHists(TList  *dirList, Int_t nHistCopy=-1);
  virtual void    IgnorePrefixes(Bool_t ignore=kTRUE) {ignorePrefixes = ignore;}
  virtual Int_t   PrintInfoHists(TList  *dirList,  const Char_t *fname="printinfo.out");

  // Reference analyses:
  virtual void    SetRefAnalysis(const Char_t* refOutFile, const Char_t* refResultsFile,
                    const Char_t* refCutsFile=0, const Char_t* refInFile=0);

  virtual void    SetDefaultLogYList(const Char_t *dirName="EventQA");
  virtual Int_t   AddToLogYList(const Char_t *HistName="");
  virtual Int_t   RemoveFromLogYList(const Char_t *HistName="");
  virtual Int_t   ExamineLogYList();

  virtual void    SetDefaultLogXList(const Char_t *dirName="EventQA");
  virtual Int_t   AddToLogXList(const Char_t *HistName="");
  virtual Int_t   RemoveFromLogXList(const Char_t *HistName="");
  virtual Int_t   ExamineLogXList();

  virtual void    SetDefaultPrintList(const Char_t *dirName="EventQA",
                                      const Char_t *analType="FullTable");
  virtual Int_t   AddToPrintList(const Char_t *HistName="");
  virtual Int_t   RemoveFromPrintList(const Char_t *HistName="");
  virtual Int_t   ExaminePrintList();

  virtual Int_t   Overlay1D(Char_t *dirName,Char_t *inHist1,Char_t *inHist2);
  virtual Int_t   Overlay2D(Char_t *dirName,Char_t *inHist1,Char_t *inHist2);

  virtual Int_t   GetRunYear(const Char_t *filename);

  virtual void    SetDetectors(const Char_t *detectors);
  virtual Bool_t  DetectorIn(const Char_t *detector);

// Inline methods
  void SetHistsNamesDraw(const Char_t *firstName="*", const Char_t *lastName="*");
  void SetZones(Int_t columns=2, Int_t rows=3);   
  // SetPaperSize -->  A4 is 20,26  US letter is 20,24
  void SetPaperSize(Int_t width=20, Int_t height=24);

  // SetOutFile -->  type="ps","pdf","jpg",etc...see TPad::Print()
  void SetOutFile(const Char_t *fileName="", const Char_t* type=0);
  void SetPostScriptFile(const Char_t *psFileName="");
  void SetPDFFile(const Char_t *pdfFileName="");

  void SetPntrToMaker(StMaker *m1);
  void SetPntrToPlainFile(TFile *m1);
  void SetGlobalTitle(const Char_t *globalTitle="");
  TH1** getNewHist();
  Int_t getNewHistSize();
  
  Int_t GetNumOfPosPrefixes() {return numOfPosPrefixes;}
  const Char_t* GetPrefix(Int_t n) {return possiblePrefixes[n];}
  const Char_t* GetSuffix(Int_t n) {return possibleSuffixes[n];}

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHistUtil.h,v 2.20 2016/06/13 20:31:11 genevb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StHistUtil, 1)   //needed for all code that will be used in CINT
    };
        
inline void StHistUtil::SetHistsNamesDraw(const Char_t *firstName, const Char_t *lastName)
  { m_FirstHistName = firstName;  m_LastHistName  = lastName; }

inline void StHistUtil::SetZones(Int_t columns, Int_t rows)
  { m_PadColumns =columns; m_PadRows = rows;}

inline void StHistUtil::SetPaperSize(Int_t width, Int_t height)
  { m_PaperWidth = width; m_PaperHeight = height;}

inline void StHistUtil::SetPostScriptFile(const Char_t *psFileName)
  { SetOutFile(psFileName,"ps"); }

inline void StHistUtil::SetPDFFile(const Char_t *pdfFileName)
  { SetOutFile(pdfFileName,"pdf"); }

inline void StHistUtil::SetPntrToMaker(StMaker *m1) 
  {m_PntrToMaker = m1;}

inline void StHistUtil::SetPntrToPlainFile(TFile *m1) 
  {m_PntrToPlainFile = m1;}

inline void StHistUtil::SetGlobalTitle(const Char_t *globalTitle)
  { m_GlobalTitle = globalTitle;}

inline TH1**  StHistUtil::getNewHist()
  {return newHist;}

inline Int_t StHistUtil::getNewHistSize()
  {return maxHistCopy; }


class StHistUtilRef : public TNamed {
  public:
    StHistUtilRef(const char* name, const char* opts, const int mode, const double cut);
    int Mode;
    double Cut;
    virtual const char* Options() { return GetTitle(); }
  ClassDef(StHistUtilRef,1)   //needed for all code that will be used in CINT
};
  
#endif
