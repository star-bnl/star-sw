#define MAXWIN 100 /* all 3 types, catalog, dataset, and table */
static XtAppContext gAppCon;
static Widget gMainWindow,gAppShell;
/* No code uses this:
   static Widget gSigFigScalePopup; */
static Display *gDisplay;

/* These are global variables necesitated by the function pointer passed
** to RunTheRows, which lets me keep the cuts mechanism in one place. */
#define HIST 100
static float gRunTotal;
static int   gRunWhichHilitedLine,gRunNRowsDone,gRunWhWin;
static int gUp,gDown,gLeft,gRight;
static float gMin,gMax;
static int gHist[HIST];
#define YTICKADJUSTX (-0.20)
#define YTICKADJUSTY 0.02
#define XTICKADJUSTX (-0.04)
#define XTICKADJUSTY (-.14) /* (-0.10) */
#define MAINLABELX  (-0.20)
#define MAINLABELY  1.3
#define TICKSIZEX (-0.070)     /* fraction of height  */
#define TICKSIZEY (-0.025)     /* fraction of width  */
#define MAXTICKS 4.0
#define LEFTPS 150.0
#define BOTTPS 150.0
#define RIGHTMARGIN 20
#define WIDTH 400.0
#define HEIGH 120.0
#define LEFTMARGIN 100
#define TOPMARGIN 120
#define BOTTOMMARGIN 80
#define PS 20000
char gPs[PS],gSumCol[100];
#define NOTUSED 0
#define XtCP XtCallbackProc
#define TEXT_SIZE_PART_1 400
#define TEXT_SIZE_PART_2 2000
#define TEXT_SIZE_PART_3 50000
int gNPs,gNWin,gNGraphicsUp,gHistWhLine;
size_t gLast;
Widget gVer2;
#define NAME 30
#define GRAPHWIDTH 450
#define GRAPHHITE 400
#define HILITE_TURN_ON 0
#define HILITE_TOGGLE  1
#define HILITE_TURN_OFF 2
#define WIN_TYPE_D_TREE  0
#define WIN_TYPE_PRIMARY  1
#define WIN_TYPE_TABLE    2
#define WIN_TYPE_GRAPHICS 3

#define ROW_SEL_MANUAL		0
#define ROW_SEL_ALL		1
#define ROW_SEL_CUTS		2
#define ROW_SEL_NEXT10		3
#define ROW_SEL_NOT_USED 	4
#define MAXROW_DIV_BY_8 10000
#define MAX_CUTS_STRING 400
#define NUM_RADIO 4

#define COL 58  /* number of columns in table window, lower 2 parts */
#define WIDE 10  /* width (chars) for each column in write part of table win */
#define MCOL_INIT 4
#define MCOL 12  /* max columns allowed to be selected in table win */
#define EXT 13 /* length of constant beginning part of header info */
               /* (WFW+1)*NWF+EXT < COL */
size_t LastRow(int,int);
size_t FirstRow(int);
 /* comment 6tx tlm="ThisLineMeans".
    For TableBrowser windows
    this is a column number for dsColumnName()
    and related functions [see ColumnList() in ds.c].  For primary
    and dataset windows, however, it is such an index for gDs[]. */
 /* 7tx Good if win_type==WIN_TYPE_TABLE, in which case tlm[]
    points to column numbers. */
 /* 8tx Subscripting ruins 1-1 corres between columns and text lines
    that the user sees in the clickable window. */
char gCuts[COL+4];
int gBreakRowsLoop;
#define RW 12  /* num Rows in Write part init (table window type only) */
typedef struct {
  int    win_type;	/* one of WIN_TYPE_XXX */
  int    rowSel;	/* one of ROW_SEL_XXX */
  char   tableName[NAME];
  char   textTop[TEXT_SIZE_PART_1],*textClickPart;
  char   *textOutput;
  int    nlcpwtto; /* num line clickable part window type table only */
  int nolipw; /*Number Of Line In Primary Window clicked to make this window*/
  /* BBB compare to MAX_LINES_CLICK_PART */
  int    tlm[MAX_LINES_CLICK_PART]; /* See 6tx.  This is wh_gDs or colNum. */
  int    wh_gDs;  /* See 7tx. */
  int    width,ncol,nRowsWritePart;
  Widget shell,txtWidClickH,txtWidWriteH,rowWidget,rad[NUM_RADIO];
  Widget txtWidTop,txtWidClick,txtWidWrite;
  int isHilited[MAX_LINES_CLICK_PART],useCuts;
  int hlLst[MAX_LINES_CLICK_PART]; /* in order that user clicked on them */
  int nhlLst;
  int    subscript[MAX_LINES_CLICK_PART]; /* see comment 8tx */
  size_t nRow;
  int    whichRadio; /* 0 to NUM_RADIO-1, good if win_type==WIN_TYPE_TABLE */
} WINDOW_INFO;
WINDOW_INFO *gWin[MAXWIN]; /* the memory is malloc'ed, except for the small
                           ** amount needed to hold the MAXWIN pointers. */
#define ARGS Arg args[50]; int nn;

/* DPM: Need to have these decl's to pacify compilers. */
extern void ExposeCB (Widget, caddr_t, caddr_t);
extern void ContractCase (int);
extern void ExpandExceptCase (int);
extern void PrimaryList (char *, int, int *, int *, int, char *, int);
extern void MakeDrawingArea (Widget,Dimension, Dimension);
extern void dsu_Clear ();
extern void ExpandCase (int);
