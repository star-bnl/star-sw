//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// 
// 

#include "TBrowser.h"
#include "St_NodeView.h"
#include "St_NodePosition.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodeView                                                          //
//                                                                      //
// St_NodeView class is a special kind of St_DataSet with one extra     //
// pointer to wrap any TObject onto St_DataSet object                   //
//                                                                      //
//  BE CAREFUL !!!                                                      //
//  One has to use it carefully no conrol over that extra object        //
//  is performed. This means: the onject m_Obj data-member points to can//
//  be destroyed with no this kbject notifying.                         //
//  There is no tool /protection to check whether m_Obj is till alive.  //
//  It is one's  code responsilitiy                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(St_NodeView)
//_____________________________________________________________________________
St_NodeView::St_NodeView(St_Node &pattern,const St_NodePosition *nodePosition,EDataSetPass iopt)
            : St_ObjectSet(pattern.GetName(),(TObject *)nodePosition)
{
  //
  // Creates St_DataSet (clone) with a topology similar with St_DataSet *pattern
  //
  //  Parameters:
  //  -----------
  //  pattern        - the pattern dataset
  //  iopt = kStruct - clone only my structural links
  //         kAll    - clone all links
  //         kRefs   - clone only refs
  //         kMarked - clone marked (not implemented yet) only
  //
  //   All new-created sets become the structural ones anyway.
  //

  //  cout << "ctor for " << GetName() << " - " << GetTitle() << endl;
  SetTitle(pattern.GetTitle());

  St_NodePosition *position = 0;
  TIter next(pattern.GetListOfPositions());
  Bool_t optsel = (iopt == kStruct);
  Bool_t optall = (iopt == kAll);
  while (position = (St_NodePosition *)next()) {
    // define the the related St_Node
     St_Node *node = position->GetNode(); 
     if (node) {
       St_DataSet *parent = node->GetParent(); 
       if ( optall || (optsel && parent == (St_DataSet *)&pattern) )
                                  Add(new St_NodeView(*node,position));
     }
  }
}
//_____________________________________________________________________________
void St_NodeView::Browse(TBrowser *b){
  St_ObjectSet::Browse(b);
  St_NodePosition *pos = GetPosition();
  if (pos) pos->Browse(b);
//    b->Add(pos); 
}

//_____________________________________________________________________________
St_Node *St_NodeView::GetNode(){ 
  St_NodePosition *pos = GetPosition();
  if (pos)
    return pos->GetNode();
  return 0;
}

