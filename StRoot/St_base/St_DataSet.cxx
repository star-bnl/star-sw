//*-- Author :    Valery Fine   27/04/98  (E-mail:fine@bnl.gov)

#include "St_DataSet.h"
#include "TBrowser.h"

ClassImp(St_TableIter)
ClassImp(St_DataSetIter)
//______________________________________________________________________________
 St_DataSetIter::St_DataSetIter(St_DataSet *l, Bool_t dir)
 : s_RootDataSet(l)
 , s_WorkingDataSet(l)
 , s_Next(l ? new TIter(l->s_ListOfDataSet,dir):0)
{ }

//______________________________________________________________________________
St_DataSet *St_DataSetIter::AddTable(St_Table *table, St_DataSet *dataset)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // AddTable                                                                  //
 //                                                                           //
 // St_DataSet dataset != 0 - Add a STAF table to the St_DataSet dataset      //
 //                                                                           //
 //                     = 0 -  (by default) to the current St_DataSet defined //
 //                             with s_WorkingDataSet data member             //
 //                                                                           //
 ///////////////////////////////////////////////////////////////////////////////

  St_DataSet *set =  dataset;
  if (!set) set = Pwd();
  if (set) {
   set = set->InsertTable(table);
  }
  return set;
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::AddTable(St_Table *table, const Char_t *path)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 //   AddTable                                                                //
 //                                                                           //
 // Char_t path != 0 - Add a STAF table to the St_DataSet dataset defined     //
 //                                                                           //
 //                     = 0 -  (by default) to the current St_DataSet defined //
 //                             with s_WorkingDataSet data member             //
 //                                                                           //
 ///////////////////////////////////////////////////////////////////////////////
 St_DataSet *set = 0;
 if (path && strlen(path)) set = Next(path);
 return AddTable(table,set);
}

//______________________________________________________________________________
St_DataSet *St_DataSetIter::Cd(Char_t *dirname){
/////////////////////////////////////////////////////////////////////
//                                                                 //
// St_DataSet *St_DataSetIter::Cd(Char_t *dirname)                 //
//                                                                 //
// Change the current working directory to dirname                 //
// return the pointer to the previous "working" St_DataSet object  //
//                                                                 //
// Returns the pointer to the new "working" St_DataSet             //
//         0,    if the new directory doesn't exist.               //
//                                                                 //
/////////////////////////////////////////////////////////////////////
  St_DataSet *set =  Next(dirname);
  if (set) s_WorkingDataSet = set;
  return set;
}

//______________________________________________________________________________
void *St_DataSetIter::GetTable(const Char_t *path)
{
 St_Table *table = GetTableObj(path);
 if (table) return table->GetArray();
 return 0;
};
//______________________________________________________________________________
St_Table *St_DataSetIter::GetTableObj(const Char_t *path)
{ 
  St_DataSet *set = Next(path);
  if (set) return set->GetTableObj();
  return 0;
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Dir(Char_t *dirname)
{
  St_DataSet *set = s_WorkingDataSet;
  if (dirname) set = Next(dirname);
  if (set) set->ls();
  return set;
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Mkdir(Char_t *dirname){
 St_DataSet *set = 0;
 set = Next(dirname,0,kTRUE);
 if (!s_Next)  Reset();  // Create a new iterator 
 return set;
}

//______________________________________________________________________________
Int_t St_DataSetIter::Rmdir(St_DataSet *dataset,Option_t *option){
  St_DataSet *set = dataset;
  if (set) {
    delete set;
    if (set == s_RootDataSet) s_RootDataSet = 0;
    s_WorkingDataSet = s_RootDataSet;
  }
  return (Int_t)dataset;
}

//______________________________________________________________________________
St_DataSet *St_DataSetIter::Next(const Char_t *path, St_DataSet *rootset, 
                                 Bool_t mkdirflag)
{ 
 ////////////////////////////////////////////////////////////////////////////////
 //                                                                            //
 //           "path" ::= <relative path> | <absolute path> | <empty>           //
 //                                                                            //
 //  "relative path" ::= <dataset name> | <dataset name>/<dataset name>        //
 //                                                                            //
 //  "absolute path" ::= /<relative path>                                      //
 //  "empty"         ::= zero pointer | pointer to zero length string          //
 //                                                                            //
 // "relative path": the search is done against of s_WorkingDataSet data mem   //
 // "absolute path": the search is done against of s_RootDataSet    data mem   //
 // "empty path"   : no search is done just next St-DataSet is returned if any //
 //                                                                            //
 ////////////////////////////////////////////////////////////////////////////////

   if (!path || !strlen(path)) return rootset;

   St_DataSet *dataset = rootset;
   const Char_t pathseparator='/';
   const Char_t *startpos = path;
   const Char_t *seppos = startpos ? strchr(startpos,pathseparator) : 0;  

 // delete all "blanks" 

 //*-*
 //*-* define the path type
 //
   if ( startpos && seppos==startpos )
   {
      //*-* "absolute path":

      startpos = seppos+1;        
      seppos = strchr(startpos,pathseparator);
      if (!dataset) dataset = s_RootDataSet;
   }
   else 
      if (!dataset) 
           dataset = s_WorkingDataSet;  //*-* "relative path"        

   ULong_t ldirname = 0;

   if (seppos)
      ldirname=ULong_t(seppos-startpos);
   else 
      ldirname = strlen(startpos);

   if (ldirname) {
      Char_t *dirname = new Char_t[ldirname+1];
      strncpy(dirname,startpos,ldirname);
      dirname[ldirname]=0;
     
      St_DataSet *thisset = dataset;
      Bool_t found = kFALSE;
      if (mkdirflag && !s_RootDataSet) {

      // There is no "root" St_DataSet object
      //     Let's create it
        St_DataSet *set = new St_DataSet(dirname,dataset);
        if (dataset) 
              dataset->Add(set);
        else  
              dataset = set;

        thisset   = dataset;
        if (!s_RootDataSet) {
          s_RootDataSet       = dataset;
          s_WorkingDataSet    = dataset;
        }
        found = kTRUE;
      }
      else {
        TList *list = dataset->GetListOfDataset();
        if (list) {
          TIter next(list);
          St_DataSet *obj = 0;
          while (!(found = dataset->IsThisDir(dirname)) && (obj = (St_DataSet *)next()) )
                dataset = obj; 
        } else 
          found = dataset->IsThisDir(dirname); 
      }
      if (!found) dataset = 0;
      if (!found && mkdirflag) {
          found = kTRUE;
          dataset = new St_DataSet(dirname,thisset);
          if (thisset)
             thisset->Add(dataset);            
      }
      // Go to the next recursive level
      if (found) 
        dataset = Next(seppos,dataset,mkdirflag);
       
      delete [] dirname;
   }
   return dataset;
}

//______________________________________________________________________________
Bool_t St_DataSet::IsThisDir(const Char_t *dirname)
{
  return !strcmp(GetName(),dirname);
}

//______________________________________________________________________________
void St_DataSetIter::Reset(St_DataSet *l) 
{
  if (l) {
    s_RootDataSet    = l;
    s_WorkingDataSet = l;
    if (s_Next) delete s_Next; s_Next = 0;
    if (s_RootDataSet->s_ListOfDataSet) 
             s_Next = new TIter(s_RootDataSet->s_ListOfDataSet);
  } 
  else {
    if (s_Next)
        s_Next->Reset();
    else if (s_RootDataSet->s_ListOfDataSet)
        s_Next = new TIter(s_RootDataSet->s_ListOfDataSet);
  }
}
//______________________________________________________________________________
//______________________________________________________________________________

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// St_DataSet can be iterated using an iterator object (see St_DataSetIter)//
//                                                                         //
// Depending on the concrete collection class there may be                 //
// some additional methods of iterating. See the repective classes.        //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

ClassImp(St_DataSet)

//______________________________________________________________________________
St_DataSet::St_DataSet(const Char_t *name, St_DataSet *parent) 
 : s_StafTable(0), s_ListOfDataSet(0)
{
  if (parent) s_ListOfDataSet = new TList(parent);
  else s_ListOfDataSet = new TList;
  SetName(name);
}
//______________________________________________________________________________
St_DataSet::~St_DataSet(){
 // Delete table
  if (s_StafTable) {
      s_StafTable->SetTablePointer(NULL); 
      delete s_StafTable; 
      s_StafTable = 0;
  }
 // Delete list of the St_DataSet
  if (s_ListOfDataSet) {
   // First we should break our relationship with the parent if any
    St_DataSet *parent = GetParent();
    if (parent) parent->Remove(this);
    s_ListOfDataSet->Delete(); 
    delete s_ListOfDataSet; 
    s_ListOfDataSet = 0;
    s_StafTable = 0;
  }
}
//______________________________________________________________________________
void St_DataSet::Add(St_DataSet *dataset)
{ 
  if (!dataset) return; 
  if (!s_ListOfDataSet) s_ListOfDataSet = new TList;
  // Check whether this new child has got any partent yet
  if (!dataset->GetParent()) dataset->SetParent(this);
  s_ListOfDataSet->Add(dataset);
}    

//______________________________________________________________________________
void St_DataSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
   St_DataSetIter next(this);
   St_DataSet *obj;
   if (b)
       while (obj = next()) {
          if (obj->GetTableObj()) 
                 b->Add(obj->GetTableObj());
          else 
                 b->Add(obj);
       }
}
//______________________________________________________________________________
St_DataSet *St_DataSet::InsertTable(St_Table *table)
{
  St_DataSet *set =0;
// Insert the Staf dataset wrapper with table
  if (table) {
   set = new St_DataSet(table->GetName(),this);
   if (set) { 
      set->Add(table);
      Add(set);
   }
  }
  return set;
}

//______________________________________________________________________________
void  St_DataSet::ls(Option_t *option)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Option_t *option)                                           //
 //                                                                 //
 //    option= "*" means print all levels                           //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////

  if (option && !strcmp(option,"*")) ls(Int_t(0));
  else                               ls(Int_t(1));
}

//______________________________________________________________________________
void  St_DataSet::ls(Int_t deep)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Int_t deep)                                                 //
 //                                                                 //
 //  Prints the list of the Staf dataset.                           //
 //                                                                 //
 //  Parameter:                                                     //
 //  =========                                                      //
 //    Int_t deep >0 the number of levels to be printed             //
 //               =0 all levels will be printed                     //
 //            No par - ls() prints only level out                  //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////

 if (s_StafTable) s_StafTable->ls();
 else             TNamed::ls();

  if (s_ListOfDataSet && deep != 1 ) {
    TIter next(s_ListOfDataSet);
    St_DataSet *d=0;
    while (d = (St_DataSet *)next()) {
        IncreaseDirLevel();
        d->ls(deep == 0 ? 0 : --deep);
        DecreaseDirLevel();
    }
  }
}
 
//______________________________________________________________________________
void St_DataSet::Remove(St_DataSet *set)
{ 
  if (s_ListOfDataSet && set) s_ListOfDataSet->Remove(set); 
}


