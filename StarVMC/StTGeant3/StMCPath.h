// $Id: StMCPath.h,v 1.1 2005/03/09 18:35:35 perev Exp $
//
//
// Class StMCPath
// ------------------
#ifndef STMCPATH_H
#define STMCPATH_H

class StMCPath
{
public:
   StMCPath()				{ fSize=0;}
   StMCPath(const StMCPath &from);
  ~StMCPath(){}
StMCPath &operator=(const StMCPath &);
int  GetSize() const 			{ return fSize;}   
void GetLevel(int lev,int &volid,int &copy,int &idx, int &ndx) const; 
void SetLevel(int lev,int  volid,int  copy,int  idx, int  ndx);  
void SetSize (int size) 		{ fSize = size;}
private:
  int   fSize;
  int   fPath[100][4];

};
#endif
