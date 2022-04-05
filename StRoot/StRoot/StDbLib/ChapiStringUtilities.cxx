#include "ChapiStringUtilities.h"
#include <sys/types.h>
#include <string.h>
using namespace std;
typedef vector<string>::const_iterator VCI;

namespace chapi_string_utilities
{

vector<string> slice(string A, string sep)
{
  vector<string> v;
  string tmp = "";

  u_int N = A.size();
  for (u_int i=0; i<N; i++)
    {
      string comp = A.substr(i,1);
      if (comp != sep)
        {
          tmp += comp;
        }
      else
        {
          if (tmp!="") {v.push_back(tmp);}
          tmp = "";
        }
    }
v.push_back(tmp);

 return v;
}
//////////////////////////////////////////////////////////
map<string,string> associate_pieces(vector<string> v, string sep)
{
  map<string,string> m;
  VCI b = v.begin();
  VCI e = v.end();
  for (VCI i = b; i!=e; ++i)
    {
      vector<string> ss = slice(*i,sep);
      if (ss[0] !="")
	{
	  m[ss[0]] = ss[1];
	}
    }
  return m;
}

//////////////////////////////////////////////////////////////
void cut_string_after_sub(string& input, const string& sub)
{  
  /* Cut string after and including the substring */

  string::size_type I;
  string::iterator b;
  string::iterator e;
  I = input.find_last_of(sub);
  if (I==string::npos) return;
  b = input.begin() + I;
  e = input.end();
  input.erase(b,e);
}
//////////////////////////////////////////////////////////////
bool good_character(char* src)
{
  if (strncmp(src," ",1)==0) return false;
  if (strncmp(src,"\n",1)==0) return false;
  if (strncmp(src,"\t",1)==0) return false;
  if (strncmp(src,"\r",1)==0) return false;
  return true;
}
//////////////////////////////////////////////////////////////
string filter_string(char* src)
{
  int n=strlen(src);
  string rtrn;

  for (int i=0; i<n; i++)
    {
      if (good_character(&src[i]))
	{
	  rtrn += src[i];
	}
    }
  return rtrn;
}
//////////////////////////////////////////////////////////////

}
