#include <cstdio>
#include "wcpplib/stream/definp.h"
#include "wcpplib/stream/findmark.h"

using namespace std;

int definp_int(const String& str)
{
  mfunnamep("int definp_int(const String& str)");
  int i = 0;
  mcout<<"definp_int: starting, read int "<<str<<"\n";
  if(str != String()) {
    // search for mark
    int i_ret = findmark(std::cin, str.c_str());
    check_econd11(i_ret , != 1 , mcerr);
  }
  cin>>i;
  Iprintn(mcout, i);
  check_econd11(cin.good() , != 1 , mcerr);
  mcout<<"int is read\n";
  return i;
}

long definp_long(const String& str)
{
  mfunnamep("int definp_long(const String& str)");
  long i=0;
  mcout<<"definp_long: starting, read long "<<str<<"\n";
  if(str != String())
  {
    // search for mark
    int i_ret = findmark(cin, str.c_str());
    check_econd11(i_ret , != 1 , mcerr);
  }
  cin>>i;
  Iprintn(mcout, i);
  check_econd11(cin.good() , != 1 , mcerr);
  mcout<<"long is read\n";
  return i;
}

double definp_double(const String& str)
{
  mfunnamep("int definp_double(void)");
  double d;
  mcout<<"definp_double: starting, read double "<<str<<"\n";
  if(str != String())
  {
    // search for mark
    int i_ret = findmark(cin, str.c_str());
    check_econd11(i_ret , != 1 , mcerr);
  }
  cin>>d;
  Iprintn(mcout, d);
  check_econd11(cin.good() , != 1 , mcerr);
  mcout<<"double is read\n";
  return d;
}

String definp_String(const String& str)
{
  mfunnamep("int definp_String(const String& str)");
  String istr;
  mcout<<"definp_String: starting, read String "<<str<<"\n";
  if(str != String())
  {
    // search for mark
    int i_ret = findmark(cin, str.c_str());
    check_econd11(i_ret , != 1 , mcerr);
  }
  cin>>istr;
  Iprintn(mcout, istr);
  check_econd11(cin.good() , != 1 , mcerr);
  mcout<<"String is read\n";
  return istr;
}

long set_position(const String& word, std::istream& istrm, 
		  int s_rewind, int s_req_sep)
{
  mfunnamep(
	"int set_position(const String& word, std::istream& istrm, int s_rewind, int s_req_sep)");
  check_econd11a(istrm.good() , != 1 , 
	  "before seekg, call for variable named "<<word<<'\n' , mcerr);
  long nbeg, nnext; char prev;
  if(s_rewind == 1)
    istrm.seekg(0);
  if(s_req_sep == 0)
  {
    //int iret = findmark(istrm, word.c_str());
    int iret = findmark_b(istrm, word, word.length(), 
			  nbeg, nnext, prev);
    
    check_econd11a(iret , != 1 , "The keyword \""<<word.c_str()<<"\" is not found\n" , mcerr);
    check_econd11a(istrm.good() , != 1 , 
		   "after the call of findmark for variable named "<<word<<'\n' , mcerr);
  }
  else
  {
    do
    {
      int iret = findmark_b(istrm, word, word.length(),
			    //  DynLinArr< T > ws, long qws, 
			    nbeg, nnext, prev);
      check_econd11a(iret , != 1 , 
		     "The keyword \""<<word.c_str()<<"\" is not found\n" , 
		     mcerr);
      check_econd11a(istrm.good() , != 1 , 
		     "after the call of findmark for variable named "<<word<<'\n' , mcerr);
      if(nbeg == 0) return nbeg;  // no need to search for separator
      if(prev== '\n' || prev == ' ') return nbeg; // good separator
    }
    while(1);  // infinite loop  
  }
  check_econd11a(istrm.good() , != 1 , 
		 "after findmark_b, call for variable named "<<word<<'\n' , mcerr);
  return nbeg;
}

void remove_end_comments(std::istream& istr, String commark, 
						 DynLinArr< char >& istring)
{
  mfunnamep("void remove_end_comments(...)");
  //mcout<<"remove_end_comments is started\n";
  long marklen = commark.length();
  //Iprintn(mcout, marklen);
  DynLinArr<char> inpmark(marklen+1);  // temporary array for testing for "mark"
  //                       +1 for '\0'
  inpmark[marklen] = '\0';
  long qel = 0;
  istring.put_qel(1);
  int ic;
  int s_com = 0; // sign of comment
  while((ic=istr.get())!=EOF)
  {
    //mcout<<"read new ic="<<char(ic)<<'\n';
	//Iprint2n(mcout, qel, s_com);
    if(s_com == 1)
    {
      if(ic == '\n')
	{
        //mcout<<"comment will be annulled\n";
	  s_com = 0;
	  
	  if(qel >= istring.get_qel() - 2)
	  {
	    istring.put_qel(istring.get_qel() * 2);
	    //Iprint2n(mcout, qel, istring.get_qel());
	  }
	  istring[qel++] = ic;
	}
      }
      else
      {
	long n;
	for(n=1; n<marklen; n++)
	{
	  inpmark[n-1] = inpmark[n];
	}
	inpmark[marklen-1] = ic;
	//Iprintn(mcout, inpmark);
	if(strcmp(commark.c_str(), &(inpmark[0])) == 0)
	{  // yes, mark of comment is found
	  //mcout<<"comment is recognized\n";
	  s_com = 1;
	  qel = qel - (marklen-1);  // to annul beginning of the comment mark
	}
	else
	{   // just add new symbol
	  if(qel >= istring.get_qel() - 2)
	  {
	    istring.put_qel(istring.get_qel() * 2);
	    //Iprint2n(mcout, qel, istring.get_qel());
	  }
	  //for debug:
	  //if(char(ic) == ' ')
	  //  istring[qel++] = '_';
	  //else
	  istring[qel++] = ic;
	}
      }
  }
  istring[qel++] = '\0';
  //mcout<<"remove_end_comments is finished\n";
}
