//:Copyright 1995, Lawrence Berkeley Laboratory
//:>--------------------------------------------------------------------
//:FILE:        tst_asuString1.C
//:DESCRIPTION: Tests of the string class.
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     27jul95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <stream.h>
#include "sutClasses.hh"

//:>--------------------------------------------------------------------
//:ROUTINE:     main
//:DESCRIPTION: Program Mainline
//:ARGUMENTS:   -- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------

int main()
{
   string s("this is only a test");
   string t(" Nothing can go wrong");
   string u("...go wrong...");
   string v;

   cout << "s = (" << s.show() << ")\n";
   cout << "t = (" << t.show() << ")\n";
   cout << "u = (" << u.show() << ")\n";

   v = s + t;
   cout << endl;
   cout << "v = s + t" << endl;
   cout << "v = (" << v.show() << ")\n";

   v += u;
   cout << endl;
   cout << "v += u   (20jul95 - not working properly)" << endl;
   cout << "v = (" << v.show() << ")\n";

   v = "This is an other string.";
   cout << endl;
   cout << "v = This is an other string." << endl;
   cout << "v = (" << v.show() << ")\n";

   cout << "string_class: ending" << endl;

   return 1;
}

