#!/bin/sh
gawk  '
BEGIN {n=0}
{    if(index("/*",$1) == 0) {
       r =",";s = " "; t=$0; gsub(r,s,t);
       u ="[()]"; gsub(u,s,t);# print t;
       d =":"; gsub(d,s,t); # print t;
       ns = split(t,list); # print "ns = "ns " " list[1]"  "list[2];
       if (n>0) {
#       print t;
       for (l=1;l<=ns;l++){
           if (index(list[l],"inout")>0) {m++; k++;inn[m-1]=list[l+1]; 
                                                   out[k-1]=list[l+1];
#          print "m="m",k="k", list[l]="list[l]", list[l+1]="list[l+1];
           break;                 }
           else {
                 if (index(list[l],"in")>0)  {m++; inn[m-1]=list[l+1]; break;}
                 if (index(list[l],"out")>0) {k++; out[k-1]=list[l+1]; break; }
                }            
                }
                }
       else {for (l=1;l<=1;l++){
#         print "l="l" list = "list[l];
         if (index("interface",list[l])>0) {n++; x=list[l+1]; break;}}}
       nn = n; 
       if (m>nn) nn=m;
       if (k>nn) nn=k;
}}
END {#  print "m="m",k="k;
#        print "========================== " x " =========================================";
#                          {printf "%16s | \t %16s \t -> %16s \n", "Module", "Input", "Output"} 
#      print "==========================================================================";
      f = x ; print "<TR VALIGN='TOP'>"
      for (i=1;i<=nn;i++){
        f = " "; y = " "; z = " ";
        fref = " "; yref= " "; zref = " ";
        if (i==1) {f=       x "</a>"; fref="<a href=\"#"x"\">";}
        if (i<=m) {y=inn[i-1] "</a>"; yref="<a href=\"#"inn[i-1]"\">";}
        if (i<=k) {z=out[i-1] "</a>"; zref="<a href=\"#"out[i-1]"\">";}
        print "<TD>" fref f "<TD>" yref y"<TD>" zref z "</TD></TR>" 
}

}
' $1
exit
