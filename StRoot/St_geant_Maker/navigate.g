 subroutine navigates
+CDE,TYPING,GCBANK,GCVOLU,GCUNIT.

 Integer k,mother,daughter,where,who,copy,found;
 Integer item(20),count(20),list(20),node(20);


 "first generation" found=0;
 { k, where, who, copy } =1;
 { list(1), item(1), count(1), node(1) } =0; 


 loop  " to find i-th content of mother for all generations "
 {
   item(k)+=1; mother=list(k); 

   if item(k)<=max(1,count(k))
   {  
      If k>1 { where=LQ(mother-item(k)); who=Q(where+2); copy=Q(where+3); }

      daughter=LQ(JVOLUM-who); found+=1; 

      " Call Gmap (Q(daughter+1),Q(where+1),node(k),node(k+1));"  

      if (Q(daughter+3)==0) next; 

      "birth" k+=1; count(k)=Q(daughter+3); item(k)=0; list(k)=daughter; next;
   }
      "death" k-=1; if (k<=0) break;
 }
 print *,' total objects found ',found;
 end
*
*----------------------------------------------------------------------------
 function agvolume(node,par,pos,mot,who,mcopy,par1)
+CDE,TYPING,GCBANK,GCVOLU,GCUNIT.
 integer  agvolume,node,par,pos,mot,old,LOCB,par1
*
 Integer k,n,mother,daughter,where,who,copy,found,ier,ia,mcopy,nvol
 Integer item(20),count(20),list(20),nodes(0:20),
         Lnam(20),Lnum(20),Iax(20),Lvol(20);
 Integer birth(50000);
 save    k,mother,daughter,where,found,copy,birth;
 save    item,count,list,nodes;
* integer i;
 character cn*4;

 agvolume=0;
 If node==0
 { "first generation" found=0; call vzero(birth,50000)
   { k, where, who, copy } =1; 
   { list(1), item(1), count(1), nodes(0) } =0; 
   nvol=0; if (JVOLUM>0) nvol=IQ(JVOLUM-1);
   if (Nvol>50000)  print *,'agvolume error: too manu volumes in geometry'
 }
 else
 { nodes(k)=node; 
   n=Q(daughter+3); if (n<0) n=-Q(3+LQ(daughter-1)); ia=Q(1+LQ(daughter-1));
   if n!=0 { k+=1; count(k)=n; item(k)=0; list(k)=daughter; iax(k)=ia; }
   :next:;  item(k)+=1;  mother=list(k); 
   if item(k)>abs(count(k)) 
   { k-=1; birth(Lvol(k))+=1; if (k>1) go to :next:; node=nodes(1); return }
   if count(k)>0 { where=LQ(mother-item(k)); copy=Q(where+3); }
   else          { where=LQ(mother-1);       copy=item(k);    } 
   who=Q(where+2); 
 }
   daughter=LQ(JVOLUM-who); found+=1; 
   par=LOCB(Q(daughter+1)); pos=LOCB(Q(where+1)); 
   mot=0; mcopy=1;  
   if (k>1) { mot=LOCB(Q(mother+1)); mcopy=Lnum(k-1); }
   node=nodes(k-1); agvolume=1;
   Lnam(k)=IQ(JVOLUM+who); Lnum(k)=copy; Lvol(k)=who;

   Call GLVOLU(k,Lnam, Lnum, ier)
   par1=LOCB(Q(LQ(JGPAR-Nlevel)+1))
   call UHTOC(Lnam(k),4,cn,4)
*   if (count(k)<0) print *,count(k),copy,Nlevel,(GRMAT(i,Nlevel),i=1,9);
*   if (count(k)=-30 & iax(k)==2) _
*   print *,count(k),copy,Nlevel,cn,(GRMAT(i,Nlevel),i=1,9);
   if (k>1 & birth(Lvol(k-1))>0) node=0;
end

  subroutine navigate
  integer agvolume,found,node,i1,i2,moth,old
  node=0; found=0;
  while agvolume(node,i1,i2,moth,old)>0  {  found+=1; node=found; }
  print *,' found objects =',found;
  end

