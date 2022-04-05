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
     function AGVOLUME(node,par,pos,mot,who,copy,par1,npar,matName)
*----------------------------------------------------------------------------
*	node - ????
*	par  - pointer to current volume (address of q(volu+1))
*	mot  - pointer to current mother volume parameters
*	who  - id of current volume
*	copy - copy number of current volume
*	par1 - pointer to current volume parameters
*	npar - number of current volume parameters
*	matName - name of material

+CDE,TYPING,GCBANK,GCVOLU,GCUNIT.
 integer  *8 node,par,pos,mot,LONGB,par1
 integer  agvolume,np,ish,npar,natt,npr
 integer  matName(6);
 
*
 Integer k,n,mother,daughter,where,who,copy,found,ier,ia,mcopy,nvol
 Integer *8 nodes(0:20)
 Integer item(20),count(20),list(20),
         Lnam(20),Lnum(20),Iax(20),Lvol(20);
 Integer birth(50000);
 save    k,mother,daughter,where,found,birth;
 save    item,count,list,nodes;

 character cn*4;
 real    para(100),attr(100),parb(100),vdist;
 save    para
 integer i;

 integer numed,jmed,imat,jmat,ln;


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
   par=LONGB(Q(daughter+1)); np=Q(daughter+5); ish=Q(daughter+2);
   pos=LONGB(Q(where+1)); 


   numed = Q(daughter+4);
   jmed = LQ(JTMED-numed);
   imat = Q(jmed+6);
   jmat = LQ(JMATE-imat);
   call uhtoc(Q(jmat+1),99,matName,20);
   
   mot=0; mcopy=1;  
   if (k>1) { mot=LONGB(Q(mother+1)); mcopy=Lnum(k-1); }
   node=nodes(k-1); agvolume=1;
   Lnam(k)=IQ(JVOLUM+who); Lnum(k)=copy; Lvol(k)=who;

   Call GLVOLU(k,Lnam, Lnum, ier)

* have to check JGPAR later
   if np==0  { npar=Q(where+9); call UCOPY (Q(where+10),para,npar)  }
   else      { npar=np; call UCOPY (Q(LQ(JGPAR-Nlevel)+1),para,np)  }

   IF      ISH==28   {  NPAR = 12 }
   ELSE IF ISH==4       " trap "
   {  NPAR = 11
      call AgXY2RF(PARA(2),PARA(3))
      call AgT2A(PARA(7))
      call AgT2A(PARA(11))
   }
   ELSE IF ISH==10      " para "
   {  call AgXY2RF(PARA(5),PARA(6))
      call AgT2A(PARA(4))
   }

   par1=LONGB(para); 
   call UCOPY(Lnam(k),para(npar+1),1);
  
*  call UHTOC(Lnam(k),4,cn,4)
*  call GFPARA (cn,copy,0,npr,natt,parb,attr)
*  if (vdist(parb,para,3)>.001) print *,'****',cn,np,(para(i),parb(i),i=1,3)

   if (k>1 & birth(Lvol(k-1))>0) node=0;
end

  subroutine navigate
  integer *8 node,i1,i2,moth
  integer agvolume,found,old,dum(6)
  node=0; found=0;
  while agvolume(node,i1,i2,moth,old,0,0,0,dum)>0  {  found+=1; node=found; }
  print *,' found objects =',found;
  end


   subroutine AgT2A(t)
+CDE,typing,GCONST.
   real alp,x,y,phi,r,t
      alp = ATAN(t)*RADDEG
      if (alp>90.) alp-=180.
      t = alp
      return
   entry    AgXY2RF(x,y)
      phi = 0
      r   = SQRT(x*x+y*y)
      if (r  > 0.) phi = ATAN2(y,x)*RADDEG
      if (phi< 0.) phi+= 360.
      x=ATAN(r)*RADDEG; y=phi
      return
   end

