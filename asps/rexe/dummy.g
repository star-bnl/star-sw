     subroutine my_kuip 
     character   command*(*), string*(*)
     character   K_command*20,K_string*20
     common /rc_kuip_data/  Ind,K_Npar,K_command,K_string(50)     

     entry      set_kupatl ( command, npar)
     i=index(command,char(0));  if (i<=0) i=1
     K_npar    = Npar 
     K_command = command(1:i-1)
     do Ind=1,50 { K_string(ind)=' ' }
     Ind       = 0
     return

     entry      set_kugets ( string, len)
     i=index(string,char(0));  if (i<=0) i=1
     Ind            = Ind+1
     K_string(Ind)  = string(1:i-1)
     return

     end


subroutine traceqc;      print *,' dummy traceqc called';       end
subroutine pawfca;       print *,' dummy pawfca called ';       end
subroutine pawbrk;       print *,' dummy pawbrk called ';       end
subroutine pawcs;        print *,' dummy pawcs  called ';       end
subroutine pgexi;        print *,' dummy pgexi  called ';       end
*yf subroutine gkfort;       print *,' dummy gkfort called ';       end
*yf subroutine gpdcay;       print *,' dummy gpdcay called ';       end
subroutine csrmsl;       print *,' dummy csrmsl called ';       end
*yf subroutine csjcal;       print *,' dummy csjcal called ';       end
*yf subroutine csaddr;       print *,' dummy csaddr called ';       end
subroutine csfile;       print *,' dummy csfile called ';       end
subroutine csext;        print *,' dummy csext  called ';       end
subroutine csexec;       print *,' dummy csexec called ';       end

subroutine tdm_clear_all;       print *,' dummy tdm_clear_all called ';   end
subroutine tdm_map_table;       print *,' dummy tdm_map_table called ';   end
subroutine ami_module_register; print *,' dummy ami_module_register  ';   end
subroutine agpawq;              print *,' dummy agpawq called ';          end
subroutine hplopt;              print *,' dummy hplopt called ';          end
subroutine TDM_NEW_TABLE;              print *,' dummy TDM_NEW_TABLE called ';          end
subroutine AMI_CALL;              print *,' dummy AMI_CALL called ';          end
subroutine dui_cdir;              print *,' dummy dui_cdir called ';          end

