***********************************************************************
*
* Include-files:
*
**** avh_moncar.h *****************************************************
*      integer avh_moncar_nmax
*      parameter(avh_moncar_nmax=10)
***********************************************************************
*
**** avh_moncar_priv.h ************************************************
*      include 'avh_moncar.h'
*      integer nmax
*      parameter(nmax=avh_moncar_nmax)
*      double precision 
*     & sum0(nmax),sum1(nmax),sum2(nmax),wmax(nmax)
*     &,tot0(nmax),tot1(nmax),tot2(nmax),nbat(nmax),nevt(nmax)
*      common/avh_moncar_com/
*     & sum0,sum1,sum2,wmax
*     &,tot0,tot1,tot2,nbat,nevt
***********************************************************************


      subroutine avh_moncar_init(label)
*  ********************************************************************
*  ********************************************************************
      implicit none
      include 'avh_moncar_priv.h'
      integer label
      sum0(label) = 0d0
      sum1(label) = 0d0
      sum2(label) = 0d0
      wmax(label) = 0d0
      tot0(label) = 0d0
      tot1(label) = 0d0
      tot2(label) = 0d0
      nbat(label) = 0d0
      nevt(label) = 0d0
c      open(51,file='MONCAR') !DEBUG
      end


      subroutine avh_moncar_collect(label ,wght)
*  ********************************************************************
*  ********************************************************************
      implicit none
      include 'avh_moncar_priv.h'
      integer label
      double precision wght
      sum0(label) = sum0(label) + 1d0
      sum1(label) = sum1(label) + wght
      sum2(label) = sum2(label) + wght*wght
      if (wght.gt.wmax(label)) wmax(label) = wght
      end


      subroutine avh_moncar_batch(label ,wght)
*  ********************************************************************
*  ********************************************************************
      implicit none
      include 'avh_moncar_priv.h'
      integer label
      double precision wght ,ave,var ,avetot,sigtot
      if (sum0(label).lt.2) return
      nbat(label) = nbat(label) + 1d0
      nevt(label) = nevt(label) + sum0(label)
      ave = sum1(label)/sum0(label)
      var = (sum2(label)/sum0(label) - ave*ave)/(sum0(label)-1d0)
      tot0(label) = tot0(label) + wght
      tot1(label) = tot1(label) + wght*ave
      tot2(label) = tot2(label) + wght*wght*var
      sum0(label) = 0d0
      sum1(label) = 0d0
      sum2(label) = 0d0
c      avetot = tot1(label)/tot0(label) !DEBUG
c      sigtot = dsqrt( tot2(label) )/tot0(label) !DEBUG
c      if (label.eq.1) write(51,*) nevt(label) !DEBUG
c     &,ave+dsqrt(var),ave-dsqrt(var),avetot+sigtot,avetot-sigtot !DEBUG
      end


      subroutine avh_moncar_result(label ,ave,sig,eff,rev)
*  ********************************************************************
*  ********************************************************************
      implicit none
      include 'avh_moncar_priv.h'
      integer label
      double precision ave,sig,eff,rev
      if (nbat(label).gt.1d0) then
        ave = tot1(label)/tot0(label)
        sig = dsqrt( tot2(label) )/tot0(label)
        rev = nevt(label)
        if (wmax(label).ne.0d0) then
          eff = ave/wmax(label)
        else
          eff = 1d0
        endif
      else
        call avh_moncar_interm(label ,ave,sig,eff,rev)
      endif
c      close(51) !DEBUG
      end


      subroutine avh_moncar_interm(label ,ave,sig,eff,rev)
*  ********************************************************************
*  ********************************************************************
      implicit none
      include 'avh_moncar_priv.h'
      integer label
      double precision ave,sig,eff,rev
      if (sum0(label).gt.0d0) then
        ave = sum1(label)/sum0(label)
      else
        ave = 0d0
      endif
      if (sum0(label).gt.1d0) then
        sig = sum2(label)/sum0(label)
        sig = sig - ave*ave
        if (sig.gt.0d0) then
          sig = dsqrt(sig/(sum0(label)-1d0))
        else
          sig = 0d0
        endif
      else
        sig = dabs(ave)
      endif
      if (wmax(label).ne.0d0) then
        eff = ave/wmax(label)
      else
        eff = 1d0
      endif
      rev = sum0(label)
      end
