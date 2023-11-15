*deck,usercreep    USERDISTRIB  parallel                                gal
      SUBROUTINE usercreep (impflg, ldstep, isubst, matId , elemId,
     &                      kDInPt, kLayer, kSecPt, nstatv, nprop,
     &                      prop  , time  , dtime , temp  , dtemp , 
     &                      toffst, Ustatev, creqv , pres  , seqv  ,
     &                      delcr , dcrda)
      
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS:"USERCREEP"::usercreep        
c*************************************************************************
c     *** primary function ***
c           Define creep laws when creep table options are
c           TB,CREEP with TBOPT=100.
c           Demonstrate how to implement usercreep subroutine 
c
c            Creep equation is 
c               dotcreq := k0 * seqv ^ n * creqv ^ m * exp (-b/T)
c
c               seqv  is equivalent effective stress (Von-Mises stress)
c               creqv is equivalent effective creep strain
c               T     is the temperature
c               k0, m, n, b are materials constants,
c
c           This model corresponds to  primary creep function  TBOPT = 1
c
c                                                          gal 10.01.1998
c
c*************************************************************************
c Copyright ANSYS.  All Rights Reserved.
c
c     input arguments
c     ===============
c      impflg   (in ,sc   ,i)             Explicit/implicit integration 
c                                         flag (currently not used)
c      ldstep   (in ,sc   ,i)             Current load step
c      isubst   (in ,sc   ,i)             Current sub step 
c      matId    (in ,sc   ,i)             number of material index
c      elemId   (in ,sc   ,i)             Element number
c      kDInPt   (in ,sc   ,i)             Material integration point
c      kLayer   (in ,sc   ,i)             Layer number
c      kSecPt   (in ,sc   ,i)             Section point
c      nstatv   (in ,sc   ,i)             Number of state variables
c      nprop    (in ,sc   ,i)             size of mat properties array    
c
c      prop     (dp ,ar(*),i)             mat properties array    
c                                         This array is passed all the creep
c                                         constants defined by command
c                                         TBDATA associated with TB,CREEP
c                                         (do not use prop(13), as it is used 
c                                         elsewhere)
c                                         at temperature temp.
c      time                               Current time
c      dtime                              Current time increment
c      temp                               Current temperature
c      dtemp                              Current temperature increment
c      toffst   (dp, sc,   i)             temperature offset from absolute zero
c      seqv     (dp ,sc  , i)             equivalent effective stress
c      creqv    (dp ,sc  , i)             equivalent effective creep strain
c      pres     (dp ,sc  , i)             hydrostatic pressure stress, -(Sxx+Syy+Szz)/3
c                                         note that: constitutive consistency is not accounted for
c                                         if creep strains are function of pressure 
c
c     input output arguments              input desc     / output desc
c     ======================              ==========       ===========
c      Ustatev  (dp,ar(*), i/o)           user defined iinternal state variables at 
c                                         time 't' / 't+dt'.
c                                         This array will be passed in containing the 
c                                         values of these variables at start of the 
c                                         time increment. They must be updated in this
c                                         subroutine to their values at the end of 
c                                         time increment, if any of these internal 
c                                         state variables are associated with the 
c                                         creep behavior.
c
c     output arguments
c     ================
c      delcr    (dp ,sc  , o)             incremental creep strain
c      dcrda    (dp,ar(*), o)             output array
c                                         dcrda(1) - derivitive of incremental creep 
c                                                    strain to effective stress
c                                         dcrda(2) - derivitive of incremental creep 
c                                                    strain to creep strain
c
c     local variables
c     ===============
c      c1,c2,c3,c4 (dp, sc, l)            temporary variables as creep constants 
c      con1        (dp ,sc, l)            temporary variable
c      t           (dp ,sc, l)            temporary variable
c
c*************************************************************************
c
c --- parameters
c
#include "impcom.inc"

      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO = 0.0d0)

      
c --- argument list
c
      INTEGER          ldstep, isubst, matId , elemId,
     &                 kDInPt, kLayer, kSecPt, nstatv,
     &                 impflg, nprop ,trig
      DOUBLE PRECISION dtime , time  , temp  , dtemp , toffst,
     &                 creqv , seqv  , pres
      DOUBLE PRECISION prop(*), dcrda(*), Ustatev(nstatv)
c
c --- local variables
c
      DOUBLE PRECISION B    , n    , C    , m    ,
     &                 con1  , delcr , t, w0, w , dw,
     &                 tmp1  , tmp2 , tmp3      
      DOUBLE PRECISION E, K1c, Sk, bf, Cyctime, Nf, DNf, Nut,
     &                   w0cr, w0f, wcr, wf ,dwfdN, Ndt, dwcr, dwf
c*************************************************************************
c
c *** skip when stress and creep strain are all zero
      if (seqv.LE.ZERO.AND.creqv.LE.ZERO) GO TO 990
c *** add temperature off set
      t       = temp + toffst
c *** Primary creep function 
c        de/dt := B * (seqv/(1-w) ^ n
c *** Primary creep damage function       
c        dwc/dt := C * (seqv/(1-wc) ^ m  
c *** Primary Fatigue damage function (Y Duyi, W  Zhenlim. IJFatigue 23/2001)
c        wf =-Df/LN(Nf)*LN(1-N/Nf)
c *** Summary damage
c        w = wc / (1-wf) + wf / (1-wc)
c     —читываем свойства материала      
      B       = prop(1) 
      n       = prop(2)
      C       = prop(3)
      m       = prop(4)
      E       = prop(5)
      K1c     = prop(6)
      Cyctime = prop(7)
      Sk      = Prop(8)
      bf      = Prop(9)
      trig    = Prop(10)
c вводим ее дл€ тестовой отладки
c      trig    = 1
      w0      = zero
      w0f     = zero
      wf      = zero
      w0cr    = zero
      wcr     = zero
      w       = zero
c *** берем значение функций с предыдущей итерации
         w0 = Ustatev(1)
           w0f = Ustatev(2)
            w0cr = Ustatev(3)
c ***************************      
c ***********Fatigue*********
c ***************************
c  в основу положена модель предолженна€ в статье
c     Dattoma V, Giancane S, Nobile R, et al. (2006) 
c     Ftg life predict. under variable loading based on a new non-linear continuum dmg. mech. model. 
c            International Journal of Fatigue 28: 89Ц95.
      Nf =(seqv/Sk)**(1/bf)
      DNf= 1-(seqv*seqv)/(2*E*K1c)
c         «десь будет мутна€ процедура согласовани€ текущей поврежденности
c     с тем, которую мы бы получили в результате одноосных испытаний
c     ѕо текущему значению поврежденности определ€ем, какой был бы цикл 
c     при одноосных испытани€х. Ќа основании этого определ€ем скорость 
c     накоплени€ повреждений.
c     » так сначала определ€ем цикл. ќн не соотноситс€ с реальным циклом
      con1=-w0*LOG(Nf)/DNf
      Nut=Nf*(1-EXP(con1))
c     определ€ем скорость накоплени€ повреждений на данном цикле
      dwfdN = DNf/((Nf-Nut)*LOG(Nf))
c     определ€ем количество циклов за приращение времени   
      Ndt=dtime*3600/Cyctime
c     параметр поврежденности в конце итерации
      wf = -DNf/LOG(Nf)*LOG(1-(Nut + Ndt)/Nf)
      dwf=wf-w0
      IF(trig.eq.1) dwf=0
      
c ***************************      
c ***********CREEP***********
c ***************************
        
c *** константа интегрировани€ дл€ полузчести
c      con1    = -1/(m+1)     
      con1 = - (1- w0)**(m+1)/(m+1)-C*seqv**m*(time-dtime)
c *** прогнозируем значение функции повреждений в конце итерации  
      tmp1=-(m+1)*(C*seqv**m*(time)+con1)
      IF(tmp1.le.0) THEN 
      wcr = 0
      ELSE    
      wcr = 1-(tmp1)**(1/(m+1))
      END IF
      IF(wcr.gt.1) w = 0.99999999    
      dwcr=wcr-w0
      IF(trig.eq.2) dwcr=0
      
c ***************************      
c ********Interaction********
c *************************** 
      
      dw=dwcr/(1-dwf)+dwf/(1-dwcr)
      w=w0+dw
c *** calculate incremental creep strain      
      IF(creqv .le. TINY) creqv = sqrt(TINY)
      delcr   = ZERO
      
      
      
      
      IF(B.gt.ZERO) delcr   = B*((Seqv/(1-w))**n)*dtime

c *** derivitive of incremental creep strain to effective stress
      tmp1=(1-w)*(m+1)*(con1+C*seqv**m*time)
      tmp2=C*seqv*seqv**(m-1)*time*m
      tmp3=B*n*dtime*(seqv/(1-w))**(n-1)
           
      dcrda(1)= tmp3*(1/(1-w)-tmp2/tmp1)

c *** derivitive of incremental creep strain to effective creep strain
      dcrda(2)= 0
c *** write the effective creep strain to last state variable for verification
      if (nstatv .gt. 0) then
         Ustatev(1) = w
         Ustatev(2) = Ustatev(2) + dwf
         Ustatev(3) = Ustatev(3) + dwcr
      end if
 990  continue
      return
      end
