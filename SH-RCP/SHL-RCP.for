c<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
c SHL-RCP: Sharp Lagrangian method for solving reverse convex programming problems
c<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

c<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
c Main problem
c=====================================================================
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      DOUBLE PRECISION spoint(20,maxvar)
      COMMON /cnselect/nselect,/cnstart/nstart,/cnconst/nconst
     1  ,/cnstarting/nstarting,/crandom/nrandom,/cspoint/spoint
    !  open(42,file='Results-reordered-single.txt')
    !  open(43,file='startingpoints1.txt')
      open(44,file='Results.txt')
      open(78,file='startingpoints.txt',status='old',form='formatted')
c=====================================================================
c Input data   
c=====================================================================
      PRINT *,' '
      PRINT *,'For single starting point press 1, for many press 2:'      
      READ *,nstarting
      PRINT *,' '
      if(nstarting.eq.2) then
       PRINT *,'Input file is available: press 1, if yes, 2 otherwise:'
       READ *,nrandom
       PRINT *,'Enter the number of starting points:'      
       READ *,nstart  
      end if
      if(nstarting.eq.1) nstart=1      
c----------------------------------------------------------------------
c      write(42,141) 
c141   format('Prob  ','   n','           In. obj.','       In. const.'
c     1 ,'      Optval-SL','       Const-SL'
c     1 ,'      Optimal val.','   Final const.','   #f1.eval.'
c     2 ,'   #f1.grad','   #con.eval.','   #g1.grad','    #g2.grad'
c     3 ,'     CPU')
c      write(42,*)
c----------------------------------------------------------------------
      write(44,151) 
151   format('Prob  ','   n','   Optimal val.','   Final const.'
     1 ,'   #f1.eval.','   #f1.grad','   #con.eval.','   #g1.grad'
     2 ,'    #g2.grad','    #con.aver','    f.eval.tot','   grad.tot'
     3 ,'     CPU')
      write(44,*) 
c======================================================================
      if(nrandom.eq.1) then
       do i=1,nstart+1
        read(78,*,END=901,ERR=900) (spoint(i,k),k=1,200)
        nrecord=i
       end do
  900  stop 'Error in input file'       
  901  WRITE(42,*) 'Input complete. Number of records: ',nrecord
       WRITE(42,*)
      end if
c----------------------------------------------------------------------
      jp=14                                                           ! number of test problems
      do i=1,jp
       nselect=i
       print *,nselect
!**********************************************************************          
       if(i.eq.1) then                                                  !(RCP-1)
           n=2
           nconst=1
           call sharp(n)
       end if  

       if(i.eq.2) then                                                  !(RCP-2)
           n=2
           nconst=4
           call sharp(n)
       end if

       if(i.eq.3) then                                                  !(RCP-3)
           n=2
           nconst=4                                                     
           call sharp(n)
       end if        
       
       if(i.eq.4) then                                                  !(RCP-4)
           n=2
           nconst=1
           call sharp(n)
       end if        
       
       if(i.eq.5) then                                                  !(RCP-5) 
           n=2
           nconst=3
           call sharp(n)
       end if        
       
       if(i.eq.6) then                                                  !(RCP-6)
           nconst=1
           do k=1,4
            if(k.eq.1) n=3
            if(k.eq.2) n=5
            if(k.eq.3) n=10
            if(k.eq.4) n=20
            if(k.eq.5) n=50
            if(k.eq.6) n=100
            if(k.eq.7) n=200
            if(k.eq.8) n=400            
            call sharp(n)
           end do 
       end if       
       
       if(i.eq.7) then                                                  !(RCP-7)
           nconst=1
            do k=1,12
            if(k.eq.1) n=2                                                       
            if(k.eq.2) n=3
            if(k.eq.3) n=4
            if(k.eq.4) n=5
            if(k.eq.5) n=6
            if(k.eq.6) n=7
            if(k.eq.7) n=8            
            if(k.eq.8) n=9
            if(k.eq.9) n=10
            if(k.eq.10) n=11
            if(k.eq.11) n=12
            if(k.eq.12) n=20
           call sharp(n)            
           end do             
       end if         
       
       if(i.eq.8) then                                                  !(RCP-8)
           n=2
           nconst=7
           call sharp(n)
       end if        
       
       if(i.eq.9) then                                                  !(RCP-9)
           nconst=1
           do k=1,6                                                 
            if(k.eq.1) n=2
            if(k.eq.2) n=5
            if(k.eq.3) n=10
            if(k.eq.4) n=20
            if(k.eq.5) n=50
            if(k.eq.6) n=100  
            if(k.eq.7) n=200  
           call sharp(n)
           end do           
       end if        
       
       if(i.eq.10) then                                                 !(RCP-10)
           n=3
           nconst=3
           call sharp(n)
       end if        
       
       if(i.eq.11) then                                                 !(RCP-11)
           n=2
           nconst=3
           call sharp(n)
       end if        
       
       if(i.eq.12) then                                                 !(RCP-12)
           n=2
           nconst=2
           call sharp(n)
       end if        
              
       if(i.eq.13) then                                                 !(RCP-13)
           n=3
           nconst=2                                                    
           call sharp(n)           
       end if  
       
       if(i.eq.14) then                                                 !(RCP-14) 
           nconst=3
           do k=1,6
            if(k.eq.1) n=2
            if(k.eq.2) n=5
            if(k.eq.3) n=10
            if(k.eq.4) n=20
            if(k.eq.5) n=50
            if(k.eq.6) n=100
            if(k.eq.7) n=200            
            call sharp(n)
           end do 
       end if   
      end do                 
c=======================================================
c      CLOSE(42)
c      CLOSE(43)
      CLOSE(44)
      CLOSE(78)
      STOP
      END
      
c**************************************************************************
c  begining of sharp ...
c description of box constraints
c--------------------------------------------------------------------------
      subroutine sharp(n)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxit=10000)
      double precision x(maxvar), xbar(maxvar), xl(maxvar), xu(maxvar)
     1 ,spoint(20,maxvar) 
      COMMON /csize/m,/cbox/xl,xu,/cpen/pen,/csm/sm,dbox,/cpenbox/penbox
     1 ,/cnf1/nf1,/cngrad1/ngrad1,/cuc/u,c,/cnselect/nselect,/cxbar/xbar
     2 ,/cind/ind,/cnstart/nstart,/cind1/ind1,/cncon/ncon
     3 ,/cncong1/ncong1,/cncong2/ncong2,/cnstarting/nstarting
     4 ,/crandom/nrandom,/cspoint/spoint,/cnconst/nconst
c=====================================================================
       if(nselect.eq.1) then                                            !(RCP-1)
        do i=1,n                                                          
         xl(i) = 0.0d+00                                                  
         xu(i) = 1.0d+01
        end do
       end if
       
       if(nselect.eq.2) then                                            !(RCP-2)
        xl(1) = 0.0d+00                                                  
        xu(1) = 8.0d+00
        xl(2) = 0.0d+00
        xu(2) = 6.0d+00
       end if
       
       if(nselect.eq.3) then                                            !(RCP-3)
        xl(1) = 0.0d+00                                                  
        xu(1) = 1.0d+01
        xl(2) = 9.0d-01
        xu(2) = 2.1d+00
       end if        
       
       if(nselect.eq.4) then 
        do i=1,n
         xl(i) = -2.0d+00                                                !(RCP-4)
         xu(i) = 2.0d+00
        end do
       end if        
       
       if(nselect.eq.5) then
        do i=1,n                                                          
         xl(i) = -1.0d+01                                                !(RCP-5) 
         xu(i) = 1.0d+01
        end do
       end if       
       
       if(nselect.eq.6) then                                            !(RCP-6)     
        do i=1,n
          xl(i) = -1.0d+00
          xu(i) = 1.0d+00
        end do       
       end if         
       
       if(nselect.eq.7) then
        do i=1,n                                                          
         xl(i) = 0.0d+00                                                 !(RCP-7)
         xu(i) = 1.0d+00
        end do
       end if       
       
       if(nselect.eq.8) then
        do i=1,n                                                          
         xl(i) = 0.0d+00                                                 !(RCP-8)
         xu(i) = 1.0d+00
        end do
       end if        
       
       if(nselect.eq.9) then
        do i=1,n                                                          
         xl(i) = 0.0d+00                                                 !(RCP-9)
         xu(i) = 1.0d+01
        end do
       end if        
       
       if(nselect.eq.10) then
        do i=1,n                                                          
         xl(i) = -1.0d+01                                               !(RCP-10) 
         xu(i) = 1.0d+01
        end do
       end if        
       
       if(nselect.eq.11) then
        do i=1,n                                                          
         xl(i) = -1.0d+02                                                !(RCP-11)
         xu(i) = 1.0d+02
        end do
       end if        
       
       if(nselect.eq.12) then
        do i=1,n                                                          
         xl(i) = -1.0d+02                                                !(RCP-12)
         xu(i) = 1.0d+02
        end do
       end if        
       
       if(nselect.eq.13) then
        do i=1,n                                                          
         xl(i) = 0.0d+00                                                 !(RCP-13) 
         xu(i) = 1.0d+02
        end do
       end if  
       
       if(nselect.eq.14) then
        do i=1,n                                                          
         xl(i) = -1.0d+02                                                !(RCP-14)
         xu(i) = 1.0d+02
        end do
       end if      
              
       nprob1=nselect       
c=====================================================================
c   Generating starting points
c=====================================================================
      if(nrandom.eq.2) then
       PRINT *,'Generating starting points'
       nrecord1=0
       d3=1.0d+30
       do i=1,n
        d3=dmin1(d3,xu(i)-xl(i))
       end do
       d3=1.0d-02*d3
c========================================================================
       do j=1,nstart
  1     do i=1,n
  20      continue
          call random_seed()
          call random_number(beta)
          x(i)=beta*xl(i)+(1.0d+00-beta)*xu(i)
          if (i.gt.1) then
           d4=dabs(x(i)-x(i-1))
           IF(d4.lt.1.0d-04) GO TO 20
          end if
        end do
        do k=1,nrecord1
         d2=0.0d+00
         do j1=1,n
          d2=d2+(x(j1)-spoint(k,j1))**2
         end do
         IF(d2.lt.d3) GO TO 1
        end do
        nrecord1=nrecord1+1
        do j1=1,n
         spoint(nrecord1,j1)=x(j1)
        end do
       end do
       do i=1,nrecord1
c        write(43,243) (spoint(i,j),j=1,n)
       end do
c 243    format(200f8.3)
      end if
c=====================================================================
c  Problem parameters
c=====================================================================
      m = n
      tlimit=7.2d+03
      eps=1.0d-03
      penbox=1.0d+03
c=====================================================================
      do istart=1,nstart
       nf1=0
       ngrad1=0
       ncon=0
       ncong1=0
       ncong2=0
       call cpu_time(time1)
       if(nstarting.gt.1) then 
        do i=1,m
          x(i)=spoint(istart,i)
        end do
       end if
!*********************************************************************** 
! select starting points for each test problem        
       if(nstarting.eq.1) then
        SELECT CASE(nselect) 
!**********************************************************************
        CASE(1) 
         x(1)=1.0d+00                                                   !(RCP-1) 
         x(2)=4.0d+00  

        CASE(2)                                                         !(RCP-2)     
         x(1)=8.0d+00
         x(2)=2.0d+00
         
        CASE(3)                                                        !(RCP-3)       
          x(1)=2.0d+00
          x(2)=2.0d+00           
         
        CASE(4)                                                         !(RCP-4)       
         x(1)=-1.0d+00
         x(2)=0.0d+00         
         
        CASE(5)                                                         !(RCP-5) 
         do i=1,m
          x(i)=-5.0d-01
         end do         
         
        CASE(6)                                                         !(RCP-6)
         x(1)=1.0d+00
         do i=2,m
          x(i)=-1.0d+00
         end do          
         
        CASE(7)                                                         !(RCP-7)      
         do i=1,m
          x(i)=0.0d+00
         end do         
         
         CASE(8)                                                        !(RCP-8) 
          x(1)=0.5d+00
          x(2)=0.5d+00          
         
         CASE(9) 
          do i=1,m                                                       !(RCP-9)        
           x(i)=0.0d+00  
          end do         
         
         CASE(10)                                                        !(RCP-10)       
          x(1)=2.0d+00
          x(2)=1.0d+00 
          x(3)=2.0d+00          

         CASE(11)                                                       !(RCP-11)        
          x(1)=0.5d+00
          x(2)=0.5d+00        

         CASE(12)                                                       !(RCP-12)        
          x(1)=1.0d+00
          x(2)=1.0d+00 

         CASE(13) 
          x(1)=1.0d+00                                                  !(RCP-13)        
          x(2)=2.0d+00
          x(3)=2.0d+00 
             
         CASE(14)                                                       !(RCP-14)     
          DO i=1,n
           x(i)=1.0d+00
          END DO 
                                                                                                   
        END SELECT 
       end if                         
!***********************************************************************                                                          
       u=1.0d+02
       c=1.0d+02
       ind1=0          
       do i=1,m
        xbar(i)=x(i)
       end do
       call func1(x,f)
       finit=f
       call const12(x,g1,g2)              
       g=g1-g2                                                           ! g+ 
       ginit=g
       print 441,finit,ginit
 441   format(2f14.4)
c       go to 2
       if(ginit.le.eps) then
         fsl=finit
         gsl=ginit
         ind1=1
         go to 2
       end if
       call sharplinesearch(x,g)
       call func1(x,f1)
       fsl=f1
       call const12(x,g1,g2)              
       gsl=g1-g2     
  2    continue
       pen=1.0d+01
       if(nselect.eq.6) pen=5.0d-01
       if(nselect.eq.7) pen=1.0d+02
       if(nselect.eq.2) pen=1.0d+03
       if(nselect.eq.3) pen=5.0d+02
       if(nselect.eq.9) pen=5.0d+00
       ind=2
       call optimum(x,fvalue)                                    
       call const12(x,g1,g2)
       g=g1-g2
       print 441,fvalue,g                                                ! g+ 
       call cpu_time(time4)
       time5=time4-time1
       WRITE(42,41) nprob1,n,finit,ginit,fsl,gsl,fvalue,g,nf1,ngrad1
     1  ,ncon,ncong1,ncong2,time5

       nfaver = floor((dble(nf1+nconst*ncon))/dble(nconst+1))
       ncong3 = floor(dble(ncong1+ncong2)/2)
       ngaver = floor((dble(ngrad1+nconst*ncong3))/dble(nconst+1))

       WRITE(44,451) nprob1,n,fvalue,g,nf1,ngrad1,ncon,ncong1,ncong2
     1  ,ncong3,nfaver,ngaver,time5
      END DO
  41  FORMAT(i4,i6,f20.4,f15.5,f15.6,f15.5,f15.6,f15.5,5i12,f12.4)
 451  FORMAT(i4,i6,2f15.6,8i12,f12.4)
      RETURN
      END

c**************************************************************************
c  begining of under and overestimations - Sharp Lagrangian estimate
c--------------------------------------------------------------------------
      subroutine sharplinesearch(x,g1)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000, maxcon=100)
      double precision x(maxvar),x1(maxvar),x2(maxvar),w2(maxdg,maxvar)
     1 , x3(maxvar)
      COMMON /csize/m,/cuc/u,c,/cw12/w2,/cnsub/nsub
       beta=1.0d-04
       teta=5.0d+00
       eps=1.0d-03
       tau=1.0d-01
       g2=1.0d+30
       do i=1,m
        x3(i)=x(i)
       end do
       tau=tau/2.0d+00
  2    k=0
       tau=2.0d+00*tau
       call esubdif(tau)
       call sharplag(x3,x1,hbar)
       call const12(x1,h1,h2)
       g1=h1-h2
       subg=g1
       step=1.0d-03
       step=step/teta
  1    step=step*teta
       k=k+1
       if(k.gt.20) then
        do i=1,m
         x(i)=x2(i)
        end do
        g1=g2       
        return
       end if
       u=dmax1(0.0d+00,u-step*subg)
       c=dmin1(5.0d+04,c+step*subg)
       do i=1,m
        x3(i)=x1(i)
       end do
       call sharplag(x3,x1,hbar1)
       call const12(x1,h1,h2)
       g1=h1-h2
       if(g1.le.eps) then
        do i=1,m
         x(i)=x1(i)
        end do       
        return
       end if
       if(g2.gt.g1) then
        g2=g1
        do i=1,m
         x2(i)=x1(i)
        end do       
       end if
       hbar2=hbar1-hbar-beta*step*subg
       if(hbar2.ge.0.0d+00) then
        do i=1,m
         x3(i)=x1(i)
        end do
        go to 2
       end if
       go to 1
      end subroutine

      subroutine sharplag(x,x1,f1)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000, maxcon=100)
      double precision x(maxvar), w2(maxdg,maxvar),x1(maxvar),x2(maxvar)  
      COMMON /csize/m,/cuc/u,c,/cw12/w2,/cnsub/nsub,/ccur/jcur
     1 ,/cind/ind
       ind=1
       do i=1,m
        x2(i)=x(i)
       end do
       f1=1.0d+30
       do j=1,nsub
         jcur=j
         call optimum(x2,fvalue)
         if(f1.ge.fvalue) then
          f1=fvalue
          do k=1,m
           x1(k)=x2(k)
          end do
         end if
       end do
      end subroutine
c=================================================================
c Calculation of e-subdifferentials of f_2 and h_2.
c==================================================================
      subroutine esubdif(tau)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000)
      double precision x(maxvar), d(maxvar), x3(maxvar),w2(maxdg,maxvar)
     1 , v2(maxvar)
      common /csize/m,/cw12/w2,/cnsub/nsub,/cxbar/x
c====================================================================
      nlimit=min(6,2*m)
      m1=2*m
      nsub=0
      do nb=1,m1
       nsub=nsub+1
       if(nsub.gt.nlimit) return
       do j=1,m
        d(j)=0.0d+00
       end do

       n1=nb/2
       n2=2*n1
       if(nb.eq.n2) then
         d(n1)=-1.0d+00
       end if
       if(nb.ne.n2) then
         n3=(nb+1)/2
         d(n3)=1.0d+00
       end if

       do j=1,m
         x3(j)=x(j)+tau*d(j)
       end do
       call gradientC2(x3,v2)
       do j=1,m
          w2(nsub,j)=v2(j)
       end do
      end do
c-------------------------------------------------------------------
      return
      end

c=====================================================================
      subroutine fest(x,f)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000,maxdg=1000)
      double precision x(maxvar), w2(maxdg,maxvar),xbar(maxvar)
     1 ,xl(maxvar),xu(maxvar)
      COMMON /csize/m,/cw12/w2,/cxbar/xbar,/cuc/u,c,/cbox/xl,xu
     1 ,/ccur/jcur,/cpenbox/penbox  
       call func1(x,f1)
       call const12(x,h1,h2)
       f3=f1+(c-u)*h1
       d1=0.0d+00
       d2=0.0d+00
       do k=1,m
          d1=d1+(c-u)*w2(jcur,k)*(x(k)-xbar(k))
          d2=d2+dmax1(0.0d+00,xl(k)-x(k),x(k)-xu(k))
       end do
       f=f3-d1+penbox*d2
      return
      end

c=====================================================================
      subroutine subgradest(x,v)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000)
      double precision x(maxvar),v(maxvar),gradf1(maxvar),xu(maxvar)
     1 ,gradh1(maxvar), w2(maxdg,maxvar),xl(maxvar)
      common /csize/m,/cuc/u,c,/ccur/jcur,/cw12/w2,/cbox/xl,xu
     1 ,/cpenbox/penbox
      call gradient1(x,gradf1)
      call gradientC1(x,gradh1)
      do k=1,m
       v(k)=gradf1(k)+(c-u)*gradh1(k)-(c-u)*w2(jcur,k)
      end do
      do k=1,m
       if(x(k).le.xl(k)) v(k)=v(k)-penbox
       if(x(k).ge.xu(k)) v(k)=v(k)+penbox
      end do
      return
      end

c================================================================
c Penalty function part: augsub minimizes the penalty function. 
c It includes subroutines: funcp1, funcp2, gradp1, gradp2
c  subgrad1, armijo1.  
c Augmented subgradient method (AUGSUB) is applied as a local search
c================================================================
      subroutine optimum(x,f)                    
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar)
      COMMON /cind/ind,/csize/m
      m1=m 
      if(ind.eq.1) then
       call TMPBNGC(m1,x,f)
      end if
      if(ind.eq.2) then 
       call augsub(x)
       call func1(x,f)
      end if 
      return
      end

c -------------------------------------------------------------------------- 
      subroutine augsub(x)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000, maxit=100000)
      allocatable fvalues(:),w(:,:),prod(:,:)
      double precision x(maxvar),x1(maxvar),g(maxvar),v(maxvar)
     1 ,z(maxdg), grad2(maxvar)
      INTEGER ij(maxdg)                                                 
      common /cvert/jvertex,/cij/ij,/cz/z,/ckmin/kmin,/citer/niter                   
      common /csize/m,/cgrad2/grad2,/cind1/ind1    
      allocate(fvalues(maxit))
      allocate(w(maxdg,maxvar))
      allocate(prod(maxdg,maxdg))
      dist1=1.0d-07
      c1=-2.0d-01
      div=1.0d-01
      eps0=1.0d-07
      tau0=1.0d+00
      taumin=1.0d-07*tau0
      if(ind1.eq.0) then
       tau0=1.0d+00
       taumin=1.0d-09*tau0
      end if
      maxiter=10000
      sdif=1.0d-05
      mturn=5
      niter=0
      nbundle=min(140,2*m)
c -------------------------------------------------------------------------- 
      tau=tau0/div
      call funcp1(x,fmax)
      call funcp2(x,fmin)
      f2=fmax-fmin
  1   tau=div*tau
c! ----------------------------------------      
c!    Tau STOPPING CONDITION 
c! ----------------------------------------  
      IF(tau.lt.taumin) return
      do i=1,m
       g(i)=1.0d+00/dsqrt(DBLE(m))
      end do
      nnew=0
c -------------------------------------------------------------------------- 
   2  niter=niter+1
c! ---------------------------------------------      
c!    Iteration limit STOPPING CONDITION 
c! ---------------------------------------------     
        IF(niter.gt.maxiter) RETURN
        nnew=nnew+1
        f1=f2
        print *,tau,f2
        fmax1=fmax        
        fvalues(niter)=f1
c---------------------------------------------------------------
        if (nnew.gt.mturn) then
         mturn2=niter-mturn+1
         ratio1=(fvalues(mturn2)-f1)/(dabs(f1)+1.0d+00)
         IF(ratio1.LT.sdif) GO TO 1
        end if
        if (nnew.GE.(2*mturn)) then
         mturn2=niter-2*mturn+1
         ratio1=(fvalues(mturn2)-f1)/(dabs(f1)+1.0d+00)
         IF(ratio1.LT.(1.0d+01*sdif)) GO TO 1
        end if
c--------------------------------------------------------------
        do nsub=1,nbundle
            call subgrad(nsub,fmax1,x,tau,g,v)
            dotprod=0.0d+00
            do i=1,m+1
             dotprod=dotprod+v(i)*v(i)
            end do
            r=dsqrt(dotprod)
            IF(r.lt.eps0) GO TO 1          
            IF(nsub.eq.1) then
                rmean=r
                kmin=1
                rmin=r
            END if
            IF(nsub.gt.1) then
                rmin=dmin1(rmin,r)
                IF(r.eq.rmin) kmin=nsub
                rmean=((nsub-1)*rmean+r)/dble(nsub)
            END if
            toler=dmax1(eps0,dist1*rmean)
            do i=1,nsub-1
             prod(nsub,i)=0.0d+00
             do j=1,m+1
              prod(nsub,i)=prod(nsub,i)+w(i,j)*v(j)
             end do
             prod(i,nsub)=prod(nsub,i)
            end do
            prod(nsub,nsub)=dotprod 

            do i=1,m+1
             w(nsub,i)=v(i)
            end do
            call wolfe(nsub,prod)

            do i=1,m+1
             v(i)=0.0d+00
             do j=1,jvertex
              v(i)=v(i)+w(ij(j),i)*z(j)
             END do
            END do 
c -------------------------------------------------------------------------- 
            r=0.d+00
            do i=1,m+1 
             r=r+v(i)*v(i)
            end do
            r=dsqrt(r)
            if(r.lt.toler) GO TO 1
c -------------------------------------------------------------------------- 
            do i=1,m
              g(i)=-v(i)/r
              x1(i)=x(i)+tau*g(i)
            end do
c -------------------------------------------------------------------------- 
            call funcp1(x1,fmax4)
            call funcp2(x1,fmin4)
            f4=fmax4-fmin4
            f3=(f4-f1)/tau
            decreas=c1*r
            if(f3.le.decreas) then
                  fmax=fmax4 
                  call armijo(x,g,f1,f5,fmax,f5max,f4,tau,sigma,r)
                  f2=f5
                  fmax=f5max
                  do i=1,m
                   x(i)=x(i)+sigma*g(i)
                  end do
                  tau=1.2d+00*tau
                  GO TO 2
            end if
        END do
      go to 1
      return
      end

c!==============================================================
c!  Subroutines Wolfe and Equations solves quadratic
c!  programming problem, to find
c!  descent direction, Step 3, Algorithm 2.
c!===============================================================

      subroutine wolfe(ndg,prod)
      implicit double precision (a-h,o-z)
      PARAMETER(maxdg=1000)
      common /w01/a,/cij/ij,/cvert/jvertex,/cz/z,/ckmin/kmin
      INTEGER ij(maxdg)
      double precision z(maxdg),z1(maxdg),a(maxdg,maxdg)
     1 ,prod(maxdg,maxdg)
      j9=0
      jmax=1000*ndg
      jvertex=1
      ij(1)=kmin
      z(1)=1.0d+00
c!=======================================
c!  To calculate X
c!=======================================
 1    r=0.0d+00
      do i=1,jvertex
       do j=1,jvertex
        r=r+z(i)*z(j)*prod(ij(j),ij(i))
       end do
      end do
      IF(ndg.eq.1) RETURN
c!========================================
c!  To calculate <X,P_J> and J
c!========================================
      t0=1.0d+30
      do i=1,ndg
        t1=0.0d+00
        do j=1,jvertex
          t1=t1+z(j)*prod(i,ij(j))
        end do
        if(t1.lt.t0) then
            t0=t1
            kmax=i
        end if
      end do
c!========================================
c!  First stopping criterion
c!========================================
      rm=prod(kmax,kmax)
      do j=1,jvertex
       rm=dmax1(rm,prod(ij(j),ij(j)))
      end do
      r2=r-1.0d-12*rm
      if(t0.gt.r2) RETURN
c!========================================
c!  Second stopping criterion
c!========================================
      do i=1,jvertex
       if(kmax.eq.ij(i)) RETURN
      end do
c!========================================
c! Step 1(e) from Wolfe's algorithm
c!========================================
      jvertex=jvertex+1
      ij(jvertex)=kmax
      z(jvertex)=0.0d+00
c!========================================
 2    do i=1,jvertex
       do j=1,jvertex
        a(j,i)=1.0d+00+prod(ij(j),ij(i))
       end do
      end do
      j9=j9+1
      if(j9.gt.jmax) RETURN
      call equations(jvertex,z1)
      do i=1,jvertex
       if(z1(i).le.1.0d-10) go to 3
      end do      
      do i=1,jvertex
       z(i)=z1(i)
      end do
      go to 1
  3   teta=1.0d+00
      do i=1,jvertex
       z5=z(i)-z1(i)
       if(z5.gt.1.0d-10) teta=dmin1(teta,z(i)/z5)
      end do
      do i=1,jvertex
       z(i)=(1.0d+00-teta)*z(i)+teta*z1(i)
       if(z(i).le.1.0d-10) then
           z(i)=0.0d+00
           kzero=i
       end if
      end do
      j2=0
      do i=1,jvertex
       IF(i.ne.kzero) then
           j2=j2+1
           ij(j2)=ij(i)
           z(j2)=z(i)
       END if
      end do
      jvertex=j2
      go to 2
      return
      end

c===================================================================
      subroutine equations(n,z1)
      implicit double precision (a-h,o-z)
      PARAMETER(maxdg=1000)
      allocatable b(:,:)
      common /w01/a
      double precision a(maxdg,maxdg),z1(maxdg)
      allocate(b(maxdg,maxdg)) 
      do i=1,n
       do j=1,n
        b(j,i)=a(j,i)
       end do
       b(n+1,i)=1.0d+00
      end do
      do i=1,n
       r=b(i,i)
       if(r.le.1.0d-10) r=1.0d-10
       do j=i,n+1
        b(j,i)=b(j,i)/r
       end do
       do k=i+1,n+1
        do j=i+1,n
         b(k,j)=b(k,j)-b(k,i)*b(i,j)
        end do
       end do
      end do
      z1(n)=b(n+1,n)
      do i=1,n-1
        k=n-i
        z1(k)=b(n+1,k)
        do j=k+1,n
         z1(k)=z1(k)-b(j,k)*z1(j)
        END do
      end do
      z2=0.0d+00
      do i=1,n
       z2=z2+z1(i)
      end do
      do i=1,n
       z1(i)=z1(i)/z2
      end do
      deallocate(b)
      return
      end

c=====================================================================
c Subroutine subgrad1 calculates subgradients 
c=====================================================================
      subroutine subgrad(nsub,fmax1,x,tau,g,v)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000)
      double precision x1(maxvar),g(maxvar),x(maxvar),grad1(maxvar)
     1 ,grad2(maxvar),v(maxvar)
      common /csize/m,/cgrad2/grad2
      
      do k=1,m
        x1(k)=x(k)+tau*g(k)
      end do

      call gradp1(x1,grad1)
      call funcp1(x1,ftau) 
      if(nsub.eq.1) call gradp2(x,grad2)
      alpha=ftau-fmax1
      do i=1,m
       v(i)=grad1(i)-grad2(i)
       alpha=alpha-tau*grad1(i)*g(i)
      end do
      v(m+1)=alpha
      return
      end

c===========================================================
c Line search (Armijo-type), Step 5 AUGSUB
c===========================================================
      subroutine armijo(x,g,f1,f5,f6,f5max,f4,tau,sigma,r)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxdg=1000)
      common /csize/m
      double precision x(maxvar),g(maxvar),x1(maxvar)
      sigma=tau
      f5=f4
      f5max=f6
      k=0
      sigma1=tau
  1   k=k+1
      IF(k.gt.20) RETURN
      sigma1=2.0d+00*sigma1
      do i=1,m
       x1(i)=x(i)+sigma1*g(i)
      end do
      call funcp1(x1,fmax)
      call funcp2(x1,fmin)
      f50=fmax-fmin
      f30=f50-f1+1.0d-02*sigma1*r
      IF(f30.le.0.0d+00) then
       sigma=sigma1
       f5=f50
       f5max=fmax
       GO TO 1
      end if
      return
      end
      
c -------------------------------------------------------------------------- 
! funcp1 
c -------------------------------------------------------------------------- 

      subroutine funcp1(x,f) 
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar)
      COMMON /cpen/pen 
      call func1(x,f1)
      call const12(x,g1,g2)
      f=f1+pen*g1
      return
      end
c -------------------------------------------------------------------------- 

      subroutine funcp2(x,f) 
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar)
      COMMON /cpen/pen 
      call const12(x,g1,g2)
      f=pen*g2
      return
      end
c -------------------------------------------------------------------------- 

      subroutine gradp1(x,grad) 
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar),grad(maxvar),grad0(maxvar)
     1 ,grad1(maxvar)
      COMMON /csize/m,/cpen/pen 
      call gradient1(x,grad0)
      call gradientC1(x,grad1)
      do i=1,m
       grad(i)=grad0(i)+pen*grad1(i)
      end do
      return
      end
c -------------------------------------------------------------------------- 
 
      subroutine gradp2(x,grad) 
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar),grad(maxvar),grad2(maxvar)
      COMMON /csize/m,/cpen/pen 
      call gradientC2(x,grad2)
      do i=1,m
       grad(i)=pen*grad2(i)
      end do
      return
      end
c=================================================================
c End of AugSub
c=================================================================      

c**************************************************************************
c  begining of defining objective and constraint functions and their gradients
c--------------------------------------------------------------------------
! DC components of objective function f=f1 & f2=0 (box for X is added to f1)
c--------------------------------------------------------------------------
      subroutine func1(x,f)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar),XL(MAXVAR),XU(MAXVAR), y(maxvar)
      COMMON /cbox/XL,XU,/csize/m,/cnf1/nf1,/cnselect/nselect
     1 ,/cpenbox/penbox 
       nf1=nf1+1               
!**********************************************************************
       SELECT CASE(nselect)
       
        CASE(1)                                                         !(RCP-1) 
         f=x(1)

        CASE(2)                                                         !(RCP-2) 
         f=-1.5d+00*x(1)+x(2)
         
        CASE(3)                                                         !(RCP-3)      
           f=-x(1)           
         
        CASE(4)                                                         !(RCP-4)
         f=ABS(x(1)+1.0d+00)+x(2)**2         
         
        CASE(5)                                                         !(RCP-5)      
           f=dmax1(x(1),x(2))         
                  
        CASE(6)                                                         !(RCP-6) 
         y(1)=-2.5d-01
         do i=2,m
          y(i)=1.0d+00
         end do                                                              
         f=0.0d+00
         do i=1,m
          f=f+(x(i)-y(i))**2
         end do
         f=5.0d-01*f           
         
        CASE(7)                                                         !(RCP-7)
           f=x(1)         
         
        CASE(8)                                                         !(RCP-8)      
           f=x(1)+4.0d-01*x(2)          
         
        CASE(9) 
           f=0.0d+00                                                    !(RCP-9)      
           do i=1,m
            f=f+dble(i+1)*x(i)
           end do          
         
        CASE(10)                                                         !(RCP-10)      
           f=x(1)**2+x(2)**2+x(3)**2         
         
        CASE(11)                                                        !(RCP-11)      
           f=(x(1)-2.0d+00)**2+(x(2)-1.0d+00)**2            
        
        CASE(12)                                                        !(RCP-12)      
           f= abs(x(1))+abs(x(2)) 
                  
        CASE(13)                                                        !(RCP-13)      
           f=x(1)**2+3.0d+00*x(2)**2+4.0d+00*x(3)**2   
           
        CASE(14)                                                         !(RCP-14)
         m1 = floor(2.0d+00*m/3.0+00)  
         f = 0.0d+00
         DO i =1,m1
          f = f+x(i)**2
         END DO   
                                                                      
       END SELECT
       
       f1=0.0d+00                                                       !(box)
       do i=1,m
        f1=f1+dmax1(0.0d+00,xl(i)-x(i),x(i)-xu(i))
       end do
       f=f+penbox*f1 
             
       return
       end

c--------------------------------------------------------------------------
! Gradients of DC componenets of objective f1 
c--------------------------------------------------------------------------
      subroutine gradient1(x,grad)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000)
      double precision x(maxvar),grad(maxvar),XL(MAXVAR),XU(MAXVAR),
     1  y(maxvar)        
      COMMON /cbox/XL,XU,/csize/m,/cngrad1/ngrad1,/cnselect/nselect,
     1      /cn/n,/cpenbox/penbox
      ngrad1=ngrad1+1
     
      SELECT CASE(nselect)
!**********************************************************************
      CASE(1)                                                           !(RCP-1)
       grad(1)= 1.0d+00
       grad(2)= 0.0d+00     

      CASE(2)                                                           !(RCP-2)
       grad(1)= -1.5d+00
       grad(2)= 1.0d+00
       
      CASE(3)                                                           !(RCP-3)
       grad(1)= -1.0d+00
       grad(2)= 0.0d+00       
       
      CASE(4)                                                           !(RCP-4)
       x1=x(1)+1.0d+00                                                      
       IF(x1.le.0) THEN
            grad(1)=-1.0d+00 
         ELSE  
            grad(1)=1.0d+00
       END IF
       grad(2)=2.0d+00*x(2)       
       
      CASE(5)                                                           !(RCP-5) 
       IF(x(1).le.x(2)) THEN
          grad(1)=0.0d+00 
          grad(2)=1.0d+00
         ELSE  
          grad(1)=1.0d+00 
          grad(2)=0.0d+00
       END IF       
       
      CASE(6)                                                           !(RCP-6)
        y(1)=-2.5d-01
        do i=2,m
          y(i)=1.0d+00
        end do
        do i=1,m
          grad(i)=x(i)-y(i)
        end do        
       
      CASE(7)                                                           !(RCP-7)   
        grad(1) = 1.0d+00
        do i=2,m
          grad(i)=0.0d+00
        end do       
       
      CASE(8)                                                           !(RCP-8)
       grad(1)= 1.0d+00
       grad(2)= 4.0d-01        
       
      CASE(9)                                                          !(RCP-9)
        do i=1,m
          grad(i)=dble(i+1)
        end do        
       
      CASE(10)                                                           !(RCP-10)
       grad(1)= 2.0d+00*x(1)
       grad(2)= 2.0d+00*x(2)
       grad(3)= 2.0d+00*x(3)         
       
      CASE(11)                                                          !(RCP-11)
       grad(1)= 2.0d+00*(x(1)-2.0d+00)
       grad(2)= 2.0d+00*(x(2)-1.0d+00)       
       
      CASE(12)                                                          !(RCP-12)       
         DO i = 1,m
          IF (x(i)<=0.0d+00) THEN 
             grad(i) = -1.0d+00
            ELSE
             grad(i) = 1.0d+00
          END IF
         END DO        

      CASE(13)                                                          !(RCP-13)
       grad(1)= 2.0d+00*x(1) 
       grad(2)= 6.0d+00*x(2) 
       grad(3)= 8.0d+00*x(3) 
       
      CASE(14)                                                         !(RCP-14)
        m1 = floor(2.0d+00*m/3.0+00) 
        DO i = 1,m   
          grad(i) = 0.0d+00
        END DO        
        DO i =1,m1 
          grad(i) = 2.0d+00*x(i)
        END DO       
                                                        
      END SELECT  
            
      do i=1,m                                                          !(gradient Box)
        if(x(i).le.xl(i)) then
          grad(i)=grad(i)-penbox
        end if
        if(x(i).ge.xu(i)) then
          grad(i)=grad(i)+penbox
        end if
      end do              
      return
      end

c--------------------------------------------------------------------------
! DC constraint funcions and their gradients    g(const) = g1(const) -g2(const) 
c --------------------------------------------------------------------------
      subroutine const(x,g1,g2)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxcon=100)
      double precision x(maxvar),g1(maxcon),g2(maxcon)
      COMMON /cnc1/nc1,/cnselect/nselect,/csize/m 
!**********************************************************************  
      SELECT CASE(nselect)        
         CASE(1)                                                        !(RCP-1)
          !first constraint
           g1(1)= 0.0d+00
           g2(1)= x(1)**2-x(2)+1.0d+00 

      CASE(2)                                                           !(RCP-2)
          !first constraint  
           g1(1)= x(1)-x(2)-6.0d+00    
           g2(1)= 0.0d+00 

          !second constraint
           g1(2)= -2.0d+00*x(1)+x(2)-5.0d+00
           g2(2)= 0.0d+00
           
          !third constraint  
           g1(3)= 0.0d+00      
           g2(3)=x(1)**2-8.0d+00*x(1)+4.0d+00*x(2)**2-1.6d+01*x(2)
     1            +7.0d+00

        !fourth constraint  
           g1(4)= 0.0d+00     
           g2(4)= x(1)**2-1.6d+01*x(1)+2.25d+00*x(2)**2-2.25d+01*x(2)
     1      +4.725d+01
     
      CASE(3)                                                           !(RCP-3)
        ! first constraint
          g1(1)= 3.0d+00*x(1)-x(2)-6.0d+00
          g2(1)= 0.0d+00
           
        !second constraint  
          g1(2)= 3.0d+00*x(1)+x(2)-9.0d+00     
          g2(2)= 0.0d+00           
         
        !third constraint  
          g1(3)= 0.0d+00     
          g2(3)= (x(1)-2.0d+00)**2+(x(2)-1.0d+00)**2-1.0d+00
           
        !forth constraint  
          g1(4)= 0.0d+00   
          g2(4)= (x(1)-2.0d+00)**2+(x(2)-2.0d+00)**2-1.0d+00           
     
      CASE(4)                                                           !(RCP-4)
        !first constraint
          g1(1) = 0.0d+00
          g2(1) = x(1)**2+x(2)**2-2.0d+00     
     
      CASE(5)                                                           !(RCP-5) 
        ! first constraint
          g1(1) = 0.0d+00
          g2(1) = (x(1)+1.0d+00)**2+x(2)**2-1.0d+00                                  

        !second constraint  
          g1(2)= 0.0d+00    
          g2(2)= x(1)**2+(x(2)+1.0d+00)**2-1.0d+00 
      
        !third constraint  
          g1(3)= x(1)**2+x(2)**2-1.0d+01    
          g2(3)= 0.0d+00      
     
      CASE(6)                                                           !(RCP-6)    
        ! first constraint
          g1(1) = 0.0d+00
          g2(1) = -dble(m)+5.0d-01
          do i=1,m
           g2(1) = g2(1)+x(i)**2
          end do     
     
      CASE(7)                                                           !(RCP-7)
        ! first constraint
          g1(1) = 0.0d+00
          g2(1) = 0.0d+00
          do i=1,m
           g2(1)= g2(1)+x(i)**2 
          end do
          g2(1)=g2(1)- dble(m)+5.0d-01      
     
      CASE(8)                                                           !(RCP-8)
        ! first constraint
          g1(1)= 0.0d+00
          g2(1)= 2.42d+00*(x(1)+4.0d-01)**2-1.1d+00*x(1)-x(2)+2.35d-01
           
        !second constraint  
          g1(2)= 0.0d+00
          g2(2)= 1.1d+00*x(1)**2-1.3d+00*x(1)+x(2)+1.7d-01
           
        !third constraint  
          g1(3)= 0.0d+00 
          g2(3)= EXP(-5.0d+00*x(1)+4.0d+00)+x(2)-1.2d+00
                 
        !forth constraint  
          g1(4)= 0.0d+00
          g2(4)= (x(1)-5.0d-01)**2+(x(2)-5.0d-01)**2-9.00d-02                 
                     
        !fifth constraint  
          g1(5)= 0.0d+00
          g2(5)=2.2d+01*(x(1)-3.0d-01)**2-1.1d+00*x(1)-x(2)+1.155d+00           
           
        !sixth constraint  
          g1(6)= 0.0d+00 
          g2(6)= 2.2d+00*(x(1)-5.0d-01)**2-1.1d+00*x(1)-x(2)+1.475d+00
                    
        !seventh constraint  
          g1(7)= 0.0d+00
          g2(7)= 2.0d+01*(x(1)-1.0d-01)**2-1.3d+00*x(1)+x(2)-5.0d-01     
     
       CASE(9)                                                          !(RCP-9)
        ! first constraint
          g1(1)= 0.0d+00 
          g2(1)= -1.0d+00 
          do i=1,m
           g2(1)= g2(1)+ dble(i)*x(i)**2
          end do      
     
       CASE(10)                                                          !(RCP-10)
        ! first constraint
          g1(1)=0.0d+00
          g2(1)=x(1)**2+x(2)**2+x(3)**2-4.0d+00     
              
        ! second constraint
          g1(2)=x(1)**2+x(2)**2+x(3)**2-9.0d+00       
          g2(2)=0.0d+00
       
        ! third constraint
          g1(3)=2.0d+00*x(1)-x(2)+2.0d+00*x(3)-3.0d+00       
          g2(3)=0.0d+00      
     
         CASE(11)                                                       !(RCP-11)
        ! first constraint
          g1(1)= 0.0d+00
          g2(1)= x(1)**2+x(2)**2-1.0d+00
           
        !second constraint  
          g1(2)= x(1)**2+x(2)**2-9.0d+00  
          g2(2)= 0.0d+00
           
        !third constraint  
          g1(3)= x(1)-1.5d+00
          g2(3)= 0.0d+00      
     
         CASE(12)                                                       !(RCP-12)
        ! first constraint
          g1(1)= (x(1)+1.5d+00)**2+x(2)**2-4.0d+00
          g2(1)= 0.0d+00
           
        !second constraint  
          g1(2)= 0.0d+00 
          g2(2)= (x(1)-1.0d+00)**2+(x(2)-1.0d+00)**2-1.0d+00          
         
         CASE(13)                                                       !(RCP-13)
         ! first constraint                                                                                                                            
           g1(1) = 0.0d+00                                                                        
           DO i = 1,m                                                   
             g1(1) = g1(1)+x(i)**2
           END DO 
           g1(1) = g1(1)-6.0d+00
           g2(1) = 0.0d+00    
                    
         ! second constraint
           g1(2) = 0.0d+00       
           g2(2) = 2.0d+00*x(1)**2-2.0d+00*x(2)-4.0d+00*x(3)+8.0d+00  
           
         CASE(14)                                                         !(RCP-14)
         ! first constraint
           g1(1)=0.0d+00                                                    
           DO i = 1,m  
            g1(1)=g1(1)+x(i)**2
           END DO
           g1(1) = g1(1)-2.0d+00*dble(m)  
           g2(1)=0.0d+00    
                   
         ! second constraint
           g1(2)=0.0d+00       
           g2(2)=-1.0d+00
           DO i=1,m-1    
            g2(2)=g2(2)+dble(i)*x(i)**2
           END DO
           
         ! third constraint
           g1(3)=0.0d+00       
           g2(3)=-2.0d+00
           DO i=2,m  
            g2(3)=g2(3)+dble(i)*x(i)**2
           END DO                       
                                      
       END SELECT
       return
       end

c --------------------------------------------------------------------------
!  gradZ(Y,X) Z =1,2 for DC componenets 1 & 2; Y is for constraints (=nconst); X is dimensional of x (n)
c --------------------------------------------------------------------------
      subroutine constgrad(x,grad1,grad2)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxcon=100)
      double precision x(maxvar),grad1(maxcon,maxvar)
     1  ,grad2(maxcon,maxvar)
      COMMON /cnselect/nselect, /csize/m     

!**********************************************************************  
       SELECT CASE(nselect)   
            
       CASE(1)                                                          !(RCP-1)   
           ! first constraint
            grad1(1,1)=0.0d+00 
            grad1(1,2)=0.0d+00  

            grad2(1,1)=2.0d+00*x(1) 
            grad2(1,2)=-1.0d+00    
           
       CASE(2)                                                          !(RCP-2)   
          ! first constraint  
            grad1(1,1)=1.0d+00 
            grad1(1,2)=-1.0d+00 
              
            grad2(1,1)= 0.0d+00
            grad2(1,2)= 0.0d+00 
                     
          ! second constraint  
             grad1(2,1)=-2.0d+00 
             grad1(2,2)=1.0d+00 
                  
             grad2(2,1)= 0.0d+00
             grad2(2,2)= 0.0d+00  
           
          ! third constraint  
             grad1(3,1)= 0.0d+00
             grad1(3,2)= 0.0d+00 
                  
             grad2(3,1)= 2.0d+00*x(1)-8.0d+00 
             grad2(3,2)= 8.0d+00*x(2)-1.6d+01                                                           
           
          ! fourth constraint  
             grad1(4,1)=0.0d+00 
             grad1(4,2)=0.0d+00
                  
             grad2(4,1)= 2.0d+00*x(1)-1.6d+01
             grad2(4,2)= 4.5d+00*x(2)-2.25d+01 
             
        CASE(3)                                                         !(RCP-3)   
           ! first constraint
           grad1(1,1)=3.0d+00 
           grad1(1,2)=-1.0d+00  
              
           grad2(1,1)=0.0d+00 
           grad2(1,2)=0.0d+00    
 
         ! second constraint  
           grad1(2,1)= 3.0d+00 
           grad1(2,2)= 1.0d+00
                  
           grad2(2,1)= 0.0d+00
           grad2(2,2)= 0.0d+00
           
         ! third constraint
           grad1(3,1)=0.0d+00 
           grad1(3,2)=0.0d+00  
              
           grad2(3,1)= 2.0d+00*(x(1)-2.0d+00)
           grad2(3,2)= 2.0d+00*(x(2)-1.0d+00)   
 
         ! forth constraint  
           grad1(4,1)= 0.0d+00 
           grad1(4,2)= 0.0d+00
                  
           grad2(4,1)= 2.0d+00*(x(1)-2.0d+00)
           grad2(4,2)= 2.0d+00*(x(2)-2.0d+00)             
             
       CASE(4)                                                          !(RCP-4) 
           do i=1,m
            grad1(1,i) = 0.0d+00          
           end do
           grad2(1,1)= 2.0d+00*x(1)
           grad2(1,2)= 2.0d+00*x(2)             
             
       CASE(5)                                                          !(RCP-5) 
        ! first constraint
           grad1(1,1) = 0.0d+00          
           grad1(1,2) = 0.0d+00
           
           grad2(1,1) = 2.0d+00*(x(1)+1)
           grad2(1,2) = 2.0d+00*x(2)
           
        ! second constraint 
           grad1(2,1) = 0.0d+00          
           grad1(2,2) = 0.0d+00    
           
           grad2(2,1) = 2.0d+00*x(1)
           grad2(2,2) = 2.0d+00*(x(2)+1)
           
       !third constraint  
           grad1(3,1) = 2.0d+00*x(1)          
           grad1(3,2) = 2.0d+00*x(2)   
           
           grad2(3,1) = 0.0d+00
           grad2(3,2) = 0.0d+00              
             
       CASE(6)                                                          !(RCP-6)      
          do i=1,m
           grad1(1,i) = 0.0d+00          
           grad2(1,i) = 2.0d+00*x(i)
          end do             
             
       CASE(7)                                                          !(RCP-7) 
      ! first constraint                        
          do i=1,m
           grad1(1,i) = 0.0d+00
           grad2(1,i) = 2.0d+00*x(i)                           
          end do               
             
         CASE(8)                                                        !(RCP-8)   
           ! first constraint
           grad1(1,1)=0.0d+00 
           grad1(1,2)=0.0d+00
              
           grad2(1,1)= 4.84d+00*(x(1)+4.0d-01)-1.1d+00
           grad2(1,2)= -1.0d+00
            
         ! second constraint  
           grad1(2,1)= 0.0d+00 
           grad1(2,2)= 0.0d+00
                  
           grad2(2,1)= 2.2d+00*x(1)-1.3d+00
           grad2(2,2)= 1.0d+00
           
         ! third constraint  
           grad1(3,1)=0.0d+00 
           grad1(3,2)=0.0d+00
                  
           grad2(3,1)= -5.0d+00*EXP(-5.0d+00*x(1)+4.0d+00)
           grad2(3,2)= 1.0d+00
                     
         ! forth constraint  
           grad1(4,1)= 0.0d+00
           grad1(4,2)= 0.0d+00
                  
           grad2(4,1)= 2.0d+00*(x(1)-5.0d-01)
           grad2(4,2)= 2.0d+00*(x(2)-5.0d-01)
           
         ! fifth constraint  
           grad1(5,1)= 0.0d+00 
           grad1(5,2)= 0.0d+00
                  
           grad2(5,1)= 4.4d+01*(x(1)-3.0d-01)-1.1d+00
           grad2(5,2)= -1.0d+00
           
         ! sixth constraint  
           grad1(6,1)=0.0d+00
           grad1(6,2)=0.0d+00
                  
           grad2(6,1)= 4.4d+00*(x(1)-5.0d-01)-1.1d+00
           grad2(6,2)= -1.0d+00
                     
         ! seventh constraint  
           grad1(7,1)= 0.0d+00
           grad1(7,2)= 0.0d+00
                  
           grad2(7,1)= 4.0d+01*(x(1)-1.0d-01)-1.3d+00
           grad2(7,2)= 1.0d+00              
             
       CASE(9)                                                         !(RCP-9)   
         ! first constraint
           do i=1,m 
            grad1(1,i)=0.0d+00 
            grad2(1,i)= 2.0d+00*dble(i)*x(i)
           end do             
             
       CASE(10)                                                          !(RCP-10) 
        ! first constraint
          grad1(1,1) = 0.0d+00
          grad1(1,2) = 0.0d+00
          grad1(1,3) = 0.0d+00
         
          grad2(1,1) = 2.0d+00*x(1) 
          grad2(1,2) = 2.0d+00*x(2)
          grad2(1,3) = 2.0d+00*x(3)   
       
        ! second constraint  
          grad1(2,1) = 2.0d+00*x(1)
          grad1(2,2) = 2.0d+00*x(2)
          grad1(2,3) = 2.0d+00*x(3) 
          
          grad2(2,1) = 0.0d+00
          grad2(2,2) = 0.0d+00
          grad2(2,3) = 0.0d+00
       
        ! third constraint  
          grad1(3,1) = 2.0d+00
          grad1(3,2) = -1.0d+00
          grad1(3,3) = 2.0d+00 
         
          grad2(3,1) = 0.0d+00
          grad2(3,2) = 0.0d+00
          grad2(3,3) = 0.0d+00              
             
      CASE(11)                                                          !(RCP-10)   
         ! first constraint
           grad1(1,1)=0.0d+00  
           grad1(1,2)=0.0d+00  
              
           grad2(1,1)=2.0d+00*x(1) 
           grad2(1,2)=2.0d+00*x(2)    
           
         ! second constraint  
           grad1(2,1)= 2.0d+00*x(1)
           grad1(2,2)= 2.0d+00*x(2)
                  
           grad2(2,1)= 0.0d+00
           grad2(2,2)= 0.0d+00
           
         ! third constraint  
           grad1(3,1)=1.0d+00
           grad1(3,2)=0.0d+00 
                  
           grad2(3,1)=0.0d+00
           grad2(3,2)=0.0d+00             
             
       CASE(12)                                                         !(RCP-12)   
         ! first constraint
           grad1(1,1)=2.0d+00*(x(1)+1.5d+00)  
           grad1(1,2)=2.0d+00*x(2) 
              
           grad2(1,1)=0.0d+00 
           grad2(1,2)=0.0d+00  
           
         ! second constraint  
           grad1(2,1)= 0.0d+00
           grad1(2,2)= 0.0d+00
                  
           grad2(2,1)= 2.0d+00*(x(1)-1.0d+00)
           grad2(2,2)= 2.0d+00*(x(2)-1.0d+00)              
           
       CASE(13)                                                         !(RCP-13) 
           do i=1,5
            do j=1,3
             grad1(i,j)=0.0d+00
             grad2(i,j)=0.0d+00
            end do
           end do

        ! first constraint           
           DO i = 1,m                                  
            grad1(1,i) = dble(2)*x(i)
           END DO     

        ! second constraint  
           grad2(2,1) = 4.0d+00*x(1)
           grad2(2,2) = -2.0d+00 
           grad2(2,3) = -4.0d+00    
           
       CASE(14)                                                         !(RCP-14) 
         ! first constraint
           DO i=1,m  
            grad1(1,i)=2.0d+00*x(i)
            grad2(1,i)=0.0d+00
           END DO  
       
         ! second constraint  
           DO i=1,m-1
            grad1(2,i)=0.0d+00  
            grad2(2,i)=2.0d+00*dble(i)*x(i)
           END DO
           grad1(2,m)=0.0d+00  
           grad2(2,m)=0.0d+00
           
         ! third constraint  
           grad1(3,1)=0.0d+00  
           grad2(3,1)=0.0d+00
           DO i=2,m
            grad1(3,i)=0.0d+00  
            grad2(3,i)=2.0d+00*dble(i)*x(i) 
           END DO              
                
       END SELECT
       return
       end
c --------------------------------------------------------------------------
! DC componenets of constraint function g+(x) = max{0,g_i(x)}  (g_i(x) <0 )
c -------------------------------------------------------------------------- 
      subroutine const12(x,h1,h2) ! g1+ g2+
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxcon=100)
      double precision x(maxvar),g1(maxcon),g2(maxcon)
      COMMON /cnconst/nconst,/cimax/imax,/cncon/ncon      
       ncon=ncon+1
       call const(x,g1,g2)

       d1=0.0d+00
       do i=1,nconst
        d1=d1+g2(i)
       end do
       h2=d1
       
       imax=0
       do i=1,nconst
        d2=g1(i)
        do j=1,nconst
         if(j.ne.i) then
          d2=d2+g2(j)
         end if
        end do
        if(d1.lt.d2) then
         d1=d2
         imax=i
        end if
       end do
       h1=d1                           
      return
      end

c -------------------------------------------------------------------------- 
      subroutine gradientC1(x,grad)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxcon=100)
      allocatable grad1(:,:), grad2(:,:)
      double precision x(maxvar),grad(maxvar)
      COMMON /cimax/imax,/cnconst/nconst,/csize/m,/cncong1/ncong1
      allocate(grad1(maxcon,maxvar))
      allocate(grad2(maxcon,maxvar))

      ncong1=ncong1+1
      call constgrad(x,grad1,grad2)
      do i=1,m
       grad(i)=0.0d+00
      end do
      if(imax.eq.0) then
       do i=1,nconst
        do j=1,m
         grad(j)=grad(j)+grad2(i,j)
        end do
       end do
      end if                    

      if(imax.gt.0) then
       do j=1,m
        grad(j)=grad(j)+grad1(imax,j)
       end do
       do i=1,nconst
        if(i.ne.imax) then
         do j=1,m
          grad(j)=grad(j)+grad2(i,j)                                    ! grad - subgradient of h_1
         end do
        end if
       end do
      end if                    
      return
      end

c-------------------------------------------------------------------------- 
      subroutine gradientC2(x,grad)
      implicit double precision (a-h,o-z)
      PARAMETER(maxvar=2000, maxcon=100)
      allocatable grad1(:,:), grad2(:,:)
      double precision x(maxvar),grad(maxvar)
      COMMON /cnconst/nconst,/csize/m,/cncong2/ncong2
      allocate(grad1(maxcon,maxvar))
      allocate(grad2(maxcon,maxvar))      

      ncong2=ncong2+1
      call constgrad(x,grad1,grad2)
      do i=1,m
       grad(i)=0.0d+00
      end do
      do i=1,nconst
        do j=1,m
         grad(j)=grad(j)+grad2(i,j)                                     ! grad - subgradient of h_2
        end do
      end do
      return
      end

c====================================================================
c  Optimization of estimations
c====================================================================

      SUBROUTINE TMPBNGC(m1,xx,ff)
C
C*******************************************************************************
C
      implicit double precision (a-h,o-z)
      INTEGER           N1,JMAX2,M,MC,MG,MP1,NOUT,IPRINT,LIWORK,LWORK
      PARAMETER        (N1      = 400,
     &                  JMAX2   = N1+3,
     &                  M      = 1,
     &                  MC     = 0,
     &                  MG     = 0,
c     &                  MG     = 1,
     &                  MP1    = M,
c     &                  MP1    = M+1,
     &                  NOUT   = 6,
     &                  IPRINT = 3,
     &                  LIWORK = 2*(MP1*(JMAX2+1)+MC+N1),
     &                LWORK=N1*(N1+2*MP1*JMAX2+2*MP1+2*MC+2*MG+2*M+31)/2
     &                           +MP1*(6*JMAX2+10)+JMAX2+5*MC+MG+M+18)
      ALLOCATABLE WORK(:)
      INTEGER           NITER,NFASG,IERR,I,LMAX,
     &                  IX(N1),IC(MC+1),IWORK(LIWORK),IUSER(1)
      DOUBLE PRECISION  RL,EPS,FEAS,FF,
     &                  X(N1),F(M+MG),GAM(MP1),BL(N1),BU(N1),XX(N1),
     &                 CG(N1,MC+1),CL(MC+1),CU(MC+1),USER(1)
      EXTERNAL          FASG
      ALLOCATE(WORK(LWORK))
C
      N=m1
      JMAX=N+3
      NITER = 200
      NFASG = 200

c      IX(1) =  0
c      IX(2) =  0

      IC(1) =  0

      DO I=1,N
        X(I)  = XX(I)
        IX(I) = 0
      END DO
      RL=0.01D+00
      LMAX=20
      GAM(1)=0.5D+00

      EPS=1.0D-04
      FEAS=1.0D-08
C
      CALL MPBNGC(N,X,IX,BL,BU,M,MG,MC,IC,CL,CU,CG,F,FASG,RL,LMAX,
     &            GAM,EPS,FEAS,JMAX,NITER,NFASG,NOUT,IPRINT,IERR,
     &            IWORK,LIWORK,WORK,LWORK,IUSER,USER)
C
      PRINT*
      PRINT*,'IERR = ',IERR
      PRINT*
      DO 70 I=1,N
          PRINT*,'X(',I,')=',X(I)
70    CONTINUE
      PRINT*
      PRINT*,'F(X)  = ',F
      PRINT*,'NITER = ',NITER
      PRINT*,'NFASG = ',NFASG
      PRINT*
      ff=F(1)
      DO I=1,N
       XX(I)=X(I)
      END DO
      RETURN
      END

C
C******************************************************************************
C
      SUBROUTINE FASG(N,X,M,F,G,IERR,IUSER,USER)
C     FASG   - SUBROUTINE FASG(N,X,MM,F,G,IERR,IUSER,USER),                   *
C              supplied by the user. Calculates the function values F(X) and  *
C              the transposed generalized Jacobian (N times MM) matrix G(X).  *
C              If there exists problems in function or subgradient            *
C              calculations, set IERR = 8.                                    *
C                                                                             * 
C******************************************************************************
C
      INTEGER           N,M,IUSER(1),IERR, KA
      DOUBLE PRECISION  F(M),X(N),V(N),G(N,M),USER(1),F1

C     Add your functions here.
c===============================================================
      call fest(x,f1)
      f(1)=f1
c=====================================================================
c Add gradient here
c=====================================================================
      call subgradest(x,v)

      do i=1,n
       G(i,1)=v(i)
      end do
c===============================================================
      RETURN
      END

      SUBROUTINE MPBNGC(N,X,IX,BL,BU,M,MG,MC,IC,CL,CU,CG,F,FASG,RL,LMAX,
     &                  GAM,EPS,FEAS,JMAX,NITER,NFASG,NOUT,IPRINT,IERR,
     &                  IWORK,LIWORK,WORK,LWORK,IUSER,USER)
C******************************************************************************
C                                                                             *
C  The Multiobjective Proximity control Bundle method for nonsmooth,          *
C  Nonconvex and Generally Constrained optimization:                          *
C                                                                             *
C             Minimize       f (x),...,f (x),                                 *
C                    n        1         m                                     *
C              x in R                                                         *
C                                                                             *
C             subject to                                                      *
C                                   T                                         *
C                            c  <= c x <= c  ,                                *
C                             l     g      u                                  *
C                                                                             *
C                            b <= x <= b  and                                 *
C                             l         u                                     *
C                                                                             *
C                            f (x) <= 0,   j = m + 1,...,m + mg.              *
C                             j                                               *
C                                                                             *
C  utilizing new version of PLQDF1 by Ladislav Luksan as a quadratic solver.  *
C                                                                             *
C-----------------------------------------------------------------------------*
C  Version  3.1  of  MPBNGC  dated  September 9, 2004.                        *
C  Last modified by Ladislav Luksan 2009.                                     *
C-----------------------------------------------------------------------------*
C  Copyright by:          ********************************                    *
C---------------          *       Marko M. Mäkelä        *                    *
C                         ********************************                    *
C                         *     University of Turku      *                    *
C                         *  Department of Mathematics   *                    *
C                         *        FI-20014 Turku        *                    *
C                         *           Finland            *                    *
C                         ********************************                    *
C-----------------------------------------------------------------------------*
C  Parameters:                                                                *
C-------------                                                                *
C     N      - The number of variables                         (1 <= N).      *
C     X      - The vector of dimension N.                                     *
C                Input:  The feasible initial approximation to the solution.  *
C                Output: The best approximation to the solution.              *
C     IX     - Types if box constraints for individual variables.             *
C                0 : Free variable.                                           *
C                1 : Lower bound    - BL(I) <= X(I).                          *
C                2 : Upper bound    - X(I)  <= BU(I).                         *
C                3 : Two-side bound - BL(I) <= X(I) <= BU(I).                 *
C                5 : Fixed variable.                                          *
C     BL     - Lower bounds of X.                                             *
C     BU     - Upper bounds of X.                                             *
C     M      - The number of objective functions.              (1 <= M).      *
C     MG     - The number of general constraint functions.     (0 <= MG).     *
C     MC     - The number of linear constraints.               (0 <= MC).     *
C     IC     - Types of individual linear constraints.                        *
C                0 : Constraint is not used.                                  *
C                1 : Lower bound    - CL(I) <= CG(.,I)*X.                     *
C                2 : Upper bound    - CG(.,I)*X <= CU(I).                     *
C                3 : Two-side bound - CL(I) <= CG(.,I)*X <= CU(I).            *
C                5 : Equality       - CG(.,I)*X = CL(I).                      *
C     CL     - Lower bounds of CG(.,I)*X.                                     *
C     CU     - Upper bounds of CG(.,I)*X.                                     *
C     CG     - Coefficients of linear constraints: i-th column of CG          *
C              corresponds to i-th linear constraint.                         *
C     F      - The MM (= M + MG) vector of function values at the solution.   *
C     FASG   - SUBROUTINE FASG(N,X,MM,F,G,IERR,IUSER,USER),                   *
C              supplied by the user. Calculates the function values F(X) and  *
C              the transposed generalized Jacobian (N times MM) matrix G(X).  *
C              If there exists problems in function or subgradient            *
C              calculations, set IERR = 8.                                    *
C     RL     - Line search parameter                           (0 < RL < 0.5).*
C     LMAX   - The maximum number of FASG calls in line search (0 < LMAX).    *
C     GAM    - The MP1 vector of distance measure parameters   (0 <=GAM(I)).  *
C              (GAM(I) = 0  if f is convex).                                  *
C              Here                                                           *
C                       MP1 =  M + 1 if MG > 0,                               *
C                       MP1 =  M     if MG = 0.                               *
C     EPS    - The final objective function accuracy parameter (0 < EPS).     *
C     FEAS   - The tolerance for constraint feasibility        (0 < FEAS).    *
C     JMAX   - The maximum number of stored subgradients       (2 <=JMAX).    *
C     NITER  - Input : The maximum number of iterations        (0 < NITER).   *
C              Output: Number of used iterations.                             *
C     NFASG  - Input : The maximum number of FASG calls        (1 < NFASG).   *
C     NOUT   - The output file number.                                        *
C              Output: Number of used function and subgradient calls.         *
C     IPRINT - Printout control parameter.                                    *
C               -1 : No printout.                                             *
C                0 : Only the error messages.                                 *
C                1 : The final values of the objective functions.             *
C                2 : The whole final solution.                                *
C                3 : At each iteration values of the objective functions.     *
C                4 : At each iteration the whole solution.                    *
C     IERR   - Failure parameter.                                             *
C                0 : Everything is OK.                                        *
C                1 : Number of calls of FASG = NFASG.                         *
C                2 : Number of iterations = NITER.                            *
C                3 : Invalid input parameters.                                *
C                4 : Not enough working space.                                *
C                5 : Failure in quadratic program.                            *
C                6 : The starting point is not feasible.                      *
C                7 : Failure in attaining the demanded accuracy.              *
C                8 : Failure in function or subgradient calculations          *
C                    (assigned by the user).                                  *
C     IWORK  - Working array.                                                 *
C     LIWORK - Dimension of IWORK                                             *
C           LIWORK >= 2*(MP1*(JMAX+1)+MC+N).                                  *
C     WORK   - Working array of dimension LWORK.                              *
C     LWORK  - Dimension of WORK                                              *
C           LWORK >= N*(N+2*MP1*JMAX+2*MP1+2*MC+2*MG+2*M+31)/2                *
C                    +MP1*(6*JMAX+10)+JMAX+5*MC+MG+M+18.                      *
C     IUSER  - Working array of the user.                                     *
C     USER   - Working array of the user.                                     *
C                                                                             *
C-----------------------------------------------------------------------------*
C  Machine-Depended Constants:                                                *
C-----------------------------                                                *
C     DRELPR - Relative precision.                                            *
C              The smallest positive number such that  1.0 + DRELPR > 1.0     *
C                                                                             *
      DOUBLE PRECISION  DRELPR
      PARAMETER        (DRELPR = 2.775576D-17)
C                                                                             *
C******************************************************************************
C
      INTEGER           N,M,MG,MC,IX(N),IC(MC),LMAX,JMAX,NITER,NFASG,
     &                  NOUT,IPRINT,IERR,LIWORK,LWORK,IWORK(LIWORK),
     &                  IUSER(*)
      DOUBLE PRECISION
     &                  X(N),BL(N),BU(N),CL(MC),CU(MC),CG(N,MC),
     &                  F(M+MG),RL,GAM(M+1),EPS,FEAS,WORK(LWORK),
     &                  USER(*)
      EXTERNAL          FASG
C
C********************* Local Variables ********************
C
      INTEGER           JKP,MM,MP1,MJKP,MTOT,LSG,LFK,LFK1,LSK,
     &                  LSKJKP,LALFA,LAL1,LXUD,LFXUD,LAMDA,LSUML,
     &                  LH,LCF,LCFD,LAF,LAFD,LS,LG,LAZ,LAR,
     &                  LIA,LIAA
      DOUBLE PRECISION  ZERO,ZER0
      PARAMETER        (ZERO = 1.0D+05*DRELPR,
     &                  ZER0 = 1.0D+02*DRELPR)
C
C************************** Start *************************
C
      JKP    = JMAX+1
      MM     = M+MG
      MP1    = M
      IF (MG.GT.0) MP1=MP1+1
      MJKP   = MP1*JKP
      MTOT   = MJKP
      LSG    = 1
      LFK    = LSG+N*(MTOT+MM)
      LFK1   = LFK+MJKP
      LSK    = LFK1+MP1
      LSKJKP = LSK+JMAX
      LALFA  = LSKJKP+MP1
      LAL1   = LALFA+MTOT
      LXUD   = LAL1+MP1
      LFXUD  = LXUD+N
      LAMDA  = LFXUD+MM
      LSUML  = LAMDA+MTOT
      LH     = LSUML+MP1
      LCF    = LH+N
      LCFD   = LCF+MC
      LAF    = LCFD+MC
      LAFD   = LAF+MTOT
      LS     = LAFD+MTOT
      LG     = LS+N+1
      LAZ    = LG+N+1
      LAR    = LAZ+N+1
      LIA    = 1
      LIAA   = LIA+MTOT
      IERR   = -1
C
C******************* Parameter Checking *******************
C
      CALL CHECK(N,M,MP1,MG,MC,RL,LMAX,GAM,EPS,FEAS,JMAX,NITER,
     &           NFASG,IPRINT,LIWORK,LWORK,IERR)
C
C************************ Iteration ***********************
C

      IF (IERR.LT.0) THEN
         CALL ITERA(N,X,IX,BL,BU,M,MG,F,FASG,MC,IC,CL,CU,CG,RL,LMAX,
     &              GAM,EPS,FEAS,JMAX,JKP,MJKP,MM,MP1,NITER,NFASG,NOUT,
     &              IPRINT,IERR,ZERO,ZER0,WORK(LSG),WORK(LFK),
     &              WORK(LFK1),WORK(LSK),WORK(LSKJKP),WORK(LALFA),
     &              WORK(LAL1),WORK(LXUD),WORK(LFXUD),WORK(LAMDA),
     &              WORK(LSUML),WORK(LH),WORK(LCF),WORK(LCFD),
     &              WORK(LAF),WORK(LAFD),WORK(LS),WORK(LG),WORK(LAZ),
     &              WORK(LAR),IWORK(LIA),IWORK(LIAA),IUSER,USER)
      END IF
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE ITERA(N,XK,IX,XL,XU,M,MG,FXK,FASG,NC,IC,CL,CU,CG,RL,
     &                 LMAX,GAM,EPS,FEAS,JMAX,JKP,MJKP,MM,MP1,NITER,
     &                 NFASG,NOUT,IPRINT,IERR,ZERO,ZER0,SG,FK,FK1,SK,
     &                 SKJKP,ALFA,AL1,XUD,FXUD,DLAMDA,SUML,H,CF,CFD,AF,
     &                 AFD,S,G,AZ,AR,IA,IAA,IUSER,USER)
C
C******************************************************************************
C
      INTEGER           N,M,MG,NC,IX(N),IC(NC),LMAX,JMAX,JKP,MJKP,MM,
     &                  MP1,NITER,NFASG,NOUT,IPRINT,IERR,IA(MJKP),
     &                  IAA(N+1),IUSER(*)
      DOUBLE PRECISION  XK(N),XL(N),XU(N),FXK(MM),CL(NC),CU(NC),
     &                  CG(N,NC),RL,GAM(MP1),EPS,FEAS,ZERO,ZER0,
     &                  SG(N,MJKP+MM),FK(MP1,JKP),FK1(MP1),
     &                  SK(JMAX),SKJKP(MP1),ALFA(MJKP),AL1(MP1),
     &                  XUD(N),FXUD(MM),DLAMDA(MJKP),SUML(MP1),
     &                  H(N),CF(NC),CFD(NC),AF(MJKP),AFD(MJKP),S(N+1),
     &                  G(N+1),AZ(N+1),AR((N+1)*(N+2)),USER(*)
      EXTERNAL          FASG
C
C********************* Local Variables ********************
C
      INTEGER           I,J,K,L,JK,JK1,JK2,NFUN,NM,NZ,IUK,IND1,MSAMEE,
     &                  MAXSME,KBF,KBC,ITERL,KC
      DOUBLE PRECISION  U,UK,PKPK,DKDK,DKNORM,TL,VK,WK,EXWK,SK1,UMIN,
     &                  EPSV,ALPKT,ETA9,EPS7,EPS9,GMAX,UMAX,PRO,MXVDOT
     &
      INTRINSIC         DSQRT,DMIN1,DMAX1,DABS
      PARAMETER        (MAXSME = 10,
     &                  ETA9   = 1.0D+60,
     &                  EPS7   = 1.0D-12,
     &                  EPS9   = 1.0D-10)
*
*     INITIAL OPERATIONS WITH SIMPLE BOUNDS
*
      KBF=0
      DO 10 I = 1,N
          IF (IX(I).GT.0) THEN
              KBF=2
              IF ((IX(I).EQ.3.OR.IX(I).EQ.4).AND.XU(I).LE.XL(I)) THEN
                  XU(I) = XL(I)
                  XK(I) = XL(I)
                  IX(I) = 5
              ELSE IF (IX(I).EQ.5 .OR. IX(I).EQ.6) THEN
                  XL(I) = XK(I)
                  XU(I) = XK(I)
                  IX(I) = 5
              END IF
              IF (IX(I).EQ.1.OR.IX(I).EQ.3) XK(I) = DMAX1(XK(I),XL(I))
              IF (IX(I).EQ.2.OR.IX(I).EQ.3) XK(I) = DMIN1(XK(I),XU(I))
          ENDIF
   10 CONTINUE
*
*     INITIAL OPERATIONS WITH GENERAL LINEAR CONSTRAINTS
*
      KBC=0
      IF (NC.GT.0) THEN
          KBC=2
          DO 20 KC = 1,NC
              IF ((IC(KC).EQ.3.OR.IC(KC).EQ.4) .AND.
     +            CU(KC).LE.CL(KC)) THEN
                  CU(KC) = CL(KC)
                  IC(KC) = 5
              ELSE IF (IC(KC).EQ.5 .OR. IC(KC).EQ.6) THEN
                  CU(KC) = CL(KC)
                  IC(KC) = 5
              END IF
              CF(KC) = MXVDOT(N,XK,CG(1,KC))
   20     CONTINUE
*
*     DETERMINATION OF AN INITIAL FEASIBLE POINT
*
          CALL MXVSET(N,0.0D0,G)
          CALL PLLPB2(N,NC,XK,IX,H,XL,XU,CF,CFD,IC,IAA,CL,CU,CG,AR,AZ,
     +                G,G,S,1,KBF,KBC,ETA9,EPS7,EPS9,UMAX,GMAX,NZ,
     +                ITERL)
      ELSE IF (KBF.GT.0) THEN
          DO 30 I = 1,N
              IF (IX(I).GE.5) IX(I) = -IX(I)
              IF (IX(I).LE.0) THEN
              ELSE IF ((IX(I).EQ.1.OR.IX(I).EQ.3) .AND.
     +                 XK(I).LE.XL(I)) THEN
                  XK(I) = XL(I)
              ELSE IF ((IX(I).EQ.2.OR.IX(I).EQ.3) .AND.
     +                 XK(I).GE.XU(I)) THEN
                  XK(I) = XU(I)
              END IF
              CALL PLNEWS(XK,IX,XL,XU,EPS9,I,ITERL)
              IF (IX(I).GT.10) IX(I) = 10 - IX(I)
   30     CONTINUE
      END IF
C
C********************* Initialization *********************
C
      K=1
      JK=1
      JK1=2
      JK2=3
      IND1=1
      MSAMEE=0
      NFUN=1
      CALL FASG(N,XK,MM,FXK,SG(1,IND1),IERR,IUSER,USER)
      IF (IERR.EQ.8) GOTO 300
      CALL CMAX(N,M,MG,FXK,SG(1,IND1))
      UK=0.0D+00
      DO 40 L=1,MP1
         PKPK=PRO(N,SG(1,L),SG(1,L))
         UK=UK+DSQRT(PKPK)
         FK(L,1)=FXK(L)
         FK(L,JKP)=FXK(L)
         SKJKP(L)=0.0D+00
         ALFA(L)=0.0D+00
         ALFA(MP1+L)=0.0D+00
         DO 35 I=1,N
            SG(I,MP1+L)=SG(I,L)
 35      CONTINUE
 40   CONTINUE
      SK(1)=0.0D+00
      IF (MG.GT.0) THEN
         ALFA(MP1)=-FXK(MP1)
         ALFA(2*MP1)=-FXK(MP1)
         IF (FXK(MP1).GT.FEAS) IERR=6
         FXK(MP1)=0.0D+00
         UK=UK-DSQRT(PKPK)
      END IF
      UK=DMAX1(UK/M,ZERO)
      U=UK
      NM=MP1
      EPSV=1.0D+15
      UMIN=1.0D-10*UK
      IUK=0
C
C************************ Iteration ***********************
C
60    CONTINUE
C
C***************** Quadratic Program Call *****************
C
      CALL QUADNW(N,NM,NC,XK,IX,XL,XU,CF,IC,CL,CU,CG,AF,AFD,SG,AZ,AR,
     &            IA,IAA,S,G,H,UK,ALFA,DLAMDA,VK,KBF,KBC,IERR)
C
C**********************************************************
C        Varmistetaan, ettei menna ulos laatikkorajoitteista.
C        3.7.1996
C        Merkkivirhe jälkimmäisestä korjattu.
C        27.11.2002
C**********************************************************
C
      WK=0.0D+00
      IF (IERR.LT.0) THEN
         ALPKT=0.0D+00
         DO 140 L=1,MP1
            IF (K.EQ.1) THEN
               DLAMDA(L+MP1)=1.0D+00
               DLAMDA(L)=0.0D+00
               SUML(L)=1.0D+00
            ELSE
               SUML(L)=0.0D+00
               DO 90 J=1,JK1
                  SUML(L)=SUML(L)+DLAMDA(L+(J-1)*MP1)
90             CONTINUE
               DO 100 J=1,JK1
                  IF (SUML(L).LE.ZERO) THEN
                     DLAMDA(L+(J-1)*MP1)=1.0D+00/JK1
                     SUML(L)=0.0D+00
                  ELSE
                     DLAMDA(L+(J-1)*MP1)=DLAMDA(L+(J-1)*MP1)/SUML(L)
                  END IF
100            CONTINUE
            END IF
            FK(L,JKP)=DLAMDA(L)*FK(L,JKP)
            SKJKP(L)=DLAMDA(L)*SKJKP(L)
            DO 110 I=1,N
               SG(I,L)=DLAMDA(L)*SG(I,L)
110         CONTINUE
            DO 130 J=1,JK
               FK(L,JKP)=FK(L,JKP)+DLAMDA(L+J*MP1)*FK(L,J)
               SKJKP(L)=SKJKP(L)+DLAMDA(L+J*MP1)*SK(J)
               DO 120 I=1,N
                  SG(I,L)=SG(I,L)+DLAMDA(L+J*MP1)*SG(I,L+J*MP1)
120            CONTINUE
130         CONTINUE
            ALFA(L)=DMAX1(DABS(FXK(L)-FK(L,JKP)),
     &                    GAM(L)*SKJKP(L)*SKJKP(L))
            ALPKT=ALPKT+SUML(L)*ALFA(L)
140      CONTINUE
         PKPK=PRO(N,S,S)
         DKDK=PKPK
         DKNORM=DSQRT(DKDK)
C
C******************* Stopping Criterion *******************
C
         EXWK=WK
         WK=0.5D+00*UK*DKDK+ALPKT
         IF (DABS(WK-EXWK).LT.ZERO) THEN
            MSAMEE=MSAMEE+1
         ELSE
            MSAMEE=0
         END IF
         IF ((WK.LE.EPS).OR.(MSAMEE.GE.MAXSME)) THEN
            IF (WK.LE.EPS) THEN
               IERR=0
            ELSE
               IERR=7
            END IF
            K=K-1
            IF (IPRINT.GT.0)
     &         CALL PRINT(N,XK,M,FXK,NFUN,WK,K,IPRINT,NOUT)
         ELSE
            IF (IPRINT.GT.2)
     &         CALL PRINT(N,XK,M,FXK,NFUN,WK,K-1,IPRINT,NOUT)
C
C************* Line Search and Weight Updating ************
C
         CALL LSAWUD(N,M,U,IUK,UMIN,EPSV,XK,FXK,FASG,RL,LMAX,NC,MG,
     &               GAM,NFASG,NFUN,IERR,SG,S,JK1,MJKP,MM,MP1,FK1,
     &               SK1,AL1,ALPKT,DKNORM,TL,VK,XUD,FXUD,ZER0,FEAS,
     &               IUSER,USER)
C
C**********************************************************
C
            IF (K.EQ.NITER) THEN
               IERR=2
               IF ((IPRINT.GT.0).AND.(IERR.LT.8))
     &            CALL PRINT(N,XK,M,FXK,NFUN,WK,K,IPRINT,NOUT)
            END IF
         END IF
         IF (IERR.LT.0) THEN
C
C****************** Subgradient Deletion ******************
C
            IF (JK.EQ.JMAX) THEN
               CALL DEL(N,M,MP1,MG,NC,JK,JKP,FK,SK,SG,ALFA,AFD)
               JK1=JK+1
               JK2=JK+2
            END IF
C
C***************** Linearization Updating *****************
C
            IF (TL.GT.ZER0) THEN
               DO 160 L=1,MP1
                  FK(L,JKP)=FK(L,JKP)+TL*PRO(N,SG(1,L),S)
                  SKJKP(L)=SKJKP(L)+TL*DKNORM
                  ALFA(L)=DMAX1(DABS(FXK(L)-FK(L,JKP)),
     &                          GAM(L)*SKJKP(L)*SKJKP(L))
160            CONTINUE
            END IF
            NM=MP1*JK2
            DO 170 L=1,MP1
               ALFA(L+MP1*JK1)=AL1(L)
               FK(L,JK1)=FK1(L)
170         CONTINUE
            SK(JK1)=SK1
C
C********************** Serious Step **********************
C
            IF (TL.GT.ZER0) THEN
               DO 190 J=1,JK
                  SK(J)=SK(J)+TL*DKNORM
                  DO 180 L=1,MP1
                     IF (K.EQ.1) THEN
                        FK(L,J)=FK(L,J)+TL*AFD(L+(J-1)*MP1)
                     ELSE
                        FK(L,J)=FK(L,J)+TL*AFD(L+J*MP1)
                     END IF
                     ALFA(L+J*MP1)=DMAX1(DABS(FXK(L)-FK(L,J)),
     &                                   GAM(L)*SK(J)*SK(J))
180               CONTINUE
190            CONTINUE
               DO 200 J=1,NC
                  CF(J)=PRO(N,CG(1,J),XK)
200            CONTINUE
            END IF
C
C************************ Updating ************************
C
            K=K+1
            JK=JK+1
            JK1=JK+1
            JK2=JK+2
            UK=U
            GOTO 60
         END IF
      END IF
300   CONTINUE
      NITER=K
      NFASG=NFUN
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE CHECK(N,M,MP1,MG,MC,RL,LMAX,GAM,EPS,FEAS,JMAX,NITER,
     &                 NFASG,IPRINT,LIWORK,LWORK,IERR)
C******************************************************************************
C                                                                             *
C  Procedure, which checks the input parameters.                              *
C                                                                             *
C******************************************************************************
C
      INTEGER           N,M,MP1,MG,MC,LMAX,JMAX,NITER,NFASG,IPRINT,
     &                  LIWORK,LWORK,IERR
      DOUBLE PRECISION  RL,EPS,FEAS,
     &                  GAM(MP1)
C
C********************* Local Variables ********************
C
      INTEGER           LIW,LW,L
      DOUBLE PRECISION  ZERO,HALF
      PARAMETER (ZERO = 0.0D+00,
     &           HALF = 0.5D+00)
C
C************************** Start *************************
C
      LIW = 2*(MP1*(JMAX+1)+N)
      LW  = N*(N+2*MP1*JMAX+2*MP1+2*MG+2*M+31)/2
     &      +MP1*(6*JMAX+10)+JMAX+2*MC+MG+M+18

      IF ((N.LT.1).OR.(M.LT.0).OR.(MG.LT.0).OR.(MC.LT.0).OR.
     &   (RL.LE.ZERO).OR.(RL.GE.HALF).OR.(LMAX.LE.0).OR.
     &   (EPS.LE.ZERO).OR.(FEAS.LE.ZERO).OR.(JMAX.LT.2).OR.
     &   (NITER.LE.0).OR.(NFASG.LE.1).OR.(IPRINT.LT.-1).OR.
     &   (IPRINT.GT.4)) THEN
         IERR=3
      ELSE
         IF ((LIWORK.LT.LIW).OR.(LWORK.LT.LW)) IERR=4
      END IF
      DO 10 L=1,MP1
         IF (GAM(L).LT.ZERO) IERR=3
10    CONTINUE
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE DEL(N,M,MP1,MG,MC,JK,JKP,FK,SK,SG,ALFA,AFD)
C******************************************************************************
C                                                                             *
C  Procedure, which deletes the smallest indexis from the vectors FK, SK,     *
C  SG and ALFA.                                                               *
C                                                                             *
C******************************************************************************
C
      INTEGER           N,M,MP1,MG,MC,JK,JKP
      DOUBLE PRECISION  FK(MP1,JKP),SK(JK),SG(N,MP1*JKP+M+MG),
     &                  ALFA(MP1*JKP),AFD(MP1*JKP)
C
C********************* Local Variables ********************
C
      INTEGER           I,J,L
C
C*********************** Iteration ************************
C
      DO 30 J=1,JK-1
         SK(J)=SK(J+1)
         DO 20 L=1,MP1
            FK(L,J)=FK(L,J+1)
            ALFA(L+J*MP1)=ALFA(L+(J+1)*MP1)
            AFD(L+J*MP1)=AFD(L+(J+1)*MP1)
            DO 10 I=1,N
               SG(I,L+J*MP1)=SG(I,L+(J+1)*MP1)
10          CONTINUE
20       CONTINUE
30    CONTINUE
      DO 50 L=1,MP1
         DO 40 I=1,N
            SG(I,L+MP1*JK)=SG(I,L+MP1*(JK+1))
40       CONTINUE
50    CONTINUE
      JK=JK-1
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE H(M,MM,HXK,LM,FXK,FXUD,NFEA,FEAS)
C******************************************************************************
C                                                                             *
C  Procedure, which calculates the current value of H(y;x).                   *
C                                                                             *
C******************************************************************************
C
      INTEGER           M,MM,LM,NFEA
      DOUBLE PRECISION  HXK,FXK(MM),FXUD(MM),FEAS
C
C********************* Local Variables ********************
C
      INTEGER           L
C
C*********************** Iteration ************************
C
      NFEA=0
      HXK=FXUD(1)-FXK(1)
      LM=1
      DO 10 L=1,M
         IF (HXK.LT.(FXUD(L)-FXK(L))) THEN
            HXK=FXUD(L)-FXK(L)
            LM=L
         END IF
10    CONTINUE
      IF ((MM-M).GT.0) THEN
         IF (FXUD(M+1).GT.FEAS) NFEA=1
      END IF
      RETURN
      END
C
C*****************************************************************************

C******************************************************************************
      SUBROUTINE CMAX(N,M,MG,FXK,SG)
C******************************************************************************
C                                                                             *
C  Procedure, which calculates the maximum of the constraint functions.       *
C                                                                             *
C******************************************************************************
C
      INTEGER           N,M,MG
      DOUBLE PRECISION  FXK(M+MG),SG(N,M+MG)
C
C********************* Local Variables ********************
C
      INTEGER           I,L,IMAX
      DOUBLE PRECISION  FMAX
C
C*********************** Iteration ************************
C
      IF (MG.GT.1) THEN
         FMAX=FXK(M+1)
         IMAX=1
         DO 10 L=2,MG
            IF (FMAX.LT.FXK(M+L)) THEN
               FMAX=FXK(M+L)
               IMAX=L
            END IF
10       CONTINUE
         IF (IMAX.GT.1) THEN
            FXK(M+1)=FMAX
            DO 20 I=1,N
               SG(I,M+1)=SG(I,M+IMAX)
20          CONTINUE
         END IF
      END IF
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE LSAWUD(N,M,UK,IUK,UMIN,EPSV,XK,FXK,FASG,RL,LMAX,MC,MG,
     &                  GAM,NFASG,NFUN,IERR,SG,S,JK1,MJKP,MM,MP1,FK1,
     &                  SK1,AL1,ALPKT,DKNORM,TL,VK,XUD,FXUD,ZER0,FEAS,
     &                  IUSER,USER)
C******************************************************************************
C                                                                             *
C  Line Search Algorithm and Weight Updating Algorithm, which minimizes       *
C  the function                                                               *
C                t  ----> f ( x + t d )                                       *
C                              k     k                                        *
C                                                                             *
C  by using the quadratic interpolation  g : R --> R  such that               *
C                                                                             *
C      g ( 0 ) = f ( x )  ,  g ( 1 ) = f ( x + d )  and  g' ( 0 ) = v         *
C                     k                     k   k                    k        *
C                                                                             *
C  and chooses the weight UK by using the safequarded quadratic interpolation.*
C                                                                             *
C******************************************************************************
C
      INTEGER           N,M,IUK,LMAX,MC,MG,NFASG,NFUN,IERR,JK1,
     &                  MJKP,MM,MP1,IUSER(*)
      DOUBLE PRECISION  UK,UMIN,EPSV,RL,SK1,ALPKT,DKNORM,TL,VK,
     &                  ZER0,FEAS,
     &                  XK(N),FXK(MM),GAM(MP1),SG(N,MJKP+MM),S(N),
     &                  FK1(MP1),AL1(MP1),XUD(N),FXUD(MM),
     &                  USER(*)
C
C********************* Local Variables ********************
C
      INTEGER           I,L,LM,LFUN,NFEA
      DOUBLE PRECISION  U,UINT,PRO,T,TU,HXK,HXTU,RR,TT,ZET
      INTRINSIC         DMAX1,DMIN1,DABS,MAX0,MIN0
      PARAMETER  (RR  = 0.5D+00,
     &            TT  = 0.1D+00)
      EXTERNAL FASG
C
C********************* Initialization *********************
C
      ZET=1.0D+00-1.0D+00/(2.0D+00*(1.0D+00-RL))
      U=UK
      TL=0.0D+00
      TU=1.0D+00
      T=1.0D+00
      DO 10 I=1,N
         XUD(I)=XK(I)+S(I)
10    CONTINUE
      NFUN=NFUN+1
      CALL FASG(N,XUD,MM,FXUD,SG(1,MP1*JK1+1),IERR,IUSER,USER)
      IF (IERR.EQ.8) GOTO 80
      CALL CMAX(N,M,MG,FXUD,SG(1,MP1*JK1+1))
      CALL H(M,MM,HXK,LM,FXK,FXUD,NFEA,FEAS)
      LFUN=1
      IF (NFUN.EQ.NFASG) IERR=1
      HXTU=HXK
      IF (DABS(VK).GE.ZER0) THEN
         UINT=2.0D+00*UK*(1.0D+00-HXK/VK)
      ELSE
         UINT=UK
      END IF
C
C*********************** Iteration ************************
C
20    CONTINUE
      IF ((HXK.LE.(RL*T*VK)).AND.(NFEA.EQ.0)) THEN
         TL=T
         DO 30 I=1,N
            XK(I)=XUD(I)
30       CONTINUE
         DO 40 L=1,M
            FXK(L)=FXUD(L)
40       CONTINUE
      ELSE
         TU=T
         HXTU=HXK
      END IF
      IF (IERR.LT.0) THEN
C
C******************** Long Serious Step *******************
C
         IF (TL.GT.TT) THEN
            DO 50 L=1,MP1
               FK1(L)=FXUD(L)
               AL1(L)=FXK(L)-FXUD(L)
50          CONTINUE
            SK1=0.0D+00
            IF (TL.GT.(1.0D+00-ZER0)) THEN
               IF ((HXK.LE.(RR*VK)).AND.(IUK.GT.0)) THEN
                  U=UINT
               ELSE
                  IF (IUK.GT.3) U=0.5D+00*UK
               END IF
               U=DMAX1(U,DMAX1(0.1D+00*UK,UMIN))
               EPSV=DMAX1(EPSV,-2.0D+00*VK)
               IUK=MAX0(IUK+1,1)
               IF (UK.GT.U) THEN
                  UK=U
                  IUK=1
               END IF
            END IF
         ELSE
C
C*************** Short Serious or Null Step ***************
C
            SK1=(T-TL)*DKNORM
            DO 60 L=1,MP1
               FK1(L)=FXUD(L)-(T-TL)*PRO(N,SG(1,MP1*JK1+L),S)
               AL1(L)=DMAX1(DABS(FXK(L)-FK1(L)),GAM(L)*SK1*SK1)
60          CONTINUE
            IF ((-AL1(LM)+PRO(N,SG(1,MP1*JK1+LM),S)*UK).LT.(RR*VK)
     &         .AND.(LFUN.LT.LMAX)) THEN
C
C************************ Updating ************************
C
               IF (TL.LT.ZER0) THEN
                  T=ZET*TU
                  IF ((TU*VK-HXTU).GT.ZER0)
     &               T=DMAX1(T,(0.5D+00*VK*TU*TU)/(TU*VK-HXTU))
                  T=DMIN1(T,(1.0D+00-ZET)*TU)
               ELSE
                  T=0.5D+00*(TL+TU)
               END IF
               DO 70 I=1,N
                  XUD(I)=XK(I)+T*S(I)
70             CONTINUE
               NFUN=NFUN+1
               LFUN=LFUN+1
               CALL FASG(N,XUD,MM,FXUD,SG(1,MP1*JK1+1),IERR,
     &                   IUSER,USER)
               IF (IERR.EQ.8) GOTO 80
               CALL CMAX(N,M,MG,FXUD,SG(1,MP1*JK1+1))
               CALL H(M,MM,HXK,LM,FXK,FXUD,NFEA,FEAS)
               IF (NFUN.EQ.NFASG) IERR=1
               GOTO 20
            END IF
            IF (TL.LT.ZER0) THEN
               EPSV=DMIN1(EPSV,UK*DKNORM+ALPKT)
               IF ((AL1(LM).GT.DMAX1(EPSV,-10.0D+00*VK)).AND.
     &            (IUK.LT.-3)) U=UINT
               U=DMIN1(U,10.0D+00*UK)
               IUK=MIN0(IUK-1,-1)
               IF (UK.LT.U) THEN
                  UK=U
                  IUK=-1
               END IF
            END IF
         END IF
      END IF
80    CONTINUE
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE PRINT(N,X,M,FX,NFUN,WK,IT,IPRINT,NOUT)
C******************************************************************************
C
C******************************************************************************
C
      INTEGER           N,M,NFUN,IT,IPRINT,NOUT
      DOUBLE PRECISION  WK,
     &                  X(N),FX(M)
C
C********************* Local Variables ********************
C
      INTEGER           I,ITEMP,L
      CHARACTER         FORM1*70 ,FORM2*33, FORM3*33
      INTRINSIC         MOD
C
C**********************************************************
C
      DATA FORM1 /'(1X,''Iter:'',I4,''  Nfun:'',I4,''  f'',IZ,''(x) ='',
     &G14.7,''     Eps ='',G14.7)'/
      DATA FORM2 /'(16X,''       f'',IZ,''(x) ='',G14.7)'/
      DATA FORM3 /'(16X,''        x('',IZ,'') ='',G14.7)'/
C
      IF (IPRINT.GT.0) THEN
         IF (IT.GE.10000) FORM1(14:14)='5'
         ITEMP=INT(LOG10(FLOAT(M)))+1
         WRITE (FORM1(36:36),'(I1)') ITEMP
         WRITE (FORM2(18:18),'(I1)') ITEMP
         WRITE (NOUT,FORM1) IT,NFUN,1,FX(1),WK
         DO 10 L=2,M
            WRITE (NOUT,FORM2) L,FX(L)
10       CONTINUE
         IF (MOD(IPRINT,2).EQ.0) THEN
            WRITE (NOUT,999)
            ITEMP=INT(LOG10(FLOAT(N)))+1
            WRITE (FORM3(20:20),'(I1)') ITEMP
            DO 20 I=1,N
               WRITE (NOUT,FORM3) I,X(I)
20          CONTINUE
            WRITE (NOUT,999)
         END IF
      END IF
      RETURN
999   FORMAT(' ')
      END
C
C******************************************************************************

C******************************************************************************
      DOUBLE PRECISION FUNCTION PRO(N,X,Y)
C******************************************************************************
C                                                                             *
C  Algorithm, which calculates the inner product of two N-dimensional         *
C  vectors X and Y, e. g.                                                     *
C                                                                             *
C                        N                                                    *
C                 PRO = SUM X(I)*Y(I)                                         *
C                       I=1                                                   *
C                                                                             *
C******************************************************************************
C
      INTEGER           N
      DOUBLE PRECISION  X(N),Y(N)
C
C********************* Local Variables ********************
C
      INTEGER           I
      DOUBLE PRECISION  SUM
C
C*********************** Iteration ************************
C
      SUM=0.0D+00
      DO 10 I=1,N
         SUM=SUM+X(I)*Y(I)
10    CONTINUE
      PRO=SUM
      RETURN
      END
C
C******************************************************************************

C******************************************************************************
      SUBROUTINE QUADNW(N,NA,NC,X,IX,XL,XU,CF,IC,CL,CU,CG,AF,AFD,AG,AZ,
     &                  AR,IA,IAA,S,G,H,U,ALFA,DLAMDA,V,KBF,KBC,IERR)
C******************************************************************************
C                                                                             *
C  Procedure, which calls PLQDF1 by Ladislav Luksan.                          *
C                                                                             *
C******************************************************************************
C
      INTEGER           N,NA,NC,IX(N),IC(NC),IA(NA),IAA(N+1),KBF,KBC,
     &                  IERR
      DOUBLE PRECISION  X(N),XL(N),XU(N),CF(NC),CL(NC),CU(NC),CG(NC*N),
     &                  AF(NA),AFD(NA),AG(NA*N),AZ(N+1),
     &                  AR((N+1)*(N+2)/2),S(N+1),G(N+1),H(N),U,ALFA(NA),
     &                  DLAMDA(NA),V
C
C********************* Local Variables ********************
C
      INTEGER           NZ,ITERQ,I,K
      DOUBLE PRECISION  ETA0,ETA2,ETA9,EPS7,EPS9,UMAX,GMAX,MXVDOT
      PARAMETER (ETA0  = 1.0D-15,
     &           ETA2  = 1.0D-12,
     &           ETA9  = 1.0D+60,
     &           EPS7  = 1.0D-14,
     &           EPS9  = 1.0D-12)
      DO 2 I=1,NA
      IA(I)=2
      AF(I)=-ALFA(I)
    2 CONTINUE
      DO 3 I=1,N
      H(I)=U
    3 CONTINUE
C
C***************** Quadratic Program Call *****************
C
      CALL PLQDF1(N,NA,NC,X,IX,XL,XU,AF,AFD,IA,IAA,AG,AR,AZ,CF,IC,CL,
     &           CU,CG,G,H,S,2,KBF,KBC,10,ETA0,ETA2,ETA9,EPS7,
     &           EPS9,V,UMAX,GMAX,NZ,ITERQ)
C
C**********************************************************
C
      IF (ITERQ.LE.0) THEN
          IERR=5
          RETURN
      ENDIF
      K=1
      DO 4 I=1,NA
      DLAMDA(I)=0.0D+00
      IF (IA(I).LT.0) AFD(I)=MXVDOT(N,AG(K),S)
      K=K+N
    4 CONTINUE
      DO 5 I=1,N-NZ
      K=IAA(I)
      IF(K.GT.NC) DLAMDA(K-NC)=-AZ(I)
    5 CONTINUE
      RETURN
      END
      
* SUBROUTINE PLQDF1             ALL SYSTEMS                   91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DUAL RANGE SPACE QUADRATIC PROGRAMMING METHOD FOR MINIMAX
* APPROXIMATION WITH LINEAR CONSTRAINTS.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  II  NA  NUMBER OF LINEAR APPROXIMATED FUNCTIONS.
*  II  NC  NUMBER OF LINEAR CONSTRAINTS.
*  RI  X(NF)  VECTOR OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  AF(NA)  VECTOR CONTAINING VALUES OF THE APPROXIMATED
*         FUNCTIONS.
*  RO  AFD(NA)  VECTOR CONTAINING INCREMENTS OF THE APPROXIMATED
*         FUNCTIONS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  IO  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RI  AG(NF*NA)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         APPROXIMATED FUNCTIONS.
*  RO  AR((NF+1)*(NF+2)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF THE
*         ORTHOGONAL PROJECTION.
*  RO  AZ(NF+1)  VECTOR OF LAGRANGE MULTIPLIERS.
*  RI  CF(NF)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCTIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CL(NC)  VECTOR CONTAINING LOWER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CU(NC)  VECTOR CONTAINING UPPER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RO  G(NF+1)  GRADIENT OF THE LAGRANGIAN FUNCTION.
*  RU  H(NF*(NF+1)/2)  TRIANGULAR DECOMPOSITION OR INVERSION OF THE
*         HESSIAN MATRIX APPROXIMATION.
*  RO  S(NF+1)  DIRECTION VECTOR.
*  II  MFP  TYPE OF FEASIBLE POINT. MFP=1-ARBITRARY FEASIBLE POINT.
*         MFP=2-OPTIMUM FEASIBLE POINT. MFP=3-REPEATED SOLUTION.
*  II  KBF  SPECIFICATION OF SIMPLE BOUNDS. KBF=0-NO SIMPLE BOUNDS.
*         KBF=1-ONE SIDED SIMPLE BOUNDS. KBF=2=TWO SIDED SIMPLE BOUNDS.
*  II  KBC  SPECIFICATION OF LINEAR CONSTRAINTS. KBC=0-NO LINEAR
*         CONSTRAINTS. KBC=1-ONE SIDED LINEAR CONSTRAINTS. KBC=2=TWO
*         SIDED LINEAR CONSTRAINTS.
*  IU  IDECF  DECOMPOSITION INDICATOR. IDECF=0-NO DECOMPOSITION.
*         IDECF=1-GILL-MURRAY DECOMPOSITION. IDECF=9-INVERSION.
*         IDECF=10-DIAGONAL MATRIX.
*  RI  ETA0  MACHINE PRECISION.
*  RI  ETA2  TOLERANCE FOR POSITIVE DEFINITENESS OF THE HESSIAN MATRIX.
*  RI  ETA9  MAXIMUM FOR REAL NUMBERS.
*  RI  EPS7  TOLERANCE FOR LINEAR INDEPENDENCE OF CONSTRAINTS.
*  RI  EPS9  TOLERANCE FOR ACTIVITY OF CONSTRAINTS.
*  RO  XNORM  VALUE OF LINEARIZED MINIMAX FUNCTION.
*  RO  UMAX  MAXIMUM ABSOLUTE VALUE OF A NEGATIVE LAGRANGE MULTIPLIER.
*  RO  GMAX  MAXIMUM ABSOLUTE VALUE OF A PARTIAL DERIVATIVE.
*  IO  N  DIMENSION OF MANIFOLD DEFINED BY ACTIVE CONSTRAINTS.
*  IO  ITERQ  TYPE OF FEASIBLE POINT. ITERQ=1-ARBITRARY FEASIBLE POINT.
*         ITERQ=2-OPTIMUM FEASIBLE POINT. ITERQ=-1 FEASIBLE POINT DOES
*         NOT EXISTS. ITERQ=-2 OPTIMUM FEASIBLE POINT DOES NOT EXISTS.
*
* SUBPROGRAMS USED :
*  S   PLMINA  DETERMINATION OF THE NEW ACTIVE FUNCTION.
*  S   PLMINL  DETERMINATION OF THE NEW ACTIVE LINEAR CONSTRAINT.
*  S   PLMINS  DETERMINATION OF THE NEW ACTIVE SIMPLE BOUND.
*  S   PLMINT  DETERMINATION OF THE NEW ACTIVE TRUST REGION BOUND.
*  S   PLADF1  CONSTRAINT ADDITION.
*  S   PLRMF0  CONSTRAINT DELETION.
*  S   MXPDGF  GILL-MURRAY DECOMPOSITION OF A DENSE SYMMETRIC MATRIX.
*  S   MXPDGB  BACK SUBSTITUTION AFTER GILL-MURRAY DECOMPOSITION.
*  S   MXDPRB  BACK SUBSTITUTION AFTER CHOLESKI DECOMPOSITION.
*  S   MXDSMM  MATRIX VECTOR PRODUCT.
*  S   MXVDIR  VECTOR AUGMENTED BY THE SCALED VECTOR.
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*  S   MXVCOP  COPYING OF A VECTOR.
*  S   MXVINA  ABSOLUTE VALUES OF ELEMENTS OF INTEGER VECTOR.
*  S   MXVINV  CHANGE OF INTEGER VECTOR AFTER CONSTRAINT ADDITION.
*  S   MXVNEG  COPYING OF A VECTOR WITH CHANGE OF THE SIGN.
*  S   MXVSET  INITIATION OF A VECTOR.
*
* L.LUKSAN: DUAL METHOD FOR SOLVING A SPECIAL PROBLEM OF QUADRATIC
* PROGRAMMING AS A SUBPROBLEM AT LINEARLY CONSTRAINED NONLINEAR MINIMAX
* APPROXIMATION. KYBERNETIKA 20 (1984) 445-457.
*
      SUBROUTINE PLQDF1(NF,NA,NC,X,IX,XL,XU,AF,AFD,IA,IAA,AG,AR,AZ,CF,
     +                  IC,CL,CU,CG,G,H,S,MFP,KBF,KBC,IDECF,ETA0,ETA2,
     +                  ETA9,EPS7,EPS9,XNORM,UMAX,GMAX,N,ITERQ)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS7,EPS9,ETA0,ETA2,ETA9,GMAX,UMAX,XNORM
      INTEGER IDECF,ITERQ,KBC,KBF,MFP,N,NA,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AF(*),AFD(*),AG(*),AR(*),AZ(*),CF(*),CG(*),CL(*),
     +                 CU(*),G(*),H(*),S(*),X(*),XL(*),XU(*)
      INTEGER IA(*),IAA(*),IC(*),IX(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NADD,NDECF,NFG,NFH,NFV,NIT,NRED,NREM,NRES
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BET,CON,E,GAM,PAR,Q,QO,SNORM,STEP,STEP1,STEP2,T,
     +                 TEMP
      INTEGER I,IER,INEW,INF,IOLD,J,K,KA,KC,KNEW,KOLD,KREM,L,NAA,NAR
      INTEGER NCYC,MCYC
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT,MXVMAX
      EXTERNAL MXVDOT,MXVMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL MXDPGB,MXDPGF,MXDPRB,MXDSMM,MXVCOP,MXVDIR,MXVINA,MXVINV,
     +         MXVMUL,MXVNEG,MXVSET,PLADF1,PLDLAG,PLMINA,PLMINL,PLMINS,
     +         PLRMF0
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MIN,SIGN
C     ..
C     .. Common blocks ..
      COMMON /STAT/NDECF,NRES,NRED,NREM,NADD,NIT,NFV,NFG,NFH
C     ..
      NADD = 0
      NREM = 0
      T = 1.0D0
      CON = ETA9
      IF (IDECF.LT.0) IDECF = 1
      IF (IDECF.EQ.0) THEN
*
*     GILL-MURRAY DECOMPOSITION
*
          TEMP = ETA2
          CALL MXDPGF(NF,H,INF,TEMP,STEP)
          NDECF = NDECF + 1
          IDECF = 1
      END IF

      IF (IDECF.GE.2 .AND. IDECF.LE.8) THEN
          ITERQ = -10
          RETURN

      END IF
*
*     INITIATION
*
      NRED = 0
      NCYC = 0
      MCYC = MAX(1000,100*(NA+NC))
      IF (MFP.EQ.3) GO TO 10
      N = NF
      NAA = 0
      NAR = 0
C      XNORM = -ETA9
      XNORM = -1.0D20
      Q = 2.0D0 * XNORM
      CALL MXVINA(NA,IA)
      IF (KBF.GT.0) CALL MXVINA(NF,IX)
      IF (KBC.GT.0) CALL MXVINA(NC,IC)
*
*     DIRECTION DETERMINATION
*
   10 CALL MXVSET(NF,0.0D0,S)
      NCYC = NCYC + 1
      DO 20 J = 1,NAA
          L = IAA(J)
          IF (L.GT.NC) THEN
              L = L - NC
              CALL MXVDIR(NF,AZ(J),AG((L-1)*NF+1),S,S)

          ELSE IF (L.GT.0) THEN
              CALL MXVDIR(NF,AZ(J),CG((L-1)*NF+1),S,S)

          ELSE
              L = -L
              S(L) = S(L) + AZ(J)
          END IF

   20 CONTINUE
      CALL MXVCOP(NF,S,G)
      IF (NAA.GT.0) THEN
          IF (IDECF.EQ.1) THEN
              CALL MXDPGB(NF,H,S,0)

          ELSE IF (IDECF.EQ.9) THEN
              CALL MXDSMM(NF,H,G,S)

          ELSE
              CALL MXVMUL(NF,H,S,S,-1)
          END IF

      END IF
*
*     INITIAL MINIMAX VARIABLE
*
      IF (NAA.EQ.1) THEN
          IF (INEW.LE.NC) THEN
          ELSE
              TEMP = AF(INEW-NC) + MXVDOT(NF,AG((INEW-NC-1)*NF+1),S)
              XNORM = -SIGN(1,KNEW)*TEMP
          ENDIF
      END IF
*
*     CHECK OF FEASIBILITY
*
      INEW = 0
      PAR = 0.0D0
      CALL PLMINA(NF,NA,NC,AF,AFD,IA,AG,S,INEW,KNEW,EPS9,XNORM,PAR)
      IF (NAA.GT.0) THEN
          CALL PLMINL(NF,NC,CF,IC,CL,CU,CG,S,KBC,INEW,KNEW,EPS9,PAR)
          CALL PLMINS(NF,IX,X,XL,XU,S,KBF,INEW,KNEW,EPS9,PAR)
      END IF

      IF (INEW.EQ.0) THEN
*
*     SOLUTION ACHIEVED
*
          CALL MXVNEG(NF,G,G)
          ITERQ = 2
          RETURN

      ELSE IF (NCYC.GE.MCYC) THEN
*
*     CYCLE OCCURRED
*
          CALL MXVNEG(NF,G,G)
          ITERQ = 4
          RETURN

      ELSE
*
*     IRREGULAR CASE
*
          QO = Q
          Q=0.5D0 * MXVDOT(NF,G,S) + XNORM
          IF (Q.LE.QO) THEN
              CALL MXVNEG(NF,G,G)
              ITERQ = 3
              RETURN
          END IF

          SNORM = 0.0D0
      END IF

   30 IER = 0
*
*     STEPSIZE DETERMINATION
*
      CALL PLADF1(NF,NC,IA,IAA,AG,AR,CG,H,S,G,IDECF,N,INEW,KNEW,IER,
     +            EPS7,GMAX,UMAX,E,T)
      CALL PLDLAG(NF,NC,IA,IAA,S,N,KOLD)
      IF (KOLD.EQ.0) THEN
*
*     ZERO STEPSIZE
*
          STEP1 = 0.0D0
          STEP = STEP1
          SNORM = SIGN(1,KNEW)
          XNORM = XNORM - PAR

      ELSE
*
*     PRIMAL STEPSIZE
*
          CALL MXDPRB(NAA,AR,S,1)
          BET = E - MXVDOT(NAA,S,G)
          GAM = BET/MXVDOT(NAA,S,S)
          UMAX = BET*GAM + UMAX
          IF (UMAX.LE.EPS7*GMAX) THEN
              STEP1 = CON

          ELSE
              STEP1 = -PAR/UMAX
          END IF
*
*     DUAL STEPSIZE
*
          CALL MXDPRB(NAA,AR,S,-1)
          CALL MXDPRB(NAA,AR,G,-1)
          CALL MXVDIR(NAA,GAM,S,G,G)
          IF (KNEW.LT.0) CALL MXVNEG(NAA,G,G)
          STEP = MXVMAX(NAA,G)
          IOLD = 0
          STEP2 = CON
          DO 40 J = 1,NAA
              L = IAA(J)
              IF (L.GT.NC) THEN
                  L = L - NC
                  K = IA(L)

              ELSE IF (L.GT.0) THEN
                  K = IC(L)

              ELSE
                  L = -L
                  K = IX(L)
              END IF

              IF (K.LE.-5) THEN

              ELSE IF ((K.EQ.-1.OR.K.EQ.-3.) .AND. G(J).LE.0.0D0) THEN

              ELSE IF ((K.EQ.-2.OR.K.EQ.-4.) .AND. G(J).GE.0.0D0) THEN

              ELSE IF (ABS(G(J)).LE.ETA0*STEP) THEN

              ELSE
                  TEMP = AZ(J)/G(J)
                  IF (STEP2.GT.TEMP) THEN
                      IOLD = J
                      STEP2 = TEMP
                  END IF

              END IF

   40     CONTINUE
*
*     FINAL STEPSIZE
*
          STEP = MIN(STEP1,STEP2)
          IF (STEP.GE.CON) THEN
*
*     FEASIBLE SOLUTION DOES NOT EXIST
*
              ITERQ = -1
              RETURN

          END IF
*
*     NEW LAGRANGE MULTIPLIERS
*
          CALL MXVDIR(NAA,-STEP,G,AZ,AZ)
          SNORM = SNORM + SIGN(1,KNEW)*STEP
          XNORM = XNORM + SIGN(1,KNEW)*STEP*GAM
          PAR = PAR - (STEP/STEP1)*PAR
      END IF

      IF (STEP.EQ.STEP1) THEN
          IF (N.LT.0) THEN
*
*     IMPOSSIBLE SITUATION
*
              ITERQ = -5
              RETURN

          END IF
*
*     CONSTRAINT ADDITION
*
          IF (IER.EQ.0) THEN
              N = N - 1
              NAA = NAA + 1
              NAR = NAR + NAA
              AZ(NAA) = SNORM
          END IF

          IF (INEW.GT.NC) THEN
              KA = INEW - NC
              CALL MXVINV(IA,KA,KNEW)

          ELSE IF (INEW.GT.0) THEN
              KC = INEW
              CALL MXVINV(IC,KC,KNEW)

          ELSE IF (ABS(KNEW).EQ.1) THEN
              I = -INEW
              CALL MXVINV(IX,I,KNEW)

          ELSE
              I = -INEW
              IF (KNEW.GT.0) IX(I) = -3
              IF (KNEW.LT.0) IX(I) = -4
          END IF

          NRED = NRED + 1
          NADD = NADD + 1
          GO TO 10

      ELSE
*
*     CONSTRAINT DELETION
*
          DO 50 J = IOLD,NAA - 1
              AZ(J) = AZ(J+1)
   50     CONTINUE
          CALL PLRMF0(NF,NC,IX,IA,IAA,AR,IC,G,N,IOLD,KREM,IER)
          NAR = NAR - NAA
          NAA = NAA - 1
          CALL MXVINA(NA,IA)
          IF (KBC.GT.0) CALL MXVINA(NC,IC)
          IF (KBF.GT.0) CALL MXVINA(NF,IX)
          DO 60 J = 1,NAA
              L = IAA(J)
              IF (L.GT.NC) THEN
                  L = L - NC
                  IA(L) = -IA(L)

              ELSE IF (L.GT.0) THEN
                  IC(L) = -IC(L)

              ELSE
                  L = -L
                  IX(L) = -IX(L)
              END IF

   60     CONTINUE
          GO TO 30

      END IF

      END
* SUBROUTINE PLMINA             ALL SYSTEMS                   90/12/01
* PORTABILITY : ALL SYSTEMS
* 90/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE NEW ACTIVE FUNCTION.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NA  NUMBER OF CURRENT LINEAR APPROXIMATED FUNCTIONS.
*  II  NC  NUMBER OF CURRENT LINEAR CONSTRAINTS.
*  RI  AF(NA)  VECTOR CONTAINING VALUES OF THE APPROXIMATED
*         FUNCTIONS.
*  RO  AFD(NA)  VECTOR CONTAINING INCREMENTS OF THE APPROXIMATED
*         FUNCTIONS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  RI  AG(NF*NA)  VECTOR CONTAINING SCALING PARAMETERS.
*  RI  S(NF)  DIRECTION VECTOR.
*  IO  INEW  INDEX OF THE NEW ACTIVE FUNCTION.
*  IO  KNEW  SIGNUM OF THE NEW ACTIVE GRADIENT.
*  RI  EPS9  TOLERANCE FOR ACTIVE FUNCTIONS.
*  RO  XNORM  VALUE OF LINEARIZED MINIMAX FUNCTION.
*  RA  PAR  AUXILIARY VARIABLE.
*
* SUBPROGRAMS USED :
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLMINA(NF,NA,NC,AF,AFD,IA,AG,S,INEW,KNEW,EPS9,XNORM,
     +                  PAR)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9,PAR,XNORM
      INTEGER INEW,KNEW,NA,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AF(*),AFD(*),AG(*),S(*)
      INTEGER IA(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION POM,TEMP
      INTEGER JCG,KA
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN
C     ..
      JCG = 1
      DO 10 KA = 1,NA
          IF (IA(KA).GT.0) THEN
              TEMP = MXVDOT(NF,AG(JCG),S)
              AFD(KA) = TEMP
              TEMP = AF(KA) + TEMP
              IF (IA(KA).EQ.1 .OR. IA(KA).GE.3) THEN
                  POM = XNORM + TEMP
                  IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(XNORM),1.0D0))) THEN
                      INEW = KA + NC
                      KNEW = 1
                      PAR = POM
                  END IF

              END IF

              IF (IA(KA).EQ.2 .OR. IA(KA).GE.3) THEN
                  POM = XNORM - TEMP
                  IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(XNORM),1.0D0))) THEN
                      INEW = KA + NC
                      KNEW = -1
                      PAR = POM
                  END IF

              END IF

          END IF

          JCG = JCG + NF
   10 CONTINUE
      RETURN

      END
* SUBROUTINE PLMINL             ALL SYSTEMS                   90/12/01
* PORTABILITY : ALL SYSTEMS
* 90/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE NEW ACTIVE LINEAR CONSTRAINT.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF CONSTRAINTS.
*  RI  CF(NC)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCTIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CL(NC)  VECTOR CONTAINING LOWER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CU(NC)  VECTOR CONTAINING UPPER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RI  S(NF)  DIRECTION VECTOR.
*  II  KBC  SPECIFICATION OF LINEAR CONSTRAINTS. KBC=0-NO LINEAR
*         CONSTRAINTS. KBC=1-ONE SIDED LINEAR CONSTRAINTS. KBC=2=TWO
*         SIDED LINEAR CONSTRAINTS.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*  IO  KNEW  SIGNUM OF THE NEW ACTIVE NORMAL.
*  RI  EPS9  TOLERANCE FOR ACTIVE CONSTRAINTS.
*  RA  PAR  AUXILIARY VARIABLE.
*
* SUBPROGRAMS USED :
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLMINL(NF,NC,CF,IC,CL,CU,CG,S,KBC,INEW,KNEW,EPS9,PAR)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9,PAR
      INTEGER INEW,KBC,KNEW,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CG(*),CL(*),CU(*),S(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION POM,TEMP
      INTEGER JCG,KC
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN
C     ..
      IF (KBC.GT.0) THEN
          JCG = 1
          DO 10 KC = 1,NC
              IF (IC(KC).GT.0) THEN
                  TEMP = CF(KC) + MXVDOT(NF,CG(JCG),S)
                  IF (IC(KC).EQ.1 .OR. IC(KC).GE.3) THEN
                      POM = TEMP - CL(KC)
                      IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(CL(KC)),
     +                    1.0D0))) THEN
                          INEW = KC
                          KNEW = 1
                          PAR = POM
                      END IF

                  END IF

                  IF (IC(KC).EQ.2 .OR. IC(KC).GE.3) THEN
                      POM = CU(KC) - TEMP
                      IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(CU(KC)),
     +                    1.0D0))) THEN
                          INEW = KC
                          KNEW = -1
                          PAR = POM
                      END IF

                  END IF

              END IF

              JCG = JCG + NF
   10     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE PLMINS             ALL SYSTEMS                   91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE NEW ACTIVE SIMPLE BOUND.
*
* PARAMETERS :
*  II  NF DECLARED NUMBER OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  XO(NF)  SAVED VECTOR OF VARIABLES.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  S(NF)  DIRECTION VECTOR.
*  II  KBF  SPECIFICATION OF SIMPLE BOUNDS. KBF=0-NO SIMPLE BOUNDS.
*         KBF=1-ONE SIDED SIMPLE BOUNDS. KBF=2=TWO SIDED SIMPLE BOUNDS.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*  IO  KNEW  SIGNUM OF THE NEW NORMAL.
*  RI  EPS9  TOLERANCE FOR ACTIVE CONSTRAINTS.
*  RA  PAR  AUXILIARY VARIABLE.
*
      SUBROUTINE PLMINS(NF,IX,XO,XL,XU,S,KBF,INEW,KNEW,EPS9,PAR)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9,PAR
      INTEGER INEW,KBF,KNEW,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION S(*),XL(*),XO(*),XU(*)
      INTEGER IX(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION POM,TEMP
      INTEGER I
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN
C     ..
      IF (KBF.GT.0) THEN
          DO 10 I = 1,NF
              IF (IX(I).GT.0) THEN
                  TEMP = 1.0D0
                  IF (IX(I).EQ.1 .OR. IX(I).GE.3) THEN
                      POM = XO(I) + S(I)*TEMP - XL(I)
                      IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(XL(I)),
     +                    TEMP))) THEN
                          INEW = -I
                          KNEW = 1
                          PAR = POM
                      END IF

                  END IF

                  IF (IX(I).EQ.2 .OR. IX(I).GE.3) THEN
                      POM = XU(I) - S(I)*TEMP - XO(I)
                      IF (POM.LT.MIN(PAR,-EPS9*MAX(ABS(XU(I)),
     +                    TEMP))) THEN
                          INEW = -I
                          KNEW = -1
                          PAR = POM
                      END IF

                  END IF

              END IF

   10     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE PLDLAG               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* VECTOR OF LAGRANGE MULTIPLIERS FOR DUAL QP METHOD IS DETERMINED.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEARIZED CONSTRAINTS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  II  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RO  AZ(NF+1)  OUTPUT VECTOR.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  IA  KOLD  AUXILIARY VARIABLE.
*
      SUBROUTINE PLDLAG(NF,NC,IA,IAA,AZ,N,KOLD)
C     .. Scalar Arguments ..
      INTEGER KOLD,N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AZ(*)
      INTEGER IA(*),IAA(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER J,L,NAA
C     ..
      NAA = NF - N
      KOLD = 0
      DO 10 J = 1,NAA
          L = IAA(J)
          IF (L.GT.NC) THEN
              L = L - NC
              TEMP = 1.0D0
              IF (IA(L).EQ.-2 .OR. IA(L).EQ.-4) TEMP = -TEMP
              AZ(J) = TEMP
              KOLD = 1

          ELSE
              AZ(J) = 0.0D0
          END IF

   10 CONTINUE
      RETURN

      END
* SUBROUTINE PLADF1               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* TRIANGULAR DECOMPOSITION OF KERNEL OF THE GENERAL PROJECTION
* IS UPDATED AFTER FUNCTION OR CONSTRAINT ADDITION.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEARIZED CONSTRAINTS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  IU  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RI  AG(NF*NA)  MATRIX WHOSE COLUMNS ARE GRADIENTS OF THE LINEAR
*          APPROXIMATED FUNCTIONS.
*  RU  AR((NF+1)*(NF+2)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF
*        THE ORTHOGONAL PROJECTION.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RI  H(NF*(NF+1)/2)  TRIANGULAR DECOMPOSITION OR INVERSION OF THE
*         HESSIAN MATRIX APPROXIMATION.
*  RA  S(NF+1)  AUXILIARY VECTOR.
*  RO  G(NF+1)  VECTOR USED IN THE DUAL RANGE SPACE QUADRATIC
*        PROGRAMMING METHOD.
*  IU  IDECF  DECOMPOSITION INDICATOR. IDECF=0-NO DECOMPOSITION.
*         IDECF=1-GILL-MURRAY DECOMPOSITION. IDECF=9-INVERSION.
*         IDECF=10-DIAGONAL MATRIX.
*  IU  N  ACTUAL NUMBER OF VARIABLES.
*  II  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*  II  KNEW  SIGNUM OF THE NEW ACTIVE GRADIENT.
*  IO  IER  ERROR INDICATOR.
*  RI  EPS7  TOLERANCE FOR LINEAR INDEPENDENCE OF CONSTRAINTS.
*  RO  GMAX  MAXIMUM ABSOLUTE VALUE OF A PARTIAL DERIVATIVE.
*  RO  UMAX  MAXIMUM ABSOLUTE VALUE OF A NEGATIVE LAGRANGE MULTIPLIER.
*  RO  E  AUXILIARY VARIABLE.
*  RI  T  AUXILIARY VARIABLE.
*
* SUBPROGRAMS USED :
*  S   MXPDGB  BACK SUBSTITUTION AFTER GILL-MURRAY DECOMPOSITION.
*  S   MXDPRB  BACK SUBSTITUTION AFTER CHOLESKI DECOMPOSITION.
*  S   MXDSMM  MATRIX-VECTOR PRODUCT.
*  S   MXDSMV  COPYING OF A ROW OF DENSE SYMMETRIC MATRIX.
*  S   MXVCOP  COPYING OF A VECTOR.
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLADF1(NF,NC,IA,IAA,AG,AR,CG,H,S,G,IDECF,N,INEW,KNEW,
     +                  IER,EPS7,GMAX,UMAX,E,T)
C     .. Scalar Arguments ..
      DOUBLE PRECISION E,EPS7,GMAX,T,UMAX
      INTEGER IDECF,IER,INEW,KNEW,N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AG(*),AR(*),CG(*),G(*),H(*),S(*)
      INTEGER IA(*),IAA(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NADD,NDECF,NFG,NFH,NFV,NIT,NRED,NREM,NRES
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION POM,TEMP
      INTEGER J,JAG,JOB,K,L,NAA,NAR
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
C     .. External Subroutines ..
      EXTERNAL MXDPGB,MXDPRB,MXDSMM,MXDSMV,MXVCOP,MXVMUL,MXVSET
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SIGN,SQRT
C     ..
C     .. Common blocks ..
      COMMON /STAT/NDECF,NRES,NRED,NREM,NADD,NIT,NFV,NFG,NFH
C     ..
      JOB = 1
      E = 0.0D0
      IF (INEW.GT.NC) E = SIGN(1,KNEW)
      IER = 0
      IF (JOB.EQ.0 .AND. N.LT.0) IER = 2
      IF (INEW.EQ.0) IER = 3
      IF (IDECF.GE.2 .AND. IDECF.LE.8) IER = -2
      IF (IER.NE.0) RETURN
      NAA = NF - N
      NAR = NAA* (NAA+1)/2
      IF (INEW.GT.NC) THEN
          JAG = (INEW-NC-1)*NF + 1
          IF (IDECF.EQ.1) THEN
              CALL MXVCOP(NF,AG(JAG),S)
              CALL MXDPGB(NF,H,S,0)

          ELSE IF (IDECF.EQ.9) THEN
              CALL MXDSMM(NF,H,AG(JAG),S)

          ELSE
              CALL MXVCOP(NF,AG(JAG),S)
              CALL MXVMUL(NF,H,S,S,-1)
          END IF

          GMAX = MXVDOT(NF,AG(JAG),S) + T

      ELSE IF (INEW.GT.0) THEN
          JAG = (INEW-1)*NF + 1
          IF (IDECF.EQ.1) THEN
              CALL MXVCOP(NF,CG(JAG),S)
              CALL MXDPGB(NF,H,S,0)

          ELSE IF (IDECF.EQ.9) THEN
              CALL MXDSMM(NF,H,CG(JAG),S)

          ELSE
              CALL MXVCOP(NF,CG(JAG),S)
              CALL MXVMUL(NF,H,S,S,-1)
          END IF

          GMAX = MXVDOT(NF,CG(JAG),S)

      ELSE
          K = -INEW
          IF (IDECF.EQ.1) THEN
              CALL MXVSET(NF,0.0D0,S)
              S(K) = 1.0D0
              CALL MXDPGB(NF,H,S,0)

          ELSE IF (IDECF.EQ.9) THEN
              CALL MXDSMV(NF,H,S,K)

          ELSE
              CALL MXVSET(NF,0.0D0,S)
              S(K) = 1.0D0/H(K)
          END IF

          GMAX = S(K)
      END IF

      IF (NAA.GT.0) THEN
          POM = T*E
          DO 10 J = 1,NAA
              L = IAA(J)
              IF (L.GT.NC) THEN
                  L = L - NC
                  G(J) = MXVDOT(NF,AG((L-1)*NF+1),S)
                  IF (INEW.GT.NC) THEN
                      TEMP = POM
                      IF (IA(L).EQ.-2 .OR. IA(L).EQ.-4) TEMP = -TEMP
                      G(J) = G(J) + TEMP
                  END IF

              ELSE IF (L.GT.0) THEN
                  G(J) = MXVDOT(NF,CG((L-1)*NF+1),S)

              ELSE
                  L = -L
                  G(J) = S(L)
              END IF

   10     CONTINUE
      END IF

      IF (N.LT.0) THEN
          CALL MXDPRB(NAA,AR,G,1)
          UMAX = 0.0D0
          IER = 2
          RETURN

      ELSE IF (NAA.EQ.0) THEN
          UMAX = GMAX

      ELSE
          CALL MXDPRB(NAA,AR,G,1)
          UMAX = GMAX - MXVDOT(NAA,G,G)
          CALL MXVCOP(NAA,G,AR(NAR+1))
      END IF

      IF (UMAX.LE.EPS7*GMAX) THEN
          IER = 1
          RETURN

      ELSE
          NAA = NAA + 1
          NAR = NAR + NAA
          IAA(NAA) = INEW
          AR(NAR) = SQRT(UMAX)
          IF (JOB.EQ.0) THEN
              N = N - 1
              NADD = NADD + 1
          END IF

      END IF

      RETURN

      END
* SUBROUTINE PLRMF0             ALL SYSTEMS                   91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* OPERATIONS AFTER CONSTRAINT DELETION.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF CONSTRAINTS.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  IU  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RU  AR((NF+1)*(NF+2)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF THE
*         ORTHOGONAL PROJECTION.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RA  S(NF+1)  AUXILIARY VECTOR.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  II  IOLD  INDEX OF THE OLD ACTIVE CONSTRAINT.
*  IO  KREM  AUXILIARY VARIABLE.
*  IO  IER  ERROR INDICATOR.
*
* SUBPROGRAMS USED :
*  S   PLRMR0  CORRECTION OF KERNEL OF THE ORTHOGONAL PROJECTION
*         AFTER CONSTRAINT DELETION.
*
      SUBROUTINE PLRMF0(NF,NC,IX,IA,IAA,AR,IC,S,N,IOLD,KREM,IER)
C     .. Scalar Arguments ..
      INTEGER IER,IOLD,KREM,N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AR(*),S(*)
      INTEGER IA(*),IAA(*),IC(*),IX(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NADD,NDECF,NFG,NFH,NFV,NIT,NRED,NREM,NRES
C     ..
C     .. Local Scalars ..
      INTEGER L
C     ..
C     .. External Subroutines ..
      EXTERNAL PLRMR0
C     ..
C     .. Common blocks ..
      COMMON /STAT/NDECF,NRES,NRED,NREM,NADD,NIT,NFV,NFG,NFH
C     ..
      CALL PLRMR0(NF,IAA,AR,S,N,IOLD,KREM,IER)
      N = N + 1
      NREM = NREM + 1
      L = IAA(NF-N+1)
      IF (L.GT.NC) THEN
          L = L - NC
          IA(L) = -IA(L)

      ELSE IF (L.GT.0) THEN
          IC(L) = -IC(L)

      ELSE
          L = -L
          IX(L) = -IX(L)
      END IF

      RETURN

      END
* SUBROUTINE PLRMR0               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* TRIANGULAR DECOMPOSITION OF KERNEL OF THE ORTHOGONAL PROJECTION IS
* UPDATED AFTER CONSTRAINT DELETION.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  IU  ICA(NF)  VECTOR CONTAINING INDICES OF ACTIVE CONSTRAINTS.
*  RU  CR(NF*(NF+1)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF THE
*         ORTHOGONAL PROJECTION.
*  RA  G(NF)  AUXILIARY VECTOR.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  II  IOLD  INDEX OF THE OLD ACTIVE CONSTRAINT.
*  IO  KREM  AUXILIARY VARIABLE.
*  IO  IER  ERROR INDICATOR.
*
* SUBPROGRAMS USED :
*  S   MXVCOP  COPYING OF A VECTOR.
*  S   MXVORT  DETERMINATION OF AN ELEMENTARY ORTHOGONAL MATRIX FOR
*         PLANE ROTATION.
*  S   MXVROT  PLANE ROTATION OF A VECTOR.
*  S   MXVSET  INITIATION OF A VECTOR.
*
      SUBROUTINE PLRMR0(NF,ICA,CR,G,N,IOLD,KREM,IER)
C     .. Scalar Arguments ..
      INTEGER IER,IOLD,KREM,N,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CR(*),G(*)
      INTEGER ICA(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CK,CL
      INTEGER I,J,K,KC,L,NCA
C     ..
C     .. External Subroutines ..
      EXTERNAL MXVCOP,MXVORT,MXVROT,MXVSET
C     ..
      NCA = NF - N
      IF (IOLD.LT.NCA) THEN
          K = IOLD* (IOLD-1)/2
          KC = ICA(IOLD)
          CALL MXVCOP(IOLD,CR(K+1),G)
          CALL MXVSET(NCA-IOLD,0.0D0,G(IOLD+1))
          K = K + IOLD
          DO 20 I = IOLD + 1,NCA
              K = K + I
              CALL MXVORT(CR(K-1),CR(K),CK,CL,IER)
              CALL MXVROT(G(I-1),G(I),CK,CL,IER)
              L = K
              DO 10 J = I,NCA - 1
                  L = L + J
                  CALL MXVROT(CR(L-1),CR(L),CK,CL,IER)
   10         CONTINUE
   20     CONTINUE
          K = IOLD* (IOLD-1)/2
          DO 30 I = IOLD,NCA - 1
              L = K + I
              ICA(I) = ICA(I+1)
              CALL MXVCOP(I,CR(L+1),CR(K+1))
              K = L
   30     CONTINUE
          ICA(NCA) = KC
          CALL MXVCOP(NCA,G,CR(K+1))
          KREM = 1
      END IF

      RETURN

      END
* SUBROUTINE MXDPGB                ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* SOLUTION OF A SYSTEM OF LINEAR EQUATIONS WITH A DENSE SYMMETRIC
* POSITIVE DEFINITE MATRIX A+E USING THE FACTORIZATION A+E=L*D*TRANS(L)
* OBTAINED BY THE SUBROUTINE MXDPGF.
*
* PARAMETERS :
*  II  N ORDER OF THE MATRIX A.
*  RI  A(N*(N+1)/2) FACTORIZATION A+E=L*D*TRANS(L) OBTAINED BY THE
*         SUBROUTINE MXDPGF.
*  RU  X(N)  ON INPUT THE RIGHT HAND SIDE OF A SYSTEM OF LINEAR
*         EQUATIONS. ON OUTPUT THE SOLUTION OF A SYSTEM OF LINEAR
*         EQUATIONS.
*  II  JOB  OPTION. IF JOB=0 THEN X:=(A+E)**(-1)*X. IF JOB>0 THEN
*         X:=L**(-1)*X. IF JOB<0 THEN X:=TRANS(L)**(-1)*X.
*
* METHOD :
* BACK SUBSTITUTION
*
      SUBROUTINE MXDPGB(N,A,X,JOB)
*     .. Scalar Arguments ..
      INTEGER          JOB,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*),X(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,II,IJ,J
*     ..
      IF (JOB.GE.0) THEN
*
*     PHASE 1 : X:=L**(-1)*X
*
          IJ = 0
          DO 20 I = 1,N
              DO 10 J = 1,I - 1
                  IJ = IJ + 1
                  X(I) = X(I) - A(IJ)*X(J)
   10         CONTINUE
              IJ = IJ + 1
   20     CONTINUE
      END IF

      IF (JOB.EQ.0) THEN
*
*     PHASE 2 : X:=D**(-1)*X
*
          II = 0
          DO 30 I = 1,N
              II = II + I
              X(I) = X(I)/A(II)
   30     CONTINUE
      END IF

      IF (JOB.LE.0) THEN
*
*     PHASE 3 : X:=TRANS(L)**(-1)*X
*
          II = N* (N-1)/2
          DO 50 I = N - 1,1,-1
              IJ = II
              DO 40 J = I + 1,N
                  IJ = IJ + J - 1
                  X(I) = X(I) - A(IJ)*X(J)
   40         CONTINUE
              II = II - I
   50     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE MXDPGF                ALL SYSTEMS                89/12/01
* PORTABILITY : ALL SYSTEMS
* 89/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* FACTORIZATION A+E=L*D*TRANS(L) OF A DENSE SYMMETRIC POSITIVE DEFINITE
* MATRIX A+E WHERE D AND E ARE DIAGONAL POSITIVE DEFINITE MATRICES AND
* L IS A LOWER TRIANGULAR MATRIX. IF A IS SUFFICIENTLY POSITIVE
* DEFINITE THEN E=0.
*
* PARAMETERS :
*  II  N ORDER OF THE MATRIX A.
*  RU  A(N*(N+1)/2)  ON INPUT A GIVEN DENSE SYMMETRIC (USUALLY POSITIVE
*         DEFINITE) MATRIX A STORED IN THE PACKED FORM. ON OUTPUT THE
*         COMPUTED FACTORIZATION A+E=L*D*TRANS(L).
*  IO  INF  AN INFORMATION OBTAINED IN THE FACTORIZATION PROCESS. IF
*         INF=0 THEN A IS SUFFICIENTLY POSITIVE DEFINITE AND E=0. IF
*         INF<0 THEN A IS NOT SUFFICIENTLY POSITIVE DEFINITE AND E>0. IF
*         INF>0 THEN A IS INDEFINITE AND INF IS AN INDEX OF THE
*         MOST NEGATIVE DIAGONAL ELEMENT USED IN THE FACTORIZATION
*         PROCESS.
*  RU  ALF  ON INPUT A DESIRED TOLERANCE FOR POSITIVE DEFINITENESS. ON
*         OUTPUT THE MOST NEGATIVE DIAGONAL ELEMENT USED IN THE
*         FACTORIZATION PROCESS (IF INF>0).
*  RO  TAU  MAXIMUM DIAGONAL ELEMENT OF THE MATRIX E.
*
* METHOD :
* P.E.GILL, W.MURRAY : NEWTON TYPE METHODS FOR UNCONSTRAINED AND
* LINEARLY CONSTRAINED OPTIMIZATION, MATH. PROGRAMMING 28 (1974)
* PP. 311-350.
*
      SUBROUTINE MXDPGF(N,A,INF,ALF,TAU)
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALF,TAU
      INTEGER          INF,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION BET,DEL,GAM,RHO,SIG,TOL
      INTEGER          I,IJ,IK,J,K,KJ,KK,L
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,MAX
*     ..
      L = 0
      INF = 0
      TOL = ALF
*
*     ESTIMATION OF THE MATRIX NORM
*
      ALF = 0.0D0
      BET = 0.0D0
      GAM = 0.0D0
      TAU = 0.0D0
      KK = 0
      DO 20 K = 1,N
          KK = KK + K
          BET = MAX(BET,ABS(A(KK)))
          KJ = KK
          DO 10 J = K + 1,N
              KJ = KJ + J - 1
              GAM = MAX(GAM,ABS(A(KJ)))
   10     CONTINUE
   20 CONTINUE
      BET = MAX(TOL,BET,GAM/N)
*      DEL = TOL*BET
      DEL = TOL*MAX(BET,1.0D0)
      KK = 0
      DO 60 K = 1,N
          KK = KK + K
*
*     DETERMINATION OF A DIAGONAL CORRECTION
*
          SIG = A(KK)
          IF (ALF.GT.SIG) THEN
              ALF = SIG
              L = K
          END IF

          GAM = 0.0D0
          KJ = KK
          DO 30 J = K + 1,N
              KJ = KJ + J - 1
              GAM = MAX(GAM,ABS(A(KJ)))
   30     CONTINUE
          GAM = GAM*GAM
          RHO = MAX(ABS(SIG),GAM/BET,DEL)
          IF (TAU.LT.RHO-SIG) THEN
              TAU = RHO - SIG
              INF = -1
          END IF
*
*     GAUSSIAN ELIMINATION
*
          A(KK) = RHO
          KJ = KK
          DO 50 J = K + 1,N
              KJ = KJ + J - 1
              GAM = A(KJ)
              A(KJ) = GAM/RHO
              IK = KK
              IJ = KJ
              DO 40 I = K + 1,J
                  IK = IK + I - 1
                  IJ = IJ + 1
                  A(IJ) = A(IJ) - A(IK)*GAM
   40         CONTINUE
   50     CONTINUE
   60 CONTINUE
      IF (L.GT.0 .AND. ABS(ALF).GT.DEL) INF = L
      RETURN

      END
* SUBROUTINE MXDPRB                ALL SYSTEMS                89/12/01
* PORTABILITY : ALL SYSTEMS
* 89/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* SOLUTION OF A SYSTEM OF LINEAR EQUATIONS WITH A DENSE SYMMETRIC
* POSITIVE DEFINITE MATRIX A USING THE FACTORIZATION A=TRANS(R)*R.
*
* PARAMETERS :
*  II  N ORDER OF THE MATRIX A.
*  RI  A(N*(N+1)/2) FACTORIZATION A=TRANS(R)*R.
*  RU  X(N)  ON INPUT THE RIGHT HAND SIDE OF A SYSTEM OF LINEAR
*         EQUATIONS. ON OUTPUT THE SOLUTION OF A SYSTEM OF LINEAR
*         EQUATIONS.
*  II  JOB  OPTION. IF JOB=0 THEN X:=A**(-1)*X. IF JOB>0 THEN
*         X:=TRANS(R)**(-1)*X. IF JOB<0 THEN X:=R**(-1)*X.
*
* METHOD :
* BACK SUBSTITUTION
*
      SUBROUTINE MXDPRB(N,A,X,JOB)
*     .. Scalar Arguments ..
      INTEGER          JOB,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*),X(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,II,IJ,J
*     ..
      IF (JOB.GE.0) THEN
*
*     PHASE 1 : X:=TRANS(R)**(-1)*X
*
          IJ = 0
          DO 20 I = 1,N
              DO 10 J = 1,I - 1
                  IJ = IJ + 1
                  X(I) = X(I) - A(IJ)*X(J)
   10         CONTINUE
              IJ = IJ + 1
              X(I) = X(I)/A(IJ)
   20     CONTINUE
      END IF

      IF (JOB.LE.0) THEN
*
*     PHASE 2 : X:=R**(-1)*X
*
          II = N* (N+1)/2
          DO 40 I = N,1,-1
              IJ = II
              DO 30 J = I + 1,N
                  IJ = IJ + J - 1
                  X(I) = X(I) - A(IJ)*X(J)
   30         CONTINUE
              X(I) = X(I)/A(II)
              II = II - I
   40     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE MXDSMM                ALL SYSTEMS                89/12/01
* PORTABILITY : ALL SYSTEMS
* 89/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* MULTIPLICATION OF A DENSE SYMMETRIC MATRIX A BY A VECTOR X.
*
* PARAMETERS :
*  II  N  ORDER OF THE MATRIX A.
*  RI  A(N*(N+1)/2)  DENSE SYMMETRIC MATRIX STORED IN THE PACKED FORM.
*  RI  X(N)  INPUT VECTOR.
*  RO  Y(N)  OUTPUT VECTOR EQUAL TO  A*X.
*
      SUBROUTINE MXDSMM(N,A,X,Y)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*),X(*),Y(*)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER          I,J,K,L
*     ..
      K = 0
      DO 30 I = 1,N
          TEMP = 0.0D0
          L = K
          DO 10 J = 1,I
              L = L + 1
              TEMP = TEMP + A(L)*X(J)
   10     CONTINUE
          DO 20 J = I + 1,N
              L = L + J - 1
              TEMP = TEMP + A(L)*X(J)
   20     CONTINUE
          Y(I) = TEMP
          K = K + I
   30 CONTINUE
      RETURN

      END
* SUBROUTINE MXDSMV                ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* K-TH ROW OF A DENSE SYMMETRIC MATRIX A IS COPIED TO THE VECTOR X.
*
* PARAMETERS :
*  II  N  ORDER OF THE MATRIX A.
*  RI  A(N*(N+1)/2)  DENSE SYMMETRIC MATRIX STORED IN THE PACKED FORM.
*  RO  X(N)  OUTPUT VECTOR.
*  II  K  INDEX OF COPIED ROW.
*
      SUBROUTINE MXDSMV(N,A,X,K)
*     .. Scalar Arguments ..
      INTEGER          K,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*),X(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,L
*     ..
      L = K* (K-1)/2
      DO 10 I = 1,N
          IF (I.LE.K) THEN
              L = L + 1

          ELSE
              L = L + I - 1
          END IF

          X(I) = A(L)
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVCOP                ALL SYSTEMS                88/12/01
* PORTABILITY : ALL SYSTEMS
* 88/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* COPYING OF A VECTOR.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  X(N)  INPUT VECTOR.
*  RO  Y(N)  OUTPUT VECTOR WHERE Y:= X.
*
      SUBROUTINE MXVCOP(N,X,Y)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*),Y(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
      DO 10 I = 1,N
          Y(I) = X(I)
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVDIR                ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* VECTOR AUGMENTED BY THE SCALED VECTOR.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  A  SCALING FACTOR.
*  RI  X(N)  INPUT VECTOR.
*  RI  Y(N)  INPUT VECTOR.
*  RO  Z(N)  OUTPUT VECTOR WHERE Z:= Y + A*X.
*
      SUBROUTINE MXVDIR(N,A,X,Y,Z)
*     .. Scalar Arguments ..
      DOUBLE PRECISION A
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*),Y(*),Z(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
      DO 10 I = 1,N
          Z(I) = Y(I) + A*X(I)
   10 CONTINUE
      RETURN

      END
* FUNCTION MXVDOT                  ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DOT PRODUCT OF TWO VECTORS.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  X(N)  INPUT VECTOR.
*  RI  Y(N)  INPUT VECTOR.
*  RR  MXVDOT  VALUE OF DOT PRODUCT MXVDOT=TRANS(X)*Y.
*
      DOUBLE PRECISION
     +  FUNCTION MXVDOT(N,X,Y)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*),Y(*)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER          I
*     ..
      TEMP = 0.0D0
      DO 10 I = 1,N
          TEMP = TEMP + X(I)*Y(I)
   10 CONTINUE
      MXVDOT = TEMP
      RETURN

      END
* SUBROUTINE MXVINA             ALL SYSTEMS                   90/12/01
* PORTABILITY : ALL SYSTEMS
* 90/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* ELEMENTS OF THE INTEGER VECTOR ARE REPLACED BY THEIR ABSOLUTE VALUES.
*
* PARAMETERS :
*  II  N DIMENSION OF THE INTEGER VECTOR.
*  IU  IX(N)  INTEGER VECTOR WHICH IS UPDATED SO THAT IX(I):=ABS(IX(I))
*         FOR ALL I.
*
      SUBROUTINE MXVINA(N,IX)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      INTEGER          IX(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     ..
      DO 10 I = 1,N
          IX(I) = ABS(IX(I))
          IF (IX(I).GT.10) IX(I) = IX(I) - 10
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVINV               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* CHANGE OF THE INTEGER VECTOR ELEMENT FOR THE CONSTRAINT ADDITION.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  IU  IX(N)  INTEGER VECTOR.
*  II  I  INDEX OF THE CHANGED ELEMENT.
*  II  JOB  CHANGE SPECIFICATION
*
      SUBROUTINE MXVINV(IX,I,JOB)
*     .. Scalar Arguments ..
      INTEGER          I,JOB
*     ..
*     .. Array Arguments ..
      INTEGER          IX(*)
*     ..
      IF ((IX(I).EQ.3.OR.IX(I).EQ.5) .AND. JOB.LT.0) IX(I) = IX(I) + 1
      IF ((IX(I).EQ.4.OR.IX(I).EQ.6) .AND. JOB.GT.0) IX(I) = IX(I) - 1
      IX(I) = -IX(I)
      RETURN

      END
* FUNCTION MXVMAX               ALL SYSTEMS                   91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* L-INFINITY NORM OF A VECTOR.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  X(N)  INPUT VECTOR.
*  RR  MXVMAX  L-INFINITY NORM OF THE VECTOR X.
*
      DOUBLE PRECISION
     +  FUNCTION MXVMAX(N,X)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,MAX
*     ..
      MXVMAX = 0.0D0
      DO 10 I = 1,N
          MXVMAX = MAX(MXVMAX,ABS(X(I)))
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVMUL             ALL SYSTEMS                   89/12/01
* PORTABILITY : ALL SYSTEMS
* 89/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* VECTOR IS PREMULTIPLIED BY THE POWER OF A DIAGONAL MATRIX.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  D(N)  DIAGONAL MATRIX STORED AS A VECTOR WITH N ELEMENTS.
*  RI  X(N)  INPUT VECTOR.
*  RO  Y(N)  OUTPUT VECTOR WHERE Y:=(D**K)*X.
*  II  K  INTEGER EXPONENT.
*
      SUBROUTINE MXVMUL(N,D,X,Y,K)
*     .. Scalar Arguments ..
      INTEGER          K,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION D(*),X(*),Y(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
*     .. External Subroutines ..
      EXTERNAL         MXVCOP
*     ..
      IF (K.EQ.0) THEN
          CALL MXVCOP(N,X,Y)

      ELSE IF (K.EQ.1) THEN
          DO 10 I = 1,N
              Y(I) = X(I)*D(I)
   10     CONTINUE

      ELSE IF (K.EQ.-1) THEN
          DO 20 I = 1,N
              Y(I) = X(I)/D(I)
   20     CONTINUE

      ELSE
          DO 30 I = 1,N
              Y(I) = X(I)*D(I)**K
   30     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE MXVNEG                ALL SYSTEMS                88/12/01
* PORTABILITY : ALL SYSTEMS
* 88/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* CHANGE THE SIGNS OF VECTOR ELEMENTS.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  X(N)  INPUT VECTOR.
*  RO  Y(N)  OUTPUT VECTOR WHERE Y:= - X.
*
      SUBROUTINE MXVNEG(N,X,Y)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*),Y(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
      DO 10 I = 1,N
          Y(I) = -X(I)
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVORT               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF AN ELEMENTARY ORTHOGONAL MATRIX FOR PLANE ROTATION.
*
* PARAMETERS :
*  RU  XK  FIRST VALUE FOR PLANE ROTATION (XK IS TRANSFORMED TO
*         SQRT(XK**2+XL**2))
*  RU  XL  SECOND VALUE FOR PLANE ROTATION (XL IS TRANSFORMED TO
*         ZERO)
*  RO  CK  DIAGONAL ELEMENT OF THE ELEMENTARY ORTHOGONAL MATRIX.
*  RO  CL  OFF-DIAGONAL ELEMENT OF THE ELEMENTARY ORTHOGONAL MATRIX.
*  IO  IER  INFORMATION ON THE TRANSFORMATION. IER=0-GENERAL PLANE
*         ROTATION. IER=1-PERMUTATION. IER=2-TRANSFORMATION SUPPRESSED.
*
      SUBROUTINE MXVORT(XK,XL,CK,CL,IER)
*     .. Scalar Arguments ..
      DOUBLE PRECISION CK,CL,XK,XL
      INTEGER          IER
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION DEN,POM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,SQRT
*     ..
      IF (XL.EQ.0.0D0) THEN
          IER = 2

      ELSE IF (XK.EQ.0.0D0) THEN
          XK = XL
          XL = 0.0D0
          IER = 1

      ELSE
          IF (ABS(XK).GE.ABS(XL)) THEN
              POM = XL/XK
              DEN = SQRT(1.0D0+POM*POM)
              CK = 1.0D0/DEN
              CL = POM/DEN
              XK = XK*DEN

          ELSE
              POM = XK/XL
              DEN = SQRT(1.0D0+POM*POM)
              CL = 1.0D0/DEN
              CK = POM/DEN
              XK = XL*DEN
          END IF

          XL = 0.0D0
          IER = 0
      END IF

      RETURN

      END
* SUBROUTINE MXVROT               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* PLANE ROTATION IS APPLIED TO TWO VALUES.
*
* PARAMETERS :
*  RU  XK  FIRST VALUE FOR PLANE ROTATION.
*  RU  XL  SECOND VALUE FOR PLANE ROTATION.
*  RI  CK  DIAGONAL ELEMENT OF THE ELEMENTARY ORTHOGONAL MATRIX.
*  RI  CL  OFF-DIAGONAL ELEMENT OF THE ELEMENTARY ORTHOGONAL MATRIX.
*  II  IER  INFORMATION ON THE TRANSFORMATION. IER=0-GENERAL PLANE
*         ROTATION. IER=1-PERMUTATION. IER=2-TRANSFORMATION SUPPRESSED.
*
      SUBROUTINE MXVROT(XK,XL,CK,CL,IER)
*     .. Scalar Arguments ..
      DOUBLE PRECISION CK,CL,XK,XL
      INTEGER          IER
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION YK,YL
*     ..
      IF (IER.EQ.0) THEN
          YK = XK
          YL = XL
          XK = CK*YK + CL*YL
          XL = CL*YK - CK*YL

      ELSE IF (IER.EQ.1) THEN
          YK = XK
          XK = XL
          XL = YK
      END IF

      RETURN

      END
* SUBROUTINE MXVSET                ALL SYSTEMS                88/12/01
* PORTABILITY : ALL SYSTEMS
* 88/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* A SCALAR IS SET TO ALL THE ELEMENTS OF A VECTOR.
*
* PARAMETERS :
*  II  N  VECTOR DIMENSION.
*  RI  A  INITIAL VALUE.
*  RO  X(N)  OUTPUT VECTOR SUCH THAT X(I)=A FOR ALL I.
*
      SUBROUTINE MXVSET(N,A,X)
*     .. Scalar Arguments ..
      DOUBLE PRECISION A
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
      DO 10 I = 1,N
          X(I) = A
   10 CONTINUE
      RETURN

      END

* SUBROUTINE PLLPB2             ALL SYSTEMS                   91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE INITIAL FEASIBLE POINT AND THE LINEAR PROGRAMMING
* SUBROUTINE.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEAR CONSTRAINTS.
*  RI  X(NF)  VECTOR OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RO  XO(NF)  SAVED VECTOR OF VARIABLES.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  CF(NF)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCYIONS.
*  RO  CFD(NF)  VECTOR CONTAINING INCREMENTS OF THE CONSTRAINT
*         FUNCTIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  II  ICA(NF)  VECTOR CONTAINING INDICES OF ACTIVE CONSTRAINTS.
*  RI  CL(NC)  VECTOR CONTAINING LOWER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CU(NC)  VECTOR CONTAINING UPPER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RI  CR(NF*(NF+1)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF THE
*         ORTHOGONAL PROJECTION.
*  RO  CZ(NF)  VECTOR OF LAGRANGE MULTIPLIERS.
*  RI  G(NF)  GRADIENT OF THE OBJECTIVE FUNCTION.
*  RO  GO(NF)  SAVED GRADIENT OF THE OBJECTIVE FUNCTION.
*  RI  S(NF)  DIRECTION VECTOR.
*  II  MFP  TYPE OF FEASIBLE POINT. MFP=1-ARBITRARY FEASIBLE POINT.
*         MFP=2-OPTIMUM FEASIBLE POINT. MFP=3-REPEATED SOLUTION.
*  II  KBF  SPECIFICATION OF SIMPLE BOUNDS. KBF=0-NO SIMPLE BOUNDS.
*         KBF=1-ONE SIDED SIMPLE BOUNDS. KBF=2=TWO SIDED SIMPLE BOUNDS.
*  II  KBC  SPECIFICATION OF LINEAR CONSTRAINTS. KBC=0-NO LINEAR
*         CONSTRAINTS. KBC=1-ONE SIDED LINEAR CONSTRAINTS. KBC=2=TWO
*         SIDED LINEAR CONSTRAINTS.
*  RI  ETA9  MAXIMUM FOR REAL NUMBERS.
*  RI  EPS7  TOLERANCE FOR LINEAR INDEPENDENCE OF CONSTRAINTS.
*  RI  EPS9  TOLERANCE FOR ACTIVITY OF CONSTRAINTS.
*  RO  UMAX  MAXIMUM ABSOLUTE VALUE OF A NEGATIVE LAGRANGE MULTIPLIER.
*  RO  GMAX  MAXIMUM ABSOLUTE VALUE OF A PARTIAL DERIVATIVE.
*  IO  N  DIMENSION OF THE MANIFOLD DEFINED BY ACTIVE CONSTRAINTS.
*  IO  ITERL  TYPE OF FEASIBLE POINT. ITERL=1-ARBITRARY FEASIBLE POINT.
*         ITERL=2-OPTIMUM FEASIBLE POINT. ITERL=-1 FEASIBLE POINT DOES
*         NOT EXISTS. ITERL=-2 OPTIMUM FEASIBLE POINT DOES NOT EXISTS.
*
* SUBPROGRAMS USED :
*  S   PLINIT  DETERMINATION OF INITIAL POINT SATISFYING SIMPLE BOUNDS.
*  S   PLMAXL  MAXIMUM STEPSIZE USING LINEAR CONSTRAINTS.
*  S   PLMAXS  MAXIMUM STEPSIZE USING SIMPLE BOUNDS.
*  S   PLMAXT  MAXIMUM STEPSIZE USING TRUST REGION BOUNDS.
*  S   PLNEWL  IDENTIFICATION OF ACTIVE LINEAR CONSTRAINTS.
*  S   PLNEWS  IDENTIFICATION OF ACTIVE SIMPLE BOUNDS.
*  S   PLNEWT  IDENTIFICATION OF ACTIVE TRUST REGION BOUNDS.
*  S   PLDIRL  NEW VALUES OF CONSTRAINT FUNCTIONS.
*  S   PLDIRS  NEW VALUES OF VARIABLES.
*  S   PLSETC  INITIAL VALUES OF CONSTRAINT FUNCTIONS.
*  S   PLSETG  DETERMINATION OF THE FIRST PHASE GRADIENT VECTOR.
*  S   PLGLAG  GRADIENT OF THE LAGRANGIAN FUNCTION IS DETERMINED.
*  S   PLSLAG  NEGATIVE PROJECTED GRADIENT IS DETERMINED.
*  S   PLTLAG  THE OPTIMUM LAGRANGE MULTIPLIER IS DETERMINED.
*  S   PLVLAG  AN AUXILIARY VECTOR IS DETERMINED.
*  S   PLADR0  CONSTRAINT ADDITION.
*  S   PLRMF0  CONSTRAINT DELETION.
*  S   MXDPRB  BACK SUBSTITUTION AFTER CHOLESKI DECOMPOSITION.
*  S   MXDSMI  DETERMINATION OF THE INITIAL UNIT DENSE SYMMETRIC
*         MATRIX.
*  S   MXVCOP  COPYING OF A VECTOR.
*  S   MXVDIF  DIFFERENCE OF TWO VECTORS.
*  S   MXVINA  ABSOLUTE VALUES OF ELEMENTS OF AN INTEGER VECTOR.
*  S   MXVINC  UPDATE OF AN INTEGER VECTOR.
*  S   MXVIND  CHANGE OF THE INTEGER VECTOR FOR CONSTRAINT ADDITION.
*  S   MXVINT  CHANGE OF THE INTEGER VECTOR FOR TRUST REGION BOUND
*         ADDITION.
*  S   MXVMUL  DIAGONAL PREMULTIPLICATION OF A VECTOR.
*  S   MXVNEG  COPYING OF A VECTOR WITH CHANGE OF THE SIGN.
*  S   MXVSET  INITIATION OF A VECTOR.
*
      SUBROUTINE PLLPB2(NF,NC,X,IX,XO,XL,XU,CF,CFD,IC,ICA,CL,CU,CG,CR,
     +                  CZ,G,GO,S,MFP,KBF,KBC,ETA9,EPS7,EPS9,UMAX,GMAX,
     +                  N,ITERL)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS7,EPS9,ETA9,GMAX,UMAX
      INTEGER ITERL,KBC,KBF,MFP,N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CFD(*),CG(*),CL(*),CR(*),CU(*),CZ(*),G(*),
     +                 GO(*),S(*),X(*),XL(*),XO(*),XU(*)
      INTEGER IC(*),ICA(*),IX(*)
C     ..
C     .. Scalars in Common ..
      INTEGER NADD,NDECF,NFG,NFH,NFV,NIT,NRED,NREM,NRES
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CON,DMAX,POM
      INTEGER I,IER,INEW,IOLD,IPOM,KC,KREM,MODE
C     ..
C     .. External Subroutines ..
      EXTERNAL MXDPRB,MXDSMI,MXVCOP,MXVINA,MXVIND,MXVNEG,PLADR0,PLDIRL,
     +         PLDIRS,PLINIT,PLMAXL,PLMAXS,PLNEWL,PLNEWS,PLRMF0,PLSETC,
     +         PLSETG,PLSLAG,PLTLAG,PLVLAG
C     ..
C     .. Common blocks ..
      COMMON /STAT/NDECF,NRES,NRED,NREM,NADD,NIT,NFV,NFG,NFH
C     ..
      CON = ETA9
*
*     INITIATION
*
      CALL MXVCOP(NF,X,XO)
      CALL MXVCOP(NF,G,GO)
      IPOM = 0
      NRED = 0
      KREM = 0
      ITERL = 1
      DMAX = 0.0D0
      IF (MFP.EQ.3) GO TO 40
      IF (KBF.GT.0) CALL MXVINA(NF,IX)
*
*     SHIFT OF VARIABLES FOR SATISFYING SIMPLE BOUNDS
*
      CALL PLINIT(NF,X,IX,XL,XU,EPS9,KBF,INEW,ITERL)
      IF (ITERL.LT.0) THEN
          GO TO 60

      END IF

      N = NF
      DO 10 I = 1,NF
          IF (KBF.GT.0 .AND. IX(I).LT.0) THEN
              N = N - 1
              ICA(NF-N) = -I
          END IF

   10 CONTINUE
      CALL MXDSMI(NF-N,CR)
      IF (NC.GT.0) THEN
*
*     ADDITION OF ACTIVE CONSTRAINTS AND INITIAL CHECK OF FEASIBILITY
*
          CALL MXVINA(NC,IC)
          IF (NF.GT.N) CALL PLSETC(NF,NC,X,XO,CF,IC,CG,S)
          DO 20 KC = 1,NC
              IF (IC(KC).NE.0) THEN
                  INEW = 0
                  CALL PLNEWL(KC,CF,IC,CL,CU,EPS9,INEW)
                  CALL PLADR0(NF,N,ICA,CG,CR,S,EPS7,GMAX,UMAX,INEW,NADD,
     +                        IER)
                  CALL MXVIND(IC,KC,IER)
                  IF (IC(KC).LT.-10) IPOM = 1
              END IF

   20     CONTINUE
      END IF

   30 IF (IPOM.EQ.1) THEN
*
*     CHECK OF FEASIBILITY AND UPDATE OF THE FIRST PHASE OBJECTIVE
*     FUNCTION
*
          CALL PLSETG(NF,NC,IC,CG,G,INEW)
          IF (INEW.EQ.0) IPOM = 0
      END IF

      IF (IPOM.EQ.0 .AND. ITERL.EQ.0) THEN
*
*     FEASIBILITY ACHIEVED
*
          ITERL = 1
          CALL MXVCOP(NF,GO,G)
          IF (MFP.EQ.1) GO TO 60
      END IF
*
*     LAGRANGE MULTIPLIERS DETERMINATION
*
   40 IF (NF.GT.N) THEN
          CALL PLVLAG(NF,N,NC,ICA,CG,CG,G,CZ)
          CALL MXDPRB(NF-N,CR,CZ,0)
          CALL PLTLAG(NF,N,NC,IX,IC,ICA,CZ,IC,EPS7,UMAX,IOLD)

      ELSE
          IOLD = 0
          UMAX = 0.0D0
      END IF
*
*     PROJECTED GRADIENT DETERMINATION
*
      IF (N.GT.0) THEN
          CALL MXVNEG(NF,G,S)
          CALL PLSLAG(NF,N,NC,ICA,CG,CZ,CG,S,EPS7,GMAX)

      ELSE
          GMAX = 0.0D0
      END IF

      MODE = 1 - IPOM
      INEW = 0
      IF (GMAX.EQ.0.0D0) THEN
*
*     OPTIMUM ON A LINEAR MANIFOLD OBTAINED
*
          IF (IOLD.EQ.0) THEN
              IF (IPOM.EQ.0) THEN
*
*     OPTIMAL SOLUTION ACHIEVED
*
                  ITERL = 2
                  GO TO 60

              ELSE
                  IPOM = 0
                  DO 50 KC = 1,NC
                      IF (IC(KC).LT.-10) THEN
                          INEW = 0
                          CALL PLNEWL(KC,CF,IC,CL,CU,EPS9,INEW)
                          IF (IC(KC).LT.-10) IPOM = 1
                      END IF

   50             CONTINUE
                  IF (IPOM.EQ.0) THEN
*
*     OPTIMAL SOLUTION ACHIEVED
*
                      CALL MXVCOP(NF,GO,G)
                      ITERL = 2
                      GO TO 60

                  ELSE
*
*     FEASIBLE SOLUTION DOES NOT EXIST
*
                      CALL MXVCOP(NF,GO,G)
                      ITERL = -1
                      GO TO 60

                  END IF

              END IF

          ELSE
*
*     CONSTRAINT DELETION
*
              CALL PLRMF0(NF,NC,IX,IC,ICA,CR,IC,S,N,IOLD,KREM,IER)
              DMAX = 0.0D0
              GO TO 40

          END IF

      ELSE
*
*     STEPSIZE SELECTION
*
          POM = CON
          CALL PLMAXL(NF,NC,CF,CFD,IC,CL,CU,CG,S,POM,KBC,KREM,INEW)
          CALL PLMAXS(NF,X,IX,XL,XU,S,POM,KBF,KREM,INEW)
          IF (INEW.EQ.0) THEN
              IF (IPOM.EQ.0) THEN
*
*     BOUNDED SOLUTION DOES NOT EXIST
*
                  ITERL = -2

              ELSE
*
*     FEASIBLE SOLUTION DOES NOT EXIST
*
                  ITERL = -3
              END IF

              GO TO 60

          ELSE
*
*     STEP REALIZATION
*
              CALL PLDIRS(NF,X,IX,S,POM,KBF)
              CALL PLDIRL(NC,CF,CFD,IC,POM,KBC)
*
*     CONSTRAINT ADDITION
*
              IF (INEW.GT.0) THEN
                  KC = INEW
                  INEW = 0
                  CALL PLNEWL(KC,CF,IC,CL,CU,EPS9,INEW)
                  CALL PLADR0(NF,N,ICA,CG,CR,S,EPS7,GMAX,UMAX,INEW,NADD,
     +                        IER)
                  CALL MXVIND(IC,KC,IER)

              ELSE IF (INEW+NF.GE.0) THEN
                  I = -INEW
                  INEW = 0
                  CALL PLNEWS(X,IX,XL,XU,EPS9,I,INEW)
                  CALL PLADR0(NF,N,ICA,CG,CR,S,EPS7,GMAX,UMAX,INEW,NADD,
     +                        IER)
                  CALL MXVIND(IX,I,IER)
              END IF

              DMAX = POM
              NRED = NRED + 1
              GO TO 30

          END IF

      END IF

   60 CONTINUE
      RETURN

      END
* SUBROUTINE PLDIRL               ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE NEW VALUES OF THE CONSTRAINT FUNCTIONS.
*
* PARAMETERS :
*  II  NC  NUMBER OF CONSTRAINTS.
*  RU  CF(NF)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCTIONS.
*  RI  CFD(NF)  VECTOR CONTAINING INCREMENTS OF THE CONSTRAINT
*         FUNCTIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  STEP  CURRENT STEPSIZE.
*  II  KBC  SPECIFICATION OF LINEAR CONSTRAINTS. KBC=0-NO LINEAR
*         CONSTRAINTS. KBC=1-ONE SIDED LINEAR CONSTRAINTS. KBC=2=TWO
*         SIDED LINEAR CONSTRAINTS.
*
      SUBROUTINE PLDIRL(NC,CF,CFD,IC,STEP,KBC)
C     .. Scalar Arguments ..
      DOUBLE PRECISION STEP
      INTEGER KBC,NC
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CFD(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      INTEGER KC
C     ..
      IF (KBC.GT.0) THEN
          DO 10 KC = 1,NC
              IF (IC(KC).GE.0 .AND. IC(KC).LE.10) THEN
                  CF(KC) = CF(KC) + STEP*CFD(KC)

              ELSE IF (IC(KC).LT.-10) THEN
                  CF(KC) = CF(KC) + STEP*CFD(KC)
              END IF

   10     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE PLDIRS               ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE NEW VECTOR OF VARIABLES.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  RU  X(NF)  VECTOR OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  S(NF)  DIRECTION VECTOR.
*  RI  STEP  CURRENT STEPSIZE.
*  II  KBF  SPECIFICATION OF SIMPLE BOUNDS. KBF=0-NO SIMPLE BOUNDS.
*         KBF=1-ONE SIDED SIMPLE BOUNDS. KBF=2=TWO SIDED SIMPLE BOUNDS.
*
      SUBROUTINE PLDIRS(NF,X,IX,S,STEP,KBF)
C     .. Scalar Arguments ..
      DOUBLE PRECISION STEP
      INTEGER KBF,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION S(*),X(*)
      INTEGER IX(*)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
      DO 10 I = 1,NF
          IF (KBF.LE.0) THEN
              X(I) = X(I) + STEP*S(I)

          ELSE IF (IX(I).GE.0 .AND. IX(I).LE.10) THEN
              X(I) = X(I) + STEP*S(I)

          ELSE IF (IX(I).LT.-10) THEN
              X(I) = X(I) + STEP*S(I)
          END IF

   10 CONTINUE
      RETURN

      END
* SUBROUTINE PLINIT             ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE INITIAL POINT WHICH SATISFIES SIMPLE BOUNDS.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  RU  X(NF)  VECTOR OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  EPS9  TOLERANCE FOR ACTIVE CONSTRAINTS.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*  IO  IND  INDICATOR. IF IND.NE.0 THEN TRUST REGION BOUNDS CANNOT
*         BE SATISFIED.
*
* SUBPROGRAMS USED :
*  S   PLNEWS  TEST ON ACTIVITY OF A GIVEN SIMPLE BOUND.
*
      SUBROUTINE PLINIT(NF,X,IX,XL,XU,EPS9,KBF,INEW,IND)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9
      INTEGER IND,INEW,KBF,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION X(*),XL(*),XU(*)
      INTEGER IX(*)
C     ..
C     .. Local Scalars ..
      INTEGER I
C     ..
C     .. External Subroutines ..
      EXTERNAL PLNEWS
C     ..
      IND = 0
      IF (KBF.GT.0) THEN
          DO 10 I = 1,NF
              CALL PLNEWS(X,IX,XL,XU,EPS9,I,INEW)
              IF (IX(I).LT.5) THEN

              ELSE IF (IX(I).EQ.5) THEN
                  IX(I) = -5

              ELSE IF (IX(I).EQ.11 .OR. IX(I).EQ.13) THEN
                  X(I) = XL(I)
                  IX(I) = 10 - IX(I)

              ELSE IF (IX(I).EQ.12 .OR. IX(I).EQ.14) THEN
                  X(I) = XU(I)
                  IX(I) = 10 - IX(I)
              END IF

   10     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE PLMAXL               ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE MAXIMUM STEPSIZE USING LINEAR CONSTRAINTS.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF CURRENT LINEAR CONSTRAINTS.
*  RI  CF(NF)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCYIONS.
*  RO  CFD(NF)  VECTOR CONTAINING INCREMENTS OF THE CONSTRAINT
*         FUNCTIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CL(NC)  VECTOR CONTAINING LOWER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CU(NC)  VECTOR CONTAINING UPPER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RI  S(NF)  DIRECTION VECTOR.
*  RO  STEP  MAXIMUM STEPSIZE.
*  II  KBC  SPECIFICATION OF LINEAR CONSTRAINTS. KBC=0-NO LINEAR
*         CONSTRAINTS. KBC=1-ONE SIDED LINEAR CONSTRAINTS. KBC=2=TWO
*         SIDED LINEAR CONSTRAINTS.
*  II  KREM  INDICATION OF LINEARLY DEPENDENT GRADIENTS.
*  IO  INEW  INDEX OF THE NEW ACTIVE FUNCTION.
*
* SUBPROGRAMS USED :
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLMAXL(NF,NC,CF,CFD,IC,CL,CU,CG,S,STEP,KBC,KREM,INEW)
C     .. Scalar Arguments ..
      DOUBLE PRECISION STEP
      INTEGER INEW,KBC,KREM,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CFD(*),CG(*),CL(*),CU(*),S(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER JCG,KC
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
      IF (KBC.GT.0) THEN
          JCG = 1
          DO 10 KC = 1,NC
              IF (KREM.GT.0 .AND. IC(KC).GT.10) IC(KC) = IC(KC) - 10
              IF (IC(KC).GT.0 .AND. IC(KC).LE.10) THEN
                  TEMP = MXVDOT(NF,CG(JCG),S)
                  CFD(KC) = TEMP
                  IF (TEMP.LT.0.0D0) THEN
                      IF (IC(KC).EQ.1 .OR. IC(KC).GE.3) THEN
                          TEMP = (CL(KC)-CF(KC))/TEMP
                          IF (TEMP.LE.STEP) THEN
                              INEW = KC
                              STEP = TEMP
                          END IF

                      END IF

                  ELSE IF (TEMP.GT.0.0D0) THEN
                      IF (IC(KC).EQ.2 .OR. IC(KC).GE.3) THEN
                          TEMP = (CU(KC)-CF(KC))/TEMP
                          IF (TEMP.LE.STEP) THEN
                              INEW = KC
                              STEP = TEMP
                          END IF

                      END IF

                  END IF

              ELSE IF (IC(KC).LT.-10) THEN
                  TEMP = MXVDOT(NF,CG(JCG),S)
                  CFD(KC) = TEMP
                  IF (TEMP.GT.0.0D0) THEN
                      IF (IC(KC).EQ.-11 .OR. IC(KC).EQ.-13 .OR.
     +                    IC(KC).EQ.-15) THEN
                          TEMP = (CL(KC)-CF(KC))/TEMP
                          IF (TEMP.LE.STEP) THEN
                              INEW = KC
                              STEP = TEMP
                          END IF

                      END IF

                  ELSE IF (TEMP.LT.0.0D0) THEN
                      IF (IC(KC).EQ.-12 .OR. IC(KC).EQ.-14 .OR.
     +                    IC(KC).EQ.-16) THEN
                          TEMP = (CU(KC)-CF(KC))/TEMP
                          IF (TEMP.LE.STEP) THEN
                              INEW = KC
                              STEP = TEMP
                          END IF

                      END IF

                  END IF

              END IF

              JCG = JCG + NF
   10     CONTINUE
      END IF

      RETURN

      END
* SUBROUTINE PLMAXS               ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF THE MAXIMUM STEPSIZE USING THE SIMPLE BOUNDS
* FOR VARIABLES.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  RI  X(NF)  VECTOR OF VARIABLES.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  S(NF)  DIRECTION VECTOR.
*  RO  STEP  MAXIMUM STEPSIZE.
*  II  KBF  SPECIFICATION OF SIMPLE BOUNDS. KBF=0-NO SIMPLE BOUNDS.
*         KBF=1-ONE SIDED SIMPLE BOUNDS. KBF=2=TWO SIDED SIMPLE BOUNDS.
*  IO  KREM  INDICATION OF LINEARLY DEPENDENT GRADIENTS.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*
      SUBROUTINE PLMAXS(NF,X,IX,XL,XU,S,STEP,KBF,KREM,INEW)
C     .. Scalar Arguments ..
      DOUBLE PRECISION STEP
      INTEGER INEW,KBF,KREM,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION S(*),X(*),XL(*),XU(*)
      INTEGER IX(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER I
C     ..
      IF (KBF.GT.0) THEN
          DO 10 I = 1,NF
              IF (KREM.GT.0 .AND. IX(I).GT.10) IX(I) = IX(I) - 10
              IF (IX(I).GT.0 .AND. IX(I).LE.10) THEN
                  IF (S(I).LT.0.0D0) THEN
                      IF (IX(I).EQ.1 .OR. IX(I).GE.3) THEN
                          TEMP = (XL(I)-X(I))/S(I)
                          IF (TEMP.LE.STEP) THEN
                              INEW = -I
                              STEP = TEMP
                          END IF

                      END IF

                  ELSE IF (S(I).GT.0.0D0) THEN
                      IF (IX(I).EQ.2 .OR. IX(I).GE.3) THEN
                          TEMP = (XU(I)-X(I))/S(I)
                          IF (TEMP.LE.STEP) THEN
                              INEW = -I
                              STEP = TEMP
                          END IF

                      END IF

                  END IF

              END IF

   10     CONTINUE
      END IF

      KREM = 0
      RETURN

      END
* SUBROUTINE PLNEWL             ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* TEST ON ACTIVITY OF A GIVEN LINEAR CONSTRAINT.
*
* PARAMETERS :
*  II  KC  INDEX OF A GIVEN CONSTRAINT.
*  RI  CF(NC)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCTIONS.
*  IU  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CL(NC)  VECTOR CONTAINING LOWER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  CU(NC)  VECTOR CONTAINING UPPER BOUNDS FOR CONSTRAINT FUNCTIONS.
*  RI  EPS9  TOLERANCE FOR ACTIVE CONSTRAINTS.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*
      SUBROUTINE PLNEWL(KC,CF,IC,CL,CU,EPS9,INEW)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9
      INTEGER INEW,KC
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CL(*),CU(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX
C     ..
      IF (IC(KC).LT.-10) IC(KC) = -IC(KC) - 10
      IF (IC(KC).LE.0) THEN

      ELSE IF (IC(KC).EQ.1) THEN
          TEMP = EPS9*MAX(ABS(CL(KC)),1.0D0)
          IF (CF(KC).GT.CL(KC)+TEMP) THEN

          ELSE IF (CF(KC).GE.CL(KC)-TEMP) THEN
              IC(KC) = 11
              INEW = KC

          ELSE
              IC(KC) = -11
          END IF

      ELSE IF (IC(KC).EQ.2) THEN
          TEMP = EPS9*MAX(ABS(CU(KC)),1.0D0)
          IF (CF(KC).LT.CU(KC)-TEMP) THEN

          ELSE IF (CF(KC).LE.CU(KC)+TEMP) THEN
              IC(KC) = 12
              INEW = KC

          ELSE
              IC(KC) = -12
          END IF

      ELSE IF (IC(KC).EQ.3 .OR. IC(KC).EQ.4) THEN
          TEMP = EPS9*MAX(ABS(CL(KC)),1.0D0)
          IF (CF(KC).GT.CL(KC)+TEMP) THEN
              TEMP = EPS9*MAX(ABS(CU(KC)),1.0D0)
              IF (CF(KC).LT.CU(KC)-TEMP) THEN

              ELSE IF (CF(KC).LE.CU(KC)+TEMP) THEN
                  IC(KC) = 14
                  INEW = KC

              ELSE
                  IC(KC) = -14
              END IF

          ELSE IF (CF(KC).GE.CL(KC)-TEMP) THEN
              IC(KC) = 13
              INEW = KC

          ELSE
              IC(KC) = -13
          END IF

      ELSE IF (IC(KC).EQ.5 .OR. IC(KC).EQ.6) THEN
          TEMP = EPS9*MAX(ABS(CL(KC)),1.0D0)
          IF (CF(KC).GT.CL(KC)+TEMP) THEN
              TEMP = EPS9*MAX(ABS(CU(KC)),1.0D0)
              IF (CF(KC).LT.CU(KC)-TEMP) THEN

              ELSE IF (CF(KC).LE.CU(KC)+TEMP) THEN
                  IC(KC) = 16
                  INEW = KC

              ELSE
                  IC(KC) = -16
              END IF

          ELSE IF (CF(KC).GE.CL(KC)-TEMP) THEN
              IC(KC) = 15
              INEW = KC

          ELSE
              IC(KC) = -15
          END IF

      END IF

      RETURN

      END
* SUBROUTINE PLNEWS             ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* TEST ON ACTIVITY OF A GIVEN SIMPLE BOUND.
*
* PARAMETERS :
*  RI  X(NF)  VECTOR OF VARIABLES.
*  IU  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  RI  XL(NF)  VECTOR CONTAINING LOWER BOUNDS FOR VARIABLES.
*  RI  XU(NF)  VECTOR CONTAINING UPPER BOUNDS FOR VARIABLES.
*  RI  EPS9  TOLERANCE FOR ACTIVE CONSTRAINTS.
*  II  I  INDEX OF TESTED SIMPLE BOUND.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*
      SUBROUTINE PLNEWS(X,IX,XL,XU,EPS9,I,INEW)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS9
      INTEGER I,INEW
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION X(*),XL(*),XU(*)
      INTEGER IX(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX
C     ..
      TEMP = 1.0D0
      IF (IX(I).LE.0) THEN

      ELSE IF (IX(I).EQ.1) THEN
          IF (X(I).LE.XL(I)+EPS9*MAX(ABS(XL(I)),TEMP)) THEN
              IX(I) = 11
              INEW = -I
          END IF

      ELSE IF (IX(I).EQ.2) THEN
          IF (X(I).GE.XU(I)-EPS9*MAX(ABS(XU(I)),TEMP)) THEN
              IX(I) = 12
              INEW = -I
          END IF

      ELSE IF (IX(I).EQ.3 .OR. IX(I).EQ.4) THEN
          IF (X(I).LE.XL(I)+EPS9*MAX(ABS(XL(I)),TEMP)) THEN
              IX(I) = 13
              INEW = -I
          END IF

          IF (X(I).GE.XU(I)-EPS9*MAX(ABS(XU(I)),TEMP)) THEN
              IX(I) = 14
              INEW = -I
          END IF

      END IF

      RETURN

      END

* SUBROUTINE PLSETC             ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DETERMINATION OF INITIAL VALUES OF THE CONSTRAINT FUNCTIONS.
*
* PARAMETERS :
*  II  NF  NUMBER OF VARIABLES.
*  II  NC  NUMBER OF CURRENT LINEAR CONSTRAINTS.
*  RI  X(NF)  VECTOR OF VARIABLES.
*  RI  XO(NF)  SAVED VECTOR OF VARIABLES.
*  RU  CF(NF)  VECTOR CONTAINING VALUES OF THE CONSTRAINT FUNCYIONS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CG(NF*MCL)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RA  S(NF)  AUXILIARY VECTOR.
*
* SUBPROGRAMS USED :
*  S   MXVDIF  DIFFERENCE OF TWO VECTORS.
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*  S   MXVMUL  DIAGONAL PREMULTIPLICATION OF A VECTOR.
*
      SUBROUTINE PLSETC(NF,NC,X,XO,CF,IC,CG,S)
C     .. Scalar Arguments ..
      INTEGER NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CF(*),CG(*),S(*),X(*),XO(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      INTEGER JCG,KC
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
C     .. External Subroutines ..
      EXTERNAL MXVDIF
C     ..
      CALL MXVDIF(NF,X,XO,S)
      JCG = 0
      DO 10 KC = 1,NC
          IF (IC(KC).NE.0) CF(KC) = CF(KC) + MXVDOT(NF,S,CG(JCG+1))
          JCG = JCG + NF
   10 CONTINUE
      RETURN
      END

* SUBROUTINE PLSETG             ALL SYSTEMS                   97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* GRADIENT DETERMINATION IN THE FIRST PHASE OF LP SUBROUTINE.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  NC  NUMBER OF CONSTRAINTS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RO  G(NF)  GRADIENT OF THE OBJECTIVE FUNCTION.
*  IO  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*
* SUBPROGRAMS USED :
*  S   MXVDIR  VECTOR AUGMENTED BY THE SCALED VECTOR.
*  S   MXVSET  INITIATION OF A VECTOR.
*
      SUBROUTINE PLSETG(NF,NC,IC,CG,G,INEW)
C     .. Scalar Arguments ..
      INTEGER INEW,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CG(*),G(*)
      INTEGER IC(*)
C     ..
C     .. Local Scalars ..
      INTEGER KC
C     ..
C     .. External Subroutines ..
      EXTERNAL MXVDIR,MXVSET
C     ..
      CALL MXVSET(NF,0.0D0,G)
      INEW = 0
      DO 10 KC = 1,NC
          IF (IC(KC).GE.-10) THEN

          ELSE IF (IC(KC).EQ.-11 .OR. IC(KC).EQ.-13 .OR.
     +             IC(KC).EQ.-15) THEN
              CALL MXVDIR(NF,-1.0D0,CG((KC-1)*NF+1),G,G)
              INEW = 1

          ELSE IF (IC(KC).EQ.-12 .OR. IC(KC).EQ.-14 .OR.
     +             IC(KC).EQ.-16) THEN
              CALL MXVDIR(NF,1.0D0,CG((KC-1)*NF+1),G,G)
              INEW = 1
          END IF

   10 CONTINUE
      RETURN

      END
      
* SUBROUTINE PLSLAG               ALL SYSTEMS                97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* NEGATIVE PROJECTED GRADIENT IS DETERMINED USING LAGRANGE MULTIPLIERS.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEARIZED CONSTRAINTS.
*  II  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RI  AG(NF*NC)  MATRIX WHOSE COLUMNS ARE GRADIENTS OF THE LINEAR
*         APPROXIMATED FUNCTIONS.
*  RO  AZ(NF+1)  VECTOR OF LAGRANGE MULTIPLIERS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RO  S(NF)  NEGATIVE PROJECTED GRADIENT OF THE QUADRATIC FUNCTION.
*  RI  EPS7  TOLERANCE FOR LINEAR AND QUADRATIC PROGRAMMING.
*  RO  GMAX  NORM OF THE TRANSFORMED GRADIENT.
*
* SUBPROGRAMS USED :
*  S   UXVDIR  VECTOR AUGMENTED BY THE SCALED VECTOR.
*  RF  UXVMAX  L-INFINITY NORM OF A VECTOR.
*
      SUBROUTINE PLSLAG(NF,N,NC,IAA,AG,AZ,CG,S,EPS7,GMAX)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS7,GMAX
      INTEGER N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AG(*),AZ(*),CG(*),S(*)
      INTEGER IAA(*)
C     ..
C     .. Local Scalars ..
      INTEGER J,L,NAA
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVMAX
      EXTERNAL MXVMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL MXVDIR
C     ..
      NAA = NF - N
      DO 10 J = 1,NAA
          L = IAA(J)
          IF (L.GT.NC) THEN
              L = L - NC
              CALL MXVDIR(NF,AZ(J),AG((L-1)*NF+1),S,S)

          ELSE IF (L.GT.0) THEN
              CALL MXVDIR(NF,AZ(J),CG((L-1)*NF+1),S,S)

          ELSE
              L = -L
              S(L) = S(L) + AZ(J)
          END IF

   10 CONTINUE
      GMAX = MXVMAX(NF,S)
      IF (GMAX.LE.EPS7) GMAX = 0.0D0
      RETURN

      END
* SUBROUTINE PLTLAG               ALL SYSTEMS                97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* MAXIMUM ABSOLUTE VALUE OF THE NEGATIVE LAGRANGE MULTIPLIER IS
* COMPUTED.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEARIZED CONSTRAINTS.
*  II  IX(NF)  VECTOR CONTAINING TYPES OF BOUNDS.
*  II  IA(NA)  VECTOR CONTAINING TYPES OF DEVIATIONS.
*  II  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RI  AZ(NF+1)  VECTOR OF LAGRANGE MULTIPLIERS.
*  II  IC(NC)  VECTOR CONTAINING TYPES OF CONSTRAINTS.
*  RI  EPS7  TOLERANCE FOR LINEAR AND QUADRATIC PROGRAMMING.
*  RO  UMAX  MAXIMUM ABSOLUTE VALUE OF THE NEGATIVE LAGRANGE MULTIPLIER.
*  IO  IOLD  INDEX OF THE REMOVED CONSTRAINT.
*
      SUBROUTINE PLTLAG(NF,N,NC,IX,IA,IAA,AZ,IC,EPS7,UMAX,IOLD)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS7,UMAX
      INTEGER IOLD,N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AZ(*)
      INTEGER IA(*),IAA(*),IC(*),IX(*)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER J,K,L,NAA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
      IOLD = 0
      UMAX = 0.0D0
      NAA = NF - N
      DO 10 J = 1,NAA
          TEMP = AZ(J)
          L = IAA(J)
          IF (L.GT.NC) THEN
              L = L - NC
              K = IA(L)

          ELSE IF (L.GT.0) THEN
              K = IC(L)

          ELSE
              L = -L
              K = IX(L)
          END IF

          IF (K.LE.-5) THEN

          ELSE IF ((K.EQ.-1.OR.K.EQ.-3) .AND. UMAX+TEMP.GE.0.0D0) THEN

          ELSE IF ((K.EQ.-2.OR.K.EQ.-4) .AND. UMAX-TEMP.GE.0.0D0) THEN

          ELSE
              IOLD = J
              UMAX = ABS(TEMP)
          END IF

   10 CONTINUE
      IF (UMAX.LE.EPS7) IOLD = 0
      RETURN

      END
* SUBROUTINE PLVLAG               ALL SYSTEMS                97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* GRADIENT OF THE OBJECTIVE FUNCTION IS PREMULTIPLIED BY TRANSPOSE
* OF THE MATRIX WHOSE COLUMNS ARE NORMALS OF CURRENT ACTIVE CONSTRAINTS
* AND GRADIENTS OF CURRENT ACTIVE FUNCTIONS.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  II  N  ACTUAL NUMBER OF VARIABLES.
*  II  NC  NUMBER OF LINEARIZED CONSTRAINTS.
*  II  IAA(NF+1)  VECTOR CONTAINING INDICES OF ACTIVE FUNCTIONS.
*  RI  AG(NF*NA)  VECTOR CONTAINING SCALING PARAMETERS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RI  G(NF)  GRADIENT OF THE OBJECTIVE FUNCTION.
*  RO  GN(NF+1)  OUTPUT VECTOR.
*
* SUBPROGRAMS USED :
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLVLAG(NF,N,NC,IAA,AG,CG,G,GN)
C     .. Scalar Arguments ..
      INTEGER N,NC,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION AG(*),CG(*),G(*),GN(*)
      INTEGER IAA(*)
C     ..
C     .. Local Scalars ..
      INTEGER J,L,NAA
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
      NAA = NF - N
      DO 10 J = 1,NAA
          L = IAA(J)
          IF (L.GT.NC) THEN
              L = L - NC
              GN(J) = MXVDOT(NF,AG((L-1)*NF+1),G)

          ELSE IF (L.GT.0) THEN
              GN(J) = MXVDOT(NF,CG((L-1)*NF+1),G)

          ELSE
              L = -L
              GN(J) = G(L)
          END IF

   10 CONTINUE
      RETURN

      END
* SUBROUTINE PLADR0               ALL SYSTEMS                97/12/01
* PORTABILITY : ALL SYSTEMS
* 97/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* TRIANGULAR DECOMPOSITION OF KERNEL OF THE ORTHOGONAL PROJECTION
* IS UPDATED AFTER CONSTRAINT ADDITION.
*
* PARAMETERS :
*  II  NF  DECLARED NUMBER OF VARIABLES.
*  IU  N  ACTUAL NUMBER OF VARIABLES.
*  IU  ICA(NF)  VECTOR CONTAINING INDICES OF ACTIVE CONSTRAINTS.
*  RI  CG(NF*NC)  MATRIX WHOSE COLUMNS ARE NORMALS OF THE LINEAR
*         CONSTRAINTS.
*  RU  CR(NF*(NF+1)/2)  TRIANGULAR DECOMPOSITION OF KERNEL OF THE
*         ORTHOGONAL PROJECTION.
*  RA  S(NF)  AUXILIARY VECTOR.
*  RI  EPS7  TOLERANCE FOR LINEAR INDEPENDENCE OF CONSTRAINTS.
*  RO  GMAX  MAXIMUM ABSOLUTE VALUE OF A PARTIAL DERIVATIVE.
*  RO  UMAX  MAXIMUM ABSOLUTE VALUE OF A NEGATIVE LAGRANGE MULTIPLIER.
*  II  INEW  INDEX OF THE NEW ACTIVE CONSTRAINT.
*  IU  NADD  NUMBER OF CONSTRAINT ADDITIONS.
*  IO  IER  ERROR INDICATOR.
*
* SUBPROGRAMS USED :
*  S   MXSPRB  SPARSE BACK SUBSTITUTION.
*  S   MXVCOP  COPYING OF A VECTOR.
*  RF  MXVDOT  DOT PRODUCT OF TWO VECTORS.
*
      SUBROUTINE PLADR0(NF,N,ICA,CG,CR,S,EPS7,GMAX,UMAX,INEW,NADD,IER)
C     .. Scalar Arguments ..
      DOUBLE PRECISION EPS7,GMAX,UMAX
      INTEGER IER,INEW,N,NADD,NF
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION CG(*),CR(*),S(*)
      INTEGER ICA(*)
C     ..
C     .. Local Scalars ..
      INTEGER I,J,K,L,NCA,NCR
C     ..
C     .. External Functions ..
      DOUBLE PRECISION MXVDOT
      EXTERNAL MXVDOT
C     ..
C     .. External Subroutines ..
      EXTERNAL MXDPRB,MXVCOP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     ..
      IER = 0
      IF (N.LE.0) IER = 2
      IF (INEW.EQ.0) IER = 3
      IF (IER.NE.0) RETURN
      NCA = NF - N
      NCR = NCA* (NCA+1)/2
      IF (INEW.GT.0) THEN
          CALL MXVCOP(NF,CG((INEW-1)*NF+1),S)
          GMAX = MXVDOT(NF,CG((INEW-1)*NF+1),S)
          DO 10 J = 1,NCA
              L = ICA(J)
              IF (L.GT.0) THEN
                  CR(NCR+J) = MXVDOT(NF,CG((L-1)*NF+1),S)

              ELSE
                  I = -L
                  CR(NCR+J) = S(I)
              END IF

   10     CONTINUE

      ELSE
          K = -INEW
          GMAX = 1.0D0
          DO 20 J = 1,NCA
              L = ICA(J)
              IF (L.GT.0) THEN
                  CR(NCR+J) = CG((L-1)*NF+K)*GMAX

              ELSE
                  CR(NCR+J) = 0.0D0
              END IF

   20     CONTINUE
      END IF

      IF (NCA.EQ.0) THEN
          UMAX = GMAX

      ELSE
          CALL MXDPRB(NCA,CR,CR(NCR+1),1)
          UMAX = GMAX - MXVDOT(NCA,CR(NCR+1),CR(NCR+1))
      END IF

      IF (UMAX.LE.EPS7*GMAX) THEN
          IER = 1
          RETURN

      ELSE
          N = N - 1
          NCA = NCA + 1
          NCR = NCR + NCA
          ICA(NCA) = INEW
          CR(NCR) = SQRT(UMAX)
          NADD = NADD + 1
      END IF

      RETURN

      END
* SUBROUTINE MXDSMI                ALL SYSTEMS                88/12/01
* PORTABILITY : ALL SYSTEMS
* 88/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* DENSE SYMMETRIC MATRIX A IS SET TO THE UNIT MATRIX WITH THE SAME
* ORDER.
*
* PARAMETERS :
*  II  N  ORDER OF THE MATRIX A.
*  RO  A(N*(N+1)/2)  DENSE SYMMETRIC MATRIX STORED IN THE PACKED FORM
*         WHICH IS SET TO THE UNIT MATRIX (I.E. A:=I).
*
      SUBROUTINE MXDSMI(N,A)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,M
*     ..
      M = N* (N+1)/2
      DO 10 I = 1,M
          A(I) = 0.0D0
   10 CONTINUE
      M = 0
      DO 20 I = 1,N
          M = M + I
          A(M) = 1.0D0
   20 CONTINUE
      RETURN

      END
* SUBROUTINE MXVDIF                ALL SYSTEMS                88/12/01
* PORTABILITY : ALL SYSTEMS
* 88/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* VECTOR DIFFERENCE.
*
* PARAMETERS :
*  RI  X(N)  INPUT VECTOR.
*  RI  Y(N)  INPUT VECTOR.
*  RO  Z(N)  OUTPUT VECTOR WHERE Z:= X - Y.
*
      SUBROUTINE MXVDIF(N,X,Y,Z)
*     .. Scalar Arguments ..
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION X(*),Y(*),Z(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I
*     ..
      DO 10 I = 1,N
          Z(I) = X(I) - Y(I)
   10 CONTINUE
      RETURN

      END
* SUBROUTINE MXVIND               ALL SYSTEMS                91/12/01
* PORTABILITY : ALL SYSTEMS
* 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
* CHANGE OF THE INTEGER VECTOR ELEMENT FOR THE CONSTRAINT ADDITION.
*
* PARAMETERS :
*  IU  IX(N)  INTEGER VECTOR.
*  II  I  INDEX OF THE CHANGED ELEMENT.
*  II JOB  CHANGE SPECIFICATION. IS JOB.EQ.0 THEN IX(I)=10-IX(I).
*
      SUBROUTINE MXVIND(IX,I,JOB)
      INTEGER IX(*),I,JOB
      IF (JOB.EQ.0) IX(I)=10-IX(I)
      RETURN
      END
