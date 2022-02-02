!*************************************************************************
!*                                                                       *
!*     BEM-DC - Bundle Enrichement Method for Nonsmooth DC optimization  *
!*                                                                       *
!*     by Napsu Karmitsa, Adil Bagirov and Sona Taheri                   *
!*     (last modified 02.02.2022).                                       *
!*     The code is partly based on the code snippets by Kaisa Joki       *
!*     and it uses a quadratic solver by Ladislav Luksan.                *
!*                                                                       *
!*     The software is free for academic teaching and research           *
!*     purposes but we ask you to refer the appropriate references       *
!*     given below, if you use it.                                       *
!*                                                                       *
!*************************************************************************
!*
!*     Codes included:
!*
!*     bem.f03               - Mainprogram (this file).
!*     initbem.f03           - Initialization of parameters for BEM-DC.
!*     functions.f03         - Computation of DC components f_1 and f_2, 
!*                             and the starting point supplied by user.   
!*                             Includes now some test examples.
!*     parameters.f03        - Parameters and constants.
!*     plqdf1.f              - Quadratic solver by L. Luksan.
!*
!*     Makefile              - makefile.
!*
!*     Calling sequence:
!*
!*       ./bem
!*
!*     Give your functions and starting point in functions.f03.
!*     If you want to tune other parameters, modify initbem.f03.
!* 
!*     As default the results are written in
!*        
!*     results1.txt          - value of the objective, numbers of evaluations etc. 
!*     results2.txt          - optimal point.

!*****************************************************************************************
!*
!*     * PROGRAM bem *
!*
!*     Main program for BEM-DC.
!*
!*****************************************************************************************

PROGRAM bem

    USE r_precision, ONLY : prec        ! Precision for reals.
    USE param, ONLY : zero, two, large  ! Parameters.         
    
    USE initbem, ONLY : &      ! Initialization of parameters.
        outfile1, &            ! Results with objective value, numbers of evaluations etc.
        outfile2, &            ! The optimal point.
        tol, &                 ! Stopping tolerance.
        pp, &                  ! proximity parameter.
        ppmin, &               ! proximity threshold
        sdp, sdp1, &           ! sufficient decrease parameter. 
        n, &                   ! Size of the problem.
        x, &                   ! Vector of variables. 
        tt, &                  ! step size.   
        rt1, &                 ! step size reduction. 
        teta,&                 ! null step parameter.  
        mit, mfe, &            ! Maximum number of iterations and function evaluations.
        size_b, &              ! The maximum size of the bundle.
        size_tb, &             ! The maximum size of the temporary bundle. 
        ls, &                  ! Indicator for line search:
                               !   0 - no line search;
                               !   1 - simple line search in serious steps.   
        init_par               ! User specified initialization of parameters.
    USE initbem, ONLY : &      ! Problem formulation. Delete this if you use your own problem.
        pnf1,pnf2,sp
    USE functions, ONLY : &
        init_x, &              ! Initialization of x (supplied by user).
        myf1,myf2, &           ! Comutation of the component functions f1 and f2 (supplied by user).
        myg1,myg2              ! Computation of the subgradient of functions f1 and f2  (supplied by user).
    
    IMPLICIT NONE

    TYPE bundle_element
        INTEGER :: jk                              ! bundle index
        REAL(KIND=prec), DIMENSION(n) :: x_bundle  ! bundle point
        REAL(KIND=prec), DIMENSION(n) :: g1_bundle ! bundle subgradient 1
        REAL(KIND=prec), DIMENSION(n) :: g2_bundle ! bundle subgradient 2
        REAL(KIND=prec) :: f1_bundle               ! function value at bundle point
        REAL(KIND=prec) :: f2_bundle               ! function value at bundle point
        REAL(KIND=prec) :: alpha1_bundle           ! linearization error 1
        REAL(KIND=prec) :: alpha2_bundle           ! linearization error 2
        REAL(KIND=prec) :: g2norm_bundle           ! norm of g2
    END TYPE bundle_element
    
    TYPE tbundle_element                            ! Temporary bundle used in inner iterations. 
        INTEGER :: tjk                              ! bundle index
        REAL(KIND=prec), DIMENSION(n) :: tx_bundle  ! bundle point
        REAL(KIND=prec), DIMENSION(n) :: tg1_bundle ! bundle subgradient 1
        REAL(KIND=prec) :: tf1_bundle               ! function value at bundle point
        REAL(KIND=prec) :: talpha1_bundle           ! linearization error 1
    END TYPE tbundle_element
    
    TYPE(bundle_element), DIMENSION(size_b) :: bundle
    TYPE(tbundle_element), DIMENSION(size_tb) :: tbundle
    
    REAL(KIND=prec), DIMENSION(n) :: & 
        g1,g2, &                                  ! The subgradients of f1 and f2.
        d,d_old, &                                ! The search direction. d_old, just for testing
        y                                         ! Auxiliary point.
    REAL(KIND=prec) :: &
        f1,f2, &                                  ! The values of f1 and f2 at x.
        f11,f22,f10,f20, &                        ! The values of f11 and f22 at x+d for pp update 
        v, &                                      ! The value for search direction problem.
        f_init, &                                 ! Value of the objective at initial point.
        f_current,f_current1,f1_c,f2_c, &         ! Values of the objective at current point.
        fy, fd, &                                 ! Value of objective at auxiliary point.
        alpha1, alpha2, &                         ! Linearization errors.
        atol, &                                   ! Tolerance for linearization error.
        g1norm, &                                 ! The norm of g1 in the current iteration point.
        gmaxnorm, &                               ! Maximum norm of subgradients in B2.
        dnorm, &                                  ! The norm of the search direction.
        sdp_init, ppinit, &                       ! Auxiliary variables.
        tt1                                       !
    
    REAL :: &
        start_time, &                             ! Start and finish CPU time.
        finish_time  
        
    INTEGER :: &
        nit,niit, &          ! Number of outer and inner iterations,
        nfe,nge1,nge2,nf0,&  ! Number of function and subgradient evaluations. 
        nb,nb1,nb2, &        ! Current sizes of the bundles.
        ntb, &               ! Current size of the temporary bundle.
        iglob, &             ! Index of bundle that gives the global minimum.
        bfull, &             ! Index for updating the bundle:
                             !  0 - bundle is not yet full;
                             !  1 - bundle is full.
        iterm, &             ! Cause of termination
                             !  1 - desired accuracy;
                             !  2 - nit > mit;
                             !  3 - nfe > mfe;
                             !  4 - No descent direction found.
                             ! -3 - failure in function computing, supplied by user;
        i, &                 ! Index for loops.
        nff                  ! 
                             
    ! Intrinsic Functions
    INTRINSIC ABS
    
    PRINT*,'BEM-DC     - Bundle Enrichement Method for Nonsmooth DC optimization'
        
    OPEN(42,file=outfile1)
    OPEN(41,file=outfile2)
        
    ! Initialization
    atol = 0.00000001_prec   ! Tolerance for linearization error.
    sdp_init = sdp           ! Initial sufficient decrease parameter.
    bfull = 0                ! The bundle is not yet full.
    iterm = 0
    nit  = 0
    niit = 0
    nff = 0
    nf0 = 0
    CALL init_par()          ! User specified parameters.    
    CALL init_x()            ! x.
    
    CALL cpu_time(start_time)
    
    WRITE(42,*) 'BEM-DC     - Bundle Enrichement Method for Nonsmooth DC optimization'
    WRITE(42,*)
    WRITE(42,*) 'Problem formulation: f1 = ',pnf1 ,'f2 =',pnf2 ,'x1 = ',sp 
    WRITE(42,*)
     
    WRITE(41,*) 'BEM-DC     - Bundle Enrichement Method for Nonsmooth DC optimization'
    WRITE(41,*)
    
    !***********************************************************************    
    ! Step 0. Compute function values and subgradients
    !***********************************************************************    
    CALL myf1(n,x,f1,iterm)
    CALL myf2(n,x,f2,iterm)
    nfe  = 1
    f_current = f1-f2
    f_init = f_current
    f1_c=f1
    f2_c=f2
    PRINT*,'f original = ',f_current
    WRITE(42,*) 'f original = ',f_current
                        
    CALL myg1(n,x,g1,iterm)
    CALL myg2(n,x,g2,iterm)
    nge1  = 1
    nge2  = 1 
            
    ppinit = SQRT(DOT_PRODUCT(g1-g2,g1-g2))   
    pp = ppinit
    
    g1norm = SQRT(DOT_PRODUCT(g1,g1))    ! The norm in the current iteration point.     
    gmaxnorm = SQRT(DOT_PRODUCT(g2,g2))  ! There is only one subgradient in the bundle.  
   
    ! Set the bundle and index sets
    bundle(1) % jk = 0 
    bundle(1) % x_bundle = x
    bundle(1) % g1_bundle = g1
    bundle(1) % g2_bundle = g2
    bundle(1) % f1_bundle = f1
    bundle(1) % f2_bundle = f2
    bundle(1) % alpha1_bundle = zero
    bundle(1) % alpha2_bundle = zero
    bundle(1) % g2norm_bundle = gmaxnorm
    nb = 1
    nb1  = 1
    nb2  = 1
    
    !***********************************************************************            
    ! Start of the outer iteration   
    !***********************************************************************
         
    outerloop: DO !WHILE (nit <= mit) 
        nit = nit + 1   ! Counter for outer iterations.
        niit = 0        ! Counter for inner iterations.
        PRINT*,'OUTER',nit, f_current
               
        ! Set the temporary bundle as an empty set
        ntb = 0 
        
        ! Start of the inner iteration    
        innerloop: DO
            niit = niit + 1
            PRINT*,'inner',niit
            
            !***********************************************************************            
            ! Step 1. Compute the search direction
            !***********************************************************************    
 
            IF (bfull == 0) THEN
                CALL quadratic_solver(x, n, nb, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob)
            ELSE
                CALL quadratic_solver(x, n, size_b, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob)
            END IF
                         
            !***********************************************************************                        
            ! Step 2. Stopping test
            !***********************************************************************        
            IF (v >= -tol) THEN
                iterm = 1 
                
                WRITE(41,*) ' Nf = ',nfe,' Ng1 = ',nge1,' Ng2 = ',nge2,' f = ',f_current,&
                    &' CPU time = ',finish_time-start_time,'  nff=',nff 
                WRITE(41,*)  
                WRITE(41,*) '  ----  x_optimal ----- '    
                
                EXIT outerloop ! Desired accuracy.
            END IF
            
            !***********************************************************************                        
            ! Step 3
            !***********************************************************************                        
                       
            tt=1.0_prec 
            
            !***********************************************************************                        
            ! Step 4. Descent test 1
            !***********************************************************************
                                                               
            if (nit < 5) then            
             CALL myf1(n,x+d,f1,iterm) 
             CALL myf2(n,x+d,f2,iterm)
            
             nfe  = nfe + 1
            
             IF (f1-f2-f_current < sdp*tt*v) THEN
                 EXIT innerloop 
             END IF
            END IF 
            
            !***********************************************************************      
            ! Step 5.  
            !***********************************************************************  
                
            tt1 = 0.001_prec

            if(nit < 5) then
             IF(tt*dnorm > teta) THEN
              CALL myf1(n,x+tt1*d,f10,iterm) 
              CALL myf2(n,x+tt1*d,f20,iterm)
            
              fd = f10 - f20 - f_current-tt1*sdp*v
            
              nfe = nfe + 1
              nff = nff + 1
              IF (fd < zero) THEN    
               DO WHILE (tt*dnorm > teta)       
                 tt= rt1 * tt                                                       
                 CALL myf1(n,x+tt*d,f1,iterm)
                 CALL myf2(n,x+tt*d,f2,iterm)
                 nfe = nfe + 1 
                 nff = nff + 1            
                 IF (f1-f2-f_current < tt*sdp*v) THEN
                     EXIT innerloop ! Descent direction.
                 END IF
               END DO
              END IF
             END IF
            END IF
            
            !***********************************************************************                        
            ! Step 6. Stopping test and direction switch
            !***********************************************************************
              
            tt=1.0_prec  

            d_old=d
            IF (bfull == 0) THEN
                CALL quadratic_solver2(x, n, nb, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob)
            ELSE
                CALL quadratic_solver2(x, n, size_b, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob)
            END IF
            
            PRINT*,'  Step 4: v =',v
            IF (v >= -tol) THEN
                iterm = 1                           
                
                WRITE(41,*) 
                WRITE(41,*) '  ----  f_optimal, No.func.eval, No.grad.eval ----- '
                WRITE(41,*) ' Nf = ',nfe,' Ng1 = ',nge1,' Ng2 = ',nge2,' f = ',f_current,&
                    &' CPU time = ',finish_time-start_time,'  nff=',nff 
                WRITE(41,*)  
                WRITE(41,*) '  ----  x_optimal ----- '   
            
                EXIT outerloop ! Desired accuracy.
            END IF
                               
            !***********************************************************************                           
            ! Step 7. Descent test 2
            !***********************************************************************  
                                  
            PRINT*,'  Step 5: v=',v
            IF(DOT_PRODUCT(d_old-d,d_old-d) == 0.0_prec) PRINT*,'    Step 5: d is not changing'
            print*,v,nit,niit,f1-f2,f_current,DOT_PRODUCT(d_old-d,d_old-d),DOT_PRODUCT(d,d)            
            CALL myf1(n,x+d,f1,iterm) 
            CALL myf2(n,x+d,f2,iterm)  
            
            nfe = nfe + 1   
	        IF (f1-f2-f_current < sdp*v) THEN
                PRINT*,'    Step 5: Descent direction 2 (30): v=',v,'pp=',pp     
               
                EXIT innerloop ! Descent direction.
            END IF
              
            !***********************************************************************                        
            ! Step 8. Stepsize update
            !***********************************************************************  
              
            tt1 = 0.001_prec
            IF(tt*dnorm > teta) THEN
             if(nit<5) then
              CALL myf1(n,x+tt1*d,f10,iterm) 
              CALL myf2(n,x+tt1*d,f20,iterm)

              nfe = nfe + 1
              nff = nff + 1
           
              fd = f10 - f20 - f_current-tt1*sdp*v
              IF (fd < zero) THEN    

               DO WHILE (tt*dnorm > teta)       
                 tt= rt1 * tt                                                       
                 CALL myf1(n,x+tt*d,f1,iterm)
                 CALL myf2(n,x+tt*d,f2,iterm)
                 nfe = nfe + 1 
                 nff = nff + 1                    
                 IF (f1-f2-f_current < tt*sdp*v) THEN
                     EXIT innerloop ! Descent direction.
                 END IF
               END DO
              END IF
             END IF 
            END IF
           
            !***********************************************************************                             
            !        ! Step 9 Compute a new bundle element
            !***********************************************************************            
           
            CALL myg1(n,x+tt*d,g1,iterm)
           
            nge1 = nge1 + 1

            alpha1 = f1_c - f1 + tt*DOT_PRODUCT(g1,d) 
        
            !***********************************************************************        
            ! Step 10. Bundle enrichement
            !***********************************************************************                        
            
            ntb  = ntb + 1 ! Updating the temporary bundle indices.
                        
            ! Adding new element to temporary bundle.
            
            tbundle(ntb) % tx_bundle = x+tt*d  
            tbundle(ntb) % tg1_bundle = g1
            tbundle(ntb) % tf1_bundle = f1            
            tbundle(ntb) % talpha1_bundle = alpha1
            
            
            IF (niit >= 5000) THEN   
                iterm = 4                           
                
                EXIT outerloop
            END IF
            
            ! Additional stopping with function calls.
            IF (nfe >= mfe) THEN
                iterm = 3                 
                EXIT outerloop ! too many function calls
            END IF

        END DO innerloop

        !***********************************************************************            
        ! Step 11. Serious step        
        !***********************************************************************

        CALL myf1(n,x+d,f11,iterm)
        CALL myf2(n,x+d,f22,iterm)                            
                     
        pp = pp*(1-((f11-f22-f_current)/v))    
                                                             
        pp = MAX(pp,ppinit/10,ppmin)   ! Proximity parameter.           
        
        x = x + tt*d         
        y = x

       
        f1_c = f1
        f2_c = f2
        f_current = f1-f2         
        f_current1 = f_current
        tt1=tt
        
        IF (ls == 1) THEN ! Simple line search is used
            
            linesearch: DO
                y = y + 2.0_prec*tt*d 
                CALL myf1(n,y,f1,iterm)
                CALL myf2(n,y,f2,iterm)
                nf0=nf0+1
                fy = f1-f2
                nfe = nfe + 1
                IF (fy-f_current <= tt*sdp1*v) THEN 
                    x = y
                    f1_c = f1
                    f2_c = f2
                    f_current1 = fy
                    tt1 = tt1+2.0_prec*tt
                ELSE
                    tt = tt1
                    f_current = f_current1
                    EXIT linesearch
                END IF
            END DO linesearch
             
        END IF   
        write(41,*) nf0,v,tt
        
        ! Additional stopping with iterations.
        IF (nit == mit) THEN
            iterm = 2
            EXIT outerloop ! too many iterations
        END IF
        
        CALL myg1(n,x,g1,iterm)
        CALL myg2(n,x,g2,iterm)
        
        nge1  = nge1 + 1
        nge2  = nge2 + 1
        g1norm = SQRT(DOT_PRODUCT(g1,g1))   ! The norm in the current iteration point.
        gmaxnorm = SQRT(DOT_PRODUCT(g2,g2)) ! Just temporary, we will check later the real maximum norm.    
        
        !***********************************************************************                 
        ! Step  12. Bundle enrichement and index sets definitions
        !***********************************************************************    
                        
        nb  = nb + 1                          ! Updating the bundle indices.
        IF (nb > size_b) THEN
            PRINT*,'bundle full',nit
            bfull = 1                         ! the bundle is full. 
            nb = 2                            ! The first index should be the current iteration point.
        END IF
        IF (nb == iglob) nb = nb+1            ! the bundle element that gave us the global minimum is not overwritten.
        IF (nb > size_b) nb = 2               ! The previous step may move us out of the bounds again.
               
        bundle(nb) = bundle(1)
        
        ! linearization errors and bundle indices.
        nb1=1 
        nb2=1
        
        IF (bfull == 0) THEN
            DO i = 2,nb 
                alpha1 = f1_c - bundle(i) % f1_bundle - DOT_PRODUCT(bundle(i) % g1_bundle,x-bundle(i) % x_bundle) 
                alpha2 = f2_c - bundle(i) % f2_bundle - DOT_PRODUCT(bundle(i) % g2_bundle,x-bundle(i) % x_bundle) 

                IF (ABS(alpha1-alpha2) < atol) THEN
                    bundle(i) % jk = 0
                    nb1=nb1+1
                    nb2=nb2+1
                ELSE IF (alpha1-alpha2 <= -atol) THEN   
                    bundle(i) % jk = 1
                    nb1=nb1+1
                ELSE
                    bundle(i) % jk = -1
                    nb2=nb2+1
                END IF
            END DO

        ELSE
            DO i = 2,size_b 
                alpha1 = f1_c - bundle(i) % f1_bundle - DOT_PRODUCT(bundle(i) % g1_bundle,x-bundle(i) % x_bundle) 
                alpha2 = f2_c - bundle(i) % f2_bundle - DOT_PRODUCT(bundle(i) % g2_bundle,x-bundle(i) % x_bundle) 
           
                IF (ABS(alpha1-alpha2) < atol) THEN
                    bundle(i) % jk = 0
                    nb1=nb1+1
                    nb2=nb2+1
                ELSE IF (alpha1-alpha2 <= -atol) THEN   
                    bundle(i) % jk = 1
                    nb1=nb1+1
                ELSE 
                    bundle(i) % jk = -1
                    nb2=nb2+1
                END IF
            END DO
            
        END IF
        
        bundle(1) % x_bundle = x 
        bundle(1) % g1_bundle = g1
        bundle(1) % g2_bundle = g2
        bundle(1) % f1_bundle = f1_c
        bundle(1) % f2_bundle = f2_c
        bundle(1) % alpha1_bundle = zero 
        bundle(1) % alpha2_bundle = zero
        bundle(1) % g2norm_bundle = gmaxnorm

    END DO outerloop  
    !***********************************************************************                            
    ! Results:
    IF (iterm == 1) THEN
        PRINT*,' The problem has been solved with the desired accuracy. ', f_current,nit,nfe,nge1,nge2
    ELSE IF (iterm == 2) THEN  
        PRINT*,' The maximum number of iterations is reached. ', f_current,nit,nfe,nge1,nge2
    ELSE IF (iterm == 3) THEN
        PRINT*,' The maximum number of function evaluations is reached. ', f_current,nit,nfe,nge1,nge2
    ELSE IF (iterm == 4) THEN
        PRINT*,' The maximum number of inner iterations is reached. ', f_current,nit,nfe,nge1,nge2
    ELSE
        PRINT*,' The problem has not been solved. ', f_current,nit,nfe,nge1,nge2
    END IF
    
    CALL cpu_time(finish_time)
    WRITE(41,*) x
    WRITE(42,*)
    WRITE(42,*) 'Ni = ',nit,' Nf = ',nfe,' Ng1 = ',nge1,' Ng2 = ',nge2,' f = ',f_current,&
        &'term = ',iterm,' CPU time = ',finish_time-start_time 
           
    PRINT*,'cpu time: ',finish_time-start_time       
    CLOSE(42)
    CLOSE(41)

    STOP

CONTAINS

    !===========================================================================================
    SUBROUTINE quadratic_solver(x, n, nb, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob) 
        ! Solves the subproblems in the original search direction problem
        ! Calls PLQDF1 by Ladislav Luksan
        !
        ! INPUT: * 'x'               : The current iteratioin point
        !        * 'n','nb1','nb2'   : The number of variables and the sizes of the bundles b1 and b2, respectively    
        !        * 'bundle'          : The bundle
        !        * 'pp'              : The proximity parameter
        !
        ! NOTICE: The dimensions of vectors 'x' has to be 'n'.
              
        IMPLICIT NONE    
        EXTERNAL PLQDF1             ! The QUADRATIC SOLVER by Ladislav Luksan
                   
        TYPE(bundle_element), DIMENSION(size_b), INTENT(IN)    :: bundle     ! bundle
        TYPE(tbundle_element), DIMENSION(size_tb), INTENT(IN)  :: tbundle    ! temporary bundle           
        REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: x         ! current iteration point
               
        REAL(KIND=prec), INTENT(IN) :: pp                      ! proximity parameter
        REAL(KIND=prec), DIMENSION(:), INTENT(OUT) :: d        ! search direction
        REAL(KIND=prec), INTENT(OUT) :: v                      ! solution for the search direction problem
        REAL(KIND=prec), INTENT(OUT) :: dnorm                  ! norm of d
               
        INTEGER, INTENT(IN)  :: n      ! number of variables
        INTEGER, INTENT(IN)  :: nb     ! current bundle size
        INTEGER, INTENT(IN)  :: nb1    ! current bundle size of b1
        INTEGER, INTENT(IN)  :: ntb    ! current temporary bundle  
        INTEGER, INTENT(OUT) :: iglob  ! index of bundle that gives the global optimum.
                     
        !*************************** LOCAL VARIABLES ************************************
            
        ! .. Parameters used in PLQDF1 ..
        REAL(KIND=prec), PARAMETER :: ETA0  = 1.0E-15_prec  
        REAL(KIND=prec), PARAMETER :: ETA2  = 1.0E-12_prec
        REAL(KIND=prec), PARAMETER :: ETA9  = 1.0E+60_prec
        REAL(KIND=prec), PARAMETER :: EPS7  = 1.0E-14_prec
        REAL(KIND=prec), PARAMETER :: EPS9  = 1.0E-12_prec 

        ! .. Scalar Arguments in PLQDF1 ..               
        REAL(KIND=prec) :: GMAX   ! output of PLQDF1: maximum absolute value of a partial derivative
        REAL(KIND=prec) :: UMAX   ! output of PLQDF1: maximum absolute value of a negative lagrange multiplier
        REAL(KIND=prec) :: XNORM  ! output of PLQDF1: value of linearized minimax function = delta_1 + delta_2  
               
        REAL(KIND=prec) :: alpha_b2        ! a linearization error of b2
        REAL(KIND=prec) :: v_trial,v2,a    ! help variables
               
        INTEGER :: i, j, k, l              ! help variables
               
        INTEGER, PARAMETER :: NC = 0             ! number of constraints is zero
        INTEGER :: IDECF=10, KBC=0, KBF=0, MFP=2 ! IDECF=10 diagonal matrix; KBC=0 no linear constraints; KBF=0 no simple bounds; MFP=2 optimum feasible point
        INTEGER :: ITERQ, NM                     ! output values of PLQDF1: ITERQ=type of feasible point 
                                                 ! N=dimension of manifold defined by active constraints 
        ! .. Array Arguments in PLQDF1..
        INTEGER, DIMENSION(n) :: IX              ! vector containing types of bounds
        INTEGER, DIMENSION(nb1+ntb) :: IA        ! vector containing types of deviations 
        INTEGER, DIMENSION(NC) :: IC             ! vector containing types of constraints. NOT significant because NC=0
        INTEGER, DIMENSION(n+1) :: IAA           ! Output of PLQDF1: vector containing indicies of active functions
               
        REAL(KIND=prec), DIMENSION(n) :: XL, XU          ! lower and upper bounds for x. NOT significant because variables are unbounded
        REAL(KIND=prec), DIMENSION(n) :: H               ! diagonal matrix (1/t)*I
        REAL(KIND=prec), DIMENSION(NC) :: CF, CL, CU     ! NOT significant since NC=0 (are related to constraints)
        REAL(KIND=prec), DIMENSION(nb1+ntb) ::  AF       ! vector of bundle function values (-alpha)
        REAL(KIND=prec), DIMENSION(nb1+ntb) :: AFD       ! Output of PLQDF1: vector containing increments of the approximated functions
        REAL(KIND=prec), DIMENSION(n+1) :: AZ            ! Output of PLQDF1: vector of Lagrange multipliers
        REAL(KIND=prec), DIMENSION(n+1) :: S             ! Output of PLQDF1: direction vector        
        REAL(KIND=prec), DIMENSION(n+1) :: G             ! Output of PLQDF1: gradient of the Lagrangian function
        REAL(KIND=prec), DIMENSION(n*(nb1+ntb)) :: AG    ! matrix whose columns are bundle subgradients 
        REAL(KIND=prec), DIMENSION((n+1)*(n+2)/2) :: AR  ! Output of PLQDF1: triangular decomposition of kernel of the orthogonal projection
        REAL(KIND=prec), DIMENSION(n*NC) :: CG           ! NOT significant since NC=0. matrix whose columns are normals of the linear constraints            
               
        ! .. Some other varibles ..    
        REAL(KIND=prec), DIMENSION(n*(nb1+ntb)) :: grad_b1    ! Vector with all subgradients in b1.
        REAL(KIND=prec), DIMENSION(nb1+ntb) :: alpha_b1       ! Vector with all linearization errors in b1.
        REAL(KIND=prec), DIMENSION(n) :: grad_b2              ! A subgradient of b2


        !************************** SUBPROBLEM SOLVER STARTS *********************************   
        !****************************** INITIALIZATIONS **************************************
                 
        v2=large      
        v = large         
        IX = 0             ! types of bounds: 0 - unbounded variables
        IA = 2             ! types of deviations                     
        H = pp             ! diagonal matrix                
        j = 0
        
        DO i = 1, nb
            IF ((bundle(i) % jk) >= 0) THEN
                j=j+1               
                grad_b1((j-1)*n+1:j*n) = bundle(i) % g1_bundle
                alpha_b1(j) = bundle(i) % alpha1_bundle
            END IF
        END DO
        
        ! Adding components in temporary bundle
        DO i = 1, ntb
            j=j+1
            grad_b1((j-1)*n+1:j*n) = tbundle(i) % tg1_bundle
            alpha_b1(j) = tbundle(i) % talpha1_bundle
        END DO
        
        subproblems1: DO i = 1, nb   
            IF (bundle(i) % jk > 0) CYCLE subproblems1 ! each index in b2 is looked through !                   
            grad_b2 = bundle(i) % g2_bundle
            alpha_b2 = bundle(i) % alpha2_bundle
                                  
            DO j = 1, nb1+ntb
                k = (j-1)*n
                DO l = 1, n
                    AG(k+l) = grad_b1(k+l) - grad_b2(l)
                END DO
                AF(j) = - alpha_b1(j) + alpha_b2 
            END DO
                        
           
            ! Calls PLQDF1 by Ladislav Luksan
            CALL PLQDF1(n,nb1+ntb,NC,X,IX,XL,XU,AF,AFD,IA,IAA,AG,AR,AZ, &
                & CF,IC,CL,CU,CG,G,H,S,MFP,KBF,KBC,IDECF,ETA0,ETA2, &
                & ETA9,EPS7,EPS9,XNORM,UMAX,GMAX,NM,ITERQ)                        
                                   
            ! Global solution                   
            a = DOT_PRODUCT(S(1:n),S(1:n))           
            v_trial = XNORM + (a) / (2.0_prec*pp) 
            IF (v > v_trial) THEN
                v2 = xnorm 
                v = v_trial
                dnorm = a
                iglob = i
                DO j = 1, n  
                    d(j) = S(j)                    
                END DO                
            END IF                  
        END DO subproblems1
                
        v=v2 
            
    END SUBROUTINE quadratic_solver
    !===========================================================================================
    SUBROUTINE quadratic_solver2(x, n, nb, nb1, bundle, ntb, tbundle, pp, d, v, dnorm, iglob) 
        
        ! NOTICE: The dimensions of vectors 'x' has to be 'n'.
              
        IMPLICIT NONE    
        EXTERNAL PLQDF1             ! The QUADRATIC SOLVER by Ladislav Luksan
                   
        TYPE(bundle_element), DIMENSION(size_b), INTENT(IN)    :: bundle     ! bundle
        TYPE(tbundle_element), DIMENSION(size_tb), INTENT(IN)  :: tbundle    ! temporary bundle   
        
        REAL(KIND=prec), DIMENSION(:), INTENT(IN) :: x         ! current iteration point               
        REAL(KIND=prec), INTENT(IN) :: pp                      ! proximity parameter
        REAL(KIND=prec), DIMENSION(:), INTENT(OUT) :: d        ! search direction
        REAL(KIND=prec), INTENT(OUT) :: v                      ! solution for the search direction problem
        REAL(KIND=prec), INTENT(OUT) :: dnorm                  ! norm of d
               
        INTEGER, INTENT(IN)  :: n      ! number of variables
        INTEGER, INTENT(IN)  :: nb     ! current bundle size
        INTEGER, INTENT(IN)  :: nb1    ! current bundle size of b1
        INTEGER, INTENT(IN)  :: ntb    ! current temporary bundle  
        INTEGER, INTENT(OUT) :: iglob  ! index of bundle that gives the global optimum.
                     
        !*************************** LOCAL VARIABLES ************************************
            
        ! .. Parameters used in PLQDF1 ..
        REAL(KIND=prec), PARAMETER :: ETA0  = 1.0E-15_prec  
        REAL(KIND=prec), PARAMETER :: ETA2  = 1.0E-12_prec
        REAL(KIND=prec), PARAMETER :: ETA9  = 1.0E+60_prec
        REAL(KIND=prec), PARAMETER :: EPS7  = 1.0E-14_prec
        REAL(KIND=prec), PARAMETER :: EPS9  = 1.0E-12_prec 

        ! .. Scalar Arguments in PLQDF1 ..               
        REAL(KIND=prec) :: GMAX   ! output of PLQDF1: maximum absolute value of a partial derivative
        REAL(KIND=prec) :: UMAX   ! output of PLQDF1: maximum absolute value of a negative lagrange multiplier
        REAL(KIND=prec) :: XNORM  ! output of PLQDF1: value of linearized minimax function = delta_1 + delta_2  
               
        REAL(KIND=prec) :: alpha_b2        ! a linearization error of b2
        REAL(KIND=prec) :: v_trial,v2,a    ! help variables               
        INTEGER :: i, j, k, l              ! help variables
               
        INTEGER, PARAMETER :: NC = 0             ! number of constraints is zero
        INTEGER :: IDECF=10, KBC=0, KBF=0, MFP=2 ! IDECF=10 diagonal matrix; KBC=0 no linear constraints; KBF=0 no simple bounds; MFP=2 optimum feasible point
        INTEGER :: ITERQ, NM                     ! output values of PLQDF1: ITERQ=type of feasible point 
                                                 ! N=dimension of manifold defined by active constraints 
        ! .. Array Arguments in PLQDF1..
        INTEGER, DIMENSION(n) :: IX              ! vector containing types of bounds
        INTEGER, DIMENSION(nb1+ntb) :: IA        ! vector containing types of deviations 
        INTEGER, DIMENSION(NC) :: IC             ! vector containing types of constraints. NOT significant because NC=0
        INTEGER, DIMENSION(n+1) :: IAA           ! Output of PLQDF1: vector containing indicies of active functions
               
        REAL(KIND=prec), DIMENSION(n) :: XL, XU          ! lower and upper bounds for x. NOT significant because variables are unbounded
        REAL(KIND=prec), DIMENSION(n) :: H               ! diagonal matrix (1/t)*I
        REAL(KIND=prec), DIMENSION(NC) :: CF, CL, CU     ! NOT significant since NC=0 (are related to constraints)
        REAL(KIND=prec), DIMENSION(nb1+ntb) ::  AF       ! vector of bundle function values (-alpha)
        REAL(KIND=prec), DIMENSION(nb1+ntb) :: AFD       ! Output of PLQDF1: vector containing increments of the approximated functions
        REAL(KIND=prec), DIMENSION(n+1) :: AZ            ! Output of PLQDF1: vector of Lagrange multipliers
        REAL(KIND=prec), DIMENSION(n+1) :: S             ! Output of PLQDF1: direction vector
        REAL(KIND=prec), DIMENSION(n+1) :: G             ! Output of PLQDF1: gradient of the Lagrangian function
        REAL(KIND=prec), DIMENSION(n*(nb1+ntb)) :: AG    ! matrix whose columns are bundle subgradients 
        REAL(KIND=prec), DIMENSION((n+1)*(n+2)/2) :: AR  ! Output of PLQDF1: triangular decomposition of kernel of the orthogonal projection
        REAL(KIND=prec), DIMENSION(n*NC) :: CG           ! NOT significant since NC=0. matrix whose columns are normals of the linear constraints            
               
        ! .. Some other varibles ..    
        REAL(KIND=prec), DIMENSION(n*(nb1+ntb)) :: grad_b1   ! Vector with all subgradients in b1.
        REAL(KIND=prec), DIMENSION(nb1+ntb) :: alpha_b1      ! Vector with all linearization errors in b1.
        REAL(KIND=prec), DIMENSION(n) :: grad_b2             ! A subgradient of b2


        !************************** SUBPROBLEM SOLVER STARTS *********************************   
        !****************************** INITIALIZATIONS **************************************
        v2=large      
        v = large         
        IX = 0             ! types of bounds: 0 - unbounded variables
        IA = 2             ! types of deviations             
        H =  pp              ! diagonal matrix 
               
        j = 0
        DO i = 1, nb
            IF ((bundle(i) % jk) >= 0) THEN
                j=j+1
                grad_b1((j-1)*n+1:j*n) = bundle(i) % g1_bundle
                alpha_b1(j) = bundle(i) % alpha1_bundle
            END IF
        END DO
        ! Adding components in temporary bundle
        DO i = 1, ntb
            j=j+1
            grad_b1((j-1)*n+1:j*n) = tbundle(i) % tg1_bundle
            alpha_b1(j) = tbundle(i) % talpha1_bundle
        END DO
        
        subproblems1: DO i = 1, 1 ! current iterate is in bundle index 1.   
                              
            grad_b2 = bundle(i) % g2_bundle
            alpha_b2 = bundle(i) % alpha2_bundle
                                 
            DO j = 1, nb1+ntb
                k = (j-1)*n
                DO l = 1, n
                    AG(k+l) = grad_b1(k+l) - grad_b2(l)
                END DO
                AF(j) = - alpha_b1(j) + alpha_b2 
            END DO
                        
            ! Calls PLQDF1 by Ladislav Luksan
            CALL PLQDF1(n,nb1+ntb,NC,X,IX,XL,XU,AF,AFD,IA,IAA, &
                & AG,AR,AZ,CF,IC,CL,CU,CG,G,H,S,MFP,KBF,KBC,IDECF, &
                & ETA0,ETA2,ETA9,EPS7,EPS9,XNORM,UMAX,GMAX,NM,ITERQ)
                     
            ! Global solution                   
            a = DOT_PRODUCT(S(1:n),S(1:n))           
            v_trial = XNORM + (a) / (2.0_prec*pp) 
            
            IF (v > v_trial) THEN
                v2 = xnorm 
                v = v_trial
                dnorm = a
                iglob = i
                DO j = 1, n  ! S has dimension n+1 but the last element is something else.
                    d(j) = S(j) 
                    !d(j) = S(j) / SQRT(a) !supposing a ne 0
                END DO
                
            END IF    
              
        END DO subproblems1

        v=v2 
              
    END SUBROUTINE quadratic_solver2

END PROGRAM bem
