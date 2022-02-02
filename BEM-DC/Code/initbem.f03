!*************************************************************************
!*                                                                       *
!*     Initialization of parameters for BEM-DC                           *
!*     (last modified 02.02.2022)                                        *
!*                                                                       *
!*************************************************************************
!*
!*     Modules included:
!*
!*     initbem          ! Initialization of parameters for BEM-DC (main program bem).
!*

MODULE initbem  ! Initialization of parameters.

    USE r_precision, ONLY : prec        ! Precision for reals.
    IMPLICIT NONE

    ! Parameters
    INTEGER, PARAMETER :: &
        n        = 10, &                 ! Number of variables, from user.
        mit      = 10000, &              ! Maximum number of iterations.
        mfe      = 500000, &             ! Maximum number of function evaluations.
        size_b   = min(n+3,100), &       ! Size of the bundle.  
        size_tb  = 5000, &               ! Size of the temporary bundle. 
        ls       = 1                     ! Indicator for line search:
                                         !   0 - no line search (default);
                                         !   1 - simple line search in serious steps.   
        
    ! Names of output files:
    CHARACTER(LEN=80), SAVE :: &  
        outfile1 = 'results1.txt', &     ! Objective value, number of evaluations etc. %%%%%pnf1pnf2sp-n
        outfile2 = 'results2.txt'        ! Optimal points.
        
    INTEGER, SAVE :: &
        pnf1     = 10, &                 ! Number of the first component function, nf1 = 1,...,18. 19 = zero
                                         ! pnf1 =  1,2,3,4,5,10,11,12,13,16,17
        pnf2     = 10, &                 ! Number of the second component function, nf2 = 1,...,18, 19 = zero, nf2 /= nf1.
        sp       = 10                    ! Number of the starting point, sp = 1,...,5.
 
    ! Input real parameters. 
    REAL(KIND=prec), SAVE :: &
        tol      = 10.0_prec**(-6), &    ! Stopping tolerance. (\eta in paper) 
        sdp      = 10.0_prec**(-1), &    ! Sufficient decrease parameter.  (\mu in paper)    
        sdp1     = 10.0_prec**(-4), &    !
        eps      = 10.0_prec**(-5), &    ! Enlargement parameter.
        tt       = 1.0_prec,        &    ! step size.     
        teta     =  0.00005,        &    ! null step parameter. (theta in paper) 
        rt1      =  0.6 ,           &    ! step size reduction. (\sigma in paper) 
        ppmin    = 10.0_prec**(-5), &    ! proximity threshold (\delta_min in paper) 
        r1       = 0.8, &                ! Decrease parameter 1. 
        r2       = 0.2, &                ! Decrease parameter 2.
        r3       = 20000000.0, &         ! Increase parameter. 
        pp                               ! Proximity parameter (no need for initial value). 
        
 
    REAL(KIND=prec), DIMENSION(n), SAVE :: & 
        x                                ! Vector of variables
        

        
CONTAINS

    SUBROUTINE init_par()  ! Initialization of parameters. May be left empty.

        IMPLICIT NONE
        
        
    END SUBROUTINE init_par


END MODULE initbem
